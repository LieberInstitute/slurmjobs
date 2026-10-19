test_that("prefix metacharacters are selected and replaced literally", {
    prefixes <- c("01.", "01+", "01[", "01(", "01|", "01$", "01^", "01?", "01*", "01{", "01]", "01)")
    if (.Platform$OS.type == "windows") prefixes <- setdiff(prefixes, c("01|", "01?", "01*"))
    for (before in prefixes) {
        path <- normalizePath(withr::local_tempdir())
        stem <- paste0(before, "qc")
        writeLines(paste("reference", stem), file.path(path, paste0(stem, ".R")))
        writeLines("unrelated bytes", file.path(path, "01Xqc.R"))
        after <- "02[+.$"
        expect_no_error(renumber(path, before, after, recursive_edits = TRUE))
        expect_identical(readLines(file.path(path, paste0(after, "qc.R"))), paste("reference", paste0(after, "qc")))
        expect_identical(readLines(file.path(path, "01Xqc.R")), "unrelated bytes")
        expect_false(file.exists(file.path(path, paste0(stem, ".R"))))
    }
})

test_that("regex-only matches do not satisfy expect_matches", {
    for (prefix in c("01.", "01+", "01[", "01|02")) {
        path <- normalizePath(withr::local_tempdir())
        writeLines("original", file.path(path, "01Xqc.R"))
        writeLines("other", file.path(path, "011qc.R"))
        writeLines("other", file.path(path, "02_model.R"))
        before <- overwrite_snapshot(path)
        expect_error(renumber(path, prefix, "03"), "did not match")
        expect_identical(overwrite_snapshot(path), before)
        expect_no_error(renumber(path, prefix, "03", expect_matches = FALSE, recursive_edits = TRUE))
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("multi-dot files retain their version in stems and references", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("01_qc.v2 01_qc.v3", file.path(path, "01_qc.v2.R"))
    writeLines("Rscript 01_qc.v2.R", file.path(path, "01_qc.v2.sh"))
    expect_no_error(renumber(path, "01", "02", recursive_edits = TRUE))
    expect_identical(readLines(file.path(path, "02_qc.v2.R")), "02_qc.v2 01_qc.v3")
    expect_identical(readLines(file.path(path, "02_qc.v2.sh")), "Rscript 02_qc.v2.R")
})

test_that("dotted directories retain their whole name as the replacement stem", {
    path <- normalizePath(withr::local_tempdir())
    directory <- file.path(path, "01_qc.v2")
    dir.create(directory)
    writeLines("01_qc.v2 01_qc.v3", file.path(directory, "references.R"))
    expect_no_error(renumber(path, "01", "02", recursive_edits = TRUE))
    expect_identical(readLines(file.path(path, "02_qc.v2", "references.R")), "02_qc.v2 01_qc.v3")
})

test_that("different dotted stems are rejected rather than conflated", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("one", file.path(path, "01_qc.v1.R"))
    writeLines("two", file.path(path, "01_qc.v2.R"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, "01", "02", recursive_edits = TRUE), "different base names")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("literal prefix mappings support dots swaps and full filename prefixes", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("01.qc 02.model", file.path(path, "01.qc.R"))
    writeLines("02.model 01.qc", file.path(path, "02.model.R"))
    expect_no_error(renumber(path, c("01.", "02."), c("02.", "01."), recursive_edits = TRUE))
    expect_identical(readLines(file.path(path, "02.qc.R")), "02.qc 01.model")
    expect_identical(readLines(file.path(path, "01.model.R")), "01.model 02.qc")

    path <- normalizePath(withr::local_tempdir())
    writeLines("01_qc.R", file.path(path, "01_qc.R"))
    expect_no_error(renumber(path, "01_qc.R", "02_qc.R", recursive_edits = TRUE))
    expect_identical(readLines(file.path(path, "02_qc.R")), "02_qc.R")
})

test_that("overlapping literal prefixes still fail before any changes", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("01_qc.v2", file.path(path, "01_qc.v2.R"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, c("01", "01_qc"), c("02", "03_qc"), recursive_edits = TRUE),
        "duplicate source")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("logs use full literal script stem and supported array suffixes", {
    path <- normalizePath(withr::local_tempdir())
    logs <- overwrite_fixture(path)
    file.remove(file.path(path, "code", "01_qc.sh"), file.path(path, "code", "01_qc.R"),
        file.path(logs, "01_qc.txt"))
    old <- "01.qc[v2]+"
    new <- "02.qc[v2]+"
    writeLines(paste("#SBATCH -o", file.path(logs, paste0(old, ".txt"))),
        file.path(path, "code", paste0(old, ".sh")))
    related <- paste0(old, c(".txt", ".log", "_a_c.txt", "_b_12.log", ".1.txt", ".002.log"))
    unrelated <- c("01.unrelated.log", "01Xqc[v2]+.txt", paste0(old,
        c("other.txt", ".v3.txt", ".1other.log", "-other.log")))
    for (name in c(related, unrelated)) writeLines(name, file.path(logs, name))
    expect_no_error(renumber(file.path(path, "code"), "01.", "02.", recursive_edits = TRUE))
    for (name in related) {
        target <- paste0(new, substring(name, nchar(old) + 1L))
        expect_identical(readLines(file.path(logs, target)), name)
        expect_false(file.exists(file.path(logs, name)))
    }
    for (name in unrelated) expect_identical(readLines(file.path(logs, name)), name)
})

test_that("narrowed log discovery retains collision checks for related logs", {
    path <- normalizePath(withr::local_tempdir())
    logs <- overwrite_fixture(path)
    writeLines("related source", file.path(logs, "01_qc.1.txt"))
    writeLines("protected destination", file.path(logs, "02_qc.1.txt"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(file.path(path, "code"), "01", "02", recursive_edits = TRUE), "overwrite")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("complete shell filename prefixes rename their associated log stems", {
    path <- normalizePath(withr::local_tempdir())
    logs <- overwrite_fixture(path)
    expect_no_error(renumber(file.path(path, "code"), "01_qc.sh", "02_model.sh"))
    expect_identical(readLines(file.path(logs, "02_model.txt")), "original log")
    expect_true(file.exists(file.path(path, "code", "01_qc.R")))
})

test_that("existing content substring matching policy remains unchanged", {
    path <- normalizePath(withr::local_tempdir())
    writeLines(c("archived_01_qc_backup", "# 01_qc"), file.path(path, "01_qc.R"))
    expect_no_error(renumber(path, "01", "02", recursive_edits = TRUE))
    expect_identical(readLines(file.path(path, "02_qc.R")), c("archived_02_qc_backup", "# 02_qc"))
})

test_that("missing literal prefixes do not shift other replacement mappings", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("01.qc", file.path(path, "01.qc.R"))
    expect_no_error(renumber(path, c("09[", "01."), c("10[", "02."),
        expect_matches = FALSE, recursive_edits = TRUE))
    expect_identical(readLines(file.path(path, "02.qc.R")), "02.qc")
    expect_identical(list.files(path), "02.qc.R")
})

test_that("duplicate destinations of genuinely related logs remain rejected", {
    path <- normalizePath(withr::local_tempdir())
    logs <- overwrite_fixture(path)
    writeLines(paste("#SBATCH -o", file.path(logs, "03_model.txt")),
        file.path(path, "code", "03_model.sh"))
    writeLines("second source log", file.path(logs, "03_model.txt"))
    before <- overwrite_snapshot(path)
    # Distinct final shell filenames (one loses .sh) but identical log stems.
    expect_error(renumber(file.path(path, "code"), c("01_qc.sh", "03_model.sh"),
        c("02_common", "02_common.sh"), recursive_edits = TRUE), "duplicate destination")
    expect_identical(overwrite_snapshot(path), before)
})
