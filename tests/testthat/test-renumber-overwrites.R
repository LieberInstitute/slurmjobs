test_that("occupancy includes dangling links without resolving their targets", {
    skip_on_os("windows")
    path <- normalizePath(withr::local_tempdir())
    regular <- file.path(path, "file")
    directory <- file.path(path, "directory")
    missing <- file.path(path, "missing")
    dangling <- file.path(path, "dangling")
    writeLines("keep me", regular)
    dir.create(directory)
    expect_true(file.symlink(missing, dangling))
    expect_identical(.renumber_path_exists(c(regular, directory, dangling, missing)),
        c(TRUE, TRUE, TRUE, FALSE))
    expect_identical(.renumber_path_exists(character()), logical())
    expect_error(.renumber_move(regular, dangling), "Occupied move destination")
    expect_identical(readLines(regular), "keep me")
    expect_identical(Sys.readlink(dangling), missing)
})

test_that("existing log destinations are rejected before recursive edits", {
    for (external in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        log_dir <- if (external) file.path(path, "other-logs") else file.path(path, "code", "logs")
        overwrite_fixture(path, log_dir)
        writeLines("irreplaceable destination", file.path(log_dir, "02_qc.txt"))
        before <- overwrite_snapshot(path)
        expect_error(renumber(file.path(path, "code"), "01", "02", recursive_edits = TRUE),
            "proposed renaming plan")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("temporary destinations cannot overwrite files or empty directories", {
    for (kind in c("file", "directory")) {
        path <- normalizePath(withr::local_tempdir())
        log_dir <- overwrite_fixture(path)
        target <- file.path(log_dir, "02_qc.txttemp_slurmjobs")
        if (kind == "file") writeLines("keep me", target) else dir.create(target)
        before <- overwrite_snapshot(path)
        expect_error(renumber(file.path(path, "code"), "01", "02", recursive_edits = TRUE),
            "proposed renaming plan")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("immediate temporary destinations are rejected before content changes", {
    path <- normalizePath(withr::local_tempdir())
    writeLines('x <- "01_qc"', file.path(path, "01_qc.R"))
    writeLines("protected temporary path", file.path(path, "02_qc.Rtemp_slurmjobs"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, "01", "02", recursive_edits = TRUE), "proposed renaming plan")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("hidden final destinations are occupied paths too", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("source", file.path(path, "01_qc.R"))
    writeLines("hidden destination", file.path(path, ".02_qc.R"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, "01", ".02", recursive_edits = TRUE), "proposed renaming plan")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("dangling symlink destinations and intermediates are never replaced", {
    skip_on_os("windows")
    for (intermediate in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        log_dir <- overwrite_fixture(path)
        target <- file.path(log_dir, paste0("02_qc.txt", if (intermediate) "temp_slurmjobs" else ""))
        expect_true(file.symlink(file.path(path, "does-not-exist"), target))
        before <- overwrite_snapshot(path)
        expect_error(renumber(file.path(path, "code"), "01", "02", recursive_edits = TRUE),
            "proposed renaming plan")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("unrelated logs sharing a numeric prefix are not selected", {
    path <- normalizePath(withr::local_tempdir())
    log_dir <- overwrite_fixture(path)
    writeLines(c(paste("#SBATCH -o", file.path(log_dir, "03_model.txt")), "Rscript 03_model.R"),
        file.path(path, "code", "03_model.sh"))
    writeLines("model", file.path(path, "code", "03_model.R"))
    writeLines("unrelated source log", file.path(log_dir, "03_qc.txt"))
    expect_no_error(renumber(file.path(path, "code"), c("01", "03"), c("02", "02"),
        recursive_edits = TRUE))
    expect_identical(readLines(file.path(log_dir, "03_qc.txt")), "unrelated source log")
    expect_identical(readLines(file.path(log_dir, "02_qc.txt")), "original log")
})

test_that("duplicate and aliased source paths are rejected", {
    path <- normalizePath(withr::local_tempdir())
    writeLines('x <- "01_qc"', file.path(path, "01_qc.R"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, c("01", "01_qc"), c("02", "03_qc"), recursive_edits = TRUE),
        "proposed renaming plan")
    expect_identical(overwrite_snapshot(path), before)

    # The shell and the data source select the same physical file via two paths.
    path <- normalizePath(withr::local_tempdir())
    dir.create(file.path(path, "logs"))
    writeLines(c("#SBATCH -o logs/01_qc.txt", "echo work"), file.path(path, "01_qc.sh"))
    writeLines("source", file.path(path, "01_qc.txt"))
    if (.Platform$OS.type != "windows") {
        expect_true(file.symlink(path, file.path(path, "logs", "alias")))
        writeLines(c("#SBATCH -o logs/alias/01_qc.txt", "echo work"), file.path(path, "01_qc.sh"))
        # Avoid traversing the intentionally cyclic directory alias in snapshots.
        before <- readLines(file.path(path, "01_qc.txt"))
        expect_error(renumber(path, "01", "02"), "proposed renaming plan")
        expect_identical(readLines(file.path(path, "01_qc.txt")), before)
        expect_true(file.exists(file.path(path, "01_qc.sh")))
    }
})

test_that("replacement prefixes cannot escape a source directory", {
    for (prefix in c("../outside/02", "/absolute/02", "..\\outside\\02")) {
        path <- normalizePath(withr::local_tempdir())
        dir.create(file.path(path, "code"))
        dir.create(file.path(path, "outside"))
        writeLines("source", file.path(path, "code", "01_qc.R"))
        writeLines("protected", file.path(path, "outside", "02_qc.R"))
        before <- overwrite_snapshot(path)
        expect_error(renumber(file.path(path, "code"), "01", prefix, recursive_edits = TRUE),
            "[Pp]refix|proposed renaming plan")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("file-directory conflicts are rejected without changing either object", {
    for (source_is_directory in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        source <- file.path(path, "01_qc")
        target <- file.path(path, "02_qc")
        if (source_is_directory) {
            dir.create(source)
            writeLines("source child", file.path(source, "child"))
            writeLines("destination", target)
        } else {
            writeLines("source", source)
            dir.create(target)
        }
        before <- overwrite_snapshot(path)
        expect_error(renumber(path, "01", "02"), "proposed renaming plan")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("moving a parent and its log child is rejected before content changes", {
    path <- normalizePath(withr::local_tempdir())
    dir.create(file.path(path, "01_qc"))
    writeLines("source log", file.path(path, "01_qc", "01_qc.txt"))
    writeLines(c("#SBATCH -o 01_qc/01_qc.txt", "echo work"), file.path(path, "01_qc.sh"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, "01", "02", recursive_edits = TRUE, protect_log_location = FALSE), "source inside")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("script and log cycles remain valid without overwrites", {
    path <- normalizePath(withr::local_tempdir())
    logs <- overwrite_fixture(path)
    writeLines(c(paste("#SBATCH -o", file.path(logs, "02_qc.txt")), "Rscript 02_qc.R"),
        file.path(path, "code", "02_qc.sh"))
    writeLines("second script", file.path(path, "code", "02_qc.R"))
    writeLines("second log", file.path(logs, "02_qc.txt"))
    expect_no_error(renumber(file.path(path, "code"), c("01", "02"), c("02", "01")))
    expect_identical(readLines(file.path(logs, "02_qc.txt")), "original log")
    expect_identical(readLines(file.path(logs, "01_qc.txt")), "second log")
    expect_identical(readLines(file.path(path, "code", "01_qc.R")), "second script")
})

test_that("directory cycles preserve every child", {
    path <- normalizePath(withr::local_tempdir())
    for (name in c("01_qc", "02_qc")) {
        dir.create(file.path(path, name))
        writeLines(name, file.path(path, name, "result.bin"))
    }
    expect_no_error(renumber(path, c("01", "02"), c("02", "01")))
    expect_identical(readLines(file.path(path, "01_qc", "result.bin")), "02_qc")
    expect_identical(readLines(file.path(path, "02_qc", "result.bin")), "01_qc")
})

test_that("invalid and empty prefix maps cannot mutate a directory", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("original", file.path(path, "01_qc.R"))
    before <- overwrite_snapshot(path)
    for (bad in list(NA_character_, "", 2)) {
        expect_error(renumber(path, "01", bad), "Prefixes")
        expect_identical(overwrite_snapshot(path), before)
    }
    expect_no_error(renumber(path, character(), character(), recursive_edits = TRUE))
    expect_identical(overwrite_snapshot(path), before)
    expect_no_error(renumber(path, "09", "10", expect_matches = FALSE))
    expect_identical(overwrite_snapshot(path), before)
})
