scope_fixture <- function() {
    root <- normalizePath(withr::local_tempdir(.local_envir = parent.frame()))
    base <- file.path(root, "code")
    outside <- file.path(root, "code-other") # must not pass a plain prefix check
    dir.create(base)
    dir.create(outside)
    testthat::local_mocked_bindings(.renumber_project_root = function() base,
        .package = "slurmjobs", .env = parent.frame())
    writeLines("01_qc original", file.path(base, "01_qc.R"))
    writeLines("01_qc private log", file.path(outside, "01_qc.txt"))
    list(root = root, base = base, outside = outside)
}

test_that("absolute and relative outside logs stop before any changes", {
    for (recursive in c(FALSE, TRUE)) {
        for (directive in c("#SBATCH -o ", "log_path=")) {
            for (absolute in c(FALSE, TRUE)) {
                fixture <- scope_fixture()
                log <- if (absolute) file.path(fixture$outside, "01_qc.txt") else "../code-other/01_qc.txt"
                writeLines(c(paste0(directive, log), "Rscript 01_qc.R"),
                    file.path(fixture$base, "01_qc.sh"))
                before <- overwrite_snapshot(fixture$root)
                expect_error(renumber(fixture$base, "01", "02", recursive_edits = recursive),
                    "outside")
                expect_identical(overwrite_snapshot(fixture$root), before)
            }
        }
    }
})

test_that("empty and nonexistent outside log directories do not block changes", {
    for (missing in c(FALSE, TRUE)) {
        fixture <- scope_fixture()
        directory <- file.path(fixture$outside, "missing", "logs")
        if (!missing) dir.create(directory, recursive = TRUE)
        writeLines(paste("#SBATCH -o", file.path(directory, "01_qc.txt")),
            file.path(fixture$base, "01_qc.sh"))
        before <- overwrite_snapshot(fixture$outside)
        expect_no_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE))
        expect_identical(overwrite_snapshot(fixture$outside), before)
        expect_true(file.exists(file.path(fixture$base, "02_qc.R")))
    }
})

test_that("internal logs and scripts without log directives remain supported", {
    for (absolute in c(FALSE, TRUE)) {
        fixture <- scope_fixture()
        logs <- file.path(fixture$base, "logs")
        dir.create(logs)
        writeLines("original log", file.path(logs, "01_qc.txt"))
        log <- if (absolute) file.path(logs, "01_qc.txt") else "logs/01_qc.txt"
        writeLines(paste("#SBATCH -o", log), file.path(fixture$base, "01_qc.sh"))
        outside_before <- overwrite_snapshot(fixture$outside)
        expect_no_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE))
        expect_identical(readLines(file.path(logs, "02_qc.txt")), "original log")
        expect_identical(overwrite_snapshot(fixture$outside), outside_before)
    }
    fixture <- scope_fixture()
    writeLines("Rscript 01_qc.R", file.path(fixture$base, "01_qc.sh"))
    expect_no_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE))
    expect_identical(readLines(file.path(fixture$base, "02_qc.sh")), "Rscript 02_qc.R")
})

test_that("symbolic links cannot redirect selected scripts directories or logs outside", {
    skip_on_os("windows")
    for (kind in c("R", "sh", "directory", "logs")) {
        for (recursive in c(FALSE, TRUE)) {
            fixture <- scope_fixture()
            source <- file.path(fixture$base, "01_qc.R")
            if (kind %in% c("R", "sh")) {
                file.remove(source)
                target <- file.path(fixture$outside, paste0("01_qc.", kind))
                writeLines("01_qc protected", target)
                expect_true(file.symlink(target, file.path(fixture$base, paste0("01_qc.", kind))))
            } else if (kind == "directory") {
                file.remove(source)
                expect_true(file.symlink(fixture$outside, file.path(fixture$base, "01_qc")))
            } else {
                expect_true(file.symlink(fixture$outside, file.path(fixture$base, "logs")))
                log <- if (kind == "logs") "logs/01_qc.txt" else "logs/not-created/01_qc.txt"
                writeLines(paste("#SBATCH -o", log), file.path(fixture$base, "01_qc.sh"))
            }
            before <- overwrite_snapshot(fixture$root)
            expect_error(renumber(fixture$base, "01", "02", recursive_edits = recursive),
                "outside|linked directory")
            expect_identical(overwrite_snapshot(fixture$root), before)
        }
    }
})

test_that("recursive edits cannot reach external scripts through links or hard links", {
    skip_on_os("windows")
    for (kind in c("symbolic", "hard", "directory")) {
        fixture <- scope_fixture()
        target <- file.path(fixture$outside, "references.py")
        writeLines("01_qc private references", target)
        if (kind == "directory") {
            expect_true(file.symlink(fixture$outside, file.path(fixture$base, "references")))
        } else {
            link <- if (kind == "symbolic") file.symlink else file.link
            expect_true(link(target, file.path(fixture$base, "references.py")))
        }
        before <- overwrite_snapshot(fixture$root)
        expect_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE),
            "outside|linked")
        expect_identical(overwrite_snapshot(fixture$root), before)
    }
})

test_that("selected hard linked files are rejected even without content edits", {
    fixture <- scope_fixture()
    file.remove(file.path(fixture$base, "01_qc.R"))
    target <- file.path(fixture$outside, "original.R")
    writeLines("01_qc private script", target)
    skip_if_not(file.link(target, file.path(fixture$base, "01_qc.R")), "Hard links unavailable")
    before <- overwrite_snapshot(fixture$root)
    expect_error(renumber(fixture$base, "01", "02"), "linked")
    expect_identical(overwrite_snapshot(fixture$root), before)
})

test_that("base_dir aliases establish the physical root and lexical traversal stays bounded", {
    skip_on_os("windows")
    fixture <- scope_fixture()
    alias <- file.path(fixture$root, "base-alias")
    expect_true(file.symlink(fixture$base, alias))
    outside_before <- overwrite_snapshot(fixture$outside)
    expect_no_error(renumber(alias, "01", "02", recursive_edits = TRUE))
    expect_true(file.exists(file.path(fixture$base, "02_qc.R")))
    expect_identical(overwrite_snapshot(fixture$outside), outside_before)

    fixture <- scope_fixture()
    # lexically looks like code/01_qc.txt, but logs/.. resolves to code-other.
    nested <- file.path(fixture$outside, "nested")
    dir.create(nested)
    expect_true(file.symlink(nested, file.path(fixture$base, "logs")))
    writeLines("#SBATCH -o logs/../01_qc.txt", file.path(fixture$base, "01_qc.sh"))
    before <- overwrite_snapshot(fixture$root)
    expect_error(renumber(fixture$base, "01", "02"), "outside")
    expect_identical(overwrite_snapshot(fixture$root), before)
})

test_that("relative dot components in internal log directories remain supported", {
    fixture <- scope_fixture()
    logs <- file.path(fixture$base, "logs")
    dir.create(logs)
    writeLines("original log", file.path(logs, "01_qc.txt"))
    writeLines("#SBATCH -o logs/../logs/01_qc.txt", file.path(fixture$base, "01_qc.sh"))
    expect_no_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE))
    expect_identical(readLines(file.path(logs, "02_qc.txt")), "original log")
})

test_that("recovery refuses journal operations or backups outside its original scope", {
    for (field in c("move", "edit", "backup", "committed")) {
        fixture <- scope_fixture()
        directory <- file.path(fixture$base, ".slurmjobs-renumber")
        dir.create(directory)
        original <- file.path(fixture$outside, "01_qc.txt")
        operation <- if (field == "edit") {
            list(kind = "edit", path = original,
                prepared = file.path(directory, "prepared-1"), backup = file.path(directory, "original-1"))
        } else {
            list(kind = "move", from = original, to = file.path(fixture$outside, "02_qc.txt"),
                stamp = .renumber_stamp(original))
        }
        journal <- list(format = 2L, base_dir = fixture$base,
            status = if (field == "committed") "committed" else "publishing",
            prepared = character(), backups = if (field == "backup") original else character(),
            operations = if (field == "backup") list() else list(operation))
        saveRDS(journal, file.path(directory, "journal.rds"))
        before <- overwrite_snapshot(fixture$root)
        expect_error(renumber_recover(directory), "outside")
        expect_identical(overwrite_snapshot(fixture$root), before)
    }
})

test_that("journal symlinks and hard links cannot redirect metadata writes", {
    skip_on_os("windows")
    for (hard in c(FALSE, TRUE)) {
        fixture <- scope_fixture()
        directory <- file.path(fixture$base, ".slurmjobs-renumber")
        dir.create(directory)
        target <- file.path(fixture$outside, "private.rds")
        saveRDS(list(secret = "irreplaceable outside data"), target)
        link <- if (hard) file.link else file.symlink
        expect_true(link(target, file.path(directory, "journal.rds")))
        before <- overwrite_snapshot(fixture$root)
        expect_error(renumber_recover(directory), "outside|linked")
        expect_identical(overwrite_snapshot(fixture$root), before)
    }
})

test_that("recovery refuses a redirected parent without touching outside files", {
    skip_on_os("windows")
    fixture <- scope_fixture()
    nested <- file.path(fixture$base, "logs")
    dir.create(nested)
    original <- file.path(nested, "01_qc.txt")
    writeLines("original log", original)
    directory <- file.path(fixture$base, ".slurmjobs-renumber")
    dir.create(directory)
    stamp <- .renumber_stamp(original)
    expect_true(file.rename(original, file.path(nested, "02_qc.txttemp_slurmjobs")))
    journal <- list(format = 2L, base_dir = fixture$base, status = "publishing",
        prepared = character(), backups = character(), operations = list(list(kind = "move",
            from = original, to = file.path(nested, "02_qc.txttemp_slurmjobs"), stamp = stamp)))
    saveRDS(journal, file.path(directory, "journal.rds"))
    expect_true(file.rename(nested, file.path(fixture$base, "saved-logs")))
    writeLines("outside staged lookalike", file.path(fixture$outside, "02_qc.txttemp_slurmjobs"))
    expect_true(file.symlink(fixture$outside, nested))
    before <- overwrite_snapshot(fixture$root)
    expect_error(renumber_recover(directory), "outside")
    expect_identical(overwrite_snapshot(fixture$root), before)
})

test_that("moving a recovery directory cannot broaden its recorded scope", {
    fixture <- scope_fixture()
    directory <- file.path(fixture$base, ".slurmjobs-renumber")
    dir.create(directory)
    saveRDS(list(format = 2L, base_dir = fixture$base, status = "publishing",
        prepared = character(), backups = character(), operations = list()),
        file.path(directory, "journal.rds"))
    moved <- file.path(fixture$outside, ".slurmjobs-renumber")
    expect_true(file.rename(directory, moved))
    before <- overwrite_snapshot(fixture$root)
    expect_error(renumber_recover(moved), "moved or linked")
    expect_identical(overwrite_snapshot(fixture$root), before)
})

test_that("individual log links cannot alias outside files", {
    skip_on_os("windows")
    for (hard in c(FALSE, TRUE)) {
        fixture <- scope_fixture()
        logs <- file.path(fixture$base, "logs")
        dir.create(logs)
        link <- if (hard) file.link else file.symlink
        expect_true(link(file.path(fixture$outside, "01_qc.txt"), file.path(logs, "01_qc.txt")))
        writeLines("#SBATCH -o logs/01_qc.txt", file.path(fixture$base, "01_qc.sh"))
        before <- overwrite_snapshot(fixture$root)
        expect_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE),
            "outside|linked")
        expect_identical(overwrite_snapshot(fixture$root), before)
    }
})
