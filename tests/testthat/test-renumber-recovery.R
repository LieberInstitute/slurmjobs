recovery_fixture <- function() {
    path <- normalizePath(withr::local_tempdir(.local_envir = parent.frame()))
    for (stem in c("01_qc", "02_model")) {
        dir.create(file.path(path, stem))
        writeLines(paste0("data for ", stem), file.path(path, stem, "result.bin"))
        writeBin(charToRaw(paste0("x <- '", stem, "'; y <- 'temp_slurmjobs'\r\n")),
            file.path(path, stem, "script.R"))
    }
    writeBin(charToRaw("x <- '01_qc'; y <- '02_model'"), file.path(path, "references.py"))
    path
}

run_recovery_fixture <- function(path) {
    renumber(path, c("01", "02"), c("02", "01"), recursive_edits = TRUE)
}

test_that("native separator paths support recursive edits and rollback", {
    for (fail in c(FALSE, TRUE)) {
        # On Windows this deliberately supplies backslashes, as in the vignette.
        path <- normalizePath(withr::local_tempdir())
        source <- file.path(path, "01_qc.R")
        writeLines("01_qc original", source)
        before <- overwrite_snapshot(path)
        if (fail) {
            move <- .renumber_move
            count <- 0L
            error <- with_mocked_bindings(
                tryCatch(renumber(path, "01", "02", recursive_edits = TRUE), error = identity),
                .renumber_move = function(from, to) {
                    count <<- count + 1L
                    if (count == 1L) stop("injected native-path move failure")
                    move(from, to)
                })
            expect_s3_class(error, "slurmjobs_rename_error")
            expect_true(error$recovered)
            expect_identical(overwrite_snapshot(path), before)
        } else {
            expect_no_error(renumber(path, "01", "02", recursive_edits = TRUE))
            expect_identical(readLines(file.path(path, "02_qc.R")), "02_qc original")
            expect_false(file.exists(source))
        }
        expect_false(dir.exists(file.path(path, ".slurmjobs-renumber")))
    }
})

test_that("recovery accepts native paths saved in a journal", {
    path <- normalizePath(withr::local_tempdir())
    directory <- file.path(path, ".slurmjobs-renumber")
    dir.create(directory)
    source <- file.path(path, "01_qc.R")
    writeLines("original", source)
    saveRDS(list(format = 2L, base_dir = path, status = "publishing",
        prepared = character(), backups = character(), operations = list()),
        file.path(directory, "journal.rds"))
    expect_no_error(renumber_recover(directory))
    expect_false(dir.exists(directory))
    expect_identical(readLines(source), "original")
})

test_that("prepared edits preserve line endings and substitute swaps simultaneously", {
    path <- recovery_fixture()
    expect_no_error(run_recovery_fixture(path))
    expect_identical(readBin(file.path(path, "02_qc", "script.R"), "raw", n = 1000),
        charToRaw("x <- '02_qc'; y <- 'temp_slurmjobs'\r\n"))
    expect_identical(readBin(file.path(path, "references.py"), "raw", n = 1000),
        charToRaw("x <- '02_qc'; y <- '01_model'"))
    expect_identical(readLines(file.path(path, "01_model", "result.bin")), "data for 02_model")
    expect_false(dir.exists(file.path(path, ".slurmjobs-renumber")))
    expect_identical(.renumber_edit_bytes(charToRaw("01_qc 02_qc"),
        c("01_qc" = "02_qc", "02_qc" = "01_qc")), charToRaw("02_qc 01_qc"))
})

test_that("every publication move can fail without changing original paths or bytes", {
    # Scripts are edited in place, followed by two directory moves per phase.
    # Exercise errors both before and after the underlying rename.
    for (after_move in c(FALSE, TRUE)) {
        for (fail_at in seq_len(4L)) {
            path <- recovery_fixture()
            before <- overwrite_snapshot(path)
            count <- 0L
            move <- .renumber_move
            error <- with_mocked_bindings(
                tryCatch(run_recovery_fixture(path), error = identity),
                .renumber_move = function(from, to) {
                    count <<- count + 1L
                    if (count == fail_at && !after_move) stop("injected move failure")
                    move(from, to)
                    if (count == fail_at && after_move) stop("injected move failure")
                }
            )
            expect_s3_class(error, "slurmjobs_rename_error")
            expect_true(error$recovered)
            expect_identical(overwrite_snapshot(path), before)
        }
    }
})

test_that("preparation failures leave originals unchanged", {
    path <- recovery_fixture()
    before <- overwrite_snapshot(path)
    count <- 0L
    edit <- .renumber_edit_bytes
    local_mocked_bindings(.renumber_edit_bytes = function(...) {
        count <<- count + 1L
        if (count == 2L) stop("injected preparation failure")
        edit(...)
    })
    expect_error(run_recovery_fixture(path), "changes from this call were rolled back")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("journal write failures before and after publication are recoverable", {
    for (fail_at in 2:13) {
        path <- recovery_fixture()
        before <- overwrite_snapshot(path)
        save <- .renumber_save_journal
        count <- 0L
        err <- with_mocked_bindings(
            tryCatch(run_recovery_fixture(path), error = identity),
            .renumber_save_journal = function(...) {
                count <<- count + 1L
                if (count == fail_at) stop("injected journal write failure")
                save(...)
            }
        )
        expect_s3_class(err, "slurmjobs_rename_error")
        expect_true(err$recovered)
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("R interrupts trigger rollback", {
    path <- recovery_fixture()
    before <- overwrite_snapshot(path)
    move <- .renumber_move
    count <- 0L
    err <- with_mocked_bindings(
        tryCatch(run_recovery_fixture(path), error = identity),
        .renumber_move = function(from, to) {
            count <<- count + 1L
            move(from, to)
            if (count == 3L) stop(structure(list(message = "interrupted", call = NULL),
                class = c("interrupt", "condition")))
        }
    )
    expect_true(err$recovered)
    expect_identical(overwrite_snapshot(path), before)
})

test_that("failed rollback retains originals and can be retried", {
    path <- recovery_fixture()
    before <- overwrite_snapshot(path)
    move <- .renumber_move
    count <- 0L
    err <- with_mocked_bindings(
        tryCatch(run_recovery_fixture(path), error = identity),
        .renumber_move = function(from, to) {
            count <<- count + 1L
            if (count >= 3L) stop("injected persistent failure")
            move(from, to)
        }
    )
    expect_false(err$recovered)
    expect_true(file.exists(file.path(err$recovery_dir, "journal.rds")))
    expect_error(run_recovery_fixture(path), "unfinished renumber")
    expect_no_error(renumber_recover(err$recovery_dir))
    expect_identical(overwrite_snapshot(path), before)
})

test_that("recovery conflicts preserve competing files and all recovery material", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("original", file.path(path, "01_qc.R"))
    move <- .renumber_move
    count <- 0L
    err <- with_mocked_bindings(
        tryCatch(renumber(path, "01", "02"), error = identity),
        .renumber_move = function(from, to) {
            count <<- count + 1L
            if (count == 2L) {
                writeLines("concurrent writer", file.path(path, "01_qc.R"))
                stop("injected failure")
            }
            move(from, to)
        }
    )
    expect_false(err$recovered)
    expect_identical(readLines(file.path(path, "01_qc.R")), "concurrent writer")
    expect_identical(readLines(file.path(path, "02_qc.Rtemp_slurmjobs")), "original")
    expect_error(renumber_recover(err$recovery_dir), "Recovery conflict")
    # Resolve the conflict by preserving the competing file under another name.
    expect_true(file.rename(file.path(path, "01_qc.R"), file.path(path, "other.R")))
    expect_no_error(renumber_recover(err$recovery_dir))
    expect_identical(readLines(file.path(path, "01_qc.R")), "original")
    expect_identical(readLines(file.path(path, "other.R")), "concurrent writer")
})

test_that("a destination arriving before a move is preserved", {
    # Match renumber()'s destination spelling on Windows so injection is reached.
    path <- normalizePath(withr::local_tempdir(), winslash = "/")
    writeLines("original", file.path(path, "01_qc.R"))
    move <- .renumber_move
    target <- file.path(path, "02_qc.R")
    injected <- FALSE
    err <- with_mocked_bindings(
        tryCatch(renumber(path, "01", "02"), error = identity),
        .renumber_move = function(from, to) {
            if (identical(to, target)) {
                injected <<- TRUE
                writeLines("concurrent writer", target)
            }
            move(from, to)
        }
    )
    expect_true(injected)
    expect_s3_class(err, "slurmjobs_rename_error")
    expect_true(err$recovered)
    expect_identical(readLines(target), "concurrent writer")
    expect_identical(readLines(file.path(path, "01_qc.R")), "original")
})

test_that("changed inputs abort publication and linked edit targets are refused", {
    path <- recovery_fixture()
    prepare <- .renumber_prepare
    local_mocked_bindings(.renumber_prepare = function(...) {
        result <- prepare(...)
        writeLines("concurrent edit", file.path(path, "references.py"))
        result
    })
    expect_error(run_recovery_fixture(path), "Inputs changed")
    expect_identical(readLines(file.path(path, "references.py")), "concurrent edit")
    expect_true(dir.exists(file.path(path, "01_qc")))
})

test_that("linked scripts are rejected before publication", {
    skip_on_os("windows")
    for (hard in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        writeLines("01_qc", file.path(path, "original.txt"))
        link <- if (hard) file.link else file.symlink
        expect_true(link(file.path(path, "original.txt"), file.path(path, "01_qc.R")))
        before <- overwrite_snapshot(path)
        expect_error(renumber(path, "01", "02", recursive_edits = TRUE), "linked or non-regular")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("directory renames retain data identity without copying it", {
    path <- recovery_fixture()
    source <- file.path(path, "01_qc", "result.bin")
    before <- .renumber_stamp(source)
    expect_no_error(run_recovery_fixture(path))
    expect_identical(.renumber_stamp(file.path(path, "02_qc", "result.bin")), before)
})

test_that("a journal update interrupted at commit never reports rollback", {
    path <- recovery_fixture()
    save <- .renumber_save_journal
    err <- with_mocked_bindings(
        tryCatch(run_recovery_fixture(path), error = identity),
        .renumber_save_journal = function(journal, directory) {
            save(journal, directory)
            if (journal$status == "committed") stop("injected error just after commit")
        }
    )
    expect_match(conditionMessage(err), "completed, but cleanup failed")
    expect_true(dir.exists(file.path(path, "02_qc")))
    expect_no_error(renumber_recover(file.path(path, ".slurmjobs-renumber")))
    expect_identical(readLines(file.path(path, "02_qc", "result.bin")), "data for 01_qc")
})

test_that("recovery rejects changed staged sources", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("original", file.path(path, "01_qc.R"))
    move <- .renumber_move
    count <- 0L
    err <- with_mocked_bindings(
        tryCatch(renumber(path, "01", "02"), error = identity),
        .renumber_move = function(from, to) {
            count <<- count + 1L
            if (count >= 2L) stop("injected failure")
            move(from, to)
        }
    )
    staged <- file.path(path, "02_qc.Rtemp_slurmjobs")
    writeLines("modified after interruption", staged)
    expect_error(renumber_recover(err$recovery_dir), "Recovery conflict")
    expect_identical(readLines(staged), "modified after interruption")
    expect_true(file.exists(file.path(err$recovery_dir, "journal.rds")))
})

test_that("recovery can read a journal interrupted during rotation", {
    for (generation in c("journal-next.rds", "journal-previous.rds")) {
        path <- normalizePath(withr::local_tempdir())
        writeLines("original", file.path(path, "01_qc.R"))
        move <- .renumber_move
        count <- 0L
        err <- with_mocked_bindings(
            tryCatch(renumber(path, "01", "02"), error = identity),
            .renumber_move = function(from, to) {
                count <<- count + 1L
                if (count >= 2L) stop("injected failure")
                move(from, to)
            }
        )
        journal <- file.path(err$recovery_dir, "journal.rds")
        alternate <- file.path(err$recovery_dir, generation)
        if (file.exists(alternate)) expect_true(file.remove(alternate))
        expect_true(file.rename(journal, alternate))
        expect_no_error(renumber_recover(err$recovery_dir))
        expect_identical(readLines(file.path(path, "01_qc.R")), "original")
    }
})

test_that("empty recovery folders and recursive directory links are handled safely", {
    path <- normalizePath(withr::local_tempdir())
    directory <- file.path(path, ".slurmjobs-renumber")
    dir.create(directory)
    expect_no_error(renumber_recover(directory))
    expect_false(dir.exists(directory))
    skip_on_os("windows")
    writeLines("01_qc", file.path(path, "01_qc.R"))
    expect_true(file.symlink(path, file.path(path, "cycle")))
    before <- overwrite_snapshot(path)
    expect_error(renumber(path, "01", "02", recursive_edits = TRUE), "linked directory")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("a fresh R session can recover abrupt process termination", {
    # quit() bypasses the transaction's error/interrupt handlers entirely.
    for (terminate_at in seq_len(4L)) {
        path <- recovery_fixture()
        before <- overwrite_snapshot(path)
        terminated <- run_recovery_process(function(path, terminate_at) {
            library(slurmjobs)
            namespace <- asNamespace("slurmjobs")
            move <- get(".renumber_move", namespace)
            count <- 0L
            unlockBinding(".renumber_move", namespace)
            assign(".renumber_move", function(from, to) {
                move(from, to)
                count <<- count + 1L
                if (count == terminate_at) quit(save = "no", status = 23L, runLast = FALSE)
            }, namespace)
            lockBinding(".renumber_move", namespace)
            renumber(path, c("01", "02"), c("02", "01"), recursive_edits = TRUE)
        }, list(path = path, terminate_at = terminate_at))
        expect_identical(terminated$status, 23L, info = paste(terminated$output, collapse = "\n"))
        recovery_dir <- file.path(path, ".slurmjobs-renumber")
        expect_true(dir.exists(recovery_dir))
        recovered <- run_recovery_process(function(directory) {
            slurmjobs::renumber_recover(directory)
        }, list(directory = recovery_dir))
        expect_identical(recovered$status, 0L, info = paste(recovered$output, collapse = "\n"))
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("nested logs and edited scripts roll back together", {
    path <- normalizePath(withr::local_tempdir())
    overwrite_fixture(path)
    before <- overwrite_snapshot(path)
    move <- .renumber_move
    count <- 0L
    error <- with_mocked_bindings(
        tryCatch(renumber(file.path(path, "code"), "01", "02", recursive_edits = TRUE),
            error = identity),
        .renumber_move = function(from, to) {
            count <<- count + 1L
            move(from, to)
            if (count == 5L) stop("injected failure after script and log staging")
        }
    )
    expect_true(error$recovered)
    expect_identical(overwrite_snapshot(path), before)
})

test_that("partial script writes roll back bytes while retaining file identity", {
    for (fail_at in 1:3) {
        for (length in c(0L, 1L, 1000L)) {
            path <- recovery_fixture()
            before <- overwrite_snapshot(path)
            scripts <- .renumber_code_files(path)
            identities <- lapply(scripts, function(x) .renumber_stamp(x)[c("type", "device_id", "inode")])
            write <- .renumber_write_bytes
            count <- 0L
            error <- with_mocked_bindings(
                tryCatch(run_recovery_fixture(path), error = identity),
                .renumber_write_bytes = function(path, bytes) {
                    count <<- count + 1L
                    if (count == fail_at) {
                        write(path, head(bytes, length))
                        stop("injected partial script write")
                    }
                    write(path, bytes)
                }
            )
            expect_true(error$recovered)
            expect_identical(overwrite_snapshot(path), before)
            expect_identical(lapply(scripts, function(x) .renumber_stamp(x)[c("type", "device_id", "inode")]),
                identities)
        }
    }
})

test_that("script editing retains inode ownership permissions and unmodified bytes", {
    path <- normalizePath(withr::local_tempdir())
    source <- file.path(path, "01_qc.R")
    writeBin(charToRaw("01_qc\r\n"), source)
    expect_true(Sys.chmod(source, "0750"))
    fields <- c("device_id", "inode", "permissions", "user", "group")
    before <- fs::file_info(source)[fields]
    expect_no_error(renumber(path, "01", "02", recursive_edits = TRUE))
    target <- file.path(path, "02_qc.R")
    expect_identical(fs::file_info(target)[fields], before)
    expect_identical(.renumber_read_bytes(target), charToRaw("02_qc\r\n"))
})

test_that("shorter and longer edits truncate the existing file correctly", {
    for (after in c("2", "00002")) {
        path <- normalizePath(withr::local_tempdir())
        writeBin(charToRaw("01_qc"), file.path(path, "01_qc.R"))
        expect_no_error(renumber(path, "01", after, recursive_edits = TRUE))
        expect_identical(.renumber_read_bytes(file.path(path, paste0(after, "_qc.R"))),
            charToRaw(paste0(after, "_qc")))
    }
})

test_that("restoration can be interrupted and retried without replacing the file", {
    path <- recovery_fixture()
    before <- overwrite_snapshot(path)
    write <- .renumber_write_bytes
    count <- 0L
    error <- with_mocked_bindings(
        tryCatch(run_recovery_fixture(path), error = identity),
        .renumber_write_bytes = function(path, bytes) {
            count <<- count + 1L
            if (count >= 2L) {
                write(path, head(bytes, 1L))
                stop("injected partial write and restoration failure")
            }
            write(path, bytes)
        }
    )
    expect_false(error$recovered)
    count <- 0L
    with_mocked_bindings(
        expect_error(renumber_recover(error$recovery_dir), "interrupted restoration"),
        .renumber_write_bytes = function(path, bytes) {
            count <<- count + 1L
            write(path, head(bytes, 2L))
            stop("interrupted restoration")
        }
    )
    expect_no_error(renumber_recover(error$recovery_dir))
    expect_identical(overwrite_snapshot(path), before)
})

test_that("corrupted script backups and replacement file identities block recovery", {
    for (conflict in c("backup", "identity")) {
        path <- normalizePath(withr::local_tempdir())
        source <- file.path(path, "01_qc.R")
        writeLines("01_qc original script", source)
        write <- .renumber_write_bytes
        error <- with_mocked_bindings(
            tryCatch(renumber(path, "01", "02", recursive_edits = TRUE), error = identity),
            .renumber_write_bytes = function(path, bytes) {
                write(path, head(bytes, 1L))
                stop("persistent write failure")
            }
        )
        expect_false(error$recovered)
        if (conflict == "backup") {
            writeBin(charToRaw("corrupted backup"), file.path(error$recovery_dir, "original-1"))
            expect_error(renumber_recover(error$recovery_dir), "backup is missing or changed")
        } else {
            expect_true(file.rename(source, file.path(path, "saved-original")))
            writeLines("unrelated replacement file", source)
            expect_error(renumber_recover(error$recovery_dir), "identity or links changed")
            expect_identical(readLines(source), "unrelated replacement file")
        }
        expect_true(file.exists(file.path(error$recovery_dir, "journal.rds")))
    }
})

test_that("completed script edits with external content changes block recovery", {
    path <- recovery_fixture()
    record <- .renumber_record_edit
    error <- with_mocked_bindings(
        tryCatch(run_recovery_fixture(path), error = identity),
        .renumber_record_edit = function(state, edit) {
            record(state, edit)
            writeLines("external change to completed edit", edit$path)
            stop("injected failure after external edit")
        }
    )
    expect_false(error$recovered)
    expect_error(renumber_recover(error$recovery_dir), "completed script edit changed")
    expect_identical(readLines(file.path(path, "01_qc", "script.R")),
        "external change to completed edit")
})

test_that("a fresh process recovers a script terminated during its content write", {
    for (partial in c(0L, 1L, 1000L)) {
        path <- recovery_fixture()
        before <- overwrite_snapshot(path)
        terminated <- run_recovery_process(function(path, partial) {
            library(slurmjobs)
            namespace <- asNamespace("slurmjobs")
            write <- get(".renumber_write_bytes", namespace)
            unlockBinding(".renumber_write_bytes", namespace)
            assign(".renumber_write_bytes", function(path, bytes) {
                write(path, head(bytes, partial))
                quit(save = "no", status = 23L, runLast = FALSE)
            }, namespace)
            lockBinding(".renumber_write_bytes", namespace)
            renumber(path, c("01", "02"), c("02", "01"), recursive_edits = TRUE)
        }, list(path = path, partial = partial))
        expect_identical(terminated$status, 23L, info = paste(terminated$output, collapse = "\n"))
        recovery_dir <- file.path(path, ".slurmjobs-renumber")
        recovered <- run_recovery_process(function(directory) {
            slurmjobs::renumber_recover(directory)
        }, list(directory = recovery_dir))
        expect_identical(recovered$status, 0L, info = paste(recovered$output, collapse = "\n"))
        expect_identical(overwrite_snapshot(path), before)
    }
})
