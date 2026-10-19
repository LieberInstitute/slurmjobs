project_log_fixture <- function(log_root) {
    overwrite_fixture(log_root, file.path(log_root, "logs"))
    list(project = log_root, base = file.path(log_root, "code"), logs = file.path(log_root, "logs"))
}

test_that("directories cannot be renamed through the log exception", {
    for (protect in c(FALSE, TRUE)) {
        for (external in c(FALSE, TRUE)) {
            for (extension in c("txt", "log")) {
                path <- normalizePath(withr::local_tempdir())
                logs <- if (external) file.path(path, "private-data") else file.path(path, "code", "logs")
                overwrite_fixture(path, logs)
                file.remove(file.path(logs, "01_qc.txt"))
                target <- file.path(logs, paste0("01_qc.", extension))
                dir.create(target)
                writeLines("unrelated project data", file.path(target, "sentinel.txt"))
                writeLines("01_qc must remain unchanged", file.path(target, "references.R"))
                writeLines(c(paste("#SBATCH -o", target), "Rscript 01_qc.R"),
                    file.path(path, "code", "01_qc.sh"))
                before <- overwrite_snapshot(path)
                expect_error(renumber(file.path(path, "code"), "01", "02",
                    recursive_edits = TRUE, protect_log_location = protect),
                    "Log targets must be regular files")
                expect_identical(overwrite_snapshot(path), before)
                expect_false(dir.exists(file.path(path, "code", ".slurmjobs-renumber")))
            }
        }
    }
})

test_that("recovery cannot move directories recorded as logs", {
    for (protect in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        fixture <- project_log_fixture(path)
        target <- file.path(fixture$logs, "01_qc.txt")
        file.remove(target)
        dir.create(target)
        writeLines("unrelated data", file.path(target, "sentinel.txt"))
        directory <- file.path(fixture$base, ".slurmjobs-renumber")
        dir.create(directory)
        saveRDS(list(format = 2L, base_dir = fixture$base,
            log_policy = list(protect = protect, root = path), status = "publishing",
            prepared = character(), backups = character(),
            operations = list(list(kind = "move", is_log = TRUE,
                from = file.path(fixture$logs, "02_qc.txt"), to = target,
                stamp = .renumber_stamp(target)))), file.path(directory, "journal.rds"))
        before <- overwrite_snapshot(path)
        expect_error(renumber_recover(directory), "Log targets must be regular files")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("logs outside base_dir but inside here project are renamed", {
    for (recursive in c(FALSE, TRUE)) {
        for (relative in c(FALSE, TRUE)) {
            for (directive in c("#SBATCH -o ", "log_path=")) {
                path <- normalizePath(withr::local_tempdir())
                fixture <- project_log_fixture(path)
                # Fixture helpers mock only root discovery; production calls here::here().
                local_mocked_bindings(.renumber_project_root = function() fixture$project)
                log <- if (relative) "../logs/01_qc.txt" else file.path(fixture$logs, "01_qc.txt")
                writeLines(c(paste0(directive, log), "Rscript 01_qc.R"),
                    file.path(fixture$base, "01_qc.sh"))
                writeLines("01_qc unrelated external code", file.path(fixture$logs, "references.R"))
                expect_no_error(renumber(fixture$base, "01", "02", recursive_edits = recursive))
                expect_identical(readLines(file.path(fixture$logs, "02_qc.txt")), "original log")
                expect_false(file.exists(file.path(fixture$logs, "01_qc.txt")))
                expect_identical(readLines(file.path(fixture$logs, "references.R")), "01_qc unrelated external code")
            }
        }
    }
})

test_that("protected logs outside project fail before any changes", {
    path <- normalizePath(withr::local_tempdir())
    fixture <- project_log_fixture(path)
    # The code folder itself defines the project in this case.
    local_mocked_bindings(.renumber_project_root = function() fixture$base)
    before <- overwrite_snapshot(path)
    expect_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE),
        "outside the project directory returned by here::here")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("FALSE allows arbitrary log locations and does not call here", {
    path <- normalizePath(withr::local_tempdir())
    fixture <- project_log_fixture(path)
    local_mocked_bindings(.renumber_project_root = function() stop("Must not discover a project"))
    expect_no_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE,
        protect_log_location = FALSE))
    expect_identical(readLines(file.path(fixture$logs, "02_qc.txt")), "original log")
    expect_identical(readLines(file.path(fixture$base, "02_qc.R")), 'x <- "02_qc"')
})

test_that("FALSE does not relax non-log edit or rename boundaries", {
    skip_on_os("windows")
    for (recursive in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        fixture <- project_log_fixture(path)
        target <- file.path(path, "outside.py")
        writeLines("01_qc protected script", target)
        file.remove(file.path(fixture$base, "01_qc.R"))
        expect_true(file.symlink(target, file.path(fixture$base, "01_qc.R")))
        before <- overwrite_snapshot(path)
        expect_error(renumber(fixture$base, "01", "02", recursive_edits = recursive,
            protect_log_location = FALSE), "outside 'base_dir'")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("log protection option must be a single non-missing logical", {
    path <- normalizePath(withr::local_tempdir())
    writeLines("01_qc", file.path(path, "01_qc.R"))
    before <- overwrite_snapshot(path)
    for (bad in list(NA, NULL, c(TRUE, FALSE), "FALSE", 0)) {
        expect_error(renumber(path, "01", "02", protect_log_location = bad), "must be TRUE or FALSE")
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("project scope is checked for logs even if base_dir itself is outside it", {
    path <- normalizePath(withr::local_tempdir())
    fixture <- project_log_fixture(path)
    other_project <- file.path(path, "other-project")
    dir.create(other_project)
    local_mocked_bindings(.renumber_project_root = function() other_project)
    before <- overwrite_snapshot(path)
    expect_error(renumber(fixture$base, "01", "02"), "outside the project")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("external log rollback uses the saved policy instead of current here context", {
    for (protect in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        fixture <- project_log_fixture(path)
        local_mocked_bindings(.renumber_project_root = function() fixture$project)
        before <- overwrite_snapshot(path)
        move <- .renumber_move
        count <- 0L
        error <- with_mocked_bindings(
            tryCatch(renumber(fixture$base, "01", "02", recursive_edits = TRUE,
                protect_log_location = protect), error = identity),
            .renumber_move = function(from, to) {
                count <<- count + 1L
                if (count >= 3L) stop("injected persistent move failure")
                move(from, to)
            }
        )
        expect_false(error$recovered)
        journal <- .renumber_read_journal(error$recovery_dir)
        expect_identical(journal$log_policy$protect, protect)
        expect_true(any(vapply(journal$operations, function(x) isTRUE(x$is_log), logical(1))))
        with_mocked_bindings(
            expect_no_error(renumber_recover(error$recovery_dir)),
            .renumber_project_root = function() stop("Recovery must not consult here")
        )
        expect_identical(overwrite_snapshot(path), before)
    }
})

test_that("symlinked log parents are checked against physical project root", {
    skip_on_os("windows")
    for (protect in c(FALSE, TRUE)) {
        root <- normalizePath(withr::local_tempdir())
        project <- file.path(root, "project")
        outside <- file.path(root, "project-other")
        dir.create(project)
        dir.create(outside)
        fixture <- project_log_fixture(project)
        unlink(fixture$logs, recursive = TRUE)
        expect_true(file.symlink(outside, fixture$logs))
        writeLines("outside log", file.path(outside, "01_qc.txt"))
        local_mocked_bindings(.renumber_project_root = function() project)
        before <- overwrite_snapshot(root)
        if (protect) {
            expect_error(renumber(fixture$base, "01", "02"), "outside the project")
            expect_identical(overwrite_snapshot(root), before)
        } else {
            expect_no_error(renumber(fixture$base, "01", "02", protect_log_location = FALSE))
            expect_identical(readLines(file.path(outside, "02_qc.txt")), "outside log")
        }
    }
})

test_that("log recovery policy cannot authorize non-log operations outside base_dir", {
    path <- normalizePath(withr::local_tempdir())
    fixture <- project_log_fixture(path)
    directory <- file.path(fixture$base, ".slurmjobs-renumber")
    dir.create(directory)
    outside <- file.path(fixture$logs, "01_qc.txt")
    saveRDS(list(format = 2L, base_dir = fixture$base,
        log_policy = list(protect = FALSE, root = NULL), status = "publishing",
        prepared = character(), backups = character(),
        operations = list(list(kind = "move", is_log = FALSE, from = outside,
            to = file.path(fixture$logs, "02_qc.txt"), stamp = .renumber_stamp(outside)))),
        file.path(directory, "journal.rds"))
    before <- overwrite_snapshot(path)
    expect_error(renumber_recover(directory), "outside 'base_dir'")
    expect_identical(overwrite_snapshot(path), before)
})

test_that("real here heuristics select the project from the session context", {
    for (marker in c(FALSE, TRUE)) {
        path <- normalizePath(withr::local_tempdir())
        fixture <- project_log_fixture(path)
        if (marker) expect_true(file.create(file.path(path, ".here")))
        result <- run_recovery_process(function(project, marker) {
            setwd(file.path(project, "code"))
            library(slurmjobs)
            expected <- if (marker) project else getwd()
            stopifnot(identical(normalizePath(here::here()), normalizePath(expected)))
            if (marker) {
                renumber(getwd(), "01", "02")
            } else {
                error <- tryCatch(renumber(getwd(), "01", "02"), error = identity)
                stopifnot(inherits(error, "error"), grepl("outside the project", conditionMessage(error)))
                renumber(getwd(), "01", "02", protect_log_location = FALSE)
            }
        }, list(project = path, marker = marker))
        expect_identical(result$status, 0L, info = paste(result$output, collapse = "\n"))
        expect_identical(readLines(file.path(fixture$logs, "02_qc.txt")), "original log")
    }
})

test_that("a fresh process in another project can recover saved external log moves", {
    path <- normalizePath(withr::local_tempdir())
    fixture <- project_log_fixture(path)
    local_mocked_bindings(.renumber_project_root = function() fixture$project)
    before <- overwrite_snapshot(path)
    move <- .renumber_move
    count <- 0L
    error <- with_mocked_bindings(
        tryCatch(renumber(fixture$base, "01", "02", recursive_edits = TRUE), error = identity),
        .renumber_move = function(from, to) {
            count <<- count + 1L
            if (count >= 3L) stop("injected persistent move failure")
            move(from, to)
        }
    )
    expect_false(error$recovered)
    other_project <- normalizePath(withr::local_tempdir())
    expect_true(file.create(file.path(other_project, ".here")))
    result <- run_recovery_process(function(directory, project) {
        setwd(project)
        stopifnot(identical(normalizePath(here::here()), normalizePath(project)))
        slurmjobs::renumber_recover(directory)
    }, list(directory = error$recovery_dir, project = other_project))
    expect_identical(result$status, 0L, info = paste(result$output, collapse = "\n"))
    expect_identical(overwrite_snapshot(path), before)
})

test_that("disabling log location protection retains overwrite prevention", {
    path <- normalizePath(withr::local_tempdir())
    fixture <- project_log_fixture(path)
    writeLines("irreplaceable destination log", file.path(fixture$logs, "02_qc.txt"))
    local_mocked_bindings(.renumber_project_root = function() stop("Must not discover a project"))
    before <- overwrite_snapshot(path)
    expect_error(renumber(fixture$base, "01", "02", recursive_edits = TRUE,
        protect_log_location = FALSE), "overwrite an existing destination")
    expect_identical(overwrite_snapshot(path), before)
})
