# Identity and metadata are checked without reading/copying large data files.
.renumber_stamp <- function(path) {
    info <- fs::file_info(path, fail = TRUE)
    as.data.frame(info[c("type", "device_id", "inode", "size", "modification_time")])
}

.renumber_matches <- function(path, stamp) {
    .renumber_path_exists(path) && identical(.renumber_stamp(path), stamp)
}

.renumber_inventory <- function(paths) {
    walk <- function(path) {
        children <- if (dir.exists(path) && !fs::link_exists(path)) {
            list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
        } else character()
        c(path, unlist(lapply(children, walk), use.names = FALSE))
    }
    paths <- sort(unique(unlist(lapply(paths, walk), use.names = FALSE)))
    stats::setNames(lapply(paths, .renumber_stamp), paths)
}

# Replace our own journal via a sibling temporary file. Never alter originals
# unless the intent for their next change has already been saved successfully.
.renumber_save_journal <- function(journal, directory) {
    .renumber_check_journal_paths(journal, directory)
    temporary <- file.path(directory, "journal-next.rds")
    saveRDS(journal, temporary)
    destination <- file.path(directory, "journal.rds")
    # Windows base rename does not replace an existing file. Keep a previous
    # generation until the new journal has been installed successfully.
    previous <- file.path(directory, "journal-previous.rds")
    if (file.exists(previous) && !file.remove(previous)) stop("Cannot rotate recovery journal.")
    if (file.exists(destination) && !file.rename(destination, previous)) {
        stop("Cannot preserve recovery journal.")
    }
    if (!file.rename(temporary, destination)) stop("Cannot save recovery journal.")
    invisible(NULL)
}

.renumber_read_journal <- function(directory) {
    # A fully written next generation includes the intent preceding any move.
    for (name in c("journal.rds", "journal-next.rds", "journal-previous.rds")) {
        path <- file.path(directory, name)
        if (!file.exists(path)) next
        journal <- tryCatch(readRDS(path), error = function(e) NULL)
        if (is.list(journal) && (identical(journal$format, 1L) || identical(journal$format, 2L))) return(journal)
    }
    stop("No readable recovery journal in: ", directory, call. = FALSE)
}

.renumber_move <- function(from, to) {
    if (!.renumber_path_exists(from)) stop("Missing move source: ", from)
    if (.renumber_path_exists(to)) stop("Occupied move destination: ", to)
    # This check and rename are separate: callers must exclude concurrent writers.
    if (!file.rename(from, to)) stop("Could not rename '", from, "' to '", to, "'.")
    invisible(NULL)
}

.renumber_record_move <- function(state, from, to, is_log = FALSE) {
    .renumber_assert_move_paths(c(from, to), is_log, state$journal)
    operation <- list(kind = "move", from = from, to = to, is_log = is_log,
        stamp = .renumber_stamp(from))
    state$journal$operations <- c(state$journal$operations, list(operation))
    .renumber_save_journal(state$journal, state$directory)
    .renumber_move(from, to)
}

.renumber_cleanup <- function(journal, directory) {
    .renumber_check_journal_paths(journal, directory)
    allowed <- c("journal.rds", "journal-next.rds", "journal-previous.rds",
        basename(journal$prepared), basename(journal$backups))
    entries <- list.files(directory, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (any(!basename(entries) %in% allowed) ||
        any(fs::dir_exists(entries) | fs::link_exists(entries))) {
        stop("Unexpected entries in recovery directory; retained for inspection: ", directory)
    }
    # Remove recovery metadata last, so interrupted cleanup remains retryable.
    data <- entries[!basename(entries) %in% c("journal.rds", "journal-next.rds", "journal-previous.rds")]
    if (length(data) && !all(file.remove(data))) stop("Cannot clean up recovery files: ", directory)
    metadata <- entries[!entries %in% data]
    if (length(metadata) && !all(file.remove(metadata))) stop("Cannot clean up recovery journal: ", directory)
    if (unlink(directory, recursive = FALSE) != 0L || dir.exists(directory)) {
        # unlink on some platforms does not remove an empty directory without recursive.
        if (dir.exists(directory) && !length(list.files(directory, all.files = TRUE, no.. = TRUE))) {
            if (unlink(directory, recursive = TRUE) == 0L) return(invisible(NULL))
        }
        stop("Cannot remove empty recovery directory: ", directory)
    }
    invisible(NULL)
}

#' Recover an interrupted renumber operation
#'
#' Reverse recorded moves and script edits from an unfinished [renumber()] call.
#' Recovery does not infer progress from filename prefixes. Operations are reversed
#' in reverse order, and a move whose recorded intent was saved but not executed
#' is skipped. Repeated recovery attempts can resume a partial rollback.
#'
#' @param recovery_dir The recovery directory reported by `renumber()`, normally
#' `.slurmjobs-renumber` inside its `base_dir`.
#' @details Stop other writers before recovery. Recovery stops if a destination
#' is occupied or a renamed object's recorded identity/metadata changed, retaining the
#' journal and remaining originals for inspection. Script bytes are restored in
#' the existing file after verifying identity and the saved backup. An unfinished
#' write may have any length or content; its recorded identity must still match.
#' A completed write with unexpectedly changed contents is a recovery conflict.
#' Unfinished writes cannot be distinguished from edits by another writer to the
#' same file, so exclude concurrent writers during both execution and recovery.
#' These checks do not protect against changes between checks and writes/renames.
#' Once a call
#' committed successfully, recovery only finishes cleaning up its temporary files.
#' Recovery validates non-log paths against the original physical `base_dir`.
#' Log moves use the saved project root when `protect_log_location` was `TRUE`,
#' or may be outside the project when it was `FALSE`. Recovery never recomputes
#' the project with `here`. A moved journal or a path outside its saved scope is
#' refused. Older journals without log provenance remain confined to `base_dir`.
#' The journal is private local package state; do not edit it or use an untrusted
#' recovery directory.
#' @return Invisibly `NULL` on successful rollback or cleanup.
#' @export
renumber_recover <- function(recovery_dir) {
    if (basename(recovery_dir) != ".slurmjobs-renumber" || fs::link_exists(recovery_dir)) {
        stop("Expected an unlinked .slurmjobs-renumber recovery directory.")
    }
    directory <- normalizePath(recovery_dir, mustWork = TRUE)
    # A session can stop after deleting the journal but before removing its
    # now-empty directory. Explicit recovery may finish that last cleanup step.
    if (!length(list.files(directory, all.files = TRUE, no.. = TRUE))) {
        if (unlink(directory, recursive = TRUE) != 0L) stop("Cannot remove empty recovery directory.")
        return(invisible(NULL))
    }
    # Validate metadata paths before reading a potentially redirected journal.
    .renumber_assert_within(file.path(directory,
        c("journal.rds", "journal-next.rds", "journal-previous.rds")), directory,
        reject_aliases = TRUE)
    journal <- .renumber_read_journal(directory)
    .renumber_check_journal_paths(journal, directory)
    if (!identical(journal$status, "committed")) {
        while (length(journal$operations)) {
            index <- length(journal$operations)
            operation <- journal$operations[[index]]
            if (identical(operation$kind, "edit")) {
                .renumber_recover_edit(operation, journal, directory, index)
            } else {
                at_source <- .renumber_matches(operation$from, operation$stamp)
                at_destination <- .renumber_matches(operation$to, operation$stamp)
                if (at_source && !at_destination) {
                    # Intent never executed, or reversal completed before its journal update.
                } else if (!.renumber_path_exists(operation$from) && at_destination) {
                    .renumber_move(operation$to, operation$from)
                } else {
                    stop("Recovery conflict for '", operation$from, "' and '", operation$to,
                        "'. No replacement attempted; journal retained in: ", directory,
                        call. = FALSE)
                }
            }
            journal$operations <- journal$operations[-index]
            .renumber_save_journal(journal, directory)
        }
    }
    .renumber_cleanup(journal, directory)
    invisible(NULL)
}

.renumber_edit_bytes <- function(bytes, replacements) {
    if (!length(bytes) || !length(replacements)) return(bytes)
    # One substitution pass prevents cascades in swaps, without marker strings.
    keys <- names(replacements)[order(nchar(names(replacements)), decreasing = TRUE)]
    escape <- function(x) {
        chars <- strsplit(x, "", fixed = TRUE)[[1]]
        paste0(ifelse(chars %in% c("\\", ".", "^", "$", "|", "?", "*", "+",
            "(", ")", "[", "]", "{", "}"), paste0("\\", chars), chars), collapse = "")
    }
    pattern <- paste(vapply(keys, escape, character(1)), collapse = "|")
    charToRaw(stringr::str_replace_all(rawToChar(bytes), pattern,
        function(matches) unname(replacements[matches])))
}

.renumber_prepare <- function(files, replacements, directory) {
    edits <- list()
    for (path in files) {
        # In-place editing through a link could modify files outside the plan.
        # Refuse linked targets rather than follow aliases silently.
        info <- fs::file_info(path)
        if (fs::link_exists(path) || info$type != "file" || info$hard_links > 1 ||
            normalizePath(path, mustWork = TRUE) != .renumber_canonical_paths(path)) {
            stop("Cannot edit a linked or non-regular script: ", path)
        }
        original <- readBin(path, "raw", n = info$size)
        edited <- .renumber_edit_bytes(original, replacements)
        if (identical(original, edited)) next
        index <- length(edits) + 1L
        prepared <- file.path(directory, paste0("prepared-", index))
        writeBin(edited, prepared)
        if (!identical(readBin(prepared, "raw", n = file.info(prepared)$size), edited)) {
            stop("Prepared script verification failed: ", path)
        }
        backup <- file.path(directory, paste0("original-", index))
        writeBin(original, backup)
        if (!identical(.renumber_read_bytes(backup), original)) {
            stop("Original script backup verification failed: ", path)
        }
        edits[[index]] <- list(path = path, prepared = prepared, backup = backup,
            stamp = .renumber_stamp(path), original_hash = .renumber_hash(backup),
            edited_hash = .renumber_hash(prepared))
    }
    edits
}

.renumber_transaction <- function(base_dir, plan, files, replacements,
        protect_log_location = TRUE, log_root = base_dir) {
    directory <- file.path(base_dir, ".slurmjobs-renumber")
    .renumber_assert_plan(data.frame(before = plan$before, after = plan$staged,
        is_log = plan$is_log), base_dir, log_root)
    .renumber_assert_plan(data.frame(before = plan$before, after = plan$final,
        is_log = plan$is_log), base_dir, log_root)
    .renumber_assert_within(c(files, directory), base_dir, reject_aliases = TRUE)
    if (!dir.create(directory, mode = "0700", showWarnings = FALSE)) {
        stop("Cannot create recovery directory: ", directory)
    }
    state <- new.env(parent = emptyenv())
    state$directory <- directory
    # Register possible preparation paths before writing any of them.
    state$journal <- list(format = 2L, base_dir = base_dir,
        log_policy = list(protect = protect_log_location, root = log_root),
        status = "preparing", operations = list(),
        prepared = file.path(directory, paste0("prepared-", seq_along(files))),
        backups = file.path(directory, paste0("original-", seq_along(files))))
    # If initial journaling fails, no original has been touched; keep the folder
    # for inspection rather than risk removing unrelated entries.
    .renumber_save_journal(state$journal, directory)
    committed <- FALSE
    handle_failure <- function(error) {
        durable_commit <- tryCatch(
            identical(.renumber_read_journal(directory)$status, "committed"),
            error = function(e) FALSE)
        if (committed || durable_commit) {
            stop("Renumbering completed, but cleanup failed. Run renumber_recover('",
                directory, "'): ", conditionMessage(error), call. = FALSE)
        }
        recovery_error <- tryCatch({
            suspendInterrupts(renumber_recover(directory))
            NULL
        }, error = identity)
        message <- if (is.null(recovery_error)) {
            paste0("Renumbering failed; changes from this call were rolled back: ", conditionMessage(error))
        } else {
            paste0("Renumbering failed: ", conditionMessage(error),
                ". Automatic recovery stopped: ", conditionMessage(recovery_error),
                ". Inspect and run renumber_recover('", directory, "').")
        }
        stop(structure(list(message = message, call = NULL, recovery_dir = directory,
            recovered = is.null(recovery_error), parent = error),
            class = c("slurmjobs_rename_error", "error", "condition")))
    }
    tryCatch({
        inventory <- .renumber_inventory(unique(c(plan$before, files)))
        edits <- .renumber_prepare(files, replacements, directory)
        if (!identical(inventory, .renumber_inventory(unique(c(plan$before, files))))) {
            stop("Inputs changed during preparation.")
        }
        .renumber_validate_plan(data.frame(before = plan$before, after = plan$staged,
            is_log = plan$is_log), base_dir, log_root)
        state$journal$status <- "publishing"
        .renumber_save_journal(state$journal, directory)
        for (edit in edits) {
            .renumber_record_edit(state, edit)
        }
        for (i in seq_len(nrow(plan))) {
            .renumber_record_move(state, plan$before[i], plan$staged[i], plan$is_log[i])
        }
        for (i in seq_len(nrow(plan))) {
            .renumber_record_move(state, plan$staged[i], plan$final[i], plan$is_log[i])
        }
        state$journal$status <- "committed"
        .renumber_save_journal(state$journal, directory)
        committed <- TRUE
        .renumber_cleanup(state$journal, directory)
    }, error = handle_failure, interrupt = handle_failure)
    invisible(NULL)
}

.renumber_code_files <- function(directory) {
    entries <- list.files(directory, full.names = TRUE)
    result <- character()
    for (entry in entries) {
        if (dir.exists(entry)) {
            if (fs::link_exists(entry)) stop("Cannot recursively edit through linked directory: ", entry)
            result <- c(result, .renumber_code_files(entry))
        } else if (grepl("\\.(R|sh|py)$", entry)) {
            result <- c(result, entry)
        }
    }
    result
}

.renumber_read_bytes <- function(path) {
    readBin(path, "raw", n = file.info(path)$size)
}

.renumber_hash <- function(path) {
    value <- unname(tools::md5sum(path))
    if (is.na(value)) stop("Cannot verify script bytes: ", path)
    value
}

.renumber_same_script <- function(path, stamp) {
    if (!.renumber_path_exists(path) || fs::link_exists(path)) return(FALSE)
    info <- fs::file_info(path)
    if (info$type != "file" || info$hard_links != 1) return(FALSE)
    # Size and modification time necessarily change during an unfinished write.
    identical(.renumber_stamp(path)[c("type", "device_id", "inode")],
        stamp[c("type", "device_id", "inode")])
}

.renumber_write_bytes <- function(path, bytes) {
    # r+b requires an existing file; it never creates a missing script. Writing
    # through this connection retains the file object and its attached metadata.
    connection <- file(path, open = "r+b")
    on.exit(close(connection))
    writeBin(bytes, connection)
    truncate(connection)
    invisible(NULL)
}

.renumber_record_edit <- function(state, edit) {
    .renumber_assert_within(edit$path, state$journal$base_dir, reject_aliases = TRUE)
    .renumber_assert_within(c(edit$backup, edit$prepared), state$directory,
        reject_aliases = TRUE)
    if (!.renumber_matches(edit$path, edit$stamp) ||
        !identical(.renumber_hash(edit$path), edit$original_hash)) {
        stop("Script changed before publication: ", edit$path)
    }
    if (!identical(.renumber_hash(edit$backup), edit$original_hash) ||
        !identical(.renumber_hash(edit$prepared), edit$edited_hash)) {
        stop("Prepared script or original backup changed: ", edit$path)
    }
    operation <- c(list(kind = "edit", state = "writing"), edit)
    index <- length(state$journal$operations) + 1L
    state$journal$operations[[index]] <- operation
    .renumber_save_journal(state$journal, state$directory)
    if (!.renumber_same_script(edit$path, edit$stamp)) {
        stop("Script identity changed before writing: ", edit$path)
    }
    .renumber_write_bytes(edit$path, .renumber_read_bytes(edit$prepared))
    if (!.renumber_same_script(edit$path, edit$stamp) ||
        !identical(.renumber_hash(edit$path), edit$edited_hash)) {
        stop("Written script verification failed: ", edit$path)
    }
    state$journal$operations[[index]]$state <- "written"
    .renumber_save_journal(state$journal, state$directory)
    invisible(NULL)
}

.renumber_recover_edit <- function(operation, journal, directory, index) {
    path <- operation$path
    if (!.renumber_same_script(path, operation$stamp)) {
        stop("Recovery conflict: script identity or links changed: ", path,
            ". Journal retained in: ", directory, call. = FALSE)
    }
    if (!.renumber_path_exists(operation$backup) || fs::link_exists(operation$backup) ||
        !identical(.renumber_hash(operation$backup), operation$original_hash)) {
        stop("Original script backup is missing or changed: ", operation$backup,
            ". Journal retained in: ", directory, call. = FALSE)
    }
    current_hash <- .renumber_hash(path)
    if (identical(current_hash, operation$original_hash)) return(invisible(NULL))
    if (identical(operation$state, "written") &&
        !identical(current_hash, operation$edited_hash)) {
        stop("Recovery conflict: completed script edit changed: ", path,
            ". Journal retained in: ", directory, call. = FALSE)
    }
    # Persist restoration intent before modifying the file, so a second session
    # can resume even if this restoration itself is interrupted part way through.
    journal$operations[[index]]$state <- "restoring"
    .renumber_save_journal(journal, directory)
    .renumber_write_bytes(path, .renumber_read_bytes(operation$backup))
    if (!.renumber_same_script(path, operation$stamp) ||
        !identical(.renumber_hash(path), operation$original_hash)) {
        stop("Restored script verification failed: ", path)
    }
    invisible(NULL)
}
