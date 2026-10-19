# Resolve existing ancestors first, so a symlink followed by '..' is interpreted
# by the filesystem rather than simplified lexically before checking scope.
# Missing suffixes are supported for future destinations and nonexistent logs.
.renumber_resolve_path <- function(path) {
    if (!fs::is_absolute_path(path)) path <- file.path(getwd(), path)
    suffix <- character()
    ancestor <- path
    while (!.renumber_path_exists(ancestor)) {
        parent <- dirname(ancestor)
        if (identical(parent, ancestor)) stop("Cannot resolve path: ", path)
        suffix <- c(basename(ancestor), suffix)
        ancestor <- parent
    }
    if (fs::link_exists(ancestor) && !file.exists(ancestor)) {
        stop("The proposed renaming plan includes a dangling linked path: ", path,
            call. = FALSE)
    }
    resolved <- normalizePath(ancestor, mustWork = TRUE)
    if (length(suffix)) {
        resolved <- fs::path_norm(do.call(file.path, c(list(resolved), as.list(suffix))))
    }
    as.character(fs::path_norm(resolved))
}

.renumber_assert_within <- function(paths, base_dir, reject_aliases = FALSE,
        scope_name = "'base_dir'") {
    if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths))) {
        stop("The proposed renaming plan includes an invalid path.", call. = FALSE)
    }
    # base_dir is the fixed physical root established at entry, not a path to
    # re-resolve to a new location if an ancestor is replaced during execution.
    prefix <- if (!is.null(base_dir)) {
        paste0(sub("/$", "", as.character(fs::path_norm(base_dir))), "/")
    } else NULL
    for (path in paths) {
        resolved <- .renumber_resolve_path(path)
        if (!is.null(prefix) && !startsWith(resolved, prefix)) {
            stop("The proposed renaming plan includes a path outside ", scope_name,
                " (", base_dir, "): ", path, call. = FALSE)
        }
        if (reject_aliases && .renumber_path_exists(path)) {
            info <- fs::file_info(path)
            if (fs::link_exists(path) || (info$type == "file" && info$hard_links > 1)) {
                stop("The proposed renaming plan includes a linked or non-regular target: ",
                    path, call. = FALSE)
            }
        }
    }
    invisible(NULL)
}

.renumber_check_journal_paths <- function(journal, directory) {
    base_dir <- journal$base_dir
    if (is.null(base_dir)) base_dir <- dirname(directory) # earlier local journals
    journal$base_dir <- base_dir
    expected <- file.path(base_dir, ".slurmjobs-renumber")
    if (!identical(directory, expected) || fs::link_exists(directory)) {
        stop("Recovery directory moved or linked; refusing changes outside its original base_dir.")
    }
    .renumber_assert_within(directory, base_dir, reject_aliases = TRUE)
    .renumber_assert_within(c(journal$prepared, journal$backups), directory,
        reject_aliases = TRUE)
    for (operation in journal$operations) {
        if (identical(operation$kind, "edit")) {
            .renumber_assert_within(operation$path, base_dir, reject_aliases = TRUE)
            .renumber_assert_within(c(operation$prepared, operation$backup), directory,
                reject_aliases = TRUE)
        } else {
            .renumber_assert_move_paths(c(operation$from, operation$to),
                isTRUE(operation$is_log), journal)
        }
    }
    # Journals are rewritten during rotation; aliases here could otherwise
    # redirect saveRDS into an unrelated file outside the recovery directory.
    .renumber_assert_within(file.path(directory,
        c("journal.rds", "journal-next.rds", "journal-previous.rds")), directory,
        reject_aliases = TRUE)
    invisible(NULL)
}


.renumber_project_root <- function() {
    normalizePath(here::here(), mustWork = TRUE)
}

.renumber_assert_log_paths <- function(paths, log_root, reject_aliases = FALSE) {
    .renumber_assert_within(paths, log_root, reject_aliases,
        scope_name = "the project directory returned by here::here()")
    # Source checks also run before moves and recovery. Log exceptions authorize
    # regular files only, never directories whose names happen to end in .txt.
    if (reject_aliases) {
        existing <- paths[.renumber_path_exists(paths)]
        if (length(existing)) {
            info <- fs::file_info(existing, fail = TRUE)
            if (any(info$type != "file")) {
                stop("Log targets must be regular files: ",
                    paste(existing[info$type != "file"], collapse = ", "), call. = FALSE)
            }
        }
    }
}

.renumber_assert_plan <- function(plan, base_dir, log_root) {
    .renumber_assert_within(plan$before[!plan$is_log], base_dir, reject_aliases = TRUE)
    .renumber_assert_within(plan$after[!plan$is_log], base_dir)
    .renumber_assert_log_paths(plan$before[plan$is_log], log_root, reject_aliases = TRUE)
    .renumber_assert_log_paths(plan$after[plan$is_log], log_root)
    invisible(NULL)
}

.renumber_assert_move_paths <- function(paths, is_log, journal) {
    if (is_log) {
        policy <- journal$log_policy
        if (is.null(policy) || !is.logical(policy$protect) || length(policy$protect) != 1L ||
            is.na(policy$protect) || (policy$protect &&
                (!is.character(policy$root) || length(policy$root) != 1L || is.na(policy$root)))) {
            stop("Missing or invalid saved log protection policy.")
        }
        .renumber_assert_log_paths(paths, if (policy$protect) policy$root else NULL,
            reject_aliases = TRUE)
    } else {
        .renumber_assert_within(paths, journal$base_dir, reject_aliases = TRUE)
    }
    invisible(NULL)
}
