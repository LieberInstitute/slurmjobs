#' Re-order scripts and logs with a series of prefices (usually numbers)
#'
#' This function re-orders the prefices in names of scripts-- originally
#' intended to re-order the numbering of scripts developed in the
#' [R-Bioconductor-powered data science team's organization style](https://lcolladotor.github.io/bioc_team_ds/organizing-your-work.html#organization-since-2021),
#' where script names begin with `01`, `02`, etc. References to script names
#' within the shell or R scripts are also updated, along with the names of
#' log files they produce.
#'
#' @param base_dir A `character(1)` path to a directory containing the scripts
#' to re-order.
#' @param pre_before A `character()` vector of prefices to replace among scripts
#' in `base_dir`.
#' @param pre_after A `character()` vector of replacement prefices among scripts
#' in `base_dir`.
#' @param expect_matches A `logical(1)` indicating whether to expect that all
#' prefices in `pre_before` will match a file in `base_dir`, throwing an error if
#' not.
#' @param recursive_edits A `logical(1)` indicating whether to recursively search
#' `base_dir` for R, shell, and Python scripts and replace any instances of the
#' prefices in `pre_before` with the corresponding prefices in `pre_after`.
#' Because of the slight risk where prefices are not uniquely identifiable
#' and unexpected edits take place, the default is `FALSE`; you must opt in
#' intentionally.
#'
#' @param protect_log_location A `logical(1)`, default `TRUE`, requiring log files
#' selected through shell scripts to be inside the project returned by
#' [here::here()]. Logs may be outside `base_dir`. Set `FALSE` to permit logs in
#' any directory referenced by shell scripts, without consulting `here`.
#' This disables only log location checks; collision, link, and recovery checks
#' remain active, and non-log changes must still stay within `base_dir`.
#'
#' @details Non-log changes are confined to the physical directory selected by
#' `base_dir`. Its symbolic-link aliases resolve to that same scope. Selected
#' symbolic links and hard-linked files are refused; recursive editing does not
#' follow linked directories.
#'
#' By default, log files selected for renaming must be within the physical project
#' root returned by `here::here()`, which uses the current R session's project
#' context, not `base_dir`, to determine the root. If no logs are selected, an
#' outside or missing log directory does not prevent renumbering. The project
#' root and log protection setting are saved in the journal, so recovery uses the
#' original policy even from a different working directory or project context.
#'
#' The complete rename plan, including logs, is checked before changes.
#' Hidden files and dangling symbolic links count as occupied destinations.
#' Script edits are prepared in a private recovery directory; logs and data
#' directories are renamed in place, without copying their contents. Only changed
#' scripts have their original bytes backed up. Prepared edits are written into
#' the existing script files, retaining their identity, ownership and ordinary
#' ACLs and extended attributes. Timestamps change, and the operating system may
#' clear special security attributes on modification. Linked script edit targets
#' are refused. An interrupted write can leave partial contents until recovery.
#'
#' A journal records each move or script edit before execution. Errors and R
#' interrupts trigger rollback in reverse order. If rollback cannot finish, the error reports the
#' recovery directory. After inspecting the affected files, call
#' [renumber_recover()] with that directory to retry rollback. A leftover recovery
#' directory blocks another call in the same base directory.
#'
#' Renaming uses base R with destination checks immediately before each move.
#' This is not an atomic transaction: other processes can observe intermediate
#' paths, and a destination created between the check and rename can be replaced.
#' Do not run against files being modified by other processes. Rename recovery
#' refuses occupied destinations or changed objects. Script recovery checks file
#' identity while allowing partial contents from an unfinished write; see
#' [renumber_recover()] for its conflict checks.
#' Recovery survives ordinary R-session termination, but is not a guarantee
#' against power loss, storage corruption, or arbitrary concurrent changes.
#'
#' @return NULL
#' @export
#' @author Nicholas J. Eagles
#'
#' @importFrom fs is_absolute_path
#' @import stringr
#' 
#' @examples
#' base_dir <- file.path(tempdir(), "slurmjobs_scripts")
#' dir.create(base_dir)
#' 
#' #   Create a shell script that submits a corresponding R script
#' job_single(
#'     file.path(base_dir, "01_should_be_second.sh"),
#'     logdir = file.path(base_dir, "logs"), create_logdir = TRUE,
#'     create_shell = TRUE, command = "Rscript 01_should_be_second.R"
#' )
#' writeLines("# some code", con = file.path(base_dir, "01_should_be_second.R"))
#' 
#' #   Create an array originally designed to be submitted second
#' job_loop(
#'     file.path(base_dir, "02_should_be_first.sh"),
#'     create_shell = TRUE, logdir = file.path(base_dir, "logs"),
#'     loops = list(
#'         gene = c("gene_1", "gene_2"), method = c("method_1", "method_2")
#'     ) 
#' )
#'
#' #   Swap the order of the scripts
#' # This temporary example is outside the current project.
#' renumber(base_dir, c("01", "02"), c("02", "01"),
#'     protect_log_location = FALSE)
#' 
#' #   Check that the scripts have been properly renamed
#' list.files(base_dir)
renumber <- function(
        base_dir, pre_before, pre_after, expect_matches = TRUE,
        recursive_edits = FALSE, protect_log_location = TRUE
    ) {
    if (!is.logical(protect_log_location) || length(protect_log_location) != 1L ||
        is.na(protect_log_location)) {
        stop("'protect_log_location' must be TRUE or FALSE.")
    }
    if (!dir.exists(base_dir)) {
        stop("'base_dir' must exist.")
    }
    if (length(pre_before) != length(pre_after)) {
        stop("'pre_before' and 'pre_after' must be the same length.")
    }
    for (prefixes in list(pre_before, pre_after)) {
        if (!is.character(prefixes) || anyNA(prefixes) ||
            any(!nzchar(prefixes)) || any(grepl("[/\\\\]", prefixes))) {
            stop("Prefixes must be non-empty character strings without path separators.")
        }
    }
    if (length(pre_before) == 0L) {
        return(invisible(NULL))
    }
    base_dir <- normalizePath(base_dir, mustWork = TRUE)
    log_root <- if (protect_log_location) .renumber_project_root() else NULL
    recovery_dir <- file.path(base_dir, ".slurmjobs-renumber")
    if (.renumber_path_exists(recovery_dir)) {
        stop("An unfinished renumber operation exists. Inspect and run renumber_recover('",
            recovery_dir, "').", call. = FALSE)
    }

    all_files <- list.files(base_dir, full.names = TRUE)  
    for (x in pre_before) {
        matching_files <- basename(all_files)[
            grepl(paste0("^", x), basename(all_files))
        ]
      
        #   Check the all the prefices in 'pre_before' match a file in 'base_dir'
        if (expect_matches && (length(matching_files) == 0)) {
            stop(
                "Prefix '", x, "' did not match a file in 'base_dir'. Consider setting 'expect_matches = FALSE' to ignore missing prefixes."
            )
        }
        
        #   Prefices must uniquely specify basenames of files, a safety/
        #   convenience feature to prevent accidentally matching short strings
        #   in shell or other files and mistakenly modifying them
        matching_base = str_extract(
            matching_files, sprintf("^(%s[^.]*)\\.?.*", x), group = 1
        )
        if (length(unique(matching_base)) > 1) {
            stop(
                "Prefix '", x, "' matches multiple files with different base names, which is usually unintentional. If this was actually desired, consider modifying 'pre_before' to be more specific."
            )
        }
    }

    #   Gather a full renaming plan of all files (source and destination paths).
    #   Does not modify any files
    rename_plan_df = .renumber_loop(
        base_dir, pre_before, pre_after, all_files, log_root = log_root
    )

    # Freeze edit candidates before validating destinations. A file arriving at
    # a previously absent destination later must not be swept into recursive edits.
    all_code_files <- if (recursive_edits) {
        .renumber_code_files(base_dir)
    } else {
        character()
    }
  
    # Validate physical paths, including paths obtained from shell log directives.
    .renumber_assert_within(all_code_files, base_dir, reject_aliases = TRUE)
    rename_plan <- .renumber_validate_plan(rename_plan_df, base_dir, log_root)
    if (nrow(rename_plan) == 0L) {
        return(invisible(NULL))
    }
    replacements <- character()
    if (recursive_edits) {
        stems <- unique(unlist(lapply(pre_before, function(prefix) {
            matches <- basename(all_files)[grepl(paste0("^", prefix), basename(all_files))]
            unique(stringr::str_extract(matches, sprintf("^(%s[^.]*)\\.?.*", prefix), group = 1))
        }), use.names = FALSE))
        stems <- stems[!is.na(stems)]
        replacements <- stats::setNames(vapply(stems, function(stem) {
            index <- which(vapply(pre_before, function(prefix) startsWith(stem, prefix), logical(1)))[1]
            paste0(pre_after[index], substring(stem, nchar(pre_before[index]) + 1L))
        }, character(1)), stems)
    }
    .renumber_transaction(base_dir, rename_plan, all_code_files, replacements,
        protect_log_location, log_root)
    invisible(NULL)
}

# Resolve parents but not the last component: a dangling symlink itself is an
# occupied directory entry, not its missing target.
.renumber_canonical_paths <- function(paths) {
    file.path(normalizePath(dirname(paths), mustWork = TRUE), basename(paths))
}

.renumber_path_exists <- function(paths) {
    fs::file_exists(paths) | fs::dir_exists(paths) | fs::link_exists(paths)
}

.renumber_validate_plan <- function(plan, base_dir, log_root = base_dir) {
    if (is.null(plan$is_log)) plan$is_log <- rep(FALSE, nrow(plan))
    if (!is.logical(plan$is_log) || anyNA(plan$is_log)) stop("Invalid log provenance in rename plan.")
    .renumber_assert_plan(plan, base_dir, log_root)
    plan <- data.frame(
        before = .renumber_canonical_paths(plan$before),
        staged = .renumber_canonical_paths(plan$after),
        is_log = plan$is_log,
        stringsAsFactors = FALSE
    )
    plan$final <- file.path(dirname(plan$staged),
        sub("temp_slurmjobs$", "", basename(plan$staged)))
    .renumber_assert_plan(data.frame(before = plan$before, after = plan$final,
        is_log = plan$is_log), base_dir, log_root)
    reject <- function(reason) stop("The proposed renaming plan ", reason, call. = FALSE)
    if (anyDuplicated(plan$before)) reject("has duplicate source paths.")
    if (anyDuplicated(plan$staged) || anyDuplicated(plan$final)) {
        reject("has duplicate destination paths.")
    }
    for (source in plan$before) {
        if (any(startsWith(plan$before, paste0(source, "/")))) {
            reject("contains both a directory and a source inside it.")
        }
    }
    if (any(!.renumber_path_exists(plan$before))) reject("has missing source paths.")
    occupied <- .renumber_path_exists(plan$final) & !plan$final %in% plan$before
    if (any(occupied)) {
        reject(paste0("would overwrite an existing destination: ",
            paste(plan$final[occupied], collapse = ", ")))
    }
    if (any(.renumber_path_exists(plan$staged))) {
        reject("would overwrite an existing temporary path.")
    }
    if (any(plan$staged %in% c(plan$before, plan$final))) {
        reject("has overlapping temporary and source/destination paths.")
    }
    plan
}

#' Helper function to rename a single file pattern
#' 
#' This is employed by `renumber()` to individually and separately handle R
#' scripts, shell scripts, and logs
#' 
#' @author Nicholas J. Eagles
#' @keywords internal
.renumber_process_file = function(
        base_dir, file_regex, this_pre_before, this_pre_after, all_files,
        is_shell, negate_pattern = FALSE, scope_dir = base_dir,
        log_root = scope_dir, is_log = FALSE
    ) {
    second_condition = grepl(sprintf("%s$", file_regex), basename(all_files))
    if (negate_pattern) {
        second_condition = !second_condition
    }
    file_before <- all_files[
        grepl(sprintf("^%s", this_pre_before), basename(all_files)) &
        second_condition
    ]

    if (is_shell) {
        if (length(file_before) > 1) {
            stop(
                "Expected at most one shell script with prefix '", 
                this_pre_before, "'. Found ", length(file_before), "."
            )
        }
    }

    if (length(file_before) == 0) {
        return(data.frame(before = character(0), after = character(0), is_log = logical(0)))
    }

    if (is_log) {
        .renumber_assert_log_paths(file_before, log_root, reject_aliases = TRUE)
    } else {
        .renumber_assert_within(file_before, scope_dir, reject_aliases = TRUE)
    }
    full_pre_before <- stringr::str_extract(
        basename(file_before),
        sprintf("(^%s.*)%s$", this_pre_before, file_regex), group = 1
    )
    full_pre_after <- sub(
        paste0("^", this_pre_before), this_pre_after, full_pre_before
    )

    log_files_before <- character(0)
    log_files_after <- character(0)
  
    if (is_shell) {
        file_content = readLines(file_before)
      
        #   For 'job_loop', use the path in the line starting with
        #   'log_path='. Otherwise use the log in the line starting with
        #   '#SBATCH -o'.
        last_occurence <- rev(
            grep("^(#SBATCH -o |log_path=)", file_content)
        )[1]
        log_dir <- file_content[last_occurence] |>
            #   Extract just the path
            str_replace("^(#SBATCH -o |log_path=)", "") |>
            dirname()

        # Shell scripts without a log directive have no associated log plan.
        if (is.na(last_occurence)) {
            log_dir <- character()
        }

        #   If the log is specified with a relative path, make sure it's
        #   relative to the directory containing the shell script
        if (length(log_dir) && !fs::is_absolute_path(log_dir)) {
            log_dir <- file.path(dirname(file_before), log_dir)
        }

        #   Use recursion to plan to rename logs, where they exist
        rename_df = .renumber_process_file(
            base_dir = log_dir,
            file_regex = "\\.(txt|log)",
            this_pre_before = this_pre_before,
            this_pre_after = this_pre_after,
            all_files = if (length(log_dir)) list.files(log_dir, full.names = TRUE) else character(),
            is_shell = FALSE, scope_dir = scope_dir, log_root = log_root,
            is_log = TRUE
        )
        log_files_before <- rename_df$before
        log_files_after <- rename_df$after
    }

    #   Plan to rename scripts but append temporary suffix to avoid repeated
    #   renaming, since file.rename() executes renames sequentially
    file_after <- file.path(
        base_dir,
        paste0(
            sub(
                paste0("^", this_pre_before), this_pre_after,
                basename(file_before)
            ),
            "temp_slurmjobs"
        )
    )
    plan_df = data.frame(
        before = c(file_before, log_files_before),
        after = c(file_after, log_files_after),
        is_log = c(rep(is_log, length(file_before)), rep(TRUE, length(log_files_before)))
    )

    return(plan_df)
}

#' Helper function to gather a plan for renaming all files
#' 
#' This is employed by `renumber()`. A data.frame is returned with columns
#' "before" and "after", indicating the full set of file paths to rename
#' 
#' @author Nicholas J. Eagles
#' @keywords internal
.renumber_loop = function(base_dir, pre_before, pre_after, all_files, log_root = base_dir) {
    rename_plan_df_list = list()
    for (i in seq_len(length(pre_before))) {
        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.sh",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = TRUE, log_root = log_root
        )

        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.sh",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = FALSE,
            negate_pattern = TRUE, log_root = log_root
        )
    }
    rename_plan_df = do.call(rbind, rename_plan_df_list)
    return(rename_plan_df)
}
