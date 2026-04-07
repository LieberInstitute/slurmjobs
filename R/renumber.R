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
#' @param plots_and_processed A `logical(1)` indicating whether to also re-order
#' the corresponding `plots` and `processed-data` directories, if they exist.
#' @param expect_matches A `logical(1)` indicating whether to expect that all
#' prefices in `pre_before` will match a file in `base_dir`, throwing an error if
#' not.
#' 
#' @return NULL
#' @export
#' @author Nicholas J. Eagles
#'
#' @importFrom fs is_absolute_path
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
#' renumber(base_dir, c("01", "02"), c("02", "01"))
#' 
#' #   Check that the scripts have been properly renamed
#' list.files(base_dir)
renumber <- function(
        base_dir, pre_before, pre_after, plots_and_processed = FALSE,
        expect_matches = TRUE
    ) {
    if (!dir.exists(base_dir)) {
        stop("'base_dir' must exist.")
    }
    if (length(pre_before) != length(pre_after)) {
        stop("'pre_before' and 'pre_after' must be the same length.")
    }

    #   Check the all the prefices in 'pre_before' match a file in 'base_dir'
    all_files <- list.files(base_dir, full.names = TRUE)
    matches <- sapply(
        pre_before,
        function(x) length(grep(paste0("^", x), basename(all_files)))
    )
    if (expect_matches && !all(matches > 0)) {
        stop(
            "At least one prefix in 'pre_before' did not match a file in 'base_dir'. Consider setting 'expect_matches = FALSE' to ignore missing prefixes."
        )
    }

    all_log_dirs = c()
    for (i in seq_len(length(pre_before))) {
        temp = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.(R|py)",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = FALSE,
            edit_content = TRUE,
            expected_one_file = TRUE
        )

        temp = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.sh",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = TRUE,
            edit_content = TRUE,
            expected_one_file = TRUE
        )
        all_log_dirs = c(all_log_dirs, temp)

        temp = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "(\\.(R|py|sh)|temp_slurmjobs)",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = FALSE,
            edit_content = FALSE,
            expected_one_file = FALSE,
            negate_pattern = TRUE
        )
    }

    #   Remove temporary suffix from (non-log) file names
    all_files <- list.files(base_dir, full.names = TRUE)
    files_before <- all_files[
        grepl(
            sprintf(
                "^(%s).*temp_slurmjobs$", paste(pre_after, collapse = "|")
            ),
            basename(all_files)
        )
    ]
    files_after <- sub("temp_slurmjobs$", "", files_before)
    file.rename(files_before, files_after)

    #   Remove temporary suffix from log file names. This is slightly imprecise,
    #   as in theory there can be a mismatch of prefix and log_dir, such that
    #   a log file with the wrong prefix, also ending in 'temp_slurmjobs', could
    #   be renamed. Ignore this highly unlikely case for now
    for (log_dir in all_log_dirs) {
        all_files <- list.files(log_dir, full.names = TRUE)
        files_before <- all_files[
            grepl(
                sprintf(
                    "^(%s).*temp_slurmjobs$", paste(pre_after, collapse = "|")
                ),
                basename(all_files)
            )
        ]
        files_after <- sub("temp_slurmjobs$", "", files_before)
        file.rename(files_before, files_after)
    }

    #   For code directories, also renumber the corresponding 'processed-data'
    #   and 'plots' directories, if they exist
    if (plots_and_processed) {
        #   Split the path into components (OS-independent via fs)
        path_parts <- fs::path_split(normalizePath(base_dir))[[1]]
        path_prefix <- path_parts[1]
        path_parts <- path_parts[2:length(path_parts)]

        #   Find the index of the 'code' component (use the last occurrence in
        #   case 'code' appears multiple times in the path)
        code_index <- rev(which(path_parts == "code"))[1]

        if (!is.na(code_index)) {
            #   The sub-path below 'code' is the same for sibling directories,
            #   if it exists
            if (code_index == length(path_parts)) {
                sub_path <- character(0)
            } else {
                sub_path <- path_parts[seq(code_index + 1, length(path_parts))]
            }

            for (sibling in c("processed-data", "plots")) {
                sibling_parts <- c(
                    path_parts[seq_len(code_index - 1)],
                    sibling,
                    sub_path
                )
                sibling_dir <- paste0(
                    path_prefix, do.call(file.path, as.list(sibling_parts))
                )

                if (dir.exists(sibling_dir)) {
                    renumber(
                        sibling_dir,
                        pre_before,
                        pre_after,
                        plots_and_processed = FALSE,
                        expect_matches = FALSE
                    )
                }
            }
        }
    }

    return(invisible(NULL))
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
        is_shell, edit_content, expected_one_file, negate_pattern = FALSE
    ) {
    second_condition = grepl(sprintf("%s$", file_regex), basename(all_files))
    if (negate_pattern) {
        second_condition = !second_condition
    }
    file_before <- all_files[
        grepl(sprintf("^%s", this_pre_before), basename(all_files)) &
        second_condition
    ]

    if (expected_one_file) {
        if (length(file_before) > 1) {
            stop(
                "Expected at most one file with prefix '", this_pre_before,
                "'. Found ", length(file_before), "."
            )
        }
    }

    if (length(file_before) == 0) {
        return(invisible(NULL))
    }

    full_pre_before <- stringr::str_extract(
        basename(file_before),
        sprintf("(^%s.*)%s$", this_pre_before, file_regex), group = 1
    )
    full_pre_after <- sub(
        paste0("^", this_pre_before), this_pre_after, full_pre_before
    )
    
    log_dir = c()
    if (edit_content) {
        file_content = readLines(file_before)

        if (is_shell) {
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

            #   If the log is specified with a relative path, make sure it's
            #   relative to the directory containing the shell script
            if (!fs::is_absolute_path(log_dir)) {
                log_dir <- file.path(dirname(file_before), log_dir) |>
                    normalizePath()
            }

            #   Use recursion to rename logs, where they exist
            .renumber_process_file(
                base_dir = log_dir,
                file_regex = "\\.(txt|log)",
                this_pre_before = this_pre_before,
                this_pre_after = this_pre_after,
                all_files = list.files(log_dir, full.names = TRUE),
                is_shell = FALSE,
                edit_content = FALSE,
                expected_one_file = FALSE
            )
        }

        #   Re-write the file in place, replacing references to the
        #   old prefix
        file_content <- gsub(full_pre_before, full_pre_after, file_content)
        writeLines(file_content, con = file_before)
    }

    #   Rename scripts but append temporary suffix to avoid repeated
    #   renaming
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
    file.rename(file_before, file_after)

    return(log_dir)
}
