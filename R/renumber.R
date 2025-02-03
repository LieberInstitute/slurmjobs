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
#'
#' @return NULL
#' @export
#' @author Nicholas J. Eagles
#'
#' @examples
#' base_dir <- file.path(tempdir(), "slurmjobs_scripts")
#' dir.create(base_dir)
#' 
#' #   Create a shell script that submits a corresponding R script
#' job_single(
#'     file.path(base_dir, "01_should_be_second.sh"),
#'     logdir = "logs", create_logdir = TRUE,
#'     create_shell = TRUE, command = "Rscript 01_should_be_second.R"
#' )
#' writeLines("# some code", con = file.path(base_dir, "01_should_be_second.R"))
#' 
#' #   Create an array originally designed to be submitted second
#' job_loop(
#'     file.path(base_dir, "02_should_be_first.sh"),
#'     create_shell = TRUE, logdir = "logs"
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
renumber <- function(base_dir, pre_before, pre_after) {
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
    if (!all(matches > 0)) {
        stop("At least one prefix in 'pre_before' did not match a file in 'base_dir'.")
    }

    for (i in seq_len(length(pre_before))) {
        #   Edit the content of the shell script if it exists, and find and
        #   update log names
        shell_before <- all_files[
            grep(sprintf("^%s.*\\.sh$", pre_before[i]), basename(all_files))
        ]
        if (length(shell_before) > 1) {
            stop(
                "Expected no more than one shell script with prefix '",
                pre_before[i], "'."
            )
        } else if (length(shell_before) == 1) {
            shell_content <- readLines(shell_before)

            full_pre_before <- stringr::str_extract(
                basename(shell_before),
                sprintf("(^%s.*)\\.sh$", pre_before[i]),
                group = 1
            )
            full_pre_after <- sub(
                paste0("^", pre_before[i]), pre_after[i], full_pre_before
            )

            #   For 'job_loop', use the path in the line starting with 'log_path='.
            #   Otherwise use the log in the line starting with '#SBATCH -o'.
            last_occurence <- rev(grep("^(#SBATCH -o |log_path=)", shell_content))[1]
            log_dir <- shell_content[last_occurence] |>
                #   Extract just the path
                str_replace("^(#SBATCH -o |log_path=)", "") |>
                dirname()

            #   If the log is specified with a relative path, make sure it's
            #   relative to the directory containing the shell script
            if (!grepl("^/", log_dir)) {
                log_dir <- file.path(dirname(shell_before), log_dir) |>
                    normalizePath()
            }

            #   Rename logs, if any exist
            logs_before <- list.files(
                log_dir,
                pattern = sprintf("^%s.*\\.(txt|log)$", full_pre_before),
                full.names = TRUE
            )
            logs_after <- file.path(
                log_dir,
                sub(full_pre_before, full_pre_after, basename(logs_before))
            )
            if (length(logs_before) > 0) {
                file.rename(logs_before, logs_after)
            }

            #   Re-write the shell script in place, replacing references to the
            #   old script name
            shell_content <- gsub(full_pre_before, full_pre_after, shell_content)
            writeLines(shell_content, con = shell_before)
        }

        #   Rename scripts but append temporary suffix to avoid repeated
        #   renaming
        files_before <- all_files[
            grepl(paste0("^", pre_before[i]), basename(all_files)) &
                !grepl("temp_slurmjobs$", all_files)
        ]
        files_after <- file.path(
            base_dir,
            paste0(
                sub(
                    paste0("^", pre_before[i]),
                    pre_after[i],
                    basename(files_before)
                ),
                "temp_slurmjobs"
            )
        )
        file.rename(files_before, files_after)
    }

    #   Remove temporary suffix from script names
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

    return(invisible(NULL))
}
