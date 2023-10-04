#' Submit an array job with a specified set of task IDs
#'
#' Given a bash script that specifies the --array `sbatch` option (that is, an
#' array job), this function overwrites (temporarily if `restore` = TRUE) the
#' script in place and resubmits (when `submit` = TRUE) the array with the
#' specified `task_ids`. If this array was created with `job_single`, `task_ids`
#' may be ommitted and failed tasks are automatically inferred. This function is
#' intended to help re-run failed tasks of a large array job that was previously
#' submitted.
#'
#' @param job_bash A `character(1)` vector with the name of a bash script
#' in the current working directory.
#' @param task_ids An optional numeric vector specifying which (relative) task
#' IDs to resubmit (e.g. c(1, 4, 6)). If NULL, the task IDs will be inferred
#' by scraping the log file for the job ID for the array job as originally
#' submitted, and using `job_report()` to pull failed task IDs
#' @param submit A `logical(1)` vector determining whether to actually submit
#' the tasks or not using `qsub`.
#' @param restore A `logical(1)` vector determining whether to restore the
#' script to the original state.
#' @param verbose A `logical(1)` vector specifying whether to print details
#' about how failed tasks were determined (applicable when `task_ids` is NULL).
#'
#' @return The path to `job_bash`.
#' @export
#' @author Leonardo Collado-Torres
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' ## Choose a script name
#' job_name <- paste0("array_submit_example_", Sys.Date())
#'
#' ## Create an array job on the temporary directory
#' with_wd(tempdir(), {
#'     ## Create an array job script to use for this example
#'     job_single(
#'         name = job_name,
#'         create_shell = TRUE,
#'         task_num = 100
#'     )
#'
#'     ## Now we can submit the job for a set of task IDs
#'     array_submit(
#'         job_bash = paste0(job_name, ".sh"),
#'         task_ids = c(1, 6, 8:20, 67),
#'         submit = FALSE
#'     )
#'
#'     ## Or omit 'task_ids' to automatically grab those same failed task IDs
#'     array_submit(
#'         job_bash = paste0(job_name, ".sh"),
#'         submit = FALSE
#'     )
#' })
#'
array_submit <- function(job_bash, task_ids = NULL, submit = FALSE, restore = TRUE, verbose = FALSE) {
    ## Check that the script is in the working directory
    if (basename(job_bash) != job_bash) {
        stop(
            "The 'job_bash' script has to exist in the current working directory,\n",
            "since code may depend on relative paths.",
            call. = FALSE
        )
    }

    job_original <- readLines(job_bash)

    ############################################################################
    #   Infer failed task IDs if 'task_ids' is NULL
    ############################################################################

    if (is.null(task_ids)) {
        if (verbose) {
            message("Attempting to automatically find failed task IDs since 'task_ids' was NULL.")
        }

        #   Various errors may arise when trying to infer failed task IDs
        #   automatically. In all cases, suggest specifying 'task_ids'
        #   explicitly
        err_string <- paste(
            "Please specify 'task_ids' explicitly, as the array does not appear to have",
            "been generated with 'job_single()' or have been run completely",
            sep = "\\n"
        )

        #-----------------------------------------------------------------------
        #   Grab the highest task of the array as originally submitted
        #-----------------------------------------------------------------------

        max_task <- str_extract(
            job_original[grep("^#SBATCH --array", job_original)],
            "^#SBATCH --array=[0-9]+-([0-9]+)%[0-9]+$",
            group = 1
        )

        if (verbose) {
            message(sprintf("The highest task in %s was %s.", job_bash, max_task))
        }

        #   Halt with an error if it couldn't be found
        if (is.na(max_task)) {
            stop(
                paste(
                    "Failed to find the highest task of the original array job.",
                    err_string,
                    sep = "\\n"
                )
            )
        }

        #-----------------------------------------------------------------------
        #   Find the log associated with that highest task
        #-----------------------------------------------------------------------

        max_logs <- job_original[grep("^#SBATCH -[oe] ", job_original)] |>
            str_extract("-[oe] (.*)$", group = 1) |>
            str_replace("%a", max_task)

        if (verbose) {
            message("Found these logs (should be 2 identical) for the highest array task:")
            print(max_logs)
        }

        #   Halt if anything unexpected occurs when finding the log file
        if (any(is.na(max_logs)) || (length(max_logs) != 2) || (max_logs[1] != max_logs[2])) {
            stop(
                paste(
                    "Failed to find the original log for the highest task in the array.",
                    err_string,
                    sep = "\\n"
                )
            )
        }

        #-----------------------------------------------------------------------
        #   Grab the job ID from the log for the highest task
        #-----------------------------------------------------------------------

        #   Read in the log for the highest array task to grab the job ID (which
        #   SLURM associates with the entire array)
        max_log <- readLines(max_logs[1])
        orig_job_id <- max_log[grep("^Job id: ", max_log)] |>
            str_extract("[0-9]+")

        if (verbose) {
            message(
                sprintf(
                    "Found %s as the job ID for the highest array task",
                    orig_job_id
                )
            )
        }

        if (is.na(orig_job_id)) {
            stop(
                paste(
                    "Failed to find the job ID of the original array job.",
                    err_string,
                    sep = "\\n"
                )
            )
        }

        #-----------------------------------------------------------------------
        #   Use 'job_report' to grab failed task IDs
        #-----------------------------------------------------------------------

        task_ids <- job_report(orig_job_id) |>
            filter(status != "COMPLETED") |>
            pull(array_task_id)

        if (verbose) {
            message("The following task IDs failed:")
            print(task_ids)
        }

        if (any(is.na(task_ids))) {
            stop(
                paste(
                    "Failed to find failed tasks of the original array job",
                    err_string,
                    sep = "\\n"
                )
            )
        }
    }

    t_line <- grep("^#SBATCH --array=", job_original)
    if (length(t_line) != 1) {
        stop("Could not find the line that specifies that this is an array job,\n",
            "that is, the line that starts with: '#SBATCH --array='",
            call. = FALSE
        )
    }

    #   Just replace the array line with the specified tasks, and overwrite the
    #   script in place
    job_new <- sub(
        "^#SBATCH --array=[0-9,\\-]+%(.*)$",
        paste0("#SBATCH --array=", paste(task_ids, collapse = ","), "%\\1"),
        job_original
    )
    writeLines(job_new, con = job_bash)

    #   Invoke (sbatch) the modified script if requested
    if (submit) {
        message(paste(Sys.time(), "Resubmitting the specified tasks."))
        system(paste("sbatch", job_bash))
    }

    # Restore the original script
    if (restore) writeLines(job_original, con = job_bash)

    # Return the path
    return(invisible(job_bash))
}
