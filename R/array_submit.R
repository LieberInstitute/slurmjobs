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
#' @param name A `character(1)` vector giving either the name or path (relative
#' or absolute) to the shell or R script to create.
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
#' @return The absolute path to the shell script to create.
#' @export
#' @author Leonardo Collado-Torres
#' @author Nicholas J. Eagles
#'
#' @family shell-script creation and submission functions
#'
#' @examples
#'
#' ## Choose a script name
#' job_name <- paste0("array_submit_example_", Sys.Date())
#'
#' ## Create an array job script to use for this example
#' job_single(
#'     name = job_name,
#'     create_shell = TRUE,
#'     task_num = 100
#' )
#'
#' ## Now we can submit the job for a set of task IDs (or omit 'task_ids'
#' ## to automatically grab those same failed task IDs)
#' array_submit(
#'     name = job_name,
#'     task_ids = c(1, 6, 8:20, 67),
#'     submit = FALSE
#' )
#'
array_submit <- function(name, task_ids = NULL, submit = FALSE, restore = TRUE, verbose = FALSE) {
    #   Grab the path to the shell script, and the shell script's name,
    #   respectively
    sh_file <- parse_file_or_name(name, should_exist = FALSE, r_ok = TRUE)
    name <- strsplit(basename(sh_file), "\\.sh$")[[1]]

    job_original <- readLines(sh_file)

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
            "been generated with 'job_single()'/'job_loop()' or have been run completely",
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
            message(sprintf("The highest task in %s.sh was %s.", name, max_task))
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

        max_log <- job_original[grep("^#SBATCH -o ", job_original)] |>
            str_extract("-o (.*)$", group = 1) |>
            str_replace("%a", max_task)

        #   For jobs created with 'job_loop', the actual log used will be on the
        #   line where 'log_path' is defined, not '/dev/null'
        if (!is.na(max_log) && (max_log == "/dev/null")) {
            max_log <- job_original[grep("^log_path=", job_original)] |>
                str_extract("^log_path=(.*)/", group = 1) |>
                list.files(
                    pattern = sprintf(
                        "%s.*_%s\\.txt$",
                        name,
                        max_task
                    ),
                    full.names = TRUE
                )
        }

        if (verbose) {
            message(
                sprintf(
                    "Found the log path %s for the highest array task.", max_log
                )
            )
        }

        #   Halt if anything unexpected occurs when finding the log file
        if (is.na(max_log) || length(max_log) != 1) {
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
        if (file.exists(max_log)) {
            max_log <- readLines(max_log)
        } else {
            stop(
                paste(
                    "Could not find the log from the original array job.",
                    err_string,
                    sep = "\\n"
                )
            )
        }
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
    writeLines(job_new, con = paste0(name, ".sh"))

    #   Invoke (sbatch) the modified script if requested
    if (submit) {
        message(paste(Sys.time(), "Resubmitting the specified tasks."))
        system(sprintf("sbatch %s.sh", name))
    }

    # Restore the original script
    if (restore) writeLines(job_original, con = paste0(name, ".sh"))

    # Return the path
    return(invisible(paste0(name, ".sh")))
}
