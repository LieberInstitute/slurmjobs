#' Submit an array job with a specified set of task IDs
#'
#' Given a bash script that specifies the --array `sbatch` option (that is, an
#' array job), this function overwrites (temporarily if `restore` = TRUE) the
#' script in place and resubmits (when `submit` = TRUE) the array with the
#' specified `task_ids`. Alternatively, the job ID (`job_id`) for the original
#' run of the array job may be specified, and failed tasks are automatically
#' inferred. This function is intended to help re-run failed tasks of a large
#' array job that was previously submitted.
#'
#' @param job_bash A `character(1)` vector with the name of a bash script
#' in the current working directory.
#' @param task_ids A numeric vector specifying which (relative) task IDs to
#' resubmit (e.g. c(1, 4, 6)), or NULL if 'job_id' is provided.
#' @param job_id Optional `numeric(1)` or `character(1)` specifying the job ID
#' for the initial submission of the array job whose path is given by
#' `job_bash`. The array is assumed to have run with all of its tasks. If
#' provided, `task_ids` will automatically be inferred by which tasks failed.
#' @param submit A `logical(1)` vector determining whether to actually submit
#' the tasks or not using `qsub`.
#' @param restore A `logical(1)` vector determining whether to restore the
#' script to the original state.
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
#'     ## Or specify the ID for the original array submission to automatically
#'     ## grab those same failed task IDs
#'     array_submit(
#'         job_bash = paste0(job_name, ".sh"),
#'         job_id = 235510,
#'         submit = FALSE
#'     )
#' })
#'
array_submit <- function(
        job_bash, task_ids = NULL, job_id = NULL, submit = FALSE, restore = TRUE
    ) {
    ## Check that the script is in the working directory
    if (basename(job_bash) != job_bash) {
        stop(
            "The 'job_bash' script has to exist in the current working directory,\n",
            "since code may depend on relative paths.",
            call. = FALSE
        )
    }

    #   Check that at least one of 'task_ids' and 'job_id' is not NULL. If both
    #   are defined, 'task_ids' is ignored with a warning
    if (!(is.null(job_id) || is.null(task_ids))) {
        stop("Either 'job_id' or 'task_ids' must be non-NULL.")
    }
    if (!is.null(job_id) & !is.null(task_ids)) {
        warning("Ignoring 'task_ids' since 'job_id' was not NULL.")
    }

    job_original <- readLines(job_bash)
    t_line <- grep("^#SBATCH --array=", job_original)
    if (length(t_line) != 1) {
        stop("Could not find the line that specifies that this is an array job,\n",
            "that is, the line that starts with: '#SBATCH --array='",
            call. = FALSE
        )
    }

    #   Just replace the array line with the specified tasks, and overwrite the
    #   script in place
    job_new = sub(
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
