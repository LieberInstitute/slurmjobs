#' Submit an array job with a specified set of task IDs
#'
#' Given a bash script that specifies the SGE option -t (that is, an array job),
#' this function submits that job using `qsub` for a given list of task
#' IDs. This function is useful in case some tasks for a given job failed which
#' you can find using `qstat | grep Eqw` or other options.
#'
#' @param job_bash A `character(1)` vector with the name of a bash script
#' in the current working directory.
#' @param task_ids A set of task ids that will get parsed by [parse_task_ids].
#' @param submit A `logical(1)` vector determining whether to actually submit
#' the tasks or not using `qsub`.
#' @param restore A `logical(1)` vector determining whether to restore the
#' script to the original state.
#'
#' @return The path to `job_bash`.
#' @export
#' @author Leonardo Collado-Torres
#' @author Nicholas J. Eagles
#' @import purrr glue
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
#'     ## Now we can submit the SGE job for a set of task IDs
#'     array_submit(
#'         job_bash = paste0(job_name, ".sh"),
#'         task_ids = "225019-225038:1,225040,225043",
#'         submit = FALSE
#'     )
#' })
#'
array_submit <- function(job_bash, task_ids, submit = FALSE, restore = TRUE) {
    ## Check that the script is in the working directory
    if (basename(job_bash) != job_bash) {
        stop(
            "The 'job_bash' script has to exist in the current working directory,\n",
            "since code may depend on relative paths.",
            call. = FALSE
        )
    }

    job_original <- readLines(job_bash)
    t_line <- grep("^#SBATCH --array=", job_original))
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
