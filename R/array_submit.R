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
array_submit <- function(
        job_bash, task_ids, submit = file.exists("/cm/shared/apps/sge/sge-8.1.9/default/common/accounting_20191007_0300.txt"),
        restore = TRUE) {
    ## Check that the script is local, in case the script uses -cwd
    if (basename(job_bash) != job_bash) {
        stop(
            "The 'job_bash' script has to exist in the current working directory.\n",
            "This protects the user from the case where the script uses -cwd.",
            call. = FALSE
        )
    }

    task_ids <- parse_task_ids(task_ids)
    job_original <- readLines(job_bash)
    t_line <- which(grepl("#\\$ -t ", job_original))
    if (length(t_line) != 1) {
        stop("Could not find the line that specifies that this is an array job,\n",
            "that is, the line that starts with: #$ -t ",
            call. = FALSE
        )
    }

    ## Loop through the task ids and re-submit
    purrr::walk(task_ids, function(task) {
        ## Replace the script with the current task ID
        job_new <- job_original
        job_new[t_line] <- glue::glue("#$ -t {task}")
        writeLines(job_new, con = job_bash)


        ## If you wanted to see the files use:
        # cat(job_new)

        ## Show how to re-submit or actually re-submit:
        message(paste(Sys.time(), "resubmitting the SGE job for task", task))
        cmd <- paste("qsub", job_bash)
        message(cmd)
        if (submit) {
            ## Check if qstat can run
            qstat_status <- suppressWarnings(
                system("qstat", ignore.stdout = TRUE, ignore.stderr = TRUE)
            )
            if (qstat_status == 127) {
                system(cmd)
            } else {
                cmd_msg <- system(cmd, intern = TRUE)
                message(cmd_msg)
            }
        }

        ## Done
        return(cmd)
    })

    ## Restore the original script
    if (restore) writeLines(job_original, con = job_bash)

    ## Return the path
    return(invisible(job_bash))
}
