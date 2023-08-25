#' Build a SGE job bash script
#'
#' This function builds a template for a Son of Grid Ending (SGE) job script
#' including array jobs. Check this blog post by John Muschelli to learn
#' more about array jobs:
#' <https://hopstat.wordpress.com/2013/11/05/array-and-sequential-cluster-jobs/>.
#'
#' For a given SGE job that is currently running you can alter
#' the options using `qalter`.
#'
#' @param name A `character(1)` vector with the name of the script. Any spaces
#' will be replaced by underscores.
#' @param create_shell A `logical(1)` vector specifying whether to create a
#' shell file for the script.
#' @param queue A `character(1)` vector with the name of the SGE queue. Check
#' how busy a given queue is by running `qpic -q queuename`.
#' @param memory The amount of memory per core to request in SGE syntax. You
#' can check how much a current job is utilizing using the `qmem` JHPCE command.
#' For more detail on the memory options, check
#' <https://jhpce.jhu.edu/knowledge-base/how-to/#MemSpec>.
#' @param cores The number of cores to request. Note that the total memory
#' your job will request is `cores` multiplied by `memory`.
#' @param email The email reporting option for the email. For more information
#' check <https://jhpce.jhu.edu/knowledge-base/how-to/#Email>.
#' @param logdir The directory for the SGE log files relative to the current
#' working directory.
#' @param filesize The maximum file size in SGE format.
#' @param task_num The number of tasks for your job, which will make it into an
#' array job. If `NULL` this is ignored.
#' @param tc If `task_num` is specified, this option controls the number of
#' concurrent tasks.
#' @param command An example command to start your script.
#' @param create_logdir A `logical(1)` vector specifying whether to create the
#' `logdir` directory. Note that if `logdir` doesn't exist and you submit your
#' job with `qsub`, it will immediately fail.
#'
#' @return A character vector with the script contents. If `create_shell` was
#' specified then it also creates the actual script in the current
#' working directory.
#' @export
#' @author Leonardo Collado-Torres
#'
#' @examples
#'
#' ## A regular job
#' job_single("jhpce_job", create_logdir = FALSE)
#'
#' ## A regular job with 10 cores on the 'imaginary' queue
#' job_single("jhpce_job",
#'     cores = 10, queue = "imaginary",
#'     create_logdir = FALSE
#' )
#'
#' ## An array job
#' job_single("jhpce_job_array", task_num = 20, create_logdir = FALSE)
#'
job_single <- function(
        name, create_shell = FALSE, queue = "shared",
        memory = "10G", cores = 1L, email = "e", logdir = "logs", filesize = "100G",
        task_num = NULL, tc = 20,
        command = 'Rscript -e "options(width = 120); sessioninfo::session_info()"',
        create_logdir = TRUE) {
    ## Remove any spaces
    name <- gsub(" ", "_", name)

    ## Check if the shell file exists already
    if (create_shell) {
        sh_file <- paste0(name, ".sh")
        if (file.exists(sh_file)) {
            stop("The file ", sh_file, " already exists!", call. = FALSE)
        }
    }

    ## Check the email options
    valid_email_opts <- c("a", "e", "n", "b", "be")
    if (!email %in% valid_email_opts) {
        stop("'email' should be one of the following options:\n",
            paste(valid_email_opts, collapse = ", "),
            call. = FALSE
        )
    }

    ## Force the logs directory to be relative
    if (grepl("^/|^\\\\", logdir)) {
        stop("'logdir' has to be a relative path.")
    }

    ## Specify the cores options
    cores_text <- if (cores > 1) {
        paste0("#$ -pe local ", as.integer(cores), "\n")
    } else if (cores < 1) {
        stop("'cores' should be at least 1", call. = FALSE)
    } else {
        ## No need to specify -pe local 1
        ""
    }

    ## Specify the job queue
    queue <- if (queue == "shared" || queue == "") {
        ## There's no queue for shared
        ""
    } else {
        paste0(trimws(queue), ",")
    }

    ## Specify the array options if a task number was specified
    array_spec <- if (!is.null(task_num)) {
        paste0("#$ -t 1-", task_num, "\n#$ -tc ", tc, "\n")
    } else {
        ""
    }

    ## Create the logs directory
    if (create_logdir) {
        message(paste(Sys.time(), "creating the logs directory at: ", logdir))
        dir.create(logdir, showWarnings = FALSE)
    }

    ## Specify the log file
    log_file <- file.path(
        logdir,
        paste0(name, ifelse(!is.null(task_num), ".$TASK_ID", ""), ".txt")
    )

    ## For sgejobs version
    version <- packageVersion("sgejobs")

    ## Now build the script
    script <- glue::glue(
        '#!/bin/bash
#$ -cwd
#$ -l {queue}mem_free={memory},h_vmem={memory},h_fsize={filesize}
{cores_text}#$ -N {name}
#$ -o {log_file}
#$ -e {log_file}
#$ -m {email}
{array_spec}
echo "**** Job starts ****"
date

echo "**** JHPCE info ****"
echo "User: ${{USER}}"
echo "Job id: ${{JOB_ID}}"
echo "Job name: ${{JOB_NAME}}"
echo "Hostname: ${{HOSTNAME}}"
echo "Task id: ${{SGE_TASK_ID}}"

## Load the R module (absent since the JHPCE upgrade to CentOS v7)
module load conda_R

## List current modules for reproducibility
module list

## Edit with your job command
{command}

echo "**** Job ends ****"
date

## This script was made using sgejobs version {version}
## available from http://research.libd.org/sgejobs/

'
    )

    ## Write to a file?
    if (create_shell) {
        message(paste(Sys.time(), "creating the shell file", sh_file))
        message(paste("To submit the job use: qsub", sh_file))
        cat(script, file = sh_file)
        return(invisible(script))
    }

    ## Done!
    return(script)
}
