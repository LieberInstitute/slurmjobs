#' Build a SLURM job bash script
#'
#' This function builds a template for a Simple Linux Utility for Resource
#' Management (SLURM) job script including array jobs. Check this blog post by
#' John Muschelli to learn more about array jobs:
#' <https://hopstat.wordpress.com/2013/11/05/array-and-sequential-cluster-jobs/>.
#'
#' For a given SLURM job that is currently running you can alter
#' the options using `scontrol alter`.
#'
#' @param name A `character(1)` vector with the name of the script. Any spaces
#' will be replaced by underscores.
#' @param create_shell A `logical(1)` vector specifying whether to create a
#' shell file for the script.
#' @param partition A `character(1)` vector with the name of the SLURM
#' partition. Check how busy a given partition is by running [TODO].
#' @param memory The amount of memory per core to request. You can check how
#' much a current job is utilizing using the `sstat` command.
#' @param cores The number of cores to request. Note that the total memory
#' your job will request is `cores` multiplied by `memory`.
#' @param email The email reporting option for the email address ("BEGIN",
#' "END", "FAIL", or "ALL")
#' @param logdir The directory for the log files relative to the current
#' working directory.
#' @param task_num The number of tasks for your job, which will make it into an
#' array job. If `NULL` this is ignored.
#' @param tc If `task_num` is specified, this option controls the number of
#' concurrent tasks.
#' @param command An example command to start your script.
#' @param create_logdir A `logical(1)` vector specifying whether to create the
#' `logdir` directory. Note that if `logdir` doesn't exist and you submit your
#' job with `sbatch`, it will immediately fail.
#'
#' @return A character vector with the script contents. If `create_shell` was
#' specified then it also creates the actual script in the current
#' working directory.
#' @export
#' @author Leonardo Collado-Torres
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' ## A regular job
#' job_single("jhpce_job", create_logdir = FALSE)
#'
#' ## A regular job with 10 cores on the 'imaginary' partition
#' job_single("jhpce_job",
#'     cores = 10, partition = "imaginary",
#'     create_logdir = FALSE
#' )
#'
#' ## An array job
#' job_single("jhpce_job_array", task_num = 20, create_logdir = FALSE)
#'
job_single <- function(
        name, create_shell = FALSE, partition = "shared", memory = "10G",
        cores = 1L, email = "e", logdir = "logs", task_num = NULL, tc = 20,
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
    valid_email_opts <- c("BEGIN", "END", "FAIL", "ALL")
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

    if (cores < 1) {
        stop("'cores' should be at least 1", call. = FALSE)
    }
    cores = as.integer(cores)

    ## Specify the job partition
    partition <- paste0(trimws(queue), ",")

    ## Specify the array options if a task number was specified
    array_spec <- if (!is.null(task_num)) {
        paste0("#SBATCH --array=1-", task_num, "%", tc, "\n")
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
        paste0(
            name,
            ifelse(!is.null(task_num), ".$SLURM_ARRAY_TASK_ID", ""),
            ".txt"
        )
    )

    ## For slurmjobs version
    version <- packageVersion("slurmjobs")

    ## Now build the script
    script <- glue::glue(
        '#!/bin/bash
#SBATCH -p {partition}
#SBATCH --mem-per-cpu={memory}
#SBATCH --job-name={name}
#SBATCH -c {cores}
#SBATCH -o {log_file}
#SBATCH -e {log_file}
#SBATCH --mail-type={email}
{array_spec}

echo "**** Job starts ****"
date

echo "**** JHPCE info ****"
echo "User: ${{USER}}"
echo "Job id: ${{SLURM_JOB_ID}}"
echo "Job name: ${{SLURM_JOB_NAME}}"
echo "Node name: ${{SLURMD_NODENAME}}"
echo "Task id: ${{SLURM_ARRAY_TASK_ID}}"

## Load the R module
module load conda_R

## List current modules for reproducibility
module list

## Edit with your job command
{command}

echo "**** Job ends ****"
date

## This script was made using slurmjobs version {version}
## available from http://research.libd.org/slurmjobs/

'
    )

    ## Write to a file?
    if (create_shell) {
        message(paste(Sys.time(), "creating the shell file", sh_file))
        message(paste("To submit the job use: sbatch", sh_file))
        cat(script, file = sh_file)
        return(invisible(script))
    }

    ## Done!
    return(script)
}
