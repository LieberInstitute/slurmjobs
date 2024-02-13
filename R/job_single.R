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
#' @param name A `character(1)` vector giving either the name or path (relative
#' or absolute) to the shell script to create.
#' @param create_shell A `logical(1)` vector specifying whether to create a
#' shell file for the script.
#' @param partition A `character(1)` vector with the name of the SLURM
#' partition. Check how busy a given partition is by running
#' `squeue -p [partition]`.
#' @param memory character(1): the amount of memory in total to request, as a
#' number and unit accepted by the '--mem' SLURM parameter (e.g. '10G')
#' @param cores The number of cores to request. Note that the total memory
#' your job will request is `cores` multiplied by `memory`.
#' @param time_limit character(1): time limit specified in a format accepted by
#' the \code{--time} parameter to \code{sbatch} (e.g. "4:00:00"). Defaults to 1
#' day, following the JHPCE default (https://jhpce.jhu.edu/knowledge-base/setting-a-time-limit-for-your-slurm-job-on-jhpce/).
#' @param email The email reporting option for the email address ("BEGIN",
#' "END", "FAIL", or "ALL")
#' @param logdir The directory to contain the log, as an absolute or relative
#' path.
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
#' specified then it also creates the actual script.
#' @export
#' @author Leonardo Collado-Torres
#' @author Nicholas J. Eagles
#' @import glue utils
#'
#' @family shell-script creation and submission functions
#'
#' @examples
#'
#' ## A regular job, specified by name or path
#' job_single("jhpce_job", create_logdir = FALSE)
#' job_single("~/jhpce_job.sh", create_logdir = FALSE)
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
        cores = 1L, time_limit = "1-00:00:00", email = "ALL", logdir = "logs", task_num = NULL, tc = 20,
        command = 'Rscript -e "options(width = 120); sessioninfo::session_info()"',
        create_logdir = TRUE) {
    #   Grab the path to the shell script, and the shell script's name,
    #   respectively
    sh_file <- parse_file_or_name(name, should_exist = FALSE, r_ok = FALSE)
    name <- strsplit(basename(sh_file), "\\.sh$")[[1]]

    ## Check the email options
    valid_email_opts <- c("BEGIN", "END", "FAIL", "ALL")
    if (!email %in% valid_email_opts) {
        stop("'email' should be one of the following options:\n",
            paste(valid_email_opts, collapse = ", "),
            call. = FALSE
        )
    }

    ## Check the validity of core and memory requests
    if (cores < 1) {
        stop("'cores' should be at least 1", call. = FALSE)
    }
    cores <- as.integer(cores)

    if (!grepl("^[1-9][0-9]*[KMGT]$", memory)) {
        stop(
            "Could not parse memory request. Must be a character containing a positive integer and units 'K', 'M', 'G', or 'T'.",
            call. = FALSE
        )
    }

    ## Specify the array options if a task number was specified
    array_spec <- if (!is.null(task_num)) {
        paste0("#SBATCH --array=1-", task_num, "%", tc, "\n")
    } else {
        ""
    }

    #   Check for valid-looking time specification. Some invalid edge cases
    #   currently break the overly simple regex (e.g. '00-00:00', '1-25:61:61'),
    #   but these are unlikely to be specified by accident
    if (!grepl("^([0-9]+-)?[0-9]{1,2}(:[0-9]{1,2}){0,2}$", time_limit)) {
        stop("Invalid 'time_limit' specification. See https://slurm.schedmd.com/sbatch.html for accepted time formats.")
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
            ifelse(!is.null(task_num), ".%a", ""),
            ".txt"
        )
    )

    ## For slurmjobs version
    version <- packageVersion("slurmjobs")

    ## Now build the script
    script <- glue::glue(
        '#!/bin/bash
#SBATCH -p {partition}
#SBATCH --mem={memory}
#SBATCH --job-name={name}
#SBATCH -c {cores}
#SBATCH -t {time_limit}
#SBATCH -o {log_file}
#SBATCH -e {log_file}
#SBATCH --mail-type={email}
{array_spec}
set -e

echo "**** Job starts ****"
date

echo "**** JHPCE info ****"
echo "User: ${{USER}}"
echo "Job id: ${{SLURM_JOB_ID}}"
echo "Job name: ${{SLURM_JOB_NAME}}"
echo "Node name: ${{SLURMD_NODENAME}}"
echo "Task id: ${{SLURM_ARRAY_TASK_ID}}"

## Load the R module
module load conda_R/4.3

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
