#' Given the ID for a completed or currently running SLURM job, return a tibble
#' containing outputs from 'sacct'. In particular, returns information about
#' job name, ID, user, status, and allocated and used memory and CPUs.
#'
#' @param job_id The ID associated with a finished or currently running SLURM
#' job (character(1) or integer(1))
#'
#' @return A tibble with information about the requested job.
#' @export
#' @author Nicholas J. Eagles
#' @import dplyr stringr utils
#'
#' @examples
#'
#' #    Must be run in a SLURM environment
#' if (system("which sbatch") == 0) {
#'     job_df = job_report('234904')
#'     
#'     #    Check max virtual memory reached by this job
#'     print(job_df$max_vmem_gb)
#' }
job_report <- function(job_id) {
    job_df <- read.csv(
        text = system(
            sprintf(
                'sacct -j %s -P -o "JobID,JobIDRaw,User,JobName,Partition,AllocCPUS,ReqMem,State,MaxRSS,MaxVMSize" --units=G',
                job_id
            ),
            intern = TRUE
        ),
        sep = "|",
        na.strings = c("", "NA")
    ) |>
        as_tibble() |>
        mutate(
            array_task_id = as.integer(
                str_extract(JobID, '[0-9]+_([0-9]+).*', group = 1)
            ),
            JobID = str_extract(JobIDRaw, '^[0-9]+'),
            job_step = str_extract(JobIDRaw, '[0-9]+\\.?(.*)$', group = 1)
        ) |>
        select(-JobIDRaw)
    
    #   For batch jobs, memory-related information is only reported in the
    #   'batch' job step, but all other info we care about is in the ordinary
    #   job step, called '' here. For interactive jobs, 'sacct' doesn't return
    #   memory info
    if ('batch' %in% job_df$job_step) {
        job_df[job_df$job_step == '', c('MaxRSS', 'MaxVMSize')] = job_df[
            job_df$job_step == 'batch',
            c('MaxRSS', 'MaxVMSize')
        ]
    }

    #   Clean up column names and types
    job_df <- job_df |>
        #   Drop redundant job steps
        filter(job_step == '') |>
        #   Some character columns should be factors
        mutate(
            Partition = as.factor(Partition),
            status = as.factor(State)
        ) |>
        #   Make compatible with output from 'job_info'
        rename(
            job_id = JobID,
            name = JobName,
            cpus = AllocCPUS,
            requested_mem_gb = ReqMem,
            max_vmem_gb = MaxVMSize,
            max_rss_gb = MaxRSS
        ) |>
        select(-c(State, job_step))
    colnames(job_df) <- tolower(colnames(job_df))

    #   Convert memory-related columns to numeric (in terms of GB)
    job_df = job_df |>
        mutate(
            across(
                c("max_rss_gb", "max_vmem_gb", "requested_mem_gb"),
                ~ as.numeric(sub('G', '', .x))
            )
        )
    
    return(job_df)
}
