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
            #   For a group of pending array tasks, take the full JobID (e.g.
            #   '254220_[1-10%10]'). Otherwise, take just the initial number
            #   (e.g. '254220').
            JobID = str_extract(JobID, '^[0-9]+(_\\[[0-9]+-[0-9]+%[0-9]+\\]$)?'),
            job_step = str_extract(JobIDRaw, '[0-9]+\\.?(.*)$', group = 1)
        )
    
    #   Pending array tasks are output in a single row, but we want one row per
    #   unique task. Break into multiple rows if this is an array job with
    #   pending tasks
    has_multiple_tasks = grep('_\\[[0-9]+-[0-9]+%[0-9]+\\]$', job_df$JobID)
    if (length(has_multiple_tasks) > 0) {
        #   Verify assumptions: there should only be one row associated with
        #   multiple task IDs, and this should only occur for pending tasks
        if (length(has_multiple_tasks) > 1) {
            stop("Bug: only expected one row containing a range of pending array tasks.")
        }
        if (job_df[has_multiple_tasks, 'State'] != 'PENDING') {
            stop("Bug: multiple array tasks per row only expected to occur for pending tasks.")
        }

        #   Determine the range of task IDs this array covers. Use 'setdiff' as
        #   it appears the full range of task IDs is reported, even when 
        start = as.numeric(
            str_extract(
                job_df[has_multiple_tasks, 'JobID'],
                '.*_\\[([0-9]+)-.*',
                group = 1
            )
        )
        end = as.numeric(
            str_extract(
                job_df[has_multiple_tasks, 'JobID'],
                '.*_\\[[0-9]+-([0-9]+)%.*',
                group = 1
            )
        )
        pending_ids = setdiff(start:end, job_df$array_task_id)

        #   Duplicate rows and overwrite with the pending task IDs. Then merge
        #   back into the original tibble
        a = job_df[rep(has_multiple_tasks, length(pending_ids)),]
        a$array_task_id = pending_ids
        a$job_step = 'pending'
        job_df = job_df |>
            slice(-has_multiple_tasks) |>
            rbind(a)
    }
    
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
        filter(job_step %in% c('', 'pending')) |>
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
        select(-c(State, job_step, JobIDRaw))
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
