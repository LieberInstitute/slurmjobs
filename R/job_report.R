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
        mutate(array_task_id = NA)
    
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

        #   Determine the range of task IDs this array covers
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

        #   Duplicate rows and overwrite with the pending task IDs. Then merge
        #   back into the original tibble
        a = job_df[rep(has_multiple_tasks, end - start + 1),]
        a$array_task_id = start:end
        job_df = job_df |>
            slice(-has_multiple_tasks) |>
            rbind(a)
    }
    
    #   The 'JobID' and 'JobIDRaw' fields from 'sacct' together contain 3
    #   pieces of info. Parse them into 3 columns
    job_df = job_df |>
        mutate(
            array_task_id = ifelse(
                is.na(array_task_id),
                as.integer(
                    str_extract(JobID, '[0-9]+_([0-9]+).*', group = 1)
                ),
                array_task_id
            ),
            JobID = str_extract(JobIDRaw, '^[0-9]+'),
            job_step = str_extract(JobIDRaw, '[0-9]+\\.?(.*)$', group = 1)
        )

    #   For some reason, SLURM reports the full range of tasks as PENDING, even
    #   when some tasks may be running (or in another state). Filter out
    #   PENDING array tasks for tasks that exist in another state in a different
    #   row
    job_df = job_df |>
        filter(
            !(
                (State == 'PENDING') &
                !is.na(array_task_id) &
                (array_task_id %in% job_df$array_task_id[
                        job_df$State != 'PENDING'
                    ]
                )
            )
        )
    
    #   For batch jobs, memory-related information is only reported in the
    #   'batch' job step, but all other info we care about is in the ordinary
    #   job step, called '' here. For interactive jobs, memory info appears to
    #   be in the '0' job step. Only applies for completed jobs!
    job_df[
        (job_df$job_step == '') & (job_df$State == 'COMPLETED'),
        c('MaxRSS', 'MaxVMSize')
    ] = job_df[
        job_df$job_step %in% c('batch', '0'),
        c('MaxRSS', 'MaxVMSize')
    ]
    
    #   For currently running jobs, 'sacct' doesn't report memory info. Use
    #   'sstat' instead
    running_jobs = job_df |>
        filter(State == 'RUNNING') |>
        pull(JobID) |>
        unique()
    
    if (length(running_jobs) > 0) {
        #   Form a list of tibbles with memory info
        mem_df_list <- lapply(
            running_jobs,
            function(x) {
                command <- sprintf(
                    'sstat -P -j %s --format="JobID,MaxRSS,MaxVMSize"', x
                )
                mem_df <- read.csv(
                    text = system(command, intern = TRUE), sep = "|"
                ) |>
                    as_tibble()
            }
        )

        #   Combine list of tibbles and merge with existing tibble
        job_df = do.call(rbind, mem_df_list) |>
            mutate(JobID = as.character(JobID)) |>
            #   Here we manually take whichever the non-NA value is for each
            #   memory-related column
            rename(max_rss = MaxRSS, max_vmem = MaxVMSize) |>
            right_join(job_df, by = 'JobID') |>
            mutate(
                MaxRSS = ifelse(is.na(MaxRSS), max_rss, MaxRSS),
                MaxVMSize = ifelse(is.na(MaxVMSize), max_vmem, MaxVMSize)
            ) |>
            select(-c(max_rss, max_vmem))
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
        select(-c(State, job_step, JobIDRaw))
    colnames(job_df) <- tolower(colnames(job_df))

    #   Given a character vector containing an amount of memory (containing
    #   a numeric piece and unit, e.g. "1.03G"), return a numeric vector
    #   with the amount in GB (e.g. 1.03).
    parse_memory_str <- function(mem_str) {
        #   Grab the numeric and character portions of the string. Verify
        #   one is NA only when the other is (an indirect way of suggesting
        #   parsing succeeded, and in particular memory units are expected)
        coeff <- as.numeric(str_extract(mem_str, "[0-9]+"))
        unit <- str_extract(mem_str, "[KMG]$")
        if (!all(is.na(coeff) == is.na(unit))) {
            stop("Failed to parse memory information. This is a slurmjobs bug!")
        }

        mem_num <- case_when(
            unit == "K" ~ coeff / 1e6,
            unit == "M" ~ coeff / 1e3,
            unit == "G" ~ coeff,
            TRUE ~ NA # note this case is impossible
        )

        return(mem_num)
    }

    #   Convert memory-related columns to numeric (in terms of GB)
    job_df = job_df |>
        mutate(
            across(
                c("max_rss_gb", "max_vmem_gb", "requested_mem_gb"),
                ~ parse_memory_str(.x)
            )
        )
    
    return(job_df)
}
