#' TODO
#'
#' @param user A `character(1)` vector specifying the username of the jobs to
#' query. Set NULL to return info about all users' jobs.
#' @param partition A `character(1)` vector specifying the partition of the jobs
#' to query. Set NULL to return info about jobs of all paritions.
#'
#' @return A tibble with job information about currently running jobs.
#' @export
#' @author Nicholas J. Eagles
#' @import dplyr
#'
#' @examples
#'
#' [TODO!]

job_info = function(user = Sys.getenv('USER'), partition = 'shared') {
    #   Read several key fields from squeue and parse into a tibble
    job_df = read.csv(
            text = system(
                'squeue --format="%u,%A,%K,%j,%P,%C,%m,%t"', intern = TRUE
            )
        ) |>
        as_tibble()
    
    #   Filter to the requested user and partition. Passing NULL values to
    #   those variables signals to not subset/ filter.
    if (!is.null(user)) job_df = job_df |> filter(USER == user)
    if (!is.null(partition)) job_df = job_df |> filter(PARTITION == partition)

    #   Do some initial data cleaning
    job_df = job_df |>
        #   Only interested in running jobs
        filter(ST == 'R') |>
        mutate(
            #   Fix NAs that are actually of character type
            ARRAY_TASK_ID = ifelse(ARRAY_TASK_ID == "N/A", NA, ARRAY_TASK_ID),
            #   Some character columns should be factors
            PARTITION = as.factor(PARTITION),
            STATUS = as.factor(ST)
        ) |>
        rename(
            JOB_ID = JOBID,
            REQUESTED_MEM = MIN_MEMORY
        ) |>
        select(-ST)
    colnames(job_df) = tolower(colnames(job_df))

    #   Form a list of tibbles, containing job ID and maximum physical and
    #   virtual memory used
    mem_df_list = lapply(
        #   Only have permission to check memory of this user's jobs
        job_df |> filter(USER == Sys.getenv('USER')) |> pull(job_id),
        function(x) {
            command = sprintf(
                'sstat -P -j %s --format="JobID,MaxRSS,MaxVMSize"', x
            )
            mem_df = read.csv(
                    text = system(command, intern = TRUE), sep = "|"
                ) |>
                as_tibble()
        }
    )

    #   Combine list of tibbles and merge with existing tibble
    job_df = do.call(rbind, mem_df_list) |>
        rename(
            job_id = JobID,
            max_rss = MaxRSS,
            max_vmem = MaxVMSize
        ) |>
        right_join(job_df)
}
