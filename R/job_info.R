#' Return a tibble containing information about currently running jobs.
#'
#' This function imports the output of 'squeue' into tibble, also adding
#' information about maximal virtual memory and RSS used by jobs of this user
#' (you).
#'
#' @param user A `character(1)` vector specifying the username of the jobs to
#' query. Set NULL to return info about all users' jobs.
#' @param partition A `character(1)` vector specifying the partition of the jobs
#' to query. Set NULL to return info about jobs of all paritions.
#'
#' @return A tibble with job information about currently running jobs.
#' @export
#' @author Nicholas J. Eagles
#' @import dplyr stringr
#'
#' @examples
#'
#' #    Must be run in a SLURM environment where a 'shared' partition exists
#' job_df <- job_info(partition = "shared")
#'
#' #    Sum up requested CPUs across all running jobs
#' print(sprintf("I'm using %s CPUs total right now.", sum(job_df$cpus)))
job_info <- function(user = Sys.getenv("USER"), partition = "shared") {
    #   Read several key fields from squeue and parse into a tibble
    job_df <- read.csv(
        text = system(
            'squeue --format="%u|%A|%K|%j|%P|%C|%m|%t"',
            intern = TRUE
        ),
        sep = "|"
    ) |>
        as_tibble()

    #   Filter to the requested user and partition. Passing NULL values to
    #   those variables signals to not subset/ filter.
    if (!is.null(user)) job_df <- job_df |> filter(USER == user)
    if (!is.null(partition)) job_df <- job_df |> filter(PARTITION == partition)

    #   Do some initial data cleaning
    job_df <- job_df |>
        #   Only interested in running jobs
        filter(ST == "R") |>
        mutate(
            #   Fix NAs that are actually of character type
            ARRAY_TASK_ID = ifelse(ARRAY_TASK_ID == "N/A", NA, ARRAY_TASK_ID),
            #   Some character columns should be factors
            PARTITION = as.factor(PARTITION),
            STATUS = as.factor(ST)
        ) |>
        rename(
            JOB_ID = JOBID,
            REQUESTED_MEM_GB = MIN_MEMORY
        ) |>
        select(-ST)
    colnames(job_df) <- tolower(colnames(job_df))

    these_job_ids <- job_df |>
        filter(user == Sys.getenv("USER")) |>
        pull(job_id)

    if (length(these_job_ids) > 0) {
        #   Form a list of tibbles, containing job ID and maximum physical and
        #   virtual memory used
        mem_df_list <- lapply(
            #   Only have permission to check memory of this user's jobs
            these_job_ids,
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
        job_df <- do.call(rbind, mem_df_list) |>
            rename(
                job_id = JobID,
                max_rss_gb = MaxRSS,
                max_vmem_gb = MaxVMSize
            ) |>
            right_join(job_df)


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

        #   Clean up memory-related columns so they're numeric and represent
        #   amounts in GB
        job_df <- job_df |>
            mutate(
                across(
                    c("max_rss_gb", "max_vmem_gb", "requested_mem_gb"),
                    ~ parse_memory_str(.x)
                )
            )
    } else {
        #   Even though no memory information is returned, define the relevant
        #   columns for consistent output behavior
        job_df$max_rss_gb <- NA
        job_df$max_vmem_gb <- NA
    }

    return(job_df)
}
