job_report <- function(job_id, partition = "shared") {
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
    
    #   Filter to the requested partition. Passing NULL signals to not subset/
    #   filter.
    if (!is.null(partition)) job_df <- job_df |> filter(Partition == partition)

    #   Clean up column names and types
    job_df <- job_df |>
        mutate(
            #   Some character columns should be factors
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
        select(-State)
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
