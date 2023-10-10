library(dplyr)

partition_info <- function(partition = "shared", all_nodes = FALSE) {
    command = paste(
        #   Grab info about CPUs and memory with 'sinfo'
        "sinfo -N -O 'PartitionName,NodeList:50,FreeMem,AllocMem,Memory,StateLong,CPUsState'",
        #   Properly delimit columns by "|"
        "sed -E 's%[ /\t]+%|%g'",
        #   Use better column names for CPU-related columns
        "sed 's%CPUS(A|I|O|T)%ALLOC_CPUS|INACTIVE_CPUS|OTHER_CPUS|TOTAL_CPUS%'",
        #   Remove trailing delimiter
        "sed 's%|$%%'",
        sep = " | "
    )

    part_df = read.csv(text = system(command, intern = TRUE), sep = "|") |>
        as_tibble()
    
    #   Subset by partition if not null
    if(!is.null(partition)) {
        part_df = part_df |> filter(PARTITION == partition)
    }

    colnames(part_df) = tolower(colnames(part_df))

    part_df = part_df |>
        #   Use more informative and consistent column names
        rename(
            node = nodelist,
            alloc_mem_gb = allocmem,
            total_mem_gb = memory,
            free_mem_gb = free_mem
        ) |>
        mutate(
            state = as.factor(state),
            #   Convert from MB to GB to match other slurmjobs functions
            across(matches('.*_mem_gb$'), ~ .x / 1000)
        )

    #   When 'all_nodes' is FALSE, sum across all nodes within a partition to
    #   provide summary statistics
    if (! all_nodes) {
        part_df = part_df |>
            group_by(partition) |>
            #   Note free resources don't count hypothetically free resources (i.e. CPUs
            #   or memory that aren't being used, but aren't available for use now).
            #   'total*' columns do include these hypothetical resources
            summarize(
                free_cpus = sum(inactive_cpus[state %in% c('mixed', 'idle')]),
                total_cpus = sum(total_cpus),
                prop_free_cpus = free_cpus / total_cpus,
                free_mem_gb = sum(free_mem_gb[state %in% c('mixed', 'idle')]),
                total_mem_gb = sum(total_mem_gb),
                prop_free_mem_gb = free_mem_gb / total_mem_gb
            )
    }

    return (part_df)
}
    