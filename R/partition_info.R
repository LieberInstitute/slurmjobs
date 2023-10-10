library(dplyr)

job_info <- function(partition = "shared", all_nodes = FALSE) {
    command = paste(
        #   Grab info about CPUs and memory with 'sinfo'
        "sinfo -N -O 'PartitionName,NodeList:50,FreeMem,AllocMem,Memory,StateLong,CPUsState'",
        #   Properly delimit columns by "|"
        "sed -E 's%[ /\t]+%|%g'",
        #   Use better column names for CPU-related columns
        "sed 's%CPUS(A|I|O|T)%ALLOCATED_CPUS|INACTIVE_CPUS|OTHER_CPUS|TOTAL_CPUS%'",
        #   Remove trailing 
        "sed 's%|$%%'",
        sep = " | "
    )

    part_df = read.csv(text = system(command, intern = TRUE), sep = "|") |>
        as_tibble()
    
    #   Subset by partition if not null
    if(!is.null(partition)) {
        part_df = part_df |> filter(PARTITION == partition)
    }

    part_df |>
        group_by(PARTITION) |>
        #   Note free resources don't count hypothetically free resources (i.e. CPUs
        #   or memory that aren't being used, but aren't available for use now).
        #   'total*' columns do include these hypothetical resources
        summarize(
            free_cpus = sum(INACTIVE_CPUS[STATE %in% c('mixed', 'idle')]),
            free_mem = sum(FREE_MEM[STATE %in% c('mixed', 'idle')]),
            total_cpus = sum(TOTAL_CPUS),
            total_mem = sum(MEMORY),
            prop_free_mem = free_mem / total_mem,
            prop_free_cpus = free_cpus / total_cpus
        )
}
    