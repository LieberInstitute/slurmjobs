partition_df <- partition_info(partition = NULL, all_nodes = FALSE) |>
    #   Randomly choose 5 partitions
    slice_sample(n = 5) |>
    #   Remove information about true partition names
    mutate(partition = paste0("partition_", 1:5))

usethis::use_data(partition_df, overwrite = TRUE)
