#   All tests here are tested locally and not as part of the usual GHA CI
#   workflow, since SLURM isn't available on the cloud machines

test_that(
    "partition_info_null_partition_not_all_nodes",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        part_df <- partition_info(partition = NULL, all_nodes = FALSE)

        #   No values should be NA 
        expect_equal(any(is.na(part_df)), FALSE)

        #   There should be more than one partition in the testing environment
        expect_equal(length(unique(part_df$partition)) > 1, TRUE)

        #   Compare expected vs. actual data types for each expected column
        expected_types <- c(
            free_cpus = "integer", total_cpus = "integer",
            prop_free_cpus = "numeric", free_mem_gb = "numeric",
            total_mem_gb = "numeric", prop_free_mem_gb = "numeric",
            partition = "factor"
        )
        expected_types <- unname(expected_types[colnames(part_df)])

        actual_types <- part_df |>
            summarize_all(class) |>
            as.character()
        
        expect_equal(expected_types, actual_types)
    }
)

test_that(
    "partition_info_shared_partition_all_nodes",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        part_df <- partition_info(partition = "shared", all_nodes = TRUE)

        #   No values should be NA 
        expect_equal(any(is.na(part_df)), FALSE)

        #   We specified just one partition
        expect_equal(unique(part_df$partition), 'shared')

        #   The 'shared' partition has many compute nodes (on the local test
        #   environment)
        expect_equal(nrow(part_df) > 1, TRUE)

        #   Compare expected vs. actual data types for each expected column
        expected_types = c(
            partition = 'factor', node = 'character', free_mem_gb = 'numeric',
            alloc_mem_gb = 'numeric', total_mem_gb = 'numeric',
            state = 'factor', alloc_cpus = 'integer', inactive_cpus = 'integer',
            other_cpus = 'integer', total_cpus = 'integer'
        )
        expected_types <- unname(expected_types[colnames(part_df)])

        actual_types <- part_df |>
            summarize_all(class) |>
            as.character()
        
        expect_equal(expected_types, actual_types)
    }
)
