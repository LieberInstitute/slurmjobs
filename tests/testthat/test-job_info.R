#   All tests here are tested locally and not as part of the usual GHA CI
#   workflow, since SLURM isn't available on the cloud machines

test_that(
    "job_info_null_user_partition",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        job_df <- job_info(user = NULL, partition = NULL)

        #   Memory should be defined for a user's jobs
        any_mem_na <- job_df |>
            filter(user == Sys.getenv("USER")) |>
            select(max_rss_gb, max_vmem_gb) |>
            is.na() |>
            any()
        expect_equal(any_mem_na, FALSE)

        #   Most columns should never have NA values
        any_other_na <- job_df |>
            select(
                job_id, user, name, partition, cpus, requested_mem_gb, status
            ) |>
            is.na() |>
            any()
        expect_equal(any_other_na, FALSE)

        #   There should be more than one user and partition since user = NULL
        #   and partition = NULL (well, except in extremely unlikely cases)
        expect_equal(length(unique(job_df$user)) > 1, TRUE)
        expect_equal(length(unique(job_df$partition)) > 1, TRUE)
    }
)

test_that(
    "job_info_data_types",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        job_df <- job_info(user = NULL, partition = NULL)

        #   Compare expected vs. actual data types for each expected column
        expected_types <- c(
            job_id = "numeric", max_rss_gb = "numeric", max_vmem_gb = "numeric",
            user = "character", array_task_id = "integer", name = "character",
            partition = "factor", cpus = "integer",
            requested_mem_gb = "numeric", status = "factor"
        )
        expected_types <- unname(expected_types[colnames(job_df)])

        actual_types <- job_df |>
            summarize_all(class) |>
            as.character()

        expect_equal(expected_types, actual_types)
    }
)

test_that(
    "job_info_defined_user_partition",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        job_df <- job_info(user = Sys.getenv("USER"), partition = "shared")
        expect_equal(unique(job_df$user), Sys.getenv("USER"))
        
        #   We specified just one partition, but there should be many levels to
        #   the factor
        expect_equal(as.character(unique(job_df$partition)), 'shared')
        expect_equal(nlevels(job_df$partition) > 1, TRUE)
    }
)
