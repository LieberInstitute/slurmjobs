#   All tests here are tested locally and not as part of the usual GHA CI
#   workflow, since SLURM isn't available on the cloud machines

test_that(
    "job_report_completed_array",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        #   Use a job ID for a completed array job
        job_df <- job_report(270112)

        #   No values should be NA
        expect_equal(any(is.na(job_df)), FALSE)

        #   Compare expected vs. actual data types for each expected column
        expected_types <- c(
            job_id = "integer", user = "character", name = "character",
            cpus = "integer", requested_mem_gb = "numeric",
            max_rss_gb = "numeric", max_vmem_gb = "numeric",
            partition = "factor", array_task_id = "integer",
            exit_code = "integer", status = "factor",
            wallclock_time = "difftime"
        )
        expected_types <- unname(expected_types[colnames(job_df)])

        actual_types <- job_df |>
            summarize_all(class) |>
            as.character()

        expect_equal(expected_types, actual_types)
    }
)

test_that(
    "job_report_completed_batch",
    {
        #   Don't test on GHA, where SLURM isn't available
        skip_on_ci()

        #   Use a job ID for a completed non-array job
        job_df <- job_report("270157")

        #   No values should be NA except array_task_id
        expect_equal(any(is.na(job_df |> select(-array_task_id))), FALSE)
        expect_equal(all(is.na(job_df$array_task_id)), TRUE)
    }
)
