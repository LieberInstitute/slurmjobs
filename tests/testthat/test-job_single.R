## Choose a script name
job_name <- paste0("job_single_test_", Sys.Date())


run_test <- function(delete = TRUE, ...) {
    ## Delete it in case it exists
    if (delete) unlink(file.path(tempdir(), paste0(job_name, ".sh")))

    ## Create an array job on the temporary directory
    with_dir(tempdir(), {
        ## Create an array job script to use for this example
        job_single(
            name = job_name,
            ...
        )
    })
}

test_that("job_single", {
    expect_error(run_test(email = "hello"), "'email' should be one of")
    expect_error(run_test(cores = 0.5), "should be at least 1")
    expect_error(run_test(memory = 10), "Could not parse memory request")
    expect_error(run_test(memory = "5GB"), "Could not parse memory request")

    #   Checking several invalid time limits
    for (limit in c('nonsense', '2-9000:30', '30:00-2', '7:7:7:7:7')) {
        expect_error(run_test(time_limit = limit), "^Invalid 'time_limit'")
    }

    #   Checking several valid time limits
    for (limit in c('04:30', '2-03:30:40', '55', '1-12')) {
        expect_no_error(run_test(time_limit = limit))
    }

    run_test(create_shell = TRUE)
    expect_error(run_test(delete = FALSE, create_shell = TRUE), "already exists")
})
