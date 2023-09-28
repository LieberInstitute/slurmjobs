## Choose a script name
job_name <- paste0("job_single_test_", Sys.Date())


run_test <- function(delete = TRUE, ...) {
    ## Delete it in case it exists
    if (delete) unlink(file.path(tempdir(), paste0(job_name, ".sh")))

    ## Create an array job on the temporary directory
    with_wd(tempdir(), {
        ## Create an array job script to use for this example
        job_single(
            name = job_name,
            ...
        )
    })
}

test_that("job_single", {
    expect_error(run_test(email = "hello"), "'email' should be one of")
    expect_error(run_test(logdir = "/logs"), "relative path")
    expect_error(run_test(cores = 0.5), "should be at least 1")
    expect_error(run_test(memory = 10), "invalid memory request")
    expect_error(run_test(memory = "5GB"), "invalid memory request")
    run_test()
    expect_error(run_test(delete = FALSE, create_shell = TRUE), "already exists!")
})
