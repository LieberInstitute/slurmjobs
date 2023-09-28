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
    expect_equal(grep("pe local", run_test(cores = 1)), integer(0))
    expect_equal(grepl("#\\$ -pe local 10", run_test(cores = 10)), TRUE)
    expect_equal(grep("shared", run_test(queue = "shared")), integer(0))
    expect_equal(grepl(
        "#\\$ -l bluejay,",
        run_test(queue = " bluejay ", create_shell = TRUE)
    ), TRUE)
    expect_error(run_test(FALSE, create_shell = TRUE), "already exists!")
})
