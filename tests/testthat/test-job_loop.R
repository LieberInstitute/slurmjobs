## Choose a script name
job_name <- paste0("job_loop_test_", Sys.Date())


run_test <- function(delete = TRUE, ...) {
    ## Delete it in case it exists
    if (delete) unlink(file.path(tempdir(), paste0(job_name, ".sh")))

    ## Create an array job on the temporary directory
    with_wd(tempdir(), {
        ## Create an array job script to use for this example
        job_loop(
            name = job_name,
            ...
        )
    })
}

test_that("job_loop", {
    expect_error(run_test(loops = "hello"), "should be a named list")
    expect_error(run_test(loops = list("hello")), "should be a named list")
    expect_error(
        run_test(loops = list("hello" = 1)),
        "All elements of 'loops' should be character vectors."
    )
    expect_error(
        run_test(email = "hello", loops = list("a" = letters)),
        "'email' should be one of"
    )
    expect_error(
        run_test(logdir = "/logs", loops = list("a" = letters)),
        "relative path"
    )
    expect_error(
        run_test(cores = 0.5, loops = list("a" = letters)),
        "should be at least 1"
    )
    expect_equal(grep("pe local", run_test(
        cores = 1,
        loops = list("a" = letters)
    )), integer(0))
    expect_equal(grepl("#\\$ -pe local 10", run_test(
        cores = 10,
        loops = list("a" = letters)
    )), TRUE)
    expect_equal(grep("shared", run_test(
        queue = "shared",
        loops = list("a" = letters)
    )), integer(0))
    expect_equal(grepl(
        "#\\$ -l bluejay,",
        run_test(
            queue = " bluejay ", create_shell = TRUE,
            loops = list("a" = letters[1:5], "b" = letters[6:10])
        )
    ), TRUE)
    expect_error(run_test(FALSE,
        create_shell = TRUE,
        loops = list("a" = letters)
    ), "already exists!")
})
