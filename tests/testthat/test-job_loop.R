## Choose a script name
job_name <- paste0("job_loop_test_", Sys.Date())


run_test <- function(delete = TRUE, ...) {
    ## Delete it in case it exists
    if (delete) unlink(file.path(tempdir(), paste0(job_name, ".sh")))

    ## Create an array job on the temporary directory
    with_dir(tempdir(), {
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
    expect_error(
        run_test(loops = list("a" = letters), memory = 10),
        "Could not parse memory request"
    )
    expect_error(
        run_test(memory = "5GB", loops = list("a" = letters)),
        "Could not parse memory request"
    )
    run_test(create_shell = TRUE, loops = list("a" = letters))
    expect_error(
        run_test(
            delete = FALSE, loops = list("b" = letters), create_shell = TRUE
        ),
        "already exists!"
    )
})
