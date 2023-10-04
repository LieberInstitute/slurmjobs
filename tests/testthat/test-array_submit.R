script_name <- "my_array_job.sh"

#   Write a basic shell script with 'job_single()'
basic_job <- function() {
    with_wd(
        tempdir(),
        {
            #   Delete the shell script if it exists
            unlink(script_name)

            job_single(
                name = script_name,
                create_shell = TRUE,
                task_num = 10,
                tc = 5
            )
        }
    )
}

#   Write a basic shell script but break it by removing the '--array' line
broken_job <- function() {
    with_wd(
        tempdir(),
        {
            basic_job()

            orig_script <- readLines(script_name)
            orig_script <- sub("^#SBATCH", "something", orig_script)
            writeLines(orig_script, script_name)
        }
    )
}

#   Create (write) some shell script with the 'shell_creation_fun' function,
#   then call 'array_submit' with the '...' arguments. Return the contents of
#   the original and modified shell script (list(2) of character vectors)
run_test <- function(shell_creation_fun, ...) {
    with_wd(
        tempdir(),
        {
            shell_creation_fun()

            original <- readLines(script_name)

            array_submit(
                job_bash = script_name,
                ...
            )

            final <- readLines(script_name)
            return(NULL)
        }
    )

    return(list("original" = original, "final" = final))
}

#   The shell script should not change when restore = TRUE
test_that(
    "array_submit_restore",
    {
        temp <- run_test(basic_job, submit = FALSE, task_ids = 1:3, restore = TRUE)
        expect_equal(temp$original, temp$final)
    }
)

test_that(
    "array_submit_numeric_task_ids",
    {
        temp <- run_test(
            basic_job,
            submit = FALSE, task_ids = 1:3, restore = FALSE
        )
        modified_line <- grep("^#SBATCH --array=1,2,3%5$", temp$final)

        #   The array line should be changed exactly as specified above, and all
        #   other lines should be identical
        expect_equal(length(modified_line), 1)
        expect_equal(temp$original[-modified_line], temp$final[-modified_line])
    }
)

#   Removing the '--array=' line should yield a specific error message
test_that(
    "array_submit_broken_script",
    {
        expect_error(
            run_test(
                broken_job,
                submit = FALSE, task_ids = 1:3, restore = FALSE
            ),
            "Could not find the line that specifies that this is an array job"
        )
    }
)

#   For an array job that didn't run, task IDs should fail to be automatically
#   found
test_that(
    "array_submit_null_task_ids",
    {
        expect_error(
            run_test(basic_job, submit = FALSE, restore = FALSE),
            "Please specify 'task_ids' explicitly"
        )
    }
)
