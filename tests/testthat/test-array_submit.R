library(withr)

script_name <- "my_array_job"

#   Write a basic shell script with 'job_single()'
basic_job <- function() {
    #   Delete the shell script if it exists
    unlink(paste0(script_name, ".sh"))

    job_single(
        name = script_name,
        create_shell = TRUE,
        task_num = 10,
        tc = 5
    )
}

#   Write a basic shell script but break it by removing the '--array' line
broken_job <- function() {
    basic_job()

    orig_script <- readLines(paste0(script_name, ".sh"))
    orig_script <- sub("^#SBATCH", "something", orig_script)
    writeLines(orig_script, paste0(script_name, ".sh"))
}

#   Create (write) some shell script with the 'shell_creation_fun' function,
#   then call 'array_submit' with the '...' arguments. Return the contents of
#   the original and modified shell script (list(2) of character vectors)
run_test <- function(shell_creation_fun, ...) {
    with_dir(
        tempdir(),
        {
            shell_creation_fun()

            original <- readLines(paste0(script_name, ".sh"))

            array_submit(
                name = script_name,
                ...
            )

            final <- readLines(paste0(script_name, ".sh"))
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

test_that("array_submit repeatedly infers task IDs without restoring", {
    local_dir(local_tempdir())
    local_mocked_bindings(
        job_report = function(job_id) {
            expect_identical(job_id, expected_job_id)
            data.frame(
                array_task_id = c(failed_tasks, setdiff(current_tasks, failed_tasks)),
                status = c(
                    rep("FAILED", length(failed_tasks)),
                    rep("COMPLETED", length(setdiff(current_tasks, failed_tasks)))
                )
            )
        },
        .package = "slurmjobs"
    )

    for (limit in c("%5", "")) {
        basic_job()
        sh_file <- paste0(script_name, ".sh")
        script <- readLines(sh_file)
        script <- sub("^(#SBATCH --array=1-10)%5$", paste0("\\1", limit), script)
        writeLines(script, sh_file)
        log_dir <- "logs"
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
        current_tasks <- 1:10

        for (failed_tasks in list(c(8L, 3L, 6L), c(3L, 6L), 3L, 3L)) {
            expected_job_id <- as.character(1000L + max(current_tasks))
            writeLines(
                paste0("Job id: ", expected_job_id),
                file.path(log_dir, paste0(script_name, "_", max(current_tasks), ".txt"))
            )
            expect_no_error(array_submit(script_name, restore = FALSE, verbose = TRUE))
            updated_script <- readLines(sh_file)
            array_line <- grep("^#SBATCH --array=", updated_script)
            expect_identical(
                updated_script[array_line],
                paste0("#SBATCH --array=", paste(failed_tasks, collapse = ","), limit)
            )
            expect_identical(updated_script[-array_line], script[-array_line])
            current_tasks <- failed_tasks
        }
    }
})

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

test_that(
    "array_submit_full_path",
    {
        #   Effectively just testing that a full script path doesn't break
        #   array_submit
        script_name <- file.path(tempdir(), "my_array_job")
        temp <- run_test(basic_job, submit = FALSE, task_ids = 1:3, restore = TRUE)
        expect_equal(temp$original, temp$final)
    }
)
