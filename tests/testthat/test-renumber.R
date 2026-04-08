test_that(
    "renumber",
    {
        #   Create a temporary directory that's guaranteed to be empty
        base_dir <- file.path(tempdir(), "temp_slurmjobs")
        unlink(base_dir, recursive = TRUE)
        dir.create(base_dir)

        #   Create a set of scripts, some of which will be renumbered

        #   Legitimate scripts to renumber
        job_single(
            file.path(base_dir, "01_first.sh"),
            create_logdir = FALSE,
            create_shell = TRUE, command = "Rscript 01_first.R"
        )
        writeLines("# some code", con = file.path(base_dir, "01_first.R"))
        job_single(
            file.path(base_dir, "02_second.sh"),
            create_logdir = FALSE,
            create_shell = TRUE, command = "python 02_second.py"
        )
        job_loop(
            file.path(base_dir, "03_third.sh"),
            create_shell = TRUE,
            loops = list(a = c("a", "b"), b = c("c", "d")),
            logdir = file.path(base_dir, "logs")
        )

        #   Scripts with tricky names that should not be renumbered
        writeLines(
            "# some code",
            con = file.path(base_dir, "something_01_first.sh")
        )
        writeLines(
            "# some code",
            con = file.path(base_dir, "something_02_first.shtemp_slurmjobs")
        )

        #   Logs to renumber (skipping a couple intentionally)
        all_log_base_names <- c(
            "02_second.txt", "03_third_a_c.txt",
            "03_third_a_d.txt", "03_third_b_d.txt"
        )
        for (base_name in all_log_base_names) {
            writeLines(
                "# some log text",
                con = file.path(base_dir, "logs", base_name)
            )
        }

        renumber(base_dir, c("01", "02", "03"), c("02", "03", "01"))

        #   Check that the scripts have been properly renamed
        expected_files <- c(
            "02_first.sh", "02_first.R", "03_second.sh",
            "01_third.R", "01_third.sh", "something_01_first.sh",
            "something_02_first.shtemp_slurmjobs", "logs"
        )
        expect_equal(setequal(list.files(base_dir), expected_files), TRUE)

        #   Check that the logs have been properly renamed
        expected_files <- c(
            "03_second.txt", "01_third_a_c.txt", "01_third_a_d.txt",
            "01_third_b_d.txt"
        )
        expect_equal(
            setequal(list.files(file.path(base_dir, "logs")), expected_files),
            TRUE
        )

        #   Check that renumbering has occurred properly within all three shell
        #   scripts
        content <- readLines(file.path(base_dir, "02_first.sh"))
        expect_equal(any(grepl("^(01_first|03_first)", content)), FALSE)
        expect_equal(length(which(content == "Rscript 02_first.R")), 1)

        content <- readLines(file.path(base_dir, "03_second.sh"))
        expect_equal(any(grepl("^(02_second|01_second)", content)), FALSE)
        expect_equal(length(which(content == "python 03_second.py")), 1)

        content <- readLines(file.path(base_dir, "01_third.sh"))
        expect_equal(any(grepl("^(02_third|03_third)", content)), FALSE)
        expect_equal(length(grep("^Rscript 01_third\\.R", content)), 1)

        #   Check proper functioning of the 'plots_and_processed' parameter
        #   (when TRUE-- the other tests implicitly check FALSE)
        dir.create(file.path(base_dir, "code"))
        for (dir_type in c("processed-data", "plots")) {
            dir.create(file.path(base_dir, dir_type))
            dir.create(file.path(base_dir, dir_type, "01_second"))
            dir.create(file.path(base_dir, dir_type, "02_first"))
        }
        writeLines(
            c(
                "some_path = here('processed-data', '01_second', 'a.txt')",
                "some_variable = '01'"
            ),
            con = file.path(base_dir, "code", "01_second.R")
        )
        writeLines(
            "# some code",
            con = file.path(base_dir, "code", "02_first.R")
        )
        renumber(
            file.path(base_dir, "code"), c("01", "02"), c("02", "01"),
            plots_and_processed = TRUE
        )
        for (dir_type in c("processed-data", "plots")) {
             expect_identical(
                sort(list.files(file.path(base_dir, dir_type))),
                c("01_first", "02_second")
            )
        }
        expect_identical(
            sort(list.files(file.path(base_dir, "code"))),
            c("01_first.R", "02_second.R")
        )

        #   This is an especially tricky case, as even though the input prefix
        #   was "01", we want to only replace instances of "01_second" within
        #   the R script
        content = readLines(file.path(base_dir, "code", "02_second.R"))
        expect_identical(
            content,
            c(
                "some_path = here('processed-data', '02_second', 'a.txt')",
                "some_variable = '01'"
            )
        )

        #   Start with a clean temporary directory
        unlink(base_dir, recursive = TRUE)
        dir.create(base_dir)

        #   Attempt a sketchy renaming that would effectively delete files. It
        #   should both throw an error and not touch any files up until the 
        #   error is thrown
        job_single(
            file.path(base_dir, "01_first.sh"),
            create_logdir = FALSE,
            create_shell = TRUE, command = "Rscript 01_first.R"
        )
        writeLines("# some code", con = file.path(base_dir, "01_first.R"))
        job_single(
            file.path(base_dir, "02_second.sh"),
            create_logdir = FALSE,
            create_shell = TRUE, command = "python 02_second.py"
        )
        starting_files <- list.files(base_dir)
        expect_error(
            renumber(
                base_dir, c("01_first", "02_second"),
                c("02_something", "02_something")
            ),
            "^The proposed renaming plan"
        )
        expect_equal(setequal(list.files(base_dir), starting_files), TRUE)

        #   Attempt another dangerous rename that overwrites an existing file
        expect_error(
            renumber(base_dir, "01_first", "02_second"),
            "^The proposed renaming plan"
        )
    }
)
