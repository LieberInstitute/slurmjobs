test_that(
    "renumber",
    {
        #   Create a temporary directory that's guaranteed to be empty
        base_dir = file.path(tempdir(), 'temp_slurmjobs')
        dir.create(base_dir)

        #   Create a set of scripts, some of which will be renumbered

        #   Legitimate scripts to renumber
        job_single(
            file.path(base_dir, '01_first.sh'), create_logdir = FALSE,
            create_shell = TRUE, command = 'Rscript 01_first.R'
        )
        writeLines('# some code', con = file.path(base_dir, '01_first.R'))
        job_single(
            file.path(base_dir, '02_second.sh'), create_logdir = FALSE,
            create_shell = TRUE, command = 'python 02_second.py'
        )
        job_loop(
            file.path(base_dir, '03_third.sh'), create_shell = TRUE,
            loops = list(a = c('a', 'b'), b = c('c', 'd')),
            logdir = "logs"
        )

        #   Scripts with tricky names that should not be renumbered
        writeLines(
            '# some code',
            con = file.path(base_dir, 'something_01_first.sh')
        )
        writeLines(
            '# some code',
            con = file.path(base_dir, 'something_02_first.shtemp_slurmjobs')
        )

        #   Logs to renumber (skipping a couple intentionally)
        dir.create(file.path(base_dir, 'logs'))
        all_log_base_names = c(
            '02_second.txt', '03_third_a_c.txt',
            '03_third_a_d.txt', '03_third_b_d.txt'
        )
        for (base_name in all_log_base_names) {
            writeLines(
                '# some log text',
                con = file.path(base_dir, 'logs', base_name)
            )
        }

        renumber(base_dir, c('01', '02', '03'), c('02', '03', '01'))

        #   Check that the scripts have been properly renamed
        expected_files = c(
            '02_first.sh', '02_first.R', '03_second.sh',
            '01_third.R', '01_third.sh', 'something_01_first.sh',
            'something_02_first.shtemp_slurmjobs', 'logs'
        )
        expect_equal(setequal(list.files(base_dir), expected_files), TRUE)

        #   Check that the logs have been properly renamed
        expected_files = c(
            '03_second.txt', '01_third_a_c.txt', '01_third_a_d.txt',
            '01_third_b_d.txt'
        )
        expect_equal(
            setequal(list.files(file.path(base_dir, 'logs')), expected_files),
            TRUE
        )

        #   Check that renumbering has occurred properly within all three shell
        #   scripts
        content = readLines(file.path(base_dir, '02_first.sh'))
        expect_equal(any(grepl('^(01_first|03_first)', content)), FALSE)
        expect_equal(length(which(content == 'Rscript 02_first.R')), 1)

        content = readLines(file.path(base_dir, '03_second.sh'))
        expect_equal(any(grepl('^(02_second|01_second)', content)), FALSE)
        expect_equal(length(which(content == 'python 03_second.py')), 1)

        content = readLines(file.path(base_dir, '01_third.sh'))
        expect_equal(any(grepl('^(02_third|03_third)', content)), FALSE)
        expect_equal(length(grep('^Rscript 01_third\\.R', content)), 1)
    }
)
