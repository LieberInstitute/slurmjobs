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

        #   Scripts with tricky names that should not be renumbered
        writeLines(
            '# some code',
            con = file.path(base_dir, 'something_01_first.sh')
        )
        writeLines(
            '# some code',
            con = file.path(base_dir, 'something_02_first.shtemp_slurmjobs')
        )

        renumber(base_dir, c('01', '02'), c('02', '03'))

        #   Check that the files have been properly renamed
        expected_files = c(
            '02_first.sh', '02_first.R', '03_second.sh',
            'something_01_first.sh', 'something_02_first.shtemp_slurmjobs'
        )
        expect_equal(setequal(list.files(base_dir), expected_files), TRUE)

        #   Check that renumbering has occurred properly within both shell
        #   scripts
        content = readLines(file.path(base_dir, '02_first.sh'))
        expect_equal(any(grepl('^01_first', content)), FALSE)
        expect_equal(length(which(content == 'Rscript 02_first.R')), 1)

        content = readLines(file.path(base_dir, '03_second.sh'))
        expect_equal(any(grepl('^02_second', content)), FALSE)
        expect_equal(length(which(content == 'python 03_second.py')), 1)
    }
)
