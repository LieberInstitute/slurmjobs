test_that(
    "renumber",
    {
        base_dir = tempdir()

        #   Create a set of scripts, some of which will be renumbered
        job_single(
            file.path(base_dir, '01_first.sh'), create_logdir = FALSE,
            create_shell = TRUE, command = 'Rscript 01_first.R'
        )
        writeLines('# some code', con = file.path(base_dir, '01_first.py'))
        writeLines(
            '# some code', con = file.path(base_dir, 'something_01_first.sh')
        )
        job_single(
            file.path(base_dir, '02_second.sh'), create_logdir = FALSE,
            create_shell = TRUE, command = 'python 02_second.py'
        )

        renumber(base_dir, c('01', '02'), c('02', '03'))

        #   Check that the files have been properly renamed
        expected_files = c(
            '02_first.sh', '02_first.py', 'something_01_first.sh', '03_second.sh'
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
