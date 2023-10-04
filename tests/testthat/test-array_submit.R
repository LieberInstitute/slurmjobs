script_name = 'my_array_job.sh'

run_test = function(shell_creation_fun, ...) {
    with_wd(
        tempdir(),
        {
            shell_creation_fun()

            original = readLines(script_name)

            array_submit(
                job_bash = script_name,
                ...
            )

            final = readLines(script_name)
            return(NULL)
        }
    )

    return(list("original" = original, "final" = final))
}
