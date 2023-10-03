#' Build a bash script that loops over variables and submits SLURM jobs
#'
#' This function builds a bash script that loops over a set of variables
#' with pre-specified values to create an internal bash script that then
#' gets submitted as a SLURM job.
#'
#' @param loops A named `list` where each of the elements are character vectors.
#' The names of `loops` specify the variables used for the loops and the
#' contents specify the options to loop through for each variable.
#' @inheritParams job_single
#'
#' @return A character vector with the script contents. If `create_shell` was
#' specified then it also creates the actual script in the current
#' working directory.
#' @export
#' @author Leonardo Collado-Torres
#' @author Nicholas J. Eagles
#' @import glue purrr
#'
#' @examples
#'
#' job_loop(
#'     loops = list(region = c("DLPFC", "HIPPO"), feature = c("gene", "exon", "tx", "jxn")),
#'     name = "bsp2_test"
#' )
#'
#' job_loop(
#'     loops = list(region = c("DLPFC", "HIPPO"), feature = c("gene", "exon", "tx", "jxn")),
#'     cores = 5,
#'     task_num = 10,
#'     name = "bsp2_test_array"
#' )
#'
job_loop <- function(
        loops, name, create_shell = FALSE, partition = "shared", memory = "10G",
        cores = 1L, email = "ALL", logdir = "logs", task_num = NULL, tc = 20) {
    ## Check that the loops are correctly defined
    if (!is.list(loops)) {
        stop("'loops' should be a named list.", call. = FALSE)
    }
    if (length(names(loops)) != length(loops)) {
        stop("'loops' should be a named list.", call. = FALSE)
    }
    if (!all(purrr::map_lgl(loops, is.character))) {
        stop("All elements of 'loops' should be character vectors.", call. = FALSE)
    }

    ## Remove any spaces
    name <- gsub(" ", "_", name)

    ## Check if the shell file exists already
    if (create_shell) {
        sh_file <- paste0(name, ".sh")
        if (file.exists(sh_file)) {
            stop("The file ", sh_file, " already exists!", call. = FALSE)
        }
    }

    loop_builder <- function(loop, loop_values) {
        loop_values <- paste(loop_values, collapse = " ")
        glue::glue("for {loop} in {loop_values}; do\n")
    }

    loop_header <- purrr::map2_chr(names(loops), loops, loop_builder)
    if (length(loop_header) > 1) {
        ## Add the corresponding spaces
        header_spaces <- purrr::map2_chr(
            rep(" ", length(loop_header)),
            c(0, seq_len(length(loop_header) - 1)) * 4,
            ~ paste0(rep(.x, .y), collapse = "")
        )
        loop_header <- paste0(header_spaces, loop_header, collapse = "\n")
    }


    ## Build an example command
    command <- paste0(
        'Rscript -e "options(width = 120); ',
        "print('{",
        paste(names(loops), collapse = "}'); print('{"),
        "}');", ' sessioninfo::session_info()"'
    )

    ## Define the name of the jobs inside the loop
    inside_name <- paste0(name, "_{", paste(names(loops), collapse = "}_{"), "}")


    ## Build the core script
    script <- job_single(
        name = "{SHORT}",
        partition = partition,
        memory = memory,
        cores = cores,
        email = email,
        logdir = logdir,
        task_num = task_num,
        tc = tc,
        command = command,
        create_logdir = FALSE
    )

    ## Deal with the escaping
    script <- gsub(
        "\\$SLURM_ARRAY_TASK_ID", "\\\\$SLURM_ARRAY_TASK_ID",
        gsub(
            "\\{", "\\$\\{",
            gsub("\\$\\{", "\\\\{", script)
        )
    )
    inside_name <- gsub("\\{", "\\$\\{", inside_name)


    ## Indentation
    indent <- paste(rep(" ", 4 * length(loop_header)), collapse = "")

    script_header <- glue::glue(
        '#!/bin/bash

## Usage:
# sh {name}.sh

## Create the logs directory
mkdir -p {logdir}

{loop_header}

{indent}## Internal script name
{indent}SHORT="{inside_name}"

{indent}# Construct shell file
{indent}echo "Creating script {inside_name}"
{indent}cat > .${{SHORT}}.sh <<EOF
'
    )

    footer_spaces <- purrr::map2_chr(
        rep(" ", length(loops)),
        c(0, seq_len(length(loops) - 1)) * 4,
        ~ paste0(rep(.x, .y), collapse = "")
    )
    footer <- paste0(rev(footer_spaces), "done", collapse = "\n")

    script_footer <- glue::glue('
{indent}call="sbatch .${{SHORT}}.sh"
{indent}echo $call
{indent}$call
{footer}

')

    script_final <- glue::glue("{script_header}
{script}

EOF

{script_footer}
")

    ## Write to a file?
    if (create_shell) {
        message(paste(Sys.time(), "creating the shell file", sh_file))
        message(paste("To execute the script builder, use: sh", sh_file))
        cat(script_final, file = sh_file)
        return(invisible(script_final))
    }

    ## Done!
    return(script_final)
}
