#' Build a bash script that loops over variables and submits SLURM jobs
#'
#' This function builds a bash script functioning as an array job that loops
#' over a set of variables with pre-specified values.
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
#' @author Nicholas J. Eagles
#' @author Leonardo Collado-Torres
#' @import purrr
#' 
#' @family shell-script creation and submission functions
#'
#' @examples
#'
#' job_loop(
#'     loops = list(region = c("DLPFC", "HIPPO"), feature = c("gene", "exon", "tx", "jxn")),
#'     name = "bsp2_test_array",
#'     cores = 2
#' )
#'
job_loop <- function(
        loops, name, create_shell = FALSE, partition = "shared", memory = "10G",
        cores = 1L, tc = 20, email = "ALL", logdir = "logs") {
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

    ## Check if the shell or R script exists already
    if (create_shell) {
        file_names = c(paste0(name, ".sh"), paste0(name, ".R"))
        for (this_file in file_names) {
            if (file.exists(this_file)) {
                stop("The file ", this_file, " already exists!", call. = FALSE)
            }
        }
    }

    ## Build the command, which invokes an R script with at least one parameter
    main_command = sprintf('Rscript %s.R', name)
    all_args = paste(
        sapply(
            names(loops), function(x) sprintf('--%s ${%s}', x, x)
        ),
        collapse = " "
    )
    command <- paste(main_command, all_args)

    ## The text for the corresponding R script
    r_text = c(
        'library(getopt)',
        'library(sessioninfo)',
        '',
        '# Import command-line parameters',
        'spec <- matrix(',
        '    c(',
        sprintf('        %s,', vector_as_code(names(loops))),
        sprintf('        %s,', vector_as_code(get_short_flags(names(loops)))),
        sprintf('        rep("1", %s),', length(loops)),
        sprintf('        rep("character", %s),', length(loops)),
        sprintf('        rep("Add variable description here", %s)', length(loops)),
        '    ),',
        '    ncol = 5',
        ')',
        'opt <- getopt(spec)',
        '',
        'print("Using the following parameters:")',
        'print(opt)'
    )

    ## Build the core script
    script <- job_single(
        name = name,
        partition = partition,
        memory = memory,
        cores = cores,
        email = email,
        logdir = logdir,
        command = command,
        #   The number of tasks is the product of lengths of each loop
        task_num = prod(sapply(loops, length)),
        create_logdir = FALSE
    )

    #   Given integer(1) 'i', an index of 'loops', return a character vector
    #   whose elements represent lines of bash code. This code creates an
    #   array (the contents of loops[[i]]) and subsets it appropriately
    #   given the array's task ID
    make_bash_statements <- function(i) {
        #   Define a bash array containing all elements of this loop
        all_variable <- sprintf(
            "all_%s=(%s)",
            names(loops)[i],
            paste(loops[[i]], collapse = " ")
        )

        #   Calculate the divisor and modulus, which determine the appropriate
        #   way to index the variable associated with this loop:
        #       index = ([SLURM_ARRAY_TASK_ID] // [divisor]) % [modulus]
        temp <- get_list_indexing(loops, i)

        #   The bash code for indexing this loop's variable given the task ID
        this_variable <- sprintf(
            "%s=${all_%s[$(( $SLURM_ARRAY_TASK_ID / %s %% %s ))]}",
            names(loops)[i],
            names(loops)[i],
            temp[["divisor"]],
            temp[["modulus"]]
        )

        return(c(all_variable, this_variable, ""))
    }

    #   Most of the script's content is created via 'job_single'
    script_core <- job_single(
        name = name,
        partition = partition,
        memory = memory,
        cores = cores,
        email = email,
        logdir = logdir,
        command = command,
        #   The number of tasks is the product of lengths of each loop
        task_num = prod(sapply(loops, length)),
        create_logdir = FALSE
    ) |>
        #   Convert to a character vector with elements as lines of the file
        str_split("\\n")

    #   Write to /dev/null, since we'll directly pipe outputs to a file
    script_core <- script_core[[1]] |>
        str_replace("^(#SBATCH -[oe]) .*", "\\1 /dev/null")

    #   Get the numbers of key lines. We'll have to insert different
    #   pieces into this original script at these points
    set_e_line <- grep("^set -e$", script_core)
    version_line <- grep(
        "^## This script was made using slurmjobs version", script_core
    )

    script_final <- c(
        script_core[1:(set_e_line - 1)],
        #   Define the variables through which to loop, and subset based on the
        #   array task ID
        "## Define loops and appropriately subset each variable for the array task ID",
        unlist(lapply(1:length(loops), make_bash_statements)),
        #   Define the log path: will contain each subsetted variable as well as
        #   the array task ID
        "## Explicitly pipe script output to a log",
        sprintf(
            "log_path=%s/%s_${%s}_${SLURM_ARRAY_TASK_ID}.txt",
            logdir,
            name,
            paste(names(loops), collapse = "}_${")
        ),
        "",
        "{",
        script_core[set_e_line:(version_line - 1)],
        "} > $log_path 2>&1",
        "",
        script_core[version_line:length(script_core)]
    )

    ## Write to a file?
    if (create_shell) {
        message(
            sprintf(
                "%s Creating the shell file %s.sh and corresponding R script %s.R",
                Sys.time(), name, name
            )
        )
        message(paste("To submit the script pair, use: sbatch", sh_file))
        writeLines(r_text, con = paste0(name, ".R"))
        writeLines(script_final, con = sh_file)
        return(invisible(script_final))
    }

    ## Done!
    return(script_final)
}
