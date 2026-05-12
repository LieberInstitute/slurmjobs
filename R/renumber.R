#' Re-order scripts and logs with a series of prefices (usually numbers)
#'
#' This function re-orders the prefices in names of scripts-- originally
#' intended to re-order the numbering of scripts developed in the
#' [R-Bioconductor-powered data science team's organization style](https://lcolladotor.github.io/bioc_team_ds/organizing-your-work.html#organization-since-2021),
#' where script names begin with `01`, `02`, etc. References to script names
#' within the shell or R scripts are also updated, along with the names of
#' log files they produce.
#'
#' @param base_dir A `character(1)` path to a directory containing the scripts
#' to re-order.
#' @param pre_before A `character()` vector of prefices to replace among scripts
#' in `base_dir`.
#' @param pre_after A `character()` vector of replacement prefices among scripts
#' in `base_dir`.
#' @param expect_matches A `logical(1)` indicating whether to expect that all
#' prefices in `pre_before` will match a file in `base_dir`, throwing an error if
#' not.
#' 
#' @return NULL
#' @export
#' @author Nicholas J. Eagles
#'
#' @importFrom fs is_absolute_path
#' 
#' @examples
#' base_dir <- file.path(tempdir(), "slurmjobs_scripts")
#' dir.create(base_dir)
#' 
#' #   Create a shell script that submits a corresponding R script
#' job_single(
#'     file.path(base_dir, "01_should_be_second.sh"),
#'     logdir = file.path(base_dir, "logs"), create_logdir = TRUE,
#'     create_shell = TRUE, command = "Rscript 01_should_be_second.R"
#' )
#' writeLines("# some code", con = file.path(base_dir, "01_should_be_second.R"))
#' 
#' #   Create an array originally designed to be submitted second
#' job_loop(
#'     file.path(base_dir, "02_should_be_first.sh"),
#'     create_shell = TRUE, logdir = file.path(base_dir, "logs"),
#'     loops = list(
#'         gene = c("gene_1", "gene_2"), method = c("method_1", "method_2")
#'     ) 
#' )
#'
#' #   Swap the order of the scripts
#' renumber(base_dir, c("01", "02"), c("02", "01"))
#' 
#' #   Check that the scripts have been properly renamed
#' list.files(base_dir)
renumber <- function(base_dir, pre_before, pre_after, expect_matches = TRUE) {
    if (!dir.exists(base_dir)) {
        stop("'base_dir' must exist.")
    }
    if (length(pre_before) != length(pre_after)) {
        stop("'pre_before' and 'pre_after' must be the same length.")
    }

    #   Check the all the prefices in 'pre_before' match a file in 'base_dir'
    all_files <- list.files(base_dir, full.names = TRUE)
    matches <- sapply(
        pre_before,
        function(x) length(grep(paste0("^", x), basename(all_files)))
    )
    if (expect_matches && !all(matches > 0)) {
        stop(
            "At least one prefix in 'pre_before' did not match a file in 'base_dir'. Consider setting 'expect_matches = FALSE' to ignore missing prefixes."
        )
    }

    #   Gather a full renaming plan of all files (source and destination paths),
    #   without modifying any files
    rename_plan_df = .renumber_loop(
        base_dir, pre_before, pre_after, all_files, dry_run = TRUE
    )
  
    #   Now simulate the plan to make sure it's safe. This could be done more
    #   cleanly with dplyr::replace_values() but I don't want to depend on such
    #   a recent version of dplyr
    ending_paths = all_files
    lookup = setNames(rename_plan_df$after, rename_plan_df$before)
    ending_paths = case_when(
            ending_paths %in% names(lookup) ~ lookup[ending_paths],
            TRUE ~ ending_paths
        ) |>
        unname()
    ending_paths = file.path(
        base_dir, sub('temp_slurmjobs$', '', basename(ending_paths))
    )
    if (length(unique(ending_paths)) != length(all_files)) {
        stop(
            "The proposed renaming plan would effectively result in deletion of at least one file or directory. Please check which files exist in 'base_dir' and if supplied prefixes would result in an overwrite."
        )
    }
    
    #   Now make the edits to the code files. Return the same plan as earlier
    rename_plan_df = .renumber_loop(
        base_dir, pre_before, pre_after, all_files, dry_run = FALSE
    )

    source_paths = rename_plan_df$before
    intermediate_paths = rename_plan_df$after
    destination_paths = file.path(
        dirname(intermediate_paths),
        sub('temp_slurmjobs$', '', basename(intermediate_paths))
    )
    file.rename(source_paths, intermediate_paths)
    file.rename(intermediate_paths, destination_paths)

    return(invisible(NULL))
}

#' Helper function to rename a single file pattern
#' 
#' This is employed by `renumber()` to individually and separately handle R
#' scripts, shell scripts, and logs
#' 
#' @author Nicholas J. Eagles
#' @keywords internal
.renumber_process_file = function(
        base_dir, file_regex, this_pre_before, this_pre_after, all_files,
        is_shell, edit_content, expected_one_file, negate_pattern = FALSE
    ) {
    second_condition = grepl(sprintf("%s$", file_regex), basename(all_files))
    if (negate_pattern) {
        second_condition = !second_condition
    }
    file_before <- all_files[
        grepl(sprintf("^%s", this_pre_before), basename(all_files)) &
        second_condition
    ]

    if (expected_one_file) {
        if (length(file_before) > 1) {
            stop(
                "Expected at most one file with prefix '", this_pre_before,
                "'. Found ", length(file_before), "."
            )
        }
    }

    if (length(file_before) == 0) {
        return(data.frame(before = character(0), after = character(0)))
    }

    full_pre_before <- stringr::str_extract(
        basename(file_before),
        sprintf("(^%s.*)%s$", this_pre_before, file_regex), group = 1
    )
    full_pre_after <- sub(
        paste0("^", this_pre_before), this_pre_after, full_pre_before
    )
    
    if (edit_content || is_shell) {
        file_content = readLines(file_before)
    }

    log_files_before <- character(0)
    log_files_after <- character(0)
  
    if (is_shell) {
        #   For 'job_loop', use the path in the line starting with
        #   'log_path='. Otherwise use the log in the line starting with
        #   '#SBATCH -o'.
        last_occurence <- rev(
            grep("^(#SBATCH -o |log_path=)", file_content)
        )[1]
        log_dir <- file_content[last_occurence] |>
            #   Extract just the path
            str_replace("^(#SBATCH -o |log_path=)", "") |>
            dirname()

        #   If the log is specified with a relative path, make sure it's
        #   relative to the directory containing the shell script
        if (!fs::is_absolute_path(log_dir)) {
            log_dir <- file.path(dirname(file_before), log_dir) |>
                normalizePath(mustWork = FALSE)
        }

        #   Use recursion to plan to rename logs, where they exist
        rename_df = .renumber_process_file(
            base_dir = log_dir,
            file_regex = "\\.(txt|log)",
            this_pre_before = this_pre_before,
            this_pre_after = this_pre_after,
            all_files = list.files(log_dir, full.names = TRUE),
            is_shell = FALSE,
            edit_content = FALSE,
            expected_one_file = FALSE
        )
        log_files_before <- rename_df$before
        log_files_after <- rename_df$after
    }

    if (edit_content) {
        #   Re-write the file in place, replacing references to the
        #   old prefix
        file_content <- gsub(full_pre_before, full_pre_after, file_content)
        writeLines(file_content, con = file_before)
    }

    #   Plan to rename scripts but append temporary suffix to avoid repeated
    #   renaming, since file.rename() executes renames sequentially
    file_after <- file.path(
        base_dir,
        paste0(
            sub(
                paste0("^", this_pre_before), this_pre_after,
                basename(file_before)
            ),
            "temp_slurmjobs"
        )
    )
    plan_df = data.frame(
        before = c(file_before, log_files_before),
        after = c(file_after, log_files_after)
    )

    return(plan_df)
}

#' Helper function to gather a plan for renaming all files
#' 
#' This is employed by `renumber()`. A data.frame is returned with columns
#' "before" and "after", indicating the full set of file paths to rename
#' 
#' @author Nicholas J. Eagles
#' @keywords internal
.renumber_loop = function(base_dir, pre_before, pre_after, all_files, dry_run) {
    rename_plan_df_list = list()
    for (i in seq_len(length(pre_before))) {
        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.(R|py)",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = FALSE,
            edit_content = !dry_run,
            expected_one_file = TRUE
        )

        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.sh",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = TRUE,
            edit_content = !dry_run,
            expected_one_file = TRUE
        )

        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.(R|py|sh)",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = FALSE,
            edit_content = FALSE,
            expected_one_file = FALSE,
            negate_pattern = TRUE
        )
    }
    rename_plan_df = do.call(rbind, rename_plan_df_list)
    return(rename_plan_df)
}
