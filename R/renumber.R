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
#' @param recursive_edits A `logical(1)` indicating whether to recursively search
#' `base_dir` for R, shell, and Python scripts and replace any instances of the
#' prefices in `pre_before` with the corresponding prefices in `pre_after`.
#' Because of the slight risk where prefices are not uniquely identifiable
#' and unexpected edits take place, the default is `FALSE`; you must opt in
#' intentionally.
#' 
#' @return NULL
#' @export
#' @author Nicholas J. Eagles
#'
#' @importFrom fs is_absolute_path
#' @import stringr
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
renumber <- function(
        base_dir, pre_before, pre_after, expect_matches = TRUE,
        recursive_edits = FALSE
    ) {
    if (!dir.exists(base_dir)) {
        stop("'base_dir' must exist.")
    }
    if (length(pre_before) != length(pre_after)) {
        stop("'pre_before' and 'pre_after' must be the same length.")
    }

    all_files <- list.files(base_dir, full.names = TRUE)  
    for (x in pre_before) {
        matching_files <- basename(all_files)[
            grepl(paste0("^", x), basename(all_files))
        ]
      
        #   Check the all the prefices in 'pre_before' match a file in 'base_dir'
        if (expect_matches && (length(matching_files) == 0)) {
            stop(
                "Prefix '", x, "' did not match a file in 'base_dir'. Consider setting 'expect_matches = FALSE' to ignore missing prefixes."
            )
        }
        
        #   Prefices must uniquely specify basenames of files, a safety/
        #   convenience feature to prevent accidentally matching short strings
        #   in shell or other files and mistakenly modifying them
        matching_base = str_extract(
            matching_files, sprintf("^(%s[^.]*)\\.?.*", x), group = 1
        )
        if (length(unique(matching_base)) > 1) {
            stop(
                "Prefix '", x, "' matches multiple files with different base names, which is usually unintentional. If this was actually desired, consider modifying 'pre_before' to be more specific."
            )
        }
    }

    #   Gather a full renaming plan of all files (source and destination paths).
    #   Does not modify any files
    rename_plan_df = .renumber_loop(
        base_dir, pre_before, pre_after, all_files
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
    
    if (recursive_edits) {
        all_code_files = list.files(
            base_dir, pattern = "\\.(R|sh|py)$", full.names = TRUE,
            recursive = TRUE
        )
        full_pre_before = sapply(
            pre_before,
            function(x) {
                matching_files <- basename(all_files)[
                    grepl(paste0("^", x), basename(all_files))
                ]
                matching_base = stringr::str_extract(
                    matching_files, sprintf("^(%s[^.]*)\\.?.*", x), group = 1
                )
                #   Guaranteed to give one value based on earlier checks
                return(unique(matching_base)) 
            }
        )
        full_pre_after = stringr::str_replace(
            full_pre_before, paste0("^", pre_before),
            paste0(pre_after, 'temp_slurmjobs')
        )
        for (this_code_file in all_code_files) {
            this_code_file |>
                readLines() |>
                stringr::str_replace_all(
                    setNames(full_pre_after, full_pre_before)
                ) |>
                stringr::str_replace_all('temp_slurmjobs', '') |>
                writeLines(con = this_code_file)
        }
    }

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
        is_shell, negate_pattern = FALSE
    ) {
    second_condition = grepl(sprintf("%s$", file_regex), basename(all_files))
    if (negate_pattern) {
        second_condition = !second_condition
    }
    file_before <- all_files[
        grepl(sprintf("^%s", this_pre_before), basename(all_files)) &
        second_condition
    ]

    if (is_shell) {
        if (length(file_before) > 1) {
            stop(
                "Expected at most one shell script with prefix '", 
                this_pre_before, "'. Found ", length(file_before), "."
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

    log_files_before <- character(0)
    log_files_after <- character(0)
  
    if (is_shell) {
        file_content = readLines(file_before)
      
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
            is_shell = FALSE
        )
        log_files_before <- rename_df$before
        log_files_after <- rename_df$after
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
.renumber_loop = function(base_dir, pre_before, pre_after, all_files) {
    rename_plan_df_list = list()
    for (i in seq_len(length(pre_before))) {
        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.sh",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = TRUE
        )

        rename_plan_df_list[[length(rename_plan_df_list) + 1]] = .renumber_process_file(
            base_dir = base_dir,
            file_regex = "\\.sh",
            this_pre_before = pre_before[i],
            this_pre_after = pre_after[i],
            all_files = all_files,
            is_shell = FALSE,
            negate_pattern = TRUE
        )
    }
    rename_plan_df = do.call(rbind, rename_plan_df_list)
    return(rename_plan_df)
}
