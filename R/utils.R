#' Subset a specific list index given a particular array task index
#'
#' Given \code{this_list} and its \code{index}, determine a "divisor" and
#' "modulus" instructing how to subset the character vector at that index
#' given a particular array task index. Downstream, selecting
#' \code{this_list[[index]][[x]]} for each index yields a combination of values
#' that is unique across array indices. Here \code{x = array_index / divisor % modulus}.
#' This is a helper function for \code{job_loop}.
#'
#' @param this_list A \code{`list`} of character vectors
#' @param index \code{integer(1)} specifying the index of \code{this_list} to be
#' subsetted downstream of this function
#'
#' @return A length-2 named list of integer(1) vectors containing a divisor and
#' modulus
#'
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' array_task <- 5 # suppose this is the fifth task in an array job
#' index <- 2 # will refer to the 'feature' element of 'loops' below
#' loops <- list(
#'     region = c("DLPFC", "HIPPO"), feature = c("gene", "exon", "tx", "jxn")
#' )
#' indexing <- slurmjobs:::get_list_indexing(loops, index)
#' this_feature <- loops[[index]][[
#'     array_task %/% indexing$divisor %% indexing$modulus
#' ]]
#' sprintf(
#'     'The %ith array task will have "%s" as its feature.',
#'     array_task,
#'     this_feature
#' )
#'
get_list_indexing <- function(this_list, index) {
    if (index == length(this_list)) {
        divisor <- 1
    } else {
        divisor <- prod(
            sapply(this_list, length)[(index + 1):length(this_list)]
        )
    }
    modulus <- length(this_list[[index]])

    return(list("divisor" = divisor, "modulus" = modulus))
}

#' Find initials for a character vector
#'
#' Given a character vector 'vec', return an equal-length character vector of
#' unique one-letter initials. The first letter of each string will be used,
#' except when this results in duplicates; unique letters will be arbitrarily
#' assigned in alphabetical order in the case of duplicates.
#'
#' @param vec A character vector, assumed to start with alphabetical characters
#'
#' @return A character vector with the same length as \code{vec} giving unique
#' one-letter initials
#'
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' #    Simple example where initials are as expected: 'a', 'b', 'c'
#' fruits <- c("apple", "banana", "cherry")
#' initials <- slurmjobs:::get_short_flags(fruits)
#' print(fruits)
#'
#' #    Example with duplicates with some arbitrary initials: 'c', 'a', 'b', 'd'
#' fruits <- c("coconut", "cherry", "banana", "berry")
#' initials <- slurmjobs:::get_short_flags(fruits)
#' print(fruits)
#'
get_short_flags <- function(vec) {
    #   Try just taking the first letter of the names
    short_flags <- substr(vec, 1, 1)

    #   Get unused letters
    remaining_letters <- setdiff(letters, short_flags)
    if (length(remaining_letters) == 0) {
        stop("Can't handle more than 26 loops.")
    }

    #   Overwrite duplicates with unused letters
    dup_flags <- duplicated(short_flags)
    short_flags[dup_flags] <- remaining_letters[1:length(which(dup_flags))]

    return(short_flags)
}

#' Get the segment of code used to construct a given character vector
#'
#' Given a character vector 'vec', return a character(1) representing the line
#' of code used to generate 'vec'
#'
#' @param vec A character vector
#'
#' @return A \code{character(1)} representing the segment of code constructing
#' \code{vec}
#'
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' fruits <- c("apple", "banana", "cherry")
#' message(slurmjobs:::vector_as_code(fruits))
#'
vector_as_code <- function(vec) {
    return(sprintf('c("%s")', paste(vec, collapse = '", "')))
}

#' Parse time intervals reported by SLURM commands into \code{difftime}s
#'
#' Given a character vector 'vec', return a character(1) representing the line
#' of code used to generate 'vec'
#'
#' @param tim A \code{character()} representing time intervals as reported in
#' fields like \code{Elapsed} from \code{sacct}, in days-hours:mins:secs
#'
#' @return A \code{difftime()} vector of time intervals
#'
#' @import stringr lubridate
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' slurm_times <- c("0:00", "1:04:07", "11:03:02", "1-01:39:12", "33-14:40:54")
#' slurmjobs:::parse_slurm_time(slurm_times)
#'
parse_slurm_time <- function(tim) {
    #   First, reformat time string 'tim' to be in format
    #   [days]-[hours]:[mins]:[secs], with two digits for each quantity (other
    #   than days, which can be arbitrarily many digits in theory)
    base_time <- "0-00:00:00"
    full_time <- paste0(
        sapply(
            tim,
            function(x) substr(base_time, 1, nchar(base_time) - nchar(x))
        ),
        tim
    )

    #   Now, convert to 'difftime' objects. 'difftime' does not appear to
    #   directly handle parsing an arbitrary number of days, so we manually
    #   split the day and non-day components before adding together
    num_days <- full_time |>
        str_extract("^([0-9]+)-", group = 1) |>
        days() |>
        as.difftime()
    num_not_days <- full_time |>
        str_extract("^[0-9]+-(.*)$", group = 1) |>
        as.difftime("%H:%M:%S")

    #   Return the difftime
    return(num_days + num_not_days)
}

#' Parse script name or path into shell script path
#'
#' Given \code{name}, the absolute or relative path to an R or shell script,
#' with or without the file extension, return the path to the shell script.
#' Verify the \code{name} is legitimate in terms of file existence or existence
#' of the appropriate parent directory (if \code{name} is an absolute path).
#'
#' @param name A \code{character(1)} vector giving either the name or path
#' (relative or absolute) to the shell or R script to create
#' @param should_exist A \code{logical(1)} vector. If TRUE, the specified script
#' must already exist; if FALSE, it must not exist
#' @param r_ok A \code{logical(1)} vector. If TRUE, \code{name} may end in '.R'
#'
#' @return A \code{character(1)} giving the absolute or relative path to the
#' shell script
#'
#' @import stringr
#' @author Nicholas J. Eagles
#'
#' @examples
#'
#' acceptable_names <- c(
#'     # Absolute paths/ names may be used
#'     file.path(getwd(), "my_script.sh"),
#'     file.path(getwd(), "my_script.R"),
#'     file.path(getwd(), "my_script"),
#'     # Relative paths/ names are also acceptable
#'     "my_script.sh",
#'     "my_script.R",
#'     "my_script"
#' )
#' returned_scripts <- sapply(
#'     acceptable_names, slurmjobs:::parse_file_or_name,
#'     should_exist = FALSE, r_ok = TRUE
#' )
#' print(returned_scripts)
parse_file_or_name <- function(name, should_exist, r_ok) {
    #   Error if R script is passed when not 'r_ok'
    if (!r_ok && str_detect(name, "\\.R$")) {
        stop("Expected a name or path to a shell script, not an R script")
    }

    #   Convert name, R, or shell script extension into shell script extension
    name <- paste0(strsplit(name, "\\.(sh|R)$")[[1]], ".sh")

    #   Check if the file exists
    if (file.exists(name) != should_exist) {
        if (should_exist) {
            stop(paste(name, "does not exist"))
        } else {
            stop(paste(name, "already exists"))
        }
    }

    #   Parent directory must exist
    if (basename(name) != name) {
        if (!dir.exists(dirname(name))) {
            stop("Directory containing shell script must exist")
        }
    }

    return(name)
}
