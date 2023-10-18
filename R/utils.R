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
#' indexing <- get_list_indexing(loops, index)
#' this_feature <- this_list[[index]][[
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
#' initials <- get_short_flags(fruits)
#' print(fruits)
#'
#' #    Example with duplicates with some arbitrary initials: 'c', 'a', 'b', 'd'
#' fruits <- c("coconut", "cherry", "banana", "berry")
#' initials <- get_short_flags(fruits)
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
#' message(vector_as_code(fruits))
#'
vector_as_code <- function(vec) {
    return(sprintf('c("%s")', paste(vec, collapse = '", "')))
}
