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

#   Given a character vector 'loops', return an equal-length character vector of
#   unique one-letter initials
get_short_flags = function(loops) {
    #   Try just taking the first letter of the names
    short_flags = substr(loops, 1, 1)

    #   Get unused letters
    remaining_letters = setdiff(letters, short_flags)
    if (length(remaining_letters) == 0) {
        stop("Can't handle more than 26 loops.")
    }

    #   Overwrite duplicates with unused letters
    dup_flags = duplicated(short_flags)
    short_flags[dup_flags] = remaining_letters[1:length(which(dup_flags))]

    return(short_flags)
}

#   Given a character vector 'vec', return a character(1) representing the line
#   of code used to generate 'vec'
vector_as_code = function(vec) {
    return(sprintf('c("%s")', paste(vec, collapse = '", "')))
}
