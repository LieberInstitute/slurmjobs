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
