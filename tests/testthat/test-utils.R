#   Given a list of character vectors, where each list element represents a
#   group over which to loop independently from the other groups, verify the
#   uniqueness, completeness, and correctness of the list of combinations formed
#   from these groups. Note 'get_list_indexing' is the core function being
#   checked here.
#
#   An example correct output of 'get_list_indexing' with input
#   a = list(group1 = 'a', group2 = c('1', '2')) would yield subsetting
#   instructions to produce c("a_1", "a_2").
test_combinations = function(this_list) {
    #   Compute 'all_combos', a character vector where each element
    #   concatenates members of each group from 'this_list'.
    all_combos = c()

    total_elements = prod(sapply(this_list, length))
    for (i in 1:total_elements) {
        this_combo = c()
        for (j in 1:length(this_list)) {
            temp = get_list_indexing(this_list, j)
            this_combo = c(
                this_combo,
                this_list[[j]][[
                    i %/% temp[['divisor']] %% temp[['modulus']] + 1
                ]]
            )
        }
        all_combos = c(all_combos, paste(this_combo, collapse = "_"))
    }

    #   Verify that every combination used is unique
    expect_equal(length(all_combos), total_elements)
    expect_equal(length(unique(all_combos)), total_elements)

    combo_mat = matrix(
        unlist(strsplit(all_combos, '_')),
        byrow = TRUE,
        ncol = length(this_list)
    )

    #   Verify that each group member is used the correct number of times
    #   (technically, the above checks should be sufficient, but this more
    #   thoroughly verifies the actual content of the combinations)
    for (this_col in 1:ncol(combo_mat)) {
        #   How many times is each group member used?
        value_counts = as.numeric(table(combo_mat[,this_col]))

        #   All elements of the group should be used at least once
        expect_equal(length(value_counts), length(this_list[[this_col]]))

        #   Each group member should be used the same number of times
        expect_equal(
            all(value_counts == total_elements / length(this_list[[this_col]])),
            TRUE
        )
    }

    return(NULL)
}

#   Try three different list inputs to 'get_list_indexing' and verify it gives
#   instructions (divisor and modulus) for subsetting list groups to generate
#   complete, unique, and correct combinations
test_that(
    "get_list_indexing",
    {
        this_list = list(
            letter = c('a', 'b', 'c'),
            number = c('1', '2'),
            combo = c('one', 'two', 'three', 'four', 'five')
        )
        test_combinations(this_list)

        this_list = list(
            letter = c('a', 'b'),
            number = c('1', '2'),
            combo = c('one', 'two', 'three'),
            pet = c('cat', 'dog', 'parrot', 'fish'),
            planet = 'earth'
        )
        test_combinations(this_list)

        this_list = list(letter = c('a', 'b', 'c', 'd', 'e', 'f'))
        test_combinations(this_list)
    }
)
