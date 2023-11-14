#   Given a list of character vectors, where each list element represents a
#   group over which to loop independently from the other groups, verify the
#   uniqueness, completeness, and correctness of the list of combinations formed
#   from these groups. Note 'get_list_indexing' is the core function being
#   checked here.
#
#   An example correct output of 'get_list_indexing' with input
#   a = list(group1 = 'a', group2 = c('1', '2')) would yield subsetting
#   instructions to produce c("a_1", "a_2").
test_combinations <- function(this_list) {
    #   Compute 'all_combos', a character vector where each element
    #   concatenates members of each group from 'this_list'.
    all_combos <- c()

    total_elements <- prod(sapply(this_list, length))
    for (i in 1:total_elements) {
        this_combo <- c()
        for (j in 1:length(this_list)) {
            temp <- get_list_indexing(this_list, j)
            this_combo <- c(
                this_combo,
                this_list[[j]][[
                    i %/% temp[["divisor"]] %% temp[["modulus"]] + 1
                ]]
            )
        }
        all_combos <- c(all_combos, paste(this_combo, collapse = "_"))
    }

    #   Verify that every combination used is unique
    expect_equal(length(all_combos), total_elements)
    expect_equal(length(unique(all_combos)), total_elements)

    combo_mat <- matrix(
        unlist(strsplit(all_combos, "_")),
        byrow = TRUE,
        ncol = length(this_list)
    )

    #   Verify that each group member is used the correct number of times
    #   (technically, the above checks should be sufficient, but this more
    #   thoroughly verifies the actual content of the combinations)
    for (this_col in 1:ncol(combo_mat)) {
        #   How many times is each group member used?
        value_counts <- as.numeric(table(combo_mat[, this_col]))

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
        this_list <- list(
            letter = c("a", "b", "c"),
            number = c("1", "2"),
            combo = c("one", "two", "three", "four", "five")
        )
        test_combinations(this_list)

        this_list <- list(
            letter = c("a", "b"),
            number = c("1", "2"),
            combo = c("one", "two", "three"),
            pet = c("cat", "dog", "parrot", "fish"),
            planet = "earth"
        )
        test_combinations(this_list)

        this_list <- list(letter = c("a", "b", "c", "d", "e", "f"))
        test_combinations(this_list)
    }
)

test_that(
    "get_short_flags",
    {
        fruits <- c("coconut", "cherry", "banana", "berry")
        expect_equal(get_short_flags(fruits), c("c", "a", "b", "d"))

        #   Should fail for length > 26
        expect_error(
            get_short_flags(c(letters, letters)),
            "Can't handle more than 26 loops"
        )
    }
)

test_that(
    "vector_as_code",
    {
        fruits <- c("coconut", "cherry", "banana", "berry")
        initials <- vector_as_code(fruits)
        expect_equal(
            vector_as_code(fruits),
            'c("coconut", "cherry", "banana", "berry")'
        )
    }
)

test_that(
    "parse_slurm_time",
    {
        #   First, some typical cases using real output from the '%M'
        #   field of 'squeue'. The first uses a length > 1 vector as input
        expect_equal(
            parse_slurm_time(c("0:00", "1:04:07")),
            as.difftime(c(seconds(0), hours(1) + minutes(4) + seconds(7)))
        )
        expect_equal(
            parse_slurm_time("11:03:02"),
            as.difftime(hours(11) + minutes(3) + seconds(2))
        )
        expect_equal(
            parse_slurm_time("1-01:39:12"),
            as.difftime(days(1) + hours(1) + minutes(39) + seconds(12))
        )

        #   A case with more than 365 days
        expect_equal(
            parse_slurm_time("4000-00:00:00"),
            as.difftime(days(4000))
        )
    }
)

test_that(
    "parse_file_or_name",
    {
        #   Non-existent parent directory
        expect_error(
            parse_file_or_name(
                "/asdasda/a.sh",
                should_exist = FALSE, r_ok = FALSE
            ),
            "Directory containing shell script must exist"
        )

        #   Test 'should_exist' = TRUE
        expect_error(
            parse_file_or_name(
                file.path(tempdir(), "asdas"),
                should_exist = TRUE, r_ok = FALSE
            ),
            "does not exist"
        )

        #   Test 'should_exist' = FALSE
        writeLines("something", "a.sh")
        expect_error(
            parse_file_or_name(
                "a.sh",
                should_exist = FALSE, r_ok = FALSE
            ),
            "already exists"
        )

        #   Test 'r_ok' = FALSE
        expect_error(
            parse_file_or_name(
                file.path(tempdir(), "a.R"),
                should_exist = FALSE, r_ok = FALSE
            ),
            "Expected a name or path to a shell script, not an R script"
        )

        #   Test 'r_ok' = TRUE
        expect_equal(
            parse_file_or_name(
                file.path(tempdir(), "a.R"),
                should_exist = FALSE, r_ok = TRUE
            ),
            file.path(tempdir(), "a.sh")
        )

        #   Also try just a name
        expect_equal(
            parse_file_or_name("b", should_exist = FALSE, r_ok = FALSE),
            "b.sh"
        )
    }
)
