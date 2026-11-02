test_that("batched inventories retain scalar stamps and all original entries", {
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    nested <- file.path(root, "analysis", ".hidden")
    dir.create(nested, recursive = TRUE)
    writeBin(as.raw(1:10), file.path(nested, "result.bin"))
    writeLines("original", file.path(root, "analysis", "script.R"))
    paths <- sort(c(file.path(root, "analysis"), nested,
        file.path(nested, "result.bin"), file.path(root, "analysis", "script.R")))
    expected <- stats::setNames(lapply(paths, .renumber_stamp), paths)
    expect_identical(.renumber_inventory(c(file.path(root, "analysis"), nested)), expected)
    expect_identical(.renumber_inventory(character()), list())
    missing <- sort(file.path(root, c("absent", "absent-other")))
    # fs versions may return missing metadata or signal an error; preserve either.
    scalar <- tryCatch(stats::setNames(lapply(missing, .renumber_stamp), missing),
        error = conditionMessage)
    batched <- tryCatch(.renumber_inventory(missing), error = conditionMessage)
    expect_identical(batched, scalar)
})

test_that("batched inventories record directory links without traversing them", {
    skip_on_os("windows")
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    analysis <- file.path(root, "analysis")
    target <- file.path(root, "target")
    dir.create(analysis)
    dir.create(target)
    writeLines("do not traverse", file.path(target, "result.bin"))
    link <- file.path(analysis, "linked")
    expect_true(file.symlink(target, link))
    paths <- c(analysis, link)
    expect_identical(.renumber_inventory(analysis),
        stats::setNames(lapply(paths, .renumber_stamp), paths))
})

test_that("batched scope checks accept existing missing repeated and native paths", {
    # Default separators deliberately exercise backslashes on Windows.
    root <- normalizePath(withr::local_tempdir())
    dir.create(file.path(root, "existing"))
    path <- file.path(root, "existing", "script.R")
    writeLines("original", path)
    paths <- c(path, file.path(root, "missing", "nested", "script.R"),
        file.path(root, "existing", "..", "existing", "script.R"), path)
    expect_no_error(.renumber_assert_within(paths, root, reject_aliases = TRUE))
    expect_no_error(.renumber_assert_within(character(), root, reject_aliases = TRUE))
    expect_error(.renumber_assert_within(c(path, NA_character_), root), "invalid path")
    expect_error(.renumber_assert_within(c(path, ""), root), "invalid path")
})

test_that("batched checks preserve path failure ordering and diagnostics", {
    skip_on_os("windows")
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    base <- file.path(root, "code")
    dir.create(base)
    target <- file.path(base, "script.R")
    writeLines("original", target)
    link <- file.path(base, "linked.R")
    dangling <- file.path(base, "dangling.R")
    outside <- file.path(root, "outside.R")
    writeLines("outside", outside)
    expect_true(file.symlink(target, link))
    expect_true(file.symlink(file.path(root, "absent"), dangling))
    message_for <- function(paths) conditionMessage(tryCatch(
        .renumber_assert_within(paths, base, reject_aliases = TRUE), error = identity))
    expect_identical(message_for(c(link, outside, dangling)),
        paste0("The proposed renaming plan includes a linked or non-regular target: ", link))
    expect_identical(message_for(c(outside, dangling, link)),
        paste0("The proposed renaming plan includes a path outside 'base_dir' (", base, "): ", outside))
    expect_identical(message_for(c(dangling, link, outside)),
        paste0("The proposed renaming plan includes a dangling linked path: ", dangling))
    expect_no_error(.renumber_assert_within(c(target, link), base))
})

test_that("batched scope checks do not reuse metadata across invocations", {
    skip_on_os("windows")
    root <- normalizePath(withr::local_tempdir(), winslash = "/")
    base <- file.path(root, "code")
    dir.create(base)
    path <- file.path(base, "script.R")
    outside <- file.path(root, "outside.R")
    writeLines("original", path)
    writeLines("outside", outside)
    expect_no_error(.renumber_assert_within(path, base, reject_aliases = TRUE))
    expect_true(file.remove(path))
    expect_true(file.symlink(outside, path))
    expect_error(.renumber_assert_within(path, base, reject_aliases = TRUE), "outside")
})
