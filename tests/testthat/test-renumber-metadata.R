# Metadata utilities are used only by these platform tests, never by renumber().
metadata_command <- function(command, args) {
    output <- system2(command, args, stdout = TRUE, stderr = TRUE)
    expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
    output
}

metadata_fixture <- function() {
    path <- normalizePath(withr::local_tempdir(.local_envir = parent.frame()))
    source <- file.path(path, "01_qc.R")
    writeBin(charToRaw("01_qc\r\n"), source)
    Sys.chmod(source, "0750")
    source
}

metadata_renumber <- function(source, fail) {
    directory <- dirname(source)
    if (fail) {
        error <- with_mocked_bindings(
            tryCatch(renumber(directory, "01", "02", recursive_edits = TRUE), error = identity),
            .renumber_move = function(...) stop("injected publication failure")
        )
        expect_true(error$recovered)
        source
    } else {
        expect_no_error(renumber(directory, "01", "02", recursive_edits = TRUE))
        file.path(directory, "02_qc.R")
    }
}

test_that("ordinary ACL entries survive publication and rollback", {
    system <- Sys.info()[["sysname"]]
    if (!system %in% c("Darwin", "Linux")) skip("ACL test requires macOS or Linux utilities")
    if (system == "Linux" && any(!nzchar(Sys.which(c("setfacl", "getfacl"))))) {
        skip("Linux ACL utilities unavailable")
    }
    for (fail in c(FALSE, TRUE)) {
        source <- metadata_fixture()
        if (system == "Darwin") {
            entry <- paste0("user:", Sys.info()[["user"]],
                " allow read,write,execute,readattr,writeattr,readextattr,writeextattr,readsecurity")
            metadata_command("/bin/chmod", c("+a", shQuote(entry), shQuote(source)))
            acl <- function(path) {
                output <- metadata_command("/bin/ls", c("-led", shQuote(path)))
                output[grepl("^[[:space:]]*[0-9]+:", output)]
            }
        } else {
            metadata_command(Sys.which("setfacl"), c("-m", "u:65534:r--", shQuote(source)))
            acl <- function(path) metadata_command(Sys.which("getfacl"), c("-cp", shQuote(path)))
        }
        before <- acl(source)
        expect_true(length(before) > 0L)
        after <- metadata_renumber(source, fail)
        expect_identical(acl(after), before)
    }
})

test_that("ordinary extended attributes survive publication and rollback", {
    system <- Sys.info()[["sysname"]]
    if (!system %in% c("Darwin", "Linux")) skip("Extended attribute test requires macOS or Linux utilities")
    if (system == "Linux" && any(!nzchar(Sys.which(c("setfattr", "getfattr"))))) {
        skip("Linux extended attribute utilities unavailable")
    }
    for (fail in c(FALSE, TRUE)) {
        source <- metadata_fixture()
        key <- "user.slurmjobs-test"
        if (system == "Darwin") {
            metadata_command("/usr/bin/xattr", c("-w", key, "preserved-metadata", shQuote(source)))
            attribute <- function(path) metadata_command("/usr/bin/xattr", c("-p", key, shQuote(path)))
        } else {
            metadata_command(Sys.which("setfattr"), c("-n", key, "-v", "preserved-metadata", shQuote(source)))
            attribute <- function(path) metadata_command(Sys.which("getfattr"),
                c("--only-values", "-n", key, shQuote(path)))
        }
        before <- attribute(source)
        after <- metadata_renumber(source, fail)
        expect_identical(attribute(after), before)
    }
})
