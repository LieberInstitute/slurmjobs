# All fixtures are disposable; include ignored/hidden files in byte-level checks.
overwrite_snapshot <- function(path) {
    walk <- function(directory) {
        paths <- list.files(directory, all.files = TRUE, no.. = TRUE, full.names = TRUE)
        unlist(lapply(paths, function(x) {
            link <- Sys.readlink(x)
            if ((!is.na(link) && nzchar(link)) || !dir.exists(x)) x else c(x, walk(x))
        }), use.names = FALSE)
    }
    paths <- sort(walk(path))
    stats::setNames(lapply(paths, function(x) {
        link <- Sys.readlink(x)
        if (!is.na(link) && nzchar(link)) return(list(link = link))
        if (dir.exists(x)) return(list(directory = TRUE))
        list(bytes = readBin(x, "raw", n = file.info(x)$size))
    }), substring(paths, nchar(path) + 2L))
}


overwrite_fixture <- function(path, log_dir = file.path(path, "code", "logs")) {
    dir.create(file.path(path, "code"))
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(c(paste("#SBATCH -o", file.path(log_dir, "01_qc.txt")), "Rscript 01_qc.R"),
        file.path(path, "code", "01_qc.sh"))
    writeLines('x <- "01_qc"', file.path(path, "code", "01_qc.R"))
    writeLines("original log", file.path(log_dir, "01_qc.txt"))
    log_dir
}

