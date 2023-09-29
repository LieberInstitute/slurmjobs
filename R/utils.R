#' Temporarily evaluate an expression in a directory
#'
#' Temporarily evaluate an expression in a directory, then set the directory
#' back to the original.
#'
#' @param dir a directory to perform an expression within.
#' @param expr expression to evaluate.
#'
#' @details See here: http://plantarum.ca/code/setwd-part2/
#' @export
#' @author Tyler Smith, contributed to regionReport by David Robinson
#' https://github.com/dgrtwo
#'
#' @examples
#'
#' ## Create a directory called 'hola' and then check that it exists
#' with_wd(tempdir(), {
#'     dir.create("hola", showWarnings = FALSE)
#'     file.exists("hola")
#' })
#'
with_wd <- function(dir, expr) {
    wd <- getwd()
    on.exit(setwd(wd))
    setwd(dir)
    eval(expr, envir = parent.frame())
}
