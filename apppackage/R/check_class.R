#' @title checking class
#'
#' @param x object to check
#' @param a expected class
#'
#' @return
#' throws an error if object's class does not match needed. returns invisible NULL otherwise
#'
#' @export
#'
#' @examples
#' check_class(1, "numeric")
check_class <- function(x, a)
{
    if (a %in% class(x)) return(invisible(NULL))
    stop(sprintf("it should be class %s (its %s)", a, paste0(class(x), collapse = ", ")))
}
