#' Title
#'
#' @param x name of dict
#'
#' @return
#' @export
#'
#' @examples
get_from_dict <- function(x)
{
    gg <- res_query(query = sprintf("select * from %s_dict", x))
    hh <- gg[[paste0(x, "_id")]]
    names(hh) <- gg[[x]]
    return(hh)
}
