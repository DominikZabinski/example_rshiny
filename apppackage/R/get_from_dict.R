#' Title
#'
#' @param x name of dict
#' @param dbCon connection to database
#'
#' @return
#' @export
#'
#' @examples
get_from_dict <- function(x, dbCon)
{
    gg <- data.table(dbGetQuery(conn = dbCon, statement = sprintf("select * from %s_dict", x)))
    hh <- gg[[paste0(x, "_id")]]
    names(hh) <- gg[[x]]
    return(hh)
}
