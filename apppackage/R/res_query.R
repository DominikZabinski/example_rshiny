#' Title
#'
#' @param query query to execute
#' @param db name of ifle with database
#'
#' @return
#' @export
#'
#' @import DBI
#' @import data.table
#' @importFrom RSQLite SQLite
#'
#' @examples
res_query <- function(query, db = "mydatabase.sqlite")
{
    check_class(query, "character")
    check_class(db, "character")

    dbCon <- dbConnect(drv = SQLite(), db)
    res <- data.table(dbGetQuery(conn = dbCon, statement = query))
    dbDisconnect(conn = dbCon)
    return(res)
}
