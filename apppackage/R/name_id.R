#' @title naming id
#'
#' @param i id
#' @param addSpecialClass add class and present as a span?
#' @param info add info as in genre and type?
#'
#' @return
#' string containing basic information about id
#'
#' @export
#'
#' @import DBI
#'
#' @examples
name_id <- function(i, addSpecialClass = F, info = T)
{
    qq <- "
    select
        type, name, genre
    from music_people as m
        left join genre_dict as g on g.genre_id = m.genre_id
        left join type_dict as t on t.type_id = m.type_id
    where m.id = %s
    "
    rr <- res_query(query = sprintf(qq, i))
    res <- paste0(rr$name, " (", rr$genre, " ", rr$type, ")")
    if (!info) res <- rr$name
    if (addSpecialClass) res <- paste0("<span class='names'>", res, "</span>")
    return(res)
}
