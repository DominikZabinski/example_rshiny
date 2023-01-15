#' Title
#'
#' @param i id
#'
#' @return
#' string containing basic information about id
#'
#' @export
#'
#' @import DBI
#'
#' @examples
name_id <- function(i)
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
    paste0(rr$name, " (", rr$genre, " ", rr$type, ")")
}
