#' @title UI and server for adding relationships tab
#'
#' @param modId id to used in the module
#'
#' @return
#' \code{ui_add_rels} returns ui for the tab, \code{serv_add_rels} returns moduleServer for it
#'
#' @export
#'
#' @import shiny
#'
#' @examples
#'
#' @name add_rels
ui_add_rels <- function(modId)
{
    ns <- NS(namespace = modId)

    tags$div(
        class = "add_m_b-wrapper",
        tags$img(
            class="add_m_b-bg", src = "bgrels.svg"
        ),
        tags$div(
            class = 'add_m_b-content',
            tags$p("You can use this page to add a relationships between musicians and/or bands to the database."),
            tags$div(
                tags$span("A"),
                inline_picker_input(id = ns("type1"), choices = getOption("types")),
                tags$span("called"),
                inline_picker_input(id = ns("ob1"), liveSearch = TRUE, choices = c("")),
                tags$span("played with"),
                inline_picker_input(id = ns("type2"), choices = getOption("types")),
                tags$span("called"),
                inline_picker_input(id = ns("ob2"), liveSearch = TRUE, choices = c(""))
            ),
            actionButton(inputId = ns("add"), label = "Add it"),
            textOutput(outputId = ns("checkRes"))
        )
    )
}

#' @export
#'
#' @import data.table
#' @import DBI
#' @import shiny
#'
#' @rdname add_rels
serv_add_rels <- function(modId)
{
    moduleServer(
        id = modId,
        function(input, output, session)
        {
            observeEvent(
                eventExpr = input$type1,
                handlerExpr = {
                    qq <- "select id, name, genre_id from music_people where type_id = '%s'"
                    ww <- res_query(query = sprintf(qq, input$type1))
                    hha <- lapply(
                        X = getOption("genres"),
                        FUN = function(i)
                        {
                            hh <- as.list(ww[genre_id == i]$id)
                            names(hh) <- ww[genre_id == i]$name
                            return(hh)
                        }
                    )
                    names(hha) <- names(getOption("genres"))
                    updatePickerInput(session = session, inputId = "ob1", choices = hha)
                }
            )

            observeEvent(
                eventExpr = input$type2,
                handlerExpr = {
                    qq <- "select id, name, genre_id from music_people where type_id = '%s'"
                    ww <- res_query(query = sprintf(qq, input$type2))
                    hha <- lapply(
                        X = getOption("genres"),
                        FUN = function(i)
                        {
                            hh <- as.list(ww[genre_id == i]$id)
                            names(hh) <- ww[genre_id == i]$name
                            return(hh)
                        }
                    )
                    names(hha) <- names(getOption("genres"))
                    updatePickerInput(session = session, inputId = "ob2", choices = hha)
                }
            )

            # adding entry to the database
            observeEvent(
                eventExpr = input$add,
                handlerExpr = {
                    if (input$add == 0) return()
                    # check if name is correct
                    checkResults <- check_rel(input$ob1, input$ob2)
                    if (checkResults$res)
                    {
                        # construct entry
                        thisId <- res_query(query = "select max(id) as m from rels")$m + 1
                        ids <- as.numeric(c(input$ob1, input$ob2))
                        kk <- data.table(
                            id_A = min(ids),
                            id_B = max(ids),
                            id = thisId
                        )

                        dbCon <- dbConnect(drv = RSQLite::SQLite(), getOption('myDb'))
                        dbAppendTable(conn = dbCon, name = "rels", value = kk)
                        dbDisconnect(conn = dbCon)
                    }
                    output$checkRes <- renderText(expr = {checkResults$msg})
                }
            )
        }
    )
}

check_rel <- function(idA, idB)
{
    if (idA == idB) return(list(res = F, msg = "They only can play with someone else"))
    qq <- "select * from rels where (id_A = %s and id_B = %s) or (id_A = %s and id_B = %s)"
    res <- res_query(query = sprintf(qq, idA, idB, idB, idA))
    if (nrow(res) > 0) return(list(res = F, msg = "They've already played together"))
    return(list(res = T, msg = "All good!"))
}
