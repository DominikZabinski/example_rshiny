#' Title
#'
#' @param modId id to used in the module
#'
#' @return
#' @export
#'
#' @import shiny
#'
#' @examples
#'
#' @name add_music_band
ui_add_music_band <- function(modId)
{
    ns <- NS(namespace = modId)

    # UI structrue
    tags$div(
        class = "add_m_b-wrapper",
        tags$img(
            class="add_m_b-bg", src = "bg1.svg", style = 'background-image: linear-gradient( 102.4deg,  rgba(253,189,85,1) 7.8%, rgba(249,131,255,1) 100.3% )'
        ),
        tags$div(
            class = 'add_m_b-content',
            tags$p("You can use this page to add a musician or a band to the database."),
            column(
                width = 12,
                selectInput(inputId = ns("type"), label = "Musician or a band", choices = getOption("types"))
            ),
            column(
                width = 12,
                selectInput(inputId = ns("genre"), label = "Select a genre (one, main)", choices = getOption("genres"), multiple = F)
            ),
            column(
                width = 12,
                textInput(inputId = ns("name"), label = "Type in the name")
            ),
            actionButton(inputId = ns("add"), label = "Go on, add it!", style = "float:right;")
        )
    )
}

#' @export
#'
#' @import data.table
#' @import DBI
#' @import shiny
#'
#' @rdname add_music_band
serv_add_music_band <- function(modId)
{
    moduleServer(
        id = modId,
        function(input, output, session)
        {
            observeEvent(
                eventExpr = input$add,
                handlerExpr = {
                    # TO DO: check if name it's not empty
                    # TO DO: check if it is already in database
                    thisId <- res_query(query = "select max(id) as m from music_people")$m + 1

                    kk <- data.table(
                        id = thisId,
                        name = input$name,
                        genre_id = input$genre,
                        type_id = input$type
                    )

                    dbCon <- dbConnect(drv = RSQLite::SQLite(), getOption('myDb'))
                    dbAppendTable(conn = dbCon, name = "music_people", value = kk)
                    dbDisconnect(conn = dbCon)
                }
            )
        }
    )
}
