#' @title UI and server for adding tab
#'
#' @param modId id to used in the module
#'
#' @return
#' \code{ui_add_music_band} returns ui for the tab, \code{serv_add_music_band} returns moduleServer for it
#'
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
            br(),
            actionButton(inputId = ns("add"), label = "Go on, add it!"),
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
#' @rdname add_music_band
serv_add_music_band <- function(modId)
{
    moduleServer(
        id = modId,
        function(input, output, session)
        {
            # adding entry to the database
            observeEvent(
                eventExpr = input$add,
                handlerExpr = {
                    if (input$add == 0) return()
                    # check if name is correct
                    checkResults <- check_name(input$name, input$genre, input$type)
                    if (checkResults$res)
                    {
                        # construct entry
                        thisId <- res_query(query = "select max(id) as m from music_people")$m + 1

                        kk <- data.table(
                            id = thisId,
                            name = trimws(input$name),
                            genre_id = input$genre,
                            type_id = input$type
                        )

                        dbCon <- dbConnect(drv = RSQLite::SQLite(), getOption('myDb'))
                        dbAppendTable(conn = dbCon, name = "music_people", value = kk)
                        dbDisconnect(conn = dbCon)
                    }
                    output$checkRes <- renderText(expr = {checkResults$msg})
                }
            )

            # sending JS based on genre
            observeEvent(
                eventExpr = input$genre,
                handlerExpr = {
                    possibleColors <- c(
                        "linear-gradient( 102.4deg,  rgba(253,189,85,1) 7.8%, rgba(249,131,255,1) 100.3% )",
                        "radial-gradient( circle farthest-corner at 92.3% 71.5%,  rgba(83,138,214,1) 0%, rgba(134,231,214,1) 90% )",
                        "radial-gradient( circle farthest-corner at 50.7% 54%,  rgba(204,254,152,1) 0%, rgba(229,253,190,1) 92.4% )",
                        "radial-gradient( circle farthest-corner at 10% 20%,  rgba(255,94,247,1) 17.8%, rgba(2,245,255,1) 100.2% )"
                    )

                    session$sendCustomMessage("changeBgImgHandler", message = possibleColors[as.numeric(input$genre)])
                }
            )
        }
    )
}

check_name <- function(x, genre, type)
{
    x <- trimws(x)
    if (nchar(x) == 0) return(list(res = F, msg = "Name must contain something other than spaces"))
    qq <- "select name from music_people where type_id = %s and genre_id = %s"
    if (x %in% res_query(query = sprintf(qq, type, genre))$name) return(list(res = F, msg = "There is already someone like that in the database"))
    return(list(res = T, msg = "All good!"))
}
