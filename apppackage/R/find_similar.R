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
#' @name find_similar
ui_find_similar <- function(modId)
{
    ns <- NS(namespace = modId)

    tags$div(
        class = "add_m_b-wrapper",
        tags$img(
            class="add_m_b-bg", src = "bgsim.svg"
        ),
        tags$div(
            class = 'add_m_b-content',
            tags$div(
                tags$span("Show me something similar to a"),
                inline_picker_input(id = ns("type"), choices = getOption("types")),
                tags$span("that goes by the name of"),
                inline_picker_input(id = ns("sim"), liveSearch = TRUE, choices = c(""))
            ),
            actionButton(inputId = ns("find"), label = "Fetch me something new to listen"),
            textOutput(outputId = ns("res"))
        )
    )
}

#' @param dbCon connection to database
#'
#' @export
#'
#' @import data.table
#' @import DBI
#' @import shiny
#' @import shinyWidgets
#'
#' @rdname find_similar
server_find_similar <- function(modId, dbCon)
{
    moduleServer(
        id = modId,
        function(input, output, session)
        {
            ns <- NS(namespace = modId)

            # using updatePickerInput to optimize performance
            observeEvent(
                eventExpr = input$type,
                handlerExpr = {
                    qq <- "select id, name, genre_id from music_people where type_id = '%s'"
                    ww <- data.table(dbGetQuery(conn = dbCon, statement = sprintf(qq, input$type)))
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
                    updatePickerInput(session = session, inputId = "sim", choices = hha)
                }
            )

            # preparing data for the model
            data_model <- reactive(
                x = {
                    tt <- data.table(dbGetQuery(conn = dbCon, statement = "select id, genre_id from music_people"))
                    rA <- data.table(dbGetQuery(conn = dbCon, statement = "select id_A, id_B, genre_id from rels left join music_people as mp on mp.id = rels.id_A"))
                    rB <- data.table(dbGetQuery(conn = dbCon, statement = "select id_A, id_B, genre_id from rels left join music_people as mp on mp.id = rels.id_B"))
                    rr <- rbind(rA[,.(id = id_B, genre_id)], rB[,.(id = id_A, genre_id)])
                    rr[, val := 1]
                    rr <- rr[,.(val = sum(val)), by = .(id, genre_id)]
                    genres <- getOption("genres")
                    rr$genre <- getOption("genres")[rr$genre_id]
                    genresIn <- unique(rr$genre)
                    rr <- dcast(data = rr, id ~ genre, value.var = "val", fill = 0)

                    tt$genre <- getOption("genres")[tt$genre_id]
                    tt <- merge(tt, rr, all.x = T, by = "id")
                    for (i in genresIn)
                    {
                        w <- which(is.na(tt[[i]]))
                        tt[[i]][w] <- 0
                    }
                    return(tt)
                }
            )

            # running model to find similar choice
            output$res <- renderText(
                expr = {
                    if (input$find == 0) return()
                    tt <- data_model()
                    genresIn <- setdiff(names(tt), c("id", "genre", "genre_id"))
                    isolate(
                        expr = {
                            tt$y <- 0
                            tt[id == input$sim]$y <- 1

                            mm <- glm(formula = as.formula(paste0("y~genre + ", paste0(paste0("`", genresIn, "`"), collapse = "+"))), data = tt)
                            tt$g <- predict(object = mm, newdata = tt)
                            winner <- tt[order(-g)][id != input$sim][1]$id

                            paste0("Most similar to ", name_id(input$sim, dbCon = dbCon), " is ", name_id(winner, dbCon = dbCon))
                        }
                    )
                }
            )
        }
    )
}
