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
                tags$span("Show me something"),
                inline_picker_input(id = ns("what"), choices = c("related", "similar")),
                tags$span("to a"),
                inline_picker_input(id = ns("type"), choices = getOption("types")),
                tags$span("that goes by the name of"),
                inline_picker_input(id = ns("sim"), liveSearch = TRUE, choices = c(""))
            ),
            actionButton(inputId = ns("find"), label = "Show!"),
            fluidRow(
                column(
                    style = 'background-color: #ffffff8c;',
                    width = 4, uiOutput(outputId = ns("res"))
                ),
                column(
                    width = 8, plotOutput(outputId = ns("resPlot"))
                )
            )
        )
    )
}

#' @export
#'
#' @import data.table
#' @import DBI
#' @import shiny
#' @import shinyWidgets
#' @import ggplot2
#'
#' @rdname find_similar
server_find_similar <- function(modId)
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
                    ww <- res_query(query = sprintf(qq, input$type))
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
                    tt <- res_query(query = "select id, genre_id, type_id from music_people")
                    rA <- res_query(query = "select id_A, id_B, genre_id from rels left join music_people as mp on mp.id = rels.id_A")
                    rB <- res_query(query = "select id_A, id_B, genre_id from rels left join music_people as mp on mp.id = rels.id_B")
                    rr <- rbind(rA[,.(id = id_B, genre_id)], rB[,.(id = id_A, genre_id)])
                    rr[, val := 1]
                    rr <- rr[,.(val = sum(val)), by = .(id, genre_id)]
                    genres <- getOption("genres")
                    rr$genre <- getOption("genres")[rr$genre_id]
                    genresIn <- as.character(unique(rr$genre))
                    rr <- dcast(data = rr, id ~ genre, value.var = "val", fill = 0)

                    tt$genre <- names(getOption("genres")[tt$genre_id])
                    tt$type <- names(getOption("types")[tt$type_id])

                    tt <- merge(tt, rr, all.x = T, by = "id")
                    for (i in genresIn)
                    {
                        w <- which(is.na(tt[[i]]))
                        tt[[i]][w] <- 0
                    }
                    return(tt)
                }
            )

            # running model
            model_similar <- reactive(
                x = {
                    if (input$find == 0) return()
                    if (input$what == "related") return()
                    tt <- data_model()
                    genresIn <- setdiff(names(tt), c("id", "genre", "type", "genre_id"))
                    isolate(
                        expr = {
                            tt$y <- 0
                            tt[id == input$sim]$y <- 1

                            mm <- glm(formula = as.formula(paste0("y~genre + ", paste0(paste0("`", genresIn, "`"), collapse = "+"))), data = tt, family = "binomial")
                            tt$g <- predict(object = mm, newdata = tt, type = "response")
                        }
                    )
                    return(tt)
                }
            )

            # finding related
            data_related <- reactive(
                x = {
                    if (input$find == 0) return()
                    if (input$what == "similar") return()
                    isolate(
                        expr = {
                            qq <- "select id_A, id_B from rels where id_A = %s or id_B = %s"
                            tt <- res_query(query = sprintf(qq, input$sim, input$sim))
                        }
                    )
                    return(tt)
                }
            )
            # running model to find similar choice
            output$res <- renderUI(
                expr = {
                    if (input$find == 0) return()
                    isolate(
                        expr = {
                            if (input$what == "similar")
                            {
                                winner <- model_similar()[order(-g)][id != input$sim][1]$id

                                res <- paste0("Most similar to ", name_id(input$sim, TRUE), "</span> is ", name_id(winner, TRUE))
                                res <- paste0("<p>", res, "</p>")
                            } else
                            {
                                dd <- data_related()
                                ids <- setdiff(unique(c(dd$id_A, dd$id_B)), input$sim)
                                if (length(ids) == 0) return(paste0("There is noone related to ", name_id(input$sim)))
                                ll <- paste0(unlist(lapply(X = ids, FUN = function(i) name_id(i, TRUE))), collapse = "</li><li>")
                                ll <- paste0("<ul><li>", ll, "</li></ul>")
                                res <- paste0("<p>There are ", length(ids), " records related to ", name_id(input$sim, TRUE), ":</p>", ll)
                                res <- paste0("<div>", res, "</div>")
                            }
                            all <- paste0("<div style = 'padding: 10px'>", res, "</div>")
                            return(HTML(all))
                        }
                    )
                }
            )

            output$resPlot <- renderPlot(
                expr = {
                    if (input$find == 0) return()
                    blankPlot <- ggplot() +
                        theme(
                            panel.grid.minor = element_blank(),
                            panel.grid.major.x = element_line(color = "grey90"),
                            panel.background = element_rect(fill='transparent'),
                            plot.background = element_rect(fill='transparent', color=NA)
                        )
                    isolate(
                        expr = {
                            if (input$what == "related") return(blankPlot)
                            setT <- model_similar()[id != input$sim][order(type, -g)]
                            setT[, y2 := g / max(g)]
                            # top 10
                            setT[, ii := 1]
                            setT[, ii := cumsum(ii), by = .(type)]
                            setT <- setT[ii <= 10]
                            setT$ll <- unlist(lapply(X = setT$id, FUN = function(i) name_id(i, info = F)))
                            setT[, hjust := ifelse(y2 > .5, 1, 0)]
                            ggplot(data = setT, aes(x = -ii, y = y2)) +
                                geom_col(aes(fill = genre)) +
                                geom_text(aes(label = ll, hjust = hjust), fontface = "bold") +
                                facet_wrap(~type) +
                                scale_fill_viridis_d() +
                                theme_minimal() +
                                coord_flip() +
                                theme(
                                    panel.grid.minor = element_blank(),
                                    panel.grid.major.x = element_line(color = "grey90"),
                                    panel.grid.major.y = element_blank(),
                                    panel.background = element_rect(fill = "transparent", color = NA),
                                    plot.background = element_rect(fill = "transparent", color = NA),
                                    legend.position = "bottom",
                                    axis.text.y = element_blank(),
                                    text = element_text(face = "bold")
                                ) +
                                labs(
                                    title = "Most similar band and musicians",
                                    y = "Similarity",
                                    x = NULL, fill = "Genre"
                                )
                        }
                    )
                }, bg = "#ffffffe6"
            )
        }
    )
}
