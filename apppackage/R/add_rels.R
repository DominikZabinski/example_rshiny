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
            tags$div(
                style = "text-align: center;",
                tags$span("Someon called"),
                inline_picker_input(id = ns("optionA"), choices = paste("choice A -", 1:10)),
                tags$span("played with"),
                inline_picker_input(id = ns("optionB"), choices = paste("choice B -", 1:10))
            ),
            fluidRow(
                actionButton(inputId = ns("add_rel"), label = "Add it")
            )
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
#'
#' @rdname add_rels
serv_add_rels <- function(modId, dbCon)
{
    moduleServer(
        id = modId,
        function(input, output, session)
        {

        }
    )
}
