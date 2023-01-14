#' @title Inline pickerIinput
#'
#' @description Returns pickerInput that could be put inline as a aprt fo a sentence
#'
#' @param id id of a input
#' @param choices choices for input
#' @param liveSearch use liveSearch? (default: TRUE)
#'
#' @return picketInput as inline input
#'
#' @export
#'
#' @import shinyWidgets
#' @import htmltools
#'
#' @examples
#' tags$div(
#'     tags$span("Someon called"),
#'     inline_picker_input(id = "this", choices = 11:14, liveSearch = TRUE)
#' )
inline_picker_input <- function(id, choices, liveSearch = FALSE)
{
    res <- pickerInput(
        inputId = id,
        label = NULL,
        # width = "fit",
        choices = choices,
        options = pickerOptions(
            style = "btn-inline",
            liveSearch = liveSearch,
            height = 10
        )
    )
    res <- tagAppendAttributes(
        tag = res,
        class = "shiny-input-container-inline",
        style = "display: inline-block;"
    )
    return(res)
}
