# libraries ----
library(apppackage)

# add files from package ----
addResourcePath(prefix = "int", directoryPath = system.file(package = "apppackage"))

# options ----
options(
    myDb = "mydatabase.sqlite",
    genres = get_from_dict(x = "genre"),
    types = get_from_dict(x = "type")
)

# ui ----
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "int/css/fonts.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "int/css/tabs.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "int/css/picker_inline.css")
    ),
    tags$h1("How about ... "),
    navlistPanel(
        widths = c(2, 10),
        tabPanel(
            title = "... adding next rock star?",
            ui_add_music_band(modId = "jj")
        ),
        tabPanel(
            title = "... connecting people?",
            ui_add_rels(modId = "kk")
        ),
        tabPanel(
            title = "... exploring?",
            ui_find_similar(modId = "aa")
        )
    ),
    tags$script(src = "int/js/handlers_observers.js")
)

# server ----
server <- function(input, output) {
    serv_add_music_band(modId = "jj")
    serv_add_rels(modId = "kk")
    server_find_similar(modId = "aa")
}

# app ----
shinyApp(ui = ui, server = server, options = list(launch.browser = T))
