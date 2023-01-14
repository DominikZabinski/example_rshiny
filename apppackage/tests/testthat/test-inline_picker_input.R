test_that("basic tests", {
    expect_equal(object = class(inline_picker_input(id = "a", choices = 1:2, liveSearch = T)), expected = "shiny.tag")
})

# devtools::test_file()
