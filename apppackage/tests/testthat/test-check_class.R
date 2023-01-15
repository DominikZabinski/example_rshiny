test_that("test check_class", {
    expect_invisible(call = check_class(1, "numeric"))
    expect_error(object = check_class(1, "character"), regexp = "it should be class character \\(its numeric\\)")
})

# devtools::test_file()
