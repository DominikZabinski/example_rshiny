test_that("multiplication works", {
    expect_error(object = res_query(1, "2"), regexp = "it should be class character \\(its numeric\\)")
    expect_error(object = res_query("1", 2), regexp = "it should be class character \\(its numeric\\)")
})

# devtools::test_file()
