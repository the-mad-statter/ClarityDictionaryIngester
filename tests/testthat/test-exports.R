test_that("clarity_dictionary_select_all_sql()", {
  testthat::expect_snapshot(clarity_dictionary_select_all_sql())
})
