test_that("map_no_yes_to_0_1()", {
  expect_equal(map_no_yes_to_0_1("No"), 0)

  expect_equal(map_no_yes_to_0_1("Yes"), 1)

  expect_equal(map_no_yes_to_0_1("Maybe"), NA_integer_)
})

test_that("map_not_exported_exported_to_0_1()", {
  expect_equal(map_not_exported_exported_to_0_1("Not Exported"), 0)

  expect_equal(map_not_exported_exported_to_0_1("Exported"), 1)

  expect_equal(map_not_exported_exported_to_0_1("Maybe Exported"), NA_integer_)
})
