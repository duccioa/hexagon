test_that("Utils", {
  expect_type(generate_names(5), 'character')
  expect_length(generate_names(5), 5)
  expect_length(generate_names(10), 10)
  expect_length(generate_names(10.4), 10)
  expect_error(generate_names("10.4"), "'n' must be an integer")
})