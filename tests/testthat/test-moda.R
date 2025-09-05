test_that("Moda returns NA for empty vector", {
  expect_true(is.na(Moda(c())))
})
