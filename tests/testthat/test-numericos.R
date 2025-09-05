source(file.path("..", "..", "R", "Numericos.R"))

test_that("lanza error cuando multiple es 0", {
  expect_error(RedondearMultiplo(10, 0))
})
