source(file.path('..', '..', 'R', 'Html.R'))

test_that('Saltos y Espacios validan n', {
  expect_error(Saltos(-1))
  expect_error(Saltos(1.2))
  expect_error(Espacios(-1))
  expect_error(Espacios(1.5))
})
