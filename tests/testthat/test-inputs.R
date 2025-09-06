source(file.path("..", "..", "R", "Inputs.R"))

test_that("InputNumerico valida argumentos numéricos", {
  expect_error(InputNumerico("id", "label", "a"), "is\\.numeric\\(value\\)")
  expect_error(InputNumerico("id", "label", 1, max = "10"), "is\\.numeric\\(max\\)")
  expect_error(InputNumerico("id", "label", 1, min = "10"), "is\\.numeric\\(min\\)")
})

test_that("InputNumerico aplica límites", {
  expect_error(InputNumerico("id", "label", 5, max = 4), "value <= config\\$max")
  expect_error(InputNumerico("id", "label", 5, min = 6), "config\\$min <= value")
  expect_error(InputNumerico("id", "label", 5, min = 6, max = 3), "config\\$min <= config\\$max")
})

test_that("pick_opt maneja cho vacío sin valores negativos", {
  opts <- pick_opt(character(0))
  val <- as.numeric(sub("count > ", "", opts$`selected-text-format`))
  expect_true(val >= 0)
})
