source(file.path("..", "..", "R", "Inputs.R"))
source(file.path("..", "..", "R", "Formatos.R"))

test_that("InputNumerico valida argumentos numéricos", {
  expect_error(InputNumerico("id", "label", "a"), "is\\.numeric\\(value\\)")
  expect_error(InputNumerico("id", "label", 1, max = "10"), "is\\.numeric\\(max\\)")
  expect_error(InputNumerico("id", "label", 1, min = "10"), "is\\.numeric\\(min\\)")
})

test_that("InputNumerico permite value NULL y NA", {
  skip_if_not("autonumericInput" %in% getNamespaceExports("shiny"),
    "autonumericInput no disponible")
  expect_error(InputNumerico("id", "label", NULL), NA)
  expect_error(InputNumerico("id", "label", NA), NA)
  expect_error(InputNumerico("id", "label", NULL, min = 0, max = 10), NA)
  expect_error(InputNumerico("id", "label", NA, min = 0, max = 10), NA)
})

test_that("InputNumerico aplica límites", {
  expect_error(InputNumerico("id", "label", 5, max = 4), "value <= config\\$max")
  expect_error(InputNumerico("id", "label", 5, min = 6), "config\\$min <= value")
  expect_error(InputNumerico("id", "label", 5, min = 6, max = 3), "config\\$min <= config\\$max")
})

test_that("InputNumerico respeta rangos personalizados de porcentaje", {
  # Rangos por defecto 0-100
  expect_error(InputNumerico("id", "label", 101, type = "porcentaje"), "value <= config\\$max")
  expect_error(InputNumerico("id", "label", -1, type = "porcentaje"), "config\\$min <= value")

  # Rango personalizado
  expect_error(InputNumerico("id", "label", 60, type = "porcentaje", max = 50, min = -50), "value <= config\\$max")
  expect_error(InputNumerico("id", "label", -60, type = "porcentaje", max = 50, min = -50), "config\\$min <= value")

  # Solo max personalizado
  expect_error(InputNumerico("id", "label", 210, type = "porcentaje", max = 200), "value <= config\\$max")
  expect_error(InputNumerico("id", "label", -1, type = "porcentaje", max = 200), "config\\$min <= value")

  # Solo min personalizado
  expect_error(InputNumerico("id", "label", 5, type = "porcentaje", min = 10), "config\\$min <= value")
})

test_that("pick_opt maneja cho vacío sin valores negativos", {
  opts <- pick_opt(character(0))
  val <- as.numeric(sub("count > ", "", opts$`selected-text-format`))
  expect_true(val >= 0)
})

