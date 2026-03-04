source(file.path("..", "..", "R", "Datos.R"))

test_that("ConstruirCondicionLlaveSQL soporta múltiples valores", {
  con <- DBI::ANSI()

  condicion <- .ConstruirCondicionLlaveSQL(
    con,
    "Fecha",
    as.Date(c("2024-01-01", "2024-01-02"))
  )

  expect_match(condicion, '"Fecha" IN \(')
  expect_match(condicion, "'2024-01-01'")
  expect_match(condicion, "'2024-01-02'")
})

test_that("ConstruirCondicionLlaveSQL combina IN con NULL cuando hay NA", {
  con <- DBI::ANSI()

  condicion <- .ConstruirCondicionLlaveSQL(con, "id", c(1, NA))

  expect_identical(condicion, '("id" IN (1) OR "id" IS NULL)')
})

test_that("ConstruirCondicionLlaveSQL usa IS NULL para NA escalar", {
  con <- DBI::ANSI()

  condicion <- .ConstruirCondicionLlaveSQL(con, "id", NA)

  expect_identical(condicion, '"id" IS NULL')
})

test_that("ConstruirCondicionLlaveSQL falla si una llave no tiene valores", {
  con <- DBI::ANSI()

  expect_error(
    .ConstruirCondicionLlaveSQL(con, "id", numeric(0)),
    "al menos un valor"
  )
})
