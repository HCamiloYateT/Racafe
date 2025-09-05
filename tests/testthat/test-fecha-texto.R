source(file.path('..', '..', 'R', 'Fechas.R'))

test_that('incluye día numérico cuando dia = TRUE', {
  resultado <- FechaTexto(as.Date('2023-10-15'), dia = TRUE, dia_nombre = TRUE,
                           mes_abr = TRUE, anho_abr = TRUE)
  expect_equal(resultado, 'dom, 15 de Oct de 23')
})

test_that('omite día numérico cuando dia = FALSE', {
  resultado <- FechaTexto(as.Date('2023-10-15'), dia = FALSE, dia_nombre = TRUE,
                           mes_abr = TRUE, anho_abr = TRUE)
  expect_equal(resultado, 'dom, Oct de 23')
})
