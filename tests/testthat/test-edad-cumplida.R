source(file.path('..', '..', 'R', 'Fechas.R'))

test_that('calcula edad cuando to es posterior', {
  expect_equal(EdadCumplida(as.Date('1990-01-01'), as.Date('2000-01-01')), 10)
})

test_that('lanza error cuando to es anterior a from', {
  expect_error(
    EdadCumplida(as.Date('2000-01-01'), as.Date('1990-01-01')),
    '`to` debe ser mayor o igual que `from`'
  )
})
