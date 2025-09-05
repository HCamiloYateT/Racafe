test_that("TopAbsoluto recodifica categorias menos frecuentes", {
  datos <- data.frame(cat = c("A","A","B","B","C","D"), val = 1:6)
  res <- TopAbsoluto(datos, var_recode = cat, var_top = val, fun_Top = "n", n = 2, nom_var = "cat_top")
  expect_equal(as.character(res$cat_top), c("A","A","B","B","OTROS","OTROS"))
  expect_equal(levels(res$cat_top), c("A","B","OTROS"))
})

test_that("TopRelativo aplica umbral porcentual", {
  datos <- data.frame(cat = c("A","A","B","B","C"), val = c(10,5,4,1,1))
  res <- TopRelativo(datos, var_recode = cat, var_top = val, fun_Top = "sum", pct_min = 0.2, nom_var = "cat_top")
  expect_equal(as.character(res$cat_top), c("A","A","B","B","OTROS"))
  expect_equal(levels(res$cat_top), c("A","B","OTROS"))
})
