test_that("handouty należą wyłącznie do wykładów", {
  katalog <- materialy()
  expect_true(all(c("cwiczenie", "wyklad") %in% katalog$typ))
  expect_error(otworz_material("C01", handout = TRUE, otworz = FALSE), "tylko dla wykładu")
  expect_error(otworz_material("W01", format = "R", otworz = FALSE), "częścią ćwiczeń")
  expect_error(otworz_material("W01", format = "tex", otworz = FALSE), "handoutu wykładu")
  expect_true(file.exists(otworz_material("C01", format = "R", otworz = FALSE)))
  expect_true(file.exists(otworz_material("W01", format = "tex", handout = TRUE, otworz = FALSE)))
})
