test_that("handouty należą wyłącznie do wykładów", {
  katalog <- materialy()
  expect_true(all(c("cwiczenie", "wyklad") %in% katalog$typ))
  expect_error(otworz_material("C01", handout = TRUE, otworz = FALSE), "tylko dla wykładu")
  expect_error(otworz_material("W01", format = "R", otworz = FALSE), "nie udostępnia")
  expect_error(otworz_material("W01", format = "R", zrodlo = "pages", otworz = FALSE), "nie udostępnia")
  expect_error(otworz_material("W01", format = "tex", otworz = FALSE), "handoutu wykładu")
  expect_true(file.exists(otworz_material("C01", format = "R", otworz = FALSE)))
  expect_true(file.exists(otworz_material("W01", format = "tex", handout = TRUE, otworz = FALSE)))
  for (id in sprintf("W%02d", 2:5)) {
    expect_true(file.exists(otworz_material(id, format = "R", otworz = FALSE)))
    expect_match(otworz_material(id, format = "R", zrodlo = "pages", otworz = FALSE), "/analiza.R", fixed = TRUE)
  }
})

test_that("kontrola środowiska objaśnia wymagania oddawanego PDF", {
  wd <- getwd()
  x <- sprawdz_srodowisko()
  expect_identical(getwd(), wd)
  expect_identical(names(x), c("narzedzie", "dostepne", "znaczenie"))
  expect_type(x$dostepne, "logical")
  expect_false(anyNA(x$dostepne))
  pdf <- x[x$narzedzie %in% c("R Markdown", "knitr", "Pandoc", "XeLaTeX"), ]
  expect_equal(nrow(pdf), 4L)
  expect_true(all(grepl("Wymagane do PDF", pdf$znaczenie)))
})

test_that("katalog zadań wymienia rzeczywisty komplet Rmd i PDF", {
  x <- yaml::read_yaml(system.file("zadania", "katalog.yml", package = "badaniaZI"), eval.expr = FALSE)
  expect_identical(x$format_pracy, "rmd-1")
  expect_identical(unlist(x$odpowiedzi_zadania), sprintf("S%02d", 1:5))
  for (id in c(sprintf("Z%02d", 1:10), "PROJEKT")) {
    z <- x$zadania[[id]]
    expect_null(z$wyniki)
    folder <- badaniaZI:::folder_pracy(id)
    pliki <- if (id == "PROJEKT") unlist(z$pliki) else unlist(x$pliki_zadania)
    if (isTRUE(z$wymaga_wariantu)) pliki <- c(pliki, unlist(x$wariant_projektowy))
    expect_identical(badaniaZI:::pliki_wejscia_pracy(id), c("kurs.yml", file.path(folder, pliki)))
    pdf <- if (id == "PROJEKT") unlist(z$pliki_pdf) else unlist(x$pliki_pdf)
    expect_identical(badaniaZI:::pliki_pdf_pracy(id), file.path(folder, pdf))
  }
})
