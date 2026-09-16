test_that("rekodacja i indeks respektują granice i brak odpowiedzi", {
  expect_equal(odwroc_pozycje(c(1, 2, 3, 4, 5, NA)), c(5, 4, 3, 2, 1, NA))
  expect_error(odwroc_pozycje(c(1, 99)), "kody brak")
  p <- as.data.frame(matrix(c(rep(4, 6), rep(3, 5), NA, rep(2, 4), NA, NA, rep(NA, 6)),
                           nrow = 4, byrow = TRUE))
  expect_equal(indeks_ankiety(p), c(4, 3, NA, NA))
  expect_error(indeks_ankiety(transform(p, V1 = 99)), "kod 99")
  expect_error(indeks_ankiety(p, 7), "minimum")
})

test_that("wielokrotny wybór ma mianownik respondentów z pełną odpowiedzią", {
  x <- odpowiedzi_wielokrotne(data.frame(a = c(1, 1, 0, NA, 1), b = c(1, 0, 0, NA, NA)))
  expect_equal(x$liczba, c(2L, 1L))
  expect_equal(x$mianownik, c(3L, 3L))
  expect_equal(x$procent_respondentow, c(200 / 3, 100 / 3))
  expect_equal(x$pominiete_niepelne, c(2L, 2L))
  expect_true(all(is.na(odpowiedzi_wielokrotne(data.frame(a = NA_real_))$procent_respondentow)))
  expect_error(odpowiedzi_wielokrotne(data.frame(a = 2)), "0/1")
})

test_that("konflikt odpowiedzi nie jest po cichu usuwany", {
  d <- generuj_dane("s017")$dane
  konflikt <- d[1L, ]
  konflikt$powodzenie <- 1L - konflikt$powodzenie
  expect_error(przygotuj_ankiete(rbind(d, konflikt)), "To samo ID")
})
