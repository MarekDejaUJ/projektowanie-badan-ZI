test_that("brak p nie jest traktowany jako nieistotny wynik", {
  x <- stats::t.test(1:7, c(3, 4, 2, 7, 8, 10, 5))
  x$p.value <- NA_real_
  expect_match(wniosek_ind(x), "brak warto")
  expect_error(wniosek_ind(x, alpha = 2), "alpha")
  expect_match(badaniaZI:::format_liczba_apa(c(2, 3)), "2.00", fixed = TRUE)
})

test_that("Spearman nie otrzymuje fikcyjnych stopni swobody", {
  x <- stats::cor.test(1:8, c(1, 3, 2, 5, 4, 7, 8, 6), method = "spearman")
  expect_false(grepl("(NA)", wniosek_cor(x), fixed = TRUE))
  expect_match(wniosek_cor(x), "rho", fixed = TRUE)
  p <- stats::cor.test(1:8, c(1, 3, 2, 5, 4, 7, 8, 6))
  expect_match(wniosek_cor(p), "95% CI", fixed = TRUE)
  expect_match(wniosek_reg(stats::lm(mpg ~ 1, data = mtcars)), "tylko wyraz wolny")
})
