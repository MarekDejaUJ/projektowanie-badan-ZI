test_that("11 rubryk ma pełne kryteria i poprawne sumy", {
  for (id in c(sprintf("Z%02d", 1:10), "PROJEKT")) {
    r <- rubryka(id)
    expect_equal(sum(vapply(r$criteria, `[[`, numeric(1), "max_points")), r$max_points)
    expect_equal(r$max_points, if (id == "PROJEKT") 100 else 10)
    for (k in r$criteria) {
      expect_equal(vapply(k$levels, `[[`, numeric(1), "points"), (0:4) * k$max_points / 4)
      expect_length(unique(vapply(k$levels, `[[`, character(1), "observable_description")), 5L)
    }
    f <- formularz_oceny(id)
    expect_true(all(is.na(f$punkty)))
    expect_error(sprawdz_ocene(f, id), "kryterium")
    f$punkty <- f$maksimum
    expect_equal(sprawdz_ocene(f, id), r$max_points)
    f$punkty[1] <- f$maksimum[1] + 1
    expect_error(sprawdz_ocene(f, id), "poziom")
  }
})
