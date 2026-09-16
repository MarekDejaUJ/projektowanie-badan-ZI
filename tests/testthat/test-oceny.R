test_that("50/50, progi i ograniczenie spóźnienia działają na granicach", {
  for (p in c(49.999, 50, 59.999, 60, 69.999, 70, 79.999, 80, 89.999, 90, 100)) {
    r <- oblicz_ocene(rep(p / 10, 10), p)
    expect_equal(r$procent, p)
    oczekiwana <- if (p < 50) 2 else min(5, 3 + .5 * floor((p - 50) / 10))
    expect_equal(r$ocena, oczekiwana)
  }
  expect_equal(oblicz_ocene(rep(10, 10), 100, TRUE)$ocena, 4.5)
  expect_equal(oblicz_ocene(rep(5, 10), 100)$procent, 75)
  expect_equal(oblicz_ocene(c(rep(8, 9), NA), 100)$stan, "brak_oceny")
  expect_true(is.na(oblicz_ocene(rep(10, 10), NA_real_)$ocena))
})
