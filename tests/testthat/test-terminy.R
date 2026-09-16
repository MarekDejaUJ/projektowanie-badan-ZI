test_that("termin jest początkiem następnych ćwiczeń, a Z10 osobną datą", {
  cfg <- konfiguracja_kursu()
  expect_identical(termin_zadania("Z01", cfg)$stan, "do_ustalenia")
  cfg$daty_zajec <- list(C02 = "2026-10-20T08:15:00+02:00", C10 = "2027-01-26T09:00:00+01:00")
  expect_identical(termin_zadania("Z01", cfg)$data, cfg$daty_zajec$C02)
  expect_identical(termin_zadania("Z09", cfg)$data, cfg$daty_zajec$C10)
  expect_identical(termin_zadania("Z02", cfg)$kolejne_cwiczenie, "C03")
  expect_null(termin_zadania("Z10", cfg)$kolejne_cwiczenie)
  cfg$terminy_zadan$Z10 <- "2027-01-27T23:59:59+01:00"
  expect_identical(termin_zadania("Z10", cfg)$data, cfg$terminy_zadan$Z10)
  expect_false(any(sprawdz_konfiguracje(cfg)$waga == "blad"))
})
