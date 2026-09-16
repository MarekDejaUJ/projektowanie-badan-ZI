test_that("daty nieistniejące, złe strefy i kolejność terminów są odrzucane", {
  f <- badaniaZI:::czas_iso
  expect_equal(as.numeric(f("2027-01-27T23:59:59+01:00")), as.numeric(f("2027-01-27T22:59:59Z")))
  expect_false(is.na(f("2028-02-29T12:00:00Z")))
  for (x in c("2027-02-29T12:00:00Z", "2027-02-30T12:00:00Z", "2027-13-01T12:00:00Z",
              "2027-01-01T24:00:00Z", "2027-01-01T12:61:00Z", "2027-01-01T12:00:00+14:30", "2027-01-27"))
    expect_true(is.na(f(x)), info = x)
  cfg <- konfiguracja_kursu()
  cfg$termin_poprawek <- "2027-01-01T23:59:59+01:00"
  expect_true("termin_poprawek" %in% sprawdz_konfiguracje(cfg)$pole)
  cfg$timezone <- "nieznana"
  expect_true("timezone" %in% sprawdz_konfiguracje(cfg)$pole)
})

test_that("nieogłoszony kalendarz nie zastępuje wymaganych zasad oceny", {
  cfg <- konfiguracja_kursu()
  cfg$terminy_zadan <- setNames(rep(list(cfg$termin_projektu), 10), sprintf("Z%02d", 1:10))
  r <- sprawdz_konfiguracje(cfg, publikacja = TRUE)
  expect_identical(r$waga[r$pole == "daty_zajec"], "informacja")
  cfg$wagi <- list(zadania = .5, projekt = .6)
  expect_error(sprawdz_konfiguracje(cfg, publikacja = TRUE), "wagi")
  cfg <- konfiguracja_kursu()
  cfg$maksymalna_ocena_spozniona <- NA_real_
  expect_error(oblicz_ocene(rep(10, 10), 100, TRUE, cfg), "limitu")
  cfg <- konfiguracja_kursu()
  cfg$progi_ocen <- list("3" = 50, "100" = 90)
  expect_error(oblicz_ocene(rep(10, 10), 100, konfiguracja = cfg), "prog")
  cfg <- konfiguracja_kursu()
  cfg$terminy_zadan <- NULL
  expect_error(sprawdz_konfiguracje(cfg, publikacja = TRUE), "terminy_zadan")
})

test_that("błędne terminy i nieokreślona zasada SI dają kontrolowany raport", {
  cfg <- konfiguracja_kursu()
  expect_identical(cfg$zasady_si, "pomoc_w_kodzie")
  cfg$terminy_zadan <- "błędna wartość"
  expect_true("terminy_zadan" %in% sprawdz_konfiguracje(cfg)$pole)
  cfg <- konfiguracja_kursu()
  cfg$zasady_si <- NULL
  expect_error(sprawdz_konfiguracje(cfg, publikacja = TRUE), "zasady_si")
})
