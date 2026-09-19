test_that("gotowe analizy zapisują dwa tory i są odtwarzalne", {
  k <- tempfile("gotowe analizy ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s017", "S02", k)

  for (id in sprintf("Z%02d", 5:8)) przygotuj_zadanie(id, k)
  uruchom_analize("Z05", k, list(zmienna = "indeks", wartosc_odniesienia = 3), B = 999)
  uruchom_analize("Z06", k, list(zmienna = "indeks"), B = 999)
  pierwszy <- read.csv(file.path(k, "zadania/z06/wyniki/porownanie.csv"))
  uruchom_analize("Z06", k, list(zmienna = "indeks"), B = 999)
  drugi <- read.csv(file.path(k, "zadania/z06/wyniki/porownanie.csv"))
  expect_identical(pierwszy, drugi)
  uruchom_analize("Z07", k, B = 999)
  uruchom_analize("Z08", k, list(wykonanie = "czas_wyszukiwania", metoda = "spearman"), B = 999)

  est <- read.csv(file.path(k, "zadania/z05/wyniki/estymacja.csv"))
  por <- read.csv(file.path(k, "zadania/z06/wyniki/porownanie.csv"))
  kat <- read.csv(file.path(k, "zadania/z07/wyniki/test_kategorie.csv"))
  kor <- read.csv(file.path(k, "zadania/z08/wyniki/korelacja.csv"))
  expect_true(all(c("p_klasyczne", "p_permutacyjne", "d", "CI_dol", "CI_gora", "N") %in% names(est)))
  expect_true(all(c("p_klasyczne", "p_permutacyjne", "Hedges_g", "CI_roznicy_dol", "CI_roznicy_gora") %in% names(por)))
  expect_true(all(c("p_klasyczne", "p_Monte_Carlo", "V_Cramera", "CI_V_dol", "CI_V_gora") %in% names(kat)))
  expect_true(all(c("N", "wspolczynnik", "CI_dol", "CI_gora", "p_klasyczne", "p_permutacyjne") %in% names(kor)))
  expect_error(uruchom_analize("Z06", k, list(zmienna = "niedozwolona"), B = 999),
               "Dozwolone wartości")
})

test_that("projekt obsługuje oba rodzaje drugiej analizy", {
  for (scen in c("S01", "S02")) {
    k <- tempfile(paste0("projekt ", scen, " "))
    on.exit(unlink(k, recursive = TRUE), add = TRUE)
    utworz_projekt(paste0("id", scen), scen, k)
    uruchom_projekt(k, list(zmienna_porownania = "indeks", B = 999L))
    folder <- file.path(k, "projekty", "ilosciowy", "wyniki")
    expect_true(all(file.exists(file.path(folder, c(
      "dziennik.csv", "opis.csv", "indeks.csv", "kanaly.csv",
      "porownanie.csv", "druga_analiza.csv", "histogram.png", "grupy.png",
      "druga_analiza.png")))))
    druga <- read.csv(file.path(folder, "druga_analiza.csv"))
    if (scen == "S01")
      expect_true(all(c("wspolczynnik", "p_klasyczne", "p_permutacyjne") %in% names(druga)))
    else
      expect_true(all(c("chi2", "p_klasyczne", "p_Monte_Carlo", "V_Cramera") %in% names(druga)))
  }
})

test_that("szablony pozostawiają wyłącznie listę parametrów do zmiany", {
  k <- tempfile("struktura skryptow ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s021", "S03", k)
  expect_true(badaniaZI:::sprawdz_strukture_analizy(
    "PROJEKT", file.path(k, "projekty/ilosciowy/analiza.R")))
  for (id in sprintf("Z%02d", 1:10)) {
    przygotuj_zadanie(id, k)
    expect_true(badaniaZI:::sprawdz_strukture_analizy(
      id, file.path(k, "zadania", tolower(id), "analiza.R")))
  }
})
