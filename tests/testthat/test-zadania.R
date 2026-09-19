test_that("znaczniki raportu i naruszenie surowych danych blokują oddanie", {
  k <- tempfile("moje badania ")
  on.exit(unlink(k, recursive = TRUE))
  utworz_projekt("s017", katalog = k)
  przygotuj_zadanie("Z01", k)
  r <- sprawdz_zadanie("Z01", k, uruchom = FALSE)
  expect_false(r$ok)
  expect_true(all(r$kontrole$ok[r$kontrole$element %in% c("manifest ID", "wariant generatora", "surowe CSV", "wartości danych")]))
  plik <- file.path(k, "zadania", "z01", "odpowiedzi.md")
  tekst <- readLines(plik, encoding = "UTF-8")
  tekst <- gsub("[UZUPELNIJ]", "Odczytane dane pozwalają wskazać wynik obliczenia i objaśnić działanie konsoli oraz skryptu.", tekst, fixed = TRUE)
  writeLines(enc2utf8(tekst), plik, useBytes = TRUE)
  writeLines("wynik <- sum(1:4)", file.path(k, "zadania", "z01", "analiza.R"))
  r <- sprawdz_zadanie("Z01", k, uruchom = TRUE)
  expect_true(r$ok, info = paste(r$kontrole$element[!r$kontrole$ok], collapse = ", "))
  writeLines("github_pat_123456789012345678901234567890", file.path(k, "zadania", "z01", "wyniki", "sekret.md"))
  expect_false(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
  unlink(file.path(k, "zadania", "z01", "wyniki", "sekret.md"))
  cat("\n", file = file.path(k, "dane", "surowe.csv"), append = TRUE)
  r <- sprawdz_zadanie("Z01", k, uruchom = FALSE)
  expect_false(r$kontrole$ok[r$kontrole$element == "surowe CSV"])
})

test_that("adresy z tokenem i ścieżki obcego serwera nie są przyjmowane", {
  expect_equal(badaniaZI:::nazwa_repo("https://github.com/prowadzacy/badania-s017.git"), "prowadzacy/badania-s017")
  expect_error(badaniaZI:::nazwa_repo("https://token@github.com/prowadzacy/badania-s017.git"), "repo_url")
  expect_error(badaniaZI:::nazwa_repo("https://example.org/prowadzacy/badania-s017.git"), "repo_url")
})

test_that("świeża kontrola zachowuje bibliotekę R i usuwa tokeny z otoczenia", {
  k <- tempfile("własne R ")
  biblioteka <- tempfile("biblioteka ")
  dir.create(biblioteka)
  zmienne <- c("R_LIBS_USER", "GITHUB_PAT", "GH_TOKEN", "GITHUB_TOKEN", "GH_CONFIG_DIR", "R_TESTS")
  stan <- Sys.getenv(zmienne, unset = NA_character_)
  on.exit({
    Sys.unsetenv(zmienne)
    for (n in zmienne[!is.na(stan)]) do.call(Sys.setenv, setNames(list(stan[[n]]), n))
    unlink(c(k, biblioteka), recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(R_LIBS_USER = normalizePath(biblioteka, winslash = "/"),
             GITHUB_PAT = "prywatna_wartosc", GH_TOKEN = "prywatna_wartosc",
             GITHUB_TOKEN = "prywatna_wartosc", GH_CONFIG_DIR = "konfiguracja_konta",
             R_TESTS = "nieistniejacy-plik")
  utworz_projekt("s019", katalog = k)
  przygotuj_zadanie("Z01", k)
  md <- file.path(k, "zadania/z01/odpowiedzi.md")
  tekst <- readLines(md, encoding = "UTF-8")
  tekst <- gsub("[UZUPELNIJ]", "Własny skrypt odtwarza kontrolę środowiska w nowym procesie bez logowania do serwera i bez pobierania zależności.", tekst, fixed = TRUE)
  writeLines(enc2utf8(tekst), md, useBytes = TRUE)
  kod <- c('stopifnot(all(Sys.getenv(c("GITHUB_PAT", "GH_TOKEN", "GITHUB_TOKEN")) == ""))',
    'stopifnot(dir.exists(Sys.getenv("R_LIBS_USER")))',
    'stopifnot(normalizePath(Sys.getenv("R_LIBS_USER"), winslash="/") %in% .libPaths())',
    'stopifnot(Sys.getenv("GH_CONFIG_DIR") != "konfiguracja_konta")',
    'stopifnot(Sys.getenv("R_TESTS") == "")')
  writeLines(kod, file.path(k, "zadania/z01/analiza.R"))
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  expect_identical(Sys.getenv("GITHUB_PAT"), "prywatna_wartosc")
  expect_identical(Sys.getenv("GH_CONFIG_DIR"), "konfiguracja_konta")
})

test_that("brak eksportu i nieaktualny CSV nie przechodzą odtworzenia", {
  k <- tempfile("badaniazi wyniki ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s018", katalog = k)
  przygotuj_zadanie("Z02", k)
  md <- file.path(k, "zadania/z02/odpowiedzi.md")
  t <- readLines(md, encoding = "UTF-8")
  t <- gsub("[UZUPELNIJ]", "Własna tabela została oczyszczona z identycznych kopii, a reguły oraz liczby zmian zapisano w dzienniku. Brak nie jest zerem.", t, fixed = TRUE)
  writeLines(enc2utf8(t), md, useBytes = TRUE)
  a <- file.path(k, "zadania/z02/analiza.R")
  kod <- c('dir.create("zadania/z02/wyniki", recursive=TRUE, showWarnings=FALSE)',
    'x <- read.csv("dane/surowe.csv", encoding="UTF-8")',
    'write.csv(x, "zadania/z02/wyniki/wyczyszczone.csv", row.names=FALSE, fileEncoding="UTF-8")',
    'write.csv(data.frame(regula="test", liczba=nrow(x)), "zadania/z02/wyniki/dziennik.csv", row.names=FALSE)')
  writeLines(kod, a)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(k)
  sys.source(a, envir = new.env())
  setwd(wd)
  expect_true(sprawdz_zadanie("Z02", k)$ok)
  # Ta kontrola dotyczy odtwarzalności; celowo nie przyznaje punktów za czyszczenie.
  writeLines('x <- 1', a)
  r <- sprawdz_zadanie("Z02", k)
  expect_false(r$ok)
  expect_false(all(r$kontrole$ok[grepl(": odtworzenie$", r$kontrole$element)]))
  writeLines(kod, a)
  write.csv(data.frame(regula = "test", liczba = 1), file.path(k, "zadania/z02/wyniki/dziennik.csv"), row.names = FALSE)
  r <- sprawdz_zadanie("Z02", k)
  expect_false(r$ok)
  expect_false(r$kontrole$ok[r$kontrole$element == "zadania/z02/wyniki/dziennik.csv: zgodność CSV"])
  unlink(file.path(k, "zadania/z02/wyniki/dziennik.csv"))
  expect_false(sprawdz_zadanie("Z02", k, uruchom = FALSE)$ok)
})
