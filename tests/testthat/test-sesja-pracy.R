wyczysc_sesje <- function() {
  s <- badaniaZI:::sesja_pracy
  rm(list = ls(s, all.names = TRUE), envir = s)
}

test_that("kontekst nie zmienia katalogu i nie wybiera cudzej pracy", {
  wyczysc_sesje()
  on.exit(wyczysc_sesje(), add = TRUE)
  k <- tempfile("sesja żółta ")
  obcy <- tempfile("inna praca ")
  on.exit(unlink(c(k, obcy), recursive = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k)
  utworz_projekt("s018", katalog = obcy)
  p <- przygotuj_zadanie("Z01", k)
  cwd <- getwd()
  badaniaZI:::zapamietaj_prace(k, "C01")
  expect_identical(getwd(), cwd)
  expect_identical(plik_pracy("Z01"), p)
  expect_identical(przygotuj_zadanie("Z01"), p)
  expect_error(plik_pracy("Z01", "../kurs.yml"), "wewnątrz")
  expect_error(plik_pracy("Z01", "nieistniejacy.R"), "Brakuje pliku")
  withr::with_dir(obcy, {
    expect_error(plik_pracy("Z01"), "różne przestrzenie")
    expect_identical(plik_pracy("Z01", katalog = k), p)
  })
  cfg <- badaniaZI:::czytaj_yaml(file.path(k, "kurs.yml"))
  cfg$id <- "s019"
  badaniaZI:::pisz_linie(yaml::as.yaml(cfg), file.path(k, "kurs.yml"))
  expect_error(plik_pracy("Z01"), "zmieniła ID")
  expect_true(file.exists(p))
})

test_that("pierwszy start tworzy przestrzeń z ID i nazwiskiem bez zmiany katalogu", {
  wyczysc_sesje()
  on.exit(wyczysc_sesje(), add = TRUE)
  rodzic <- tempfile("posit cloud ")
  dir.create(rodzic)
  on.exit(unlink(rodzic, recursive = TRUE), add = TRUE)
  withr::with_dir(rodzic, {
    expect_error(rozpocznij_zajecia("C01", otworz = FALSE), "pierwszym starcie")
    expect_error(rozpocznij_zajecia("C01", "s017", otworz = FALSE), "pierwszym starcie")
    expect_error(rozpocznij_zajecia("C01", "s017", "Imię Nazwisko", otworz = FALSE), "imię i nazwisko")
    expect_false(dir.exists("moje-badania"))
    cwd <- getwd()
    expect_message(x <- rozpocznij_zajecia("C01", "s017", "Anna  Kowalska", otworz = FALSE), "s017 \\(Anna Kowalska\\)")
    expect_identical(getwd(), cwd)
    expect_identical(x$stan, "utworzono")
    k <- normalizePath("moje-badania", winslash = "/")
    expect_identical(x$katalog, k)
    cfg <- badaniaZI:::czytaj_yaml(file.path(k, "kurs.yml"))
    expect_identical(cfg$id, "s017")
    expect_identical(cfg$student, "Anna Kowalska")
    expect_null(cfg$repo)
    expect_true(all(file.exists(file.path(k, "materialy/c01",
      c("C01.html", "C01.pdf", "rubryka-Z01.html", "rubryka-Z01.pdf")))))
    expect_false(dir.exists(file.path(k, "materialy/scenariusze")))
    expect_identical(plik_pracy("Z01"), x$plik)
  })
})

test_that("kolejny start zachowuje odpowiedzi i pilnuje ID", {
  wyczysc_sesje()
  on.exit(wyczysc_sesje(), add = TRUE)
  k <- tempfile("start sesji ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  otwarty <- NULL
  local_mocked_bindings(otworz_plik_pracy = function(plik) { otwarty <<- plik; invisible(TRUE) },
    .package = "badaniaZI")
  x <- rozpocznij_zajecia("C01", "s017", "Anna Kowalska", katalog = k)
  p <- x$plik
  expect_identical(otwarty, p)
  t <- readLines(p, encoding = "UTF-8")
  t <- sub("[UZUPELNIJ_S01]", "Własny krótki akapit.", t, fixed = TRUE)
  badaniaZI:::pisz_linie(t, p)
  hash <- digest::digest(file = p)
  expect_message(x2 <- rozpocznij_zajecia("C01", otworz = FALSE), "ID: s017")
  expect_identical(x2$stan, "otwarto")
  expect_identical(x2$plik, p)
  expect_identical(digest::digest(file = p), hash)
  expect_error(rozpocznij_zajecia("C01", "s018", katalog = k), "należy do ID s017")
  expect_message(rozpocznij_zajecia("C02", "s017", "Anna Maria Kowalska", katalog = k, otworz = FALSE),
                 "Zapisano imię i nazwisko")
  expect_identical(badaniaZI:::czytaj_yaml(file.path(k, "kurs.yml"))$student, "Anna Maria Kowalska")
  expect_identical(digest::digest(file = p), hash)
})

test_that("przestrzeń bez nazwiska przypomina o jego dopisaniu", {
  wyczysc_sesje()
  on.exit(wyczysc_sesje(), add = TRUE)
  k <- tempfile("bez nazwiska ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k)
  expect_message(rozpocznij_zajecia("C01", katalog = k, otworz = FALSE), "Brakuje imienia i nazwiska")
})

test_that("każdy start przypomina wykonanie chunków, miejsce odpowiedzi i numer oddania", {
  wyczysc_sesje()
  on.exit(wyczysc_sesje(), add = TRUE)
  k <- tempfile("przypomnienie pracy ")
  on.exit(unlink(k, recursive = TRUE, force = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k, student = "Anna Kowalska")
  local_mocked_bindings(
    przygotuj_zadanie = function(id, katalog) file.path(katalog, "zadania", tolower(id), "zadanie.Rmd"),
    .package = "badaniaZI")
  for (nr in sprintf("%02d", 1:10)) {
    msg <- character()
    withCallingHandlers(rozpocznij_zajecia(paste0("C", nr), katalog = k, otworz = FALSE),
      message = function(m) { msg <<- c(msg, conditionMessage(m)); invokeRestart("muffleMessage") })
    txt <- paste(msg, collapse = "\n")
    expect_match(txt, "pierwszy blok R (chunk)", fixed = TRUE)
    expect_match(txt, "poza blokami R", fixed = TRUE)
    expect_match(txt, "[UZUPELNIJ_S01]", fixed = TRUE)
    expect_match(txt, "[UZUPELNIJ_S05]", fixed = TRUE)
    expect_match(txt, paste0('oddaj_zadanie("Z', nr, '")'), fixed = TRUE)
    expect_match(txt, "Poczekaj na potwierdzenie oddania", fixed = TRUE)
    expect_match(txt, paste0("materialy/c", nr), fixed = TRUE)
  }
  karty <- list.files(file.path(k, "materialy/scenariusze"), "^S[0-9]{2}[.](html|pdf)$")
  expect_length(karty, 40L)
})

test_that("otwarcie edytora używa navigateToFile i obsługuje brak API", {
  skip_if_not_installed("rstudioapi")
  otwarty <- NULL
  local_mocked_bindings(isAvailable = function(...) TRUE,
    hasFun = function(name, ...) identical(name, "navigateToFile"),
    navigateToFile = function(file, ...) { otwarty <<- file },
    openProject = function(...) stop("Zmiana projektu jest zabroniona"), .package = "rstudioapi")
  expect_true(badaniaZI:::otworz_plik_pracy("test.Rmd"))
  expect_identical(otwarty, "test.Rmd")
  with_mocked_bindings({
    expect_message(expect_false(badaniaZI:::otworz_plik_pracy("test.Rmd")), "bieżącym edytorze")
  }, hasFun = function(...) FALSE, .package = "rstudioapi")
})

test_that("render wskazuje własny folder niezależnie od sesji i root.dir", {
  skip_if_not_installed("knitr")
  k <- tempfile("render ścieżki ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  dir.create(k)
  rmd <- file.path(k, "pelne.Rmd")
  writeLines("Test", rmd)
  writeLines("x <- 1", file.path(k, "analiza.R"))
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(current_input = function(...) rmd, .package = "knitr")
  expect_identical(plik_pracy("Z01"), normalizePath(rmd, winslash = "/"))
  expect_identical(plik_pracy("Z01", "analiza.R"), normalizePath(file.path(k, "analiza.R"), winslash = "/"))
})
