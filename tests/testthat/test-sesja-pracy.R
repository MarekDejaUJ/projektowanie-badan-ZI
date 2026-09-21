test_that("kontekst nie zmienia katalogu i nie wybiera cudzej pracy", {
  wyloguj_github(FALSE)
  on.exit(wyloguj_github(FALSE), add = TRUE)
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
  wyloguj_github(FALSE)
  expect_length(ls(badaniaZI:::sesja_pracy), 0L)
  expect_true(file.exists(p))
})

test_that("start otwiera Rmd w tej samej sesji i zachowuje lokalną odpowiedź", {
  wyloguj_github(FALSE)
  on.exit(wyloguj_github(FALSE), add = TRUE)
  k <- tempfile("start sesji ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k, repo = "kurs/praca-s017")
  p <- przygotuj_zadanie("Z01", k)
  t <- readLines(p, encoding = "UTF-8")
  t <- sub("[UZUPELNIJ_S01]", "Własny krótki akapit.", t, fixed = TRUE)
  badaniaZI:::pisz_linie(t, p)
  hash <- digest::digest(file = p)
  pid <- Sys.getpid()
  cwd <- getwd()
  auth <- badaniaZI:::sesja_github
  auth$token <- "test-token-only"
  otwarty <- NULL
  local_mocked_bindings(
    sprawdz_repo = function(katalog, id = NULL) {
      cfg <- badaniaZI:::czytaj_yaml(file.path(katalog, "kurs.yml"))
      if (!is.null(id) && !identical(cfg$id, id)) stop("To projekt innego ID.")
      cfg
    },
    otworz_plik_pracy = function(plik) { otwarty <<- plik; invisible(TRUE) },
    .package = "badaniaZI")
  local_mocked_bindings(
    git_status = function(...) data.frame(file = "zadania/z01/zadanie.Rmd"),
    git_fetch = function(...) stop("Nie wolno pobierać przy lokalnych zmianach"),
    .package = "gert")
  expect_message(x <- rozpocznij_zajecia("C01", "s017", katalog = k), "lokalne zmiany")
  expect_identical(x$aktualizacja, "lokalne_zmiany")
  expect_identical(otwarty, p)
  expect_identical(plik_pracy("Z01"), p)
  expect_identical(Sys.getpid(), pid)
  expect_identical(getwd(), cwd)
  expect_identical(badaniaZI:::token_sesji(), "test-token-only")
  expect_identical(digest::digest(file = p), hash)
  expect_message(x2 <- rozpocznij_zajecia("C01", "s017", otworz = FALSE), "ID: s017")
  expect_identical(x2$plik, p)
  expect_error(rozpocznij_zajecia("C01", "s018", katalog = k), "innego ID")
  expect_error(rozpocznij_zajecia("C01", "s017", "https://github.com/kurs/inna", k), "Adres")
  expect_identical(digest::digest(file = p), hash)
})

test_that("czysty start pobiera aktualizację, a obce metadane blokują otwarcie", {
  wyloguj_github(FALSE)
  on.exit(wyloguj_github(FALSE), add = TRUE)
  k <- tempfile("czysty start ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k, repo = "kurs/praca-s017")
  auth <- badaniaZI:::sesja_github
  auth$token <- "test-token-only"
  pobrania <- 0L
  obce <- FALSE
  local_mocked_bindings(
    sprawdz_repo = function(katalog, ...) badaniaZI:::czytaj_yaml(file.path(katalog, "kurs.yml")),
    aktualizuj_repo_lokalne = function(katalog) {
      if (obce) {
        cfg <- badaniaZI:::czytaj_yaml(file.path(katalog, "kurs.yml"))
        cfg$id <- "s018"
        badaniaZI:::pisz_linie(yaml::as.yaml(cfg), file.path(katalog, "kurs.yml"))
      }
    }, .package = "badaniaZI")
  local_mocked_bindings(
    git_status = function(...) data.frame(file = character()),
    git_fetch = function(...) { pobrania <<- pobrania + 1L }, .package = "gert")
  expect_message(x <- rozpocznij_zajecia("C01", "s017", katalog = k, otworz = FALSE), "ID: s017")
  expect_identical(x$aktualizacja, "sprawdzono")
  expect_equal(pobrania, 1L)
  obce <- TRUE
  expect_error(rozpocznij_zajecia("C02", "s017", katalog = k, otworz = FALSE), "zmieniła tożsamość")
  expect_false(dir.exists(file.path(k, "zadania/z02")))
})

test_that("pierwsze pobranie zapamiętuje nowy katalog bez zmiany sesji", {
  wyloguj_github(FALSE)
  on.exit(wyloguj_github(FALSE), add = TRUE)
  k <- tempfile("pobranie sesji ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  auth <- badaniaZI:::sesja_github
  auth$token <- "test-token-only"
  local_mocked_bindings(pobierz_zadanie = function(repo_url, katalog, id_studenta) {
    utworz_projekt(id_studenta, katalog = katalog, repo = badaniaZI:::nazwa_repo(repo_url))
  }, .package = "badaniaZI")
  expect_message(x <- rozpocznij_zajecia("C01", "s017", "https://github.com/kurs/praca-s017", k, FALSE), "ID: s017")
  expect_identical(x$aktualizacja, "pobrano")
  expect_identical(x$plik, plik_pracy("Z01"))
})

test_that("każdy start przypomina wykonanie chunków, miejsce odpowiedzi i numer oddania", {
  wyloguj_github(FALSE)
  on.exit(wyloguj_github(FALSE), add = TRUE)
  k <- tempfile("przypomnienie pracy ")
  on.exit(unlink(k, recursive = TRUE, force = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k, repo = "kurs/praca-s017")
  auth <- badaniaZI:::sesja_github
  auth$token <- "test-token-only"
  local_mocked_bindings(
    sprawdz_repo = function(katalog, ...) badaniaZI:::czytaj_yaml(file.path(katalog, "kurs.yml")),
    przygotuj_zadanie = function(id, katalog) file.path(katalog, "zadania", tolower(id), "zadanie.Rmd"),
    .package = "badaniaZI")
  local_mocked_bindings(git_status = function(...) data.frame(file = "kurs.yml"), .package = "gert")
  for (nr in sprintf("%02d", 1:10)) {
    msg <- character()
    withCallingHandlers(rozpocznij_zajecia(paste0("C", nr), "s017", katalog = k, otworz = FALSE),
      message = function(m) { msg <<- c(msg, conditionMessage(m)); invokeRestart("muffleMessage") })
    txt <- paste(msg, collapse = "\n")
    expect_match(txt, "pierwszy blok R (chunk)", fixed = TRUE)
    expect_match(txt, "poza blokami R", fixed = TRUE)
    expect_match(txt, "[UZUPELNIJ_S01]", fixed = TRUE)
    expect_match(txt, "[UZUPELNIJ_S05]", fixed = TRUE)
    expect_match(txt, paste0('oddaj_zadanie("Z', nr, '")'), fixed = TRUE)
    expect_match(txt, "Poczekaj na potwierdzenie odbioru", fixed = TRUE)
  }
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
