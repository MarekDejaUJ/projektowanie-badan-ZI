oddanie_atrapa <- function(nc, odpowiedz = "t") {
  pokazane <- character()
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    narzedzia_pdf_gotowe = function() TRUE,
    uruchom_render_pracy = render_pdf_test, nc_put = nc$put,
    zapytaj = function(...) odpowiedz, pokaz_plik = function(p) pokazane <<- c(pokazane, p),
    .package = "badaniaZI", .env = parent.frame())
  invisible(NULL)
}

test_that("oddanie wysyła PDF do folderu zadania pod nazwą z ID i nazwiskiem", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  nc <- atrapa_nc(201L)
  oddanie_atrapa(nc)
  expect_message(x <- oddaj_zadanie("z01", k, potwierdz = FALSE), "Oddano Z01")
  expect_identical(x$stan, "wyslano")
  expect_identical(x$pliki, "s017_kowalska_anna_Z01.pdf")
  expect_length(nc$wywolania, 1L)
  w <- nc$wywolania[[1]]
  expect_identical(w$url, "https://nc.uj.edu.pl/public.php/dav/files/KtECTnaanMCQ7jm/s017_kowalska_anna_Z01.pdf")
  expect_true("X-Requested-With: XMLHttpRequest" %in% w$naglowki)
  expect_true(any(grepl("^X-NC-Nickname: Anna%20Kowalska%20s017$", w$naglowki)))
  pdf <- file.path(k, "zadania/z01/zadanie.pdf")
  expect_identical(w$bajty, readBin(pdf, "raw", file.info(pdf)$size))
  r <- status_oddania("Z01", k)
  expect_equal(nrow(r), 1L)
  expect_identical(r$plik, "s017_kowalska_anna_Z01.pdf")
  expect_identical(r$sha256, digest::digest(file = pdf, algo = "sha256"))
  expect_identical(x$sha256, r$sha256)
  oddaj_zadanie("Z01", k, potwierdz = FALSE)
  expect_equal(nrow(status_oddania("Z01", k)), 2L)
  expect_message(expect_equal(nrow(status_oddania("Z02", k)), 0L), "Brak zapisu oddania Z02")
})

test_that("potwierdzenie pokazuje PDF, a odpowiedź n niczego nie wysyła", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  nc <- atrapa_nc(201L)
  pokazane <- character()
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE, narzedzia_pdf_gotowe = function() TRUE,
    uruchom_render_pracy = render_pdf_test, nc_put = nc$put, zapytaj = function(...) "n",
    pokaz_plik = function(p) pokazane <<- c(pokazane, p), .package = "badaniaZI")
  expect_message(x <- oddaj_zadanie("Z01", k, potwierdz = TRUE), "Nie wysłano")
  expect_identical(x$stan, "niewyslane")
  expect_length(nc$wywolania, 0L)
  expect_match(pokazane, "zadania/z01/zadanie.pdf$")
  expect_false(file.exists(file.path(k, "oddania.csv")))
  local_mocked_bindings(zapytaj = function(...) "t", .package = "badaniaZI")
  expect_identical(oddaj_zadanie("Z01", k, potwierdz = TRUE)$stan, "wyslano")
  expect_length(nc$wywolania, 1L)
})

test_that("odmowa serwera i brak sieci zostawiają pracę i podają drogę ręczną", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  przypadki <- list(
    list(atrapa_nc(503L, "<s:exception>Sabre\\DAV\\Exception\\ServiceUnavailable</s:exception><s:message>Sabre\\DAV\\Exception\\NotFound: </s:message>"), "minął termin"),
    list(atrapa_nc(403L), "bez hasła"),
    list(atrapa_nc(507L), "zabrakło miejsca"),
    list(atrapa_nc(500L), "HTTP 500"))
  for (p in przypadki) {
    oddanie_atrapa(p[[1]])
    blad <- tryCatch(oddaj_zadanie("Z01", k, potwierdz = FALSE), error = conditionMessage)
    expect_match(blad, p[[2]])
    expect_match(blad, "https://nc.uj.edu.pl/s/KtECTnaanMCQ7jm", fixed = TRUE)
    expect_match(blad, "zadanie.pdf", fixed = TRUE)
  }
  expect_false(file.exists(file.path(k, "oddania.csv")))
  expect_true(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
})

test_that("bez nazwiska, w trakcie renderu i z niepełną pracą nic nie jest wysyłane", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  nc <- atrapa_nc(201L)
  oddanie_atrapa(nc)
  cfg <- badaniaZI:::czytaj_yaml(file.path(k, "kurs.yml"))
  cfg$student <- NULL
  badaniaZI:::pisz_linie(yaml::as.yaml(cfg), file.path(k, "kurs.yml"))
  expect_error(oddaj_zadanie("Z01", k, potwierdz = FALSE), "Brakuje imienia i nazwiska")
  cfg$student <- "Anna Kowalska"
  badaniaZI:::pisz_linie(yaml::as.yaml(cfg), file.path(k, "kurs.yml"))
  withr::with_options(list(knitr.in.progress = TRUE),
    expect_error(oddaj_zadanie("Z01", k, potwierdz = FALSE), "w konsoli"))
  expect_error(oddaj_zadanie("Z11", k), "zadanie")
  expect_error(oddaj_zadanie("Z01", k, potwierdz = NA), "TRUE albo FALSE")
  p <- file.path(k, "zadania/z01/zadanie.Rmd")
  t <- readLines(p, encoding = "UTF-8")
  badaniaZI:::pisz_linie(badaniaZI:::wstaw_odpowiedz(t, "S02", "[UZUPELNIJ_S02]"), p)
  expect_error(oddaj_zadanie("Z01", k, potwierdz = FALSE), "Niczego nie wysłano")
  expect_length(nc$wywolania, 0L)
})

test_that("projekt trafia do folderu projektu jako PDF i ZIP z kompletem źródeł", {
  k <- tempfile("projekt oddanie ")
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  utworz_projekt("s017", katalog = k, student = "Anna Kowalska")
  przygotuj_zadanie("Z09", k)
  wariant_projektu("Z09", "S02", katalog = k)
  przygotuj_zadanie("Z10", k)
  r <- przygotuj_raport(k)
  t <- readLines(r, encoding = "UTF-8")
  for (pole in sprintf("P%02d", 1:11)) t <- badaniaZI:::wstaw_odpowiedz(t, pole, "Własny akapit raportu.")
  badaniaZI:::pisz_linie(t, r)
  nc <- atrapa_nc(201L)
  oddanie_atrapa(nc)
  x <- oddaj_projekt(k, potwierdz = FALSE)
  expect_identical(x$pliki, c("s017_kowalska_anna_PROJEKT.pdf", "s017_kowalska_anna_PROJEKT_zrodla.zip"))
  adresy <- vapply(nc$wywolania, `[[`, character(1), "url")
  expect_true(all(startsWith(adresy, "https://nc.uj.edu.pl/public.php/dav/files/4Cr3tne4GDLkn5s/")))
  zip <- tempfile(fileext = ".zip")
  on.exit(unlink(zip), add = TRUE)
  writeBin(nc$wywolania[[2]]$bajty, zip)
  zawartosc <- zip::zip_list(zip)$filename
  expect_setequal(zawartosc, badaniaZI:::pliki_oddania("PROJEKT", k))
  expect_true(all(c("kurs.yml", "projekty/ilosciowy/raport.Rmd", "projekty/ilosciowy/analiza.R",
                    "projekty/ilosciowy/raport.pdf") %in% zawartosc))
  expect_equal(nrow(status_oddania("PROJEKT", k)), 2L)
})

test_that("adres wysyłki i folder zadania pochodzą z konfiguracji rocznika", {
  cfg <- konfiguracja_kursu()
  foldery <- vapply(c(sprintf("Z%02d", 1:10), "PROJEKT"), function(id) badaniaZI:::folder_oddania(id, cfg), character(1))
  expect_false(anyDuplicated(foldery) > 0)
  expect_true(all(grepl("^https://nc[.]uj[.]edu[.]pl/s/[A-Za-z0-9]+$", foldery)))
  expect_identical(badaniaZI:::adres_wysylki("https://nc.uj.edu.pl/index.php/s/ABC123", "a b.pdf"),
                   "https://nc.uj.edu.pl/public.php/dav/files/ABC123/a%20b.pdf")
  cfg$foldery_oddania$Z03 <- "http://inny.serwer/plik"
  expect_error(badaniaZI:::folder_oddania("Z03", cfg), "folderu oddania")
  expect_true("foldery_oddania" %in% sprawdz_konfiguracje(cfg)$pole)
  cfg <- konfiguracja_kursu()
  cfg$foldery_oddania$Z02 <- cfg$foldery_oddania$Z01
  expect_true("foldery_oddania" %in% sprawdz_konfiguracje(cfg)$pole)
})

test_that("ostrzeżenie po terminie nie blokuje oddania", {
  cfg <- konfiguracja_kursu()
  cfg$terminy_zadan$Z03 <- "2020-01-01T10:00:00+01:00"
  expect_message(expect_true(badaniaZI:::ostrzez_po_terminie("Z03", cfg)), "termin oddania Z03 minął")
  cfg$termin_projektu <- "2999-01-01T10:00:00+01:00"
  expect_false(badaniaZI:::ostrzez_po_terminie("PROJEKT", cfg))
  expect_false(badaniaZI:::ostrzez_po_terminie("Z05", konfiguracja_kursu()))
})

test_that("po błędzie sieci wysyłka jest powtarzana raz, a odmowa serwera nie", {
  plik <- tempfile(fileext = ".pdf")
  on.exit(unlink(plik), add = TRUE)
  writeBin(charToRaw("%PDF-test"), plik)
  proby <- 0L
  local_mocked_bindings(pauza_ponowienia = function() 0,
    nc_put = function(...) { proby <<- proby + 1L
      if (proby == 1L) stop("Timeout was reached") else list(status_code = 201L, content = raw()) },
    .package = "badaniaZI")
  expect_null(badaniaZI:::wyslij_do_folderu(plik, "a.pdf", "https://nc.uj.edu.pl/s/ABC", "Anna Kowalska s017"))
  expect_equal(proby, 2L)
  proby <- 0L
  local_mocked_bindings(nc_put = function(...) { proby <<- proby + 1L; stop("Timeout was reached") }, .package = "badaniaZI")
  wynik <- badaniaZI:::wyslij_do_folderu(plik, "a.pdf", "https://nc.uj.edu.pl/s/ABC", "x")
  expect_match(wynik, "brak połączenia")
  expect_equal(proby, 2L)
  proby <- 0L
  local_mocked_bindings(nc_put = function(...) { proby <<- proby + 1L; list(status_code = 403L, content = raw()) }, .package = "badaniaZI")
  wynik <- badaniaZI:::wyslij_do_folderu(plik, "a.pdf", "https://nc.uj.edu.pl/s/ABC", "x")
  expect_match(wynik, "bez hasła")
  expect_equal(proby, 1L)
})
