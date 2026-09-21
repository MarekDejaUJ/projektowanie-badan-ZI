praca_pdf_test <- function() {
  k <- tempfile("pdf żółty ")
  utworz_projekt("s017", katalog = k)
  p <- przygotuj_zadanie("Z01", k)
  tekst <- readLines(p, encoding = "UTF-8")
  for (pole in sprintf("S%02d", 1:5)) tekst <- badaniaZI:::wstaw_odpowiedz(tekst, pole, "Własny krótki akapit.")
  badaniaZI:::pisz_linie(tekst, p)
  k
}

# Atrapa sprawdza tylko transakcję i hashe, nie poprawność składu PDF.
render_pdf_test <- function(wejscie, katalog, timeout) {
  pdf <- sub("[.]Rmd$", ".pdf", file.path(katalog, wejscie))
  writeBin(charToRaw(paste0("%PDF-", paste(rep("TEST", 100), collapse = ""))), pdf)
  list(status = 0L)
}

test_that("PDF i wejścia mają hashe, a dodatkowe pliki nie należą do oddania", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_pdf_test, .package = "badaniaZI")
  w <- badaniaZI:::kontroluj_wejscia_rmd("Z01", k)
  writeLines("prywatna notatka", file.path(w$folder, "notatka.txt"))
  writeLines("nie wykonuj profilu", file.path(k, ".Rprofile"))
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  expect_true(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
  p <- badaniaZI:::pliki_oddania("Z01", k)
  expect_length(p, 7L)
  expect_false(any(grepl("notatka|Rprofile|html|log", p)))
  expect_identical(badaniaZI:::hashe_plikow(k, w$pliki), w$hashe)
  rmd <- file.path(w$folder, w$rmd)
  tekst <- readLines(rmd, encoding = "UTF-8")
  badaniaZI:::pisz_linie(badaniaZI:::wstaw_odpowiedz(tekst, "S02", "Zmieniłem interpretację."), rmd)
  expect_false(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  pdf <- file.path(w$folder, "zadanie.pdf")
  writeBin(charToRaw(paste0("%PDF-", paste(rep("PODMIANA", 100), collapse = ""))), pdf)
  expect_false(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
})

test_that("identyczny świeży render zachowuje PDF i datę pierwszego wyniku", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  wywolania <- 0L
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = function(...) {
      wywolania <<- wywolania + 1L
      render_pdf_test(...)
    }, .package = "badaniaZI")
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  p <- file.path(k, "zadania/z01/pdf.json")
  meta <- jsonlite::read_json(p, simplifyVector = FALSE)
  meta$utworzono_utc <- "2026-01-01T00:00:00Z"
  badaniaZI:::pisz_linie(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), p)
  pliki <- badaniaZI:::pliki_oddania("Z01", k)
  przed <- badaniaZI:::hashe_plikow(k, pliki)
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  expect_equal(wywolania, 2L)
  expect_identical(badaniaZI:::hashe_plikow(k, pliki), przed)
  # Fałszywe hashe nie chronią podmienionego wyniku: świeży render go zastępuje.
  pdf <- file.path(k, "zadania/z01/zadanie.pdf")
  writeBin(charToRaw(paste0("%PDF-", strrep("FAKE", 100))), pdf)
  meta$sha256_pdf <- digest::digest(file = pdf, algo = "sha256")
  badaniaZI:::pisz_linie(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), p)
  expect_true(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  expect_equal(wywolania, 3L)
  expect_identical(digest::digest(file = pdf, algo = "sha256"), unname(przed["zadania/z01/zadanie.pdf"]))
})

test_that("błąd i zmiana źródła podczas renderu zachowują poprzedni PDF", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_pdf_test, .package = "badaniaZI")
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  pliki <- badaniaZI:::pliki_oddania("Z01", k)
  przed <- badaniaZI:::hashe_plikow(k, pliki)
  with_mocked_bindings({
    r <- sprawdz_zadanie("Z01", k)
    expect_false(r$ok)
    expect_match(tail(r$kontrole$opis, 1), "Zachowano odpowiedzi")
    expect_false(any(grepl("NIE POKAZUJ SEKRETU", r$kontrole$opis)))
  }, uruchom_render_pracy = function(...) list(status = 1L, stderr = "NIE POKAZUJ SEKRETU"), .package = "badaniaZI")
  expect_identical(badaniaZI:::hashe_plikow(k, pliki), przed)
  with_mocked_bindings({
    r <- sprawdz_zadanie("Z01", k)
    expect_false(r$ok)
    expect_match(tail(r$kontrole$opis, 1), "Wejścia zmieniły")
  }, uruchom_render_pracy = function(wejscie, katalog, timeout) {
    wynik <- render_pdf_test(wejscie, katalog, timeout)
    writeLines("inna zawartość", file.path(katalog, wejscie))
    wynik
  }, .package = "badaniaZI")
  expect_identical(badaniaZI:::hashe_plikow(k, pliki), przed)
})

test_that("renderer otrzymuje odrębny pusty dom, usuwany także po błędzie", {
  k <- tempfile("test-domu-renderu-")
  dir.create(k)
  on.exit(unlink(k, recursive = TRUE, force = TRUE), add = TRUE)
  original_home <- Sys.getenv("HOME", unset = NA_character_)
  render_home <- NULL
  fail <- FALSE
  local_mocked_bindings(run = function(command, args, wd, env, ...) {
    render_home <<- unname(env["HOME"])
    expect_true(dir.exists(render_home))
    expect_true(startsWith(render_home, paste0(normalizePath(k, winslash = "/"), "/")))
    expect_length(list.files(render_home, all.files = TRUE, no.. = TRUE), 0L)
    expect_false(identical(render_home, original_home))
    if (fail) stop("testowy timeout")
    list(status = 0L)
  }, .package = "processx")
  expect_equal(badaniaZI:::uruchom_render_pracy("zadanie.Rmd", k, 10)$status, 0L)
  expect_false(dir.exists(render_home))
  fail <- TRUE
  expect_error(badaniaZI:::uruchom_render_pracy("zadanie.Rmd", k, 10), "testowy timeout")
  expect_false(dir.exists(render_home))
  expect_identical(Sys.getenv("HOME", unset = NA_character_), original_home)
  expect_true(dir.exists(k))
})

test_that("świeży R nie dziedziczy tokenów, profilu ani dowolnych zmiennych", {
  withr::local_envvar(c(GH_TOKEN = "tajne", OPENAI_API_KEY = "tajne", R_TESTS = "tajne",
    R_PROFILE_USER = "tajne", R_ENVIRON_USER = "tajne", GH_CONFIG_DIR = "tajne"))
  env <- badaniaZI:::srodowisko_renderu()
  expect_false(any(c("GH_TOKEN", "OPENAI_API_KEY", "R_TESTS", "R_PROFILE_USER", "R_ENVIRON_USER", "GH_CONFIG_DIR") %in% names(env)))
  expect_false("current" %in% env)
  p <- tempfile(fileext = ".R")
  on.exit(unlink(p), add = TRUE)
  writeLines('stopifnot(all(Sys.getenv(c("GH_TOKEN", "OPENAI_API_KEY", "R_TESTS", "R_PROFILE_USER", "R_ENVIRON_USER", "GH_CONFIG_DIR")) == ""))', p)
  r <- processx::run(badaniaZI:::rscript_bin(), c("--vanilla", p), env = env,
                    timeout = 20, windows_hide_window = TRUE)
  expect_equal(r$status, 0L)
  expect_equal(Sys.getenv("GH_TOKEN"), "tajne")
})

test_that("oddanie tworzy PDF przed połączeniem i zachowuje go offline", {
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  polaczenia <- 0L
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_pdf_test,
    sprawdz_repo = function(katalog) {
      polaczenia <<- polaczenia + 1L
      expect_true(sprawdz_zadanie("Z01", katalog, uruchom = FALSE)$ok)
      stop("Brak sieci")
    }, .package = "badaniaZI")
  expect_error(oddaj_zadanie("Z01", k), "Brak sieci")
  expect_equal(polaczenia, 1L)
  expect_true(sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok)
  p <- file.path(k, "zadania/z01/zadanie.Rmd")
  tekst <- readLines(p, encoding = "UTF-8")
  badaniaZI:::pisz_linie(badaniaZI:::wstaw_odpowiedz(tekst, "S03", "[UZUPELNIJ]"), p)
  expect_error(oddaj_zadanie("Z01", k), "S03")
  expect_equal(polaczenia, 1L)
})

test_that("niezapisany bufor edytora wymaga zapisu, nie jest nadpisywany", {
  skip_if_not_installed("rstudioapi")
  k <- praca_pdf_test()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  p <- file.path(k, "zadania/z01/zadanie.Rmd")
  tekst <- readLines(p, encoding = "UTF-8")
  hash <- digest::digest(file = p)
  bufor <- c(tekst, "Niezapisana zmiana.")
  local_mocked_bindings(isAvailable = function(...) TRUE, hasFun = function(...) TRUE,
    getSourceEditorContext = function(...) list(path = p, contents = bufor), .package = "rstudioapi")
  expect_error(badaniaZI:::sprawdz_zapis_edytora(p), "Zapisz odpowiedzi")
  expect_identical(digest::digest(file = p), hash)
  bufor <- c(tekst, "")
  expect_true(badaniaZI:::sprawdz_zapis_edytora(p))
})
