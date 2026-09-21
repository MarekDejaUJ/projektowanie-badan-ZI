praca_historii_test <- function() {
  k <- tempfile("historia pracy ")
  utworz_projekt("s017", katalog = k, repo = "kurs/praca-s017")
  gert::git_init(k)
  gert::git_config_set("user.name", "Test kursu", repo = k)
  gert::git_config_set("user.email", "test@example.org", repo = k)
  gert::git_add(c("kurs.yml", "moje-badania.Rproj", ".gitignore", ".gitattributes"), repo = k)
  baza <- gert::git_commit("Przestrzeń ćwiczeń", repo = k)
  if (gert::git_info(repo = k)$shorthand != "main") gert::git_branch_create("main", checkout = TRUE, repo = k)
  p <- przygotuj_zadanie("Z01", k)
  t <- readLines(p, encoding = "UTF-8")
  for (s in sprintf("S%02d", 1:5)) t <- badaniaZI:::wstaw_odpowiedz(t, s, "Moja interpretacja wyników.")
  badaniaZI:::pisz_linie(t, p)
  list(katalog = k, baza = baza)
}

render_historii_test <- function(wejscie, katalog, timeout) {
  pdf <- sub("[.]Rmd$", ".pdf", file.path(katalog, wejscie))
  writeBin(charToRaw(paste0("%PDF-", paste(rep("TEST", 100), collapse = ""))), pdf)
  list(status = 0L)
}

zapisz_commit_test <- function(k, pliki, tekst = "Oddaj Z01") {
  gert::git_add(pliki, repo = k)
  gert::git_commit(tekst, repo = k)
}

test_that("cała liniowa historia poprawnego ponowienia jest sprawdzana bez zmian pracy", {
  x <- praca_historii_test()
  k <- x$katalog
  on.exit({ gc(); unlink(k, recursive = TRUE) }, add = TRUE)
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_historii_test, .package = "badaniaZI")
  r <- sprawdz_zadanie("Z01", k)
  expect_true(r$ok)
  pierwsza <- zapisz_commit_test(k, r$pliki)
  expect_true(badaniaZI:::sprawdz_historie_oddania(k, "Z01", r$pliki, pierwsza, x$baza))
  p <- file.path(k, "zadania/z01/zadanie.Rmd")
  t <- badaniaZI:::wstaw_odpowiedz(readLines(p, encoding = "UTF-8"), "S02", "Poprawiona interpretacja.")
  badaniaZI:::pisz_linie(t, p)
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  druga <- zapisz_commit_test(k, r$pliki)
  h <- badaniaZI:::hashe_plikow(k, r$pliki)
  expect_true(badaniaZI:::sprawdz_historie_oddania(k, "Z01", r$pliki, druga, x$baza))
  expect_identical(gert::git_info(repo = k)$commit, druga)
  expect_identical(badaniaZI:::hashe_plikow(k, r$pliki), h)
  expect_equal(nrow(gert::git_status(repo = k)), 0L)
})

test_that("dodana i później usunięta notatka nadal blokuje wysłanie historii", {
  x <- praca_historii_test()
  k <- x$katalog
  on.exit({ gc(); unlink(k, recursive = TRUE) }, add = TRUE)
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_historii_test, .package = "badaniaZI")
  r <- sprawdz_zadanie("Z01", k)
  zapisz_commit_test(k, r$pliki)
  writeLines("Prywatna notatka.", file.path(k, "notatka.txt"))
  zapisz_commit_test(k, "notatka.txt", "Notatka")
  gert::git_rm("notatka.txt", repo = k)
  sha <- gert::git_commit("Usunięcie notatki", repo = k)
  expect_false("notatka.txt" %in% gert::git_ls(repo = k)$path)
  expect_error(badaniaZI:::sprawdz_historie_oddania(k, "Z01", r$pliki, sha, x$baza), "Niewysłana historia")
  expect_identical(gert::git_info(repo = k)$commit, sha)
})

test_that("sekret z wcześniejszego akapitu nie znika z historii po poprawce", {
  x <- praca_historii_test()
  k <- x$katalog
  on.exit({ gc(); unlink(k, recursive = TRUE) }, add = TRUE)
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_historii_test, .package = "badaniaZI")
  r <- sprawdz_zadanie("Z01", k)
  p <- file.path(k, "zadania/z01/zadanie.Rmd")
  t <- readLines(p, encoding = "UTF-8")
  sekret <- paste0("ghp_", strrep("A", 32))
  badaniaZI:::pisz_linie(badaniaZI:::wstaw_odpowiedz(t, "S03", sekret), p)
  zapisz_commit_test(k, r$pliki, "Zapis")
  badaniaZI:::pisz_linie(t, p)
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  sha <- zapisz_commit_test(k, r$pliki, "Poprawka")
  blad <- tryCatch(badaniaZI:::sprawdz_historie_oddania(k, "Z01", r$pliki, sha, x$baza), error = conditionMessage)
  expect_match(blad, "token GitHub", fixed = TRUE)
  expect_false(grepl(sekret, blad, fixed = TRUE))
  expect_identical(gert::git_info(repo = k)$commit, sha)
})

test_that("niepełny historyczny Rmd oraz merge nie są pomijane", {
  x <- praca_historii_test()
  k <- x$katalog
  on.exit({ gc(); unlink(k, recursive = TRUE) }, add = TRUE)
  local_mocked_bindings(sprawdz_narzedzia_pdf = function() TRUE,
    uruchom_render_pracy = render_historii_test, .package = "badaniaZI")
  r <- sprawdz_zadanie("Z01", k)
  p <- file.path(k, "zadania/z01/zadanie.Rmd")
  t <- readLines(p, encoding = "UTF-8")
  badaniaZI:::pisz_linie(badaniaZI:::wstaw_odpowiedz(t, "S03", "[UZUPELNIJ]"), p)
  pierwsza <- zapisz_commit_test(k, r$pliki)
  badaniaZI:::pisz_linie(t, p)
  expect_true(sprawdz_zadanie("Z01", k)$ok)
  sha <- zapisz_commit_test(k, r$pliki)
  expect_error(badaniaZI:::sprawdz_historie_oddania(k, "Z01", r$pliki, sha, x$baza), "Niewysłana historia")
  with_mocked_bindings({
    expect_error(badaniaZI:::sprawdz_historie_oddania(k, "Z01", r$pliki, sha, x$baza), "Niewysłana historia")
  }, git_commit_info = function(...) list(parents = c(x$baza, pierwsza)), .package = "gert")
})
