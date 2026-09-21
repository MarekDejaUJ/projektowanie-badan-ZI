uzupelniony_wzorzec <- function(id) {
  p <- if (id == "PROJEKT") badaniaZI:::zasob("szablony", "projekt", "raport.Rmd") else
    badaniaZI:::zasob("materialy", sub("^Z", "c", id), "pelne.Rmd")
  baza <- readLines(p, encoding = "UTF-8")
  tekst <- baza
  for (pole in badaniaZI:::pola_pracy(id)) tekst <- badaniaZI:::wstaw_odpowiedz(tekst, pole,
    c("Krótki **własny akapit**: wynik dotyczy próby, nie wszystkich osób.",
      "", "Oszacowanie $\\bar{x}$ ma jednostkę minut; p < 0,05 nie mierzy wielkości efektu."))
  list(tekst = tekst, wzorzec = baza)
}

test_that("pełne 10 Rmd i raport zachowują akapity bez limitu słów", {
  for (id in c(sprintf("Z%02d", 1:10), "PROJEKT")) {
    x <- uzupelniony_wzorzec(id)
    expect_true(badaniaZI:::sprawdz_kontrakt_rmd(x$tekst, x$wzorzec, id))
    expect_error(badaniaZI:::sprawdz_kontrakt_rmd(x$wzorzec, x$wzorzec, id), "Uzupełnij")
  }
  expect_true(badaniaZI:::czy_odpowiedz("Minimum wynosi 2 minuty."))
  expect_false(badaniaZI:::czy_odpowiedz(c("<!--", "Ukryta odpowiedź", "-->")))
  expect_false(badaniaZI:::czy_odpowiedz("**...**"))
  expect_false(badaniaZI:::czy_odpowiedz("[UZUPELNIJ_S01]"))
})

test_that("tylko jawne wybory w konkretnych chunkach są dozwolone", {
  x <- uzupelniony_wzorzec("Z01")
  t <- sub("numer = 3", "numer = 4", x$tekst, fixed = TRUE)
  expect_true(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z01"))
  t <- sub("numer = 3", "numer = 999", x$tekst, fixed = TRUE)
  expect_error(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z01"), "odczyt-rekordu")
  x <- uzupelniony_wzorzec("Z08")
  t <- sub('miara = "Pearson"', 'miara = "Spearman"', x$tekst, fixed = TRUE)
  # Polecenie w narracji pozostaje bez zmian; wybór jest tylko w chunku.
  t[!grepl('^korelacja <-', t)] <- x$tekst[!grepl('^korelacja <-', t)]
  expect_true(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z08"))
  x <- uzupelniony_wzorzec("Z09")
  for (s in sprintf("S%02d", 1:20)) {
    t <- x$tekst
    i <- grep('^projekt <- przygotuj_wariant_projektu', t)
    t[i] <- sub("S02", s, t[i], fixed = TRUE)
    expect_true(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z09"))
  }
})

test_that("zmiana kodu, YAML, opcji lub ukrycie odpowiedzi blokuje render", {
  x <- uzupelniony_wzorzec("Z01")
  sentinel <- tempfile("nie-wykonuj-")
  kod <- paste0('system("echo nie-wykonuj"); file.create(', dQuote(sentinel, FALSE), ')')
  proby <- list(
    sub('print(osoba)', kod, x$tekst, fixed = TRUE),
    sub('print(osoba)', paste0('#| eval: !expr ', kod, '\nprint(osoba)'), x$tekst, fixed = TRUE),
    sub('include=FALSE', paste0('include=FALSE, eval={', kod, '}'), x$tekst, fixed = TRUE),
    sub('source(badaniaZI::plik_pracy("Z01", "analiza.R"),', 'source("obcy.R",', x$tekst, fixed = TRUE),
    append(x$tekst, c('```{r nowy}', kod, '```')),
    sub('author:', 'knit: !expr system("echo nie-wykonuj")\nauthor:', x$tekst, fixed = TRUE),
    sub('ID: [UZUPELNIJ_ID]', 'ID: cudze_ID', x$tekst, fixed = TRUE))
  for (t in proby) expect_error(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z01"))
  expect_false(file.exists(sentinel))
  for (s in c('`r system("echo nie-wykonuj")`', '`r#komentarz\n1+1`',
              '<script>test</script>', '\\input{prywatny.txt}', '\\csname input\\endcsname{x}',
              '![obraz](https://example.org/test.png)', '^^5cinput{x}',
              '```{r}\n1+1\n```', 'Tekst\n---\nknit: obce\n---',
              'Tekst przed <!-- niezamkniętym komentarzem')) {
    t <- badaniaZI:::wstaw_odpowiedz(x$tekst, "S01", strsplit(s, "\n", fixed = TRUE)[[1]])
    expect_error(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z01"))
  }
})

test_that("brak tekstu nie niszczy granic pola podczas porównania", {
  x <- uzupelniony_wzorzec("Z01")
  t <- badaniaZI:::wstaw_odpowiedz(x$tekst, "S01", character())
  expect_identical(badaniaZI:::podziel_rmd(t, badaniaZI:::pola_pracy("Z01"))$tresc,
    badaniaZI:::podziel_rmd(x$wzorzec, badaniaZI:::pola_pracy("Z01"))$tresc)
  expect_error(badaniaZI:::sprawdz_kontrakt_rmd(t, x$wzorzec, "Z01"), "Uzupełnij")
})
