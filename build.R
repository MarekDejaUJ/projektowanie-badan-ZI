# Buduje książkę kursu w katalogu _book/ i sprawdza pliki zadań w nowych sesjach R.
# Uruchom z katalogu repozytorium:
#   Rscript build.R              książka i test plików zadań (HTML)
#   Rscript build.R --pdf        dodatkowo PDF każdego pliku zadania (wymaga XeLaTeX)
#   Rscript build.R --oddaj      dodatkowo oddaj_zadanie(..., wyslij = FALSE) dla każdego pliku
#   Rscript build.R --bez-testow tylko książka
#   Rscript build.R --linki      dodatkowo sprawdza adresy DOI i adresy dostępu z lektury.yml (wymaga sieci)

argumenty <- commandArgs(trailingOnly = TRUE)
if (!file.exists("ksiazka/_bookdown.yml")) stop("Uruchom Rscript build.R z katalogu repozytorium.")
korzen <- normalizePath(".", winslash = "/")
adres_materialow <- "https://marekdejauj.github.io/projektowanie-badan-ZI/materialy/"
adres_kursu <- paste0(adres_materialow, "start.R")
pandoc <- rmarkdown::pandoc_exec()

# 0. Bibliografia: każdy cytowany klucz ma wpis, a każdy wpis ma pozycję w lektury.yml.
pandoc_json <- function(argumenty_pandoc) {
  wynik <- tempfile(fileext = ".json")
  stopifnot(system2(pandoc, c(argumenty_pandoc, "-o", shQuote(wynik))) == 0)
  jsonlite::read_json(wynik, simplifyVector = FALSE)
}
wpisy_bibliografii <- function() pandoc_json(c(shQuote("ksiazka/bibliografia.yaml"), "-s", "-f", "markdown", "-t", "csljson"))
klucze_cytowan <- function(plik) {
  # Kod R (bloki i kod w wierszu) zastępujemy pustym tekstem: surowy kod z `$` myli parser markdown pandoc.
  t <- readLines(plik, encoding = "UTF-8", warn = FALSE)
  w_kodzie <- FALSE
  for (i in seq_along(t)) {
    if (grepl("^\\s*```", t[i])) { w_kodzie <- !w_kodzie; t[i] <- ""; next }
    t[i] <- if (w_kodzie) "" else gsub("`r [^`]*`", "X", t[i])
  }
  oczyszczony <- tempfile(fileext = ".md")
  writeLines(enc2utf8(t), oczyszczony, useBytes = TRUE)
  drzewo <- pandoc_json(c("-f", "markdown", "-t", "json", shQuote(oczyszczony)))
  id <- character()
  przejdz <- function(x) {
    if (!is.list(x)) return(invisible())
    if (identical(x$t, "Cite")) for (c in x$c[[1]]) id <<- c(id, c$citationId)
    for (el in x) przejdz(el)
  }
  przejdz(drzewo$blocks)
  unique(id)
}
wpisy <- wpisy_bibliografii()
klucze <- vapply(wpisy, function(w) w$id, "")
lektury <- yaml::read_yaml("ksiazka/lektury.yml")
pliki_rmd <- c(file.path("ksiazka", yaml::read_yaml("ksiazka/_bookdown.yml")$rmd_files), "materialy/projekt.Rmd")
cytowane <- unique(unlist(lapply(pliki_rmd, klucze_cytowan)))
bledy <- c(sprintf("klucz @%s bez wpisu w ksiazka/bibliografia.yaml", setdiff(cytowane, klucze)),
           sprintf("wpis %s bez pozycji w ksiazka/lektury.yml", setdiff(klucze, names(lektury))),
           sprintf("pozycja %s w ksiazka/lektury.yml bez wpisu w bibliografii", setdiff(names(lektury), klucze)),
           sprintf("pozycja %s w ksiazka/lektury.yml bez adresu dostępu", names(lektury)[
             vapply(lektury, function(l) !isTRUE(nzchar(l$dostep$adres)) || identical(l$dostep$adres, "do uzupełnienia"), TRUE)]))
if (length(bledy)) stop("Bibliografia kursu:\n", paste(bledy, collapse = "\n"), call. = FALSE)
message(sprintf("Bibliografia: %d wpisów, %d cytowanych kluczy.", length(klucze), length(cytowane)))

if ("--linki" %in% argumenty) {
  adresy <- unique(c(paste0("https://doi.org/", unlist(lapply(wpisy, function(w) w$DOI))),
                     vapply(lektury, function(l) l$dostep$adres, "")))
  adresy <- adresy[grepl("^https?://", adresy)]
  stan <- vapply(adresy, function(a) {
    h <- curl::new_handle(nobody = TRUE, followlocation = TRUE, timeout = 30)
    w <- tryCatch(curl::curl_fetch_memory(a, handle = h)$status_code, error = function(e) NA_integer_)
    as.integer(w)
  }, 1L)
  problem <- adresy[is.na(stan) | stan >= 400]
  message(sprintf("Adresy: %d sprawdzonych, %d bez odpowiedzi 2xx/3xx.", length(adresy), length(problem)))
  if (length(problem)) message(paste(" -", problem, stan[problem], collapse = "\n"))
}

# 1. Książka. Katalog _book jest wyłącznie wynikiem budowania, więc powstaje od nowa.
if (dir.exists("_book")) unlink("_book", recursive = TRUE)
stary <- setwd("ksiazka")
bookdown::render_book("index.Rmd", quiet = TRUE)
setwd(stary)

# Cytowania: pełny opis źródła w dymku po kliknięciu (mechanizm przypisów bs4_book)
# i polski nagłówek listy źródeł na końcu rozdziału.
for (f in list.files("_book", pattern = "\\.html$", full.names = TRUE)) {
  h <- xml2::read_html(f, encoding = "UTF-8")
  # DOI ze znakiem „#” (np. Kuhlthau, 1991): w adresie „#” rozpoczyna kotwicę, więc link dostaje kod %23.
  doi_linki <- xml2::xml_find_all(h, ".//a[starts-with(@href, 'https://doi.org/') and contains(@href, '#')]")
  for (a in doi_linki) xml2::xml_attr(a, "href") <- gsub("#", "%23", xml2::xml_attr(a, "href"), fixed = TRUE)
  opisy <- xml2::xml_find_all(h, ".//div[contains(concat(' ', normalize-space(@class), ' '), ' csl-entry ')]")
  if (!length(opisy) && !length(doi_linki)) next
  tresc <- vapply(opisy, function(w) paste(vapply(xml2::xml_contents(w), as.character, ""), collapse = ""), "")
  names(tresc) <- xml2::xml_attr(opisy, "id")
  for (a in xml2::xml_find_all(h, ".//span[contains(@class, 'citation')]//a[starts-with(@href, '#ref-')]")) {
    id <- sub("^#", "", xml2::xml_attr(a, "href"))
    if (is.na(tresc[id])) next
    xml2::xml_attr(a, "href") <- NULL
    xml2::xml_attr(a, "tabindex") <- "0"
    xml2::xml_attr(a, "role") <- "button"
    xml2::xml_attr(a, "data-toggle") <- "popover"
    xml2::xml_attr(a, "data-content") <- tresc[[id]]
  }
  for (n in xml2::xml_find_all(h, ".//h3[normalize-space(.)='References']")) {
    xml2::xml_name(n) <- "h2"
    xml2::xml_text(n) <- "Źródła rozdziału"
    xml2::xml_attr(n, "id") <- "zrodla"
  }
  xml2::write_html(h, f, format = FALSE)
}

# Szablon bs4_book ma angielskie napisy interfejsu; zastępujemy je polskimi.
tlumaczenia <- c(
  'placeholder="Search" aria-label="Search"' = 'placeholder="Szukaj" aria-label="Szukaj"',
  'aria-label="Table of contents"><h2>Table of contents</h2>' = 'aria-label="Spis treści"><h2>Spis treści</h2>',
  'aria-label="On this page"><h2>On this page</h2>' = 'aria-label="Na tej stronie"><h2>Na tej stronie</h2>',
  '>View book source <' = '>Źródła książki <',
  '>View source <' = '>Źródło rozdziału <',
  '" was written by ' = '", autor: ',
  '. It was last built on ' = '. Wersja z ',
  'This book was built by the ' = 'Książka powstała w pakiecie R ',
  '</a> R package.' = '</a>.')
for (f in list.files("_book", pattern = "\\.html$", full.names = TRUE)) {
  h <- readLines(f, encoding = "UTF-8", warn = FALSE)
  for (k in names(tlumaczenia)) h <- gsub(k, tlumaczenia[[k]], h, fixed = TRUE)
  h <- gsub('<li><a id="book-edit"[^<]*<i class="fab fa-github"></i></a></li>', "", h)
  polaczenie <- file(f, open = "wb")
  writeLines(enc2utf8(h), polaczenie, useBytes = TRUE)
  close(polaczenie)
}

# 2. Materiały pobierane przez skrypt startowy i studentów, w tym bibliografia i styl cytowania.
dir.create("_book/materialy", recursive = TRUE, showWarnings = FALSE)
pliki <- list.files("materialy", recursive = TRUE)
stopifnot(all(file.copy(file.path("materialy", pliki), file.path("_book/materialy", pliki), overwrite = TRUE)))
stopifnot(system2(pandoc, c(shQuote("ksiazka/bibliografia.yaml"), "-s", "-f", "markdown", "-t", "csljson",
                            "-o", shQuote("_book/materialy/bibliografia.json"))) == 0,
          system2(pandoc, c(shQuote("ksiazka/bibliografia.yaml"), "-s", "-f", "markdown", "-t", "biblatex",
                            "-o", shQuote("_book/materialy/bibliografia.bib"))) == 0,
          file.copy("ksiazka/apa-pl.csl", "_book/materialy/apa-pl.csl", overwrite = TRUE))
file.create("_book/.nojekyll")
message("Książka gotowa w katalogu _book/.")

# 3. Pliki zadań: kopia z lokalnym skryptem startowym, lokalną bibliografią, testowym autorem i ID.
przygotuj_kopie <- function(plik, katalog) {
  tekst <- readLines(plik, encoding = "UTF-8", warn = FALSE)
  zrodlo <- grep(adres_kursu, tekst, fixed = TRUE)
  stopifnot(length(zrodlo) == 1L)
  tekst[zrodlo] <- sprintf('options(zi.adres = "%s/materialy/"); source("%s/materialy/start.R", encoding = "UTF-8")',
                           korzen, korzen)
  tekst <- sub(paste0("^(bibliography|csl): ", adres_materialow), paste0("\\1: ", korzen, "/_book/materialy/"), tekst)
  tekst <- sub('^author: "Imię Nazwisko"$', 'author: "Anna Testowa"', tekst)
  tekst <- sub('^  id: "TWOJE_ID"$', '  id: "s017"', tekst)
  odpowiedz <- "**Odpowiedź:** Odpowiedź testowa z liczbami *M* = 1,00 i *p* = ,050."
  # W szablonie projektu odpowiedź testowa cytuje źródło, aby test objął bibliografię i styl cytowania.
  if (basename(plik) == "projekt.Rmd") odpowiedz <- paste(odpowiedz, "Test Welcha [@welch1947].")
  tekst <- sub("^\\*\\*Odpowiedź:\\*\\*$", odpowiedz, tekst)
  cel <- file.path(katalog, basename(plik))
  polaczenie <- file(cel, open = "wb")
  writeLines(enc2utf8(tekst), polaczenie, useBytes = TRUE)
  close(polaczenie)
  cel
}

uruchom <- function(kod, katalog) {
  stary <- setwd(katalog)
  on.exit(setwd(stary))
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(kod)),
                                  stdout = TRUE, stderr = TRUE))
  list(ok = is.null(attr(log, "status")), log = log)
}

if (!"--bez-testow" %in% argumenty) {
  formaty <- c("html_document", if ("--pdf" %in% argumenty) "pdf_document")
  szablony <- list.files("materialy", pattern = "^(zadanie-[0-9]{2}|projekt)\\.Rmd$", full.names = TRUE)
  wyniki <- data.frame(plik = character(), test = character(), wynik = character())
  for (p in szablony) {
    katalog <- tempfile("test-zadania-")
    dir.create(katalog)
    kopia <- przygotuj_kopie(p, katalog)
    for (f in formaty) {
      kod <- sprintf("rmarkdown::render('%s', output_format = '%s', quiet = TRUE)", basename(kopia), f)
      w <- uruchom(kod, katalog)
      roz <- if (f == "pdf_document") "pdf" else "html"
      wynik <- file.path(katalog, sub("\\.Rmd$", paste0(".", roz), basename(kopia)))
      ok <- w$ok && file.exists(wynik)
      # Projekt: cytowanie i pozycja bibliografii w stylu kursu („Welch, B. L. (1947)”).
      if (ok && roz == "html" && basename(p) == "projekt.Rmd")
        ok <- any(grepl("Welch, B. L. (1947)", readLines(wynik, encoding = "UTF-8", warn = FALSE), fixed = TRUE))
      if (!ok) message(paste(utils::tail(w$log, 25), collapse = "\n"))
      wyniki[nrow(wyniki) + 1L, ] <- c(basename(p), roz, if (ok) "OK" else "BŁĄD")
    }
    if ("--oddaj" %in% argumenty) {
      nr <- if (grepl("projekt", p)) "9" else sub("^zadanie-0?([0-9]+)\\.Rmd$", "\\1", basename(p))
      kod <- sprintf(paste0('options(zi.adres = "%s/materialy/"); source("%s/materialy/start.R", encoding = "UTF-8"); ',
                            'oddaj_zadanie(%s, plik = "%s", wyslij = FALSE)'), korzen, korzen, nr, basename(kopia))
      w <- uruchom(kod, katalog)
      pdf <- list.files(file.path(katalog, "wyniki"), pattern = "\\.(pdf|html)$")
      if (!w$ok) message(paste(utils::tail(w$log, 25), collapse = "\n"))
      wyniki[nrow(wyniki) + 1L, ] <- c(basename(p), "oddanie", if (w$ok && length(pdf)) paste("OK:", pdf[1]) else "BŁĄD")
    }
  }
  print(wyniki, row.names = FALSE)
  if (any(grepl("BŁĄD", wyniki$wynik))) stop("Część plików zadań nie przeszła testu.")
}
