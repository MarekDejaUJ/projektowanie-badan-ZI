# Buduje książkę kursu w katalogu _book/ i sprawdza pliki zadań w nowych sesjach R.
# Uruchom z katalogu repozytorium:
#   Rscript build.R              książka i test plików zadań (HTML)
#   Rscript build.R --pdf        dodatkowo PDF każdego pliku zadania (wymaga XeLaTeX)
#   Rscript build.R --oddaj      dodatkowo oddaj_zadanie(..., wyslij = FALSE) dla każdego pliku
#   Rscript build.R --bez-testow tylko książka

argumenty <- commandArgs(trailingOnly = TRUE)
if (!file.exists("ksiazka/_bookdown.yml")) stop("Uruchom Rscript build.R z katalogu repozytorium.")
korzen <- normalizePath(".", winslash = "/")
adres_kursu <- "https://marekdejauj.github.io/projektowanie-badan-ZI/materialy/start.R"

# 1. Książka. Katalog _book jest wyłącznie wynikiem budowania, więc powstaje od nowa.
if (dir.exists("_book")) unlink("_book", recursive = TRUE)
stary <- setwd("ksiazka")
bookdown::render_book("index.Rmd", quiet = TRUE)
setwd(stary)

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

# 2. Materiały pobierane przez skrypt startowy i studentów.
dir.create("_book/materialy/wyklady", recursive = TRUE, showWarnings = FALSE)
pliki <- list.files("materialy", recursive = TRUE)
stopifnot(all(file.copy(file.path("materialy", pliki), file.path("_book/materialy", pliki), overwrite = TRUE)))
file.create("_book/.nojekyll")
message("Książka gotowa w katalogu _book/.")

# 3. Pliki zadań: kopia z lokalnym skryptem startowym, testowym autorem i ID.
przygotuj_kopie <- function(plik, katalog) {
  tekst <- readLines(plik, encoding = "UTF-8", warn = FALSE)
  zrodlo <- grep(adres_kursu, tekst, fixed = TRUE)
  stopifnot(length(zrodlo) == 1L)
  tekst[zrodlo] <- sprintf('options(zi.adres = "%s/materialy/"); source("%s/materialy/start.R", encoding = "UTF-8")',
                           korzen, korzen)
  tekst <- sub('^author: "Imię Nazwisko"$', 'author: "Anna Testowa"', tekst)
  tekst <- sub('^  id: "TWOJE_ID"$', '  id: "s017"', tekst)
  tekst <- sub("^\\*\\*Odpowiedź:\\*\\*$", "**Odpowiedź:** Odpowiedź testowa z liczbami *M* = 1,00 i *p* = ,050.", tekst)
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
    kopia <- prz <- przygotuj_kopie(p, katalog)
    for (f in formaty) {
      kod <- sprintf("rmarkdown::render('%s', output_format = '%s', quiet = TRUE)", basename(kopia), f)
      w <- uruchom(kod, katalog)
      roz <- if (f == "pdf_document") "pdf" else "html"
      ok <- w$ok && file.exists(file.path(katalog, sub("\\.Rmd$", paste0(".", roz), basename(kopia))))
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
