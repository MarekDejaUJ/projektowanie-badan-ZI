praca_pdf_test <- function() {
  k <- tempfile("pdf żółty ")
  utworz_projekt("s017", katalog = k, student = "Anna Kowalska")
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

# Atrapa serwera Nextcloud: zapisuje wywołania i zwraca podany kod HTTP.
atrapa_nc <- function(kod = 201L, tresc = "") {
  e <- new.env()
  e$wywolania <- list()
  e$put <- function(plik, url, naglowki) {
    e$wywolania[[length(e$wywolania) + 1L]] <- list(url = url, naglowki = naglowki,
      bajty = readBin(plik, "raw", file.info(plik)$size))
    list(status_code = kod, content = charToRaw(tresc))
  }
  e
}
