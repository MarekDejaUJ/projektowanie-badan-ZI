# Przepływ studenta od zainstalowanego pakietu: przygotowanie PDF (TinyTeX, jak
# w nowym projekcie Posit Cloud), start ćwiczenia, odpowiedzi, PDF i oddanie.
# WYSYLKA_TESTOWA=true wysyła PDF do folderu Z01; w przeciwnym razie serwer
# zastępuje atrapa sprawdzająca adres, nazwę i bajty pliku.
library(badaniaZI)
system_nazwa <- tolower(Sys.info()[["sysname"]])
id <- paste0("test-", substr(gsub("[^a-z]", "", system_nazwa), 1, 7))
student <- paste("Test", tools::toTitleCase(system_nazwa))
przygotuj_pdf()
print(sprawdz_srodowisko())
k <- file.path(tempfile("przeplyw "), "moje-badania")
dir.create(dirname(k))
x <- rozpocznij_zajecia("C01", id_studenta = id, student = student, katalog = k, otworz = FALSE)
t <- readLines(x$plik, encoding = "UTF-8")
for (s in sprintf("S%02d", 1:5)) t <- badaniaZI:::wstaw_odpowiedz(t, s,
  paste0("Akapit testowy ", s, " z polskimi znakami: zażółć gęślą jaźń; średnia 8 min (N = 5)."))
badaniaZI:::pisz_linie(t, x$plik)
r <- sprawdz_zadanie("Z01", k)
print(r$kontrole)
stopifnot(r$ok)
pdf <- file.path(k, "zadania/z01/zadanie.pdf")
stopifnot(badaniaZI:::czy_pdf(pdf), file.size(pdf) > 50000)
oczekiwana <- paste0(badaniaZI:::nazwa_oddania(id, student, "Z01"), ".pdf")
if (!identical(tolower(Sys.getenv("WYSYLKA_TESTOWA")), "true")) {
  wyslane <- list()
  assignInNamespace("nc_put", function(plik, url, naglowki) {
    wyslane[[length(wyslane) + 1L]] <<- list(url = url, bajty = readBin(plik, "raw", file.info(plik)$size))
    list(status_code = 201L, content = raw())
  }, "badaniaZI")
}
o <- oddaj_zadanie("Z01", k, potwierdz = FALSE)
stopifnot(identical(o$stan, "wyslano"), identical(o$pliki, oczekiwana),
          identical(status_oddania("Z01", k)$sha256, digest::digest(file = pdf, algo = "sha256")))
if (exists("wyslane")) stopifnot(length(wyslane) == 1L, endsWith(wyslane[[1]]$url, oczekiwana),
  identical(wyslane[[1]]$bajty, readBin(pdf, "raw", file.info(pdf)$size)))
cat("Przepływ studenta (", R.version$platform, "): ", oczekiwana, " — OK\n", sep = "")
