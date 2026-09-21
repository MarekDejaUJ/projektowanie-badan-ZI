\name{plik_pracy}
\alias{plik_pracy}
\title{Ścieżka do roboczego Rmd, jego skryptu lub danych}
\usage{
plik_pracy(id, plik = NULL, katalog = NULL)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{plik}{Ścieżka wewnątrz folderu dokumentu; NULL wskazuje jego Rmd.}

\item{katalog}{Własna przestrzeń pracy; NULL rozpoznaje ją automatycznie.}
}
\value{
Bezwzględna ścieżka do istniejącego pliku lub katalogu.
}
\description{
W konsoli używa przestrzeni zapamiętanej przez rozpocznij_zajecia(),
bez zmiany katalogu roboczego. Podczas renderowania używa katalogu
renderowanego Rmd, także gdy odtworzono sam folder dokumentu.
Funkcja tylko wskazuje plik: nie tworzy, nie pobiera i nie wykonuje go.
}
\examples{
k <- tempfile("praca-")
utworz_projekt("s017", katalog = k)
przygotuj_zadanie("Z01", k)
plik_pracy("Z01", "analiza.R", katalog = k)
unlink(k, recursive = TRUE)
}
