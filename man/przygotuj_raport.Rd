\name{przygotuj_raport}
\alias{przygotuj_raport}
\title{Przygotowanie raportu z zapisanych odpowiedzi Z09 i Z10}
\usage{
przygotuj_raport(katalog = NULL)
}
\arguments{
\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}
}
\value{
Bezwzględna ścieżka raport.Rmd, niewidocznie.
}
\description{
Nowy raport.Rmd otrzymuje dziesięć własnych odpowiedzi według mapy kursu,
gotowy analiza.R i kopię tego samego wariantu danych. P11 pozostaje miejscem
na bibliografię i opis faktycznie wykorzystanego wsparcia. Nie trzeba
przepisywać akapitów; nieuzupełnione odpowiedzi pozostają pustymi polami.
Ponowne wywołanie zwraca istniejący raport bez kopiowania tekstu na nowo.
}
\examples{
k <- tempfile("raport-")
utworz_projekt("s017", katalog = k)
przygotuj_zadanie("Z09", k)
wariant_projektu("Z09", "S02", katalog = k)
przygotuj_zadanie("Z10", k)
przygotuj_raport(k)
unlink(k, recursive = TRUE)
}
