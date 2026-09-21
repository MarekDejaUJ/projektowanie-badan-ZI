\name{wariant_projektu}
\alias{wariant_projektu}
\title{Zachowanie wariantu na C09 i odczyt tego samego wariantu na C10}
\usage{
wariant_projektu(
  zadanie,
  scenariusz = NULL,
  id_studenta = NULL,
  katalog = NULL
)
}
\arguments{
\item{zadanie}{Z09 albo Z10.}

\item{scenariusz}{Wybrany S01--S20 na C09; na C10 pozostaw NULL.}

\item{id_studenta}{ID z początku sesji; NULL odczytuje ID z kurs.yml.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}
}
\value{
Obiekt badaniaZI_dane; obliczenia i prezentację wykonuje analiza.R.
}
\description{
Pierwsze wywołanie dla Z09 zapisuje dane w jego katalogu. Następne
wywołania oraz Z10 wyłącznie czytają zachowany zapis. Render nie tworzy
wariantu: najpierw uruchom pierwszy chunk CHALLENGE na C09 i zapisz Rmd.
Inny scenariusz, uszkodzony zapis lub obce ID powodują błąd bez nadpisania.
}
\examples{
k <- tempfile("projekt-")
utworz_projekt("s017", katalog = k)
przygotuj_zadanie("Z09", k)
x <- wariant_projektu("Z09", "S02", katalog = k)
przygotuj_zadanie("Z10", k)
y <- wariant_projektu("Z10", katalog = k)
stopifnot(identical(x$dane, y$dane))
unlink(k, recursive = TRUE)
}
