\name{wczytaj_dane}
\alias{wczytaj_dane}
\title{Odczyt zachowanego wariantu bez ponownego losowania}
\usage{
wczytaj_dane(katalog, id = NULL, scenariusz = NULL, rocznik = NULL)
}
\arguments{
\item{katalog}{Katalog utworzony przez zapisz_dane().}

\item{id}{Oczekiwane ID lub NULL, jeśli nie podano.}

\item{scenariusz}{Oczekiwany kod S01--S20 lub NULL.}

\item{rocznik}{Oczekiwany rocznik lub NULL.}
}
\value{
Obiekt badaniaZI_dane z zapisanymi danymi, słownikiem, kartą i manifestem.
}
\description{
Kontroluje kompletność, hashe plików i zgodność wartości CSV/RDS.
Hashe służą wykrywaniu zmian, nie są podpisem autora ani oceną pracy.
Nie naprawia i nie regeneruje brakujących lub zmienionych danych.
}
\examples{
k <- tempfile("wariant-")
zapisz_dane(generuj_dane("s017", "S02"), k)
x <- wczytaj_dane(k, id = "s017")
unlink(k, recursive = TRUE)
}
