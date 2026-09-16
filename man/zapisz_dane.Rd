\name{zapisz_dane}
\alias{zapisz_dane}
\title{Zapis danych i manifestu}
\usage{
zapisz_dane(dane, katalog)
}
\arguments{
\item{dane}{Wynik generuj_dane().}

\item{katalog}{Nowy katalog docelowy; istniejący nie jest nadpisywany.}
}
\value{
Ścieżka katalogu, niewidocznie.
}
\description{
Zapis danych i manifestu
}
\examples{
katalog <- tempfile("ankieta-")
zapisz_dane(generuj_dane("s017"), katalog)
unlink(katalog, recursive = TRUE)
}
