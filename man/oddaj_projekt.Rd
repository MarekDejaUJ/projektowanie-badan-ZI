\name{oddaj_projekt}
\alias{oddaj_projekt}
\title{Oddanie indywidualnego projektu ilościowego}
\usage{
oddaj_projekt(katalog = NULL, potwierdz = interactive())
}
\arguments{
\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}

\item{potwierdz}{Czy pokazać PDF i zapytać przed wysłaniem.}
}
\value{
Lista jak w oddaj_zadanie(), niewidocznie.
}
\description{
Tak jak oddaj_zadanie() tworzy i sprawdza PDF raportu, a do folderu
projektu wysyła dwa pliki: PDF oraz archiwum ZIP ze źródłami (raport.Rmd,
analiza.R, dane wariantu, metadane i PDF), np. s017_kowalska_anna_PROJEKT.pdf
i s017_kowalska_anna_PROJEKT_zrodla.zip.
}
\examples{
\dontrun{ oddaj_projekt() }
}
