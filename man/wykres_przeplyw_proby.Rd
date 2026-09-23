\name{wykres_przeplyw_proby}
\alias{wykres_przeplyw_proby}
\title{Przeplyw proby od zaproszenia do analizy}
\usage{
wykres_przeplyw_proby(etapy, n, os = etykiety_zi$liczba_osob, tytul = NULL)
}
\arguments{
\item{etapy}{Nazwy etapow w kolejnosci.}

\item{n}{Liczby jednostek na etapach.}

\item{os}{Podpis osi liczebnosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Poziome slupki podaja liczbe jednostek na kolejnych etapach badania oraz
odsetek wzgledem pierwszego etapu.
}
\examples{
p <- wykres_przeplyw_proby(c("Zaproszeni", "Rozpoczeli", "Ukonczyli"), c(300, 180, 150))
}
