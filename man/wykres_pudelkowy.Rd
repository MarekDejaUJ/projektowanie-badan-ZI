\name{wykres_pudelkowy}
\alias{wykres_pudelkowy}
\title{Wykres pudelkowy z punktami i granica wasa}
\usage{
wykres_pudelkowy(x, grupa = NULL, os = "Wartość", tytul = NULL)
}
\arguments{
\item{x}{Wartosci liczbowe; braki sa pomijane.}

\item{grupa}{Opcjonalne etykiety grup.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot.
}
\description{
Pudelko obejmuje kwartyle, kreska w srodku oznacza mediane, a punkty
pokazuja wszystkie obserwacje. Linia kropkowana oznacza granice
Q3 + 1,5 IQR, powyzej ktorej punkty leza poza wasem.
}
\examples{
wykres_pudelkowy(c(4.5, 6.5, 8, 35), os = "Czas [min]")
}
