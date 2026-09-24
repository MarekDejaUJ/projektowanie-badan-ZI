\name{wykres_wplyw}
\alias{wykres_wplyw}
\title{Wplyw jednego punktu na prosta regresji}
\usage{
wykres_wplyw(x, y, punkt, os_x = "x", os_y = "y", tytul = NULL)
}
\arguments{
\item{x, y}{Pierwotne pary.}

\item{punkt}{Wektor c(x, y) punktu dodanego.}

\item{os_x, os_y}{Podpisy osi.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kola to pierwotne pary, romb to punkt dodany; linia ciagla to prosta bez
punktu, linia przerywana to prosta z punktem. Podtytul podaje oba
nachylenia.
}
\examples{
p <- wykres_wplyw(c(1, 2, 2, 3, 4, 5), c(15, 12, 14, 10, 9, 6), c(9, 14))
}
