\name{wykres_reszty}
\alias{wykres_reszty}
\title{Reszty wobec wartosci przewidywanych}
\usage{
wykres_reszty(dane, tytul = NULL)
}
\arguments{
\item{dane}{Nazwana lista ramek z kolumnami x i y.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Panel dla kazdego elementu listy (ramka z kolumnami x i y): punkty to
reszty prostej regresji wobec wartosci przewidywanych, linia przerywana to
zero. Luk wskazuje niedopasowana srednia, lejek - rozrzut zalezny od
poziomu przewidywania.
}
\examples{
p <- wykres_reszty(list(A = data.frame(x = 1:6, y = c(15, 12, 14, 10, 9, 6))))
}
