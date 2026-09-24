\name{wykres_anova}
\alias{wykres_anova}
\title{Grupy analizy wariancji: punkty, srednie grup i srednia ogolna}
\usage{
wykres_anova(x, grupa, os = "Wartość", tytul = NULL)
}
\arguments{
\item{x}{Wartosci liczbowe.}

\item{grupa}{Etykiety grup.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kola oznaczaja obserwacje, romby srednie grup, linia przerywana srednia
ogolna, a pionowe odcinki odleglosc sredniej grupy od sredniej ogolnej
(skladnik sumy kwadratow miedzy grupami).
}
\examples{
p <- wykres_anova(c(4, 5, 6, 7, 8, 9), rep(c("A", "B"), each = 3))
}
