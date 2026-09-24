\name{wykres_theta}
\alias{wykres_theta}
\title{Siatka par do miary theta}
\usage{
wykres_theta(x, y, nazwy = c("Grupa 1", "Grupa 2"), tytul = NULL)
}
\arguments{
\item{x}{Wartosci grupy pierwszej.}

\item{y}{Wartosci grupy drugiej.}

\item{nazwy}{Nazwy obu grup.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazda komorka to para (osoba z grupy pierwszej, osoba z grupy drugiej);
napis w komorce podaje wynik porownania: ">" gdy wartosc z grupy pierwszej
jest wieksza, "=" przy remisie, "<" gdy mniejsza. Podtytul podaje liczby
par i theta = (liczba ">" + polowa remisow) / liczba par.
}
\examples{
p <- wykres_theta(c(8, 10), c(6, 10))
}
