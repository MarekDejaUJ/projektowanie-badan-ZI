\name{wykres_moc}
\alias{wykres_moc}
\title{Moc testu t jednej proby wobec liczebnosci}
\usage{
wykres_moc(d, n, alfa = 0.05, cel = 0.8, tytul = NULL)
}
\arguments{
\item{d}{Standaryzowane efekty.}

\item{n}{Siatka liczebnosci.}

\item{alfa}{Poziom istotnosci testu dwustronnego.}

\item{cel}{Moc docelowa.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywa dla kazdego standaryzowanego efektu d (typ linii odroznia efekty);
linia kropkowana oznacza moc docelowa, a legenda podaje liczebnosc, przy
ktorej krzywa ja osiaga. Obliczenie: stats::power.t.test.
}
\examples{
p <- wykres_moc(c(0.3, 0.5), seq(10, 100, 10))
}
