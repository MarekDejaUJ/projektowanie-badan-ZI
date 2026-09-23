\name{wykres_rowne_srednie}
\alias{wykres_rowne_srednie}
\title{Dwa rozklady o tej samej sredniej i roznym rozrzucie}
\usage{
wykres_rowne_srednie(srednia, sd, etykiety, os = "Wartość", tytul = NULL)
}
\arguments{
\item{srednia}{Wspolna srednia.}

\item{sd}{Wektor odchylen standardowych.}

\item{etykiety}{Nazwy krzywych.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywe gestosci rozkladu normalnego o wspolnej sredniej i roznych
odchyleniach standardowych; linia pionowa oznacza srednia.
}
\examples{
p <- wykres_rowne_srednie(8, c(1, 4), c("SD = 1", "SD = 4"), os = "Czas [min]")
}
