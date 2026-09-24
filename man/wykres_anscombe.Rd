\name{wykres_anscombe}
\alias{wykres_anscombe}
\title{Kwartet Anscombe'a}
\usage{
wykres_anscombe(tytul = NULL)
}
\arguments{
\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Cztery zbiory danych `datasets::anscombe` z prosta regresji; zbiory maja
prawie identyczne srednie, wariancje, korelacje (0,82) i proste.
}
\examples{
p <- wykres_anscombe()
}
