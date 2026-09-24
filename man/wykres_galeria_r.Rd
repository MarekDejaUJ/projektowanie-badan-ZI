\name{wykres_galeria_r}
\alias{wykres_galeria_r}
\title{Galeria korelacji o zadanych wartosciach r}
\usage{
wykres_galeria_r(r, n = 100L, tytul = NULL)
}
\arguments{
\item{r}{Wartosci wspolczynnika korelacji.}

\item{n}{Liczba par w panelu.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Panel dla kazdego r: n syntetycznych par o korelacji dokladnie r
(konstrukcja deterministyczna: kwantyle rozkladu normalnego i stala
permutacja reszt), z prosta regresji.
}
\examples{
p <- wykres_galeria_r(c(0, -0.5))
}
