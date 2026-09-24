\name{wykres_pasma_regresji}
\alias{wykres_pasma_regresji}
\title{Rozrzut z prosta regresji, pasmem sredniej i pasmem predykcji}
\usage{
wykres_pasma_regresji(
  x,
  y,
  x0 = NULL,
  os_x = "x",
  os_y = "y",
  etykiety = NULL,
  predykcja = TRUE,
  tytul = NULL
)
}
\arguments{
\item{x, y}{Wartosci par.}

\item{x0}{Opcjonalna wartosc x do odczytu przedzialow.}

\item{os_x, os_y}{Podpisy osi.}

\item{etykiety}{Opcjonalne etykiety punktow.}

\item{predykcja}{Czy rysowac pasmo predykcji.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Punkty to pary (x, y); linia ciagla to prosta metody najmniejszych
kwadratow; ciemniejsze pasmo to 95% przedzial sredniej warunkowej, jasniejsze
i szersze pasmo to 95% przedzial predykcji dla nowej osoby. Opcjonalna
pionowa linia przy x0 z napisem podaje oba przedzialy.
}
\examples{
p <- wykres_pasma_regresji(c(1, 2, 2, 3, 4, 5), c(15, 12, 14, 10, 9, 6), x0 = 3)
}
