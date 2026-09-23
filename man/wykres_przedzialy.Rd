\name{wykres_przedzialy}
\alias{wykres_przedzialy}
\title{Estymaty z przedzialami na wspolnej osi}
\usage{
wykres_przedzialy(
  etykieta,
  estymata,
  dolna,
  gorna,
  odniesienie = NULL,
  os = "Wartość",
  cyfry = 1L,
  tytul = NULL
)
}
\arguments{
\item{etykieta}{Nazwy wierszy (np. wariantow lub prob).}

\item{estymata}{Wartosci punktowe.}

\item{dolna, gorna}{Granice przedzialow.}

\item{odniesienie}{Opcjonalna wartosc odniesienia, np. 0.}

\item{os}{Podpis osi.}

\item{cyfry}{Liczba miejsc po przecinku w etykietach granic.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot.
}
\description{
Poziomy odcinek przedstawia przedzial, romb estymate, a liczby przy koncach
odcinka podaja granice. Linia przerywana oznacza wartosc odniesienia.
}
\examples{
wykres_przedzialy(c("Precyzyjny", "Szeroki"), c(2, 2), c(1.7, -3), c(2.3, 7),
  odniesienie = 0, os = "Roznica [min]")
}
