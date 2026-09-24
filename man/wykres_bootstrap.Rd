\name{wykres_bootstrap}
\alias{wykres_bootstrap}
\title{Rozklad bootstrapowy z przedzialem percentylowym}
\usage{
wykres_bootstrap(
  wartosci,
  szerokosc,
  poziom = 0.95,
  os = "Wartość statystyki",
  tytul = NULL
)
}
\arguments{
\item{wartosci}{Statystyki z prob bootstrapowych.}

\item{szerokosc}{Szerokosc przedzialu histogramu.}

\item{poziom}{Poziom przedzialu.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Histogram B statystyk bootstrapowych; linie przerywane oznaczaja kwantyle
(1 - poziom)/2 i (1 + poziom)/2, czyli granice przedzialu percentylowego.
}
\examples{
p <- wykres_bootstrap(stats::qnorm(stats::ppoints(200), 10, 1), 0.25)
}
