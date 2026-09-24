\name{wykres_ogony}
\alias{wykres_ogony}
\title{Rozklad odniesienia z polem ogonow}
\usage{
wykres_ogony(
  statystyka,
  rozklad = c("t", "normalny", "chi2", "F"),
  df = NULL,
  os = "Wartość statystyki",
  tytul = NULL
)
}
\arguments{
\item{statystyka}{Obserwowana wartosc statystyki.}

\item{rozklad}{"t", "normalny", "chi2" albo "F".}

\item{df}{Stopnie swobody (dla F wektor dwoch liczb).}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywa rozkladu statystyki przy hipotezie zerowej z pionowymi liniami przy
obserwowanej wartosci. Dla rozkladow t i normalnego pole obu ogonow
(|T| >= |statystyka|), dla chi-kwadrat i F pole prawego ogona; podtytul
podaje to pole, czyli wartosc p.
}
\examples{
p <- wykres_ogony(3, "t", 99)
}
