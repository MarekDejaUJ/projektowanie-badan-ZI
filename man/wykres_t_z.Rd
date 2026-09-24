\name{wykres_t_z}
\alias{wykres_t_z}
\title{Rozklad normalny standardowy i rozklady t}
\usage{
wykres_t_z(df, kwantyl = 0.975, os = "Wartość statystyki", tytul = NULL)
}
\arguments{
\item{df}{Stopnie swobody rozkladow t.}

\item{kwantyl}{Rzad kwantyla podanego w legendzie.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywe gestosci N(0, 1) i rozkladow t o podanych stopniach swobody,
rozroznione typem linii; legenda podaje kwantyl rzedu `kwantyl` kazdego
rozkladu, a krotkie kreski na osi oznaczaja jego polozenie.
}
\examples{
p <- wykres_t_z(c(3, 15))
}
