\name{wykres_chi2}
\alias{wykres_chi2}
\title{Gestosci rozkladow chi-kwadrat}
\usage{
wykres_chi2(df, kwantyl = 0.95, gora = 12, tytul = NULL)
}
\arguments{
\item{df}{Stopnie swobody.}

\item{kwantyl}{Rzad kwantyla podanego w legendzie.}

\item{gora}{Gorna granica osi.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywe gestosci rozkladu chi-kwadrat o podanych stopniach swobody,
rozroznione typem linii; legenda podaje wartosc krytyczna rzedu `kwantyl`.
}
\examples{
p <- wykres_chi2(c(1, 2, 4))
}
