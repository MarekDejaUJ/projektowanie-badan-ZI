\name{wykres_chi2_mc}
\alias{wykres_chi2_mc}
\title{Ogon rozkladu zerowego Monte Carlo i rozkladu chi-kwadrat}
\usage{
wykres_chi2_mc(wartosci, statystyka, df = 1, tytul = NULL)
}
\arguments{
\item{wartosci}{Statystyki chi-kwadrat z losowanych tabel.}

\item{statystyka}{Obserwowana statystyka chi-kwadrat.}

\item{df}{Liczba stopni swobody rozkladu chi-kwadrat.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Dwie krzywe udzialu wartosci co najmniej x: schodkowa z losowanych tabel
(z poprawka (1 + liczba)/(B + 1)) i gladka z rozkladu chi-kwadrat o df
stopniach swobody. Linia przerywana i punkty oznaczaja obserwowana
statystyke; podtytul podaje oba p.
}
\examples{
p <- wykres_chi2_mc(stats::rchisq(199, 1), 2.5)
}
