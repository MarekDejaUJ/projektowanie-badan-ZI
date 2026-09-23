\name{wykres_punkty_os}
\alias{wykres_punkty_os}
\title{Wszystkie pomiary na jednej osi z kwartylami}
\usage{
wykres_punkty_os(x, os = "Wartość", tytul = NULL)
}
\arguments{
\item{x}{Wartosci liczbowe; braki sa pomijane.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazdy punkt to jedna obserwacja rozsunieta w pionie; pionowe linie oznaczaja
pierwszy kwartyl, mediane i trzeci kwartyl (kwantyl typu 7).
}
\examples{
p <- wykres_punkty_os(c(2, 4, 5, 7, 8, 13, 30), os = "Czas [min]")
}
