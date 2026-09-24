\name{wykres_kwantylowy}
\alias{wykres_kwantylowy}
\title{Wykres kwantylowy wzgledem rozkladu normalnego}
\usage{
wykres_kwantylowy(dane, os = "Uporządkowane wartości", tytul = NULL)
}
\arguments{
\item{dane}{Nazwana lista wektorow liczbowych.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Panel dla kazdego elementu listy: na osi poziomej kwantyle rozkladu
normalnego standardowego, na osi pionowej uporzadkowane wartosci. Linia
przerywana przechodzi przez pierwszy i trzeci kwartyl; punkty wzdluz linii
oznaczaja zgodnosc z rozkladem normalnym, a punkty powyzej linii na prawym
koncu oznaczaja prawy ogon.
}
\examples{
p <- wykres_kwantylowy(list(A = c(1, 2, 3, 4, 10)))
}
