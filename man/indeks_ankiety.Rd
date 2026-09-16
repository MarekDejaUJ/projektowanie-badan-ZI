\name{indeks_ankiety}
\alias{indeks_ankiety}
\title{Średni indeks z pozycji ankiety}
\usage{
indeks_ankiety(pozycje, minimum = 5L)
}
\arguments{
\item{pozycje}{Ramka lub macierz numerycznych, już zgodnie ukierunkowanych pozycji.}

\item{minimum}{Minimalna liczba ważnych odpowiedzi.}
}
\value{
Wektor średnich; NA dla zbyt małej liczby odpowiedzi.
}
\description{
Średni indeks z pozycji ankiety
}
\examples{
indeks_ankiety(data.frame(p1 = c(1, NA), p2 = c(3, 5)), minimum = 2)
}
