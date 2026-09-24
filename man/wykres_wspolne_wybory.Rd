\name{wykres_wspolne_wybory}
\alias{wykres_wspolne_wybory}
\title{Granice liczby osob z dwoma wyborami}
\usage{
wykres_wspolne_wybory(n_a, n_b, N, nazwy = c("A", "B"), tytul = NULL)
}
\arguments{
\item{n_a, n_b}{Liczby osob wybierajacych opcje A i B.}

\item{N}{Liczba osob z kompletna odpowiedzia.}

\item{nazwy}{Nazwy opcji A i B.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Przy N osobach, n_a wyborach opcji A i n_b wyborach opcji B liczba osob
z oboma wyborami lezy miedzy max(0, n_a + n_b - N) a min(n_a, n_b). Dwie
belki przedstawiaja uklad z najmniejsza i z najwieksza czescia wspolna.
}
\examples{
p <- wykres_wspolne_wybory(30, 25, 50, c("e-mail", "WWW"))
}
