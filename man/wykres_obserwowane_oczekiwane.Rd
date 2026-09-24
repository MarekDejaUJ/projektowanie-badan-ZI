\name{wykres_obserwowane_oczekiwane}
\alias{wykres_obserwowane_oczekiwane}
\title{Liczebnosci obserwowane i oczekiwane w tabeli}
\usage{
wykres_obserwowane_oczekiwane(tabela, tytul = NULL)
}
\arguments{
\item{tabela}{Macierz liczebnosci z nazwami wierszy i kolumn.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Dla kazdej komorki tabeli dwa slupki: liczebnosc obserwowana O (pelny) i
oczekiwana przy niezaleznosci E (jasny); napisy podaja obie liczby.
}
\examples{
p <- wykres_obserwowane_oczekiwane(matrix(c(18, 12, 32, 8), 2, byrow = TRUE,
       dimnames = list(c("A", "B"), c("Tak", "Nie"))))
}
