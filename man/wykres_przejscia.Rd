\name{wykres_przejscia}
\alias{wykres_przejscia}
\title{Przejscia miedzy kategoriami w dwoch pomiarach tych samych osob}
\usage{
wykres_przejscia(tabela, nazwy = c("niepowodzenie", "sukces"), tytul = NULL)
}
\arguments{
\item{tabela}{Macierz 2 x 2 liczebnosci (wiersze: przed, kolumny: po).}

\item{nazwy}{Opisy kategorii 0 i 1.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kafelek tabeli 2 x 2 podaje liczbe osob z dana para wynikow: wiersze to
pomiar pierwszy, kolumny - drugi. Przekatna oznacza brak zmiany, komorki
poza przekatna - zmiane w jednym z dwoch kierunkow.
}
\examples{
p <- wykres_przejscia(matrix(c(20, 10, 2, 18), 2, byrow = TRUE))
}
