\name{wykres_macierz_korelacji}
\alias{wykres_macierz_korelacji}
\title{Macierz korelacji pozycji}
\usage{
wykres_macierz_korelacji(
  dane,
  etykiety = names(dane),
  metoda = c("pearson", "spearman"),
  tytul = NULL
)
}
\arguments{
\item{dane}{Ramka danych liczbowych: jedna kolumna na pozycje.}

\item{etykiety}{Nazwy pozycji na osiach.}

\item{metoda}{Metoda funkcji cor(): "pearson" albo "spearman".}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Dolny trojkat macierzy korelacji liczonej na osobach z kompletem
odpowiedzi; komorki podaja wspolczynnik z dwoma miejscami po przecinku,
a podtytul N, srednia korelacje i jej zakres.
}
\examples{
p <- wykres_macierz_korelacji(data.frame(a = c(1, 2, 3, 4, 5), b = c(2, 1, 4, 3, 5),
                                         c = c(1, 3, 2, 5, 4)))
}
