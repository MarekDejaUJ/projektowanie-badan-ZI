\name{wykres_mozaika}
\alias{wykres_mozaika}
\title{Wykres mozaikowy tabeli liczebnosci}
\usage{
wykres_mozaika(tabela, nazwy_kolumn = colnames(tabela), tytul = NULL)
}
\arguments{
\item{tabela}{Macierz liczebnosci z nazwami wierszy i kolumn.}

\item{nazwy_kolumn}{Opisy kolumn w legendzie (domyslnie nazwy kolumn).}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazdy wiersz tabeli jest slupkiem o szerokosci proporcjonalnej do jego
liczebnosci i wysokosci 100%; odcinki odpowiadaja kolumnom tabeli. Napisy
podaja liczebnosc i odsetek w wierszu, a podpis osi - N wiersza.
}
\examples{
p <- wykres_mozaika(matrix(c(10, 30, 32, 48), 2, byrow = TRUE,
       dimnames = list(c("A", "B"), c("0", "1"))))
}
