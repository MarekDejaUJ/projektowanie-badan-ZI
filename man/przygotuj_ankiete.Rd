\name{przygotuj_ankiete}
\alias{przygotuj_ankiete}
\title{Jawne przygotowanie danych z generatora}
\usage{
przygotuj_ankiete(dane)
}
\arguments{
\item{dane}{Surowa tabela z generuj_dane().}
}
\value{
Lista: dane i dziennik zastosowanych reguł.
}
\description{
Funkcja pokazuje referencyjne reguły czyszczenia: identyczne duplikaty,
kod 99 w pozycjach ankiety i czas spoza 0--120 minut. Nie oblicza indeksu.
}
\examples{
x <- przygotuj_ankiete(generuj_dane("s017")$dane)
x$dziennik
}
