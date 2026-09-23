\name{slownik_zmiennych}
\alias{slownik_zmiennych}
\title{Slownik zmiennych przykladu S02}
\usage{
slownik_zmiennych(zmienne = NULL)
}
\arguments{
\item{zmienne}{Opcjonalny wektor nazw kolumn; domyslnie wszystkie.}
}
\value{
Ramka danych z kolumnami zmienna, opis, skala, kodowanie, braki,
  odwrocona.
}
\description{
Odczytuje slownik danych dolaczony do pakietu: nazwe kolumny, opis
(dla pozycji ankiety pelne brzmienie stwierdzenia), skale pomiaru, kodowanie,
zapis brakow i informacje o pozycji odwroconej.
}
\examples{
slownik_zmiennych(c("czas_wyszukiwania", "powodzenie"))
}
