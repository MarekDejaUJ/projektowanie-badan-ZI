\name{zapisz_dane}
\alias{zapisz_dane}
\title{Zapis niezmiennego wariantu projektu}
\usage{
zapisz_dane(dane, katalog)
}
\arguments{
\item{dane}{Wynik generuj_dane().}

\item{katalog}{Nowy katalog docelowy; istniejący nie jest nadpisywany.}
}
\value{
Ścieżka katalogu, niewidocznie.
}
\description{
Zachowuje dane CSV/RDS, słownik, kartę scenariusza i manifest z hashami
wszystkich czterech plików. Późniejszy odczyt nie korzysta z bieżącego
generatora ani katalogu scenariuszy, dzięki czemu aktualizacja pakietu
nie zmienia wariantu rozpoczętego projektu.
}
\examples{
katalog <- tempfile("ankieta-")
zapisz_dane(generuj_dane("s017"), katalog)
unlink(katalog, recursive = TRUE)
}
