\name{wyloguj_github}
\alias{wyloguj_github}
\title{Zakończenie sesji GitHub pakietu}
\usage{
wyloguj_github(komunikat = TRUE)
}
\arguments{
\item{komunikat}{Czy wyświetlić potwierdzenie.}
}
\value{
TRUE, niewidocznie.
}
\description{
Usuwa token i zapamiętany katalog pracy z sesji pakietu. Nie usuwa plików.
Wyloguj też przeglądarkę na komputerze w sali
i zakończ sesję R bez zapisywania przestrzeni roboczej.
}
\examples{
wyloguj_github()
}
