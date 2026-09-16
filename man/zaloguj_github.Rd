\name{zaloguj_github}
\alias{zaloguj_github}
\title{Logowanie do GitHub z konsoli R}
\usage{
zaloguj_github(
  metoda = c("auto", "device", "gh"),
  client_id = NULL,
  timeout = 600,
  przegladarka = TRUE
)
}
\arguments{
\item{metoda}{"auto", "device" albo "gh".}

\item{client_id}{Publiczny identyfikator aplikacji z włączonym device flow.}

\item{timeout}{Maksymalna liczba sekund na potwierdzenie.}

\item{przegladarka}{Czy otworzyć stronę potwierdzenia.}
}
\value{
Login konta GitHub, niewidocznie. Token nie jest zwracany.
}
\description{
Hasło i 2FA podaje się wyłącznie na GitHub. Wariant device wymaga publicznego
client ID aplikacji kursu. Wariant gh wymaga GitHub CLI i używa osobnego,
tymczasowego katalogu bez systemowego magazynu poświadczeń. Po logowaniu
usuwa ten katalog; token pozostaje w pamięci bieżącej sesji R.
}
\examples{
\dontrun{
zaloguj_github()
wyloguj_github()
}
}
