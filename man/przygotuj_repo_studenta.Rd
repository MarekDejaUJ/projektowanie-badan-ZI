\name{przygotuj_repo_studenta}
\alias{przygotuj_repo_studenta}
\title{Przygotowanie prywatnego repozytorium studenta}
\usage{
przygotuj_repo_studenta(
  id_studenta,
  login_github,
  scenariusz = "S01",
  nazwa = NULL,
  rocznik = "2026-27"
)
}
\arguments{
\item{id_studenta}{Pseudonim używany w danych i nazwie repozytorium.}

\item{login_github}{Login konta GitHub studenta.}

\item{scenariusz}{S01--S20.}

\item{nazwa}{Opcjonalna nazwa repozytorium; domyślnie `ZI-ID`.}

\item{rocznik}{Rocznik konfiguracji kursu.}
}
\value{
Niewidocznie lista z adresem repozytorium i stanem zaproszenia.
}
\description{
Funkcja jest przeznaczona dla prowadzącego. Tworzy zwykłe prywatne
repozytorium na jego koncie, umieszcza w nim indywidualny projekt i wysyła
zaproszenie do wskazanego konta studenta. Nie wymaga organizacji GitHub.
}
\examples{
\dontrun{
zaloguj_github()
przygotuj_repo_studenta("s017", "login-studenta", "S02")
}
}
