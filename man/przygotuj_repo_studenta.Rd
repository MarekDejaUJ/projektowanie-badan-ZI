\name{przygotuj_repo_studenta}
\alias{przygotuj_repo_studenta}
\title{Przygotowanie prywatnego repozytorium studenta}
\usage{
przygotuj_repo_studenta(
  id_studenta,
  login_github,
  scenariusz = NULL,
  nazwa = NULL,
  rocznik = "2026-27"
)
}
\arguments{
\item{id_studenta}{Pseudonim uzywany w danych i nazwie repozytorium.}

\item{login_github}{Login konta GitHub studenta.}

\item{scenariusz}{Opcjonalna propozycja S01--S20 na C09; bez generowania danych.}

\item{nazwa}{Opcjonalna nazwa repozytorium; domyslnie `ZI-ID`.}

\item{rocznik}{Rocznik konfiguracji kursu.}
}
\value{
Niewidocznie lista z adresem repozytorium i stanem zaproszenia.
}
\description{
Funkcja jest przeznaczona dla prowadzacego. Tworzy zwykle prywatne
repozytorium na jego koncie, umieszcza w nim konfiguracje przestrzeni cwiczen i wysyla
zaproszenie do wskazanego konta studenta. Nie wymaga organizacji GitHub.
Nie generuje danych ani raportu: projekt badawczy rozpoczyna sie na C09.
}
\examples{
\dontrun{
zaloguj_github()
przygotuj_repo_studenta("s017", "login-studenta")
}
}
