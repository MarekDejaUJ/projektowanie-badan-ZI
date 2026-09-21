\name{utworz_projekt}
\alias{utworz_projekt}
\title{Utworzenie indywidualnej przestrzeni pracy}
\usage{
utworz_projekt(
  id_studenta,
  scenariusz = NULL,
  katalog = "moje-badania",
  rocznik = "2026-27",
  repo = NULL
)
}
\arguments{
\item{id_studenta}{Pseudonim studenta.}

\item{scenariusz}{Opcjonalna propozycja S01--S20 na C09. Nie generuje danych.}

\item{katalog}{Nowy katalog projektu.}

\item{rocznik}{Rocznik kursu.}

\item{repo}{Oczekiwane prywatne repozytorium jako owner/name, jeśli znane.}
}
\value{
Ścieżka do projektu RStudio, niewidocznie.
}
\description{
Przestrzeń przechowuje ćwiczenia, ale nie rozpoczyna jeszcze projektu
badawczego. Własny wariant danych wybiera się dopiero na C09.
}
\examples{
katalog <- tempfile("projekt-")
utworz_projekt("s017", katalog = katalog)
unlink(katalog, recursive = TRUE)
}
