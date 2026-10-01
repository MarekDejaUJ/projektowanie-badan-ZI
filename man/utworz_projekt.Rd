\name{utworz_projekt}
\alias{utworz_projekt}
\title{Utworzenie indywidualnej przestrzeni pracy}
\usage{
utworz_projekt(
  id_studenta,
  scenariusz = NULL,
  katalog = "moje-badania",
  rocznik = "2026-27",
  student = NULL
)
}
\arguments{
\item{id_studenta}{Pseudonim studenta.}

\item{scenariusz}{Opcjonalna propozycja S01--S20 na C09. Nie generuje danych.}

\item{katalog}{Nowy katalog projektu.}

\item{rocznik}{Rocznik kursu.}

\item{student}{Imię i nazwisko do nazw plików oddania, np. "Anna Kowalska".
Można je dopisać później w rozpocznij_zajecia().}
}
\value{
Ścieżka do projektu RStudio, niewidocznie.
}
\description{
Przestrzeń przechowuje ćwiczenia, ale nie rozpoczyna jeszcze projektu
badawczego. Własny wariant danych wybiera się dopiero na C09. Zwykle
tworzy ją rozpocznij_zajecia() przy pierwszym starcie.
}
\examples{
katalog <- tempfile("projekt-")
utworz_projekt("s017", katalog = katalog, student = "Anna Kowalska")
unlink(katalog, recursive = TRUE)
}
