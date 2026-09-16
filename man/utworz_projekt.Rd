\name{utworz_projekt}
\alias{utworz_projekt}
\title{Utworzenie indywidualnej przestrzeni pracy}
\usage{
utworz_projekt(
  id_studenta,
  scenariusz = "S01",
  katalog = "moje-badania",
  rocznik = "2026-27",
  repo = NULL
)
}
\arguments{
\item{id_studenta}{Pseudonim studenta.}

\item{scenariusz}{S01--S20.}

\item{katalog}{Nowy katalog projektu.}

\item{rocznik}{Rocznik kursu.}

\item{repo}{Oczekiwane prywatne repozytorium jako owner/name, jeśli znane.}
}
\value{
Ścieżka do projektu RStudio, niewidocznie.
}
\description{
Utworzenie indywidualnej przestrzeni pracy
}
\examples{
katalog <- tempfile("projekt-")
utworz_projekt("s017", "S03", katalog)
unlink(katalog, recursive = TRUE)
}
