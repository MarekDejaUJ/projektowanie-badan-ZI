\name{uruchom_projekt}
\alias{uruchom_projekt}
\title{Uruchomienie gotowej analizy projektu ilościowego}
\usage{
uruchom_projekt(
  katalog = ".",
  parametry = list(zmienna_porownania = "indeks", B = 4999L)
)
}
\arguments{
\item{katalog}{Katalog główny projektu studenta.}

\item{parametry}{Nazwana lista z wyborem zmiennej porównywanej i liczbą replikacji.}
}
\value{
Niewidocznie lista zapisanych wyników.
}
\description{
Uruchomienie gotowej analizy projektu ilościowego
}
\examples{
k <- tempfile("projekt-analiza-")
utworz_projekt("s017", "S02", k)
uruchom_projekt(k, list(zmienna_porownania = "indeks", B = 999L))
unlink(k, recursive = TRUE)
}
