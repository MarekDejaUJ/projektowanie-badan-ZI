\name{przygotuj_zadanie}
\alias{przygotuj_zadanie}
\title{Przygotowanie lokalnego zadania}
\usage{
przygotuj_zadanie(id, katalog = ".")
}
\arguments{
\item{id}{Z01--Z10.}

\item{katalog}{Katalog własnego projektu.}
}
\value{
Ścieżka odpowiedzi Markdown, niewidocznie. Istniejąca praca pozostaje zachowana.
}
\description{
Przygotowanie lokalnego zadania
}
\examples{
k <- tempfile("zadania-")
utworz_projekt("s017", katalog = k)
przygotuj_zadanie("Z01", k)
unlink(k, recursive = TRUE)
}
