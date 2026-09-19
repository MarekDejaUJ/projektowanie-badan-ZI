\name{uruchom_analize}
\alias{uruchom_analize}
\title{Uruchomienie gotowej analizy zadania}
\usage{
uruchom_analize(id, katalog = ".", parametry = list(), B = 4999L)
}
\arguments{
\item{id}{Identyfikator Z01--Z10.}

\item{katalog}{Katalog główny projektu studenta.}

\item{parametry}{Nazwana lista ustawień dozwolonych w danym zadaniu.}

\item{B}{Liczba replikacji bootstrapu albo permutacji; co najmniej 999.}
}
\value{
Niewidocznie lista zapisanych wyników i obiektów analizy.
}
\description{
Funkcja wykonuje odtwarzalny tok obliczeń przewidziany dla danego zadania.
Student wybiera jedynie wskazane w materiale parametry i interpretuje zapisane
wyniki. Surowy plik danych pozostaje bez zmian.
}
\examples{
k <- tempfile("analiza-")
utworz_projekt("s017", "S02", k)
przygotuj_zadanie("Z03", k)
uruchom_analize("Z03", k, list(zmienna = "czas_wyszukiwania"), B = 999)
unlink(k, recursive = TRUE)
}
