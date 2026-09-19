\name{sprawdz_zadanie}
\alias{sprawdz_zadanie}
\title{Lokalna kontrola zadania lub projektu}
\usage{
sprawdz_zadanie(id, katalog = ".", uruchom = TRUE)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{katalog}{Katalog projektu.}

\item{uruchom}{Czy wykonać zapisany skrypt (domyślnie tak).}
}
\value{
Lista: ok, tabela kontroli i jawna lista plików.
}
\description{
Kontrola nie przyznaje punktów za interpretację. Opcjonalne wykonanie skryptu
odbywa się w kopii pracy i świeżym procesie R; wyniki w oryginale muszą być zapisane.
Wykonuj w ten sposób gotowy skrypt po ustawieniu wskazanych parametrów.
Kontrolę prac oddanych wykonuje CI bez poświadczeń kursu.
}
\examples{
k <- tempfile("kontrola-")
utworz_projekt("s017", katalog = k)
sprawdz_zadanie("PROJEKT", k, uruchom = FALSE)$ok
unlink(k, recursive = TRUE)
}
