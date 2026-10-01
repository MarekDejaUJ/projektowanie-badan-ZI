\name{oddaj_zadanie}
\alias{oddaj_zadanie}
\title{Oddanie zadania z konsoli R}
\usage{
oddaj_zadanie(id, katalog = NULL, potwierdz = interactive())
}
\arguments{
\item{id}{Z01--Z10.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}

\item{potwierdz}{Czy pokazać PDF i zapytać przed wysłaniem.}
}
\value{
Lista: stan, zadanie, nazwy wysłanych plików, czas i sumy SHA-256,
  niewidocznie.
}
\description{
Sprawdza zapisany Rmd (odpowiedzi, gotowy kod, ID i dane), tworzy od
początku aktualny PDF w świeżej sesji R i wysyła go do folderu zadania
prowadzącego na nc.uj.edu.pl. Nazwa pliku zawiera ID oraz nazwisko i imię,
np. s017_kowalska_anna_Z03.pdf. W sesji interaktywnej najpierw otwiera PDF
i pyta o potwierdzenie. Ponowne oddanie dodaje nową wersję obok poprzedniej.
Funkcja nie przyznaje punktów.
}
\examples{
\dontrun{ oddaj_zadanie("Z01") }
}
