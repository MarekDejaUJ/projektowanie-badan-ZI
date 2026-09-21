\name{przygotuj_zadanie}
\alias{przygotuj_zadanie}
\title{Przygotowanie pełnego roboczego Rmd}
\usage{
przygotuj_zadanie(id, katalog = NULL)
}
\arguments{
\item{id}{Z01--Z10.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}
}
\value{
Bezwzględna ścieżka zadanie.Rmd, niewidocznie.
}
\description{
Kopiuje ćwiczenie LEARN i CHALLENGE do jednego pliku zadanie.Rmd wraz
z gotowym analiza.R. ID jest pobierane z kurs.yml; nie trzeba wpisywać
go ponownie w nagłówku. Ponowne wywołanie nie nadpisuje odpowiedzi ani
przypiętego skryptu. Stare pliki Markdown wymagają osobnej migracji.
Z10 wymaga zapisanego wariantu Z09 i zachowuje jego kopię bez losowania.
}
\examples{
k <- tempfile("zadania-")
utworz_projekt("s017", katalog = k)
przygotuj_zadanie("Z01", k)
unlink(k, recursive = TRUE)
}
