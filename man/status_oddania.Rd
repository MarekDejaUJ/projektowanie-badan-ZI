\name{status_oddania}
\alias{status_oddania}
\title{Status odbioru i kontroli}
\usage{
status_oddania(id, katalog = NULL, sha = NULL)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}

\item{sha}{Opcjonalne SHA konkretnej wersji; domyślnie ostatnie oddanie zadania.}
}
\value{
Stan odbioru; wynik kontroli Actions jest osobnym polem.
}
\description{
Domyślnie odszukuje ostatnie potwierdzone oddanie wskazanego zadania na
zdalnej gałęzi main, również po oddaniu innych zadań. Nie ocenia lokalnych,
niewysłanych zmian. Bez pokwitowania zwraca stan brak_oddania.
}
\examples{
\dontrun{ status_oddania("Z01", "moje-badania") }
}
