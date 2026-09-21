\name{pobierz_ocene}
\alias{pobierz_ocene}
\title{Pobranie prywatnej oceny}
\usage{
pobierz_ocene(id, katalog = NULL, sha = NULL)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}

\item{sha}{SHA konkretnej pracy; domyślnie ostatnie potwierdzone oddanie zadania.}
}
\value{
Lista ze stanem i oceną, jeśli opublikowano.
}
\description{
Odczytuje tylko komentarze uwierzytelnionego właściciela repozytorium,
powiązane z żądanym SHA i wersją rubryki. Brak feedbacku nie oznacza zera.
Po oddaniu kolejnych zadań nadal odszukuje ocenę wskazanego zadania.
Nowe oddanie bez oceny nie powoduje zwrócenia oceny wcześniejszej wersji.
}
\examples{
\dontrun{ pobierz_ocene("Z01", "moje-badania") }
}
