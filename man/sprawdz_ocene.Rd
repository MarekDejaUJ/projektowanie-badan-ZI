\name{sprawdz_ocene}
\alias{sprawdz_ocene}
\title{Walidacja punktów formularza}
\usage{
sprawdz_ocene(formularz, id = "PROJEKT")
}
\arguments{
\item{formularz}{Wypełniony formularz_oceny().}

\item{id}{Identyfikator rubryki.}
}
\value{
Łączna punktacja; błąd przy niekompletnej lub niedozwolonej ocenie.
}
\description{
Walidacja punktów formularza
}
\examples{
x <- formularz_oceny("Z01")
x$punkty <- x$maksimum
sprawdz_ocene(x, "Z01")
}
