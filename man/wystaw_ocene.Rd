\name{wystaw_ocene}
\alias{wystaw_ocene}
\title{Publikacja prywatnej informacji zwrotnej}
\usage{
wystaw_ocene(repo, sha, id, formularz, komentarz = "")
}
\arguments{
\item{repo}{Prywatne repo jako owner/name.}

\item{sha}{SHA ocenianej pracy.}

\item{id}{Z01--Z10 albo PROJEKT.}

\item{formularz}{Wypełniony formularz_oceny().}

\item{komentarz}{Ogólna informacja merytoryczna.}
}
\value{
URL prywatnego komentarza GitHub, niewidocznie.
}
\description{
Publikuje ocenę jako komentarz przypisany do rzeczywistego SHA pracy.
Wymaga logowania na konto właściciela repozytorium; punkty muszą odpowiadać
poziomom rubryki. Oceny nie trafiają do repozytorium materiałów ani Pages.
}
\examples{
\dontrun{
f <- formularz_oceny("Z01")
f$punkty <- f$maksimum
wystaw_ocene("prowadzacy/badania-s017", "SHA_PRACY", "Z01", f, "Poprawny odczyt wektora.")
}
}
