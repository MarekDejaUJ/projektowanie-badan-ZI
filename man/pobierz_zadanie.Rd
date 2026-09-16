\name{pobierz_zadanie}
\alias{pobierz_zadanie}
\title{Pobranie własnej przestrzeni pracy}
\usage{
pobierz_zadanie(repo_url, katalog = "moje-badania", id_studenta)
}
\arguments{
\item{repo_url}{HTTPS URL prywatnego repozytorium GitHub.}

\item{katalog}{Nowy katalog lokalny.}

\item{id_studenta}{Oczekiwany pseudonim.}
}
\value{
Ścieżka projektu RStudio, niewidocznie.
}
\description{
Pobranie własnej przestrzeni pracy
}
\examples{
\dontrun{
pobierz_zadanie("https://github.com/prowadzacy/badania-s017.git", "moje-badania", "s017")
}
}
