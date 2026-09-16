\name{rozpocznij_zajecia}
\alias{rozpocznij_zajecia}
\title{Początek zajęć na czyszczonym komputerze}
\usage{
rozpocznij_zajecia(
  cwiczenie,
  id_studenta,
  repo_url = NULL,
  katalog = "moje-badania",
  otworz = TRUE
)
}
\arguments{
\item{cwiczenie}{C01--C10.}

\item{id_studenta}{Pseudonim studenta.}

\item{repo_url}{URL własnego repo; dla istniejącego katalogu odczytywany z konfiguracji.}

\item{katalog}{Katalog lokalny.}

\item{otworz}{Czy otworzyć projekt i materiał w IDE/przeglądarce.}
}
\value{
Lista: projekt i identyfikator ćwiczenia, niewidocznie.
}
\description{
Początek zajęć na czyszczonym komputerze
}
\examples{
\dontrun{
rozpocznij_zajecia("C02", "s017", "https://github.com/prowadzacy/badania-s017.git")
}
}
