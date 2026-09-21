\name{rozpocznij_zajecia}
\alias{rozpocznij_zajecia}
\title{Początek zajęć na czyszczonym komputerze}
\usage{
rozpocznij_zajecia(
  cwiczenie,
  id_studenta,
  repo_url = NULL,
  katalog = NULL,
  otworz = TRUE
)
}
\arguments{
\item{cwiczenie}{C01--C10.}

\item{id_studenta}{Pseudonim studenta.}

\item{repo_url}{URL własnego repo; dla istniejącego katalogu odczytywany z konfiguracji.}

\item{katalog}{Katalog lokalny. NULL używa zapamiętanej lub bieżącej
przestrzeni kursu, a przy pierwszym pobraniu podkatalogu moje-badania.}

\item{otworz}{Czy otworzyć roboczy Rmd w bieżącej sesji IDE.}
}
\value{
Lista: projekt, plik, cwiczenie, id, katalog i aktualizacja
  (pobrano, sprawdzono albo lokalne_zmiany), niewidocznie.
}
\description{
Otwiera zadanie.Rmd w bieżącej sesji IDE, nie przełącza projektu RStudio
ani nie zmienia katalogu roboczego. Dzięki temu zachowuje ID i logowanie.
Zapamiętuje własną przestrzeń dla dalszych poleceń bez argumentu katalog.
Jeśli istnieją niezapisane w Gicie zmiany, zachowuje je i pomija aktualizację.
}
\examples{
\dontrun{
rozpocznij_zajecia("C02", "s017", "https://github.com/prowadzacy/badania-s017.git")
}
}
