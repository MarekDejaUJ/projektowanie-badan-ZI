\name{rozpocznij_zajecia}
\alias{rozpocznij_zajecia}
\title{Początek zajęć}
\usage{
rozpocznij_zajecia(
  cwiczenie,
  id_studenta = NULL,
  student = NULL,
  katalog = NULL,
  otworz = TRUE
)
}
\arguments{
\item{cwiczenie}{C01--C10.}

\item{id_studenta}{Pseudonim studenta; wymagany przy pierwszym starcie,
później sprawdzany z kurs.yml.}

\item{student}{Imię i nazwisko do nazw plików oddania, np. "Anna Kowalska";
wymagane przy pierwszym starcie, później można je poprawić.}

\item{katalog}{Katalog pracy. NULL używa zapamiętanej lub bieżącej
przestrzeni kursu, a przy pierwszym starcie podkatalogu moje-badania.}

\item{otworz}{Czy otworzyć roboczy Rmd w bieżącej sesji IDE.}
}
\value{
Lista: plik, cwiczenie, id, student, katalog, materialy i stan
  (utworzono albo otwarto), niewidocznie.
}
\description{
Przy pierwszym starcie tworzy w bieżącym folderze roboczym przestrzeń
moje-badania z ID i imieniem oraz nazwiskiem studenta. Przy kolejnych
odnajduje tę przestrzeń, dopisuje plik zadanie.Rmd bieżącego ćwiczenia
i udostępnia materiały do czytania w podfolderze materialy. Nie zmienia
katalogu roboczego, nie łączy się z siecią i nie nadpisuje odpowiedzi.
}
\examples{
k <- tempfile("start-")
rozpocznij_zajecia("C01", id_studenta = "s017", student = "Anna Kowalska",
                   katalog = k, otworz = FALSE)
unlink(k, recursive = TRUE)
}
