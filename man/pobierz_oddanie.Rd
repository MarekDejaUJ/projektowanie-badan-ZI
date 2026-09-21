\name{pobierz_oddanie}
\alias{pobierz_oddanie}
\title{Pobranie konkretnego oddania do niezależnej kontroli}
\usage{
pobierz_oddanie(repo, sha, id, id_studenta, katalog)
}
\arguments{
\item{repo}{Prywatne repozytorium jako owner/name.}

\item{sha}{Pełny SHA oddania, z pokwitowania odbioru.}

\item{id}{Z01--Z10 albo PROJEKT.}

\item{id_studenta}{Oczekiwane pseudonimowe ID, z prywatnego przydziału.}

\item{katalog}{Nowy prywatny katalog kontroli. Nigdy nie jest nadpisywany.}
}
\value{
Ścieżka katalogu z podkatalogiem wejscie i plikiem odbior.json,
  niewidocznie. To pobranie dowodów, nie wynik wykonania ani ocena.
}
\description{
Funkcja dla właściciela prywatnego repozytorium. Pobiera wyłącznie jawne
pliki wskazanego zadania z niezmiennego SHA, bez klonowania historii,
otwierania PDF, wykonywania Rmd ani odczytywania RDS. Każdy plik jest
sprawdzany względem identyfikatora obiektu Git. Odpowiedzi i dane pozostają
prywatne; katalog przechowuj poza publicznym repozytorium kursu.
Kontrola techniczna i ocena merytoryczna są osobnymi krokami.
}
\examples{
\dontrun{
pobierz_oddanie("prowadzacy/ZI-s017", "SHA_Z_POKWITOWANIA", "Z01",
                "s017", "prywatna-kontrola-z01")
}
}
