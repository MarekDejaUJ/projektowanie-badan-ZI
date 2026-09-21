\name{analizy_wycofane}
\alias{analizy_wycofane}
\alias{uruchom_analize}
\alias{uruchom_projekt}
\title{Wycofany eksport osobnych analiz CSV}
\usage{
uruchom_analize(id, katalog = NULL, parametry = list(), B = 4999L)

uruchom_projekt(katalog = NULL, parametry = list())
}
\arguments{
\item{id}{Dawny identyfikator zadania; funkcja go nie wykonuje.}

\item{katalog}{Dawny katalog pracy; nie jest zmieniany.}

\item{parametry}{Dawne ustawienia; nie są wykonywane.}

\item{B}{Dawna liczba replikacji; nie jest używana.}
}
\value{
Błąd z instrukcją korzystania z bieżącego Rmd.
}
\description{
Dawne funkcje pozostają wyłącznie po to, aby stary skrypt zatrzymał się
z czytelną instrukcją. Nie generują danych, nie zapisują wyników i nie
zmieniają istniejącej pracy. Bieżący kurs korzysta z pełnego Rmd oraz
towarzyszącego analiza.R. Uruchamiaj chunki, a własne akapity wpisuj w Rmd.
Do odtworzenia PDF służy sprawdz_zadanie(), a do oddania oddaj_zadanie()
lub oddaj_projekt(). Starej pracy nie należy przenosić przez ponowne
generowanie danych; zachowaj jej pliki i wersję pakietu.
}
