#' Wycofany eksport osobnych analiz CSV
#'
#' Dawne funkcje pozostają wyłącznie po to, aby stary skrypt zatrzymał się
#' z czytelną instrukcją. Nie generują danych, nie zapisują wyników i nie
#' zmieniają istniejącej pracy. Bieżący kurs korzysta z pełnego Rmd oraz
#' towarzyszącego analiza.R. Uruchamiaj chunki, a własne akapity wpisuj w Rmd.
#' Do odtworzenia PDF służy sprawdz_zadanie(), a do oddania oddaj_zadanie()
#' lub oddaj_projekt(). Starej pracy nie należy przenosić przez ponowne
#' generowanie danych; zachowaj jej pliki i wersję pakietu.
#' @param id Dawny identyfikator zadania; funkcja go nie wykonuje.
#' @param katalog Dawny katalog pracy; nie jest zmieniany.
#' @param parametry Dawne ustawienia; nie są wykonywane.
#' @param B Dawna liczba replikacji; nie jest używana.
#' @return Błąd z instrukcją korzystania z bieżącego Rmd.
#' @name analizy_wycofane
#' @export
uruchom_analize <- function(id, katalog = NULL, parametry = list(), B = 4999L) {
  .Defunct(msg = paste(
    "Osobny eksport CSV zosta\u0142 wycofany z bie\u017c\u0105cego kursu.",
    "Pracuj w zadanie.Rmd przygotowanym przez rozpocznij_zajecia():",
    "uruchom gotowe chunki i wpisz w\u0142asne akapity.",
    "Zapisany Rmd oddajesz poleceniem oddaj_zadanie(). Zachowano istniej\u0105ce pliki."
  ))
}

#' @rdname analizy_wycofane
#' @export
uruchom_projekt <- function(katalog = NULL, parametry = list()) {
  .Defunct(msg = paste(
    "Osobny eksport CSV projektu zosta\u0142 wycofany z bie\u017c\u0105cego kursu.",
    "Po wykonaniu Z09 i Z10 u\u017cyj przygotuj_raport(), pracuj w raport.Rmd",
    "i oddaj zapisany raport przez oddaj_projekt().",
    "Nie generuj ponownie wcze\u015bniejszych danych. Zachowano istniej\u0105ce pliki."
  ))
}
