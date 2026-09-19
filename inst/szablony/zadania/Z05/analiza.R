# Z05 — {{ID}}. Zmień tylko dwie wskazane wartości, jeśli wymaga tego polecenie.
library(badaniaZI)

parametry <- list(
  zmienna = "indeks",           # "indeks" albo "czas_wyszukiwania"
  wartosc_odniesienia = 3        # punkt odniesienia w jednostce wybranej zmiennej
)

uruchom_analize("Z05", katalog = ".", parametry = parametry, B = 4999L)

