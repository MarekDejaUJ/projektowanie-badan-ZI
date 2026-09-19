# Z06 — {{ID}}. Zmień tylko wartość pola `zmienna`, jeśli polecenie tego wymaga.
library(badaniaZI)

parametry <- list(
  zmienna = "indeks" # dozwolone: "indeks" albo "czas_wyszukiwania"
)

uruchom_analize("Z06", katalog = ".", parametry = parametry, B = 4999L)

