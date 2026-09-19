# Z03 — {{ID}}. Zmień tylko wartość pola `zmienna`, jeśli polecenie tego wymaga.
library(badaniaZI)

parametry <- list(
  zmienna = "czas_wyszukiwania" # dozwolone: "czas_wyszukiwania" albo "indeks"
)

uruchom_analize("Z03", katalog = ".", parametry = parametry)

