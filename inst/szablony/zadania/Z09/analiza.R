# Z09 — {{ID}}. Wybierz wynik planowanego porównania.
library(badaniaZI)

parametry <- list(
  wynik = "indeks" # dozwolone: "indeks" albo "czas_wyszukiwania"
)

uruchom_analize("Z09", katalog = ".", parametry = parametry)

