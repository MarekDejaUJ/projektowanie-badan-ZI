# Z04 — {{ID}}. Ten skrypt jest gotowy do uruchomienia.
library(badaniaZI)

# Indeks wymaga co najmniej pięciu z sześciu ważnych odpowiedzi.
parametry <- list(minimum_pozycji = 5L)

uruchom_analize("Z04", katalog = ".", parametry = parametry)

