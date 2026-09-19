# Z02 — {{ID}}. Ten skrypt jest gotowy do uruchomienia.
library(badaniaZI)

# W Z02 nie zmieniaj parametrów. Reguły czyszczenia pochodzą ze słownika danych.
parametry <- list(tryb = "czyszczenie")

uruchom_analize("Z02", katalog = ".", parametry = parametry)

