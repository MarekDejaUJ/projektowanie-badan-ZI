# Z07 — {{ID}}. Ten skrypt jest gotowy do uruchomienia.
library(badaniaZI)

# Analiza zestawia grupę z obserwowanym powodzeniem zadania.
parametry <- list(wiersze = "grupa", kolumny = "powodzenie")

uruchom_analize("Z07", katalog = ".", parametry = parametry, B = 4999L)

