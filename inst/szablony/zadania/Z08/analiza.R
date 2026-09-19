# Z08 — {{ID}}. Zmień tylko wartości dwóch wskazanych pól.
library(badaniaZI)

parametry <- list(
  wykonanie = "czas_wyszukiwania", # "czas_wyszukiwania" albo "powodzenie"
  metoda = "spearman"              # "spearman" albo "pearson"
)

uruchom_analize("Z08", katalog = ".", parametry = parametry, B = 4999L)

