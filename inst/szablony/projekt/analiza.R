# Projekt {{SCENARIUSZ}} — {{ID}}, {{ROCZNIK}}.
# Skrypt zawiera gotowy tok analizy. Student wybiera tylko wskazaną zmienną,
# uruchamia cały plik i samodzielnie interpretuje zapisane wyniki.
library(badaniaZI)

parametry <- list(
  zmienna_porownania = "indeks", # dozwolone: "indeks" albo "czas_wyszukiwania"
  B = 4999L                      # liczba replikacji; pozostaw co najmniej 999
)

uruchom_projekt(katalog = ".", parametry = parametry)
