# Projekt {{SCENARIUSZ}} — {{ID}}, {{ROCZNIK}}
# Otwórz główny projekt RStudio. Ścieżki odnoszą się do jego katalogu.
surowe <- utils::read.csv("dane/surowe.csv", encoding = "UTF-8",
                         stringsAsFactors = FALSE)
dir.create("projekty/ilosciowy/wyniki", recursive = TRUE, showWarnings = FALSE)
wyniki <- "projekty/ilosciowy/wyniki"

# Zapisz reguły czyszczenia oraz liczby zmian. Zachowaj surowe dane.
# Oblicz ukierunkowane pozycje i indeks przy minimum 5 odpowiedziach.
# Przygotuj opis i wykresy, porównanie grup oraz drugą analizę scenariusza.
# Eksportuj tabele CSV i wykresy PNG do katalogu wyniki.
# Tabele: dziennik.csv, opis.csv, indeks.csv, kanaly.csv, porownanie.csv, druga_analiza.csv.
# Wykresy: histogram.png i grupy.png; inne czytelne wykresy możesz dołączyć dodatkowo.
stop("Uzupełnij analizę własnym kodem i usuń tę instrukcję stop().")
