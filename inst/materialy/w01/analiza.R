# W01: tabele i wykresy wykładu. Małe przykłady liczbowe są dydaktyczne;
# słownik i mapa kursu pochodzą z zasobów pakietu.

# Mapa kursu: kod, rodzaj, tytuł i jedno zdanie o treści każdej jednostki.
mapa <- badaniaZI::mapa_kursu()
mapa_do_druku <- mapa
names(mapa_do_druku) <- c("Kod", "Rodzaj", "Tytuł", "Treść")

# Słownik zbioru S02: nazwa kolumny, opis, skala pomiaru i kodowanie.
slownik <- badaniaZI::slownik_zmiennych()
slownik_do_druku <- slownik[c("zmienna", "opis", "skala", "kodowanie")]
pozycja <- grepl("^pozycja_", slownik$zmienna)
slownik_do_druku$kodowanie[pozycja] <- "1–5: od 1 = zdecydowanie nie do 5 = zdecydowanie tak"
slownik_do_druku$kodowanie <- gsub("--", "–", slownik_do_druku$kodowanie, fixed = TRUE)
kod_99 <- grepl("99", slownik$braki)
slownik_do_druku$kodowanie[kod_99] <- paste0(slownik_do_druku$kodowanie[kod_99], "; 99 = brak odpowiedzi")
slownik_do_druku$zmienna <- gsub("_", "\\_", slownik_do_druku$zmienna, fixed = TRUE)
names(slownik_do_druku) <- c("Kolumna", "Opis", "Skala", "Kodowanie")

# Sześć pozycji indeksu z brzmieniem i kierunkiem.
pozycje <- slownik[grepl("^pozycja_", slownik$zmienna), c("zmienna", "opis", "odwrocona")]
pozycje_do_druku <- data.frame(
  Pozycja = sub("pozycja_", "", pozycje$zmienna),
  Brzmienie = pozycje$opis,
  Kierunek = ifelse(pozycje$odwrocona, "odwrócona: 6 − x", "bez zmiany")
)

# Etapy rozumowania od problemu do wniosku.
wykres_etapow <- badaniaZI::diagram_etapow(
  c("Problem instytucji", "Konstrukt", "Wskaźnik", "Projekt i próba", "Estymanda", "Wniosek"),
  c("Jaka decyzja wymaga informacji?",
    "Jakie pojęcie opisuje trudność, np. sprawność wyszukiwania?",
    "Co dokładnie obserwujemy lub o co pytamy?",
    "Kogo badamy, kiedy i w jakich warunkach?",
    "Jaką wielkość szacujemy i w jakiej jednostce?",
    "Co pozwala stwierdzić wybrany projekt?"), tytul = "Sześć etapów rozumowania")

# Przejście od zaproszenia do analizy: każda liczba opisuje inny etap.
etapy_proby <- data.frame(
  etap = c("Zaproszeni", "Rozpoczęli zadanie", "Ukończyli ankietę", "Mają ważny indeks"),
  n = c(300, 180, 150, 142))
wykres_lejka <- badaniaZI::wykres_przeplyw_proby(etapy_proby$etap, etapy_proby$n, tytul = "Przepływ próby")

# Średnia i mediana czasów 2, 3, 3 i 20 minut.
czasy_przyklad <- c(2, 3, 3, 20)
wykres_miar <- badaniaZI::wykres_srednia_mediana(czasy_przyklad, os = "Czas [min]",
  tytul = "Średnia i mediana czterech czasów")

# Ta sama estymata różnicy 2 minut z przedziałem wąskim i szerokim.
wykres_niepewnosci <- badaniaZI::wykres_przedzialy(
  c("Przedział wąski", "Przedział szeroki"), c(2, 2), c(1.7, -3), c(2.3, 7),
  odniesienie = 0, os = "Różnica średnich czasów: nowi minus doświadczeni [min]",
  tytul = "Ta sama estymata, dwa przedziały")

# Dwie filie: zmiana czasu przed i po wdrożeniu nowych filtrów w filii A.
wykres_filii <- badaniaZI::wykres_roznica_zmian(
  przed = c(10, 8), po = c(7, 7), grupy = c("Filia A (nowe filtry)", "Filia B (bez zmiany)"),
  kontrfakt = "Filia A przy zmianie takiej jak w filii B", os = "Średni czas [min]",
  tytul = "Różnica zmian w dwóch filiach")

# Dwa rozkłady o średniej 8 minut i różnym rozrzucie.
wykres_rozrzutu <- badaniaZI::wykres_rowne_srednie(
  8, c(1, 4), c("SD = 1 minuta", "SD = 4 minuty"), os = "Czas [min]",
  tytul = "Dwa rozkłady o tej samej średniej")

# Uproszczona piramida dowodów dla pytań o skutek interwencji.
wykres_piramidy <- badaniaZI::diagram_piramida_dowodow(c(
  "Opinia eksperta", "Opis pojedynczego przypadku", "Badanie porównawcze bez randomizacji",
  "Badanie z randomizacją", "Przegląd systematyczny z metaanalizą"), tytul = "Piramida dowodów")
