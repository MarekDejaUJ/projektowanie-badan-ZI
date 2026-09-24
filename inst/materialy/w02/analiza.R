# W02: tabele i wykresy wykładu. Małe serie liczb są syntetycznymi
# demonstracjami; czasy S02 pochodzą z przygotowanego zbioru przykładowego.

liczba <- function(x, cyfry = 2L) sub("^-", "−", formatC(x, format = "f", digits = cyfry, decimal.mark = ","))

# Zbiór S02 po regułach przygotowania z ćwiczenia C02 (148 osób, 142 ważne czasy).
dane <- badaniaZI::przygotuj_ankiete(badaniaZI::dane_przykladowe())$dane
czas <- dane$czas_wyszukiwania[!is.na(dane$czas_wyszukiwania)]

# Dwie serie pięciu czasów różnią się wyłącznie ostatnią wartością.
serie <- list(A = c(2, 4, 6, 8, 10), B = c(2, 4, 6, 8, 30))
skosnosc <- function(x) mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
opis_serii <- do.call(rbind, lapply(names(serie), function(nazwa) {
  x <- serie[[nazwa]]
  data.frame(Seria = nazwa, N = length(x), `Średnia` = liczba(mean(x)), Mediana = liczba(median(x)),
             Wariancja = liczba(var(x)), SD = liczba(sd(x)), Q1 = liczba(quantile(x, 0.25)),
             Q3 = liczba(quantile(x, 0.75)), IQR = liczba(IQR(x)), `P90` = liczba(quantile(x, 0.9)),
             g1 = liczba(skosnosc(x)), check.names = FALSE)
}))

# Składniki wariancji serii A z wierszem sumy.
odchylenia <- serie$A - mean(serie$A)
skladniki <- data.frame(`Czas x [min]` = c(serie$A, "Suma"),
                        `Odchylenie x − 6` = c(liczba(odchylenia, 0L), liczba(sum(odchylenia), 0L)),
                        `Kwadrat odchylenia` = c(liczba(odchylenia^2, 0L), liczba(sum(odchylenia^2), 0L)),
                        check.names = FALSE)

# Dwadzieścia syntetycznych odpowiedzi na pozycję 2 („Nazwy filtrów są dla mnie zrozumiałe.”).
oceny <- data.frame(odpowiedz = 1:5, liczba = c(2, 3, 5, 7, 3))
oceny$procent <- 100 * oceny$liczba / sum(oceny$liczba)
oceny$skumulowany <- cumsum(oceny$procent)
oceny_do_druku <- data.frame(`Odpowiedź` = c("1 zdecydowanie nie", "2 raczej nie", "3 ani tak, ani nie",
                                             "4 raczej tak", "5 zdecydowanie tak"),
                             `Liczebność` = oceny$liczba, `Odsetek [%]` = liczba(oceny$procent, 0L),
                             `Odsetek skumulowany [%]` = liczba(oceny$skumulowany, 0L), check.names = FALSE)

# Miary czasu S02 używane w tekście.
czas_opis <- list(n = length(czas), srednia = mean(czas), sd = sd(czas), mediana = median(czas),
                  q1 = unname(quantile(czas, 0.25)), q3 = unname(quantile(czas, 0.75)),
                  min = min(czas), max = max(czas), g1 = skosnosc(czas),
                  fd = 2 * IQR(czas) / length(czas)^(1 / 3),
                  od5do10 = sum(czas > 5 & czas <= 10), F10 = mean(czas <= 10))
grupy_opis <- do.call(rbind, lapply(split(dane$czas_wyszukiwania, dane$grupa), function(x) {
  x <- x[!is.na(x)]
  data.frame(n = length(x), mediana = median(x), q1 = unname(quantile(x, 0.25)),
             q3 = unname(quantile(x, 0.75)), granica = unname(quantile(x, 0.75) + 1.5 * IQR(x)),
             poza = sum(x > quantile(x, 0.75) + 1.5 * IQR(x)), max = max(x))
}))

# Wykresy.
wykres_ocen <- badaniaZI::wykres_czestosci(1:5, oceny$liczba,
  os = "Odpowiedź (1 = zdecydowanie nie, 5 = zdecydowanie tak)", tytul = "Dwadzieścia odpowiedzi na pozycję 2")
wykres_serii <- badaniaZI::wykres_srednia_mediana(unlist(serie), rep(c("Seria A", "Seria B"), each = 5),
  os = "Czas [min]", tytul = "Jedna długa próba przesuwa średnią")
wykres_rozrzutu <- badaniaZI::wykres_srednia_mediana(c(rep(6, 6), 2, 2, 2, 10, 10, 10),
  rep(c("Grupa 1", "Grupa 2"), each = 6), os = "Czas [min]", tytul = "Ta sama średnia, różny rozrzut")
wykres_sd_se <- badaniaZI::wykres_sd_se(czas, c(5, 10, 25, 50, 100, 142), os = "Minuty",
  tytul = "SD osób i SE średniej czasu S02")
wykres_szerokosci <- badaniaZI::wykres_histogram_panele(
  list(`Szerokość 1 min` = czas, `Szerokość 2,5 min` = czas, `Szerokość 5 min` = czas),
  c(1, 2.5, 5), poczatek = 0, os = "Czas wyszukiwania [min]", tytul = "Czas S02 przy trzech szerokościach przedziału")
wykres_gestosci <- badaniaZI::wykres_histogram(czas, 2.5, skala = "gestosc", zaznacz = c(5, 10),
  os = "Czas wyszukiwania [min]", tytul = "Histogram gęstości czasu S02")
jeden_szczyt <- 10 + 2.2 * qnorm(ppoints(60))
dwa_szczyty <- c(6 + 1.2 * qnorm(ppoints(30)), 14 + 1.2 * qnorm(ppoints(30)))
wykres_szczytow <- badaniaZI::wykres_histogram_panele(list(`Jeden szczyt` = jeden_szczyt, `Dwa szczyty` = dwa_szczyty),
  1, poczatek = 2, os = "Czas [min]", srednia = TRUE, tytul = "Ta sama średnia, różny kształt")
wykres_pudelka <- badaniaZI::wykres_pudelkowy(serie$B, os = "Czas [min]", tytul = "Seria B na wykresie pudełkowym")
wykres_pudelek_grup <- badaniaZI::wykres_pudelkowy(dane$czas_wyszukiwania, dane$grupa,
  os = "Czas wyszukiwania [min]", tytul = "Czas S02 w dwóch grupach")
wykres_dystrybuanty_B <- badaniaZI::wykres_dystrybuanta(serie$B, prog = 8, os = "Czas [min]",
  tytul = "Dystrybuanta empiryczna serii B")
wykres_dystrybuanty_S02 <- badaniaZI::wykres_dystrybuanta(czas, prog = 10, os = "Czas wyszukiwania [min]",
  tytul = "Dystrybuanta empiryczna czasu S02")
wykres_licznosci <- badaniaZI::wykres_licznosci_odsetki(c("Nowi", "Doświadczeni"), c(16, 60), c(20, 100),
  tytul = "Liczba i odsetek sukcesów w grupach 20 i 100 osób")

# Miary, pytania i wymagania jednostki (tabela podsumowania).
miary <- data.frame(
  Miara = c("Średnia", "Mediana", "Dominanta", "SD", "IQR", "Odsetek"),
  `Pytanie` = c("Jaki nakład przypada na jedną obserwację?", "Gdzie leży środek uporządkowanych wartości?",
                "Która kategoria występuje najczęściej?", "Jak daleko od średniej leżą pomiary?",
                "Jak szeroka jest środkowa połowa wartości?", "Jaka część mianownika spełnia warunek?"),
  `Jednostka` = c("jednostka pomiaru", "jednostka pomiaru", "kategoria", "jednostka pomiaru",
                  "jednostka pomiaru", "procent z nazwanym N"),
  `Skala pomiaru` = c("ilorazowa, interwałowa", "porządkowa i wyższe", "każda", "ilorazowa, interwałowa",
                      "porządkowa i wyższe", "nominalna 0/1"),
  check.names = FALSE)

# Handout: jeden histogram czasu S02 o szerokości 2,5 minuty.
rownames(opis_serii) <- NULL
wykres_histogramu <- badaniaZI::wykres_histogram(czas, 2.5, os = "Czas wyszukiwania [min]",
  tytul = "Czas S02 po przygotowaniu")
