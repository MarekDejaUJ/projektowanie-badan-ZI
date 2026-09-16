## ----przygotowanie------------------------------------------------------------
dane <- badaniaZI::przygotuj_ankiete(badaniaZI::dane_przykladowe())$dane
nazwy_pozycji <- paste0("pozycja_", 1:6)
pozycje <- dane[, nazwy_pozycji]
head(pozycje)
colSums(is.na(pozycje))


## ----pozycja------------------------------------------------------------------
jedna <- factor(pozycje$pozycja_1, levels = 1:5, ordered = TRUE)
table(jedna, useNA = "ifany")
100 * prop.table(table(jedna, useNA = "no"))


## ----kierunek-----------------------------------------------------------------
wejscie <- c(1, 2, 3, 4, 5, NA)
data.frame(wejscie = wejscie, po_odwroceniu = badaniaZI::odwroc_pozycje(wejscie))


## ----rekodacja----------------------------------------------------------------
ukierunkowane <- pozycje
ukierunkowane$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
head(ukierunkowane)


## ----minimum------------------------------------------------------------------
male <- data.frame(p1 = c(4, 4, 4), p2 = c(5, 5, 5), p3 = c(4, 4, 4),
  p4 = c(3, 3, 3), p5 = c(4, 4, NA), p6 = c(4, NA, NA))
data.frame(wazne = rowSums(!is.na(male)),
  indeks = badaniaZI::indeks_ankiety(male, minimum = 5))


## ----indeks-------------------------------------------------------------------
indeks <- data.frame(id_odpowiedzi = dane$id_odpowiedzi,
  n_pozycji = rowSums(!is.na(ukierunkowane)),
  indeks = badaniaZI::indeks_ankiety(ukierunkowane, minimum = 5))
head(indeks)
c(wazny_indeks = sum(!is.na(indeks$indeks)),
  brak_indeksu = sum(is.na(indeks$indeks)))
summary(indeks$indeks)


## ----male-kanaly--------------------------------------------------------------
male_kanaly <- data.frame(www = c(1, 1, 0, NA), email = c(1, 0, 1, NA))
badaniaZI::odpowiedzi_wielokrotne(male_kanaly)


## ----kanaly-------------------------------------------------------------------
kanaly <- badaniaZI::odpowiedzi_wielokrotne(dane[, paste0("kanal_", 1:4)])
kanaly
sum(kanaly$procent_respondentow)
