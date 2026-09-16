## ----import-------------------------------------------------------------------
surowe <- utils::read.csv(badaniaZI::plik_przykladu("csv"),
  encoding = "UTF-8", stringsAsFactors = FALSE)
dim(surowe)
head(surowe[, c("id_odpowiedzi", "grupa", "czas_wyszukiwania", "pozycja_2")])




## ----struktura----------------------------------------------------------------
wybrane <- c("id_odpowiedzi", "grupa", "czas_wyszukiwania")
str(surowe[, c(wybrane, "czestosc_korzystania")])
badaniaZI::tabela_klas_r(surowe[, wybrane])


## ----podsumowanie-------------------------------------------------------------
summary(surowe[, c("czas_wyszukiwania", "pozycja_2", "czestosc_korzystania")])


## ----duplikaty----------------------------------------------------------------
n_przed <- nrow(surowe)
liczba_duplikatow <- sum(duplicated(surowe))
dane <- surowe[!duplicated(surowe), ]
c(przed = n_przed, po = nrow(dane), kopie = liczba_duplikatow)
anyDuplicated(dane$id_odpowiedzi)


## ----braki--------------------------------------------------------------------
kod_99 <- !is.na(dane$pozycja_2) & dane$pozycja_2 == 99
liczba_99 <- sum(kod_99)
dane$pozycja_2[kod_99] <- NA
poza_zakresem <- !is.na(dane$czas_wyszukiwania) &
  (dane$czas_wyszukiwania < 0 | dane$czas_wyszukiwania > 120)
liczba_czasow <- sum(poza_zakresem)
dane$czas_wyszukiwania[poza_zakresem] <- NA
c(kod_99 = liczba_99, czasy_poza_zakresem = liczba_czasow)


## ----etykiety-----------------------------------------------------------------
dane$grupa <- factor(dane$grupa)
ocena_opis <- factor(dane$pozycja_1, levels = 1:5,
  labels = c("1 zdecydowanie nie", "2 raczej nie", "3 ani tak, ani nie",
    "4 raczej tak", "5 zdecydowanie tak"),
  ordered = TRUE)
table(ocena_opis, useNA = "ifany")


## ----dziennik-----------------------------------------------------------------
dziennik <- data.frame(
  regula = c("identyczne kopie", "pozycja_2: 99 na NA", "czas poza 0–120 na NA"),
  jednostka = c("wiersze", "komórki", "komórki"),
  liczba = c(liczba_duplikatow, liczba_99, liczba_czasow))
dziennik
