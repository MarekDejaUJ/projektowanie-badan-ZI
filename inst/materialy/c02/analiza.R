dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_', 1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
B <- 1999L
set.seed(202627)
formatuj_wynik <- function(x) {
  wynik <- x
  kolumny_p <- names(wynik)[grepl('^p($|_)', names(wynik))]
  for (nazwa in kolumny_p) if (is.numeric(wynik[[nazwa]]))
    wynik[[nazwa]] <- format.pval(wynik[[nazwa]], digits = 3, eps = 0.001)
  wynik
}
tabela_glowna <- przygotowane$dziennik
wynik_glowny <- data.frame(N_surowe = nrow(dane_surowe), N_analityczne = nrow(dane), braki_czasu = sum(is.na(dane$czas_wyszukiwania)))
wynik_klasyczny <- przygotowane$dziennik
wynik_permutacyjny <- data.frame(status = 'permutacja nast\u0119puje po czyszczeniu')
efekt <- data.frame(zmienione_elementy = sum(przygotowane$dziennik$liczba[2:4]))
wykres_glowny <- ggplot2::ggplot(dane, ggplot2::aes(x = czas_wyszukiwania)) + ggplot2::geom_histogram(bins = 18, fill = '#56B4E9', colour = 'white') + ggplot2::labs(x = 'Czas [min]', y = 'Liczba os\u00F3b', title = 'Czas po zastosowaniu regu\u0142') + badaniaZI::theme_zi()
