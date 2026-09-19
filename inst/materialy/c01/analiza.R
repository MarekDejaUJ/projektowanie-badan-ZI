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
tabela_glowna <- head(dane[c('id_odpowiedzi', 'grupa', 'czas_wyszukiwania', 'powodzenie')], 6)
wynik_glowny <- data.frame(N_surowe = nrow(dane_surowe), N_po = nrow(dane), kolumny = ncol(dane))
wynik_klasyczny <- tabela_glowna[2, , drop = FALSE]
wynik_permutacyjny <- data.frame(status = 'nie dotyczy pytania C01')
efekt <- data.frame(status = 'brak inferencji na C01')
wykres_glowny <- ggplot2::ggplot(head(dane, 20), ggplot2::aes(x = seq_len(20), y = czas_wyszukiwania)) + ggplot2::geom_point(size = 2, colour = '#0072B2') + ggplot2::labs(x = 'Kolejne rekordy', y = 'Czas [min]', title = 'Pierwsze rekordy zadania') + badaniaZI::theme_zi()
