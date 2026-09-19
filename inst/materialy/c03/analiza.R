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
x <- dane$czas_wyszukiwania[is.finite(dane$czas_wyszukiwania)]
tabela_glowna <- data.frame(N = length(x), srednia = mean(x), mediana = median(x), SD = sd(x), IQR = IQR(x), min = min(x), max = max(x))
wynik_glowny <- tabela_glowna
wynik_klasyczny <- tabela_glowna
wynik_permutacyjny <- data.frame(status = 'bez testu hipotezy na C03')
efekt <- data.frame(roznica_srednia_mediana = mean(x) - median(x))
wykres_glowny <- ggplot2::ggplot(dane, ggplot2::aes(x = czas_wyszukiwania)) + ggplot2::geom_histogram(bins = 20, fill = '#56B4E9', colour = 'white') + ggplot2::geom_vline(xintercept = median(x), colour = '#D55E00', linewidth = 1) + ggplot2::labs(x = 'Obserwowany czas [min]', y = 'Liczba os\u00F3b', title = 'Rozk\u0142ad czasu; linia oznacza median\u0119') + badaniaZI::theme_zi()
