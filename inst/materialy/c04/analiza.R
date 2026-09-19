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
tabela_glowna <- data.frame(id = dane$id_odpowiedzi, indeks = dane$indeks, liczba_pozycji = dane$liczba_pozycji)
kanaly <- badaniaZI::odpowiedzi_wielokrotne(dane[paste0('kanal_', 1:4)])
wynik_glowny <- data.frame(N_indeks = sum(!is.na(dane$indeks)), srednia = mean(dane$indeks, na.rm = TRUE), SD = sd(dane$indeks, na.rm = TRUE))
wynik_klasyczny <- head(tabela_glowna, 8)
wynik_permutacyjny <- data.frame(status = 'najpierw jako\u015B\u0107 pomiaru; test zwi\u0105zku na C08')
efekt <- kanaly
wykres_glowny <- ggplot2::ggplot(dane, ggplot2::aes(x = indeks)) + ggplot2::geom_histogram(binwidth = .25, fill = '#009E73', colour = 'white') + ggplot2::labs(x = 'Indeks samooceny [1\u20135]', y = 'Liczba os\u00F3b', title = 'Samoocena po rekodacji pozycji 3') + badaniaZI::theme_zi()
