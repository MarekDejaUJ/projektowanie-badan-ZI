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
cfg <- badaniaZI::scenariusz('S02')
tabela_glowna <- data.frame(etap=c('pytanie 1','wynik','predyktor','metoda klasyczna','metoda permutacyjna','efekt'), wartosc=c('Czy grupy r\u00F3\u017Cni\u0105 si\u0119 indeksem?','indeks','grupa','Welch','tasowanie etykiet','r\u00F3\u017Cnica i Hedges g'))
wynik_glowny <- data.frame(scenariusz='S02', projekt=cfg$projekt_badania, druga_analiza=cfg$druga_analiza)
wynik_klasyczny <- data.frame(analiza=c('por\u00F3wnanie grup','tabela kategorii'), wynik=c('indeks','powodzenie'))
wynik_permutacyjny <- data.frame(analiza=c('por\u00F3wnanie grup','tabela kategorii'), procedura=c('tasowanie grup','Monte Carlo'))
efekt <- data.frame(analiza=c('por\u00F3wnanie grup','tabela kategorii'), miara=c('r\u00F3\u017Cnica + g','V Cramera'))
wykres_glowny <- ggplot2::ggplot(dane, ggplot2::aes(x=indeks, y=czas_wyszukiwania, colour=grupa)) + ggplot2::geom_point(alpha=.6) + ggplot2::labs(x='Indeks', y='Czas [min]', colour='Grupa', title='Mapa dw\u00F3ch pyta\u0144 projektu') + badaniaZI::theme_zi()
