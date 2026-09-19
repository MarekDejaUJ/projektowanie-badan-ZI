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
x <- dane[complete.cases(dane[c('grupa','indeks')]), ]; test_t <- t.test(indeks~grupa,data=x); sr <- tapply(x$indeks,x$grupa,mean); roznica <- unname(diff(sr))
perm <- replicate(B, unname(diff(tapply(x$indeks, sample(x$grupa), mean)))); p_perm <- (1+sum(abs(perm)>=abs(roznica)))/(B+1)
tabela_glowna <- data.frame(element=c('dane','parametry','wyniki','raport','wersja'), kontrola=c('manifest i hash','jawne warto\u015Bci','CSV/PNG','liczby zgodne','zdalne SHA'))
wynik_glowny <- data.frame(N=nrow(x), grupa_1=names(sr)[1], srednia_1=sr[1], grupa_2=names(sr)[2], srednia_2=sr[2])
wynik_klasyczny <- data.frame(roznica=roznica,t=unname(test_t$statistic),df=unname(test_t$parameter),p=test_t$p.value,CI_dol=test_t$conf.int[1],CI_gora=test_t$conf.int[2])
wynik_permutacyjny <- data.frame(roznica=roznica,p_perm=p_perm,B=B)
efekt <- data.frame(jednostka='punkty indeksu', ograniczenie='dane syntetyczne; grupy obserwacyjne')
wykres_glowny <- ggplot2::ggplot(x, ggplot2::aes(x=grupa,y=indeks,fill=grupa)) + ggplot2::geom_boxplot(alpha=.75) + ggplot2::labs(x='Grupa',y='Indeks',title='Wykres do ko\u0144cowego raportu') + badaniaZI::theme_zi() + ggplot2::theme(legend.position='none')
