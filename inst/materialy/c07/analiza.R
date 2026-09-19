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
tab <- table(dane$grupa, dane$powodzenie, useNA = 'no')
chi <- suppressWarnings(chisq.test(tab, correct = FALSE)); mc <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = B))
v <- sqrt(unname(chi$statistic)/(sum(tab)*min(nrow(tab)-1,ncol(tab)-1)))
bootv <- replicate(B, { z <- dane[sample.int(nrow(dane), replace=TRUE), ]; tb <- table(factor(z$grupa, levels=rownames(tab)), factor(z$powodzenie, levels=colnames(tab))); ch <- suppressWarnings(chisq.test(tb, correct=FALSE)); sqrt(unname(ch$statistic)/(sum(tb)*min(nrow(tb)-1,ncol(tb)-1))) })
tabela_glowna <- as.data.frame.matrix(tab); tabela_glowna$grupa <- rownames(tabela_glowna); tabela_glowna$odsetek_powodzenia <- 100*prop.table(tab,1)[, '1']
wynik_glowny <- tabela_glowna
wynik_klasyczny <- data.frame(chi2 = unname(chi$statistic), df = unname(chi$parameter), p = chi$p.value, min_E = min(chi$expected))
wynik_permutacyjny <- data.frame(chi2 = unname(mc$statistic), p_Monte_Carlo = mc$p.value, B = B)
efekt <- data.frame(V_Cramera = v, CI_dol = quantile(bootv[is.finite(bootv)], .025), CI_gora = quantile(bootv[is.finite(bootv)], .975))
plot_data <- as.data.frame(prop.table(tab, 1)); names(plot_data) <- c('grupa','powodzenie','proporcja')
wykres_glowny <- ggplot2::ggplot(plot_data, ggplot2::aes(x = grupa, y = proporcja, fill = powodzenie)) + ggplot2::geom_col() + ggplot2::scale_y_continuous(labels = function(x) paste0(round(100*x), '%')) + ggplot2::labs(x = 'Grupa', y = 'Odsetek w grupie', fill = 'Powodzenie', title = 'Obserwowany wynik zadania') + badaniaZI::theme_zi()
