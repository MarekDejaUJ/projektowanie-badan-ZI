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
x <- dane[complete.cases(dane[c('indeks','czas_wyszukiwania')]), c('indeks','czas_wyszukiwania')]
cor_s <- suppressWarnings(cor.test(x$indeks, x$czas_wyszukiwania, method='spearman', exact=FALSE)); rho <- unname(cor_s$estimate)
boot <- replicate(B, { i <- sample.int(nrow(x), replace=TRUE); suppressWarnings(cor(x$indeks[i], x$czas_wyszukiwania[i], method='spearman')) })
perm <- replicate(B, suppressWarnings(cor(x$indeks, sample(x$czas_wyszukiwania), method='spearman')))
p_perm <- (1 + sum(abs(perm) >= abs(rho), na.rm=TRUE))/(B+1)
model <- lm(czas_wyszukiwania ~ indeks, data=x); sm <- summary(model); cf <- coef(sm)
tabela_glowna <- head(x, 8)
wynik_glowny <- data.frame(N_par = nrow(x), rho = rho)
wynik_klasyczny <- data.frame(rho = rho, p = cor_s$p.value, CI_dol = quantile(boot,.025,na.rm=TRUE), CI_gora = quantile(boot,.975,na.rm=TRUE))
wynik_permutacyjny <- data.frame(rho = rho, p_perm = p_perm, B = B)
efekt <- data.frame(nachylenie = cf[2,1], SE = cf[2,2], t = cf[2,3], p = cf[2,4], R2 = sm$r.squared)
wykres_glowny <- ggplot2::ggplot(x, ggplot2::aes(x = indeks, y = czas_wyszukiwania)) + ggplot2::geom_point(alpha=.65, colour='#0072B2') + ggplot2::geom_smooth(method='lm', se=TRUE, colour='#D55E00') + ggplot2::labs(x='Indeks samooceny [1\u20135]', y='Obserwowany czas [min]', title='Samoocena i wykonanie') + badaniaZI::theme_zi()
