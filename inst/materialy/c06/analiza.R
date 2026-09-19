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
x <- dane[complete.cases(dane[c('grupa', 'indeks')]), c('grupa', 'indeks')]; x$grupa <- factor(x$grupa)
test_t <- t.test(indeks ~ grupa, data = x); srednie <- tapply(x$indeks, x$grupa, mean); ns <- table(x$grupa)
roznica <- unname(srednie[2] - srednie[1])
perm <- replicate(B, unname(diff(tapply(x$indeks, sample(x$grupa), mean))))
p_perm <- (1 + sum(abs(perm) >= abs(roznica))) / (B + 1)
sdg <- tapply(x$indeks, x$grupa, sd); sp <- sqrt(((ns[1]-1)*sdg[1]^2 + (ns[2]-1)*sdg[2]^2)/(sum(ns)-2)); g <- (roznica/sp)*(1-3/(4*sum(ns)-9))
boot <- replicate(B, { z <- do.call(rbind, lapply(split(x, x$grupa), function(a) a[sample.int(nrow(a), replace=TRUE), ])); unname(diff(tapply(z$indeks, z$grupa, mean))) })
tabela_glowna <- data.frame(grupa = names(srednie), N = as.integer(ns), srednia = as.numeric(srednie), SD = as.numeric(sdg))
wynik_glowny <- tabela_glowna
wynik_klasyczny <- data.frame(t = unname(test_t$statistic), df = unname(test_t$parameter), p = test_t$p.value)
wynik_permutacyjny <- data.frame(roznica_2_minus_1 = roznica, p_perm = p_perm, B = B)
efekt <- data.frame(roznica = roznica, Hedges_g = unname(g), CI_dol = quantile(boot, .025), CI_gora = quantile(boot, .975))
wykres_glowny <- ggplot2::ggplot(x, ggplot2::aes(x = grupa, y = indeks, fill = grupa)) + ggplot2::geom_boxplot(width = .55, alpha = .75) + ggplot2::labs(x = 'Grupa', y = 'Indeks samooceny', title = 'Rozk\u0142ady w dw\u00F3ch grupach') + badaniaZI::theme_zi() + ggplot2::theme(legend.position = 'none')
