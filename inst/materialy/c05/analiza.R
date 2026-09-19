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
x <- dane$indeks[is.finite(dane$indeks)]; mu0 <- 3
test_t <- t.test(x, mu = mu0)
roznica <- mean(x) - mu0; d <- roznica / sd(x)
boot <- replicate(B, mean(sample(x, replace = TRUE)))
perm <- replicate(B, mean(sample(c(-1, 1), length(x), replace = TRUE) * (x - mu0)))
p_perm <- (1 + sum(abs(perm) >= abs(roznica))) / (B + 1)
tabela_glowna <- data.frame(N = length(x), srednia = mean(x), SE = sd(x)/sqrt(length(x)), CI_dol = test_t$conf.int[1], CI_gora = test_t$conf.int[2])
wynik_glowny <- tabela_glowna
wynik_klasyczny <- data.frame(t = unname(test_t$statistic), df = unname(test_t$parameter), p = test_t$p.value)
wynik_permutacyjny <- data.frame(roznica = roznica, p_perm = p_perm, B = B)
efekt <- data.frame(roznica_punktow = roznica, d = d, boot_CI_dol = quantile(boot, .025), boot_CI_gora = quantile(boot, .975))
wykres_glowny <- ggplot2::ggplot(data.frame(stat = perm), ggplot2::aes(x = stat)) + ggplot2::geom_histogram(bins = 30, fill = '#009E73', colour = 'white') + ggplot2::geom_vline(xintercept = c(-abs(roznica), abs(roznica)), colour = '#D55E00', linewidth = 1) + ggplot2::labs(x = 'R\u00F3\u017Cnica \u015Bredniej przy H0', y = 'Liczba losowa\u0144', title = 'Rozk\u0142ad zerowy po zmianie znak\u00F3w') + badaniaZI::theme_zi()
