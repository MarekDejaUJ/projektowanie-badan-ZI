# W03: przykłady liczbowe z jawnych statystyk, modele rozkładów i symulacje
# dydaktyczne. Populacja symulacji jest modelem o znanej średniej 10 minut.

liczba <- function(x, cyfry = 2L) sub("^-", "−", formatC(x, format = "f", digits = cyfry, decimal.mark = ","))
kolory <- badaniaZI::paleta_zi()

# Wynik testu t jednej próby i dwa przedziały 95% z podanych statystyk opisowych.
wynik_z_opisu <- function(n, srednia, SD, odniesienie, jednostka) {
  stopifnot(n >= 2, SD > 0)
  SE <- SD / sqrt(n)
  t <- (srednia - odniesienie) / SE
  margines <- qt(.975, df = n - 1) * SE
  data.frame(N = n, srednia = srednia, SD = SD, SE = SE,
             odniesienie = odniesienie, roznica = srednia - odniesienie,
             CI_srednia_dol = srednia - margines, CI_srednia_gora = srednia + margines,
             CI_roznica_dol = srednia - odniesienie - margines,
             CI_roznica_gora = srednia - odniesienie + margines,
             t = t, df = n - 1, p = 2 * pt(-abs(t), df = n - 1),
             d = (srednia - odniesienie) / SD, jednostka = jednostka)
}
przyklad_indeks <- wynik_z_opisu(100, 3.24, .8, 3, 'punkty')
przyklad_czas <- wynik_z_opisu(16, 11.2, 6, 10, 'minuty')
# Tabela do druku: polskie nazwy wielkości i wartości z przecinkiem dziesiętnym.
przeloz_wynik <- function(w) {
  data.frame(
    `Wielkość` = c("Liczba osób N", "Średnia", "SD osób", "SE średniej", "Wartość odniesienia",
                   "Różnica od odniesienia", "95% CI średniej", "95% CI różnicy",
                   "Statystyka t", "Stopnie swobody df", "Wartość p (dwustronna)", "Standaryzowana różnica d"),
    `Wartość` = c(w$N, liczba(w$srednia), liczba(w$SD), liczba(w$SE, 3L), liczba(w$odniesienie),
                  liczba(w$roznica), paste0("[", liczba(w$CI_srednia_dol, 3L), "; ", liczba(w$CI_srednia_gora, 3L), "]"),
                  paste0("[", liczba(w$CI_roznica_dol, 3L), "; ", liczba(w$CI_roznica_gora, 3L), "]"),
                  liczba(w$t), w$df, liczba(w$p, 4L), liczba(w$d)),
    Jednostka = c("osoby", rep(w$jednostka, 7), "bez jednostki", "bez jednostki", "prawdopodobieństwo", "SD osób"),
    check.names = FALSE)
}
se_i_n <- data.frame(`Liczba osób N` = c(25, 100, 400), `SD [min]` = liczba(4, 1L),
                     `SE = SD/√N [min]` = liczba(4 / sqrt(c(25, 100, 400)), 1L), check.names = FALSE)

# Model dwumianowy: trzy niezależne próby, prawdopodobieństwo sukcesu 0,5.
dwumian <- data.frame(k = 0:3, p = dbinom(0:3, 3, .5), F = pbinom(0:3, 3, .5))
dwumian_do_druku <- data.frame(`Liczba sukcesów k` = dwumian$k, `P(K = k)` = liczba(dwumian$p, 3L),
                               `F(k) = P(K ≤ k)` = liczba(dwumian$F, 3L), check.names = FALSE)
wykres_dwumianu <- badaniaZI::wykres_rozklad_dyskretny(dwumian$k, dwumian$p,
  tytul = "Model dwumianowy: trzy próby, π = 0,5")

# Model wykładniczy czasu z intensywnością 0,1 na minutę (średnia 10 minut).
wykladniczy <- data.frame(
  `Wielkość` = c("Gęstość f(5) [1/min]", "F(5) = P(X ≤ 5)", "P(5 < X ≤ 10) = F(10) − F(5)", "Mediana: kwantyl rzędu 0,5 [min]"),
  `Wartość` = liczba(c(dexp(5, .1), pexp(5, .1), pexp(10, .1) - pexp(5, .1), qexp(.5, .1)), 3L),
  `Wywołanie R` = c("dexp(5, 0.1)", "pexp(5, 0.1)", "pexp(10, 0.1) - pexp(5, 0.1)", "qexp(0.5, 0.1)"),
  check.names = FALSE)
wykres_wykladniczy <- badaniaZI::wykres_gestosc_pole(function(x) dexp(x, .1), 0, 40, a = 5, b = 10,
  linie = qexp(.5, .1), etykiety_linii = "mediana 6,93", os = "Czas x [min]", os_y = "Gęstość f(x) [1/min]",
  tytul = "Model wykładniczy czasu, λ = 0,1 na minutę")

# Rozkład normalny standardowy: środkowe 95% pola między −1,96 a 1,96.
wykres_normalny <- badaniaZI::wykres_gestosc_pole(dnorm, -4, 4, a = qnorm(.025), b = qnorm(.975),
  linie = c(qnorm(.025), qnorm(.975)), etykiety_linii = c("−1,96", "1,96"), os = "Wartość z",
  os_y = "Gęstość φ(z)", tytul = "Rozkład normalny standardowy N(0, 1)")
prefiksy <- data.frame(
  `Wywołanie R` = c("dexp(5, 0.1)", "pexp(5, 0.1)", "qexp(0.5, 0.1)", "pnorm(1.96)", "qnorm(0.975)",
                    "qt(0.975, 15)", "pchisq(3.84, 1)"),
  Prefiks = c("d: gęstość", "p: dystrybuanta", "q: kwantyl", "p: dystrybuanta", "q: kwantyl", "q: kwantyl",
              "p: dystrybuanta"),
  Wynik = liczba(c(dexp(5, .1), pexp(5, .1), qexp(.5, .1), pnorm(1.96), qnorm(.975), qt(.975, 15),
                   pchisq(3.84, 1)), 3L),
  check.names = FALSE)

# Symulacja: populacja log-normalna o średniej 10 i SD 4 minuty; 2000 prób po 40 osób.
set.seed(202627)
sdlog <- sqrt(log(1 + (4 / 10)^2))
meanlog <- log(10) - sdlog^2 / 2
losuj_czasy <- function(n) rlnorm(n, meanlog = meanlog, sdlog = sdlog)
jedna_proba <- losuj_czasy(40)
powtorzenia <- t(replicate(2000, {
  x <- losuj_czasy(40)
  SE <- sd(x) / sqrt(length(x))
  c(srednia = mean(x), SD = sd(x), SE = SE,
    dol = mean(x) - qt(.975, 39) * SE, gora = mean(x) + qt(.975, 39) * SE)
}))
powtorzenia <- as.data.frame(powtorzenia)
powtorzenia$obejmuje <- powtorzenia$dol <= 10 & powtorzenia$gora >= 10
symulacja <- list(srednia_estymat = mean(powtorzenia$srednia), sd_estymat = sd(powtorzenia$srednia),
                  pokrycie = 100 * mean(powtorzenia$obejmuje), pominiete40 = sum(!head(powtorzenia$obejmuje, 40)),
                  min_sr = min(powtorzenia$srednia), max_sr = max(powtorzenia$srednia))
podsumowanie_symulacji <- data.frame(
  `Wielkość` = c("Liczba prób", "Osoby w próbie", "Parametr μ [min]", "Średnia 2000 estymat [min]",
                 "SD 2000 estymat [min]", "SE z modelu: 4/√40 [min]", "Przedziały obejmujące μ [%]"),
  `Wartość` = c("2000", "40", "10", liczba(symulacja$srednia_estymat), liczba(symulacja$sd_estymat, 3L),
                liczba(4 / sqrt(40), 3L), liczba(symulacja$pokrycie)),
  check.names = FALSE)
wykres_rozkladow <- badaniaZI::wykres_histogram_panele(
  list(`Czasy 40 osób jednej próby` = jedna_proba, `Średnie 2000 prób po 40 osób` = powtorzenia$srednia),
  c(1, 1), poczatek = 0, os = "Minuty", odniesienie = 10, etykieta_odniesienia = "μ =",
  os_y = "Liczba osób albo prób", tytul = "Rozrzut osób i rozrzut średnich")

# 40 pierwszych przedziałów: typ linii i kształt punktu odróżniają przedziały pomijające μ.
odcinki <- head(powtorzenia, 40)
odcinki$proba <- seq_len(nrow(odcinki))
odcinki$status <- factor(ifelse(odcinki$obejmuje, "obejmuje μ", "pomija μ"), levels = c("obejmuje μ", "pomija μ"))
wykres_przedzialow <- ggplot2::ggplot(odcinki, ggplot2::aes(x = srednia, y = proba, linetype = status, shape = status)) +
  ggplot2::geom_segment(ggplot2::aes(x = dol, xend = gora, yend = proba), colour = kolory[['primary']]) +
  ggplot2::geom_point(size = 1.8, colour = kolory[['primary']], fill = 'white') +
  ggplot2::geom_vline(xintercept = 10, linetype = 'dotted', colour = kolory[['dark']]) +
  ggplot2::scale_linetype_manual(values = c('obejmuje μ' = 'solid', 'pomija μ' = 'dashed'), name = NULL) +
  ggplot2::scale_shape_manual(values = c('obejmuje μ' = 16, 'pomija μ' = 21), name = NULL) +
  ggplot2::labs(x = 'Średni czas i przedział 95% [min]', y = 'Numer próby',
                title = '40 przedziałów z powtarzanych prób') +
  badaniaZI::theme_zi()

# Rozkład t wobec rozkładu normalnego i ogony testu dla t = 3.
wykres_t_z <- badaniaZI::wykres_t_z(c(3, 15, 99), tytul = "Rozkład normalny standardowy i rozkłady t")
wykres_t <- badaniaZI::wykres_ogony(3, "t", 99, os = "Statystyka t przy H0 (df = 99)",
  tytul = "Dwustronne p: oba ogony dla |t| ≥ 3")
wykres_bledy <- badaniaZI::wykres_bledy_testu(3, 99, tytul = "Błędy decyzji przy d = 0,30 i n = 100")
wykres_mocy <- badaniaZI::wykres_moc(c(.2, .3, .5), seq(5, 250, 5), tytul = "Moc testu t jednej próby")
wykres_wielu_testow <- badaniaZI::wykres_wielokrotne_testy(1:20, tytul = "Seria niezależnych testów")
wykres_estymatorow <- badaniaZI::wykres_rozklady_estymatora(c(10, 10, 11.5, 11.5), c(.63, 1.6, .63, 1.6),
  c("Nieobciążony, precyzyjny", "Nieobciążony, nieprecyzyjny", "Obciążony, precyzyjny", "Obciążony, nieprecyzyjny"),
  parametr = 10, os = "Estymata średniego czasu [min]", tytul = "Obciążenie i precyzja estymatora")

# Nowe symulacje dopisane po symulacji głównej (ziarno 2026 dla odtwarzalności).
set.seed(2026)
srednie_ctg <- lapply(c(2, 5, 40), function(n) replicate(2000, mean(losuj_czasy(n))))
names(srednie_ctg) <- paste0("Średnie z n = ", c(2, 5, 40))
wykres_ctg <- badaniaZI::wykres_histogram_panele(srednie_ctg, 0.5, poczatek = 0, os = "Średni czas [min]",
  odniesienie = 10, etykieta_odniesienia = "μ =", os_y = "Liczba prób",
  tytul = "Rozkład średniej przy rosnącym n")
ctg_opis <- vapply(srednie_ctg, function(x) c(min = min(x), max = max(x), sd = sd(x),
  g1 = mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5), numeric(4))

# Rozkład empiryczny i estymowany jednej próby 40 osób.
jedna_opis <- list(n = 40, srednia = mean(jedna_proba), sd = sd(jedna_proba), min = min(jedna_proba),
                   max = max(jedna_proba), meanlog = mean(log(jedna_proba)), sdlog = sd(log(jedna_proba)))
porownanie_F <- data.frame(c = c(5, 10, 15, 20),
  schodki = ecdf(jedna_proba)(c(5, 10, 15, 20)),
  normalny = pnorm(c(5, 10, 15, 20), jedna_opis$srednia, jedna_opis$sd),
  lognormalny = plnorm(c(5, 10, 15, 20), jedna_opis$meanlog, jedna_opis$sdlog))
porownanie_F_do_druku <- data.frame(`Próg c [min]` = porownanie_F$c,
  `Dystrybuanta empiryczna` = liczba(porownanie_F$schodki, 3L),
  `Model normalny` = liczba(porownanie_F$normalny, 3L),
  `Model log-normalny` = liczba(porownanie_F$lognormalny, 3L), check.names = FALSE)
wykres_dystrybuant_modeli <- badaniaZI::wykres_dystrybuanta(jedna_proba, dopasuj = c("normalny", "lognormalny"),
  os = "Czas [min]", tytul = "Rozkład empiryczny i dwa modele: dystrybuanty")
wykres_gestosci_modeli <- badaniaZI::wykres_histogram(jedna_proba, 2, skala = "gestosc",
  dopasuj = c("normalny", "lognormalny"), os = "Czas [min]", tytul = "Rozkład empiryczny i dwa modele: gęstości")

# Trzy przedziały jednej próby: CI średniej, predykcja i środkowe 95% obserwacji.
tq <- qt(.975, 39)
trzy <- data.frame(
  etykieta = c("95% CI średniej", "Środkowe 95% czasów", "95% przedział predykcji"),
  estymata = jedna_opis$srednia,
  dolna = c(jedna_opis$srednia - tq * jedna_opis$sd / sqrt(40), quantile(jedna_proba, .025),
            jedna_opis$srednia - tq * jedna_opis$sd * sqrt(1 + 1 / 40)),
  gorna = c(jedna_opis$srednia + tq * jedna_opis$sd / sqrt(40), quantile(jedna_proba, .975),
            jedna_opis$srednia + tq * jedna_opis$sd * sqrt(1 + 1 / 40)))
wykres_trzech_przedzialow <- badaniaZI::wykres_przedzialy(trzy$etykieta, trzy$estymata, trzy$dolna, trzy$gorna,
  os = "Czas [min]", cyfry = 2L, tytul = "Trzy przedziały jednej próby 40 osób")

# Bootstrap średniej i zmiana znaków odchyleń od 10 minut dla jednej próby.
B <- 1999L
bootstrap <- replicate(B, mean(sample(jedna_proba, replace = TRUE)))
odchylenia <- jedna_proba - 10
T_obs <- mean(odchylenia)
zerowy <- replicate(B, mean(sample(c(-1, 1), 40, replace = TRUE) * odchylenia))
skrajne <- sum(abs(zerowy) >= abs(T_obs) - 1e-12)
wykres_bootstrapu <- badaniaZI::wykres_bootstrap(bootstrap, 0.2, os = "Średnia bootstrapowa [min]",
  tytul = "Rozkład bootstrapowy średniej")
wykres_zerowy <- badaniaZI::wykres_rozklad_zerowy(zerowy, T_obs, 0.25, os = "Średnia odchyleń od 10 minut",
  tytul = "Rozkład zerowy zmian znaków")
