## ----przygotowanie------------------------------------------------------------
dane <- badaniaZI::przygotuj_ankiete(badaniaZI::dane_przykladowe())$dane
p <- dane[, paste0("pozycja_", 1:6)]
p$pozycja_3 <- badaniaZI::odwroc_pozycje(p$pozycja_3)
dane$indeks <- badaniaZI::indeks_ankiety(p, minimum = 5)
proba <- dane[!is.na(dane$indeks), ]
c(osoby_po_czyszczeniu = nrow(dane), wazny_indeks = nrow(proba))


## ----estymata-----------------------------------------------------------------
wartosci <- proba$indeks
n <- length(wartosci)
srednia <- mean(wartosci)
odchylenie <- stats::sd(wartosci)
se <- odchylenie / sqrt(n)
data.frame(n = n, srednia = srednia, sd = odchylenie, se = se)


## ----przedzial-t--------------------------------------------------------------
wynik_ci <- stats::t.test(wartosci, conf.level = .95)
granice_t <- unname(wynik_ci$conf.int)
granice_t
srednia + c(-1, 1) * stats::qt(.975, df = n - 1) * se


## ----bootstrap----------------------------------------------------------------
set.seed(2027)
B <- 1999
srednie_boot <- replicate(B, {
  numery <- sample.int(n, size = n, replace = TRUE)
  mean(wartosci[numery])
})
head(srednie_boot)
granice_boot <- unname(stats::quantile(srednie_boot, c(.025, .975)))
granice_boot


## ----tabela-------------------------------------------------------------------
estymacja <- data.frame(metoda = c("t", "bootstrap percentylowy"),
  n = n, srednia = srednia, dolna = c(granice_t[1], granice_boot[1]),
  gorna = c(granice_t[2], granice_boot[2]), poziom = .95,
  B = c(NA, B), ziarno = c(NA, 2027))
estymacja
bootstrap <- data.frame(replika = seq_len(B), srednia = srednie_boot)


## ----wykres-bootstrap, fig.cap="Syntetyczna ankieta S02; N i B w podtytule. Rozkład średnich bootstrapowych."----
wykres_boot <- ggplot2::ggplot(bootstrap, ggplot2::aes(x = srednia)) +
  ggplot2::geom_histogram(bins = 30, fill = badaniaZI::paleta_zi()["primary"],
    color = "white") +
  ggplot2::geom_vline(xintercept = srednia, linewidth = .7) +
  ggplot2::geom_vline(xintercept = granice_boot, linetype = "dashed",
    color = badaniaZI::paleta_zi()["accent"]) +
  ggplot2::labs(title = "Średnie z prób bootstrapowych",
    subtitle = paste("N =", n, "osób; B =", B, "replik"),
    x = "Średnia indeksu (punkty 1–5)", y = "Liczba replik") +
  badaniaZI::theme_zi()
wykres_boot


## ----permutacja---------------------------------------------------------------
grupy <- sort(unique(proba$grupa))
roznica <- function(g) {
  mean(wartosci[g == grupy[1]]) - mean(wartosci[g == grupy[2]])
}
obserwowana <- roznica(proba$grupa)
set.seed(2028)
permutacje <- replicate(B, roznica(sample(proba$grupa)))
p_perm <- (1 + sum(abs(permutacje) >= abs(obserwowana))) / (B + 1)
data.frame(pierwsza = grupy[1], druga = grupy[2],
  roznica = obserwowana, p_monte_carlo = p_perm)
