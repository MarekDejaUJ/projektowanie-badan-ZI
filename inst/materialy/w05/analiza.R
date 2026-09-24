# W05: sześć hipotetycznych par do odczytu korelacji i regresji, wyniki
# pomocnicze dla zbioru S02 oraz galeria rozszerzeń z osobnymi danymi.

liczba <- function(x, cyfry = 2L) sub("^-", "−", formatC(x, format = "f", digits = cyfry, decimal.mark = ","))
wartosc_p <- function(p) if (p < 0.0001) "< 0,0001" else liczba(p, 4L)
przedzial <- function(a, b, cyfry = 3L) paste0("[", liczba(a, cyfry), "; ", liczba(b, cyfry), "]")
kolory <- badaniaZI::paleta_zi()

# Sześć osób: x — deklarowana użyteczność katalogu 1–5, y — czas zadania w minutach.
mini <- data.frame(osoba = letters[1:6], x = c(1, 2, 2, 3, 4, 5), y = c(15, 12, 14, 10, 9, 6))
mini$dx <- mini$x - mean(mini$x)
mini$dy <- mini$y - mean(mini$y)
mini$iloczyn <- mini$dx * mini$dy
mini$ranga_x <- rank(mini$x)
mini$ranga_y <- rank(mini$y)
pary_do_druku <- data.frame(Osoba = mini$osoba, `x: ocena 1–5` = mini$x, `y: czas [min]` = mini$y, check.names = FALSE)
odchylenia_do_druku <- data.frame(Osoba = c(mini$osoba, "Suma"),
                                  `x − 2,83` = c(liczba(mini$dx), liczba(0)),
                                  `y − 11` = c(liczba(mini$dy, 0L), liczba(sum(mini$dy), 0L)),
                                  Iloczyn = c(liczba(mini$iloczyn), liczba(sum(mini$iloczyn))), check.names = FALSE)
rangi_do_druku <- data.frame(Osoba = mini$osoba, x = mini$x, `Ranga x` = liczba(mini$ranga_x, 1L), y = mini$y,
                             `Ranga y` = liczba(mini$ranga_y, 0L),
                             `D = różnica rang` = liczba(mini$ranga_x - mini$ranga_y, 1L), check.names = FALSE)

test <- cor.test(mini$x, mini$y)
spearman <- suppressWarnings(cor.test(mini$x, mini$y, method = "spearman", exact = FALSE))
z_r <- atanh(unname(test$estimate))
korelacja <- data.frame(
  `Wielkość` = c("Liczba par N", "Pearson r", "Statystyka t", "Stopnie swobody", "Wartość p (dwustronna)",
                 "95% CI ρ_P (transformacja Fishera)", "Spearman r_S", "Statystyka S = ΣD²"),
  `Wartość` = c(6, liczba(test$estimate, 3L), liczba(test$statistic, 3L), 4, wartosc_p(test$p.value),
                przedzial(test$conf.int[1], test$conf.int[2]), liczba(spearman$estimate, 3L),
                liczba(spearman$statistic, 1L)), check.names = FALSE)

model <- lm(y ~ x, mini)
sm <- summary(model)
cf <- coef(sm)
ci <- confint(model)
regresja <- data.frame(Parametr = c("Wyraz wolny b0 [min]", "Nachylenie b1 [min na punkt]"),
                       Estymata = liczba(cf[, 1], 3L), SE = liczba(cf[, 2], 3L), t = liczba(cf[, 3], 3L),
                       df = 4, p = vapply(cf[, 4], wartosc_p, character(1)),
                       `95% CI` = mapply(przedzial, ci[, 1], ci[, 2], MoreArgs = list(cyfry = 2L)), check.names = FALSE)
dopasowanie <- data.frame(`Wielkość` = c("R²", "SD reszt s_e [min]", "SSE (suma kwadratów reszt)", "SST (suma kwadratów od średniej)"),
                          `Wartość` = c(liczba(sm$r.squared, 3L), liczba(sm$sigma, 3L), liczba(sum(resid(model)^2)),
                                        liczba(sum(mini$dy^2), 0L)), check.names = FALSE)
diagnostyka <- data.frame(Osoba = mini$osoba, x = mini$x, y = mini$y,
                          `Przewidywane ŷ` = liczba(fitted(model)), `Reszta e` = liczba(resid(model)),
                          `Dźwignia h` = liczba(hatvalues(model), 3L), `Odległość Cooka` = liczba(cooks.distance(model), 3L),
                          check.names = FALSE)

# Pełne wyliczenie 6! = 720 ustawień czasów względem ocen.
permutacje <- function(x) {
  if (length(x) == 1L) return(matrix(x, nrow = 1))
  do.call(rbind, lapply(seq_along(x), function(i) cbind(x[i], permutacje(x[-i]))))
}
indeksy <- permutacje(1:6)
zerowy <- apply(indeksy, 1, function(i) cor(mini$x, mini$y[i]))
k <- sum(abs(zerowy) >= abs(unname(test$estimate)) - 1e-12)

# Bootstrap par: losowanie całych wierszy (x, y) z powtórzeniami.
set.seed(2026)
bootstrap_r <- replicate(1999, {
  i <- sample(6, replace = TRUE)
  if (length(unique(mini$x[i])) < 2 || length(unique(mini$y[i])) < 2) NA_real_ else cor(mini$x[i], mini$y[i])
})
boot_nieokreslone <- sum(is.na(bootstrap_r))

# Prognoza przy x = 3: średnia warunkowa i nowa osoba.
pred <- do.call(rbind, lapply(c("confidence", "prediction"), function(typ) {
  z <- predict(model, data.frame(x = 3), interval = typ)
  data.frame(`Przedział` = if (typ == "confidence") "Średnia warunkowa przy x = 3" else "Nowa osoba z x = 3",
             `Prognoza [min]` = liczba(z[, 1]), `95% przedział [min]` = przedzial(z[, 2], z[, 3], 2L), check.names = FALSE)
}))

# Kalibracja samooceny: przewidywany i uzyskany procent poprawnych odpowiedzi.
kalibracja <- data.frame(wynik = c(20, 30, 40, 50, 60), samoocena = c(40, 50, 60, 70, 80))
kalibracja_do_druku <- data.frame(Osoba = 1:5, `Wynik [%]` = kalibracja$wynik, `Samoocena [%]` = kalibracja$samoocena,
                                  `Błąd samooceny [pp]` = kalibracja$samoocena - kalibracja$wynik, check.names = FALSE)

# Zbiór S02: indeks deklarowanej użyteczności a czas (liczby do porównania z ćwiczeniem C08).
dane_s02 <- badaniaZI::przygotuj_ankiete(badaniaZI::dane_przykladowe())$dane
poz <- dane_s02[paste0("pozycja_", 1:6)]
poz$pozycja_3 <- badaniaZI::odwroc_pozycje(poz$pozycja_3)
dane_s02$indeks <- badaniaZI::indeks_ankiety(poz, minimum = 5L)
pary_s02 <- dane_s02[complete.cases(dane_s02[c("indeks", "czas_wyszukiwania")]), ]
s02 <- list(n = nrow(pary_s02), r = cor(pary_s02$indeks, pary_s02$czas_wyszukiwania),
            r_s = cor(pary_s02$indeks, pary_s02$czas_wyszukiwania, method = "spearman"))

# Wykresy głównej części.
wykres <- badaniaZI::wykres_pasma_regresji(mini$x, mini$y, x0 = 3, os_x = "Deklarowana użyteczność katalogu [1–5]",
  os_y = "Czas [min]", etykiety = mini$osoba, tytul = "Sześć par do nauki odczytu")
ksztalty <- rbind(data.frame(typ = "Monotoniczny: y = 30/x", x = 1:9, y = 30 / (1:9)),
                  data.frame(typ = "Kształt U: y = (x − 5)² + 2", x = 1:9, y = (1:9 - 5)^2 + 2))
wykres_ksztaltow <- ggplot2::ggplot(ksztalty, ggplot2::aes(x, y)) +
  ggplot2::geom_line(colour = kolory[["secondary"]]) +
  ggplot2::geom_point(colour = kolory[["primary"]], size = 2) +
  ggplot2::facet_wrap(~typ, scales = "free_y") +
  ggplot2::labs(x = "x [jednostki umowne]", y = "Czas [min]", title = "Zależność monotoniczna i zależność w kształcie U") +
  badaniaZI::theme_zi()
wykres_anscombe <- badaniaZI::wykres_anscombe(tytul = "Kwartet Anscombe'a: ta sama korelacja, cztery układy")
wykres_galerii_r <- badaniaZI::wykres_galeria_r(c(0, -0.3, -0.6, -0.9), tytul = "Cztery wartości r")
wykres_perm <- badaniaZI::wykres_rozklad_zerowy(zerowy, unname(test$estimate), 0.1, poczatek = -1.05, dokladny = TRUE,
  os = "r po zmianie parowania", tytul = "Wszystkie 720 ustawień czasów")
wykres_bootstrapu <- badaniaZI::wykres_bootstrap(bootstrap_r, 0.02, os = "r w próbie bootstrapowej",
  tytul = "Bootstrap par: rozkład r")
reszty_demo <- list(
  `Sześć par` = mini[c("x", "y")],
  `Krzywizna` = data.frame(x = 1:12, y = 2 + 0.12 * (1:12)^2 + c(.2, -.1, .1, -.2, .1, 0, -.1, .2, -.1, .1, -.2, .1)),
  `Lejek` = data.frame(x = 1:12, y = 5 + 1:12 + (1:12) * c(.35, -.3, .25, -.35, .3, -.25, .35, -.3, .25, -.35, .3, -.25)))
wykres_reszt <- badaniaZI::wykres_reszty(reszty_demo, tytul = "Reszty wobec wartości przewidywanych")
wykres_qq_reszt <- badaniaZI::wykres_kwantylowy(list(`Reszty sześciu par` = resid(model)), os = "Reszta [min]",
  tytul = "Wykres kwantylowy reszt")
wykres_wplywu <- badaniaZI::wykres_wplyw(mini$x, mini$y, c(9, 14), os_x = "Deklarowana użyteczność (x = 9 poza skalą)",
  os_y = "Czas [min]", tytul = "Jeden punkt o dużej dźwigni")
wykres_kalibracji <- badaniaZI::wykres_kalibracja(kalibracja$samoocena, kalibracja$wynik,
  tytul = "Kalibracja samooceny pięciu osób")

# Galeria rozszerzeń: każdy przykład ma osobne, hipotetyczne dane.
model_nieliniowy <- function() {
  d <- data.frame(proby = 0:8)
  d$czas <- 3 + 12 * exp(-.4 * d$proby) + c(0, .3, -.4, .2, -.2, .25, -.1, .15, -.2)
  m <- stats::nls(czas ~ a + b * exp(-c * proby), data = d, start = list(a = 3, b = 12, c = .4))
  siatka <- data.frame(proby = seq(0, 8, length.out = 101))
  siatka$czas <- predict(m, newdata = siatka)
  przyklady <- data.frame(proby = c(0, 1, 2, 4, 8))
  przyklady$czas <- predict(m, newdata = przyklady)
  wykres <- ggplot2::ggplot(d, ggplot2::aes(proby, czas)) +
    ggplot2::geom_line(data = siatka, colour = kolory[["accent"]]) +
    ggplot2::geom_point(colour = kolory[["primary"]], size = 2) +
    ggplot2::labs(x = "Wcześniejsze próby [liczba]", y = "Czas [min]", title = "Model nieliniowy: odczyt krzywej",
                  subtitle = "Dziewięć hipotetycznych osób, po jednym pomiarze") +
    badaniaZI::theme_zi()
  list(parametry = coef(m), przyklady = przyklady, wykres = wykres)
}
nieliniowy <- model_nieliniowy()
nieliniowy_do_druku <- data.frame(`Wcześniejsze próby` = nieliniowy$przyklady$proby,
                                  `Średni czas [min]` = liczba(nieliniowy$przyklady$czas), check.names = FALSE)

wiolina_dane <- data.frame(grupa = rep(c("Nowi (N = 12)", "Doświadczeni (N = 12)"), each = 12),
                           czas = c(2, 3, 4, 5, 6, 6, 7, 8, 10, 12, 15, 18, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 8, 12))
wykres_wioliny <- ggplot2::ggplot(wiolina_dane, ggplot2::aes(grupa, czas)) +
  ggplot2::geom_violin(trim = TRUE, bw = 1.6, fill = kolory[["secondary"]], alpha = .5) +
  ggplot2::geom_boxplot(width = .12, fill = "white", outlier.shape = NA) +
  ggplot2::geom_point(position = ggplot2::position_nudge(x = .13), size = 1.2, colour = kolory[["primary"]]) +
  ggplot2::labs(x = NULL, y = "Czas [min]", title = "Wykres wiolinowy, kwartyle i obserwacje",
                subtitle = "Syntetyczne czasy; ta sama reguła wygładzenia w obu grupach") +
  badaniaZI::theme_zi()
wiolina_opis <- do.call(rbind, lapply(split(wiolina_dane$czas, wiolina_dane$grupa), function(x)
  data.frame(mediana = median(x), q1 = unname(quantile(x, .25)), q3 = unname(quantile(x, .75)), max = max(x))))

pareto <- data.frame(bariera = c("Logowanie", "Link", "Filtr", "Format", "Inne"), liczba = c(12, 9, 5, 3, 1))
pareto$bariera <- factor(pareto$bariera, levels = pareto$bariera)
pareto$skumulowany <- 100 * cumsum(pareto$liczba) / sum(pareto$liczba)
wykres_pareto <- badaniaZI::wykres_czestosci(as.character(pareto$bariera), pareto$liczba, os = "Główna bariera zgłoszenia",
  tytul = "Pareto: bariery od najczęstszej")
pareto_do_druku <- data.frame(Bariera = pareto$bariera, `Zgłoszenia` = pareto$liczba,
                              `Narastająco [%]` = liczba(pareto$skumulowany, 1L), check.names = FALSE)

urzadzenia <- data.frame(urzadzenie = c("Komputer", "Telefon", "Tablet"), N = c(24, 16, 8))
urzadzenia$udzial <- 100 * urzadzenia$N / sum(urzadzenia$N)
urzadzenia$etykieta <- paste0(urzadzenia$urzadzenie, "\n", liczba(urzadzenia$udzial, 1L), "%")
wykres_kola <- ggplot2::ggplot(urzadzenia, ggplot2::aes(x = "", y = N, fill = urzadzenie)) +
  ggplot2::geom_col(width = 1, colour = "white") +
  ggplot2::geom_text(ggplot2::aes(label = etykieta), position = ggplot2::position_stack(vjust = .5), size = 3.2) +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::scale_fill_manual(values = unname(kolory[c("secondary", "warning", "success")])) +
  ggplot2::labs(title = "Jedno urządzenie bieżącej sesji", subtitle = "N = 48 hipotetycznych osób, rozłączne kategorie") +
  ggplot2::theme_void() + ggplot2::theme(legend.position = "none")
urzadzenia_do_druku <- data.frame(`Urządzenie` = urzadzenia$urzadzenie, N = urzadzenia$N,
                                  `Udział [%]` = liczba(urzadzenia$udzial, 1L), check.names = FALSE)

mapa <- data.frame(grupa = rep(c("Nowi", "Doświadczeni"), each = 3),
                   zadanie = rep(c("Katalog", "Repozytorium", "Weryfikacja"), 2),
                   N = c(15, 20, 10, 12, 20, 15), powodzenia = c(9, 12, 8, 9, 18, 9))
mapa$procent <- 100 * mapa$powodzenia / mapa$N
mapa$SE <- sqrt(mapa$procent / 100 * (1 - mapa$procent / 100) / mapa$N)
mapa$etykieta <- paste0(mapa$powodzenia, "/", mapa$N, "\n", round(mapa$procent), "%")
wykres_mapy <- ggplot2::ggplot(mapa, ggplot2::aes(zadanie, grupa, fill = procent)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(ggplot2::aes(label = etykieta), size = 3.5) +
  ggplot2::scale_fill_gradient(low = "#FFFFFF", high = kolory[["secondary"]], limits = c(0, 100)) +
  ggplot2::labs(x = "Rodzaj zadania", y = NULL, fill = "Powodzenie [%]",
                title = "Mapa cieplna: udział i mianownik w każdej komórce",
                subtitle = "92 hipotetyczne osoby, każda wykonuje jedno zadanie") +
  badaniaZI::theme_zi()
mapa_do_druku <- data.frame(Grupa = mapa$grupa, Zadanie = mapa$zadanie, N = mapa$N, Powodzenia = mapa$powodzenia,
                            `Odsetek [%]` = liczba(mapa$procent, 1L), `SE odsetka` = liczba(mapa$SE, 3L), check.names = FALSE)

wydajnosc <- data.frame(rownoczesne = c(20, 40, 60, 80, 100), czas_s = c(.35, .42, .8, 1.6, 3.2),
                        na_sekunde = c(15, 28, 40, 44, 43))
wydajnosc_dlugie <- rbind(
  data.frame(obciazenie = wydajnosc$rownoczesne, wynik = wydajnosc$czas_s, miara = "Mediana czasu odpowiedzi [s]"),
  data.frame(obciazenie = wydajnosc$rownoczesne, wynik = wydajnosc$na_sekunde, miara = "Przepustowość [żądania/s]"))
wykres_wydajnosci <- ggplot2::ggplot(wydajnosc_dlugie, ggplot2::aes(obciazenie, wynik)) +
  ggplot2::geom_line(colour = kolory[["primary"]]) +
  ggplot2::geom_point(colour = kolory[["accent"]], size = 2) +
  ggplot2::facet_wrap(~miara, ncol = 1, scales = "free_y") +
  ggplot2::labs(x = "Równoczesne żądania [liczba]", y = NULL, title = "Wydajność techniczna usługi",
                subtitle = "Pięć hipotetycznych ustawień testu technicznego") +
  badaniaZI::theme_zi()

reszty_czastkowe <- function() {
  d <- data.frame(praktyka = 1:12, trudnosc = rep(c(1, 3, 2), 4))
  d$czas <- 18 - .5 * d$praktyka + 2 * d$trudnosc + c(.5, -.2, .3, -.4, .1, -.2, .4, -.3, .2, -.1, .3, -.5)
  m <- lm(czas ~ praktyka + trudnosc, data = d)
  b <- unname(coef(m)["praktyka"])
  d$czastkowe <- resid(m) + b * d$praktyka
  wykres <- ggplot2::ggplot(d, ggplot2::aes(praktyka, czastkowe)) +
    ggplot2::geom_abline(intercept = 0, slope = b, colour = kolory[["accent"]]) +
    ggplot2::geom_point(colour = kolory[["primary"]], size = 2) +
    ggplot2::scale_x_continuous(breaks = c(1, 3, 6, 9, 12)) +
    ggplot2::labs(x = "Wcześniejsze próby [liczba]", y = "Reszta + składnik praktyki [min]",
                  title = "Reszty cząstkowe dla praktyki",
                  subtitle = "N = 12 hipotetycznych osób; model uwzględnia też trudność zadania") +
    badaniaZI::theme_zi()
  list(wspolczynniki = coef(m), wykres = wykres)
}
czastkowe <- reszty_czastkowe()
