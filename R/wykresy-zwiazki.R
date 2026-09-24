# Wykresy związku dwóch zmiennych: korelacja, regresja prosta, diagnostyka
# i kalibracja. Funkcje zwracają obiekt ggplot; odczyt słowny zapisuje materiał.

#' Rozrzut z prosta regresji, pasmem sredniej i pasmem predykcji
#'
#' Punkty to pary (x, y); linia ciagla to prosta metody najmniejszych
#' kwadratow; ciemniejsze pasmo to 95% przedzial sredniej warunkowej, jasniejsze
#' i szersze pasmo to 95% przedzial predykcji dla nowej osoby. Opcjonalna
#' pionowa linia przy x0 z napisem podaje oba przedzialy.
#' @param x,y Wartosci par.
#' @param x0 Opcjonalna wartosc x do odczytu przedzialow.
#' @param os_x,os_y Podpisy osi.
#' @param etykiety Opcjonalne etykiety punktow.
#' @param predykcja Czy rysowac pasmo predykcji.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_pasma_regresji(c(1, 2, 2, 3, 4, 5), c(15, 12, 14, 10, 9, 6), x0 = 3)
wykres_pasma_regresji <- function(x, y, x0 = NULL, os_x = "x", os_y = "y", etykiety = NULL,
                                  predykcja = TRUE, tytul = NULL) {
  stopifnot(length(x) == length(y), length(x) >= 3L)
  d <- data.frame(x = x, y = y)
  m <- stats::lm(y ~ x, data = d)
  s <- data.frame(x = seq(min(x), max(x), length.out = 100))
  ci <- stats::predict(m, s, interval = "confidence")
  pi <- stats::predict(m, s, interval = "prediction")
  s$fit <- ci[, 1]; s$ci_d <- ci[, 2]; s$ci_g <- ci[, 3]; s$pi_d <- pi[, 2]; s$pi_g <- pi[, 3]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y))
  if (predykcja) p <- p + ggplot2::geom_ribbon(data = s, ggplot2::aes(x = .data$x, ymin = .data$pi_d, ymax = .data$pi_g),
                                               inherit.aes = FALSE, fill = kolory_zi[["light"]], colour = "gray60",
                                               linetype = "dotted")
  p <- p + ggplot2::geom_ribbon(data = s, ggplot2::aes(x = .data$x, ymin = .data$ci_d, ymax = .data$ci_g),
                                inherit.aes = FALSE, fill = kolory_zi[["secondary"]], alpha = 0.45) +
    ggplot2::geom_line(data = s, ggplot2::aes(x = .data$x, y = .data$fit), colour = kolory_zi[["accent"]], linewidth = 0.9) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2.6)
  if (!is.null(etykiety)) p <- p + ggplot2::geom_text(data = data.frame(x = x, y = y, e = etykiety),
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$e), vjust = -0.9, size = 3.2)
  podpis <- paste0("Prosta: y = ", liczba_pl(stats::coef(m)[1], 2L), " ", ifelse(stats::coef(m)[2] < 0, "\u2212 ", "+ "),
                   liczba_pl(abs(stats::coef(m)[2]), 3L), "\u00b7x; ciemne pasmo: \u015brednia",
                   if (predykcja) "; jasne: nowa osoba" else "")
  if (!is.null(x0)) {
    c0 <- stats::predict(m, data.frame(x = x0), interval = "confidence")
    p0 <- stats::predict(m, data.frame(x = x0), interval = "prediction")
    p <- p + ggplot2::geom_vline(xintercept = x0, linetype = "dashed", colour = kolory_zi[["dark"]])
    podpis <- paste0(podpis, "\nPrzy x = ", os_pl(x0), ": \u015brednia ", liczba_pl(c0[1], 2L), " [",
                     liczba_pl(c0[2], 2L), "; ", liczba_pl(c0[3], 2L), "], nowa osoba [",
                     liczba_pl(p0[2], 2L), "; ", liczba_pl(p0[3], 2L), "]")
  }
  p + ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os_x, y = os_y, title = tytul, subtitle = podpis) +
    theme_zi()
}

#' Reszty wobec wartosci przewidywanych
#'
#' Panel dla kazdego elementu listy (ramka z kolumnami x i y): punkty to
#' reszty prostej regresji wobec wartosci przewidywanych, linia przerywana to
#' zero. Luk wskazuje niedopasowana srednia, lejek - rozrzut zalezny od
#' poziomu przewidywania.
#' @param dane Nazwana lista ramek z kolumnami x i y.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_reszty(list(A = data.frame(x = 1:6, y = c(15, 12, 14, 10, 9, 6))))
wykres_reszty <- function(dane, tytul = NULL) {
  stopifnot(is.list(dane), !is.null(names(dane)))
  d <- do.call(rbind, lapply(names(dane), function(n) {
    m <- stats::lm(y ~ x, data = dane[[n]])
    data.frame(przewidywane = stats::fitted(m), reszty = stats::resid(m), panel = n)
  }))
  d$panel <- factor(d$panel, levels = names(dane))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$przewidywane, y = .data$reszty)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = kolory_zi[["dark"]]) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free") +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = "Warto\u015b\u0107 przewidywana", y = "Reszta", title = tytul) +
    theme_zi()
}

#' Kalibracja deklaracji wzgledem wyniku
#'
#' Lewy panel: deklaracja wobec wyniku z linia zgodnosci y = x (przerywana).
#' Prawy panel (Bland-Altman): roznica deklaracja minus wynik wobec sredniej
#' obu pomiarow; linia ciagla to sredni blad, linie kropkowane to granice
#' zgodnosci sredni blad +/- 1,96 SD roznic.
#' @param deklaracja,wynik Pomiary w tej samej jednostce.
#' @param jednostka Jednostka w podpisach osi.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_kalibracja(c(40, 50, 60), c(20, 30, 40))
wykres_kalibracja <- function(deklaracja, wynik, jednostka = "%", tytul = NULL) {
  stopifnot(length(deklaracja) == length(wynik))
  panele <- c(paste0("Deklaracja wobec wyniku [", jednostka, "]"),
              paste0("R\u00f3\u017cnica wobec \u015bredniej [", jednostka, "]"))
  roznica <- deklaracja - wynik
  sr <- mean(roznica); s <- stats::sd(roznica)
  d <- rbind(data.frame(os_x = wynik, os_y = deklaracja, panel = panele[1]),
             data.frame(os_x = (deklaracja + wynik) / 2, os_y = roznica, panel = panele[2]))
  d$panel <- factor(d$panel, levels = panele)
  zgodnosc <- data.frame(panel = factor(panele[1], levels = panele))
  linie <- data.frame(panel = factor(panele[2], levels = panele), y = c(sr, sr - 1.96 * s, sr + 1.96 * s),
                      typ = c("\u015bredni b\u0142\u0105d", "granica", "granica"))
  zakres <- range(c(deklaracja, wynik))
  # Pusty obszar: panel zgodnosci ma rowne osie, panel roznic obejmuje zero.
  puste <- data.frame(os_x = c(zakres, mean(zakres), mean(zakres)),
                      os_y = c(zakres, 0, 2 * sr + sign(sr + (sr == 0))),
                      panel = factor(panele[c(1, 1, 2, 2)], levels = panele))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$os_x, y = .data$os_y)) +
    ggplot2::geom_blank(data = puste) +
    ggplot2::geom_abline(data = zgodnosc, ggplot2::aes(slope = 1, intercept = 0), linetype = "dashed",
                         colour = kolory_zi[["dark"]]) +
    ggplot2::geom_hline(data = linie, ggplot2::aes(yintercept = .data$y, linetype = .data$typ),
                        colour = kolory_zi[["accent"]], show.legend = FALSE) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2.6) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free") +
    ggplot2::scale_linetype_manual(values = c("\u015bredni b\u0142\u0105d" = "solid", granica = "dotted")) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = NULL, y = NULL, title = tytul,
                  subtitle = paste0("\u015aredni b\u0142\u0105d deklaracji: ", liczba_pl(sr, 1L), "; SD r\u00f3\u017cnic: ",
                                    liczba_pl(s, 1L))) +
    theme_zi()
}

#' Galeria korelacji o zadanych wartosciach r
#'
#' Panel dla kazdego r: n syntetycznych par o korelacji dokladnie r
#' (konstrukcja deterministyczna: kwantyle rozkladu normalnego i stala
#' permutacja reszt), z prosta regresji.
#' @param r Wartosci wspolczynnika korelacji.
#' @param n Liczba par w panelu.
#' @param tytul Tytul wykresu.
#' @param dane Opcjonalna ramka z kolumnami x i y: pierwszy panel przedstawia
#'   te dane po standaryzacji, z obliczonym r.
#' @param etykieta_danych Nazwa panelu danych.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_galeria_r(c(0, -0.5))
wykres_galeria_r <- function(r, n = 100L, tytul = NULL, dane = NULL, etykieta_danych = "Dane") {
  stopifnot(all(abs(r) < 1))
  x <- stats::qnorm(stats::ppoints(n))
  e <- stats::qnorm(stats::ppoints(n))[order(sin(seq_len(n) * 12.9898))]
  e <- stats::resid(stats::lm(e ~ x))
  x <- x / stats::sd(x); e <- e / stats::sd(e)
  panele <- paste0("r = ", os_pl(r))
  d <- do.call(rbind, lapply(seq_along(r), function(i)
    data.frame(x = x, y = r[i] * x + sqrt(1 - r[i]^2) * e, panel = panele[i])))
  if (!is.null(dane)) {
    stopifnot(is.data.frame(dane), all(c("x", "y") %in% names(dane)))
    dane <- dane[is.finite(dane$x) & is.finite(dane$y), , drop = FALSE]
    nazwa <- paste0(etykieta_danych, ": r = ", liczba_pl(stats::cor(dane$x, dane$y), 2L))
    d <- rbind(data.frame(x = as.vector(scale(dane$x)), y = as.vector(scale(dane$y)), panel = nazwa), d)
    panele <- c(nazwa, panele)
  }
  d$panel <- factor(d$panel, levels = panele)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 1.2, alpha = 0.8) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = kolory_zi[["accent"]], linewidth = 0.8) +
    ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::scale_x_continuous(labels = NULL) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::labs(x = "x (standaryzowane)", y = "y (standaryzowane)", title = tytul,
                  subtitle = if (is.null(dane)) paste0(n, " syntetycznych par w ka\u017cdym panelu") else
                    paste0("Pierwszy panel: ", nrow(dane), " par z danych; pozosta\u0142e: ", n, " par syntetycznych")) +
    theme_zi()
}

#' Kwartet Anscombe'a
#'
#' Cztery zbiory danych `datasets::anscombe` z prosta regresji; zbiory maja
#' prawie identyczne srednie, wariancje, korelacje (0,82) i proste.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_anscombe()
wykres_anscombe <- function(tytul = NULL) {
  a <- datasets::anscombe
  d <- do.call(rbind, lapply(1:4, function(i)
    data.frame(x = a[[paste0("x", i)]], y = a[[paste0("y", i)]],
               panel = paste0("Zbi\u00f3r ", i, ": r = ", liczba_pl(stats::cor(a[[paste0("x", i)]], a[[paste0("y", i)]]), 2L)))))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = kolory_zi[["accent"]],
                         linewidth = 0.8, fullrange = TRUE) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2) +
    ggplot2::facet_wrap(~panel, nrow = 2) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = "x", y = "y", title = tytul) +
    theme_zi()
}

#' Wplyw jednego punktu na prosta regresji
#'
#' Kola to pierwotne pary, romb to punkt dodany; linia ciagla to prosta bez
#' punktu, linia przerywana to prosta z punktem. Podtytul podaje oba
#' nachylenia.
#' @param x,y Pierwotne pary.
#' @param punkt Wektor c(x, y) punktu dodanego.
#' @param os_x,os_y Podpisy osi.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_wplyw(c(1, 2, 2, 3, 4, 5), c(15, 12, 14, 10, 9, 6), c(9, 14))
wykres_wplyw <- function(x, y, punkt, os_x = "x", os_y = "y", tytul = NULL) {
  stopifnot(length(x) == length(y), length(punkt) == 2L)
  b_bez <- stats::coef(stats::lm(y ~ x))
  b_z <- stats::coef(stats::lm(c(y, punkt[2]) ~ c(x, punkt[1])))
  zakres <- range(c(x, punkt[1]))
  proste <- data.frame(model = rep(c("Bez dodanego punktu", "Z dodanym punktem"), each = 2),
                       x = rep(zakres, 2),
                       y = c(b_bez[1] + b_bez[2] * zakres, b_z[1] + b_z[2] * zakres))
  ggplot2::ggplot() +
    ggplot2::geom_line(data = proste, ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$model),
                       colour = kolory_zi[["accent"]], linewidth = 0.9) +
    ggplot2::geom_point(data = data.frame(x = x, y = y), ggplot2::aes(x = .data$x, y = .data$y),
                        colour = kolory_zi[["primary"]], size = 2.6) +
    ggplot2::geom_point(data = data.frame(x = punkt[1], y = punkt[2]), ggplot2::aes(x = .data$x, y = .data$y),
                        shape = 18, size = 5, colour = kolory_zi[["dark"]]) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os_x, y = os_y, title = tytul,
                  subtitle = paste0("Nachylenie bez punktu ", liczba_pl(b_bez[2], 2L), ", z punktem ",
                                    liczba_pl(b_z[2], 2L))) +
    theme_zi()
}

#' Macierz korelacji pozycji
#'
#' Dolny trojkat macierzy korelacji liczonej na osobach z kompletem
#' odpowiedzi; komorki podaja wspolczynnik z dwoma miejscami po przecinku,
#' a podtytul N, srednia korelacje i jej zakres.
#' @param dane Ramka danych liczbowych: jedna kolumna na pozycje.
#' @param etykiety Nazwy pozycji na osiach.
#' @param metoda Metoda funkcji cor(): "pearson" albo "spearman".
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_macierz_korelacji(data.frame(a = c(1, 2, 3, 4, 5), b = c(2, 1, 4, 3, 5),
#'                                          c = c(1, 3, 2, 5, 4)))
wykres_macierz_korelacji <- function(dane, etykiety = names(dane), metoda = c("pearson", "spearman"),
                                     tytul = NULL) {
  metoda <- match.arg(metoda)
  stopifnot(is.data.frame(dane), ncol(dane) >= 2L, length(etykiety) == ncol(dane))
  kompletne <- dane[stats::complete.cases(dane), , drop = FALSE]
  stopifnot(nrow(kompletne) >= 3L)
  R <- stats::cor(kompletne, method = metoda)
  k <- ncol(R)
  d <- expand.grid(w = seq_len(k), kol = seq_len(k))
  d <- d[d$w > d$kol, ]
  d$r <- R[cbind(d$w, d$kol)]
  d$wiersz <- factor(etykiety[d$w], levels = rev(etykiety))
  d$kolumna <- factor(etykiety[d$kol], levels = etykiety)
  r <- R[lower.tri(R)]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$kolumna, y = .data$wiersz, fill = .data$r)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = liczba_pl(.data$r, 2L)), size = 3.4) +
    ggplot2::scale_fill_gradient(low = kolory_zi[["light"]], high = kolory_zi[["secondary"]],
                                 limits = c(min(0, r), 1), name = "r", labels = os_pl) +
    ggplot2::labs(x = NULL, y = NULL, title = tytul,
                  subtitle = paste0("N = ", nrow(kompletne), " os\u00f3b z kompletem; \u015brednia korelacja ",
                                    liczba_pl(mean(r), 2L), " (od ", liczba_pl(min(r), 2L), " do ",
                                    liczba_pl(max(r), 2L), ")")) +
    theme_zi() + ggplot2::theme(panel.grid = ggplot2::element_blank())
}
