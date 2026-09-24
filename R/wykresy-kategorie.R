# Wykresy odpowiedzi kategorialnych: pozycje porzadkowe, wybory wielokrotne
# i tabele liczebnosci. Slowny odczyt wykresu zapisuje material.

#' Rozklady odpowiedzi kilku pozycji porzadkowych
#'
#' Jedna pozioma belka na pozycje; belka ma dlugosc 100% waznych odpowiedzi
#' pozycji, a odcinki odpowiadaja kategoriom. Napisy podaja odsetek kategorii
#' (od 7%). Braki sa pomijane w mianowniku kazdej pozycji.
#' @param pozycje Ramka danych: jedna kolumna na pozycje, kody kategorii.
#' @param etykiety Nazwy pozycji na osi.
#' @param kategorie Kody kategorii w kolejnosci skali.
#' @param nazwy_kategorii Opisy kategorii w legendzie.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_likert(data.frame(p1 = c(1, 2, 4, 5, 4), p2 = c(3, 3, 4, NA, 2)))
wykres_likert <- function(pozycje, etykiety = names(pozycje), kategorie = 1:5,
                          nazwy_kategorii = as.character(kategorie), tytul = NULL) {
  stopifnot(is.data.frame(pozycje), length(etykiety) == ncol(pozycje),
            length(nazwy_kategorii) == length(kategorie), length(kategorie) >= 2L)
  d <- do.call(rbind, lapply(seq_along(pozycje), function(j) {
    n <- as.integer(table(factor(pozycje[[j]], levels = kategorie)))
    stopifnot(sum(n) > 0)
    data.frame(pozycja = etykiety[j], kategoria = nazwy_kategorii, n = n, N = sum(n),
               proc = 100 * n / sum(n))
  }))
  kolory <- grDevices::colorRampPalette(c(kolory_zi[["accent"]], "#EDEDED", kolory_zi[["primary"]]))(length(kategorie))
  kolory <- stats::setNames(kolory, nazwy_kategorii)
  d$tekst <- ifelse(d$kategoria %in% nazwy_kategorii[c(1L, length(kategorie))], "white", "black")
  d$etykieta <- ifelse(d$proc >= 7, paste0(liczba_pl(d$proc, 0L), "%"), "")
  d$kategoria <- factor(d$kategoria, levels = rev(nazwy_kategorii))
  d$pozycja <- factor(d$pozycja, levels = rev(etykiety))
  N <- unique(d$N)
  podpis <- paste0("N wa\u017cnych odpowiedzi: ",
                   if (length(N) == 1L) N else paste0(min(N), "\u2013", max(N)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$proc, y = .data$pozycja, fill = .data$kategoria)) +
    ggplot2::geom_col(width = 0.7, colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta, colour = .data$tekst, group = .data$kategoria),
                       position = ggplot2::position_stack(vjust = 0.5), size = 3) +
    ggplot2::scale_fill_manual(values = kolory, breaks = nazwy_kategorii, name = NULL) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(labels = os_pl, breaks = seq(0, 100, 25), expand = c(0, 0)) +
    ggplot2::labs(x = "Odsetek wa\u017cnych odpowiedzi [%]", y = NULL, title = tytul, subtitle = podpis) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
    theme_zi() + ggplot2::theme(legend.position = "bottom", plot.title.position = "plot")
}

#' Granice liczby osob z dwoma wyborami
#'
#' Przy N osobach, n_a wyborach opcji A i n_b wyborach opcji B liczba osob
#' z oboma wyborami lezy miedzy max(0, n_a + n_b - N) a min(n_a, n_b). Dwie
#' belki przedstawiaja uklad z najmniejsza i z najwieksza czescia wspolna.
#' @param n_a,n_b Liczby osob wybierajacych opcje A i B.
#' @param N Liczba osob z kompletna odpowiedzia.
#' @param nazwy Nazwy opcji A i B.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_wspolne_wybory(30, 25, 50, c("e-mail", "WWW"))
wykres_wspolne_wybory <- function(n_a, n_b, N, nazwy = c("A", "B"), tytul = NULL) {
  stopifnot(n_a >= 0, n_b >= 0, n_a <= N, n_b <= N, length(nazwy) == 2L)
  wsp <- c(max(0, n_a + n_b - N), min(n_a, n_b))
  uklady <- c(paste0("Najmniejsza cz\u0119\u015b\u0107 wsp\u00f3lna: ", wsp[1]),
              paste0("Najwi\u0119ksza cz\u0119\u015b\u0107 wsp\u00f3lna: ", wsp[2]))
  czesci <- c(paste0("Tylko ", nazwy[1]), paste0(nazwy[1], " i ", nazwy[2]), paste0("Tylko ", nazwy[2]),
              "\u017badna z dw\u00f3ch")
  d <- do.call(rbind, lapply(1:2, function(i)
    data.frame(uklad = uklady[i], czesc = czesci,
               n = c(n_a - wsp[i], wsp[i], n_b - wsp[i], N - n_a - n_b + wsp[i]))))
  d$tekst <- ifelse(d$czesc == czesci[2], "white", "black")
  d$etykieta <- ifelse(d$n > 0, d$n, "")
  d$czesc <- factor(d$czesc, levels = rev(czesci))
  d$uklad <- factor(d$uklad, levels = rev(uklady))
  kolory <- stats::setNames(c(kolory_zi[["secondary"]], kolory_zi[["primary"]], kolory_zi[["warning"]],
                              kolory_zi[["light"]]), czesci)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$uklad, fill = .data$czesc)) +
    ggplot2::geom_col(width = 0.55, colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta, colour = .data$tekst, group = .data$czesc),
                       position = ggplot2::position_stack(vjust = 0.5), size = 3.3) +
    ggplot2::scale_fill_manual(values = kolory, breaks = czesci, name = NULL) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = etykiety_zi$liczba_osob, y = NULL, title = tytul,
                  subtitle = paste0("N = ", N, "; ", nazwy[1], ": ", n_a, ", ", nazwy[2], ": ", n_b)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    theme_zi() + ggplot2::theme(legend.position = "bottom", plot.title.position = "plot")
}

#' Wykres mozaikowy tabeli liczebnosci
#'
#' Kazdy wiersz tabeli jest slupkiem o szerokosci proporcjonalnej do jego
#' liczebnosci i wysokosci 100%; odcinki odpowiadaja kolumnom tabeli. Napisy
#' podaja liczebnosc i odsetek w wierszu, a podpis osi - N wiersza.
#' @param tabela Macierz liczebnosci z nazwami wierszy i kolumn.
#' @param nazwy_kolumn Opisy kolumn w legendzie (domyslnie nazwy kolumn).
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_mozaika(matrix(c(10, 30, 32, 48), 2, byrow = TRUE,
#'        dimnames = list(c("A", "B"), c("0", "1"))))
wykres_mozaika <- function(tabela, nazwy_kolumn = colnames(tabela), tytul = NULL) {
  stopifnot(is.matrix(tabela), !is.null(rownames(tabela)), !is.null(colnames(tabela)),
            all(tabela >= 0), all(rowSums(tabela) > 0), length(nazwy_kolumn) == ncol(tabela))
  n_w <- unname(rowSums(tabela))
  prawa <- cumsum(n_w) / sum(n_w)
  lewa <- c(0, utils::head(prawa, -1))
  d <- do.call(rbind, lapply(seq_len(nrow(tabela)), function(i) {
    udzial <- unname(tabela[i, ]) / n_w[i]
    gora <- cumsum(udzial)
    data.frame(wiersz = rownames(tabela)[i], kolumna = nazwy_kolumn, xmin = lewa[i], xmax = prawa[i],
               ymin = gora - udzial, ymax = gora, n = as.vector(tabela[i, ]), proc = 100 * udzial)
  }))
  d$etykieta <- ifelse(d$n > 0, paste0(d$n, " (", liczba_pl(d$proc, 1L), "%)"), "")
  d$kolumna <- factor(d$kolumna, levels = nazwy_kolumn)
  kolory <- stats::setNames(rep(c(kolory_zi[["light"]], kolory_zi[["secondary"]], kolory_zi[["warning"]],
                                  kolory_zi[["info"]]), length.out = ncol(tabela)), nazwy_kolumn)
  srodki <- (lewa + prawa) / 2
  ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax,
                                    fill = .data$kolumna), colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(x = (.data$xmin + .data$xmax) / 2, y = (.data$ymin + .data$ymax) / 2,
                                    label = .data$etykieta), size = 3.3) +
    ggplot2::scale_fill_manual(values = kolory, name = NULL) +
    ggplot2::scale_x_continuous(breaks = srodki, labels = paste0(rownames(tabela), "\nN = ", n_w),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(os_pl(100 * v), "%"), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Odsetek w wierszu", title = tytul,
                  subtitle = "Szeroko\u015b\u0107 s\u0142upka proporcjonalna do liczebno\u015bci wiersza") +
    theme_zi() + ggplot2::theme(legend.position = "bottom", panel.grid = ggplot2::element_blank())
}

#' Ogon rozkladu zerowego Monte Carlo i rozkladu chi-kwadrat
#'
#' Dwie krzywe udzialu wartosci co najmniej x: schodkowa z losowanych tabel
#' (z poprawka (1 + liczba)/(B + 1)) i gladka z rozkladu chi-kwadrat o df
#' stopniach swobody. Linia przerywana i punkty oznaczaja obserwowana
#' statystyke; podtytul podaje oba p.
#' @param wartosci Statystyki chi-kwadrat z losowanych tabel.
#' @param statystyka Obserwowana statystyka chi-kwadrat.
#' @param df Liczba stopni swobody rozkladu chi-kwadrat.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_chi2_mc(stats::rchisq(199, 1), 2.5)
wykres_chi2_mc <- function(wartosci, statystyka, df = 1, tytul = NULL) {
  wartosci <- wartosci[is.finite(wartosci)]
  B <- length(wartosci)
  stopifnot(B >= 1L, statystyka >= 0, df > 0)
  gora <- max(stats::qchisq(0.99, df), 1.3 * statystyka)
  s <- sort(unique(c(seq(0, gora, length.out = 400), wartosci[wartosci <= gora])))
  ogon_mc <- function(v) (1 + sum(wartosci >= v - 1e-12)) / (B + 1)
  nazwy <- c(paste0("Monte Carlo, B = ", B), paste0("\u03c7\u00b2(", df, ")"))
  d <- rbind(data.frame(x = s, y = vapply(s, ogon_mc, numeric(1)), rozklad = nazwy[1]),
             data.frame(x = s, y = stats::pchisq(s, df, lower.tail = FALSE), rozklad = nazwy[2]))
  d$rozklad <- factor(d$rozklad, levels = nazwy)
  p <- c(ogon_mc(statystyka), stats::pchisq(statystyka, df, lower.tail = FALSE))
  punkty <- data.frame(x = statystyka, y = p, rozklad = factor(nazwy, levels = nazwy))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$rozklad)) +
    ggplot2::geom_step(data = d[d$rozklad == nazwy[1], ], colour = kolory_zi[["primary"]], linewidth = 0.9,
                       direction = "vh") +
    ggplot2::geom_line(data = d[d$rozklad == nazwy[2], ], colour = kolory_zi[["dark"]], linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = statystyka, linetype = "dotted", colour = kolory_zi[["accent"]]) +
    ggplot2::geom_point(data = punkty, ggplot2::aes(shape = .data$rozklad), size = 3, colour = kolory_zi[["accent"]]) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17), name = NULL) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, 1)) +
    ggplot2::labs(x = "Warto\u015b\u0107 statystyki \u03c7\u00b2", y = "Udzia\u0142 warto\u015bci \u2265 x",
                  title = tytul,
                  subtitle = paste0("Przy \u03c7\u00b2 = ", liczba_pl(statystyka, 3L), ": Monte Carlo ",
                                    liczba_pl(p[1], 3L), ", \u03c7\u00b2(", df, ") ", liczba_pl(p[2], 3L))) +
    theme_zi() + ggplot2::theme(legend.position = "bottom")
}

#' Przejscia miedzy kategoriami w dwoch pomiarach tych samych osob
#'
#' Kafelek tabeli 2 x 2 podaje liczbe osob z dana para wynikow: wiersze to
#' pomiar pierwszy, kolumny - drugi. Przekatna oznacza brak zmiany, komorki
#' poza przekatna - zmiane w jednym z dwoch kierunkow.
#' @param tabela Macierz 2 x 2 liczebnosci (wiersze: przed, kolumny: po).
#' @param nazwy Opisy kategorii 0 i 1.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_przejscia(matrix(c(20, 10, 2, 18), 2, byrow = TRUE))
wykres_przejscia <- function(tabela, nazwy = c("niepowodzenie", "sukces"), tytul = NULL) {
  stopifnot(is.matrix(tabela), all(dim(tabela) == c(2L, 2L)), all(tabela >= 0), length(nazwy) == 2L)
  d <- expand.grid(przed = 1:2, po = 1:2)
  d$n <- tabela[cbind(d$przed, d$po)]
  d$rodzaj <- ifelse(d$przed == d$po, "bez zmiany", ifelse(d$po > d$przed, "poprawa", "pogorszenie"))
  d$etykieta <- paste0(d$n, " os.\n", d$rodzaj)
  d$przed <- factor(nazwy[d$przed], levels = rev(nazwy))
  d$po <- factor(nazwy[d$po], levels = nazwy)
  kolory <- c(`bez zmiany` = kolory_zi[["light"]], poprawa = kolory_zi[["secondary"]],
              pogorszenie = kolory_zi[["warning"]])
  ggplot2::ggplot(d, ggplot2::aes(x = .data$po, y = .data$przed, fill = .data$rodzaj)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), size = 3.6) +
    ggplot2::scale_fill_manual(values = kolory, guide = "none") +
    ggplot2::labs(x = "Pomiar po", y = "Pomiar przed", title = tytul,
                  subtitle = paste0("N = ", sum(tabela), "; poprawa ", tabela[1, 2], ", pogorszenie ", tabela[2, 1],
                                    ", bez zmiany ", tabela[1, 1] + tabela[2, 2])) +
    theme_zi() + ggplot2::theme(panel.grid = ggplot2::element_blank())
}
