# Wykresy porównań grup, par i tabel kategorii dla wykładów i ćwiczeń.
# Funkcje zwracają obiekt ggplot; słowny odczyt wykresu zapisuje materiał.

#' Wykres kwantylowy wzgledem rozkladu normalnego
#'
#' Panel dla kazdego elementu listy: na osi poziomej kwantyle rozkladu
#' normalnego standardowego, na osi pionowej uporzadkowane wartosci. Linia
#' przerywana przechodzi przez pierwszy i trzeci kwartyl; punkty wzdluz linii
#' oznaczaja zgodnosc z rozkladem normalnym, a punkty powyzej linii na prawym
#' koncu oznaczaja prawy ogon.
#' @param dane Nazwana lista wektorow liczbowych.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_kwantylowy(list(A = c(1, 2, 3, 4, 10)))
wykres_kwantylowy <- function(dane, os = "Uporz\u0105dkowane warto\u015bci", tytul = NULL) {
  stopifnot(is.list(dane), !is.null(names(dane)))
  panele <- vapply(seq_along(dane), function(i)
    paste0(names(dane)[i], " (N = ", sum(is.finite(dane[[i]])), ")"), character(1))
  punkty <- do.call(rbind, lapply(seq_along(dane), function(i) {
    x <- sort(dane[[i]][is.finite(dane[[i]])])
    data.frame(teoria = stats::qnorm(stats::ppoints(length(x))), x = x, panel = panele[i])
  }))
  linie <- do.call(rbind, lapply(seq_along(dane), function(i) {
    x <- dane[[i]][is.finite(dane[[i]])]
    q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
    z <- stats::qnorm(c(0.25, 0.75))
    b <- diff(q) / diff(z)
    data.frame(nachylenie = b, wyraz = q[1] - b * z[1], panel = panele[i])
  }))
  punkty$panel <- factor(punkty$panel, levels = panele)
  linie$panel <- factor(linie$panel, levels = panele)
  ggplot2::ggplot(punkty, ggplot2::aes(x = .data$teoria, y = .data$x)) +
    ggplot2::geom_abline(data = linie, ggplot2::aes(slope = .data$nachylenie, intercept = .data$wyraz),
                         linetype = "dashed", colour = kolory_zi[["dark"]]) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = "Kwantyle rozk\u0142adu normalnego standardowego", y = os, title = tytul) +
    theme_zi()
}

#' Gestosci rozkladow chi-kwadrat
#'
#' Krzywe gestosci rozkladu chi-kwadrat o podanych stopniach swobody,
#' rozroznione typem linii; legenda podaje wartosc krytyczna rzedu `kwantyl`.
#' @param df Stopnie swobody.
#' @param kwantyl Rzad kwantyla podanego w legendzie.
#' @param gora Gorna granica osi.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_chi2(c(1, 2, 4))
wykres_chi2 <- function(df, kwantyl = 0.95, gora = 12, tytul = NULL) {
  stopifnot(all(df > 0))
  s <- seq(0.02, gora, length.out = 500)
  nazwy <- paste0("df = ", df, ": ", liczba_pl(stats::qchisq(kwantyl, df), 2L))
  d <- do.call(rbind, lapply(seq_along(df), function(i)
    data.frame(x = s, y = stats::dchisq(s, df[i]), rozklad = nazwy[i])))
  d$rozklad <- factor(d$rozklad, levels = nazwy)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$rozklad)) +
    ggplot2::geom_line(colour = kolory_zi[["dark"]], linewidth = 0.8) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")[seq_along(df)],
                                   name = paste0("Warto\u015b\u0107 krytyczna ", liczba_pl(kwantyl, 2L))) +
    ggplot2::coord_cartesian(ylim = c(0, 0.6)) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = "Warto\u015b\u0107 statystyki \u03c7\u00b2", y = "G\u0119sto\u015b\u0107", title = tytul) +
    theme_zi()
}

#' Pary pomiarow przed i po
#'
#' Kazdy odcinek laczy dwa pomiary jednej osoby; etykieta przy pomiarze
#' "po" podaje identyfikator i zmiane po minus przed.
#' @param przed,po Pomiary tej samej osoby w dwoch momentach.
#' @param id Identyfikatory osob.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_pary(c(6, 8), c(5, 6), c("a", "b"))
wykres_pary <- function(przed, po, id, os = "Warto\u015b\u0107", tytul = NULL) {
  stopifnot(length(przed) == length(po), length(po) == length(id))
  momenty <- c(etykiety_zi$przed, etykiety_zi$po)
  d <- data.frame(id = rep(id, 2), moment = factor(rep(momenty, each = length(id)), levels = momenty),
                  y = c(przed, po))
  # Etykiety osob o tym samym wyniku "po" sa rozsuniete w pionie.
  przesuniecie <- stats::ave(po, po, FUN = function(v) seq_along(v) - (length(v) + 1) / 2)
  krok <- 0.06 * max(diff(range(c(przed, po))), 1)
  e <- data.frame(id = id, moment = factor(momenty[2], levels = momenty), y = po + przesuniecie * krok,
                  etykieta = paste0(id, ": ", ifelse(po - przed > 0, "+", ""), sub("^-", "\u2212", po - przed)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$moment, y = .data$y, group = .data$id)) +
    ggplot2::geom_line(colour = kolory_zi[["primary"]]) +
    ggplot2::geom_point(size = 2.4, colour = kolory_zi[["primary"]]) +
    ggplot2::geom_text(data = e, ggplot2::aes(label = .data$etykieta), hjust = -0.25, size = 3.1) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.3, 0.8))) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = NULL, y = os, title = tytul) +
    theme_zi()
}

#' Grupy analizy wariancji: punkty, srednie grup i srednia ogolna
#'
#' Kola oznaczaja obserwacje, romby srednie grup, linia przerywana srednia
#' ogolna, a pionowe odcinki odleglosc sredniej grupy od sredniej ogolnej
#' (skladnik sumy kwadratow miedzy grupami).
#' @param x Wartosci liczbowe.
#' @param grupa Etykiety grup.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_anova(c(4, 5, 6, 7, 8, 9), rep(c("A", "B"), each = 3))
wykres_anova <- function(x, grupa, os = "Warto\u015b\u0107", tytul = NULL) {
  stopifnot(length(x) == length(grupa))
  d <- data.frame(x = x, g = factor(grupa, levels = unique(grupa)))
  s <- stats::aggregate(x ~ g, d, mean)
  ogolna <- mean(x)
  s$etykieta <- paste0("\u015brednia ", liczba_pl(s$x, 2L))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$g, y = .data$x)) +
    ggplot2::geom_hline(yintercept = ogolna, linetype = "dashed", colour = kolory_zi[["dark"]]) +
    ggplot2::geom_segment(data = s, ggplot2::aes(x = .data$g, xend = .data$g, y = ogolna, yend = .data$x),
                          colour = kolory_zi[["accent"]], linewidth = 1,
                          position = ggplot2::position_nudge(x = 0.18)) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2.6, shape = 16) +
    ggplot2::geom_point(data = s, shape = 18, size = 5, colour = kolory_zi[["accent"]],
                        position = ggplot2::position_nudge(x = 0.18)) +
    ggplot2::geom_text(data = s, ggplot2::aes(label = .data$etykieta), hjust = -0.2, size = 3.1,
                       position = ggplot2::position_nudge(x = 0.18)) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.6, 1))) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = NULL, y = os, title = tytul,
                  subtitle = paste0("Linia przerywana: \u015brednia og\u00f3lna ", liczba_pl(ogolna, 2L))) +
    theme_zi()
}

#' Siatka par do miary theta
#'
#' Kazda komorka to para (osoba z grupy pierwszej, osoba z grupy drugiej);
#' napis w komorce podaje wynik porownania: ">" gdy wartosc z grupy pierwszej
#' jest wieksza, "=" przy remisie, "<" gdy mniejsza. Podtytul podaje liczby
#' par i theta = (liczba ">" + polowa remisow) / liczba par.
#' @param x Wartosci grupy pierwszej.
#' @param y Wartosci grupy drugiej.
#' @param nazwy Nazwy obu grup.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_theta(c(8, 10), c(6, 10))
wykres_theta <- function(x, y, nazwy = c("Grupa 1", "Grupa 2"), tytul = NULL) {
  d <- expand.grid(i = seq_along(x), j = seq_along(y))
  d$wx <- x[d$i]; d$wy <- y[d$j]
  d$wynik <- factor(ifelse(d$wx > d$wy, ">", ifelse(d$wx == d$wy, "=", "<")), levels = c(">", "=", "<"))
  wieksze <- sum(d$wynik == ">"); remisy <- sum(d$wynik == "="); mniejsze <- sum(d$wynik == "<")
  theta <- (wieksze + remisy / 2) / nrow(d)
  ggplot2::ggplot(d, ggplot2::aes(x = factor(.data$wx), y = factor(.data$wy))) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$wynik), colour = "white", show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$wynik), size = 5) +
    ggplot2::scale_fill_manual(values = c(">" = kolory_zi[["secondary"]], "=" = kolory_zi[["light"]],
                                          "<" = kolory_zi[["warning"]])) +
    ggplot2::labs(x = nazwy[1], y = nazwy[2], title = tytul,
                  subtitle = paste0(nrow(d), " par: ", wieksze, " \u201e>\u201d, ", remisy, " \u201e=\u201d, ",
                                    mniejsze, " \u201e<\u201d; \u03b8 = (", wieksze, " + ", liczba_pl(remisy / 2, 1L),
                                    ")/", nrow(d), " = ", liczba_pl(theta, 2L))) +
    theme_zi() + ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Liczebnosci obserwowane i oczekiwane w tabeli
#'
#' Dla kazdej komorki tabeli dwa slupki: liczebnosc obserwowana O (pelny) i
#' oczekiwana przy niezaleznosci E (jasny); napisy podaja obie liczby.
#' @param tabela Macierz liczebnosci z nazwami wierszy i kolumn.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_obserwowane_oczekiwane(matrix(c(18, 12, 32, 8), 2, byrow = TRUE,
#'        dimnames = list(c("A", "B"), c("Tak", "Nie"))))
wykres_obserwowane_oczekiwane <- function(tabela, tytul = NULL) {
  stopifnot(is.matrix(tabela), !is.null(rownames(tabela)), !is.null(colnames(tabela)))
  E <- outer(rowSums(tabela), colSums(tabela)) / sum(tabela)
  komorki <- expand.grid(w = rownames(tabela), k = colnames(tabela), stringsAsFactors = FALSE)
  rodzaje <- c("Obserwowana O", "Oczekiwana E")
  d <- rbind(
    data.frame(komorki, rodzaj = rodzaje[1], y = as.vector(tabela), etykieta = paste0("O ", as.vector(tabela))),
    data.frame(komorki, rodzaj = rodzaje[2], y = as.vector(E), etykieta = paste0("E ", liczba_pl(as.vector(E), 2L))))
  d$rodzaj <- factor(d$rodzaj, levels = rodzaje)
  d$k <- factor(d$k, levels = colnames(tabela))
  d$w <- factor(d$w, levels = rownames(tabela))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$k, y = .data$y, fill = .data$rodzaj)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75, colour = kolory_zi[["dark"]]) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), position = ggplot2::position_dodge(width = 0.8),
                       vjust = -0.4, size = 3) +
    ggplot2::facet_wrap(~w, nrow = 1) +
    ggplot2::scale_fill_manual(values = c(kolory_zi[["primary"]], "white"), name = NULL) +
    ggplot2::scale_y_continuous(labels = os_pl, expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = NULL, y = "Liczba os\u00f3b", title = tytul) +
    theme_zi()
}
