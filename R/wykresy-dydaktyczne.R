# Wykresy dydaktyczne wspólne dla wykładów i ćwiczeń. Funkcje przyjmują
# polskie etykiety z materiału; słowny odczyt wykresu zapisuje materiał.

etykiety_zi <- list(
  pomiar = "Pomiar osoby",
  srednia = "\u015arednia",
  mediana = "Mediana",
  liczba_osob = "Liczba os\u00f3b",
  gestosc = "G\u0119sto\u015b\u0107 [1/min]",
  przed = "Przed",
  po = "Po"
)

liczba_pl <- function(x, cyfry = 1L) {
  wynik <- formatC(x, format = "f", digits = cyfry, decimal.mark = ",")
  sub("^-", "\u2212", wynik)
}

os_pl <- function(x) {
  wynik <- format(x, decimal.mark = ",", trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  wynik[is.na(x)] <- NA
  sub("^-", "\u2212", wynik)
}

zawin_tekst <- function(x, szerokosc) {
  vapply(x, function(s) paste(strwrap(s, width = szerokosc), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

#' Diagram etapow rozumowania badawczego
#'
#' Rysuje etapy od gory do dolu, polaczone strzalkami. Kazdy etap moze miec
#' krotkie pytanie oraz grupe (np. plan badania i analiza danych), zaznaczona
#' odcieniem tla i nazwa w nawiasie.
#' @param etapy Nazwy etapow w kolejnosci rozumowania.
#' @param opisy Krotkie pytania lub opisy etapow; domyslnie puste.
#' @param grupy Nazwy grup etapow; domyslnie jedna grupa bez nazwy.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' diagram_etapow(c("Problem", "Pomiar", "Wniosek"),
#'   c("Jaka decyzja?", "Co obserwujemy?", "Co wolno stwierdzic?"))
diagram_etapow <- function(etapy, opisy = NULL, grupy = NULL, tytul = NULL) {
  stopifnot(is.character(etapy), length(etapy) >= 2L)
  n <- length(etapy)
  if (is.null(opisy)) opisy <- rep("", n)
  if (is.null(grupy)) grupy <- rep("", n)
  stopifnot(length(opisy) == n, length(grupy) == n)
  d <- data.frame(y = rev(seq_len(n)), numer = seq_len(n), etap = etapy,
                  opis = opisy, grupa = factor(grupy, levels = unique(grupy)))
  d$etykieta <- paste0(d$numer, ". ", d$etap,
                       ifelse(nzchar(grupy), paste0(" (", grupy, ")"), ""),
                       ifelse(nzchar(d$opis), paste0("\n", zawin_tekst(d$opis, 70)), ""))
  strzalki <- data.frame(y = d$y[-n] - 0.36, yend = d$y[-1] + 0.36)
  tla <- c(kolory_zi[["light"]], "#DCEBF5", "#FBE8D8", "#E3F2EA")
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_tile(ggplot2::aes(x = 5, y = .data$y, fill = .data$grupa),
                       width = 10, height = 0.72, colour = kolory_zi[["primary"]],
                       linewidth = 0.4, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(x = 0.2, y = .data$y, label = .data$etykieta),
                       hjust = 0, size = 3.2, lineheight = 0.95, colour = kolory_zi[["dark"]]) +
    ggplot2::geom_segment(data = strzalki,
                          ggplot2::aes(x = 5, xend = 5, y = .data$y, yend = .data$yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "cm")),
                          colour = kolory_zi[["dark"]]) +
    ggplot2::scale_fill_manual(values = rep(tla, length.out = nlevels(d$grupa))) +
    ggplot2::scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    ggplot2::labs(title = tytul) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  p
}

#' Przeplyw proby od zaproszenia do analizy
#'
#' Poziome slupki podaja liczbe jednostek na kolejnych etapach badania oraz
#' odsetek wzgledem pierwszego etapu.
#' @param etapy Nazwy etapow w kolejnosci.
#' @param n Liczby jednostek na etapach.
#' @param os Podpis osi liczebnosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_przeplyw_proby(c("Zaproszeni", "Rozpoczeli", "Ukonczyli"), c(300, 180, 150))
wykres_przeplyw_proby <- function(etapy, n, os = etykiety_zi$liczba_osob, tytul = NULL) {
  stopifnot(length(etapy) == length(n), length(n) >= 2L, all(n >= 0))
  d <- data.frame(etap = factor(etapy, levels = rev(etapy)), n = n)
  d$etykieta <- paste0(d$n, " (", liczba_pl(100 * d$n / d$n[1], 1L), "% z ", d$n[1], ")")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$etap)) +
    ggplot2::geom_col(fill = kolory_zi[["secondary"]], width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), hjust = -0.08, size = 3.4) +
    ggplot2::scale_x_continuous(labels = os_pl, expand = ggplot2::expansion(mult = c(0, 0.6))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = os, y = NULL, title = tytul) +
    theme_zi() +
    ggplot2::theme(plot.title.position = "plot")
}

#' Pomiary osob ze srednia i mediana
#'
#' Kazda osoba jest kolem na osi wartosci; romb oznacza srednia, a pionowa
#' kreska mediane grupy. Rozne ksztalty pozwalaja odczytac wykres bez koloru.
#' @param x Wartosci liczbowe; braki sa pomijane.
#' @param grupa Opcjonalne etykiety grup lub serii.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_srednia_mediana(c(2, 3, 3, 20), os = "Czas [min]")
wykres_srednia_mediana <- function(x, grupa = NULL, os = "Warto\u015b\u0107", tytul = NULL) {
  if (is.null(grupa)) grupa <- rep(" ", length(x))
  stopifnot(is.numeric(x), length(grupa) == length(x))
  d <- data.frame(x = x, grupa = factor(grupa, levels = unique(grupa)))
  d <- d[is.finite(d$x), , drop = FALSE]
  s <- do.call(rbind, lapply(split(d$x, d$grupa), function(v)
    data.frame(srednia = mean(v), mediana = stats::median(v))))
  s$grupa <- factor(rownames(s), levels = levels(d$grupa))
  miary <- rbind(
    data.frame(grupa = s$grupa, wartosc = s$srednia, miara = etykiety_zi$srednia),
    data.frame(grupa = s$grupa, wartosc = s$mediana, miara = etykiety_zi$mediana))
  d$miara <- etykiety_zi$pomiar
  poziomy <- c(etykiety_zi$pomiar, etykiety_zi$srednia, etykiety_zi$mediana)
  ksztalty <- stats::setNames(c(16, 18, 124), poziomy)
  kolory <- stats::setNames(unname(kolory_zi[c("primary", "accent", "dark")]), poziomy)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$grupa)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$miara, colour = .data$miara), size = 2.8,
                        position = ggplot2::position_jitter(width = 0, height = 0.1, seed = 2026)) +
    ggplot2::geom_point(data = miary, ggplot2::aes(x = .data$wartosc, shape = .data$miara,
                        colour = .data$miara), size = 5) +
    ggplot2::scale_shape_manual(values = ksztalty, breaks = poziomy, name = NULL) +
    ggplot2::scale_colour_manual(values = kolory, breaks = poziomy, name = NULL) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = NULL, title = tytul) +
    theme_zi()
}

#' Estymaty z przedzialami na wspolnej osi
#'
#' Poziomy odcinek przedstawia przedzial, romb estymate, a liczby przy koncach
#' odcinka podaja granice. Linia przerywana oznacza wartosc odniesienia.
#' @param etykieta Nazwy wierszy (np. wariantow lub prob).
#' @param estymata Wartosci punktowe.
#' @param dolna,gorna Granice przedzialow.
#' @param odniesienie Opcjonalna wartosc odniesienia, np. 0.
#' @param os Podpis osi.
#' @param cyfry Liczba miejsc po przecinku w etykietach granic.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_przedzialy(c("Precyzyjny", "Szeroki"), c(2, 2), c(1.7, -3), c(2.3, 7),
#'   odniesienie = 0, os = "Roznica [min]")
wykres_przedzialy <- function(etykieta, estymata, dolna, gorna, odniesienie = NULL,
                              os = "Warto\u015b\u0107", cyfry = 1L, tytul = NULL) {
  n <- length(etykieta)
  stopifnot(n >= 1L, length(estymata) == n, length(dolna) == n, length(gorna) == n,
            all(dolna <= estymata), all(estymata <= gorna))
  d <- data.frame(etykieta = factor(etykieta, levels = rev(etykieta)),
                  estymata = estymata, dolna = dolna, gorna = gorna)
  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$etykieta))
  if (!is.null(odniesienie))
    p <- p + ggplot2::geom_vline(xintercept = odniesienie, linetype = 2, colour = "gray40")
  p +
    ggplot2::geom_segment(ggplot2::aes(x = .data$dolna, xend = .data$gorna, yend = .data$etykieta),
                          linewidth = 1.1, colour = kolory_zi[["primary"]]) +
    ggplot2::geom_point(ggplot2::aes(x = .data$estymata), shape = 18, size = 4.5,
                        colour = kolory_zi[["accent"]]) +
    ggplot2::geom_text(ggplot2::aes(x = .data$dolna, label = liczba_pl(.data$dolna, cyfry)),
                       vjust = 2, size = 3.2) +
    ggplot2::geom_text(ggplot2::aes(x = .data$gorna, label = liczba_pl(.data$gorna, cyfry)),
                       vjust = 2, size = 3.2) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = NULL, title = tytul) +
    theme_zi()
}

#' Roznica zmian w dwoch grupach
#'
#' Linie lacza pomiar przed i po w dwoch grupach. Linia przerywana pokazuje
#' przebieg pierwszej grupy przy zalozeniu zmiany rownej zmianie grupy drugiej.
#' @param przed,po Dwuelementowe wektory srednich przed i po zmianie.
#' @param grupy Nazwy dwoch grup; pierwsza grupa przechodzi interwencje.
#' @param kontrfakt Nazwa linii przebiegu przy trendzie drugiej grupy.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_roznica_zmian(c(10, 8), c(7, 7), c("Filia A", "Filia B"), "A przy trendzie B")
wykres_roznica_zmian <- function(przed, po, grupy, kontrfakt, os = "Warto\u015b\u0107",
                                 tytul = NULL) {
  stopifnot(length(przed) == 2L, length(po) == 2L, length(grupy) == 2L)
  poziomy <- c(grupy, kontrfakt)
  d <- data.frame(moment = factor(rep(c(etykiety_zi$przed, etykiety_zi$po), 3L),
                                  levels = c(etykiety_zi$przed, etykiety_zi$po)),
                  wartosc = c(przed[1], po[1], przed[2], po[2], przed[1], przed[1] + po[2] - przed[2]),
                  seria = factor(rep(poziomy, each = 2L), levels = poziomy))
  d$etykieta <- liczba_pl(d$wartosc, 0L)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$moment, y = .data$wartosc, group = .data$seria)) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$seria, colour = .data$seria), linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$seria, colour = .data$seria), size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), nudge_x = 0.08, hjust = 0, size = 3.3) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid", "dashed"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17, 1), name = NULL) +
    ggplot2::scale_colour_manual(values = unname(kolory_zi[c("primary", "accent", "dark")]), name = NULL) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = NULL, y = os, title = tytul) +
    theme_zi()
}

#' Dwa rozklady o tej samej sredniej i roznym rozrzucie
#'
#' Krzywe gestosci rozkladu normalnego o wspolnej sredniej i roznych
#' odchyleniach standardowych; linia pionowa oznacza srednia.
#' @param srednia Wspolna srednia.
#' @param sd Wektor odchylen standardowych.
#' @param etykiety Nazwy krzywych.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_rowne_srednie(8, c(1, 4), c("SD = 1", "SD = 4"), os = "Czas [min]")
wykres_rowne_srednie <- function(srednia, sd, etykiety, os = "Warto\u015b\u0107", tytul = NULL) {
  stopifnot(length(srednia) == 1L, length(sd) == length(etykiety), all(sd > 0))
  x <- seq(srednia - 4 * max(sd), srednia + 4 * max(sd), length.out = 400)
  d <- do.call(rbind, lapply(seq_along(sd), function(i)
    data.frame(x = x, gestosc = stats::dnorm(x, srednia, sd[i]), krzywa = etykiety[i])))
  d$krzywa <- factor(d$krzywa, levels = etykiety)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$gestosc)) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$krzywa, colour = .data$krzywa), linewidth = 1) +
    ggplot2::geom_vline(xintercept = srednia, colour = "gray40") +
    ggplot2::scale_colour_manual(values = rep(unname(kolory_zi[c("primary", "accent", "success")]),
                                              length.out = length(sd)), name = NULL) +
    ggplot2::scale_linetype_manual(values = rep(c("solid", "dashed", "dotted"), length.out = length(sd)),
                                   name = NULL) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = "G\u0119sto\u015b\u0107", title = tytul) +
    theme_zi()
}

#' Uproszczona piramida dowodow
#'
#' Poziomy rysowane od podstawy do wierzcholka; kazdy wyzszy poziom jest wezszy.
#' @param poziomy Nazwy poziomow od podstawy do wierzcholka.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' diagram_piramida_dowodow(c("Opinia", "Opis przypadku", "Badanie z randomizacja"))
diagram_piramida_dowodow <- function(poziomy, tytul = NULL) {
  n <- length(poziomy)
  stopifnot(is.character(poziomy), n >= 2L)
  szer <- seq(10, 6, length.out = n)
  d <- data.frame(xmin = 5 - szer / 2, xmax = 5 + szer / 2, ymin = seq_len(n) - 1,
                  ymax = seq_len(n) - 0.08, etykieta = paste0(seq_len(n), ". ", poziomy))
  ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                    ymin = .data$ymin, ymax = .data$ymax),
                       fill = kolory_zi[["light"]], colour = kolory_zi[["primary"]], linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(x = 5, y = (.data$ymin + .data$ymax) / 2, label = .data$etykieta),
                       size = 3.3, colour = kolory_zi[["dark"]]) +
    ggplot2::labs(title = tytul) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

#' Wszystkie pomiary na jednej osi z kwartylami
#'
#' Kazdy punkt to jedna obserwacja rozsunieta w pionie; pionowe linie oznaczaja
#' pierwszy kwartyl, mediane i trzeci kwartyl (kwantyl typu 7).
#' @param x Wartosci liczbowe; braki sa pomijane.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_punkty_os(c(2, 4, 5, 7, 8, 13, 30), os = "Czas [min]")
wykres_punkty_os <- function(x, os = "Warto\u015b\u0107", tytul = NULL) {
  x <- x[is.finite(x)]
  stopifnot(length(x) >= 4L)
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), type = 7, names = FALSE)
  linie <- data.frame(wartosc = q, miara = factor(c("Q1", etykiety_zi$mediana, "Q3"),
                                                   levels = c("Q1", etykiety_zi$mediana, "Q3")))
  ggplot2::ggplot(data.frame(x = x, y = 0), ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(alpha = 0.65, colour = kolory_zi[["primary"]], size = 1.8,
                        position = ggplot2::position_jitter(width = 0, height = 0.3, seed = 2026)) +
    ggplot2::geom_vline(data = linie, ggplot2::aes(xintercept = .data$wartosc, linetype = .data$miara),
                        colour = kolory_zi[["dark"]]) +
    ggplot2::scale_linetype_manual(values = c("dashed", "solid", "dotdash"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = NULL, limits = c(-0.5, 0.5)) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = NULL, title = tytul, subtitle = paste0("N = ", length(x))) +
    theme_zi()
}

#' Wykres pudelkowy z punktami i granica wasa
#'
#' Pudelko obejmuje kwartyle, kreska w srodku oznacza mediane, a punkty
#' pokazuja wszystkie obserwacje. Linia kropkowana oznacza granice
#' Q3 + 1,5 IQR, powyzej ktorej punkty leza poza wasem.
#' @param x Wartosci liczbowe; braki sa pomijane.
#' @param grupa Opcjonalne etykiety grup.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_pudelkowy(c(4.5, 6.5, 8, 35), os = "Czas [min]")
wykres_pudelkowy <- function(x, grupa = NULL, os = "Warto\u015b\u0107", tytul = NULL) {
  if (is.null(grupa)) grupa <- rep(" ", length(x))
  stopifnot(is.numeric(x), length(grupa) == length(x))
  d <- data.frame(x = x, grupa = factor(grupa, levels = unique(grupa)))
  d <- d[is.finite(d$x), , drop = FALSE]
  granice <- do.call(rbind, lapply(split(d$x, d$grupa), function(v) {
    q <- stats::quantile(v, c(0.25, 0.75), type = 7, names = FALSE)
    data.frame(granica = q[2] + 1.5 * diff(q))
  }))
  granice$grupa <- factor(rownames(granice), levels = levels(d$grupa))
  granice$etykieta <- paste0("Q3 + 1,5 IQR = ", liczba_pl(granice$granica, 1L))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$grupa)) +
    ggplot2::geom_boxplot(width = 0.45, outlier.shape = NA, fill = kolory_zi[["light"]],
                          colour = kolory_zi[["dark"]], orientation = "y") +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2.4,
                        position = ggplot2::position_jitter(width = 0, height = 0.08, seed = 2026)) +
    ggplot2::geom_segment(data = granice, ggplot2::aes(x = .data$granica, xend = .data$granica,
                          y = as.numeric(.data$grupa) - 0.35, yend = as.numeric(.data$grupa) + 0.35),
                          linetype = "dotted", colour = kolory_zi[["accent"]], linewidth = 0.9) +
    ggplot2::geom_text(data = granice, ggplot2::aes(x = .data$granica, y = as.numeric(.data$grupa) + 0.42,
                       label = .data$etykieta), size = 3, hjust = 0.5, vjust = 0) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = NULL, title = tytul) +
    theme_zi()
}

#' Histogram o jawnej szerokosci przedzialu
#'
#' Przedzialy zaczynaja sie od wartosci `poczatek`; podtytul podaje N i
#' szerokosc przedzialu. Skala "gestosc" przedstawia pole slupkow rowne 1.
#' @param x Wartosci liczbowe; braki sa pomijane.
#' @param szerokosc Szerokosc przedzialu w jednostkach x.
#' @param poczatek Poczatek pierwszego przedzialu.
#' @param skala "liczebnosc" albo "gestosc".
#' @param os Podpis osi wartosci.
#' @param jednostka Jednostka szerokosci przedzialu w podtytule.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_histogram(c(2, 4, 5, 7, 8, 13, 30), szerokosc = 5, os = "Czas [min]")
wykres_histogram <- function(x, szerokosc, poczatek = 0, skala = c("liczebnosc", "gestosc"),
                             os = "Warto\u015b\u0107", jednostka = "min", tytul = NULL) {
  skala <- match.arg(skala)
  x <- x[is.finite(x)]
  stopifnot(length(x) >= 2L, is.numeric(szerokosc), szerokosc > 0)
  estetyka <- if (skala == "gestosc") ggplot2::aes(x = .data$x, y = ggplot2::after_stat(.data$density)) else
    ggplot2::aes(x = .data$x)
  ggplot2::ggplot(data.frame(x = x), estetyka) +
    ggplot2::geom_histogram(binwidth = szerokosc, boundary = poczatek, closed = "right",
                            fill = kolory_zi[["secondary"]], colour = "white") +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = if (skala == "gestosc") etykiety_zi$gestosc else etykiety_zi$liczba_osob,
                  title = tytul,
                  subtitle = paste0("N = ", length(x), "; szeroko\u015b\u0107 przedzia\u0142u ",
                                    liczba_pl(szerokosc, if (szerokosc %% 1 == 0) 0L else 1L), " ", jednostka)) +
    theme_zi()
}

#' Te same obserwacje w kilku skalach
#'
#' Kazdy panel jest osia jednej reprezentacji (minuty, rangi, min-max,
#' standaryzacja z, log2); etykiety punktow wskazuja te same osoby.
#' @param x Dodatnie wartosci liczbowe bez brakow.
#' @param id Etykiety obserwacji.
#' @param jednostka Nazwa jednostki surowych wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_transformacje(c(2, 4, 4, 8), c("a", "b", "c", "d"))
wykres_transformacje <- function(x, id, jednostka = "minuty", tytul = NULL) {
  stopifnot(is.numeric(x), all(is.finite(x)), all(x > 0), length(id) == length(x), stats::sd(x) > 0)
  skale <- c(paste0("Warto\u015b\u0107 [", jednostka, "]"), "Ranga", "Min\u2013max (0\u20131)",
             "Standaryzacja z", "log2")
  wartosci <- list(x, rank(x), (x - min(x)) / diff(range(x)), (x - mean(x)) / stats::sd(x), log2(x))
  d <- do.call(rbind, lapply(seq_along(skale), function(i) {
    z <- data.frame(skala = skale[i], wartosc = wartosci[[i]], id = id)
    stats::aggregate(id ~ skala + wartosc, z, paste, collapse = ", ")
  }))
  d$skala <- factor(d$skala, levels = skale)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$wartosc, y = 0)) +
    ggplot2::geom_hline(yintercept = 0, colour = "gray75") +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = .data$id), vjust = -0.9, size = 3.2,
                       position = ggplot2::position_jitter(width = 0, height = 0, seed = 1)) +
    ggplot2::facet_wrap(~skala, ncol = 1, scales = "free_x") +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(breaks = NULL, limits = c(-0.5, 1)) +
    ggplot2::labs(x = NULL, y = NULL, title = tytul) +
    theme_zi()
}

#' Odsetek brakow zmiennych w grupach
#'
#' Panel dla kazdej grupy; slupek podaje odsetek brakow zmiennej, a napis
#' liczbe brakow i liczbe osob w grupie.
#' @param dane Ramka danych.
#' @param zmienne Nazwy kolumn do sprawdzenia.
#' @param grupa Nazwa kolumny grupujacej.
#' @param etykiety Czytelne nazwy zmiennych; domyslnie nazwy kolumn.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' d <- data.frame(g = c("a", "a", "b", "b"), x = c(1, NA, 2, 3), y = c(NA, NA, 1, 1))
#' wykres_braki(d, c("x", "y"), "g")
wykres_braki <- function(dane, zmienne, grupa, etykiety = zmienne, tytul = NULL) {
  stopifnot(is.data.frame(dane), all(c(zmienne, grupa) %in% names(dane)), length(etykiety) == length(zmienne))
  d <- do.call(rbind, lapply(split(dane, dane[[grupa]]), function(z)
    data.frame(grupa = z[[grupa]][1], zmienna = etykiety, braki = vapply(zmienne,
      function(v) sum(is.na(z[[v]])), integer(1)), n = nrow(z))))
  d$procent <- 100 * d$braki / d$n
  d$zmienna <- factor(d$zmienna, levels = rev(etykiety))
  d$etykieta <- paste0(d$braki, "/", d$n, " (", liczba_pl(d$procent, 1L), "%)")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$procent, y = .data$zmienna)) +
    ggplot2::geom_col(fill = kolory_zi[["warning"]], width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), hjust = -0.08, size = 3.2) +
    ggplot2::facet_wrap(~grupa, ncol = 1) +
    ggplot2::scale_x_continuous(labels = os_pl, limits = c(0, max(10, max(d$procent) * 1.9))) +
    ggplot2::labs(x = "Braki [% os\u00f3b w grupie]", y = NULL, title = tytul) +
    theme_zi()
}

#' Os czasu zdarzen w logu
#'
#' Kazdy wiersz to jedna osoba lub sesja; odcinek biegnie od pierwszego do
#' ostatniego zdarzenia, a punkty z podpisami oznaczaja zdarzenia.
#' @param log Ramka z kolumnami id, minuta i zdarzenie.
#' @param os Podpis osi czasu.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot.
#' @export
#' @examples
#' wykres_log(data.frame(id = c("u01", "u01"), minuta = c(0, 6.5), zdarzenie = c("start", "koniec")))
wykres_log <- function(log, os = "Minuta sesji", tytul = NULL) {
  stopifnot(is.data.frame(log), all(c("id", "minuta", "zdarzenie") %in% names(log)))
  d <- log
  d$id <- factor(d$id, levels = rev(unique(d$id)))
  zakresy <- do.call(rbind, lapply(split(d$minuta, d$id), function(m)
    data.frame(start = min(m), koniec = max(m))))
  zakresy$id <- factor(rownames(zakresy), levels = levels(d$id))
  zakresy$etykieta <- paste0(liczba_pl(zakresy$koniec - zakresy$start, 1L), " min")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$minuta, y = .data$id)) +
    ggplot2::geom_segment(data = zakresy, ggplot2::aes(x = .data$start, xend = .data$koniec,
                          y = .data$id, yend = .data$id), linewidth = 1.2, colour = kolory_zi[["secondary"]]) +
    ggplot2::geom_point(size = 3, colour = kolory_zi[["primary"]]) +
    ggplot2::geom_text(ggplot2::aes(label = .data$zdarzenie), vjust = -1.1, size = 3.1) +
    ggplot2::geom_text(data = zakresy, ggplot2::aes(x = .data$koniec, y = .data$id, label = .data$etykieta),
                       hjust = -0.25, size = 3.1) +
    ggplot2::scale_x_continuous(labels = os_pl, expand = ggplot2::expansion(mult = c(0.05, 0.2))) +
    ggplot2::labs(x = os, y = NULL, title = tytul) +
    theme_zi()
}
