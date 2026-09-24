# Wykresy rozkładów empirycznych i teoretycznych dla wykładów i ćwiczeń.
# Funkcje zwracają obiekt ggplot; słowny odczyt wykresu zapisuje materiał.

# Liczebności przedziałów domkniętych z prawej strony: (a, a + h].
przedzialy_histogramu <- function(x, szerokosc, poczatek = NULL) {
  x <- x[is.finite(x)]
  if (is.null(poczatek)) poczatek <- szerokosc * floor(min(x) / szerokosc)
  stopifnot(poczatek <= min(x))
  k <- max(1L, ceiling((max(x) - poczatek) / szerokosc - 1e-9))
  granice <- poczatek + szerokosc * (0:k)
  n <- as.vector(table(cut(x, granice, right = TRUE, include.lowest = TRUE)))
  data.frame(od = utils::head(granice, -1), do = granice[-1], n = n)
}

# Siatka wartości do rysowania krzywych gęstości i dystrybuant.
siatka_osi <- function(od, do, n = 400L) seq(od, do, length.out = n)

#' Histogram o jawnej szerokosci przedzialu
#'
#' Przedzialy sa domkniete z prawej strony i zaczynaja sie od wartosci
#' `poczatek`; podtytul podaje N i szerokosc przedzialu. Skala "gestosc"
#' przedstawia slupki o lacznym polu 1. Opcjonalnie wyroznia przedzialy
#' lezace w zakresie `zaznacz` i dodaje krzywe modeli o parametrach
#' oszacowanych z danych.
#' @param x Wartosci liczbowe; braki sa pomijane.
#' @param szerokosc Szerokosc przedzialu w jednostkach x.
#' @param poczatek Poczatek pierwszego przedzialu.
#' @param skala "liczebnosc" albo "gestosc".
#' @param os Podpis osi wartosci.
#' @param jednostka Jednostka szerokosci przedzialu w podtytule.
#' @param tytul Tytul wykresu.
#' @param zaznacz Opcjonalny wektor dwoch granic; przedzialy histogramu
#'   miedzy granicami sa wyroznione, a podtytul podaje ich udzial.
#' @param dopasuj Krzywe modeli: "normalny" (srednia i SD danych) lub
#'   "lognormalny" (srednia i SD logarytmow danych).
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_histogram(c(2, 4, 5, 7, 8, 13, 30), szerokosc = 5, os = "Czas [min]")
wykres_histogram <- function(x, szerokosc, poczatek = 0, skala = c("liczebnosc", "gestosc"),
                             os = "Warto\u015b\u0107", jednostka = "min", tytul = NULL,
                             zaznacz = NULL, dopasuj = character()) {
  skala <- match.arg(skala)
  x <- x[is.finite(x)]
  stopifnot(length(x) >= 2L, is.numeric(szerokosc), length(szerokosc) == 1L, szerokosc > 0,
            all(dopasuj %in% c("normalny", "lognormalny")))
  d <- przedzialy_histogramu(x, szerokosc, min(poczatek, szerokosc * floor(min(x) / szerokosc)))
  mnoznik <- if (skala == "gestosc") 1 / (length(x) * szerokosc) else 1
  d$y <- d$n * mnoznik
  d$wyrozniony <- if (is.null(zaznacz)) FALSE else d$od >= min(zaznacz) - 1e-9 & d$do <= max(zaznacz) + 1e-9
  podpis <- paste0("N = ", length(x), "; szeroko\u015b\u0107 przedzia\u0142u ",
                   liczba_pl(szerokosc, if (szerokosc %% 1 == 0) 0L else 1L), " ", jednostka)
  if (!is.null(zaznacz)) {
    k <- sum(d$n[d$wyrozniony])
    podpis <- paste0(podpis, "; od ", os_pl(min(zaznacz)), " do ", os_pl(max(zaznacz)), ": ",
                     k, " z ", length(x), " (", liczba_pl(100 * k / length(x), 1L), "%)")
  }
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$od, xmax = .data$do, ymin = 0, ymax = .data$y,
                                    fill = .data$wyrozniony), colour = "white", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = kolory_zi[["secondary"]], `TRUE` = kolory_zi[["warning"]]))
  if (length(dopasuj)) {
    siatka <- siatka_osi(min(d$od), max(d$do))
    krzywe <- do.call(rbind, lapply(dopasuj, function(m) {
      y <- if (m == "normalny") stats::dnorm(siatka, mean(x), stats::sd(x)) else
        ifelse(siatka > 0, stats::dlnorm(pmax(siatka, 1e-12), mean(log(x[x > 0])), stats::sd(log(x[x > 0]))), 0)
      # Gęstość modelu w skali osi: liczebność oczekiwana = gęstość * N * h.
      data.frame(x = siatka, y = y * length(x) * szerokosc * mnoznik,
                 model = if (m == "normalny") "Model normalny" else "Model log-normalny")
    }))
    p <- p + ggplot2::geom_line(data = krzywe, ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$model),
                                colour = kolory_zi[["dark"]], linewidth = 0.8) +
      ggplot2::scale_linetype_manual(values = c("Model normalny" = "solid", "Model log-normalny" = "dashed"),
                                     name = NULL)
  }
  p +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = if (skala == "gestosc") etykiety_zi$gestosc else etykiety_zi$liczba_osob,
                  title = tytul, subtitle = podpis) +
    theme_zi()
}

#' Histogramy w panelach o wspolnej osi
#'
#' Kazdy element listy tworzy jeden panel; nazwa elementu i N sa tytulem
#' panelu. Szerokosc przedzialu moze byc wspolna albo rozna dla paneli, co
#' pozwala porownac ten sam zbior przy kilku szerokosciach.
#' @param dane Nazwana lista wektorow liczbowych.
#' @param szerokosc Szerokosc przedzialu; jedna liczba albo wartosc na panel.
#' @param poczatek Poczatek pierwszego przedzialu; domyslnie wielokrotnosc
#'   szerokosci ponizej minimum.
#' @param os Podpis osi wartosci.
#' @param srednia Czy oznaczyc srednia panelu linia przerywana.
#' @param tytul Tytul wykresu.
#' @param odniesienie Opcjonalna wartosc odniesienia (np. parametr populacji)
#'   oznaczona we wszystkich panelach linia kropkowana z opisem.
#' @param etykieta_odniesienia Opis linii odniesienia.
#' @param os_y Podpis osi liczebnosci; domyslnie liczba osob.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_histogram_panele(list(A = c(1, 2, 2, 3), B = c(1, 1, 4, 4)), 1)
wykres_histogram_panele <- function(dane, szerokosc, poczatek = NULL, os = "Warto\u015b\u0107",
                                    srednia = FALSE, tytul = NULL, odniesienie = NULL,
                                    etykieta_odniesienia = "parametr", os_y = NULL) {
  stopifnot(is.list(dane), length(dane) >= 1L, !is.null(names(dane)), all(nzchar(names(dane))))
  szerokosc <- rep_len(szerokosc, length(dane))
  panele <- vapply(seq_along(dane), function(i)
    paste0(names(dane)[i], " (N = ", sum(is.finite(dane[[i]])), ")"), character(1))
  d <- do.call(rbind, lapply(seq_along(dane), function(i) {
    z <- przedzialy_histogramu(dane[[i]], szerokosc[i], poczatek)
    z$panel <- panele[i]
    z
  }))
  d$panel <- factor(d$panel, levels = panele)
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$od, xmax = .data$do, ymin = 0, ymax = .data$n),
                       fill = kolory_zi[["secondary"]], colour = "white") +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y")
  if (srednia) {
    s <- data.frame(panel = factor(panele, levels = panele),
                    m = vapply(dane, function(x) mean(x[is.finite(x)]), numeric(1)))
    s$etykieta <- paste0("\u015brednia ", liczba_pl(s$m, 1L))
    p <- p + ggplot2::geom_vline(data = s, ggplot2::aes(xintercept = .data$m), linetype = "dashed",
                                 colour = kolory_zi[["accent"]], linewidth = 0.8) +
      ggplot2::geom_text(data = s, ggplot2::aes(x = .data$m, y = Inf, label = .data$etykieta),
                         vjust = 1.4, hjust = -0.05, size = 3.2)
  }
  if (!is.null(odniesienie)) {
    r <- data.frame(panel = factor(panele[1], levels = panele), x = odniesienie,
                    etykieta = paste0(etykieta_odniesienia, " ", os_pl(odniesienie)))
    p <- p + ggplot2::geom_vline(xintercept = odniesienie, linetype = "dotted", colour = kolory_zi[["dark"]],
                                 linewidth = 0.8) +
      ggplot2::geom_text(data = r, ggplot2::aes(x = .data$x, y = Inf, label = .data$etykieta),
                         vjust = 1.4, hjust = -0.05, size = 3.2)
  }
  p + ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = if (is.null(os_y)) etykiety_zi$liczba_osob else os_y, title = tytul) +
    theme_zi()
}

#' Czestosci kategorii z odsetkiem skumulowanym
#'
#' Slupki podaja odsetek kazdej kategorii (napis: liczebnosc i odsetek), a
#' linia z trojkatami odsetek skumulowany do danej kategorii wlacznie.
#' Odsetek skumulowany ma sens dla kategorii uporzadkowanych.
#' @param kategorie Etykiety kategorii w kolejnosci skali.
#' @param liczebnosc Liczby odpowiedzi w kategoriach.
#' @param os Podpis osi kategorii.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_czestosci(1:5, c(2, 3, 5, 7, 3), os = "Odpowiedz")
wykres_czestosci <- function(kategorie, liczebnosc, os = "Kategoria", tytul = NULL) {
  stopifnot(length(kategorie) == length(liczebnosc), all(liczebnosc >= 0), sum(liczebnosc) > 0)
  n <- sum(liczebnosc)
  d <- data.frame(k = factor(as.character(kategorie), levels = as.character(kategorie)),
                  n = liczebnosc, proc = 100 * liczebnosc / n)
  d$skum <- cumsum(d$proc)
  d$etykieta <- paste0(d$n, " (", liczba_pl(d$proc, 0L), "%)")
  d$etykieta_skum <- paste0(liczba_pl(d$skum, 0L), "%")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$k)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$proc), fill = kolory_zi[["secondary"]], width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(y = .data$proc / 2, label = .data$etykieta), size = 3.1) +
    ggplot2::geom_line(ggplot2::aes(y = .data$skum, group = 1), linetype = "dashed",
                       colour = kolory_zi[["dark"]]) +
    ggplot2::geom_point(ggplot2::aes(y = .data$skum), shape = 17, size = 2.6, colour = kolory_zi[["dark"]]) +
    ggplot2::geom_text(ggplot2::aes(y = .data$skum, label = .data$etykieta_skum), hjust = 1.3, vjust = -0.4,
                       size = 3.1) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, 110), breaks = seq(0, 100, 25)) +
    ggplot2::labs(x = os, y = "Odsetek odpowiedzi [%]", title = tytul,
                  subtitle = paste0("N = ", n, "; s\u0142upki: odsetek kategorii; tr\u00f3jk\u0105ty i linia: odsetek skumulowany")) +
    theme_zi()
}

#' Dystrybuanta empiryczna z opcjonalnym modelem
#'
#' Schodki przedstawiaja dystrybuante empiryczna: udzial obserwacji mniejszych
#' lub rownych wartosci na osi. Opcjonalnie oznacza wartosc dystrybuanty przy
#' progu i dodaje dystrybuanty modeli o parametrach oszacowanych z danych.
#' @param x Wartosci liczbowe; braki sa pomijane.
#' @param prog Opcjonalny prog c; punkt i napis podaja F(c).
#' @param dopasuj Modele: "normalny" i/lub "lognormalny".
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_dystrybuanta(c(2, 4, 6, 8, 30), prog = 8, os = "Czas [min]")
wykres_dystrybuanta <- function(x, prog = NULL, dopasuj = character(), os = "Warto\u015b\u0107", tytul = NULL) {
  x <- sort(x[is.finite(x)])
  stopifnot(length(x) >= 2L, all(dopasuj %in% c("normalny", "lognormalny")))
  zakres <- range(x)
  margines <- 0.06 * diff(zakres)
  u <- unique(x)
  Fn <- stats::ecdf(x)
  schodki <- data.frame(x = c(zakres[1] - margines, u, zakres[2] + margines),
                        F = c(0, Fn(u), 1), linia = "Dystrybuanta empiryczna")
  p <- ggplot2::ggplot() +
    ggplot2::geom_step(data = schodki, ggplot2::aes(x = .data$x, y = .data$F, linetype = .data$linia),
                       direction = "hv", colour = kolory_zi[["primary"]], linewidth = 0.9)
  typy <- c("Dystrybuanta empiryczna" = "solid")
  if (length(dopasuj)) {
    siatka <- siatka_osi(zakres[1] - margines, zakres[2] + margines)
    krzywe <- do.call(rbind, lapply(dopasuj, function(m) {
      if (m == "normalny") data.frame(x = siatka, F = stats::pnorm(siatka, mean(x), stats::sd(x)),
                                      linia = "Model normalny") else
        data.frame(x = siatka, F = stats::plnorm(pmax(siatka, 1e-12), mean(log(x[x > 0])), stats::sd(log(x[x > 0]))),
                   linia = "Model log-normalny")
    }))
    p <- p + ggplot2::geom_line(data = krzywe, ggplot2::aes(x = .data$x, y = .data$F, linetype = .data$linia),
                                colour = kolory_zi[["dark"]], linewidth = 0.7)
    typy <- c(typy, "Model normalny" = "dashed", "Model log-normalny" = "dotted")
  }
  if (!is.null(prog)) {
    punkt <- data.frame(x = prog, F = Fn(prog),
                        etykieta = paste0("F(", os_pl(prog), ") = ", liczba_pl(Fn(prog), 2L)))
    p <- p + ggplot2::geom_segment(data = punkt, ggplot2::aes(x = .data$x, xend = .data$x, y = 0, yend = .data$F),
                                   linetype = "dotted", colour = kolory_zi[["accent"]]) +
      ggplot2::geom_segment(data = punkt, ggplot2::aes(x = -Inf, xend = .data$x, y = .data$F, yend = .data$F),
                            linetype = "dotted", colour = kolory_zi[["accent"]]) +
      ggplot2::geom_point(data = punkt, ggplot2::aes(x = .data$x, y = .data$F), size = 3, shape = 18,
                          colour = kolory_zi[["accent"]]) +
      ggplot2::geom_text(data = punkt, ggplot2::aes(x = .data$x, y = .data$F, label = .data$etykieta),
                         hjust = -0.15, vjust = 1.3, size = 3.4)
  }
  p + ggplot2::scale_linetype_manual(values = typy[names(typy) %in% c(schodki$linia[1],
                                     if (length(dopasuj)) unique(krzywe$linia))], name = NULL,
                                     guide = if (length(dopasuj)) "legend" else "none") +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = os, y = "Udzia\u0142 obserwacji \u2264 warto\u015bci", title = tytul,
                  subtitle = paste0("N = ", length(x))) +
    theme_zi()
}

#' Liczebnosci i odsetki w grupach o roznych mianownikach
#'
#' Dwa panele: liczba zdarzen (np. sukcesow) i odsetek zdarzen w grupie.
#' Napisy podaja licznik z mianownikiem oraz odsetek.
#' @param grupy Nazwy grup.
#' @param licznik Liczby zdarzen w grupach.
#' @param mianownik Liczebnosci grup.
#' @param zdarzenie Nazwa zdarzenia w liczbie mnogiej, np. "sukcesy".
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_licznosci_odsetki(c("Nowi", "Doswiadczeni"), c(16, 60), c(20, 100))
wykres_licznosci_odsetki <- function(grupy, licznik, mianownik, zdarzenie = "sukcesy", tytul = NULL) {
  stopifnot(length(grupy) == length(licznik), length(licznik) == length(mianownik),
            all(mianownik > 0), all(licznik >= 0), all(licznik <= mianownik))
  g <- factor(grupy, levels = grupy)
  panele <- c(paste0("Liczba: ", zdarzenie), paste0("Odsetek [%]: ", zdarzenie, " w grupie"))
  d <- rbind(
    data.frame(g = g, panel = panele[1], y = licznik, etykieta = paste0(licznik, " z ", mianownik)),
    data.frame(g = g, panel = panele[2], y = 100 * licznik / mianownik,
               etykieta = paste0(liczba_pl(100 * licznik / mianownik, 1L), "%")))
  d$panel <- factor(d$panel, levels = panele)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$g, y = .data$y)) +
    ggplot2::geom_col(fill = kolory_zi[["secondary"]], width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), vjust = -0.4, size = 3.3) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = os_pl, expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(x = NULL, y = NULL, title = tytul) +
    theme_zi()
}

#' Odchylenie standardowe i blad standardowy wobec wielkosci proby
#'
#' Dla kazdej liczebnosci n bierze pierwsze n waznych obserwacji zbioru,
#' oblicza SD osob i SE sredniej = SD / pierwiastek z n. Linia ciagla z kolami
#' przedstawia SD, linia przerywana z trojkatami SE.
#' @param x Wartosci liczbowe w kolejnosci zbioru; braki sa pomijane.
#' @param n Wektor liczebnosci podprob (nie wiekszych od liczby waznych x).
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_sd_se(c(2, 4, 6, 8, 10, 3, 9, 5, 7, 12), c(5, 10))
wykres_sd_se <- function(x, n, os = "Minuty", tytul = NULL) {
  x <- x[is.finite(x)]
  stopifnot(all(n >= 2), all(n <= length(x)))
  sd_n <- vapply(n, function(k) stats::sd(x[seq_len(k)]), numeric(1))
  poziomy <- c("SD os\u00f3b", "SE \u015bredniej")
  d <- rbind(data.frame(n = n, y = sd_n, miara = poziomy[1]),
             data.frame(n = n, y = sd_n / sqrt(n), miara = poziomy[2]))
  d$miara <- factor(d$miara, levels = poziomy)
  d$etykieta <- liczba_pl(d$y, 2L)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$y, linetype = .data$miara, shape = .data$miara)) +
    ggplot2::geom_line(colour = kolory_zi[["primary"]]) +
    ggplot2::geom_point(size = 2.6, colour = kolory_zi[["primary"]]) +
    ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), vjust = -0.9, size = 3, show.legend = FALSE) +
    ggplot2::scale_x_log10(breaks = n) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, max(d$y) * 1.15)) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17), name = NULL) +
    ggplot2::labs(x = "Liczba os\u00f3b n (skala logarytmiczna)", y = os, title = tytul) +
    theme_zi()
}

#' Rozklad dyskretny: prawdopodobienstwa i dystrybuanta
#'
#' Lewy panel: slupki P(K = k) z wartosciami; prawy panel: schodkowa
#' dystrybuanta F(k) = P(K <= k) z punktami przy kazdym k.
#' @param k Wartosci zmiennej losowej.
#' @param p Prawdopodobienstwa wartosci k.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @param zaznacz Opcjonalne wartosci k wyroznione kolorem slupka; podtytul
#'   podaje ich laczne prawdopodobienstwo.
#' @param dystrybuanta Czy dodac panel dystrybuanty.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_rozklad_dyskretny(0:3, stats::dbinom(0:3, 3, 0.5))
wykres_rozklad_dyskretny <- function(k, p, os = "Liczba sukces\u00f3w k", tytul = NULL,
                                     zaznacz = NULL, dystrybuanta = TRUE) {
  stopifnot(length(k) == length(p), all(p >= 0), abs(sum(p) - 1) < 1e-6)
  if (!dystrybuanta) {
    d <- data.frame(k = k, y = p, etykieta = liczba_pl(p, 3L), wyrozniony = k %in% zaznacz)
    podpis <- if (length(zaznacz)) paste0("Wyr\u00f3\u017cnione warto\u015bci ", paste(zaznacz, collapse = ", "),
                                           ": \u0142\u0105cznie ", liczba_pl(sum(p[k %in% zaznacz]), 3L)) else NULL
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data$k, y = .data$y)) +
      ggplot2::geom_col(ggplot2::aes(fill = .data$wyrozniony), width = 0.6, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = .data$etykieta), vjust = -0.4, size = 3) +
      ggplot2::scale_fill_manual(values = c(`FALSE` = kolory_zi[["secondary"]], `TRUE` = kolory_zi[["warning"]])) +
      ggplot2::scale_x_continuous(breaks = k) +
      ggplot2::scale_y_continuous(labels = os_pl, expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(x = os, y = "Prawdopodobie\u0144stwo", title = tytul, subtitle = podpis) +
      theme_zi())
  }
  panele <- c("P(K = k)", "F(k) = P(K \u2264 k)")
  slupki <- data.frame(k = k, y = p, panel = panele[1], etykieta = liczba_pl(p, 3L))
  F <- cumsum(p)
  schodki <- data.frame(k = c(min(k) - 0.5, k, max(k) + 0.5), y = c(0, F, 1), panel = panele[2])
  punkty <- data.frame(k = k, y = F, panel = panele[2], etykieta = liczba_pl(F, 3L))
  for (nazwa in c("slupki", "schodki", "punkty")) {
    z <- get(nazwa)
    z$panel <- factor(z$panel, levels = panele)
    assign(nazwa, z)
  }
  ggplot2::ggplot() +
    ggplot2::geom_col(data = slupki, ggplot2::aes(x = .data$k, y = .data$y), width = 0.5,
                      fill = kolory_zi[["secondary"]]) +
    ggplot2::geom_text(data = slupki, ggplot2::aes(x = .data$k, y = .data$y, label = .data$etykieta),
                       vjust = -0.4, size = 3.2) +
    ggplot2::geom_step(data = schodki, ggplot2::aes(x = .data$k, y = .data$y), direction = "hv",
                       colour = kolory_zi[["primary"]]) +
    ggplot2::geom_point(data = punkty, ggplot2::aes(x = .data$k, y = .data$y), size = 2.4,
                        colour = kolory_zi[["primary"]]) +
    ggplot2::geom_text(data = punkty, ggplot2::aes(x = .data$k, y = .data$y, label = .data$etykieta),
                       vjust = -0.8, hjust = 0.9, size = 3.2) +
    ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::scale_x_continuous(breaks = k) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = os, y = "Prawdopodobie\u0144stwo", title = tytul) +
    theme_zi()
}

#' Gestosc z polem miedzy dwiema wartosciami
#'
#' Rysuje krzywa gestosci na zadanym zakresie, wyroznia pole miedzy a i b
#' (podtytul podaje jego wartosc) i opcjonalnie pionowe linie z opisem.
#' @param gestosc Funkcja gestosci jednej zmiennej.
#' @param od,do Zakres osi.
#' @param a,b Granice wyroznionego pola; NULL pomija pole.
#' @param linie Opcjonalne polozenia pionowych linii.
#' @param etykiety_linii Opisy linii.
#' @param os Podpis osi wartosci.
#' @param os_y Podpis osi gestosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_gestosc_pole(function(x) stats::dexp(x, 0.1), 0, 40, a = 5, b = 10)
wykres_gestosc_pole <- function(gestosc, od, do, a = NULL, b = NULL, linie = NULL,
                                etykiety_linii = NULL, os = "Warto\u015b\u0107", os_y = "G\u0119sto\u015b\u0107", tytul = NULL) {
  stopifnot(is.function(gestosc), od < do)
  s <- siatka_osi(od, do)
  d <- data.frame(x = s, y = gestosc(s))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y))
  podpis <- NULL
  if (!is.null(a) && !is.null(b)) {
    w <- d[d$x >= a & d$x <= b, , drop = FALSE]
    pole <- stats::integrate(gestosc, a, b)$value
    podpis <- paste0("Pole od ", os_pl(a), " do ", os_pl(b), " = ", liczba_pl(pole, 3L))
    p <- p + ggplot2::geom_area(data = w, fill = kolory_zi[["warning"]], alpha = 0.6)
  }
  p <- p + ggplot2::geom_line(colour = kolory_zi[["dark"]], linewidth = 0.9)
  if (!is.null(linie)) {
    l <- data.frame(x = linie, etykieta = if (is.null(etykiety_linii)) os_pl(linie) else etykiety_linii)
    p <- p + ggplot2::geom_vline(data = l, ggplot2::aes(xintercept = .data$x), linetype = "dashed",
                                 colour = kolory_zi[["accent"]]) +
      ggplot2::geom_text(data = l, ggplot2::aes(x = .data$x, y = Inf, label = .data$etykieta),
                         hjust = -0.05, vjust = 1.5, size = 3.2)
  }
  p + ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = os_y, title = tytul, subtitle = podpis) +
    theme_zi()
}

#' Rozklad normalny standardowy i rozklady t
#'
#' Krzywe gestosci N(0, 1) i rozkladow t o podanych stopniach swobody,
#' rozroznione typem linii; legenda podaje kwantyl rzedu `kwantyl` kazdego
#' rozkladu, a krotkie kreski na osi oznaczaja jego polozenie.
#' @param df Stopnie swobody rozkladow t.
#' @param kwantyl Rzad kwantyla podanego w legendzie.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_t_z(c(3, 15))
wykres_t_z <- function(df, kwantyl = 0.975, os = "Warto\u015b\u0107 statystyki", tytul = NULL) {
  stopifnot(all(df > 0), kwantyl > 0.5, kwantyl < 1)
  s <- siatka_osi(-4.5, 4.5)
  nazwy <- c(paste0("N(0, 1): ", liczba_pl(stats::qnorm(kwantyl), 2L)),
             paste0("t, df = ", df, ": ", liczba_pl(stats::qt(kwantyl, df), 2L)))
  d <- rbind(data.frame(x = s, y = stats::dnorm(s), rozklad = nazwy[1]),
             do.call(rbind, lapply(seq_along(df), function(i)
               data.frame(x = s, y = stats::dt(s, df[i]), rozklad = nazwy[i + 1]))))
  d$rozklad <- factor(d$rozklad, levels = nazwy)
  q <- data.frame(x = c(stats::qnorm(kwantyl), stats::qt(kwantyl, df)), rozklad = factor(nazwy, levels = nazwy))
  typy <- c("solid", "dashed", "dotdash", "dotted", "longdash")[seq_along(nazwy)]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$rozklad)) +
    ggplot2::geom_line(colour = kolory_zi[["dark"]], linewidth = 0.8) +
    ggplot2::geom_segment(data = q, ggplot2::aes(x = .data$x, xend = .data$x, y = 0, yend = 0.06,
                                                 linetype = .data$rozklad), colour = kolory_zi[["dark"]],
                          linewidth = 0.9) +
    ggplot2::scale_linetype_manual(values = typy, name = paste0("Kwantyl ", liczba_pl(kwantyl, 3L))) +
    ggplot2::scale_x_continuous(labels = os_pl, breaks = -4:4) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::labs(x = os, y = "G\u0119sto\u015b\u0107", title = tytul) +
    theme_zi()
}

#' Rozklad odniesienia z polem ogonow
#'
#' Krzywa rozkladu statystyki przy hipotezie zerowej z pionowymi liniami przy
#' obserwowanej wartosci. Dla rozkladow t i normalnego pole obu ogonow
#' (|T| >= |statystyka|), dla chi-kwadrat i F pole prawego ogona; podtytul
#' podaje to pole, czyli wartosc p.
#' @param statystyka Obserwowana wartosc statystyki.
#' @param rozklad "t", "normalny", "chi2" albo "F".
#' @param df Stopnie swobody (dla F wektor dwoch liczb).
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_ogony(3, "t", 99)
wykres_ogony <- function(statystyka, rozklad = c("t", "normalny", "chi2", "F"), df = NULL,
                         os = "Warto\u015b\u0107 statystyki", tytul = NULL) {
  rozklad <- match.arg(rozklad)
  gest <- switch(rozklad,
    t = function(x) stats::dt(x, df), normalny = stats::dnorm,
    chi2 = function(x) stats::dchisq(x, df), F = function(x) stats::df(x, df[1], df[2]))
  dwustronny <- rozklad %in% c("t", "normalny")
  if (dwustronny) {
    g <- max(4, abs(statystyka) + 1)
    s <- siatka_osi(-g, g)
    pole <- 2 * (if (rozklad == "t") stats::pt(-abs(statystyka), df) else stats::pnorm(-abs(statystyka)))
    ogon <- abs(s) >= abs(statystyka)
    linie <- c(-abs(statystyka), abs(statystyka))
  } else {
    gora <- max(statystyka * 1.4, if (rozklad == "chi2") stats::qchisq(0.999, df) else stats::qf(0.995, df[1], df[2]))
    s <- siatka_osi(0.001, gora)
    pole <- if (rozklad == "chi2") stats::pchisq(statystyka, df, lower.tail = FALSE) else
      stats::pf(statystyka, df[1], df[2], lower.tail = FALSE)
    ogon <- s >= statystyka
    linie <- statystyka
  }
  d <- data.frame(x = s, y = gest(s))
  d$y[!is.finite(d$y)] <- NA
  d$strona <- ifelse(d$x < 0, "lewy", "prawy")
  w <- d[ogon, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_area(data = w, ggplot2::aes(group = .data$strona), fill = kolory_zi[["warning"]], alpha = 0.7) +
    ggplot2::geom_line(colour = kolory_zi[["dark"]], linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = linie, linetype = "dashed", colour = kolory_zi[["accent"]]) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = "G\u0119sto\u015b\u0107", title = tytul,
                  subtitle = paste0(if (dwustronny) "Pole obu ogon\u00f3w" else "Pole prawego ogona",
                                    " = ", format_p(pole))) +
    theme_zi()
}

format_p <- function(p) {
  if (p < 0.0001) "< 0,0001" else liczba_pl(p, 4L)
}

#' Rozklady estymatora wobec parametru
#'
#' Panel dla kazdego estymatora: krzywa normalna o podanej sredniej i SD
#' (rozklad estymat w wielu probach) oraz linia przerywana parametru.
#' @param srednia Srednie rozkladow estymatorow.
#' @param sd Odchylenia standardowe rozkladow estymatorow.
#' @param etykiety Nazwy paneli.
#' @param parametr Wartosc parametru.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_rozklady_estymatora(c(10, 12), c(0.5, 0.5), c("A", "B"), 10)
wykres_rozklady_estymatora <- function(srednia, sd, etykiety, parametr, os = "Warto\u015b\u0107 estymaty", tytul = NULL) {
  stopifnot(length(srednia) == length(sd), length(sd) == length(etykiety), all(sd > 0))
  od <- min(srednia - 4 * sd, parametr); do <- max(srednia + 4 * sd, parametr)
  s <- siatka_osi(od, do)
  d <- do.call(rbind, lapply(seq_along(srednia), function(i)
    data.frame(x = s, y = stats::dnorm(s, srednia[i], sd[i]), panel = etykiety[i])))
  d$panel <- factor(d$panel, levels = etykiety)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_area(fill = kolory_zi[["secondary"]], alpha = 0.5) +
    ggplot2::geom_line(colour = kolory_zi[["dark"]]) +
    ggplot2::geom_vline(xintercept = parametr, linetype = "dashed", colour = kolory_zi[["accent"]]) +
    ggplot2::facet_wrap(~panel, ncol = 2) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::labs(x = os, y = "Cz\u0119sto\u015b\u0107 estymat", title = tytul,
                  subtitle = paste0("Linia przerywana: parametr = ", os_pl(parametr))) +
    theme_zi()
}

#' Bledy decyzji testu: alfa, beta i moc
#'
#' Krzywa ciagla: rozklad statystyki t przy H0 (srodek 0); krzywa
#' przerywana: rozklad przy H1 przesuniety o `przesuniecie`. Pole alfa lezy
#' w ogonach H0 poza wartosciami krytycznymi, pole beta pod krzywa H1
#' miedzy wartosciami krytycznymi. Podtytul podaje alfa, beta i moc.
#' @param przesuniecie Srodek rozkladu statystyki przy H1 (np. d razy
#'   pierwiastek z n).
#' @param df Stopnie swobody rozkladu t.
#' @param alfa Poziom istotnosci testu dwustronnego.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_bledy_testu(3, 99)
wykres_bledy_testu <- function(przesuniecie, df, alfa = 0.05, tytul = NULL) {
  kryt <- stats::qt(1 - alfa / 2, df)
  s <- siatka_osi(-4.5, max(4.5, przesuniecie + 4))
  h0 <- data.frame(x = s, y = stats::dt(s, df), rozklad = "Przy H0")
  h1 <- data.frame(x = s, y = stats::dt(s - przesuniecie, df), rozklad = "Przy H1")
  beta <- stats::pt(kryt - przesuniecie, df) - stats::pt(-kryt - przesuniecie, df)
  pole_alfa <- h0[abs(h0$x) >= kryt, ]
  pole_alfa$strona <- ifelse(pole_alfa$x < 0, "l", "p")
  pole_beta <- h1[abs(h1$x) < kryt, ]
  pola <- rbind(data.frame(pole_alfa[c("x", "y", "strona")], pole = "\u03b1: odrzucenie przy H0"),
                data.frame(pole_beta[c("x", "y")], strona = "b", pole = "\u03b2: pozostawienie H0 przy H1"))
  ggplot2::ggplot() +
    ggplot2::geom_area(data = pola, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$pole,
                       group = interaction(.data$pole, .data$strona)), alpha = 0.6, position = "identity") +
    ggplot2::geom_line(data = rbind(h0, h1), ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$rozklad),
                       colour = kolory_zi[["dark"]], linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = c(-kryt, kryt), linetype = "dotted", colour = kolory_zi[["dark"]]) +
    ggplot2::scale_fill_manual(values = c(kolory_zi[["accent"]], kolory_zi[["secondary"]]), name = NULL) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::scale_y_continuous(labels = os_pl) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1), linetype = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(x = "Warto\u015b\u0107 statystyki t", y = "G\u0119sto\u015b\u0107", title = tytul,
                  subtitle = paste0("Warto\u015bci krytyczne \u00b1", liczba_pl(kryt, 2L), "; \u03b1 = ", liczba_pl(alfa, 2L),
                                    "; \u03b2 = ", liczba_pl(beta, 2L), "; moc = ", liczba_pl(1 - beta, 2L))) +
    theme_zi()
}

#' Moc testu t jednej proby wobec liczebnosci
#'
#' Krzywa dla kazdego standaryzowanego efektu d (typ linii odroznia efekty);
#' linia kropkowana oznacza moc docelowa, a legenda podaje liczebnosc, przy
#' ktorej krzywa ja osiaga. Obliczenie: stats::power.t.test.
#' @param d Standaryzowane efekty.
#' @param n Siatka liczebnosci.
#' @param alfa Poziom istotnosci testu dwustronnego.
#' @param cel Moc docelowa.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_moc(c(0.3, 0.5), seq(10, 100, 10))
wykres_moc <- function(d, n, alfa = 0.05, cel = 0.8, tytul = NULL) {
  stopifnot(all(d > 0), all(n >= 2))
  potrzebne <- vapply(d, function(e) ceiling(stats::power.t.test(power = cel, delta = e, sd = 1,
    sig.level = alfa, type = "one.sample")$n), numeric(1))
  nazwy <- paste0("d = ", liczba_pl(d, 1L), " (moc ", liczba_pl(cel, 1L), " przy n = ", potrzebne, ")")
  z <- do.call(rbind, lapply(seq_along(d), function(i)
    data.frame(n = n, moc = stats::power.t.test(n = n, delta = d[i], sd = 1, sig.level = alfa,
                                                type = "one.sample")$power, efekt = nazwy[i])))
  z$efekt <- factor(z$efekt, levels = nazwy)
  ggplot2::ggplot(z, ggplot2::aes(x = .data$n, y = .data$moc, linetype = .data$efekt)) +
    ggplot2::geom_hline(yintercept = cel, linetype = "dotted", colour = kolory_zi[["accent"]]) +
    ggplot2::geom_line(colour = kolory_zi[["dark"]], linewidth = 0.8) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")[seq_along(d)], name = NULL) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(x = "Liczba os\u00f3b n", y = "Moc (1 \u2212 \u03b2)", title = tytul) +
    theme_zi()
}

#' Prawdopodobienstwo co najmniej jednego odrzucenia w serii testow
#'
#' Punkty 1 - (1 - alfa)^m dla m niezaleznych testow, w ktorych wszystkie
#' hipotezy zerowe sa prawdziwe; napisy przy wybranych m podaja wartosc.
#' @param m Liczby testow.
#' @param alfa Poziom istotnosci pojedynczego testu.
#' @param etykiety Wartosci m opisane liczba.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_wielokrotne_testy(1:20)
wykres_wielokrotne_testy <- function(m, alfa = 0.05, etykiety = c(1, 5, 10, 20), tytul = NULL) {
  d <- data.frame(m = m, p = 1 - (1 - alfa)^m)
  e <- d[d$m %in% etykiety, , drop = FALSE]
  e$etykieta <- liczba_pl(e$p, 2L)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$m, y = .data$p)) +
    ggplot2::geom_line(colour = kolory_zi[["primary"]]) +
    ggplot2::geom_point(colour = kolory_zi[["primary"]], size = 2) +
    ggplot2::geom_text(data = e, ggplot2::aes(label = .data$etykieta), vjust = -1, size = 3.3) +
    ggplot2::scale_y_continuous(labels = os_pl, limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = "Liczba niezale\u017cnych test\u00f3w m", y = "P(co najmniej jedno odrzucenie)", title = tytul,
                  subtitle = paste0("Ka\u017cda hipoteza zerowa prawdziwa; \u03b1 = ", liczba_pl(alfa, 2L))) +
    theme_zi()
}

#' Rozklad zerowy z symulacji i obserwowana statystyka
#'
#' Histogram wartosci statystyki z B powtorzen przy H0. Przedzialy, w ktorych
#' |T| jest nie mniejsze od |obserwowanej|, sa wyroznione, a linie przerywane
#' oznaczaja obserwowana wartosc i jej odbicie. Podtytul podaje B, liczbe
#' wartosci co najmniej tak skrajnych i p_MC = (k + 1) / (B + 1).
#' @param wartosci Statystyki z powtorzen przy H0.
#' @param obserwowana Statystyka z danych.
#' @param szerokosc Szerokosc przedzialu histogramu.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @param poczatek Poczatek pierwszego przedzialu; przy wartosciach na siatce
#'   (pelne wyliczenie) przesuniecie o pol szerokosci centruje slupki.
#' @param dokladny TRUE, gdy wartosci obejmuja wszystkie mozliwe uklady;
#'   podtytul podaje wtedy p = k / liczba ukladow.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_rozklad_zerowy(c(-2, -1, -1, 0, 0, 0, 1, 1, 2), 1.5, 0.5)
wykres_rozklad_zerowy <- function(wartosci, obserwowana, szerokosc, os = "Warto\u015b\u0107 statystyki", tytul = NULL,
                                  poczatek = NULL, dokladny = FALSE) {
  wartosci <- wartosci[is.finite(wartosci)]
  B <- length(wartosci)
  k <- sum(abs(wartosci) >= abs(obserwowana) - 1e-12)
  d <- przedzialy_histogramu(wartosci, szerokosc, poczatek)
  skrajne <- wartosci[abs(wartosci) >= abs(obserwowana) - 1e-12]
  # Slupek jest wyrozniony, gdy zawiera co najmniej jedna wartosc skrajna.
  d$skrajny <- vapply(seq_len(nrow(d)), function(j)
    any(skrajne > d$od[j] - 1e-12 & skrajne <= d$do[j] + 1e-12), logical(1))
  podpis <- if (dokladny) paste0("Wszystkie uk\u0142ady: ", B, "; |T| \u2265 ", liczba_pl(abs(obserwowana), 2L), ": ", k,
                                 "; p = ", k, "/", B, " = ", liczba_pl(k / B, 3L)) else
    paste0("B = ", B, "; |T| \u2265 ", liczba_pl(abs(obserwowana), 2L), ": ", k,
           "; p_MC = ", k + 1, "/", B + 1, " = ", liczba_pl((k + 1) / (B + 1), 3L))
  ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$od, xmax = .data$do, ymin = 0, ymax = .data$n,
                                    fill = .data$skrajny), colour = "white", show.legend = FALSE) +
    ggplot2::geom_vline(xintercept = c(-abs(obserwowana), abs(obserwowana)), linetype = "dashed",
                        colour = kolory_zi[["dark"]]) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = kolory_zi[["secondary"]], `TRUE` = kolory_zi[["warning"]])) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = if (dokladny) "Liczba uk\u0142ad\u00f3w" else "Liczba powt\u00f3rze\u0144",
                  title = tytul, subtitle = podpis) +
    theme_zi()
}

#' Rozklad bootstrapowy z przedzialem percentylowym
#'
#' Histogram B statystyk bootstrapowych; linie przerywane oznaczaja kwantyle
#' (1 - poziom)/2 i (1 + poziom)/2, czyli granice przedzialu percentylowego.
#' @param wartosci Statystyki z prob bootstrapowych.
#' @param szerokosc Szerokosc przedzialu histogramu.
#' @param poziom Poziom przedzialu.
#' @param os Podpis osi wartosci.
#' @param tytul Tytul wykresu.
#' @return Obiekt ggplot; wykres rysuje print().
#' @export
#' @examples
#' p <- wykres_bootstrap(stats::qnorm(stats::ppoints(200), 10, 1), 0.25)
wykres_bootstrap <- function(wartosci, szerokosc, poziom = 0.95, os = "Warto\u015b\u0107 statystyki", tytul = NULL) {
  wartosci <- wartosci[is.finite(wartosci)]
  g <- stats::quantile(wartosci, c((1 - poziom) / 2, (1 + poziom) / 2), names = FALSE)
  d <- przedzialy_histogramu(wartosci, szerokosc)
  l <- data.frame(x = g, etykieta = liczba_pl(g, 2L))
  ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$od, xmax = .data$do, ymin = 0, ymax = .data$n),
                       fill = kolory_zi[["secondary"]], colour = "white") +
    ggplot2::geom_vline(data = l, ggplot2::aes(xintercept = .data$x), linetype = "dashed",
                        colour = kolory_zi[["accent"]], linewidth = 0.8) +
    ggplot2::geom_text(data = l, ggplot2::aes(x = .data$x, y = Inf, label = .data$etykieta),
                       vjust = 1.5, hjust = -0.1, size = 3.3) +
    ggplot2::scale_x_continuous(labels = os_pl) +
    ggplot2::labs(x = os, y = "Liczba pr\u00f3b bootstrapowych", title = tytul,
                  subtitle = paste0("B = ", length(wartosci), "; \u015brednia ", liczba_pl(mean(wartosci), 2L),
                                    "; granice ", liczba_pl(100 * poziom, 0L), "%: ",
                                    liczba_pl(g[1], 2L), " i ", liczba_pl(g[2], 2L))) +
    theme_zi()
}
