#' Uruchomienie gotowej analizy zadania
#'
#' Funkcja wykonuje odtwarzalny tok obliczen przewidziany dla danego zadania.
#' Student wybiera jedynie wskazane w materiale parametry i interpretuje zapisane
#' wyniki. Surowy plik danych pozostaje bez zmian.
#'
#' @param id Identyfikator Z01--Z10.
#' @param katalog Katalog glowny projektu studenta.
#' @param parametry Nazwana lista ustawien dozwolonych w danym zadaniu.
#' @param B Liczba replikacji bootstrapu albo permutacji; co najmniej 999.
#' @return Niewidocznie lista zapisanych wynikow i obiektow analizy.
#' @export
#' @examples
#' k <- tempfile("analiza-")
#' utworz_projekt("s017", "S02", k)
#' przygotuj_zadanie("Z03", k)
#' uruchom_analize("Z03", k, list(zmienna = "czas_wyszukiwania"), B = 999)
#' unlink(k, recursive = TRUE)
uruchom_analize <- function(id, katalog = ".", parametry = list(), B = 4999L) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^Z(0[1-9]|10)$")
  if (!is.list(parametry) || (length(parametry) &&
      (is.null(names(parametry)) || any(!nzchar(names(parametry))))))
    stop("Parametry musz\u0105 by\u0107 nazwan\u0105 list\u0105.", call. = FALSE)
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) || B < 999 || B != as.integer(B))
    stop("B musi by\u0107 liczb\u0105 ca\u0142kowit\u0105 co najmniej 999.", call. = FALSE)
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  sprawdz_id(cfg$id, "id")
  surowe <- utils::read.csv(file.path(katalog, "dane", "surowe.csv"),
                            encoding = "UTF-8", stringsAsFactors = FALSE)
  gotowe <- dane_analityczne(surowe)
  folder <- file.path(katalog, "zadania", tolower(id), "wyniki")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  seed <- seed_analizy(cfg$id, cfg$scenariusz, id)
  wynik <- lokalny_rng(seed, analiza_wedlug_id(id, gotowe, cfg, parametry,
                                               as.integer(B), folder))
  invisible(wynik)
}

#' Uruchomienie gotowej analizy projektu ilosciowego
#'
#' @param katalog Katalog glowny projektu studenta.
#' @param parametry Nazwana lista z wyborem zmiennej porownywanej i liczba replikacji.
#' @return Niewidocznie lista zapisanych wynikow.
#' @export
#' @examples
#' k <- tempfile("projekt-analiza-")
#' utworz_projekt("s017", "S02", k)
#' uruchom_projekt(k, list(zmienna_porownania = "indeks", B = 999L))
#' unlink(k, recursive = TRUE)
uruchom_projekt <- function(katalog = ".", parametry = list(
                              zmienna_porownania = "indeks", B = 4999L)) {
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  sprawdz_id(cfg$id, "id")
  surowe <- utils::read.csv(file.path(katalog, "dane", "surowe.csv"),
                            encoding = "UTF-8", stringsAsFactors = FALSE)
  gotowe <- dane_analityczne(surowe)
  folder <- file.path(katalog, "projekty", "ilosciowy", "wyniki")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  B <- parametry$B %||% 4999L
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) || B < 999 || B != as.integer(B))
    stop("B musi by\u0107 liczb\u0105 ca\u0142kowit\u0105 co najmniej 999.", call. = FALSE)
  zmienna <- wybierz_zmienna(parametry$zmienna_porownania %||% "indeks",
                             c("indeks", "czas_wyszukiwania"))
  seed <- seed_analizy(cfg$id, cfg$scenariusz, "PROJEKT")
  wynik <- lokalny_rng(seed, {
    pisz_csv(gotowe$przygotowane$dziennik, file.path(folder, "dziennik.csv"))
    opis <- opis_zmiennej(gotowe$dane[[zmienna]], zmienna)
    pisz_csv(opis, file.path(folder, "opis.csv"))
    pisz_csv(gotowe$dane[c("id_odpowiedzi", "indeks", "liczba_pozycji")],
             file.path(folder, "indeks.csv"))
    pisz_csv(odpowiedzi_wielokrotne(gotowe$dane[paste0("kanal_", 1:4)]),
             file.path(folder, "kanaly.csv"))
    por <- analiza_dwoch_grup(gotowe$dane, zmienna, as.integer(B))
    pisz_csv(por$podsumowanie, file.path(folder, "porownanie.csv"))
    scen <- get("scenariusz", mode = "function")(cfg$scenariusz)
    if (tolower(scen$druga_analiza) == "spearman") {
      analiza_druga <- analiza_korelacji(gotowe$dane, "indeks", scen$zmienna_druga,
                                         as.integer(B))
      druga <- analiza_druga$podsumowanie
      zapisz_rozrzut(gotowe$dane, "indeks", scen$zmienna_druga,
                     file.path(folder, "druga_analiza.png"))
    } else {
      analiza_druga <- analiza_kategorii(gotowe$dane, "grupa", "powodzenie",
                                         as.integer(B))
      druga <- analiza_druga$podsumowanie
      zapisz_tabele(analiza_druga$tabela_macierz,
                    file.path(folder, "druga_analiza.png"))
    }
    pisz_csv(druga, file.path(folder, "druga_analiza.csv"))
    zapisz_histogram(gotowe$dane[[zmienna]], zmienna,
                     file.path(folder, "histogram.png"))
    zapisz_grupy(gotowe$dane, zmienna, file.path(folder, "grupy.png"))
    list(opis = opis, porownanie = por$podsumowanie, druga_analiza = druga)
  })
  invisible(wynik)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

seed_analizy <- function(id, scenariusz, czesc) {
  klucz <- digest::digest(paste(id, scenariusz, czesc, sep = "|"),
                          algo = "sha256", serialize = FALSE)
  strtoi(substr(klucz, 1L, 7L), base = 16L)
}

dane_analityczne <- function(surowe) {
  przygotowane <- przygotuj_ankiete(surowe)
  dane <- przygotowane$dane
  pozycje <- dane[paste0("pozycja_", 1:6)]
  pozycje$pozycja_3 <- odwroc_pozycje(pozycje$pozycja_3)
  dane$liczba_pozycji <- rowSums(!is.na(pozycje))
  dane$indeks <- indeks_ankiety(pozycje, minimum = 5L)
  list(dane = dane, przygotowane = przygotowane)
}

wybierz_zmienna <- function(x, dozwolone) {
  if (!is.character(x) || length(x) != 1L || !x %in% dozwolone)
    stop("Dozwolone warto\u015bci: ", paste(dozwolone, collapse = ", "), ".", call. = FALSE)
  x
}

opis_zmiennej <- function(x, nazwa) {
  braki <- sum(!is.finite(x))
  x <- x[is.finite(x)]
  data.frame(zmienna = nazwa, N = length(x), braki = braki,
             srednia = mean(x), mediana = stats::median(x),
             sd = stats::sd(x), IQR = stats::IQR(x),
             minimum = min(x), maksimum = max(x))
}

analiza_wedlug_id <- function(id, gotowe, cfg, parametry, B, folder) {
  dane <- gotowe$dane
  if (id == "Z01") {
    out <- data.frame(element = c("wiersze surowe", "wiersze po czyszczeniu", "kolumny"),
                      wartosc = c(nrow(gotowe$przygotowane$dane) +
                                   gotowe$przygotowane$dziennik$liczba[2],
                                  nrow(dane), ncol(dane)))
    pisz_csv(out, file.path(folder, "orientacja.csv"))
    return(list(podsumowanie = out))
  }
  if (id == "Z02") {
    pisz_csv(dane, file.path(folder, "wyczyszczone.csv"))
    pisz_csv(gotowe$przygotowane$dziennik, file.path(folder, "dziennik.csv"))
    return(list(dane = dane, dziennik = gotowe$przygotowane$dziennik))
  }
  if (id == "Z03") {
    zmienna <- wybierz_zmienna(parametry$zmienna %||% "czas_wyszukiwania",
                               c("czas_wyszukiwania", "indeks"))
    out <- opis_zmiennej(dane[[zmienna]], zmienna)
    out$braki <- sum(is.na(dane[[zmienna]]))
    pisz_csv(out, file.path(folder, "opis.csv"))
    zapisz_histogram(dane[[zmienna]], zmienna, file.path(folder, "histogram.png"))
    zapisz_grupy(dane, zmienna, file.path(folder, "boxplot.png"))
    return(list(podsumowanie = out))
  }
  if (id == "Z04") {
    slownik <- utils::read.csv(file.path(dirname(dirname(dirname(folder))), "dane", "slownik.csv"),
                               encoding = "UTF-8", stringsAsFactors = FALSE)
    pisz_csv(slownik, file.path(folder, "slownik_pomiaru.csv"))
    indeks <- dane[c("id_odpowiedzi", "indeks", "liczba_pozycji")]
    pisz_csv(indeks, file.path(folder, "indeks.csv"))
    kanaly <- odpowiedzi_wielokrotne(dane[paste0("kanal_", 1:4)])
    pisz_csv(kanaly, file.path(folder, "kanaly.csv"))
    return(list(indeks = indeks, kanaly = kanaly))
  }
  if (id == "Z05") {
    zmienna <- wybierz_zmienna(parametry$zmienna %||% "indeks",
                               c("indeks", "czas_wyszukiwania"))
    ref <- parametry$wartosc_odniesienia
    if (is.null(ref)) ref <- if (zmienna == "indeks") 3 else scenariusz(cfg$scenariusz)$czas_typowy
    out <- analiza_jednej_proby(dane[[zmienna]], zmienna, ref, B)
    pisz_csv(out$podsumowanie, file.path(folder, "estymacja.csv"))
    pisz_csv(out$bootstrap, file.path(folder, "bootstrap.csv"))
    zapisz_rozklad(out$rozklad_bootstrap, out$podsumowanie$srednia,
                   "\u015arednie w pr\u00f3bach bootstrapowych", file.path(folder, "bootstrap.png"))
    return(out)
  }
  if (id == "Z06") {
    zmienna <- wybierz_zmienna(parametry$zmienna %||% "indeks",
                               c("indeks", "czas_wyszukiwania"))
    out <- analiza_dwoch_grup(dane, zmienna, B)
    pisz_csv(out$podsumowanie, file.path(folder, "porownanie.csv"))
    pisz_csv(out$permutacja, file.path(folder, "permutacja.csv"))
    zapisz_grupy(dane, zmienna, file.path(folder, "grupy.png"))
    return(out)
  }
  if (id == "Z07") {
    out <- analiza_kategorii(dane, "grupa", "powodzenie", B)
    pisz_csv(out$tabela, file.path(folder, "tabela.csv"))
    pisz_csv(out$podsumowanie, file.path(folder, "test_kategorie.csv"))
    pisz_csv(out$permutacja, file.path(folder, "permutacja.csv"))
    zapisz_tabele(out$tabela_macierz, file.path(folder, "tabela.png"))
    return(out)
  }
  if (id == "Z08") {
    y <- wybierz_zmienna(parametry$wykonanie %||% "czas_wyszukiwania",
                         c("czas_wyszukiwania", "powodzenie"))
    metoda <- wybierz_zmienna(parametry$metoda %||% "spearman", c("pearson", "spearman"))
    out <- analiza_korelacji(dane, "indeks", y, B, metoda)
    pisz_csv(out$podsumowanie, file.path(folder, "korelacja.csv"))
    pisz_csv(out$permutacja, file.path(folder, "permutacja.csv"))
    pisz_csv(out$regresja, file.path(folder, "regresja.csv"))
    zapisz_rozrzut(dane, "indeks", y, file.path(folder, "rozrzut.png"))
    return(out)
  }
  if (id == "Z09") {
    scen <- scenariusz(cfg$scenariusz)
    out <- data.frame(
      element = c("scenariusz", "projekt", "wynik por\u00f3wnania", "predyktor grupowy",
                  "druga analiza", "status danych"),
      wartosc = c(cfg$scenariusz, scen$projekt_badania,
                  parametry$wynik %||% "indeks", "grupa", scen$druga_analiza,
                  "syntetyczne"))
    pisz_csv(out, file.path(folder, "plan_analizy.csv"))
    return(list(podsumowanie = out))
  }
  scen <- scenariusz(cfg$scenariusz)
  out <- data.frame(
    element = c("ID", "scenariusz", "N po czyszczeniu", "wa\u017cny indeks",
                "wa\u017cny czas", "analiza grup", "druga analiza"),
    wartosc = c(cfg$id, cfg$scenariusz, nrow(dane), sum(!is.na(dane$indeks)),
                sum(!is.na(dane$czas_wyszukiwania)), "klasyczna i permutacyjna",
                scen$druga_analiza))
  pisz_csv(out, file.path(folder, "kontrola_raportu.csv"))
  list(podsumowanie = out)
}

analiza_jednej_proby <- function(x, nazwa, ref, B) {
  x <- x[is.finite(x)]
  test <- stats::t.test(x, mu = ref)
  roznica <- mean(x) - ref
  d <- roznica / stats::sd(x)
  boot <- replicate(B, mean(sample(x, replace = TRUE)))
  znaki <- replicate(B, mean(sample(c(-1, 1), length(x), replace = TRUE) * (x - ref)))
  p_perm <- (1 + sum(abs(znaki) >= abs(roznica))) / (B + 1)
  ci_boot <- stats::quantile(boot, c(.025, .975), names = FALSE)
  pod <- data.frame(zmienna = nazwa, N = length(x), srednia = mean(x),
                    wartosc_odniesienia = ref, roznica = roznica,
                    SE = stats::sd(x) / sqrt(length(x)),
                    CI_dol = test$conf.int[1], CI_gora = test$conf.int[2],
                    t = unname(test$statistic), df = unname(test$parameter),
                    p_klasyczne = test$p.value, d = d, p_permutacyjne = p_perm,
                    B = B)
  list(podsumowanie = pod,
       bootstrap = data.frame(estymata = "\u015brednia", B = B,
                              CI_dol = ci_boot[1], CI_gora = ci_boot[2]),
       rozklad_bootstrap = boot, rozklad_zerowy = znaki)
}

analiza_dwoch_grup <- function(dane, zmienna, B) {
  x <- dane[stats::complete.cases(dane[c("grupa", zmienna)]), c("grupa", zmienna)]
  names(x)[2] <- "wynik"
  x$grupa <- factor(x$grupa)
  if (nlevels(x$grupa) != 2L) stop("Analiza wymaga dok\u0142adnie dw\u00f3ch grup.", call. = FALSE)
  poziomy <- levels(x$grupa)
  test <- stats::t.test(wynik ~ grupa, data = x)
  sr <- tapply(x$wynik, x$grupa, mean)
  sdg <- tapply(x$wynik, x$grupa, stats::sd)
  ng <- table(x$grupa)
  roznica <- unname(sr[2] - sr[1])
  sp <- sqrt(((ng[1] - 1) * sdg[1]^2 + (ng[2] - 1) * sdg[2]^2) / (sum(ng) - 2))
  d <- roznica / sp
  g <- d * (1 - 3 / (4 * sum(ng) - 9))
  perm <- replicate(B, {
    gperm <- sample(x$grupa)
    unname(diff(tapply(x$wynik, gperm, mean)))
  })
  pperm <- (1 + sum(abs(perm) >= abs(roznica))) / (B + 1)
  boot <- replicate(B, {
    b <- do.call(rbind, lapply(split(x, x$grupa), function(z) z[sample.int(nrow(z), replace = TRUE), ]))
    unname(diff(tapply(b$wynik, b$grupa, mean)))
  })
  ci <- stats::quantile(boot, c(.025, .975), names = FALSE)
  pod <- data.frame(zmienna = zmienna, grupa_1 = poziomy[1], N_1 = unname(ng[1]),
                    srednia_1 = unname(sr[1]), grupa_2 = poziomy[2], N_2 = unname(ng[2]),
                    srednia_2 = unname(sr[2]), roznica_2_minus_1 = roznica,
                    CI_roznicy_dol = ci[1], CI_roznicy_gora = ci[2], Hedges_g = g,
                    t_Welcha = unname(test$statistic), df = unname(test$parameter),
                    p_klasyczne = test$p.value, p_permutacyjne = pperm, B = B)
  list(podsumowanie = pod,
       permutacja = data.frame(statystyka_obserwowana = roznica,
                               p_permutacyjne = pperm, B = B),
       rozklad_zerowy = perm)
}

analiza_kategorii <- function(dane, xname, yname, B) {
  x <- dane[stats::complete.cases(dane[c(xname, yname)]), c(xname, yname)]
  tab <- table(x[[xname]], x[[yname]])
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  mc <- suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = B))
  n <- sum(tab)
  v <- sqrt(unname(chi$statistic) / (n * min(nrow(tab) - 1, ncol(tab) - 1)))
  bootv <- replicate(B, {
    idx <- sample.int(nrow(x), replace = TRUE)
    z <- table(factor(x[[xname]][idx], levels = rownames(tab)),
               factor(x[[yname]][idx], levels = colnames(tab)))
    ch <- suppressWarnings(stats::chisq.test(z, correct = FALSE))
    sqrt(unname(ch$statistic) / (sum(z) * min(nrow(z) - 1, ncol(z) - 1)))
  })
  ci <- stats::quantile(bootv[is.finite(bootv)], c(.025, .975), names = FALSE)
  prop <- prop.table(tab, margin = 1)
  tabela <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(tabela) <- c(xname, yname, "liczba")
  tabela$procent_w_grupie <- 100 * as.vector(prop)
  pod <- data.frame(N = n, chi2 = unname(chi$statistic), df = unname(chi$parameter),
                    p_klasyczne = chi$p.value, p_Monte_Carlo = mc$p.value,
                    B = B, V_Cramera = v, CI_V_dol = ci[1], CI_V_gora = ci[2],
                    min_oczekiwana = min(chi$expected))
  list(tabela = tabela, tabela_macierz = tab, podsumowanie = pod,
       permutacja = data.frame(chi2_obserwowane = unname(chi$statistic),
                               p_Monte_Carlo = mc$p.value, B = B))
}

analiza_korelacji <- function(dane, xname, yname, B, metoda = "spearman") {
  x <- dane[stats::complete.cases(dane[c(xname, yname)]), c(xname, yname)]
  metoda <- wybierz_zmienna(metoda, c("pearson", "spearman"))
  test <- suppressWarnings(stats::cor.test(x[[xname]], x[[yname]], method = metoda,
                                            exact = FALSE))
  r <- unname(test$estimate)
  boot <- replicate(B, {
    idx <- sample.int(nrow(x), replace = TRUE)
    suppressWarnings(stats::cor(x[[xname]][idx], x[[yname]][idx], method = metoda))
  })
  ci <- if (!is.null(test$conf.int)) unname(test$conf.int) else
    stats::quantile(boot[is.finite(boot)], c(.025, .975), names = FALSE)
  perm <- replicate(B, suppressWarnings(stats::cor(x[[xname]], sample(x[[yname]]), method = metoda)))
  pperm <- (1 + sum(abs(perm) >= abs(r), na.rm = TRUE)) / (B + 1)
  mod <- stats::lm(x[[yname]] ~ x[[xname]])
  sm <- summary(mod)
  coef <- stats::coef(sm)
  pod <- data.frame(x = xname, y = yname, metoda = metoda, N = nrow(x),
                    wspolczynnik = r, CI_dol = ci[1], CI_gora = ci[2],
                    p_klasyczne = test$p.value, p_permutacyjne = pperm, B = B)
  reg <- data.frame(wyraz_wolny = coef[1, 1], nachylenie = coef[2, 1],
                    SE_nachylenia = coef[2, 2], t = coef[2, 3], p = coef[2, 4],
                    R2 = sm$r.squared, N = nrow(x))
  list(podsumowanie = pod,
       permutacja = data.frame(wspolczynnik_obserwowany = r,
                               p_permutacyjne = pperm, B = B),
       regresja = reg, rozklad_zerowy = perm)
}

zapisz_histogram <- function(x, nazwa, plik) {
  x <- x[is.finite(x)]
  grDevices::png(plik, width = 1200, height = 800, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::hist(x, col = "#56B4E9", border = "white",
                 main = paste("Rozk\u0142ad:", nazwa), xlab = nazwa)
}

zapisz_grupy <- function(dane, zmienna, plik) {
  grDevices::png(plik, width = 1200, height = 800, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::boxplot(dane[[zmienna]] ~ dane$grupa, col = c("#56B4E9", "#E69F00"),
                    xlab = "Grupa", ylab = zmienna, main = paste(zmienna, "wed\u0142ug grupy"))
}

zapisz_rozklad <- function(x, obserwowana, tytul, plik) {
  grDevices::png(plik, width = 1200, height = 800, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::hist(x, breaks = 30, col = "#009E73", border = "white",
                 main = tytul, xlab = "Warto\u015b\u0107 statystyki")
  graphics::abline(v = obserwowana, col = "#D55E00", lwd = 3)
}

zapisz_tabele <- function(tab, plik) {
  prop <- prop.table(tab, margin = 1)
  grDevices::png(plik, width = 1200, height = 800, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::barplot(t(prop), beside = FALSE, col = c("#56B4E9", "#E69F00"),
                    legend.text = colnames(prop), xlab = "Grupa", ylab = "Odsetek",
                    main = "Wynik zadania w grupach")
}

zapisz_rozrzut <- function(dane, xname, yname, plik) {
  ok <- stats::complete.cases(dane[c(xname, yname)])
  grDevices::png(plik, width = 1200, height = 800, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot(dane[[xname]][ok], dane[[yname]][ok], pch = 19, col = "#0072B2AA",
                 xlab = xname, ylab = yname,
                 main = "Samoocena a wykonanie zadania")
  graphics::abline(stats::lm(dane[[yname]][ok] ~ dane[[xname]][ok]),
                   col = "#D55E00", lwd = 2)
}
