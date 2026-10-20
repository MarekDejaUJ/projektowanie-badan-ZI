# Skrypt startowy kursu „Projektowanie badań — część ilościowa”, Zarządzanie informacją UJ.
# Pierwszy blok każdego pliku zadania i konsola na początku zajęć uruchamiają go poleceniem:
#   source("https://marekdejauj.github.io/projektowanie-badan-ZI/materialy/start.R")
#
# Skrypt:
# 1. doinstalowuje brakujące pakiety CRAN i ładuje pakiety używane w zadaniach,
# 2. ustawia wygląd wykresów i wydruków,
# 3. tworzy syntetyczne dane scenariuszy: dane_zi("S02", id = "s017"),
# 4. udostępnia funkcje opisu: karta_zi(), slownik_zi(), opis_grup(), czestosci(), test_znakow(),
# 5. udostępnia pobierz_szablon(), oddaj_zadanie(), oddaj_projekt(), sprawdz_srodowisko(), przygotuj_pdf().
# Dane zależą tylko od scenariusza i ID, więc każde uruchomienie na każdym komputerze daje te same liczby.

if (getRversion() < "4.2.0") {
  stop("Kurs wymaga R w wersji 4.2 lub nowszej. Zainstaluj aktualne R z https://cran.r-project.org.")
}

.zi_wersja <- "3.1.1"
.zi_adres <- getOption("zi.adres", "https://marekdejauj.github.io/projektowanie-badan-ZI/materialy/")
.zi_rocznik <- "2026-27"

# ---------------------------------------------------------------------------------------------
# Pakiety
# ---------------------------------------------------------------------------------------------

.zi_pakiety <- c("dplyr", "ggplot2", "infer", "psych", "rstatix", "effectsize", "car",
                 "rmarkdown", "knitr", "yaml", "curl")
.zi_dolaczone <- c("dplyr", "ggplot2", "infer")

.zi_instaluj <- function(nazwy) {
  brakujace <- setdiff(nazwy, rownames(utils::installed.packages()))
  if (length(brakujace) > 0L) {
    message("Instaluję brakujące pakiety (jednorazowo, kilka minut): ", paste(brakujace, collapse = ", "))
    typ <- if (.Platform$OS.type == "windows") "binary" else getOption("pkgType")
    utils::install.packages(brakujace, repos = "https://cloud.r-project.org", type = typ, quiet = TRUE)
  }
  nadal <- nazwy[!vapply(nazwy, requireNamespace, logical(1), quietly = TRUE)]
  if (length(nadal) > 0L) {
    stop("Nie udało się zainstalować pakietów: ", paste(nadal, collapse = ", "),
         ". Uruchom nową sesję R (Session > Restart R), sprawdź połączenie z internetem i uruchom skrypt ponownie.",
         call. = FALSE)
  }
  invisible(nazwy)
}

.zi_instaluj(.zi_pakiety)
invisible(lapply(.zi_dolaczone, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))

# Paleta Okabe-Ito jest czytelna także dla osób z zaburzeniami widzenia barw.
.zi_paleta <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#999999")
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 11))
options(ggplot2.discrete.fill = .zi_paleta, ggplot2.discrete.colour = .zi_paleta,
        dplyr.summarise.inform = FALSE, width = 80, crayon.enabled = FALSE, cli.num_colors = 1L)
if (isTRUE(getOption("knitr.in.progress"))) {
  knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.align = "center", fig.crop = FALSE,
                        fig.width = 6.5, fig.height = 3.6, dpi = 300, out.width = "85%")
  # W PDF wydruk nie zawija się sam: dłuższe wiersze (np. opis hipotezy w t.test) łamiemy przed składem.
  if (knitr::is_latex_output()) {
    .zi_wydruk <- knitr::knit_hooks$get("output")
    knitr::knit_hooks$set(output = function(x, options) {
      linie <- strsplit(x, "\n", fixed = TRUE)[[1]]
      dlugie <- nchar(linie, type = "chars") > 90
      linie[dlugie] <- vapply(linie[dlugie], function(l) paste(strwrap(l, 88, exdent = 6), collapse = "\n"), "")
      .zi_wydruk(paste0(paste(linie, collapse = "\n"), if (grepl("\n$", x)) "\n"), options)
    })
  }
}

# ---------------------------------------------------------------------------------------------
# Scenariusze i dane
# ---------------------------------------------------------------------------------------------

.zi_katalog <- local({
  katalog <- NULL
  function() {
    if (is.null(katalog)) {
      zrodlo <- paste0(.zi_adres, "scenariusze.yml")
      tekst <- tryCatch(readLines(zrodlo, encoding = "UTF-8", warn = FALSE), error = function(e) {
        stop("Nie można wczytać kart scenariuszy z ", zrodlo, ". Sprawdź połączenie z internetem.", call. = FALSE)
      })
      katalog <<- yaml::yaml.load(paste(tekst, collapse = "\n"))$scenariusze
      names(katalog) <<- vapply(katalog, `[[`, "", "id")
    }
    katalog
  }
})

.zi_karta <- function(scenariusz) {
  scenariusz <- toupper(trimws(as.character(scenariusz)))
  katalog <- .zi_katalog()
  if (!scenariusz %in% names(katalog)) {
    stop("Nie ma scenariusza ", scenariusz, ". Dostępne są S01-S20, np. dane_zi(\"S02\", id = \"s017\").", call. = FALSE)
  }
  katalog[[scenariusz]]
}

# Ziarno losowania z tekstu: proste haszowanie liczbami całkowitymi daje ten sam wynik na każdym systemie.
.zi_ziarno <- function(...) {
  znaki <- utf8ToInt(enc2utf8(paste(..., sep = "|")))
  h <- 7
  for (k in znaki) h <- (h * 31 + k) %% 2147483629
  as.integer(h)
}

# Losowanie z ustalonym ziarnem, które nie zmienia stanu generatora użytkownika.
.zi_z_ziarnem <- function(ziarno, wyrazenie) {
  byl <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (byl) stan <- get(".Random.seed", envir = globalenv())
  rodzaj <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(rodzaj))
    if (byl) assign(".Random.seed", stan, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(ziarno)
  wyrazenie
}

.zi_id <- function(id) {
  id <- trimws(as.character(id)[1])
  if (is.na(id) || !nzchar(id) || identical(id, "TWOJE_ID")) {
    warning("Wpisz swoje ID w nagłówku pliku (params: id). Dane powstały dla ID „", id, "”.", call. = FALSE)
    if (is.na(id) || !nzchar(id)) id <- "TWOJE_ID"
  }
  id
}

# Dane surowe: ankieta i obserwacja zadania z typowymi problemami eksportu.
.zi_generuj <- function(k, n) {
  m <- n - 3L
  L <- length(k$poziomy)
  g <- stats::rbinom(m, 1L, 0.5)
  pk <- c(0.34, 0.28, 0.22, 0.16)[seq_len(L)]
  kat <- sample.int(L, m, replace = TRUE, prob = pk / sum(pk))
  czestosc <- sample.int(5L, m, replace = TRUE, prob = c(0.10, 0.20, 0.28, 0.25, 0.17))
  efekt <- k$efekt + stats::runif(1L, -0.12, 0.12)
  kat_u <- c(0, 0.15, -0.10, 0.20)[seq_len(L)] + stats::rnorm(L, 0, 0.05)
  kat_t <- c(0, 0.14, -0.10, 0.24)[seq_len(L)] * stats::runif(1L, 0.4, 1.8)
  u <- efekt * g + 0.22 * (czestosc - 3) + kat_u[kat] + stats::rnorm(m)
  ladunki <- c(0.80, 0.75, 0.70, 0.80, 0.45, 0.75)
  P <- vapply(1:6, function(j) {
    pmin(5L, pmax(1L, as.integer(round(3 + ladunki[j] * u + stats::rnorm(m, 0, 0.75)))))
  }, integer(m))
  P[, 3] <- 6L - P[, 3]  # pozycja 3 jest sformułowana negatywnie
  wiek <- as.integer(k$wiek[2] + round(stats::rgamma(m, shape = 2.5, scale = (k$wiek[1] - k$wiek[2]) / 2.5)))
  czas <- round(pmin(90, exp(log(k$czas_typowy) - 0.18 * u - 0.12 * g + kat_t[kat] + stats::rnorm(m, 0, 0.45))), 1)
  delta <- 0.16 + stats::rnorm(1L, 0, 0.05)
  czas_po <- round(pmin(90, exp(log(czas) - delta + stats::rnorm(m, 0, 0.30))), 1)
  powodzenie <- stats::rbinom(m, 1L, stats::plogis(0.35 + 0.70 * u + 0.20 * g))
  obserwacja <- identical(k$rodzaj, "obserwacja")
  if (obserwacja) {
    powodzenie_po <- ifelse(powodzenie == 1L, stats::rbinom(m, 1L, 0.92),
                            stats::rbinom(m, 1L, stats::plogis(-0.6 + 0.6 * u)))
  }
  kanaly <- vapply(1:4, function(j) {
    stats::rbinom(m, 1L, pmin(0.95, c(0.75, 0.45, 0.35, 0.25)[j] + 0.04 * (czestosc - 3)))
  }, integer(m))

  # Braki odpowiedzi rozkładają się losowo; kod 99 oznacza brak w eksporcie pozycji 2.
  braki <- function(p) sample.int(m, max(1L, round(p * m)))
  for (j in 1:6) P[braki(0.025), j] <- NA_integer_
  P[is.na(P[, 2]), 2] <- 99L
  czas[braki(0.02)] <- NA_real_
  czas_po[braki(0.05)] <- NA_real_
  czestosc[braki(0.02)] <- NA_integer_
  wiek[braki(0.01)] <- NA_integer_
  kanaly[braki(0.03), ] <- NA_integer_
  czas[sample(which(!is.na(czas)), 2L)] <- c(-5, 999)

  grupa <- k$grupy[g + 1L]
  inne <- sample.int(m, 4L)
  grupa[inne[1:2]] <- toupper(grupa[inne[1:2]])
  grupa[inne[3:4]] <- paste0(" ", grupa[inne[3:4]], " ")

  x <- data.frame(id = sprintf("r%03d", seq_len(m)), grupa = grupa, kategoria = k$poziomy[kat],
                  wiek = wiek, czestosc = czestosc, stringsAsFactors = FALSE)
  for (j in 1:6) x[[paste0("p", j)]] <- P[, j]
  x$czas <- czas
  x$czas_po <- czas_po
  x$powodzenie <- powodzenie
  if (obserwacja) x$powodzenie_po <- powodzenie_po
  x$kanal_www <- kanaly[, 1]
  x$kanal_email <- kanaly[, 2]
  x$kanal_media <- kanaly[, 3]
  x$kanal_osobiscie <- kanaly[, 4]
  x <- rbind(x, x[sample.int(m, 3L), , drop = FALSE])
  rownames(x) <- NULL
  x
}

#' Dane scenariusza dla jednej osoby
dane_zi <- function(scenariusz = "S02", id = "wzor", n = 240L, surowe = FALSE) {
  k <- .zi_karta(scenariusz)
  id <- .zi_id(id)
  x <- .zi_z_ziarnem(.zi_ziarno(.zi_rocznik, k$id, id, n), .zi_generuj(k, as.integer(n)))
  attr(x, "scenariusz") <- k$id
  attr(x, "id") <- id
  if (surowe) return(x)
  przygotuj_zi(x)$dane
}

# Reguły przygotowania danych: te same w każdym scenariuszu, z dziennikiem zmian.
przygotuj_zi <- function(surowe, scenariusz = attr(surowe, "scenariusz")) {
  k <- .zi_karta(scenariusz)
  x <- surowe
  dziennik <- data.frame(krok = "dane surowe", zmienione = NA_integer_, wiersze = nrow(x))
  dopisz <- function(krok, zmienione) {
    dziennik[nrow(dziennik) + 1L, ] <<- list(krok, as.integer(zmienione), nrow(x))
  }

  etykiety <- tolower(trimws(x$grupa))
  zmienione <- sum(etykiety != x$grupa, na.rm = TRUE)
  x$grupa <- factor(k$grupy[match(etykiety, tolower(k$grupy))], levels = k$grupy)
  dopisz("ujednolicone etykiety grup", zmienione)

  powtorzone <- duplicated(x)
  x <- x[!powtorzone, , drop = FALSE]
  dopisz("usunięte identyczne powtórzenia wierszy", sum(powtorzone))

  pozycje <- paste0("p", 1:6)
  kody <- sum(as.matrix(x[pozycje]) == 99L, na.rm = TRUE)
  x[pozycje] <- lapply(x[pozycje], function(v) replace(v, v == 99L, NA_integer_))
  dopisz("kod braku 99 w pozycjach zamieniony na NA", kody)

  poza <- !is.na(x$czas) & (x$czas < 0 | x$czas > 120)
  x$czas[poza] <- NA_real_
  dopisz("czas spoza 0-120 min zamieniony na NA", sum(poza))

  P <- as.matrix(x[pozycje])
  P[, 3] <- 6L - P[, 3]
  wazne <- rowSums(!is.na(P))
  x$indeks <- ifelse(wazne >= 5L, round(rowMeans(P, na.rm = TRUE), 3), NA_real_)
  dopisz("indeks bez wyniku (mniej niż 5 z 6 odpowiedzi)", sum(is.na(x$indeks)))

  x$kategoria <- factor(x$kategoria, levels = k$poziomy)
  x$powodzenie <- factor(x$powodzenie, levels = 0:1, labels = c("nie", "tak"))
  if ("powodzenie_po" %in% names(x)) {
    x$powodzenie_po <- factor(x$powodzenie_po, levels = 0:1, labels = c("nie", "tak"))
  }
  kolejnosc <- c("id", "grupa", "kategoria", "wiek", "czestosc", pozycje, "indeks", "czas", "czas_po",
                 "powodzenie", "powodzenie_po", "kanal_www", "kanal_email", "kanal_media", "kanal_osobiscie")
  x <- x[intersect(kolejnosc, names(x))]
  rownames(x) <- NULL
  attr(x, "scenariusz") <- k$id
  attr(x, "id") <- attr(surowe, "id")
  list(dane = x, dziennik = dziennik)
}

#' Karta scenariusza: problem, grupy, pomiar i zadanie
karta_zi <- function(scenariusz, markdown = isTRUE(getOption("knitr.in.progress"))) {
  k <- .zi_karta(scenariusz)
  pozycje <- paste0(seq_along(k$pozycje), ". ", unlist(k$pozycje))
  if (markdown) {
    tekst <- c(
      paste0("**", k$id, " — ", k$tytul, ".** ", k$problem), "",
      paste0("**Grupy:** ", k$grupy[1], " / ", k$grupy[2], ". ", k$opis_grup,
             " **Kategoria (", k$kategoria, "):** ", paste(k$poziomy, collapse = ", "), "."), "",
      paste0("**Zadanie obserwowane:** ", k$zadanie, " **Powodzenie:** ", k$pytanie,
             " (", k$rodzaj, ")."), "",
      paste0("**", k$indeks, "** (skala 1–5, pozycja 3 odwrócona):"), "",
      paste0(pozycje, collapse = "\n"), "")
    cat(tekst, sep = "\n")
  } else {
    cat(paste0(k$id, " — ", k$tytul), strwrap(k$problem, 90), sep = "\n")
    cat("Grupy: ", k$grupy[1], " / ", k$grupy[2], ". ", k$opis_grup, "\n",
        "Kategoria (", k$kategoria, "): ", paste(k$poziomy, collapse = ", "), "\n",
        "Zadanie: ", k$zadanie, "\nPowodzenie: ", k$pytanie, " (", k$rodzaj, ")\n",
        k$indeks, ":\n", paste0("  ", pozycje, collapse = "\n"), "\n", sep = "")
  }
  invisible(k)
}

#' Słownik zmiennych zbioru scenariusza
slownik_zi <- function(scenariusz) {
  k <- .zi_karta(scenariusz)
  wiersze <- list(
    c("id", "identyfikator respondenta", "nominalna"),
    c("grupa", paste0(k$grupy[1], " / ", k$grupy[2]), "nominalna"),
    c("kategoria", paste0(k$kategoria, ": ", paste(k$poziomy, collapse = ", ")), "nominalna"),
    c("wiek", "wiek w latach", "ilorazowa"),
    c("czestosc", "korzystanie w ostatnich 4 tygodniach: 1 nigdy ... 5 bardzo często", "porządkowa"),
    c("p1-p6", "pozycje skali: 1 zdecydowanie nie ... 5 zdecydowanie tak; p3 negatywna", "porządkowa"),
    c("indeks", paste0(k$indeks, ": średnia p1-p6 po odwróceniu p3"), "przedziałowa (umownie)"),
    c("czas", "czas zadania w minutach", "ilorazowa"),
    c("czas_po", "czas drugiej próby po krótkiej instrukcji, w minutach", "ilorazowa"),
    c("powodzenie", paste0(k$pytanie, " (", k$rodzaj, ")"), "nominalna"),
    if (identical(k$rodzaj, "obserwacja")) c("powodzenie_po", "powodzenie drugiej próby", "nominalna"),
    c("kanal_*", "kanały informacji w ostatnich 4 tygodniach: www, email, media, osobiscie (0/1)", "nominalna"))
  w <- do.call(rbind, wiersze)
  structure(data.frame(zmienna = w[, 1], skala = w[, 2 + 1], opis = w[, 2]), class = c("zi_slownik", "data.frame"))
}

# Słownik drukowany wierszami: zmienna, skala i zawinięty opis mieszczą się na stronie PDF.
print.zi_slownik <- function(x, ...) {
  for (i in seq_len(nrow(x))) {
    opis <- strwrap(x$opis[i], width = 48)
    cat(format(x$zmienna[i], width = 15), format(x$skala[i], width = 23), opis[1], "\n")
    if (length(opis) > 1L) cat(paste(strrep(" ", 40), opis[-1], "\n"), sep = "")
  }
  invisible(x)
}

# ---------------------------------------------------------------------------------------------
# Opis danych
# ---------------------------------------------------------------------------------------------

#' Liczebność, braki, średnia, odchylenie standardowe, mediana i kwartyle zmiennej w grupach
opis_grup <- function(dane, zmienna, grupa) {
  x <- if (missing(grupa)) dane else dplyr::group_by(dane, {{ grupa }})
  w <- dplyr::summarise(x,
    n = sum(!is.na({{ zmienna }})),
    braki = sum(is.na({{ zmienna }})),
    M = mean({{ zmienna }}, na.rm = TRUE),
    SD = stats::sd({{ zmienna }}, na.rm = TRUE),
    Mdn = stats::median({{ zmienna }}, na.rm = TRUE),
    Q1 = stats::quantile({{ zmienna }}, 0.25, na.rm = TRUE, names = FALSE),
    Q3 = stats::quantile({{ zmienna }}, 0.75, na.rm = TRUE, names = FALSE),
    min = min({{ zmienna }}, na.rm = TRUE),
    max = max({{ zmienna }}, na.rm = TRUE),
    .groups = "drop")
  w <- as.data.frame(w)
  liczby <- vapply(w, is.numeric, logical(1)) & !names(w) %in% c("n", "braki")
  w[liczby] <- lapply(w[liczby], round, 2)
  w
}

#' Liczebności i procenty kategorii; procent liczony wśród odpowiedzi bez braków
czestosci <- function(dane, zmienna, grupa) {
  if (missing(grupa)) {
    w <- dplyr::count(dane, {{ zmienna }})
    wazne <- !is.na(w[[1]])
    w$procent <- ifelse(wazne, round(100 * w$n / sum(w$n[wazne]), 1), NA_real_)
  } else {
    w <- dplyr::count(dane, {{ grupa }}, {{ zmienna }})
    w <- dplyr::mutate(dplyr::group_by(w, {{ grupa }}),
      procent = ifelse(is.na({{ zmienna }}), NA_real_,
                       round(100 * n / sum(n[!is.na({{ zmienna }})]), 1)))
    w <- dplyr::ungroup(w)
  }
  as.data.frame(w)
}

#' Wykres Likerta: odsetki pięciu odpowiedzi w każdej pozycji p1-p6, z treścią pozycji z karty
wykres_likert <- function(dane) {
  k <- .zi_karta(attr(dane, "scenariusz"))
  etykiety <- c("zdecydowanie nie", "raczej nie", "ani tak, ani nie", "raczej tak", "zdecydowanie tak")
  pozycje <- paste0("p", 1:6)
  opisy <- vapply(seq_along(pozycje), function(j) {
    paste(strwrap(paste0(pozycje[j], ". ", k$pozycje[[j]]), 45), collapse = "\n")
  }, "")
  dl <- data.frame(pozycja = factor(rep(pozycje, each = nrow(dane)), levels = rev(pozycje), labels = rev(opisy)),
                   odpowiedz = factor(unlist(dane[pozycje], use.names = FALSE), levels = 1:5, labels = etykiety))
  tab <- as.data.frame(prop.table(table(dl$pozycja, dl$odpowiedz), 1))
  names(tab) <- c("pozycja", "odpowiedz", "odsetek")
  tab$odpowiedz <- factor(tab$odpowiedz, levels = rev(etykiety))
  ggplot2::ggplot(tab, ggplot2::aes(x = odsetek, y = pozycja, fill = odpowiedz)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_fill_manual(values = stats::setNames(c("#8c510a", "#d8b365", "#d9d9d9", "#5ab4ac", "#01665e"), etykiety),
                               breaks = etykiety) +
    ggplot2::labs(x = "Odsetek odpowiedzi", y = NULL, fill = NULL) +
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 8),
                   legend.key.size = ggplot2::unit(0.4, "cm")) +
    (if (utils::packageVersion("ggplot2") >= "3.5.0") ggplot2::theme(legend.location = "plot")) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
}

#' Pytanie wielokrotnego wyboru: odsetek osób i odsetek wskazań dla czterech kanałów
kanaly_zi <- function(dane) {
  kolumny <- c("kanal_www", "kanal_email", "kanal_media", "kanal_osobiscie")
  odpowiedzi <- dane[stats::complete.cases(dane[kolumny]), kolumny]
  wskazania <- colSums(odpowiedzi)
  w <- data.frame(kanal = c("strona WWW", "e-mail", "media społecznościowe", "kontakt osobisty"),
                  osoby = nrow(odpowiedzi), wskazania = as.integer(wskazania),
                  proc_osob = round(100 * wskazania / nrow(odpowiedzi), 1),
                  proc_wskazan = round(100 * wskazania / sum(wskazania), 1))
  w <- w[order(w$wskazania, decreasing = TRUE), , drop = FALSE]
  rownames(w) <- NULL
  w
}

#' Tabela 2 x 2 (grupy w wierszach): odsetki zdarzenia, różnica (RD), iloraz ryzyka (RR) i iloraz szans (OR)
miary_ryzyka <- function(tabela, zdarzenie = "tak") {
  t2 <- tabela[, c(zdarzenie, setdiff(colnames(tabela), zdarzenie))]
  a <- t2[1, 1]; b <- t2[1, 2]; c <- t2[2, 1]; d <- t2[2, 2]
  p1 <- a / (a + b); p2 <- c / (c + d)
  z <- stats::qnorm(0.975)
  se_rd <- sqrt(p1 * (1 - p1) / (a + b) + p2 * (1 - p2) / (c + d))
  se_rr <- sqrt(1 / a - 1 / (a + b) + 1 / c - 1 / (c + d))
  se_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  rr <- p1 / p2
  or <- (a * d) / (b * c)
  w <- data.frame(
    miara = c(paste0("p1: ", rownames(t2)[1]), paste0("p2: ", rownames(t2)[2]), "RD = p1 - p2", "RR = p1 / p2", "OR"),
    wartosc = c(p1, p2, p1 - p2, rr, or),
    dolna_95 = c(NA, NA, p1 - p2 - z * se_rd, rr * exp(-z * se_rr), or * exp(-z * se_or)),
    gorna_95 = c(NA, NA, p1 - p2 + z * se_rd, rr * exp(z * se_rr), or * exp(z * se_or)))
  w[-1] <- lapply(w[-1], round, 3)
  w
}

#' Test permutacyjny jednej średniej: losowa zmiana znaków odchyleń od wartości odniesienia
test_znakow <- function(x, mu0, B = 4999L, ziarno = 2026L) {
  d <- x[!is.na(x)] - mu0
  obserwowana <- mean(d)
  rozklad <- .zi_z_ziarnem(ziarno, replicate(B, mean(d * sample(c(-1, 1), length(d), replace = TRUE))))
  p <- (sum(abs(rozklad) >= abs(obserwowana)) + 1) / (B + 1)
  w <- data.frame(n = length(d), srednia = round(mean(x, na.rm = TRUE), 3), mu0 = mu0,
                  roznica = round(obserwowana, 3), B = B, p_perm = round(p, 4))
  attr(w, "rozklad") <- rozklad
  w
}

# Liczba w zapisie polskim (przecinek dziesiętny), np. do akapitów w książce kursu.
.zi_pl <- function(x, cyfry = 2) sub("^-", "−", formatC(x, format = "f", digits = cyfry, decimal.mark = ","))
# Wartość p z operatorem: "< 0,001" albo "= 0,012".
.zi_p <- function(p) if (p < 0.001) "< 0,001" else paste("=", .zi_pl(p, 3))

# ---------------------------------------------------------------------------------------------
# Szablony, kontrola środowiska i oddawanie
# ---------------------------------------------------------------------------------------------

.zi_foldery <- c(
  Z01 = "https://nc.uj.edu.pl/s/KtECTnaanMCQ7jm", Z02 = "https://nc.uj.edu.pl/s/SGifNFa5f97ezNx",
  Z03 = "https://nc.uj.edu.pl/s/yTFgjqKHFRXMGoT", Z04 = "https://nc.uj.edu.pl/s/4dENQZEg7qqkMcY",
  Z05 = "https://nc.uj.edu.pl/s/QqmoQQZjCadQq8D", Z06 = "https://nc.uj.edu.pl/s/8s8AYMZ9jmFJZD9",
  Z07 = "https://nc.uj.edu.pl/s/4zQw9wacmNwYRSx", Z08 = "https://nc.uj.edu.pl/s/NZdmG3KwxXnCAGQ",
  Z09 = "https://nc.uj.edu.pl/s/aH2yRXoR6r6dkCc", Z10 = "https://nc.uj.edu.pl/s/j2Jx2MEYDjMB738",
  PROJEKT = "https://nc.uj.edu.pl/s/4Cr3tne4GDLkn5s")

.zi_plik_studenta <- ".zi-student.yml"

.zi_nazwa_szablonu <- function(numer) {
  if (identical(tolower(as.character(numer)), "projekt")) return("projekt.Rmd")
  nr <- suppressWarnings(as.integer(numer))
  if (length(nr) != 1L || is.na(nr) || nr < 1L || nr > 10L) {
    stop("Podaj numer ćwiczenia od 1 do 10, np. pobierz_szablon(3).", call. = FALSE)
  }
  if (nr >= 9L) "projekt.Rmd" else sprintf("zadanie-%02d.Rmd", nr)
}

#' Pobranie pliku zadania z nagłówkiem uzupełnionym o ID oraz imię i nazwisko
pobierz_szablon <- function(numer, id = NULL, student = NULL, plik = NULL, nadpisz = FALSE) {
  nazwa <- .zi_nazwa_szablonu(numer)
  if (is.null(plik)) plik <- nazwa
  osoba <- if (file.exists(.zi_plik_studenta)) yaml::read_yaml(.zi_plik_studenta) else list()
  if (!is.null(id)) osoba$id <- trimws(id)
  if (!is.null(student)) osoba$student <- trimws(gsub("\\s+", " ", student))
  if (!is.null(id) || !is.null(student)) yaml::write_yaml(osoba, .zi_plik_studenta)
  if (file.exists(plik) && !nadpisz) {
    message("Plik ", plik, " już istnieje. Otwieram go bez nadpisywania.")
  } else {
    tekst <- readLines(paste0(.zi_adres, nazwa), encoding = "UTF-8", warn = FALSE)
    if (!is.null(osoba$student)) tekst <- sub('^author: "Imię Nazwisko"$', paste0('author: "', osoba$student, '"'), tekst)
    if (!is.null(osoba$id)) tekst <- sub('^  id: "TWOJE_ID"$', paste0('  id: "', osoba$id, '"'), tekst)
    polaczenie <- file(plik, open = "wb")
    writeLines(enc2utf8(tekst), polaczenie, useBytes = TRUE)
    close(polaczenie)
    message("Pobrano plik ", plik, if (is.null(osoba$id)) ". Wpisz w nagłówku swoje ID oraz imię i nazwisko." else
            paste0(" dla ID ", osoba$id, "."))
  }
  if (interactive()) utils::file.edit(plik)
  invisible(plik)
}

#' Sprawdzenie R, pakietów, Pandoc i XeLaTeX; pdf = TRUE składa próbny PDF
sprawdz_srodowisko <- function(pdf = FALSE) {
  wersje <- vapply(.zi_pakiety, function(p) as.character(tryCatch(utils::packageVersion(p), error = function(e) NA)), "")
  pandoc <- tryCatch(as.character(rmarkdown::pandoc_version()), error = function(e) NA)
  xelatex <- unname(Sys.which("xelatex"))
  cat("Kurs ZI, skrypt startowy ", .zi_wersja, "\n", R.version.string, " | ", utils::sessionInfo()$running, "\n",
      "Pandoc: ", if (is.na(pandoc)) "BRAK (uruchom w RStudio lub Positronie)" else pandoc, "\n",
      "XeLaTeX: ", if (nzchar(xelatex)) xelatex else "BRAK (przygotuj_pdf() instaluje TinyTeX)", "\n",
      "Pakiety: ", paste0(names(wersje), " ", ifelse(is.na(wersje), "BRAK", wersje), collapse = ", "), "\n", sep = "")
  wynik <- !anyNA(wersje) && !is.na(pandoc)
  if (pdf) {
    tmp <- tempfile("zi-test-")
    dir.create(tmp)
    rmd <- file.path(tmp, "test.Rmd")
    writeLines(enc2utf8(c("---", "title: Test", "output:", "  pdf_document:", "    latex_engine: xelatex", "---", "",
                          "Zażółć gęślą jaźń: $\\bar{x} = 2{,}5$, $\\alpha = 0{,}05$.")), rmd, useBytes = TRUE)
    ok <- tryCatch(file.exists(rmarkdown::render(rmd, quiet = TRUE)), error = function(e) FALSE)
    cat("Próbny PDF: ", if (ok) "OK" else "BŁĄD (uruchom przygotuj_pdf())", "\n", sep = "")
    wynik <- wynik && ok
  }
  invisible(wynik)
}

#' Instalacja TinyTeX i pakietów LaTeX potrzebnych do PDF (jednorazowo, kilka minut)
przygotuj_pdf <- function() {
  if (!nzchar(Sys.which("xelatex"))) {
    message("Instaluję TinyTeX (jednorazowo, kilka minut). Nie zamykaj sesji R.")
    tinytex::install_tinytex()
  }
  if (isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))) {
    potrzebne <- c("fontspec", "unicode-math", "lm", "lm-math", "babel-polish", "hyphen-polish", "geometry",
                   "hyperref", "bookmark", "booktabs", "fancyvrb", "float", "framed", "xcolor", "upquote",
                   "microtype", "parskip", "xurl", "fvextra", "lineno", "etoolbox")
    brak <- setdiff(potrzebne, tinytex::tl_pkgs())
    if (length(brak) > 0L) tinytex::tlmgr_install(brak)
  }
  invisible(sprawdz_srodowisko(pdf = TRUE))
}

.zi_naglowek <- function(plik) {
  yaml <- tryCatch(rmarkdown::yaml_front_matter(plik), error = function(e) {
    stop("Nagłówek pliku ", basename(plik), " jest uszkodzony: ", conditionMessage(e),
         "\nPorównaj go z nowym plikiem: pobierz_szablon(..., plik = \"wzor.Rmd\").", call. = FALSE)
  })
  autor <- yaml$author
  autor <- if (is.character(autor) && length(autor) == 1L) trimws(gsub("\\s+", " ", autor)) else ""
  slowa <- strsplit(autor, " ", fixed = TRUE)[[1]]
  if (length(slowa) < 2L || autor == "Imię Nazwisko") {
    stop("Wpisz swoje imię i nazwisko w nagłówku pliku ", basename(plik), ", np.\n",
         "  author: \"Anna Kowalska\"\nZapisz plik i uruchom polecenie ponownie.", call. = FALSE)
  }
  id <- yaml$params$id
  id <- if (is.list(id)) id$value else id
  if (is.null(id) || !nzchar(trimws(id)) || identical(trimws(id), "TWOJE_ID")) {
    stop("Wpisz swoje ID w nagłówku pliku ", basename(plik), ":\n  params:\n    id: \"s017\"\n",
         "Zapisz plik i uruchom polecenie ponownie.", call. = FALSE)
  }
  list(imie = paste(slowa[-length(slowa)], collapse = " "), nazwisko = slowa[length(slowa)], id = trimws(id))
}

# "Gęślą Zażółć" -> "gesla_zazolc"; polskie litery zamieniane jawnie, bo iconv różni się między systemami.
.zi_do_nazwy <- function(x) {
  x <- chartr("ąćęłńóśźżĄĆĘŁŃÓŚŹŻ", "acelnoszzACELNOSZZ", x)
  x <- iconv(gsub("'", "", x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- gsub("['`^~\"]", "", x)
  x <- gsub("[^a-z0-9-]+", "_", tolower(x))
  gsub("^_+|_+$", "", x)
}

# Plik ćwiczenia rozpoznawany po nagłówkach zadań; projekt po nagłówkach części P1-P10.
.zi_numery <- function(plik) {
  linie <- readLines(plik, encoding = "UTF-8", warn = FALSE)
  numery <- unique(as.integer(sub("^## Zadanie ([0-9]+)\\..*$", "\\1", grep("^## Zadanie [0-9]+\\.", linie, value = TRUE))))
  if (any(grepl("^## P[0-9]+\\.", linie))) numery <- c(numery, 9L, 10L)
  numery
}

.zi_znajdz_plik <- function(nr) {
  pliki <- list.files(pattern = "\\.Rmd$", ignore.case = TRUE)
  pasujace <- pliki[vapply(pliki, function(p) nr %in% .zi_numery(p), logical(1))]
  if (length(pasujace) == 0L) {
    stop("W folderze ", normalizePath(getwd(), winslash = "/"), " nie ma pliku ćwiczenia ", nr, ".\n",
         "Pobierz go poleceniem pobierz_szablon(", nr, ") albo wskaż plik: oddaj_zadanie(", nr, ", plik = \"moj-plik.Rmd\").",
         call. = FALSE)
  }
  pasujace <- pasujace[order(file.mtime(pasujace), decreasing = TRUE)]
  if (length(pasujace) > 1L) message("Oddaję najnowszy plik: ", pasujace[1], ".")
  pasujace[1]
}

.zi_zapisz_otwarte <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    try(rstudioapi::documentSaveAll(), silent = TRUE)
  }
}

# Render w nowym procesie R: wynik zależy tylko od pliku, jak przy przycisku Knit po restarcie R.
# Plik powstaje obok Rmd (rysunki mają wtedy ścieżki względne) i dopiero potem trafia do wyniki/.
.zi_renderuj <- function(plik, nazwa, format) {
  katalog <- dirname(plik)
  wyjscie <- file.path(katalog, "wyniki", nazwa)
  dir.create(file.path(katalog, "wyniki"), showWarnings = FALSE)
  for (f in c(wyjscie, file.path(katalog, nazwa))) if (file.exists(f)) file.remove(f)
  if (!nzchar(Sys.getenv("RSTUDIO_PANDOC"))) Sys.setenv(RSTUDIO_PANDOC = rmarkdown::find_pandoc()$dir)
  kod <- sprintf("rmarkdown::render(%s, output_format = '%s', output_file = %s, quiet = TRUE)",
                 encodeString(basename(plik), quote = "'"), format, encodeString(nazwa, quote = "'"))
  stary <- setwd(katalog)
  on.exit(setwd(stary))
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(kod)), stdout = TRUE, stderr = TRUE))
  if (file.exists(nazwa)) file.rename(nazwa, file.path("wyniki", nazwa))
  ok <- is.null(attr(log, "status")) && file.exists(wyjscie) && file.size(wyjscie) > 1000
  if (ok && format == "pdf_document") ok <- identical(readBin(wyjscie, "raw", 5L), charToRaw("%PDF-"))
  list(ok = ok, plik = wyjscie, log = log, blad_kodu = any(grepl("^Quitting from", log)))
}

.zi_fragment_logu <- function(log) {
  log <- log[nzchar(trimws(log)) & !grepl("^Execution halted", log)]
  i <- grep("^Quitting from", log)[1]
  if (!is.na(i)) {
    e <- utils::tail(grep("^(Error|Błąd)", log[seq_len(i)]), 1L)
    if (length(e) == 0L) return(paste(log[i:min(length(log), i + 8L)], collapse = "\n"))
    return(paste(log[e:i], collapse = "\n"))
  }
  i <- grep("^! ", log)
  if (length(i) > 0L) return(paste(log[sort(unique(pmin(c(i, i + 1L), length(log))))], collapse = "\n"))
  paste(utils::tail(log, 12L), collapse = "\n")
}

# Folder „File drop” w Nextcloud przyjmuje plik bez logowania; NULL oznacza sukces.
.zi_wyslij <- function(sciezka, nazwa, link, podpis) {
  url <- paste0(sub("/s/.*$", "", link), "/public.php/dav/files/", sub("^.*/s/", "", link), "/",
                utils::URLencode(nazwa, reserved = TRUE))
  naglowki <- c("X-Requested-With: XMLHttpRequest",
                paste0("X-NC-Nickname: ", utils::URLencode(enc2utf8(podpis), reserved = TRUE)))
  for (proba in 1:2) {
    odp <- tryCatch(curl::curl_upload(sciezka, url, verbose = FALSE, httpheader = naglowki,
                                      connecttimeout = 30, timeout = 300), error = function(e) e)
    if (!inherits(odp, "error") || proba == 2L) break
    Sys.sleep(5)
  }
  if (inherits(odp, "error")) return(paste0("brak połączenia z serwerem (", conditionMessage(odp), ")"))
  kod <- odp$status_code
  if (identical(kod, 503L) && grepl("NotFound", rawToChar(odp$content[odp$content != as.raw(0)]), fixed = TRUE)) kod <- 404L
  switch(as.character(kod),
         "201" = , "204" = NULL,
         "404" = "folder nie przyjmuje już plików (minął termin albo link wygasł)",
         "401" = , "403" = "folder nie przyjmuje plików bez hasła albo został zamknięty",
         "413" = "plik jest za duży",
         "507" = "w folderze prowadzącego zabrakło miejsca",
         paste0("serwer odrzucił plik (HTTP ", kod, ")"))
}

.zi_oddaj <- function(klucz, plik, wyslij, z_rmd = FALSE) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    stop("Uruchom polecenie oddania w konsoli, nie w bloku dokumentu.", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Brak programu Pandoc. Uruchom polecenie w RStudio albo Positronie.", call. = FALSE)
  }
  plik <- normalizePath(plik, winslash = "/")
  osoba <- .zi_naglowek(plik)
  nazwa <- paste(.zi_do_nazwy(osoba$id), .zi_do_nazwy(osoba$nazwisko), .zi_do_nazwy(osoba$imie), klucz, sep = "_")
  message("Tworzę PDF z pliku ", basename(plik), " w nowej sesji R. Może to potrwać około minuty...")
  wynik <- .zi_renderuj(plik, paste0(nazwa, ".pdf"), "pdf_document")
  if (!wynik$ok && wynik$blad_kodu) {
    stop("Plik nie tworzy dokumentu w nowej sesji R:\n\n", .zi_fragment_logu(wynik$log), "\n\n",
         "Popraw wskazany blok (Knit pokaże ten sam błąd) i uruchom polecenie ponownie.", call. = FALSE)
  }
  if (!wynik$ok) {
    message("PDF nie powstał, prawdopodobnie z powodu LaTeX-a:\n", .zi_fragment_logu(wynik$log),
            "\nTworzę wersję HTML, aby praca mogła zostać oddana. Na przyszłość uruchom przygotuj_pdf().")
    wynik_pdf <- wynik
    wynik <- .zi_renderuj(plik, paste0(nazwa, ".html"), "html_document")
    if (!wynik$ok) {
      stop("Nie udało się utworzyć ani PDF, ani HTML:\n\n", .zi_fragment_logu(c(wynik_pdf$log, wynik$log)), call. = FALSE)
    }
  }
  pliki <- stats::setNames(wynik$plik, basename(wynik$plik))
  if (z_rmd) pliki <- c(pliki, stats::setNames(plik, paste0(nazwa, ".Rmd")))
  message("\nGotowe do oddania (", klucz, "):\n", paste0("  ", names(pliki), collapse = "\n"),
          "\n  autor: ", osoba$imie, " ", osoba$nazwisko, ", ID ", osoba$id)
  if (!wyslij) {
    message("Nie wysyłam (wyslij = FALSE).")
    return(invisible(unname(pliki)))
  }
  if (interactive()) {
    tryCatch(utils::browseURL(wynik$plik), error = function(e) NULL)
    odp <- readline("Obejrzyj plik. Wysłać go prowadzącemu? [t/n]: ")
    if (!tolower(trimws(odp)) %in% c("t", "tak")) {
      message("Nie wysłano. Gdy będziesz gotowy, powtórz polecenie oddania.")
      return(invisible(unname(pliki)))
    }
  }
  for (n in names(pliki)) {
    problem <- .zi_wyslij(pliki[[n]], n, .zi_foldery[[klucz]], paste(osoba$nazwisko, osoba$imie, osoba$id))
    if (!is.null(problem)) {
      stop("Nie wysłano pliku ", n, ": ", problem, ".\nPlik jest gotowy: ", pliki[[n]],
           "\nMożesz go wgrać ręcznie: otwórz ", .zi_foldery[[klucz]], " i przeciągnij plik do okna przeglądarki.",
           call. = FALSE)
    }
  }
  message("Oddano ", klucz, " (", format(Sys.time(), "%d.%m.%Y %H:%M"), "). ",
          "Poprawioną wersję oddajesz tym samym poleceniem; trafi obok poprzedniej.")
  invisible(unname(pliki))
}

#' Oddanie ćwiczenia: PDF z pliku zadania trafia do folderu ćwiczenia w Nextcloud
oddaj_zadanie <- function(numer, plik = NULL, wyslij = TRUE) {
  nr <- suppressWarnings(as.integer(numer))
  if (length(nr) != 1L || is.na(nr) || nr < 1L || nr > 10L) stop("Podaj numer ćwiczenia od 1 do 10, np. oddaj_zadanie(3).", call. = FALSE)
  .zi_zapisz_otwarte()
  if (is.null(plik)) plik <- .zi_znajdz_plik(nr)
  if (!file.exists(plik)) stop("Nie ma pliku ", plik, ".", call. = FALSE)
  .zi_oddaj(sprintf("Z%02d", nr), plik, wyslij)
}

#' Oddanie projektu: PDF i plik Rmd trafiają do folderu projektu w Nextcloud
oddaj_projekt <- function(plik = "projekt.Rmd", wyslij = TRUE) {
  .zi_zapisz_otwarte()
  if (!file.exists(plik)) stop("Nie ma pliku ", plik, ". Pobierz go poleceniem pobierz_szablon(\"projekt\").", call. = FALSE)
  .zi_oddaj("PROJEKT", plik, wyslij, z_rmd = TRUE)
}

if (!isTRUE(getOption("knitr.in.progress"))) {
  message("Kurs ZI (skrypt ", .zi_wersja, "): pakiety gotowe. Dane: dane_zi(\"S02\", id = \"s017\"). ",
          "Plik zajęć: pobierz_szablon(1). Oddanie: oddaj_zadanie(1).")
}
