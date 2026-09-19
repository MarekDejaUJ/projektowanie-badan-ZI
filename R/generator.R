#' Katalog scenariuszy projektu
#' @return Ramka z identyfikatorami, tytułami i drugą analizą.
#' @export
#' @examples
#' scenariusze()
scenariusze <- function() {
  x <- czytaj_yaml(zasob("scenariusze", "katalog.yml"))$scenariusze
  data.frame(id = vapply(x, `[[`, character(1), "id"),
             tytul = vapply(x, `[[`, character(1), "tytul"),
             druga_analiza = vapply(x, `[[`, character(1), "druga_analiza"))
}

#' Szczegóły scenariusza
#' @param id Identyfikator od S01 do S20.
#' @return Lista: opis, grupy, pomiar, kwestionariusz i plan analiz.
#' @export
#' @examples
#' scenariusz("S03")$grupy
scenariusz <- function(id) {
  sprawdz_id(id, "scenariusz", "^S(0[1-9]|1[0-9]|20)$")
  katalog <- czytaj_yaml(zasob("scenariusze", "katalog.yml"))$scenariusze
  katalog[[match(id, vapply(katalog, `[[`, character(1), "id"))]]
}

#' Indywidualne syntetyczne dane ankietowe i obserwacyjne
#'
#' Dane służą do ćwiczenia analizy, a nie opisania rzeczywistej instytucji.
#' Łączą samoocenę z obserwowanym wykonaniem krótkiego zadania wyszukiwawczego.
#' Zawierają kontrolowane braki, dwa duplikaty i dwa nietypowe czasy.
#' Identyfikator jest pseudonimem przydzielonym na zajęciach, nie numerem albumu.
#' @param id Pseudonim studenta, np. "s017".
#' @param scenariusz Identyfikator S01--S20.
#' @param rocznik Rocznik kursu.
#' @param n Liczba wierszy surowych danych, od 120 do 180.
#' @return Lista z danymi, słownikiem, opisem scenariusza i manifestem.
#' @export
#' @examples
#' x <- generuj_dane("s017", "S03")
#' head(x$dane)
generuj_dane <- function(id, scenariusz = "S01", rocznik = "2026-27", n = 150L) {
  sprawdz_id(id, "id")
  cfg <- konfiguracja_kursu(rocznik)
  opis <- get("scenariusz", mode = "function")(scenariusz)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 120 || n > 180 || n != as.integer(n))
    stop("Liczba wierszy n musi by\u0107 liczb\u0105 ca\u0142kowit\u0105 od 120 do 180.", call. = FALSE)
  wersja <- cfg$wersja_generatora
  klucz <- digest::digest(paste(id, scenariusz, rocznik, wersja, n, sep = "|"),
                         algo = "sha256", serialize = FALSE)
  seed <- strtoi(substr(klucz, 1L, 7L), base = 16L)
  dane <- lokalny_rng(seed, {
    m <- n - 2L
    g <- sample(rep(0:1, length.out = m))
    czestosc <- sample(1:5, m, replace = TRUE, prob = c(.12, .2, .26, .25, .17))
    efekt <- opis$efekt_grup + stats::runif(1L, -.08, .08)
    utajony <- 3.1 + efekt * g + .18 * (czestosc - 3) + stats::rnorm(m, sd = .6)
    pozycje <- replicate(6L, as.integer(pmax(1L, pmin(5L,
                          round(utajony + stats::rnorm(m, sd = .65))))))
    pozycje[, 3L] <- 6L - pozycje[, 3L]
    czas <- round(pmax(.2, pmin(120, stats::rlnorm(m,
                          meanlog = log(opis$czas_typowy) - .16 * (utajony - 3), sdlog = .5))), 1L)
    powodzenie <- stats::rbinom(m, 1L, stats::plogis(.25 + .55 * (utajony - 3) + .2 * g))
    kanaly <- replicate(4L, stats::rbinom(m, 1L, stats::runif(1L, .25, .65)))
    x <- data.frame(id_odpowiedzi = sprintf("o%03d", seq_len(m)),
                    grupa = opis$grupy[g + 1L], czestosc_korzystania = czestosc,
                    czas_wyszukiwania = czas, stringsAsFactors = FALSE)
    for (j in 1:6) x[[paste0("pozycja_", j)]] <- pozycje[, j]
    x$powodzenie <- powodzenie
    for (j in 1:4) x[[paste0("kanal_", j)]] <- kanaly[, j]
    # Brak odpowiedzi nie jest odpowiedzią negatywną; 99 to udokumentowany kod braku.
    for (j in 1:6) {
      w <- sample.int(m, max(2L, round(.03 * m)))
      x[w, paste0("pozycja_", j)] <- if (j == 2L) 99L else NA_integer_
    }
    x$czestosc_korzystania[sample.int(m, round(.03 * m))] <- NA_integer_
    x$czas_wyszukiwania[sample.int(m, round(.03 * m))] <- NA_real_
    x$czas_wyszukiwania[sample.int(m, 2L)] <- c(-5, 999)
    w <- sample.int(m, round(.03 * m))
    for (j in 1:4) x[w, paste0("kanal_", j)] <- NA_integer_
    x <- rbind(x, x[sample.int(m, 2L), , drop = FALSE])
    rownames(x) <- NULL
    x
  })
  slownik <- slownik_scenariusza(opis)
  manifest <- list(id = id, scenariusz = scenariusz, rocznik = rocznik,
                   wersja_generatora = wersja, n = as.integer(n), klucz_wariantu = klucz,
                   seed = seed, rng = c("Mersenne-Twister", "Inversion", "Rejection"),
                   wersja_R = as.character(getRversion()), syntetyczne = TRUE,
                   hash_danych = hash_tabeli(dane))
  structure(list(dane = dane, slownik = slownik, scenariusz = opis, manifest = manifest),
            class = "badaniaZI_dane")
}

hash_tabeli <- function(dane) {
  tekst <- jsonlite::toJSON(dane, dataframe = "rows", na = "null", digits = NA)
  digest::digest(enc2utf8(as.character(tekst)), algo = "sha256", serialize = FALSE)
}

slownik_scenariusza <- function(opis) {
  nazwy <- c("id_odpowiedzi", "grupa", "czestosc_korzystania", "czas_wyszukiwania",
             paste0("pozycja_", 1:6), "powodzenie", paste0("kanal_", 1:4))
  data.frame(zmienna = nazwy,
    opis = c("Identyfikator odpowiedzi; identyczne powt\u00f3rzenie to duplikat", "Grupa por\u00f3wnania",
             "Cz\u0119sto\u015b\u0107 korzystania z zasobu", "Obserwowany czas wykonania zadania wyszukiwawczego w minutach",
             unlist(opis$pozycje), paste("Obserwowany wynik zadania:", opis$pytanie_binarne), paste("Wybrano kana\u0142:", unlist(opis$kanaly))),
    skala = c("identyfikator", "nominalna", "porz\u0105dkowa", "ilorazowa",
              rep("porz\u0105dkowa", 6), "nominalna", rep("nominalna", 4)),
    kodowanie = c("o001...", paste(opis$grupy, collapse = " / "),
                   "1=nigdy; 2=rzadko; 3=czasami; 4=cz\u0119sto; 5=bardzo cz\u0119sto",
                   "0--120 minut; warto\u015bci poza zakresem wymagaj\u0105 oceny",
                   rep("1=zdecydowanie nie; 2=raczej nie; 3=ani tak, ani nie; 4=raczej tak; 5=zdecydowanie tak", 6),
                   "0=nie; 1=tak", rep("0=nie wybrano; 1=wybrano", 4)),
    braki = c("brak niedopuszczalny", "brak niedopuszczalny", "NA", "NA",
               "NA", "99 lub NA", rep("NA", 4), "NA", rep("NA; wszystkie cztery puste = brak odpowiedzi", 4)),
    odwrocona = nazwy == "pozycja_3", stringsAsFactors = FALSE)
}

#' Zapis danych i manifestu
#' @param dane Wynik generuj_dane().
#' @param katalog Nowy katalog docelowy; istniejący nie jest nadpisywany.
#' @return Ścieżka katalogu, niewidocznie.
#' @export
#' @examples
#' katalog <- tempfile("ankieta-")
#' zapisz_dane(generuj_dane("s017"), katalog)
#' unlink(katalog, recursive = TRUE)
zapisz_dane <- function(dane, katalog) {
  if (!inherits(dane, "badaniaZI_dane")) stop("U\u017cyj wyniku generuj_dane().", call. = FALSE)
  if (dir.exists(katalog) || file.exists(katalog)) stop("Katalog ju\u017c istnieje; wybierz now\u0105 nazw\u0119.", call. = FALSE)
  if (!identical(hash_tabeli(dane$dane), dane$manifest$hash_danych))
    stop("Dane zmieniono po wygenerowaniu. Wygeneruj ponownie surow\u0105 wersj\u0119.", call. = FALSE)
  dir.create(dirname(katalog), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("dane-", tmpdir = dirname(katalog))
  if (!dir.create(tmp)) stop("Nie mo\u017cna utworzy\u0107 katalogu danych.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  pisz_csv(dane$dane, file.path(tmp, "surowe.csv"))
  pisz_csv(dane$slownik, file.path(tmp, "slownik.csv"))
  saveRDS(dane$dane, file.path(tmp, "surowe.rds"), version = 2)
  manifest <- dane$manifest
  manifest$sha256_csv <- digest::digest(file = file.path(tmp, "surowe.csv"), algo = "sha256")
  manifest$sha256_rds <- digest::digest(file = file.path(tmp, "surowe.rds"), algo = "sha256")
  jsonlite::write_json(manifest, file.path(tmp, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  if (!file.rename(tmp, katalog)) stop("Nie mo\u017cna przenie\u015b\u0107 danych do katalogu docelowego.", call. = FALSE)
  invisible(normalizePath(katalog, winslash = "/"))
}

pisz_csv <- function(x, plik) {
  for (i in seq_along(x)) if (is.character(x[[i]])) x[[i]] <- enc2utf8(x[[i]])
  # Połączenie binarne zachowuje LF także w Windows; hash nie zależy od checkoutu.
  con <- file(plik, open = "wb")
  on.exit(close(con), add = TRUE)
  utils::write.table(x, con, sep = ",", dec = ".", row.names = FALSE, na = "NA", eol = "\n")
}

#' Kontrola unikalności wariantów w prywatnym rosterze
#' @param id Wektor pseudonimów.
#' @param scenariusze Wektor scenariuszy tej samej długości albo jeden scenariusz.
#' @param rocznik Rocznik.
#' @return Prywatna tabela kluczy; błąd przy powtórzeniu ID lub kolizji RNG.
#' @export
#' @examples
#' sprawdz_warianty(c("s001", "s002"), "S01")
sprawdz_warianty <- function(id, scenariusze, rocznik = "2026-27") {
  if (anyDuplicated(id)) stop("Roster zawiera powt\u00f3rzone ID.", call. = FALSE)
  if (length(scenariusze) == 1L) scenariusze <- rep(scenariusze, length(id))
  if (!length(id) || length(scenariusze) != length(id)) stop("Podaj ID i scenariusze tej samej d\u0142ugo\u015bci.", call. = FALSE)
  x <- lapply(seq_along(id), function(i) generuj_dane(id[i], scenariusze[i], rocznik)$manifest)
  wynik <- data.frame(id = id, scenariusz = scenariusze,
                     klucz = vapply(x, `[[`, character(1), "klucz_wariantu"),
                     seed = vapply(x, `[[`, integer(1), "seed"))
  if (anyDuplicated(wynik$seed)) stop("Kolizja wariant\u00f3w RNG; przydziel inny pseudonim i sprawd\u017a ponownie.", call. = FALSE)
  wynik
}
