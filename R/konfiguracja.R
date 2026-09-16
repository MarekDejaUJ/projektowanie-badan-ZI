#' Konfiguracja rocznika
#' @param rocznik Rocznik, np. "2026-27".
#' @param plik Własny plik YAML; domyślnie konfiguracja z pakietu.
#' @return Lista parametrów rocznika.
#' @expor
#' @examples
#' konfiguracja_kursu()$cwiczenia
konfiguracja_kursu <- function(rocznik = "2026-27", plik = NULL) {
  sprawdz_id(rocznik, "rocznik", "^[0-9]{4}-[0-9]{2}$")
  if (is.null(plik)) plik <- zasob("kurs", paste0(rocznik, ".yml"))
  if (!file.exists(plik)) stop("Nie ma konfiguracji rocznika.", call. = FALSE)
  czytaj_yaml(plik)
}

czytaj_yaml <- function(plik) {
  yaml::yaml.load(paste(readLines(plik, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
                  eval.expr = FALSE)
}

#' Sprawdzenie kompletności zasad rocznika
#' @param konfiguracja Lista zwrócona przez konfiguracja_kursu().
#' @param publikacja Czy błędy i brak zasad oceny mają blokować publikację.
#' Nieogłoszony pełny kalendarz pozostaje jawną informacją do ustalenia.
#' @return Ramka problemów i ich wagi (blad albo informacja).
#' @expor
#' @examples
#' sprawdz_konfiguracje(konfiguracja_kursu())
sprawdz_konfiguracje <- function(konfiguracja, publikacja = FALSE) {
  if (!is.list(konfiguracja)) stop("Konfiguracja musi by\u0107 list\u0105 z pliku YAML.", call. = FALSE)
  wymagane <- c("termin_projektu", "termin_spozniony", "termin_poprawek", "zasada_terminow_zadan", "wagi", "progi_ocen",
                "spoznienia", "poprawki", "zaokraglenie", "laczenie_ocen", "zasady_si", "opis_si", "zrodlo_zasad_si")
  wynik <- data.frame(pole = character(), problem = character(), waga = character())
  dodaj <- function(pole, opis, waga = "blad") {
    wynik <<- rbind(wynik, data.frame(pole = pole, problem = opis, waga = waga))
  }
  for (p in wymagane) if (is.null(konfiguracja[[p]])) dodaj(p, "Do ustalenia przez prowadz\u0105cego")
  k <- konfiguracja
  schema <- jsonlite::read_json(zasob("kurs", "schema.json"))
  for (p in setdiff(unlist(schema$required), names(k))) dodaj(p, "Brak klucza wymaganego w schema.json")
  for (p in setdiff(names(k), names(schema$properties))) dodaj(p, "Nieznany klucz konfiguracji")
  if (is.null(k$daty_zajec)) dodaj("daty_zajec", "Pe\u0142ny kalendarz do ustalenia", "informacja")
  for (p in c("rocznik", "tytul", "kierunek", "kod", "projekt", "wersja_pakietu", "wersja_generatora", "wersja_rubryk", "wlasciciel_repozytoriow"))
    if (!is.character(k[[p]]) || length(k[[p]]) != 1L || is.na(k[[p]]) || !nzchar(k[[p]]))
      dodaj(p, "Wymagany niepusty tekst")
  if (!identical(k$adapter, "github")) dodaj("adapter", "Obs\u0142ugiwany adapter: github")
  if (!is.character(k$logowanie) || length(k$logowanie) != 1L || is.na(k$logowanie) || !k$logowanie %in% c("auto", "gh", "device"))
    dodaj("logowanie", "Wybierz auto, gh albo device")
  if (identical(k$logowanie, "device") && (is.null(k$oauth_client_id) || !nzchar(k$oauth_client_id)))
    dodaj("oauth_client_id", "Logowanie device wymaga client ID aplikacji")
  for (p in c("cwiczenia", "wyklady", "minuty_spotkania")) {
    oczekiwane <- c(cwiczenia = 10L, wyklady = 5L, minuty_spotkania = 90L)[[p]]
    v <- k[[p]]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v != oczekiwane)
      dodaj(p, "Nieprawid\u0142owy wymiar zaj\u0119\u0107 tej cz\u0119\u015bci")
  }
  zakres <- unlist(k$zakres_sylabusa)
  if (!is.numeric(zakres) || !identical(as.numeric(zakres), as.numeric(9:15)))
    dodaj("zakres_sylabusa", "Wymagany zakres 9--15")
  if (!identical(k$egzamin, FALSE)) dodaj("egzamin", "Ta cz\u0119\u015b\u0107 nie ma egzaminu")
  if (!(is.character(k$timezone) && length(k$timezone) == 1L && k$timezone %in% OlsonNames()))
    dodaj("timezone", "Nieznana strefa czasowa")
  czas <- function(x) !is.na(czas_iso(x))
  daty <- c("termin_projektu", "termin_spozniony", "termin_poprawek", "koniec_zajec")
  for (p in daty) if (!is.null(k[[p]]) && !czas(k[[p]])) dodaj(p, "U\u017cyj rzeczywistej daty ISO 8601 z przesuni\u0119ciem strefy")
  if (all(vapply(k[daty[1:3]], czas, logical(1))) &&
      any(diff(vapply(k[daty[1:3]], function(x) as.numeric(czas_iso(x)), numeric(1))) < 0))
    dodaj("termin_poprawek", "Termin projektu, sp\u00f3\u017anienia i poprawek musz\u0105 wyst\u0119powa\u0107 w tej kolejno\u015bci")
  if (!is.character(k$zasada_terminow_zadan) || length(k$zasada_terminow_zadan) != 1L ||
      is.na(k$zasada_terminow_zadan) || !k$zasada_terminow_zadan %in% c("nastepne_cwiczenie", "daty"))
    dodaj("zasada_terminow_zadan", "Wybierz nastepne_cwiczenie albo daty")
  if (!is.null(k$terminy_zadan) && (!is.list(k$terminy_zadan) ||
      any(!names(k$terminy_zadan) %in% sprintf("Z%02d", 1:10)) || anyDuplicated(names(k$terminy_zadan)) ||
      !all(vapply(Filter(Negate(is.null), k$terminy_zadan), czas, logical(1)))))
    dodaj("terminy_zadan", "Jawne terminy musz\u0105 mie\u0107 unikalne ID Z01--Z10 i daty ISO 8601")
  if (!is.list(k$terminy_zadan) || !czas(k$terminy_zadan$Z10)) dodaj("terminy_zadan", "Ustal osobny termin Z10 po ostatnich \u0107wiczeniach")
  if (identical(k$zasada_terminow_zadan, "daty") &&
      (!setequal(names(k$terminy_zadan), sprintf("Z%02d", 1:10)) || !all(vapply(k$terminy_zadan, czas, logical(1)))))
    dodaj("terminy_zadan", "Dla zasady daty wymagane s\u0105 wszystkie Z01--Z10")
  if (!is.null(k$daty_zajec) && (!is.list(k$daty_zajec) ||
      any(!names(k$daty_zajec) %in% c(sprintf("C%02d", 1:10), sprintf("W%02d", 1:5))) ||
      anyDuplicated(names(k$daty_zajec)) || !all(vapply(k$daty_zajec, czas, logical(1)))))
    dodaj("daty_zajec", "Podaj unikalne C01--C10/W01--W05 i daty ISO 8601; nieznany kalendarz pozostaw null")
  if (!is.null(k$wagi)) {
    w <- unlist(k$wagi)
    if (!is.numeric(w) || !setequal(names(w), c("zadania", "projekt")) || anyNA(w) ||
        any(!is.finite(w)) || any(w < 0 | w > 1) || abs(sum(w) - 1) > 1e-8) dodaj("wagi", "Wagi zadania/projekt musz\u0105 sumowa\u0107 si\u0119 do 1")
  }
  if (!is.null(k$progi_ocen)) {
    p <- unlist(k$progi_ocen)
    if (!is.numeric(p) || length(p) != 5L || !identical(names(p), sprintf("%.1f", seq(3, 5, .5))) ||
        anyNA(p) || any(!is.finite(p)) || any(p < 0 | p > 100) || any(diff(p) <= 0))
      dodaj("progi_ocen", "Progi procentowe musz\u0105 rosn\u0105\u0107 i mie\u015bci\u0107 si\u0119 w 0--100")
  }
  limit <- k$maksymalna_ocena_spozniona
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || !limit %in% seq(3, 5, .5))
    dodaj("maksymalna_ocena_spozniona", "Wymagana ocena 3--5 co 0,5")
  if (!is.character(k$zasady_si) || length(k$zasady_si) != 1L || is.na(k$zasady_si) ||
      !k$zasady_si %in% c("pomoc_w_kodzie", "pomoc_w_redakcji", "bez_si"))
    dodaj("zasady_si", "Wybierz og\u0142oszon\u0105 zasad\u0119 korzystania z SI")
  for (p in c("spoznienia", "poprawki", "zaokraglenie", "laczenie_ocen", "opis_si", "zrodlo_zasad_si"))
    if (!is.null(k[[p]]) && (!is.character(k[[p]]) || length(k[[p]]) != 1L || !nzchar(k[[p]])))
      dodaj(p, "Opisz og\u0142oszon\u0105 zasad\u0119 jednym niepustym tekstem")
  if (publikacja && any(wynik$waga == "blad")) stop("Zasady rocznika wymagaj\u0105 poprawienia: ",
                                      paste(unique(wynik$pole[wynik$waga == "blad"]), collapse = ", "), call. = FALSE)
  wynik
}

zasob <- function(...) {
  sciezka <- system.file(..., package = "badaniaZI")
  if (!nzchar(sciezka)) stop("Nie ma zasobu w zainstalowanym pakiecie.", call. = FALSE)
  sciezka
}

sprawdz_id <- function(x, nazwa, wzorzec = "^[A-Za-z0-9][A-Za-z0-9_-]{1,39}$") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !grepl(wzorzec, x))
    stop("Niepoprawne pole ", nazwa, ".", call. = FALSE)
  invisible(x)
}

lokalny_rng <- function(seed, kod) {
  byl <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (byl) stary <- get(".Random.seed", envir = .GlobalEnv)
  rodzaj <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(rodzaj))
    if (byl) assign(".Random.seed", stary, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(kod)
}
