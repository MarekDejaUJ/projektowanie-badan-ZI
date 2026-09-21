# Kontekst istnieje tylko w pamięci R, bez tokenów i bez zmiany getwd().
sesja_pracy <- new.env(parent = emptyenv())

znajdz_katalog_kursu <- function(start) {
  if (!is.character(start) || length(start) != 1L || is.na(start) || !dir.exists(start))
    return(NULL)
  start <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(start, "kurs.yml"))) return(start)
    rodzic <- dirname(start)
    if (identical(rodzic, start)) return(NULL)
    start <- rodzic
  }
}

zapamietaj_prace <- function(katalog, cwiczenie) {
  root <- znajdz_katalog_kursu(katalog)
  if (is.null(root)) stop("Brak konfiguracji w\u0142asnej pracy.", call. = FALSE)
  cfg <- czytaj_yaml(file.path(root, "kurs.yml"))
  sprawdz_id(cfg$id, "ID pracy")
  sesja_pracy$katalog <- root
  sesja_pracy$tozsamosc <- cfg[c("id", "rocznik", "repo")]
  sesja_pracy$cwiczenie <- cwiczenie
  invisible(root)
}

katalog_kursu <- function(katalog = NULL) {
  if (!is.null(katalog)) {
    root <- znajdz_katalog_kursu(katalog)
  } else if (!is.null(sesja_pracy$katalog)) {
    root <- sesja_pracy$katalog
    if (!dir.exists(root) || !file.exists(file.path(root, "kurs.yml")))
      stop("Znikn\u0105\u0142 katalog zapami\u0119tanej pracy. Uruchom ponownie rozpocznij_zajecia() z w\u0142a\u015bciwym katalogiem.", call. = FALSE)
    cfg <- czytaj_yaml(file.path(root, "kurs.yml"))
    if (!identical(cfg[c("id", "rocznik", "repo")], sesja_pracy$tozsamosc))
      stop("Konfiguracja zapami\u0119tanej pracy zmieni\u0142a ID, rocznik lub repozytorium. Wska\u017c w\u0142asny katalog i rozpocznij zaj\u0119cia ponownie.", call. = FALSE)
    lokalny <- znajdz_katalog_kursu(getwd())
    if (!is.null(lokalny) && !identical(lokalny, root))
      stop("Bie\u017c\u0105cy katalog i zapami\u0119tana praca wskazuj\u0105 r\u00f3\u017cne przestrzenie. Podaj jawnie katalog w\u0142asnej pracy.", call. = FALSE)
  } else root <- znajdz_katalog_kursu(getwd())
  if (is.null(root))
    stop("Nie znaleziono w\u0142asnej przestrzeni \u0107wicze\u0144. Odtw\u00f3rz prac\u0119 poleceniem rozpocznij_zajecia() albo wska\u017c katalog.", call. = FALSE)
  root
}

#' Ścieżka do roboczego Rmd, jego skryptu lub danych
#'
#' W konsoli używa przestrzeni zapamiętanej przez rozpocznij_zajecia(),
#' bez zmiany katalogu roboczego. Podczas renderowania używa katalogu
#' renderowanego Rmd, także gdy odtworzono sam folder dokumentu.
#' Funkcja tylko wskazuje plik: nie tworzy, nie pobiera i nie wykonuje go.
#' @param id Z01--Z10 albo PROJEKT.
#' @param plik Ścieżka wewnątrz folderu dokumentu; NULL wskazuje jego Rmd.
#' @param katalog Własna przestrzeń pracy; NULL rozpoznaje ją automatycznie.
#' @return Bezwzględna ścieżka do istniejącego pliku lub katalogu.
#' @export
#' @examples
#' k <- tempfile("praca-")
#' utworz_projekt("s017", katalog = k)
#' przygotuj_zadanie("Z01", k)
#' plik_pracy("Z01", "analiza.R", katalog = k)
#' unlink(k, recursive = TRUE)
plik_pracy <- function(id, plik = NULL, katalog = NULL) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  render <- isTRUE(getOption("knitr.in.progress")) && is.null(katalog)
  if (render) {
    wejscie <- knitr::current_input(dir = TRUE)
    if (is.null(wejscie) || !nzchar(wejscie) || !file.exists(wejscie))
      stop("Nie mo\u017cna ustali\u0107 po\u0142o\u017cenia renderowanego Rmd.", call. = FALSE)
    wejscie <- normalizePath(wejscie, winslash = "/", mustWork = TRUE)
    folder <- dirname(wejscie)
    if (is.null(plik)) return(wejscie)
  } else {
    root <- katalog_kursu(katalog)
    folder <- sciezka_pracy(root, if (id == "PROJEKT") "projekty/ilosciowy" else
      file.path("zadania", tolower(id)))
    cfg <- czytaj_yaml(file.path(root, "kurs.yml"))
    if (id == "PROJEKT") sprawdz_zapisany_raport(folder, cfg) else
      sprawdz_zapisana_prace(folder, id, cfg)
    if (is.null(plik)) plik <- if (id == "PROJEKT") "raport.Rmd" else "zadanie.Rmd"
  }
  cel <- sciezka_pracy(folder, plik)
  if (!file.exists(cel)) stop("Brakuje pliku pracy: ", plik,
    ". Odtw\u00f3rz komplet plik\u00f3w zadania; nie zmieniaj katalogu poleceniem setwd().", call. = FALSE)
  cel
}

otworz_plik_pracy <- function(plik) {
  dostepne <- requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")
  if (dostepne) {
    otwarte <- tryCatch({ rstudioapi::navigateToFile(plik); TRUE }, error = function(e) FALSE)
    if (otwarte) return(invisible(TRUE))
  }
  message("Otw\u00f3rz w bie\u017c\u0105cym edytorze plik: ", plik,
    "\nNie otwieraj teraz Rproj: zmiana sesji R wymaga ponownego logowania.")
  invisible(FALSE)
}
