pliki_wariantu <- function() {
  c("surowe.csv", "surowe.rds", "slownik.csv", "scenariusz.yml", "manifest.json")
}

odczytaj_wariant_zadania <- function(folder, cfg, scenariusz = NULL) {
  znacznik <- sciezka_pracy(folder, "wariant.yml")
  if (!file.exists(znacznik))
    stop("Brak potwierdzonego zapisu wariantu z C09. Uruchom pierwszy chunk CHALLENGE na C09; je\u015bli zapis istnia\u0142, odtw\u00f3rz komplet w\u0142asnych plik\u00f3w.", call. = FALSE)
  zapis <- czytaj_yaml(znacznik)
  x <- wczytaj_dane(sciezka_pracy(folder, "dane"), id = cfg$id,
                    scenariusz = scenariusz, rocznik = cfg$rocznik)
  pola <- c("id", "scenariusz", "rocznik", "wersja_generatora", "klucz_wariantu",
            "hash_danych", "sha256_csv", "sha256_rds", "sha256_slownik", "sha256_scenariusz")
  if (!all(vapply(pola, function(p) identical(zapis[[p]], x$manifest[[p]]), logical(1))))
    stop("Dane nie odpowiadaj\u0105 zapisanemu wyborowi projektu. Przywr\u00f3\u0107 komplet w\u0142asnego wariantu; nie zmieniaj manifestu.", call. = FALSE)
  x
}

kopiuj_wariant_zadania <- function(zrodlo, cel, cfg) {
  x <- odczytaj_wariant_zadania(zrodlo, cfg)
  dir.create(file.path(cel, "dane"))
  for (p in pliki_wariantu()) {
    plik <- sciezka_pracy(zrodlo, file.path("dane", p))
    if (!file.copy(plik, file.path(cel, "dane", p)))
      stop("Nie mo\u017cna skopiowa\u0107 zachowanego wariantu. Oryginalne dane pozostaj\u0105 bez zmian.", call. = FALSE)
  }
  if (!file.copy(file.path(zrodlo, "wariant.yml"), file.path(cel, "wariant.yml")))
    stop("Nie mo\u017cna zachowa\u0107 metadanych wariantu.", call. = FALSE)
  odczytaj_wariant_zadania(cel, cfg)
  invisible(x)
}

#' Zachowanie wariantu na C09 i odczyt tego samego wariantu na C10
#'
#' Pierwsze wywołanie dla Z09 zapisuje dane w jego katalogu. Następne
#' wywołania oraz Z10 wyłącznie czytają zachowany zapis. Render nie tworzy
#' wariantu: najpierw uruchom pierwszy chunk CHALLENGE na C09 i zapisz Rmd.
#' Inny scenariusz, uszkodzony zapis lub obce ID powodują błąd bez nadpisania.
#' @param zadanie Z09 albo Z10.
#' @param scenariusz Wybrany S01--S20 na C09; na C10 pozostaw NULL.
#' @param id_studenta ID z początku sesji; NULL odczytuje ID z kurs.yml.
#' @param katalog Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.
#' @return Obiekt badaniaZI_dane; obliczenia i prezentację wykonuje analiza.R.
#' @export
#' @examples
#' k <- tempfile("projekt-")
#' utworz_projekt("s017", katalog = k)
#' przygotuj_zadanie("Z09", k)
#' x <- wariant_projektu("Z09", "S02", katalog = k)
#' przygotuj_zadanie("Z10", k)
#' y <- wariant_projektu("Z10", katalog = k)
#' stopifnot(identical(x$dane, y$dane))
#' unlink(k, recursive = TRUE)
wariant_projektu <- function(zadanie, scenariusz = NULL, id_studenta = NULL, katalog = NULL) {
  zadanie <- toupper(zadanie)
  sprawdz_id(zadanie, "zadanie projektu", "^Z(09|10)$")
  render <- isTRUE(getOption("knitr.in.progress"))
  if (render && is.null(katalog)) {
    # Własny zapis obok Rmd pozwala odtworzyć także sam katalog zadania.
    folder <- dirname(normalizePath(knitr::current_input(dir = TRUE), winslash = "/", mustWork = TRUE))
    meta <- czytaj_yaml(sciezka_pracy(folder, "zadanie.yml"))
    cfg <- list(id = meta$id_studenta, rocznik = meta$rocznik)
  } else {
    root <- katalog_kursu(katalog)
    cfg <- czytaj_yaml(file.path(root, "kurs.yml"))
    folder <- sciezka_pracy(root, file.path("zadania", tolower(zadanie)))
  }
  sprawdz_id(cfg$id, "ID pracy")
  if (!is.null(id_studenta) && !identical(id_studenta, cfg$id))
    stop("ID sesji lub Rmd r\u00f3\u017cni si\u0119 od ID tej pracy. Otw\u00f3rz w\u0142asne zadanie.", call. = FALSE)
  sprawdz_zapisana_prace(folder, zadanie, cfg)
  if (!is.null(scenariusz)) sprawdz_id(scenariusz, "scenariusz", "^S(0[1-9]|1[0-9]|20)$")
  znacznik <- sciezka_pracy(folder, "wariant.yml")
  dane <- sciezka_pracy(folder, "dane")
  if (file.exists(znacznik)) return(odczytaj_wariant_zadania(folder, cfg, scenariusz))
  if (file.exists(dane))
    stop("Zapis danych jest niekompletny: brakuje potwierdzenia wariantu. Zachowano dane; popro\u015b o pomoc w odtworzeniu zapisu.", call. = FALSE)
  if (render || zadanie != "Z09")
    stop("Brak zapisanego wariantu C09. Uruchom pierwszy chunk CHALLENGE na C09 przed tworzeniem PDF; C10 nie losuje nowych danych.", call. = FALSE)
  if (is.null(scenariusz)) stop("Na C09 wybierz kod scenariusza, np. S02.", call. = FALSE)
  zapisz_dane(generuj_dane(cfg$id, scenariusz, rocznik = cfg$rocznik), dane)
  x <- wczytaj_dane(dane, id = cfg$id, scenariusz = scenariusz, rocznik = cfg$rocznik)
  # Oddzielne potwierdzenie chroni przed cichym losowaniem po utracie folderu danych.
  pisz_linie(yaml::as.yaml(x$manifest), znacznik)
  odczytaj_wariant_zadania(folder, cfg, scenariusz)
}
