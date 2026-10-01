#' Utworzenie indywidualnej przestrzeni pracy
#'
#' Przestrzeń przechowuje ćwiczenia, ale nie rozpoczyna jeszcze projektu
#' badawczego. Własny wariant danych wybiera się dopiero na C09. Zwykle
#' tworzy ją rozpocznij_zajecia() przy pierwszym starcie.
#' @param id_studenta Pseudonim studenta.
#' @param scenariusz Opcjonalna propozycja S01--S20 na C09. Nie generuje danych.
#' @param katalog Nowy katalog projektu.
#' @param rocznik Rocznik kursu.
#' @param student Imię i nazwisko do nazw plików oddania, np. "Anna Kowalska".
#'   Można je dopisać później w rozpocznij_zajecia().
#' @return Ścieżka do projektu RStudio, niewidocznie.
#' @export
#' @examples
#' katalog <- tempfile("projekt-")
#' utworz_projekt("s017", katalog = katalog, student = "Anna Kowalska")
#' unlink(katalog, recursive = TRUE)
utworz_projekt <- function(id_studenta, scenariusz = NULL, katalog = "moje-badania",
                          rocznik = "2026-27", student = NULL) {
  if (dir.exists(katalog) || file.exists(katalog)) stop("Projekt ju\u017c istnieje; nie nadpisuj\u0119 pracy.", call. = FALSE)
  sprawdz_id(id_studenta, "id_studenta")
  if (!is.null(student)) student <- sprawdz_studenta(student)
  konfiguracja_kursu(rocznik)
  if (!is.null(scenariusz)) get("scenariusz", mode = "function")(scenariusz)
  dir.create(dirname(katalog), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("projekt-", tmpdir = dirname(katalog))
  if (!dir.create(tmp)) stop("Nie mo\u017cna utworzy\u0107 projektu.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cfg <- list(format_pracy = "rmd-1", id = id_studenta, student = student, rocznik = rocznik,
              proponowany_scenariusz = scenariusz)
  pisz_linie(yaml::as.yaml(cfg), file.path(tmp, "kurs.yml"))
  rproj <- c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No",
             "AlwaysSaveHistory: No", "Encoding: UTF-8", "UseSpacesForTab: Yes", "NumSpacesForTab: 2")
  pisz_linie(rproj, file.path(tmp, "moje-badania.Rproj"))
  if (!file.rename(tmp, katalog)) stop("Nie mo\u017cna zapisa\u0107 projektu w wybranym katalogu.", call. = FALSE)
  invisible(normalizePath(file.path(katalog, "moje-badania.Rproj"), winslash = "/"))
}

# "Anna Maria  Kowalska-Nowak" -> "Anna Maria Kowalska-Nowak"; nazwisko to ostatni wyraz.
sprawdz_studenta <- function(student) {
  ok <- is.character(student) && length(student) == 1L && !is.na(student)
  if (ok) {
    student <- trimws(gsub("[[:space:]]+", " ", enc2utf8(student)))
    ok <- length(strsplit(student, " ", fixed = TRUE)[[1L]]) >= 2L && nchar(student) <= 80L &&
      grepl("^[[:alpha:]][[:alpha:]' -]*[[:alpha:]]$", student) &&
      !grepl("imi\u0119|imie|nazwisk", tolower(student))
  }
  if (!ok) stop("Podaj swoje imi\u0119 i nazwisko, np. student = \"Anna Kowalska\".", call. = FALSE)
  student
}

# Polskie litery zamieniane jawnie: iconv różni się między Windows, macOS i Linux.
do_nazwy_pliku <- function(x) {
  x <- chartr("\u0105\u0107\u0119\u0142\u0144\u00f3\u015b\u017a\u017c\u0104\u0106\u0118\u0141\u0143\u00d3\u015a\u0179\u017b",
              "acelnoszzACELNOSZZ", x)
  x <- iconv(gsub("'", "", x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- gsub("['`^~\"]", "", x)
  x <- gsub("[^a-z0-9-]+", "_", tolower(x))
  gsub("^_+|_+$", "", x)
}

# "s017", "Anna Maria Kowalska-Nowak", "Z03" -> "s017_kowalska-nowak_anna_maria_Z03"
nazwa_oddania <- function(id_studenta, student, zadanie) {
  slowa <- strsplit(sprawdz_studenta(student), " ", fixed = TRUE)[[1L]]
  osoba <- paste(do_nazwy_pliku(slowa[length(slowa)]), do_nazwy_pliku(paste(slowa[-length(slowa)], collapse = " ")), sep = "_")
  if (!grepl("^[a-z0-9-]+_[a-z0-9_-]+$", osoba)) stop("Imi\u0119 i nazwisko musz\u0105 zawiera\u0107 litery alfabetu \u0142aci\u0144skiego.", call. = FALSE)
  paste(id_studenta, osoba, zadanie, sep = "_")
}

# Zapis binarny zachowuje UTF-8 i LF również na stanowiskach Windows.
pisz_linie <- function(tekst, plik) {
  polaczenie <- file(plik, open = "wb")
  on.exit(close(polaczenie), add = TRUE)
  writeLines(enc2utf8(tekst), polaczenie, useBytes = TRUE)
  invisible(plik)
}
