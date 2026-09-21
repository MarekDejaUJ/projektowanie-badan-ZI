#' Utworzenie indywidualnej przestrzeni pracy
#'
#' Przestrzeń przechowuje ćwiczenia, ale nie rozpoczyna jeszcze projektu
#' badawczego. Własny wariant danych wybiera się dopiero na C09.
#' @param id_studenta Pseudonim studenta.
#' @param scenariusz Opcjonalna propozycja S01--S20 na C09. Nie generuje danych.
#' @param katalog Nowy katalog projektu.
#' @param rocznik Rocznik kursu.
#' @param repo Oczekiwane prywatne repozytorium jako owner/name, jeśli znane.
#' @return Ścieżka do projektu RStudio, niewidocznie.
#' @export
#' @examples
#' katalog <- tempfile("projekt-")
#' utworz_projekt("s017", katalog = katalog)
#' unlink(katalog, recursive = TRUE)
utworz_projekt <- function(id_studenta, scenariusz = NULL, katalog = "moje-badania",
                          rocznik = "2026-27", repo = NULL) {
  if (dir.exists(katalog) || file.exists(katalog)) stop("Projekt ju\u017c istnieje; nie nadpisuj\u0119 pracy.", call. = FALSE)
  if (!is.null(repo)) sprawdz_id(repo, "repo", "^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+$")
  sprawdz_id(id_studenta, "id_studenta")
  konfiguracja_kursu(rocznik)
  if (!is.null(scenariusz)) get("scenariusz", mode = "function")(scenariusz)
  dir.create(dirname(katalog), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("projekt-", tmpdir = dirname(katalog))
  if (!dir.create(tmp)) stop("Nie mo\u017cna utworzy\u0107 projektu.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cfg <- list(format_pracy = "rmd-1", id = id_studenta, rocznik = rocznik,
              proponowany_scenariusz = scenariusz, repo = repo, branch = "main")
  pisz_linie(yaml::as.yaml(cfg), file.path(tmp, "kurs.yml"))
  rproj <- c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No",
             "AlwaysSaveHistory: No", "Encoding: UTF-8", "UseSpacesForTab: Yes", "NumSpacesForTab: 2")
  pisz_linie(rproj, file.path(tmp, "moje-badania.Rproj"))
  pisz_linie(c(".Rhistory", ".RData", ".Ruserdata", "/.[!.]*/", "!/.github/"), file.path(tmp, ".gitignore"))
  # Git zachowuje dokładne bajty wejść, także po zapisaniu Rmd przez edytor
  # używający CRLF. Hashe PDF odnoszą się do rzeczywiście renderowanych plików.
  pisz_linie(c("* -text", "*.rds binary", "*.png binary", "*.pdf binary"),
             file.path(tmp, ".gitattributes"))
  if (!file.rename(tmp, katalog)) stop("Nie mo\u017cna zapisa\u0107 projektu w wybranym katalogu.", call. = FALSE)
  invisible(normalizePath(file.path(katalog, "moje-badania.Rproj"), winslash = "/"))
}

# Zapis binarny zachowuje UTF-8 i LF również na stanowiskach Windows.
pisz_linie <- function(tekst, plik) {
  polaczenie <- file(plik, open = "wb")
  on.exit(close(polaczenie), add = TRUE)
  writeLines(enc2utf8(tekst), polaczenie, useBytes = TRUE)
  invisible(plik)
}
