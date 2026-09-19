#' Utworzenie indywidualnej przestrzeni pracy
#' @param id_studenta Pseudonim studenta.
#' @param scenariusz S01--S20.
#' @param katalog Nowy katalog projektu.
#' @param rocznik Rocznik kursu.
#' @param repo Oczekiwane prywatne repozytorium jako owner/name, jeśli znane.
#' @return Ścieżka do projektu RStudio, niewidocznie.
#' @export
#' @examples
#' katalog <- tempfile("projekt-")
#' utworz_projekt("s017", "S03", katalog)
#' unlink(katalog, recursive = TRUE)
utworz_projekt <- function(id_studenta, scenariusz = "S01", katalog = "moje-badania",
                          rocznik = "2026-27", repo = NULL) {
  if (dir.exists(katalog) || file.exists(katalog)) stop("Projekt ju\u017c istnieje; nie nadpisuj\u0119 pracy.", call. = FALSE)
  if (!is.null(repo)) sprawdz_id(repo, "repo", "^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+$")
  x <- generuj_dane(id_studenta, scenariusz, rocznik)
  dir.create(dirname(katalog), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("projekt-", tmpdir = dirname(katalog))
  if (!dir.create(tmp)) stop("Nie mo\u017cna utworzy\u0107 projektu.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "projekty", "ilosciowy"), recursive = TRUE)
  zapisz_dane(x, file.path(tmp, "dane"))
  cfg <- list(id = id_studenta, scenariusz = scenariusz, rocznik = rocznik,
              wersja_generatora = x$manifest$wersja_generatora, repo = repo, branch = "main")
  yaml::write_yaml(cfg, file.path(tmp, "kurs.yml"))
  wartosci <- c(ID = id_studenta, SCENARIUSZ = scenariusz, ROCZNIK = rocznik,
               GENERATOR = x$manifest$wersja_generatora)
  for (plik in c("raport.md", "kwestionariusz.md", "analiza.R")) {
    tekst <- readLines(zasob("szablony", "projekt", plik), encoding = "UTF-8", warn = FALSE)
    for (pole in names(wartosci)) tekst <- gsub(paste0("{{", pole, "}}"), wartosci[[pole]], tekst, fixed = TRUE)
    writeLines(enc2utf8(tekst), file.path(tmp, "projekty", "ilosciowy", plik), useBytes = TRUE)
  }
  file.copy(zasob("scenariusze", paste0(scenariusz, ".md")), file.path(tmp, "SCENARIUSZ.md"))
  file.copy(zasob("rubryki", "PROJEKT.md"), file.path(tmp, "RUBRYKA.md"))
  rproj <- c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No",
             "AlwaysSaveHistory: No", "Encoding: UTF-8", "UseSpacesForTab: Yes", "NumSpacesForTab: 2")
  writeLines(rproj, file.path(tmp, "moje-badania.Rproj"))
  writeLines(c(".Rhistory", ".RData", ".Ruserdata", "/.[!.]*/", "!/.github/"), file.path(tmp, ".gitignore"))
  writeLines(c("* text=auto eol=lf", "*.csv -text", "*.rds binary", "*.png binary", "*.pdf binary"),
             file.path(tmp, ".gitattributes"))
  workflow <- zasob("szablony", "projekt", "workflow", "sprawdz.yml")
  if (nzchar(workflow)) {
    dir.create(file.path(tmp, ".github", "workflows"), recursive = TRUE)
    file.copy(workflow, file.path(tmp, ".github", "workflows", "sprawdz.yml"))
  }
  if (!file.rename(tmp, katalog)) stop("Nie mo\u017cna zapisa\u0107 projektu w wybranym katalogu.", call. = FALSE)
  invisible(normalizePath(file.path(katalog, "moje-badania.Rproj"), winslash = "/"))
}
