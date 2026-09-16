#' Pobranie własnej przestrzeni pracy
#' @param repo_url HTTPS URL prywatnego repozytorium GitHub.
#' @param katalog Nowy katalog lokalny.
#' @param id_studenta Oczekiwany pseudonim.
#' @return Ścieżka projektu RStudio, niewidocznie.
#' @expor
#' @examples
#' \dontrun{
#' pobierz_zadanie("https://github.com/prowadzacy/badania-s017.git", "moje-badania", "s017")
#' }
pobierz_zadanie <- function(repo_url, katalog = "moje-badania", id_studenta) {
  repo <- nazwa_repo(repo_url)
  token_sesji()
  meta <- github_api(paste0("repos/", repo))$dane
  if (!identical(meta$private, TRUE) || !identical(meta$permissions$push, TRUE))
    stop("Potrzebne jest w\u0142asne prywatne repozytorium z prawem zapisu.", call. = FALSE)
  if (file.exists(katalog) || dir.exists(katalog)) stop("Katalog ju\u017c istnieje; zachowaj prac\u0119 i wybierz nowy katalog.", call. = FALSE)
  sprawdz_id(id_studenta, "id_studenta")
  dir.create(dirname(katalog), recursive = TRUE, showWarnings = FALSE)
  tmp <- katalog
  gotowe <- FALSE
  on.exit(if (!gotowe) { gc(); unlink(tmp, recursive = TRUE) }, add = TRUE)
  operacja_git(gert::git_clone(paste0("https://", sesja_github$login, "@github.com/", repo, ".git"),
                   path = tmp, branch = "main", password = token_sesji(), verbose = FALSE))
  cfg <- czytaj_yaml(file.path(tmp, "kurs.yml"))
  if (!identical(cfg$id, id_studenta) || !identical(tolower(cfg$repo), tolower(repo)))
    stop("Repozytorium nie odpowiada Twojemu ID lub konfiguracji. Sprawd\u017a adres podany na zaj\u0119ciach.", call. = FALSE)
  gert::git_config_set("user.name", sesja_github$login, repo = tmp)
  gert::git_config_set("user.email", paste0(sesja_github$id, "+", sesja_github$login, "@users.noreply.github.com"), repo = tmp)
  gc()
  gotowe <- TRUE
  invisible(normalizePath(file.path(katalog, "moje-badania.Rproj"), winslash = "/"))
}

#' Początek zajęć na czyszczonym komputerze
#' @param cwiczenie C01--C10.
#' @param id_studenta Pseudonim studenta.
#' @param repo_url URL własnego repo; dla istniejącego katalogu odczytywany z konfiguracji.
#' @param katalog Katalog lokalny.
#' @param otworz Czy otworzyć projekt i materiał w IDE/przeglądarce.
#' @return Lista: projekt i identyfikator ćwiczenia, niewidocznie.
#' @expor
#' @examples
#' \dontrun{
#' rozpocznij_zajecia("C02", "s017", "https://github.com/prowadzacy/badania-s017.git")
#' }
rozpocznij_zajecia <- function(cwiczenie, id_studenta, repo_url = NULL,
                              katalog = "moje-badania", otworz = TRUE) {
  sprawdz_id(toupper(cwiczenie), "cwiczenie", "^C(0[1-9]|10)$")
  if (!dir.exists(katalog)) {
    if (is.null(repo_url)) stop("Podaj adres prywatnego repozytorium otrzymany na zaj\u0119ciach.", call. = FALSE)
    p <- pobierz_zadanie(repo_url, katalog, id_studenta)
  } else {
    cfg <- sprawdz_repo(katalog, id_studenta)
    if (!is.null(repo_url) && !identical(tolower(nazwa_repo(repo_url)), tolower(cfg$repo)))
      stop("Adres nie odpowiada istniej\u0105cej pracy.", call. = FALSE)
    if (nrow(gert::git_status(repo = katalog))) stop("Masz lokalne zmiany. Najpierw zapisz i oddaj prac\u0119 albo zachowaj kopi\u0119; aktualizacja jej nie nadpisze.", call. = FALSE)
    operacja_git(gert::git_fetch("origin", password = token_sesji(), repo = katalog, verbose = FALSE))
    aktualizuj_repo_lokalne(katalog)
    p <- normalizePath(file.path(katalog, "moje-badania.Rproj"), winslash = "/")
  }
  przygotuj_zadanie(sub("^C", "Z", toupper(cwiczenie)), katalog)
  if (otworz) {
    otworz_material(toupper(cwiczenie))
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) rstudioapi::openProject(p)
    else utils::browseURL(p)
  }
  invisible(list(projekt = p, cwiczenie = toupper(cwiczenie)))
}

nazwa_repo <- function(url) {
  sprawdz_id(url, "repo_url", "^https://github[.]com/[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+([.]git)?/?$")
  repo <- sub("^https://github[.]com/", "", url)
  sub("([.]git)?/$|[.]git$", "", repo)
}

operacja_git <- function(kod) {
  tryCatch(force(kod), error = function(e) stop(
    "Operacja Git nie powiod\u0142a si\u0119. Zachowano lokalne pliki; sprawd\u017a sie\u0107, uprawnienia i konflikt z wersj\u0105 zdaln\u0105. Nie u\u017cywaj force-push.",
    call. = FALSE))
}

sprawdz_repo <- function(katalog, id = NULL) {
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  if (is.null(cfg$repo) || is.null(cfg$id)) stop("Projekt nie ma przypisanego repozytorium i ID.", call. = FALSE)
  if (!is.null(id) && !identical(cfg$id, id)) stop("To projekt innego ID.", call. = FALSE)
  info <- operacja_git(gert::git_info(repo = I(katalog)))
  if (!identical(info$shorthand, "main")) stop("Prze\u0142\u0105cz si\u0119 na g\u0142\u00f3wn\u0105 ga\u0142\u0105\u017a main; nie zmieniam ga\u0142\u0119zi automatycznie.", call. = FALSE)
  sprawdz_origin(katalog, cfg$repo)
  meta <- github_api(paste0("repos/", cfg$repo))$dane
  if (!identical(meta$private, TRUE) || !identical(meta$permissions$push, TRUE))
    stop("Repozytorium musi by\u0107 prywatne i dost\u0119pne do zapisu.", call. = FALSE)
  cfg
}

sprawdz_origin <- function(katalog, repo) {
  origin <- gert::git_remote_info("origin", repo = I(katalog))
  adresy <- c(origin$url, origin$push_url)
  adresy <- sub("^https://[A-Za-z0-9-]+@github[.]com/", "https://github.com/", adresy)
  if (!length(adresy) || any(!vapply(adresy, function(url)
      identical(tolower(nazwa_repo(url)), tolower(repo)), logical(1))))
    stop("Adres pobierania lub wysy\u0142ki Git nie odpowiada przypisanemu repozytorium.", call. = FALSE)
  invisible(TRUE)
}

aktualizuj_repo_lokalne <- function(katalog, ref = "origin/main") {
  stan <- operacja_git(gert::git_merge_analysis(ref, repo = I(katalog)))
  roznice <- operacja_git(gert::git_ahead_behind(upstream = ref, repo = I(katalog)))
  if (roznice$ahead > 0L || !stan %in% c("up_to_date", "fastforward"))
    stop("Masz lokalne commity lub rozbie\u017cne wersje. Zachowano prac\u0119; najpierw oddaj lokalne zmiany albo wyja\u015bnij konflikt. Automatyczna aktualizacja nie utworzy merge commitu.", call. = FALSE)
  if (identical(stan, "fastforward")) operacja_git(gert::git_merge(ref, repo = I(katalog)))
  invisible(gert::git_info(repo = I(katalog))$commit)
}
