#' Przygotowanie prywatnego repozytorium studenta
#'
#' Funkcja jest przeznaczona dla prowadzacego. Tworzy zwykle prywatne
#' repozytorium na jego koncie, umieszcza w nim konfiguracje przestrzeni cwiczen i wysyla
#' zaproszenie do wskazanego konta studenta. Nie wymaga organizacji GitHub.
#' Nie generuje danych ani raportu: projekt badawczy rozpoczyna sie na C09.
#'
#' @param id_studenta Pseudonim uzywany w danych i nazwie repozytorium.
#' @param login_github Login konta GitHub studenta.
#' @param scenariusz Opcjonalna propozycja S01--S20 na C09; bez generowania danych.
#' @param nazwa Opcjonalna nazwa repozytorium; domyslnie `ZI-ID`.
#' @param rocznik Rocznik konfiguracji kursu.
#' @return Niewidocznie lista z adresem repozytorium i stanem zaproszenia.
#' @export
#' @examples
#' \dontrun{
#' zaloguj_github()
#' przygotuj_repo_studenta("s017", "login-studenta")
#' }
przygotuj_repo_studenta <- function(id_studenta, login_github, scenariusz = NULL,
                                    nazwa = NULL, rocznik = "2026-27") {
  sprawdz_id(id_studenta, "id_studenta")
  sprawdz_id(login_github, "login_github", "^[A-Za-z0-9][A-Za-z0-9-]{0,38}$")
  if (!is.null(scenariusz)) get("scenariusz", mode = "function")(scenariusz)
  cfg <- konfiguracja_kursu(rocznik)
  wlasciciel <- cfg$wlasciciel_repozytoriow
  sprawdz_id(wlasciciel, "wlasciciel_repozytoriow", "^[A-Za-z0-9][A-Za-z0-9-]{0,38}$")
  if (!identical(tolower(sesja_github$login), tolower(wlasciciel)))
    stop("Repozytoria kursu przygotowuje konto w\u0142a\u015bciciela wskazane w konfiguracji.", call. = FALSE)
  if (is.null(nazwa)) nazwa <- paste0("ZI-", tolower(id_studenta))
  sprawdz_id(nazwa, "nazwa", "^[A-Za-z0-9][A-Za-z0-9_.-]{1,99}$")
  repo <- paste0(wlasciciel, "/", nazwa)
  if (!is.null(github_api(paste0("repos/", repo), brak_ok = TRUE)$dane))
    stop("Repozytorium ju\u017c istnieje; nie nadpisuj\u0119 jego zawarto\u015bci.", call. = FALSE)

  lokalny <- tempfile(paste0("repo-", id_studenta, "-"))
  on.exit(unlink(lokalny, recursive = TRUE), add = TRUE)
  utworz_projekt(id_studenta, scenariusz, lokalny, rocznik = rocznik, repo = repo)
  gert::git_init(lokalny)
  gert::git_config_set("user.name", sesja_github$login, repo = lokalny)
  gert::git_config_set("user.email",
                       paste0(sesja_github$id, "+", sesja_github$login,
                              "@users.noreply.github.com"), repo = lokalny)
  pliki <- list.files(lokalny, recursive = TRUE, all.files = TRUE,
                      include.dirs = FALSE, no.. = TRUE)
  pliki <- pliki[!grepl("(^|/)\\.git(/|$)", pliki)]
  gert::git_add(pliki, repo = lokalny)
  podpis <- gert::git_signature(sesja_github$login,
    paste0(sesja_github$id, "+", sesja_github$login, "@users.noreply.github.com"))
  gert::git_commit("Przygotuj przestrze\u0144 \u0107wicze\u0144", author = podpis,
                   committer = podpis, repo = lokalny)
  if (!identical(gert::git_info(repo = lokalny)$shorthand, "main"))
    gert::git_branch_create("main", checkout = TRUE, repo = lokalny)

  github_api("user/repos", "POST", list(
    name = nazwa, private = TRUE,
    description = paste("Indywidualna praca", id_studenta, "\u2014 badaniaZI"),
    has_issues = TRUE, has_projects = FALSE, has_wiki = FALSE,
    auto_init = FALSE))
  adres <- paste0("https://github.com/", repo, ".git")
  gert::git_remote_add(paste0("https://", sesja_github$login, "@github.com/",
                             repo, ".git"), repo = lokalny)
  operacja_git(gert::git_push("origin", refspec = "refs/heads/main:refs/heads/main",
                              password = token_sesji(), force = FALSE,
                              verbose = FALSE, repo = lokalny))
  ten_sam_login <- identical(tolower(login_github), tolower(wlasciciel))
  zaproszenie <- if (ten_sam_login) {
    list(status = 204L)
  } else {
    github_api(paste0("repos/", repo, "/collaborators/", login_github),
               "PUT", list(permission = "push"))
  }
  invisible(list(repo = repo, url = sub("[.]git$", "", adres),
                 login_studenta = login_github,
                 zaproszenie = if (zaproszenie$status == 201L) "wyslane" else "aktywne"))
}
