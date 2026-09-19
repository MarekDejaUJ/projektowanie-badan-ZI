sesja_github <- new.env(parent = emptyenv())

#' Logowanie do GitHub z konsoli R
#'
#' Hasło i 2FA podaje się wyłącznie na GitHub. Wariant device wymaga publicznego
#' client ID aplikacji kursu. Wariant gh wymaga GitHub CLI i używa osobnego,
#' tymczasowego katalogu bez systemowego magazynu poświadczeń. Po logowaniu
#' usuwa ten katalog; token pozostaje w pamięci bieżącej sesji R.
#' @param metoda "auto", "device" albo "gh".
#' @param client_id Publiczny identyfikator aplikacji z włączonym device flow.
#' @param timeout Maksymalna liczba sekund na potwierdzenie.
#' @param przegladarka Czy otworzyć stronę potwierdzenia.
#' @return Login konta GitHub, niewidocznie. Token nie jest zwracany.
#' @export
#' @examples
#' \dontrun{
#' zaloguj_github()
#' wyloguj_github()
#' }
zaloguj_github <- function(metoda = c("auto", "device", "gh"), client_id = NULL,
                          timeout = 600, przegladarka = TRUE) {
  metoda <- match.arg(metoda)
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout < 30 || timeout > 1800)
    stop("Timeout musi mie\u015bci\u0107 si\u0119 w 30--1800 sekundach.", call. = FALSE)
  if (is.null(client_id)) client_id <- konfiguracja_kursu()$oauth_client_id
  if (metoda == "auto") metoda <- if (is.null(client_id)) "gh" else "device"
  wyloguj_github(komunikat = FALSE)
  token <- if (metoda == "device") token_device(client_id, timeout, przegladarka) else token_gh(timeout)
  ustaw_sesje(token)
  message("Zalogowano do GitHub jako ", sesja_github$login, ".")
  invisible(sesja_github$login)
}

#' Zakończenie sesji GitHub pakietu
#'
#' Usuwa token z sesji pakietu. Wyloguj też przeglądarkę na komputerze w sali
#' i zakończ sesję R bez zapisywania przestrzeni roboczej.
#' @param komunikat Czy wyświetlić potwierdzenie.
#' @return TRUE, niewidocznie.
#' @export
#' @examples
#' wyloguj_github()
wyloguj_github <- function(komunikat = TRUE) {
  rm(list = ls(sesja_github, all.names = TRUE), envir = sesja_github)
  if (komunikat) message("Sesja GitHub pakietu zosta\u0142a zako\u0144czona.")
  invisible(TRUE)
}

token_sesji <- function() {
  if (is.null(sesja_github$token)) stop("Najpierw uruchom zaloguj_github().", call. = FALSE)
  sesja_github$token
}

ustaw_sesje <- function(token) {
  if (!is.character(token) || length(token) != 1L || !nzchar(token)) stop("Brak tokena autoryzacji.", call. = FALSE)
  osoba <- github_api("user", token = token)$dane
  if (is.null(osoba$login) || is.null(osoba$id)) stop("GitHub nie potwierdzi\u0142 to\u017csamo\u015bci.", call. = FALSE)
  sesja_github$token <- token
  sesja_github$login <- osoba$login
  sesja_github$id <- osoba$id
  invisible(osoba$login)
}

github_api <- function(sciezka, metoda = "GET", body = NULL, token = token_sesji(), brak_ok = FALSE) {
  if (!is.character(sciezka) || length(sciezka) != 1L || grepl("(^/|://|\\.\\.)", sciezka))
    stop("Niepoprawna \u015bcie\u017cka API GitHub.", call. = FALSE)
  req <- httr2::request(paste0("https://api.github.com/", sciezka))
  req <- httr2::req_auth_bearer_token(req, token)
  req <- httr2::req_headers(req, Accept = "application/vnd.github+json", `X-GitHub-Api-Version` = "2022-11-28")
  req <- httr2::req_method(httr2::req_timeout(req, 30), metoda)
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e)
    stop("Brak po\u0142\u0105czenia z GitHub. Zachowaj lokalne pliki i pon\u00f3w po odzyskaniu sieci.", call. = FALSE))
  status <- httr2::resp_status(resp)
  if (brak_ok && status == 404L) return(list(status = status, dane = NULL))
  if (status < 200L || status >= 300L) {
    opis <- if (status == 401L) "Sesja wygas\u0142a; zaloguj si\u0119 ponownie." else
      if (status %in% c(403L, 404L)) "Sprawd\u017a dost\u0119p do prywatnego repozytorium, akceptacj\u0119 zaproszenia i autoryzacj\u0119 organizacji." else
        if (status == 409L) "Konflikt wersji zdalnej; pobierz aktualny stan bez nadpisywania pracy." else
          "Operacja GitHub nie powiod\u0142a si\u0119; zachowaj lokalne pliki."
    stop("GitHub HTTP ", status, ": ", opis, call. = FALSE)
  }
  dane <- if (status == 204L) NULL else httr2::resp_body_json(resp, simplifyVector = FALSE)
  list(status = status, dane = dane, czas_serwera = httr2::resp_header(resp, "date"))
}

oauth_post <- function(adres, ...) {
  req <- httr2::request(paste0("https://github.com/", adres))
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_body_form(req, ...)
  resp <- tryCatch(httr2::req_perform(httr2::req_timeout(req, 30)), error = function(e)
    stop("Nie mo\u017cna po\u0142\u0105czy\u0107 si\u0119 z logowaniem GitHub; spr\u00f3buj ponownie.", call. = FALSE))
  httr2::resp_body_json(resp)
}

token_device <- function(client_id, timeout, przegladarka) {
  sprawdz_id(client_id, "client_id", "^[A-Za-z0-9_.-]{5,100}$")
  x <- oauth_post("login/device/code", client_id = client_id, scope = "repo read:org")
  if (!all(c("device_code", "user_code", "verification_uri", "expires_in", "interval") %in% names(x)))
    stop("Aplikacja nie udost\u0119pnia device flow; sprawd\u017a konfiguracj\u0119 aplikacji kursu.", call. = FALSE)
  if (!identical(x$verification_uri, "https://github.com/login/device"))
    stop("GitHub zwr\u00f3ci\u0142 nieoczekiwany adres logowania.", call. = FALSE)
  message("Otw\u00f3rz ", x$verification_uri, " i wpisz kod: ", x$user_code)
  if (przegladarka) utils::browseURL(x$verification_uri)
  czekaj_device(x, client_id, min(timeout, x$expires_in))
}

czekaj_device <- function(x, client_id, timeout, post = oauth_post,
                          czekaj = Sys.sleep, zegar = function() as.numeric(Sys.time())) {
  koniec <- zegar() + timeout
  interwal <- max(5, x$interval)
  repeat {
    pozostalo <- koniec - zegar()
    if (pozostalo < interwal) stop("Kod logowania wygas\u0142; uruchom zaloguj_github() ponownie.", call. = FALSE)
    czekaj(interwal)
    y <- post("login/oauth/access_token", client_id = client_id, device_code = x$device_code,
              grant_type = "urn:ietf:params:oauth:grant-type:device_code")
    if (!is.null(y$access_token)) return(y$access_token)
    if (identical(y$error, "authorization_pending")) next
    if (identical(y$error, "slow_down")) {
      interwal <- interwal + 5
      if (interwal > 60) stop("GitHub ograniczy\u0142 logowanie; spr\u00f3buj p\u00f3\u017aniej.", call. = FALSE)
      next
    }
    if (identical(y$error, "access_denied")) stop("Logowanie anulowano na GitHub.", call. = FALSE)
    stop("Kod wygas\u0142 lub logowanie zosta\u0142o odrzucone; zacznij ponownie.", call. = FALSE)
  }
}

token_gh <- function(timeout) {
  exe <- Sys.which("gh")
  if (!nzchar(exe)) stop("Brak GitHub CLI. Potrzebna jest instalacja gh przez informatyka albo client ID aplikacji kursu.", call. = FALSE)
  katalog <- tempfile("github-sesja-")
  if (!dir.create(katalog, mode = "0700")) stop("Nie mo\u017cna przygotowa\u0107 logowania.", call. = FALSE)
  on.exit(unlink(katalog, recursive = TRUE), add = TRUE)
  env <- c("current", GH_CONFIG_DIR = katalog, GH_TOKEN = "", GITHUB_TOKEN = "", GITHUB_PAT = "", GH_ENTERPRISE_TOKEN = "",
           GITHUB_ENTERPRISE_TOKEN = "", GH_HOST = "github.com", GH_PROMPT_DISABLED = "1")
  p <- processx::process$new(exe,
    c("auth", "login", "--hostname", "github.com", "--git-protocol", "https", "--web", "--insecure-storage"),
    stdin = "|", stdout = "|", stderr = "|", env = env, cleanup = TRUE)
  on.exit(if (p$is_alive()) p$kill(), add = TRUE)
  p$write_input("n\n\n")
  koniec <- as.numeric(Sys.time()) + timeout
  while (p$is_alive()) {
    if (as.numeric(Sys.time()) > koniec) stop("Logowanie przekroczy\u0142o czas; spr\u00f3buj ponownie.", call. = FALSE)
    p$poll_io(250)
    for (tekst in c(p$read_output_lines(), p$read_error_lines())) {
      if (grepl("(gh[pousr]_|github_pat_)[A-Za-z0-9_]+", tekst)) next
      message(tekst)
    }
  }
  if (p$get_exit_status() != 0L) stop("GitHub CLI nie zako\u0144czy\u0142 logowania; sprawd\u017a po\u0142\u0105czenie i potwierdzenie w przegl\u0105darce.", call. = FALSE)
  plik <- file.path(katalog, "hosts.yml")
  if (!file.exists(plik)) stop("GitHub CLI nie zapisa\u0142 potwierdzenia logowania.", call. = FALSE)
  x <- czytaj_yaml(plik)[["github.com"]]
  token <- x$oauth_token
  if (is.null(token) && !is.null(x$user)) token <- x$users[[x$user]]$oauth_token
  if (is.null(token)) stop("Brak potwierdzonego tokena sesji.", call. = FALSE)
  token
}
