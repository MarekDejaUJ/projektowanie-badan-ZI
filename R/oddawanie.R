#' Oddanie zadania z konsoli R
#'
#' Wysyła wyłącznie jawną listę plików. Potwierdzeniem jest zdalny SHA oraz
#' rekord odbioru utworzony przez GitHub, z czasem serwera. Zielony stan
#' odbioru nie jest oceną merytoryczną ani wynikiem kontroli kodu.
#' @param id Z01--Z10.
#' @param katalog Katalog projektu.
#' @return Lista z repo, SHA, ID odbioru i czasem serwera.
#' @export
#' @examples
#' \dontrun{ oddaj_zadanie("Z01", "moje-badania") }
oddaj_zadanie <- function(id, katalog = ".") {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^Z(0[1-9]|10)$")
  oddaj_prace(id, katalog)
}

#' Oddanie indywidualnego projektu ilościowego
#' @param katalog Katalog projektu.
#' @return Pokwitowanie GitHub z SHA i czasem serwera.
#' @export
#' @examples
#' \dontrun{ oddaj_projekt("moje-badania") }
oddaj_projekt <- function(katalog = ".") oddaj_prace("PROJEKT", katalog)

oddaj_prace <- function(id, katalog) {
  cfg <- sprawdz_repo(katalog)
  kontrola <- sprawdz_zadanie(id, katalog)
  if (!kontrola$ok) stop("Popraw elementy oznaczone FALSE w sprawdz_zadanie(). Niczego nie wys\u0142ano.", call. = FALSE)
  message("Pliki oddania:\n", paste(kontrola$pliki, collapse = "\n"))
  staged <- gert::git_status(staged = TRUE, repo = katalog)
  if (any(!staged$file %in% kontrola$pliki)) stop("W indeksie Git s\u0105 pliki spoza tego oddania. Sprawd\u017a je w zak\u0142adce Git RStudio.", call. = FALSE)
  operacja_git(gert::git_fetch("origin", password = token_sesji(), repo = katalog, verbose = FALSE))
  roznice <- operacja_git(gert::git_ahead_behind(upstream = "origin/main", repo = I(katalog)))
  if (roznice$behind > 0L)
    stop("Repozytorium zdalne ma nowsze commity. Zachowaj swoje pliki i uzgodnij wersje przed oddaniem; niczego nie nadpisano ani nie wys\u0142ano.", call. = FALSE)
  operacja_git(gert::git_add(kontrola$pliki, repo = katalog))
  if (nrow(gert::git_status(staged = TRUE, repo = katalog))) {
    sygnatura <- gert::git_signature(sesja_github$login,
      paste0(sesja_github$id, "+", sesja_github$login, "@users.noreply.github.com"))
    operacja_git(gert::git_commit(paste("Oddaj", id), author = sygnatura, committer = sygnatura, repo = katalog))
  }
  sha <- gert::git_info(repo = katalog)$commit
  operacja_git(gert::git_push("origin", refspec = "refs/heads/main:refs/heads/main",
                             password = token_sesji(), force = FALSE, verbose = FALSE, repo = katalog))
  remote <- github_api(paste0("repos/", cfg$repo, "/git/ref/heads/main"))$dane$object$sha
  if (!identical(remote, sha)) stop("Zdalna ga\u0142\u0105\u017a zmieni\u0142a si\u0119 podczas wysy\u0142ki. Sprawd\u017a status i zachowaj lokaln\u0105 prac\u0119.", call. = FALSE)
  odbior <- znajdz_odbior(cfg$repo, sha, id)
  if (is.null(odbior)) odbior <- github_api(paste0("repos/", cfg$repo, "/statuses/", sha), "POST",
    list(state = "success", context = paste0("badaniaZI/odbior/", id),
         description = paste("Odebrano", id, "\u2014 kontrola i ocena osobno"),
         target_url = paste0("https://github.com/", cfg$repo, "/commit/", sha)))$dane
  wynik <- pokwitowanie(cfg$repo, sha, id, odbior)
  message("Odebrano ", id, ". SHA: ", sha, ". Czas GitHub: ", wynik$czas_serwera, ".")
  wynik
}

lista_statusow <- function(repo, sha) {
  wyniki <- list()
  strona <- 1L
  repeat {
    x <- github_api(paste0("repos/", repo, "/commits/", sha, "/statuses?per_page=100&page=", strona))$dane
    wyniki <- c(wyniki, x)
    if (length(x) < 100L) return(wyniki)
    strona <- strona + 1L
  }
}

znajdz_odbior <- function(repo, sha, id) {
  x <- lista_statusow(repo, sha)
  x <- Filter(function(y) identical(tolower(y$context), tolower(paste0("badaniaZI/odbior/", id))) &&
                identical(y$state, "success") && !is.null(y$created_at), x)
  if (!length(x)) return(NULL)
  x[[which.min(vapply(x, function(y) as.numeric(as.POSIXct(y$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), numeric(1)))]]
}

pokwitowanie <- function(repo, sha, id, odbior) {
  list(stan = "odebrane", zadanie = id, repo = repo, sha = sha, id_odbioru = odbior$id,
       czas_serwera = odbior$created_at, url = paste0("https://github.com/", repo, "/commit/", sha),
       kontrola = "sprawdz_osobno", ocena = "sprawdz_osobno")
}

#' Status odbioru i kontroli
#' @param id Z01--Z10 albo PROJEKT.
#' @param katalog Katalog projektu.
#' @param sha Opcjonalne SHA poprzedniego oddania; domyślnie lokalne HEAD.
#' @return Stan odbioru; wynik kontroli Actions jest osobnym polem.
#' @export
#' @examples
#' \dontrun{ status_oddania("Z01", "moje-badania") }
status_oddania <- function(id, katalog = ".", sha = NULL) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  cfg <- sprawdz_repo(katalog)
  if (is.null(sha)) sha <- gert::git_info(repo = katalog)$commit
  sprawdz_id(sha, "sha", "^[0-9a-f]{40}$")
  commit <- github_api(paste0("repos/", cfg$repo, "/commits/", sha), brak_ok = TRUE)$dane
  if (is.null(commit)) return(list(stan = "tylko_lokalnie", zadanie = id, sha = sha))
  odbior <- znajdz_odbior(cfg$repo, sha, id)
  if (is.null(odbior)) return(list(stan = "wyslane_bez_pokwitowania", zadanie = id, sha = sha))
  wynik <- pokwitowanie(cfg$repo, sha, id, odbior)
  runy <- github_api(paste0("repos/", cfg$repo, "/actions/runs?head_sha=", sha, "&per_page=100"))$dane$workflow_runs
  wynik$kontrole <- lapply(runy, function(r) list(nazwa = r$name, status = r$status,
                         wynik = r$conclusion, url = r$html_url))
  wynik
}
