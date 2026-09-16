#' Publikacja prywatnej informacji zwrotnej
#'
#' Publikuje ocenę jako komentarz przypisany do rzeczywistego SHA pracy.
#' Wymaga logowania na konto właściciela repozytorium; punkty muszą odpowiadać
#' poziomom rubryki. Oceny nie trafiają do repozytorium materiałów ani Pages.
#' @param repo Prywatne repo jako owner/name.
#' @param sha SHA ocenianej pracy.
#' @param id Z01--Z10 albo PROJEKT.
#' @param formularz Wypełniony formularz_oceny().
#' @param komentarz Ogólna informacja merytoryczna.
#' @return URL prywatnego komentarza GitHub, niewidocznie.
#' @expor
#' @examples
#' \dontrun{
#' f <- formularz_oceny("Z01")
#' f$punkty <- f$maksimum
#' wystaw_ocene("prowadzacy/badania-s017", "SHA_PRACY", "Z01", f, "Poprawny odczyt wektora.")
#' }
wystaw_ocene <- function(repo, sha, id, formularz, komentarz = "") {
  id <- toupper(id)
  sprawdz_id(repo, "repo", "^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+$")
  sprawdz_id(sha, "sha", "^[0-9a-f]{40}$")
  meta <- github_api(paste0("repos/", repo))$dane
  if (!identical(meta$private, TRUE) || !identical(tolower(meta$owner$login), tolower(sesja_github$login)))
    stop("Oceny wystawia w\u0142a\u015bciciel indywidualnego prywatnego repozytorium.", call. = FALSE)
  suma <- sprawdz_ocene(formularz, id)
  r <- rubryka(id)
  odbior <- znajdz_odbior(repo, sha, id)
  if (is.null(odbior)) stop("Brak pokwitowania ocenianego SHA; nie publikuj\u0119 oceny.", call. = FALSE)
  ocena <- list(zadanie = id, sha = sha, rubryka = r$version, rocznik = r$rocznik,
                punkty = suma, maksimum = r$max_points,
                kryteria = formularz[c("kryterium", "punkty", "komentarz")], komentarz = komentarz,
                id_odbioru = odbior$id)
  body <- paste0("Informacja zwrotna badaniaZI\n\n```json\n",
                 jsonlite::toJSON(ocena, auto_unbox = TRUE, pretty = TRUE, dataframe = "rows"), "\n```")
  wynik <- github_api(paste0("repos/", repo, "/commits/", sha, "/comments"), "POST", list(body = body))$dane
  invisible(wynik$html_url)
}

#' Pobranie prywatnej oceny
#'
#' Odczytuje tylko komentarze uwierzytelnionego właściciela repozytorium,
#' powiązane z żądanym SHA i wersją rubryki. Brak feedbacku nie oznacza zera.
#' @param id Z01--Z10 albo PROJEKT.
#' @param katalog Katalog projektu.
#' @param sha SHA ocenionej pracy; domyślnie lokalne HEAD.
#' @return Lista ze stanem i oceną, jeśli opublikowano.
#' @expor
#' @examples
#' \dontrun{ pobierz_ocene("Z01", "moje-badania") }
pobierz_ocene <- function(id, katalog = ".", sha = NULL) {
  id <- toupper(id)
  r <- rubryka(id)
  cfg <- sprawdz_repo(katalog)
  if (is.null(sha)) sha <- gert::git_info(repo = katalog)$commi
  sprawdz_id(sha, "sha", "^[0-9a-f]{40}$")
  owner <- strsplit(cfg$repo, "/", fixed = TRUE)[[1]][1L]
  komentarze <- list()
  page <- 1L
  repeat {
    x <- github_api(paste0("repos/", cfg$repo, "/commits/", sha, "/comments?per_page=100&page=", page))$dane
    komentarze <- c(komentarze, x)
    if (length(x) < 100L) break
    page <- page + 1L
  }
  oceny <- list()
  for (x in komentarze) {
    if (!identical(tolower(x$user$login), tolower(owner)) || !identical(x$commit_id, sha) ||
        !startsWith(x$body, "Informacja zwrotna badaniaZI\n\n```json\n")) nex
    tekst <- sub("^Informacja zwrotna badaniaZI\n\n```json\n", "", x$body)
    tekst <- sub("\n```$", "", tekst)
    o <- tryCatch(jsonlite::fromJSON(tekst), error = function(e) NULL)
    if (is.null(o) || !identical(o$sha, sha) || !identical(o$zadanie, id) || !identical(o$rubryka, r$version)) nex
    poprawna <- tryCatch(sprawdz_ocene(o$kryteria, id), error = function(e) NA_real_)
    if (is.na(poprawna) || !isTRUE(all.equal(poprawna, o$punkty))) nex
    oceny[[length(oceny) + 1L]] <- list(stan = "oceniono", ocena = o, czas_serwera = x$updated_at, url = x$html_url)
  }
  if (!length(oceny)) return(list(stan = "nieopublikowana", sha = sha, ocena = NULL))
  oceny[[which.max(vapply(oceny, function(x) as.numeric(as.POSIXct(x$czas_serwera, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), numeric(1)))]]
}
