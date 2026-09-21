#' Pobranie konkretnego oddania do niezależnej kontroli
#'
#' Funkcja dla właściciela prywatnego repozytorium. Pobiera wyłącznie jawne
#' pliki wskazanego zadania z niezmiennego SHA, bez klonowania historii,
#' otwierania PDF, wykonywania Rmd ani odczytywania RDS. Każdy plik jest
#' sprawdzany względem identyfikatora obiektu Git. Odpowiedzi i dane pozostają
#' prywatne; katalog przechowuj poza publicznym repozytorium kursu.
#' Kontrola techniczna i ocena merytoryczna są osobnymi krokami.
#' @param repo Prywatne repozytorium jako owner/name.
#' @param sha Pełny SHA oddania, z pokwitowania odbioru.
#' @param id Z01--Z10 albo PROJEKT.
#' @param id_studenta Oczekiwane pseudonimowe ID, z prywatnego przydziału.
#' @param katalog Nowy prywatny katalog kontroli. Nigdy nie jest nadpisywany.
#' @return Ścieżka katalogu z podkatalogiem wejscie i plikiem odbior.json,
#'   niewidocznie. To pobranie dowodów, nie wynik wykonania ani ocena.
#' @export
#' @examples
#' \dontrun{
#' pobierz_oddanie("prowadzacy/ZI-s017", "SHA_Z_POKWITOWANIA", "Z01",
#'                 "s017", "prywatna-kontrola-z01")
#' }
pobierz_oddanie <- function(repo, sha, id, id_studenta, katalog) {
  sprawdz_id(repo, "repo", "^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+$")
  sprawdz_id(sha, "sha", "^[0-9a-f]{40}$")
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  sprawdz_id(id_studenta, "id_studenta")
  if (!is.character(katalog) || length(katalog) != 1L || is.na(katalog) || !nzchar(katalog) ||
      file.exists(katalog) || dir.exists(katalog))
    stop("Wybierz nowy prywatny katalog kontroli. Istniej\u0105ce pliki pozostaj\u0105 bez zmian.", call. = FALSE)
  meta <- github_api(paste0("repos/", repo))$dane
  if (!identical(meta$private, TRUE) ||
      !identical(tolower(meta$owner$login), tolower(sesja_github$login)))
    stop("Oddania do oceny pobiera w\u0142a\u015bciciel ich prywatnego repozytorium.", call. = FALSE)
  odbior <- znajdz_odbior(repo, sha, id)
  if (is.null(odbior)) stop("Brak pokwitowania dla tego SHA i zadania.", call. = FALSE)
  commit <- github_api(paste0("repos/", repo, "/git/commits/", sha))$dane
  if (!identical(commit$sha, sha)) stop("GitHub nie potwierdzi\u0142 \u017c\u0105danego SHA.", call. = FALSE)
  sprawdz_id(commit$tree$sha, "SHA drzewa Git", "^[0-9a-f]{40}$")
  drzewo <- github_api(paste0("repos/", repo, "/git/trees/", commit$tree$sha, "?recursive=1"))$dane
  wpisy <- wybierz_pliki_odbioru(drzewo, id)
  dir.create(dirname(katalog), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("odbior-", tmpdir = dirname(katalog))
  if (!dir.create(tmp, mode = "0700")) stop("Nie mo\u017cna utworzy\u0107 prywatnej kopii oddania.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  wejscie <- file.path(tmp, "wejscie")
  dir.create(wejscie, mode = "0700")
  hashe <- list()
  for (x in wpisy) {
    blob <- github_api(paste0("repos/", repo, "/git/blobs/", x$sha))$dane
    bajty <- sprawdz_blob_odbioru(blob, x)
    cel <- file.path(wejscie, x$path)
    dir.create(dirname(cel), recursive = TRUE, showWarnings = FALSE)
    writeBin(bajty, cel)
    hashe[[x$path]] <- digest::digest(bajty, algo = "sha256", serialize = FALSE)
  }
  dowod <- list(format = "odbior-kontrolny-1", repo = repo, sha = sha,
    zadanie = id, id_studenta = id_studenta,
    odbior = pokwitowanie(repo, sha, id, odbior),
    pakiet = as.character(utils::packageVersion("badaniaZI")), pliki = hashe)
  pisz_linie(jsonlite::toJSON(dowod, auto_unbox = TRUE, pretty = TRUE), file.path(tmp, "odbior.json"))
  if (file.exists(katalog) || !file.rename(tmp, katalog))
    stop("Nie zapisano kopii: katalog docelowy powsta\u0142 podczas pobierania. Niczego nie nadpisano.", call. = FALSE)
  invisible(normalizePath(katalog, winslash = "/", mustWork = TRUE))
}

wybierz_pliki_odbioru <- function(drzewo, id) {
  if (!identical(drzewo$truncated, FALSE) || !is.list(drzewo$tree))
    stop("Nie otrzymano kompletnego drzewa plik\u00f3w Git.", call. = FALSE)
  pliki <- c(pliki_wejscia_pracy(id), pliki_pdf_pracy(id))
  wynik <- lapply(pliki, function(p) {
    x <- Filter(function(x) identical(x$path, p), drzewo$tree)
    limit <- if (grepl("[.]pdf$", p)) 10e6 else 2e6
    if (length(x) != 1L || !identical(x[[1L]]$type, "blob") ||
        !identical(x[[1L]]$mode, "100644") ||
        !is.numeric(x[[1L]]$size) || length(x[[1L]]$size) != 1L ||
        !is.finite(x[[1L]]$size) || x[[1L]]$size <= 0 || x[[1L]]$size > limit)
      stop("Brak zwyk\u0142ego pliku oddania lub niedopuszczalny rozmiar: ", p, ".", call. = FALSE)
    sprawdz_id(x[[1L]]$sha, "SHA pliku Git", "^[0-9a-f]{40}$")
    x[[1L]]
  })
  if (sum(vapply(wynik, `[[`, numeric(1), "size")) > 25e6)
    stop("Oddanie przekracza limit rozmiaru kontroli.", call. = FALSE)
  wynik
}

sprawdz_blob_odbioru <- function(blob, wpis) {
  if (!identical(blob$sha, wpis$sha) || !identical(blob$encoding, "base64") ||
      !isTRUE(blob$size == wpis$size) || !is.character(blob$content) ||
      length(blob$content) != 1L || is.na(blob$content) ||
      nchar(blob$content, type = "bytes") > 2 * wpis$size + 1024)
    stop("Niepoprawna odpowied\u017a dla pliku Git.", call. = FALSE)
  bajty <- tryCatch(jsonlite::base64_dec(blob$content), error = function(e) NULL)
  if (is.null(bajty) || length(bajty) != wpis$size)
    stop("Pobrany plik ma inn\u0105 d\u0142ugo\u015b\u0107 ni\u017c zapis Git.", call. = FALSE)
  naglowek <- c(charToRaw(paste0("blob ", length(bajty))), as.raw(0L))
  sha <- digest::digest(c(naglowek, bajty), algo = "sha1", serialize = FALSE)
  if (!identical(sha, wpis$sha)) stop("Pobrane bajty nie odpowiadaj\u0105 obiektowi Git.", call. = FALSE)
  bajty
}
