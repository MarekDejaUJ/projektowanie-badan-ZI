# SHA dotyczy zadania, nie ostatnio otwartego pliku ani lokalnego HEAD.
# Kolejność wyznaczają rodzice commitów, a nie zegar komputera studenta.
ostatnie_oddanie <- function(repo, id, limit = 500L) {
  sha <- github_api(paste0("repos/", repo, "/git/ref/heads/main"))$dane$object$sha
  odwiedzone <- character()
  repeat {
    sprawdz_id(sha, "SHA oddania", "^[0-9a-f]{40}$")
    if (sha %in% odwiedzone || length(odwiedzone) >= limit)
      stop("Nie uda\u0142o si\u0119 przejrze\u0107 ca\u0142ej historii odda\u0144. Podaj sha z pokwitowania lub popro\u015b prowadz\u0105cego o pomoc.", call. = FALSE)
    odwiedzone <- c(odwiedzone, sha)
    odbior <- znajdz_odbior(repo, sha, id)
    if (!is.null(odbior)) return(list(sha = sha, odbior = odbior))
    commit <- github_api(paste0("repos/", repo, "/git/commits/", sha))$dane
    if (!identical(commit$sha, sha) || is.null(commit$parents))
      stop("GitHub nie zwr\u00f3ci\u0142 kompletnej historii odda\u0144. Spr\u00f3buj ponownie.", call. = FALSE)
    if (!length(commit$parents)) return(NULL)
    if (length(commit$parents) != 1L)
      stop("Historia zawiera po\u0142\u0105czenie ga\u0142\u0119zi. Podaj sha z pokwitowania lub popro\u015b prowadz\u0105cego o pomoc.", call. = FALSE)
    sha <- commit$parents[[1L]]$sha
  }
}

# Historyczne pliki czytamy w osobnym lokalnym klonie; nie cofamy gałęzi
# ani nie nadpisujemy odpowiedzi, które student ma otwarte w edytorze.
identyczne_oddanie <- function(katalog, pliki, hashe, sha) {
  sprawdz_id(sha, "SHA oddania", "^[0-9a-f]{40}$")
  tmp <- tempfile("porownanie-oddania-")
  if (file.exists(tmp)) stop("Nie mo\u017cna przygotowa\u0107 por\u00f3wnania odda\u0144.", call. = FALSE)
  on.exit({ gc(); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)
  operacja_git(gert::git_clone(katalog, path = tmp, branch = "main", verbose = FALSE))
  operacja_git(gert::git_branch_create("porownanie", ref = sha, checkout = TRUE, repo = I(tmp)))
  if (!all(file.exists(file.path(tmp, pliki)))) return(FALSE)
  identical(hashe_plikow(tmp, pliki), hashe)
}
