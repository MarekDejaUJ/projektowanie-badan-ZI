# Jawny git_add nie ogranicza starszych commitów, które również trafią na serwer.
# Przechodzimy po rodzicach, nie po datach ani ograniczonej liście git_log.
sprawdz_historie_oddania <- function(katalog, id, pliki, sha, baza) {
  odmowa <- function() stop(paste(
    "Niewys\u0142ana historia Git zawiera zmiany spoza bezpiecznego oddania tego zadania.",
    "Zachowano wszystkie pliki i commity; niczego nie wys\u0142ano.",
    "Nie usuwaj historii ani nie u\u017cywaj force-push. Popro\u015b prowadz\u0105cego o pomoc."
  ), call. = FALSE)
  sprawdz_id(sha, "SHA pracy", "^[0-9a-f]{40}$")
  sprawdz_id(baza, "SHA wersji zdalnej", "^[0-9a-f]{40}$")
  if (identical(sha, baza)) return(invisible(TRUE))
  rewizje <- character()
  obecny <- sha
  while (!identical(obecny, baza)) {
    if (length(rewizje) >= 500L || obecny %in% rewizje) odmowa()
    info <- operacja_git(gert::git_commit_info(obecny, repo = I(katalog)))
    # Merge lub brak rodzica wymaga ręcznego wyjaśnienia; nie pomijamy drugiej gałęzi.
    if (length(info$parents) != 1L) odmowa()
    zmiany <- operacja_git(gert::git_diff(ref = obecny, repo = I(katalog)))
    if (is.null(zmiany) || !nrow(zmiany) ||
        any(!zmiany$status %in% c("A", "M")) ||
        any(!zmiany$old %in% pliki) || any(!zmiany$new %in% pliki)) odmowa()
    tekst <- c(info$author, info$committer, info$message, zmiany$patch)
    if (any(grepl("(gh[pousr]_[A-Za-z0-9_]{20,}|github_pat_[A-Za-z0-9_]{20,})", tekst), na.rm = TRUE))
      stop("Niewys\u0142any commit zawiera zapis przypominaj\u0105cy token GitHub. Nie wys\u0142ano historii, nawet je\u015bli token zosta\u0142 p\u00f3\u017aniej usuni\u0119ty. Uniewa\u017cnij ujawniony token i popro\u015b o pomoc w zachowaniu pracy.", call. = FALSE)
    rewizje <- c(rewizje, obecny)
    obecny <- info$parents[[1L]]
  }
  # Każdy wcześniejszy stan musi być kompletną pracą, nie tylko poprawny końcowy diff.
  # Kontrola czyta kopię commitu; nie uruchamia jego kodu i nie zmienia pracy studenta.
  tmp <- tempfile("historia-oddania-")
  on.exit({ gc(); unlink(tmp, recursive = TRUE) }, add = TRUE)
  operacja_git(gert::git_clone(katalog, path = tmp, branch = "main", verbose = FALSE))
  operacja_git(gert::git_branch_create("kontrola-bazy", ref = baza, checkout = TRUE, repo = I(tmp)))
  cfg <- czytaj_yaml(file.path(tmp, "kurs.yml"))
  if (!identical(cfg, czytaj_yaml(file.path(katalog, "kurs.yml")))) odmowa()
  for (i in seq_along(rewizje)) {
    operacja_git(gert::git_branch_create(paste0("kontrola-", i), ref = rewizje[i],
      checkout = TRUE, repo = I(tmp)))
    if (!identical(cfg, czytaj_yaml(file.path(tmp, "kurs.yml")))) odmowa()
    wynik <- sprawdz_zadanie(id, tmp, uruchom = FALSE)
    if (!wynik$ok || !identical(wynik$pliki, pliki)) odmowa()
  }
  if (!identical(gert::git_info(repo = I(katalog))$commit, sha)) odmowa()
  invisible(TRUE)
}
