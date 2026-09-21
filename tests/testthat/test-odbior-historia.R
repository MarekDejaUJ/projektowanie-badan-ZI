fixture_historia_odbioru <- function(poprawka = FALSE) {
  f <- new.env(parent = emptyenv())
  f$sha <- vapply(1:4, function(i) strrep(as.character(i), 40), character(1))
  f$head <- f$sha[4]
  f$parents <- setNames(list(list(), list(list(sha = f$sha[1])),
    list(list(sha = f$sha[2])), list(list(sha = f$sha[3]))), f$sha)
  f$statusy <- setNames(rep(list(list()), 4L), f$sha)
  f$statusy[[f$sha[2]]] <- list(list(id = 21L, state = "success", context = "badaniaZI/odbior/Z01",
    created_at = "2026-09-21T10:00:00Z"))
  f$statusy[[f$sha[4]]] <- list(list(id = 41L, state = "success", context = "badaniaZI/odbior/Z02",
    created_at = "2026-09-21T11:00:00Z"))
  if (poprawka) f$statusy[[f$sha[3]]] <- list(list(id = 31L, state = "success",
    context = "badaniaZI/odbior/Z01", created_at = "2026-09-21T10:30:00Z"))
  form <- formularz_oceny("Z01")
  form$punkty <- form$maksimum
  ocena <- list(zadanie = "Z01", sha = f$sha[2], rubryka = rubryka("Z01")$version,
    kryteria = form[c("kryterium", "punkty", "komentarz")], punkty = sum(form$punkty))
  f$komentarze <- setNames(rep(list(list()), 4L), f$sha)
  f$komentarze[[f$sha[2]]] <- list(list(user = list(login = "owner"), commit_id = f$sha[2],
    body = paste0("Informacja zwrotna badaniaZI\n\n```json\n",
      jsonlite::toJSON(ocena, auto_unbox = TRUE, dataframe = "rows"), "\n```"),
    updated_at = "2026-09-21T12:00:00Z", html_url = "https://github.com/owner/praca/commit/test"))
  f$calls <- character()
  f$api <- function(sciezka, metoda = "GET", ..., brak_ok = FALSE) {
    stopifnot(metoda == "GET")
    f$calls <- c(f$calls, sciezka)
    p <- sub("^repos/owner/praca/", "", sciezka)
    d <- if (p == "git/ref/heads/main") list(object = list(sha = f$head)) else
      if (startsWith(p, "git/commits/")) {
        s <- sub("git/commits/", "", p, fixed = TRUE)
        list(sha = s, parents = f$parents[[s]], committer = list(date = "1900-01-01T00:00:00Z"))
      } else if (grepl("^commits/[^/]+/statuses", p)) {
        f$statusy[[strsplit(p, "/", fixed = TRUE)[[1]][2]]]
      } else if (grepl("^commits/[^/]+/comments", p)) {
        f$komentarze[[strsplit(p, "/", fixed = TRUE)[[1]][2]]]
      } else if (grepl("^commits/[0-9a-f]{40}$", p)) {
        s <- sub("commits/", "", p, fixed = TRUE)
        if (s %in% f$sha) list(sha = s) else NULL
      } else if (startsWith(p, "actions/runs?")) list(workflow_runs = list()) else stop(p)
    list(dane = d)
  }
  f
}

test_that("status i ocena dotyczą zadania mimo późniejszych oddań i lokalnego HEAD", {
  f <- fixture_historia_odbioru()
  local_mocked_bindings(github_api = f$api, katalog_kursu = function(k) k,
    sprawdz_repo = function(k) list(repo = "owner/praca"), .package = "badaniaZI")
  local_mocked_bindings(git_info = function(...) stop("Nie używaj lokalnego HEAD"), .package = "gert")
  x <- status_oddania("z01", "lokalnie")
  expect_identical(x$sha, f$sha[2])
  expect_identical(x$stan, "odebrane")
  expect_identical(x$id_odbioru, 21L)
  expect_identical(status_oddania("Z02", "lokalnie")$sha, f$sha[4])
  expect_identical(pobierz_ocene("Z01", "lokalnie")$stan, "oceniono")
  expect_identical(pobierz_ocene("Z01", "lokalnie")$ocena$sha, f$sha[2])
  expect_identical(status_oddania("Z03", "lokalnie")$stan, "brak_oddania")
  expect_null(status_oddania("Z03", "lokalnie")$sha)
  expect_identical(pobierz_ocene("Z03", "lokalnie")$stan, "nieopublikowana")
  expect_null(pobierz_ocene("Z03", "lokalnie")$sha)
})

test_that("nowe nieocenione oddanie nie pokazuje starej oceny, jawne SHA nadal działa", {
  f <- fixture_historia_odbioru(TRUE)
  local_mocked_bindings(github_api = f$api, katalog_kursu = function(k) k,
    sprawdz_repo = function(k) list(repo = "owner/praca"), .package = "badaniaZI")
  expect_identical(status_oddania("Z01", "lokalnie")$sha, f$sha[3])
  x <- pobierz_ocene("Z01", "lokalnie")
  expect_identical(x$stan, "nieopublikowana")
  expect_identical(x$sha, f$sha[3])
  expect_null(x$ocena)
  expect_identical(pobierz_ocene("Z01", "lokalnie", f$sha[2])$stan, "oceniono")
  expect_identical(status_oddania("Z01", "lokalnie", f$sha[2])$id_odbioru, 21L)
  expect_identical(status_oddania("Z01", "lokalnie", f$sha[1])$stan, "wyslane_bez_pokwitowania")
  expect_identical(status_oddania("Z01", "lokalnie", strrep("a", 40))$stan, "tylko_lokalnie")
  expect_error(status_oddania("Z01", "lokalnie", "błędne"), "sha")
})

test_that("niepełna, połączona i zapętlona historia nie udaje braku oddania", {
  f <- fixture_historia_odbioru()
  local_mocked_bindings(github_api = f$api, .package = "badaniaZI")
  expect_error(badaniaZI:::ostatnie_oddanie("owner/praca", "Z01", limit = 2L), "całej historii")
  f$parents[[f$head]] <- list(list(sha = f$sha[3]), list(sha = f$sha[2]))
  expect_error(badaniaZI:::ostatnie_oddanie("owner/praca", "Z01"), "połączenie gałęzi")
  f$parents[[f$head]] <- list(list(sha = f$head))
  expect_error(badaniaZI:::ostatnie_oddanie("owner/praca", "Z01"), "całej historii")
  f$parents[[f$head]] <- NULL
  expect_error(badaniaZI:::ostatnie_oddanie("owner/praca", "Z01"), "kompletnej historii")
  f$head <- "nieprawidłowe SHA"
  expect_error(badaniaZI:::ostatnie_oddanie("owner/praca", "Z01"), "SHA oddania")
})

test_that("wiele pokwitowań jednego SHA zachowuje najwcześniejszy czas serwera", {
  f <- fixture_historia_odbioru()
  pierwszy <- f$statusy[[f$sha[2]]][[1L]]
  duplikat <- pierwszy
  duplikat$id <- 22L
  duplikat$created_at <- "2026-09-22T10:00:00Z"
  pages <- integer()
  local_mocked_bindings(github_api = function(sciezka, ...) {
    page <- as.integer(sub(".*page=", "", sciezka))
    pages <<- c(pages, page)
    list(dane = if (page == 1L) rep(list(duplikat), 100L) else list(pierwszy))
  }, .package = "badaniaZI")
  expect_identical(badaniaZI:::znajdz_odbior("owner/praca", f$sha[2], "Z01"), pierwszy)
  expect_identical(pages, 1:2)
})

test_that("porównanie starszych plików nie cofa gałęzi ani niezapisanych w Git odpowiedzi", {
  k <- tempfile("porownanie historii ")
  on.exit({ gc(); unlink(k, recursive = TRUE, force = TRUE) }, add = TRUE)
  dir.create(k)
  gert::git_init(k)
  gert::git_config_set("user.name", "Test kursu", repo = k)
  gert::git_config_set("user.email", "test@example.org", repo = k)
  writeLines("* -text", file.path(k, ".gitattributes"))
  writeBin(charToRaw("Moja odpowiedź.\r\n"), file.path(k, "zadanie.Rmd"))
  gert::git_add(c(".gitattributes", "zadanie.Rmd"), repo = k)
  pierwsza <- gert::git_commit("Oddaj Z01", repo = k)
  if (gert::git_info(repo = k)$shorthand != "main") gert::git_branch_create("main", checkout = TRUE, repo = k)
  pliki <- "zadanie.Rmd"
  hashe <- badaniaZI:::hashe_plikow(k, pliki)
  writeLines("Drugie zadanie", file.path(k, "inne.Rmd"))
  gert::git_add("inne.Rmd", repo = k)
  druga <- gert::git_commit("Oddaj Z02", repo = k)
  expect_true(badaniaZI:::identyczne_oddanie(k, pliki, hashe, pierwsza))
  expect_false(badaniaZI:::identyczne_oddanie(k, "inne.Rmd", badaniaZI:::hashe_plikow(k, "inne.Rmd"), pierwsza))
  writeLines("Nowa odpowiedź", file.path(k, pliki))
  zmienione <- badaniaZI:::hashe_plikow(k, pliki)
  expect_false(badaniaZI:::identyczne_oddanie(k, pliki, zmienione, pierwsza))
  expect_identical(gert::git_info(repo = k)$commit, druga)
  expect_identical(badaniaZI:::hashe_plikow(k, pliki), zmienione)
  expect_identical(gert::git_status(repo = k)$file, pliki)
})

test_that("niezmienione zadanie po innym oddaniu zachowuje SHA i czas, bez push ani nowego statusu", {
  f <- fixture_historia_odbioru()
  zgodne <- TRUE
  push <- 0L
  post <- 0L
  h <- c("zadanie.Rmd" = "hash")
  local_mocked_bindings(
    katalog_kursu = function(k) k,
    sprawdz_zadanie = function(...) list(ok = TRUE, pliki = "zadanie.Rmd"),
    hashe_plikow = function(...) h,
    sprawdz_repo = function(...) list(repo = "owner/praca"),
    token_sesji = function(...) "test",
    sprawdz_historie_oddania = function(...) TRUE,
    kontroluj_wejscia_rmd = function(...) list(),
    sprawdz_aktualnosc_pdf = function(...) TRUE,
    sprawdz_bajty_commitu = function(...) TRUE,
    identyczne_oddanie = function(katalog, pliki, hashe, sha) {
      expect_identical(sha, f$sha[2])
      expect_identical(hashe, h)
      zgodne
    },
    github_api = function(sciezka, metoda = "GET", ...) {
      if (metoda == "POST") {
        post <<- post + 1L
        expect_identical(sciezka, paste0("repos/owner/praca/statuses/", f$head))
        return(list(dane = list(id = 42L, created_at = "2026-09-21T13:00:00Z")))
      }
      f$api(sciezka, metoda, ...)
    }, .package = "badaniaZI")
  local_mocked_bindings(
    git_status = function(...) data.frame(file = character()),
    git_fetch = function(...) NULL,
    git_ahead_behind = function(...) list(ahead = 0L, behind = 0L),
    git_commit_id = function(...) f$head,
    git_info = function(...) list(commit = f$head),
    git_add = function(...) NULL,
    git_push = function(...) { push <<- push + 1L; NULL }, .package = "gert")
  x <- suppressMessages(oddaj_zadanie("Z01", "lokalnie"))
  expect_identical(x$sha, f$sha[2])
  expect_identical(x$id_odbioru, 21L)
  expect_identical(x$czas_serwera, "2026-09-21T10:00:00Z")
  expect_identical(push, 0L)
  expect_identical(post, 0L)
  # Zmienione pliki już obecne na serwerze, lecz jeszcze bez potwierdzenia:
  # stare pokwitowanie nie może potwierdzić innej treści.
  zgodne <- FALSE
  y <- suppressMessages(oddaj_zadanie("Z01", "lokalnie"))
  expect_identical(y$sha, f$head)
  expect_identical(y$id_odbioru, 42L)
  expect_identical(push, 1L)
  expect_identical(post, 1L)
})
