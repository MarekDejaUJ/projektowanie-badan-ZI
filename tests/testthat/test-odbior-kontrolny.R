fixture_odbior <- function(id = "Z01") {
  paths <- c(badaniaZI:::pliki_wejscia_pracy(id), badaniaZI:::pliki_pdf_pracy(id))
  blobs <- list()
  tree <- lapply(paths, function(p) {
    bytes <- charToRaw(paste("Nie wykonywać treści pobranego pliku", p))
    sha <- digest::digest(c(charToRaw(paste0("blob ", length(bytes))), as.raw(0), bytes),
      algo = "sha1", serialize = FALSE)
    blobs[[sha]] <<- list(sha = sha, size = length(bytes), encoding = "base64",
      content = jsonlite::base64_enc(bytes))
    list(path = p, sha = sha, size = length(bytes), type = "blob", mode = "100644")
  })
  list(tree = list(truncated = FALSE, tree = tree), blobs = blobs)
}

test_that("wybór oddania jest jawny i odrzuca linki, duplikaty i zbyt duże pliki", {
  f <- fixture_odbior()
  extra <- list(path = "sekrety.txt", mode = "100644", type = "blob", size = 100L, sha = strrep("a", 40))
  f$tree$tree <- c(f$tree$tree, list(extra))
  expect_length(badaniaZI:::wybierz_pliki_odbioru(f$tree, "Z01"), 7L)
  for (mode in c("120000", "160000", "100755")) {
    t <- f$tree
    t$tree[[1]]$mode <- mode
    expect_error(badaniaZI:::wybierz_pliki_odbioru(t, "Z01"), "zwykłego pliku")
  }
  t <- f$tree
  t$tree <- c(t$tree, list(t$tree[[1]]))
  expect_error(badaniaZI:::wybierz_pliki_odbioru(t, "Z01"), "zwykłego pliku")
  t <- f$tree
  t$tree[[1]]$size <- 2e6 + 1
  expect_error(badaniaZI:::wybierz_pliki_odbioru(t, "Z01"), "rozmiar")
  t$truncated <- TRUE
  expect_error(badaniaZI:::wybierz_pliki_odbioru(t, "Z01"), "kompletnego")
})

test_that("bajty muszą odpowiadać rozmiarowi i hash Git blob", {
  f <- fixture_odbior()
  entry <- f$tree$tree[[1]]
  blob <- f$blobs[[entry$sha]]
  expect_identical(badaniaZI:::sprawdz_blob_odbioru(blob, entry), jsonlite::base64_dec(blob$content))
  blob$content <- jsonlite::base64_enc(as.raw(rep(1L, entry$size)))
  expect_error(badaniaZI:::sprawdz_blob_odbioru(blob, entry), "bajty")
  blob$size <- entry$size + 1L
  expect_error(badaniaZI:::sprawdz_blob_odbioru(blob, entry), "odpowiedź")
})

test_that("pobranie nie wykonuje plików, nie pobiera reszty repo i nie nadpisuje", {
  f <- fixture_odbior()
  sha <- strrep("a", 40)
  tree_sha <- strrep("b", 40)
  parent <- tempfile("kontrola-prywatna-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  wyloguj_github(FALSE)
  on.exit(wyloguj_github(FALSE), add = TRUE)
  auth <- badaniaZI:::sesja_github
  auth$login <- "owner"
  calls <- character()
  local_mocked_bindings(
    znajdz_odbior = function(...) list(id = 7L, created_at = "2026-09-21T10:00:00Z"),
    github_api = function(sciezka, ...) {
      calls <<- c(calls, sciezka)
      data <- if (sciezka == "repos/owner/praca") list(private = TRUE, owner = list(login = "owner")) else
        if (grepl("/git/commits/", sciezka)) list(sha = sha, tree = list(sha = tree_sha)) else
          if (grepl("/git/trees/", sciezka)) f$tree else
            if (grepl("/git/blobs/", sciezka)) f$blobs[[sub(".*/", "", sciezka)]] else stop("Obca ścieżka")
      list(dane = data)
    }, .package = "badaniaZI")
  k <- file.path(parent, "oddanie")
  p <- pobierz_oddanie("owner/praca", sha, "Z01", "s017", k)
  expect_identical(p, normalizePath(k, winslash = "/"))
  files <- list.files(file.path(k, "wejscie"), recursive = TRUE)
  expect_setequal(files, vapply(f$tree$tree, `[[`, "", "path"))
  proof <- jsonlite::read_json(file.path(k, "odbior.json"))
  expect_identical(proof$sha, sha)
  expect_identical(proof$id_studenta, "s017")
  expect_identical(proof$pliki, as.list(badaniaZI:::hashe_plikow(file.path(k, "wejscie"), names(proof$pliki))))
  expect_length(calls[grepl("/git/blobs/", calls)], 7L)
  expect_false(dir.exists(file.path(k, "wejscie/.git")))
  old <- readBin(file.path(k, "odbior.json"), "raw", n = 10000L)
  expect_error(pobierz_oddanie("owner/praca", sha, "Z01", "s017", k), "nowy prywatny")
  expect_identical(readBin(file.path(k, "odbior.json"), "raw", n = 10000L), old)
  auth$login <- "student"
  expect_error(pobierz_oddanie("owner/praca", sha, "Z01", "s017", file.path(parent, "inne")), "właściciel")
})
