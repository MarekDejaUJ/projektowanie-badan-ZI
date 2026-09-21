test_that("odrzucony nowy klon nie zostawia plików Git tylko do odczytu", {
  k <- tempfile("bledne-id-")
  on.exit(unlink(k, recursive = TRUE, force = TRUE), add = TRUE)
  local_mocked_bindings(token_sesji = function() "test-only",
    github_api = function(...) list(dane = list(private = TRUE, permissions = list(push = TRUE))),
    .package = "badaniaZI")
  local_mocked_bindings(git_clone = function(url, path, ...) {
    utworz_projekt("s018", katalog = path, repo = "kurs/praca-s017")
    pack <- file.path(path, ".git", "objects", "pack")
    dir.create(pack, recursive = TRUE)
    p <- file.path(pack, "test.pack")
    writeBin(as.raw(1:10), p)
    Sys.chmod(p, "0444")
    invisible(path)
  }, .package = "gert")
  expect_error(pobierz_zadanie("https://github.com/kurs/praca-s017", k, "s017"), "ID")
  expect_false(dir.exists(k))
})

test_that("istniejąca praca nie podlega sprzątaniu nieudanego klonu", {
  k <- tempfile("istniejaca-praca-")
  dir.create(k)
  on.exit(unlink(k, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("Własna odpowiedź", file.path(k, "odpowiedz.txt"))
  local_mocked_bindings(token_sesji = function() "test-only",
    github_api = function(...) list(dane = list(private = TRUE, permissions = list(push = TRUE))),
    .package = "badaniaZI")
  local_mocked_bindings(git_clone = function(...) stop("Nie wolno klonować do istniejącego katalogu"),
    .package = "gert")
  expect_error(pobierz_zadanie("https://github.com/kurs/praca-s017", k, "s017"), "istnieje")
  expect_identical(readLines(file.path(k, "odpowiedz.txt")), "Własna odpowiedź")
})
