repo_testowe <- function() {
  k <- tempfile("badaniazi-git-")
  dir.create(k)
  gert::git_init(k)
  gert::git_config_set("user.name", "Test kursu", repo = k)
  gert::git_config_set("user.email", "test@example.org", repo = k)
  writeLines("wersja pierwsza", file.path(k, "plik.md"))
  gert::git_add("plik.md", repo = k)
  gert::git_commit("Pierwsza wersja", repo = k)
  k
}

test_that("aktualizacja przesuwa gałąź, a lokalny commit i rozbieżność zachowuje", {
  k <- repo_testowe()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  pierwsza <- gert::git_info(repo = k)$commi
  glowna <- gert::git_info(repo = k)$shorthand
  gert::git_branch_create("zdalna", repo = k)
  gert::git_branch_checkout("zdalna", repo = k)
  writeLines("wersja druga", file.path(k, "plik.md"))
  gert::git_add("plik.md", repo = k)
  gert::git_commit("Druga wersja", repo = k)
  druga <- gert::git_info(repo = k)$commi
  gert::git_branch_checkout(glowna, repo = k)
  expect_identical(badaniaZI:::aktualizuj_repo_lokalne(k, "zdalna"), druga)
  expect_equal(nrow(gert::git_log(repo = k)), 2)
  expect_identical(badaniaZI:::aktualizuj_repo_lokalne(k, "zdalna"), druga)
  writeLines("lokalna odpowiedź", file.path(k, "plik.md"))
  gert::git_add("plik.md", repo = k)
  gert::git_commit("Własna praca", repo = k)
  lokalna <- gert::git_info(repo = k)$commi
  expect_error(badaniaZI:::aktualizuj_repo_lokalne(k, "zdalna"), "lokalne commity")
  gert::git_branch_checkout("zdalna", repo = k)
  writeLines("zdalna odpowiedź", file.path(k, "plik.md"))
  gert::git_add("plik.md", repo = k)
  gert::git_commit("Inna odpowiedź", repo = k)
  gert::git_branch_checkout(glowna, repo = k)
  expect_error(badaniaZI:::aktualizuj_repo_lokalne(k, "zdalna"), "rozbie")
  expect_identical(gert::git_info(repo = k)$commit, lokalna)
  expect_identical(readLines(file.path(k, "plik.md")), "lokalna odpowiedź")
  expect_false(identical(pierwsza, lokalna))
})

test_that("obcy adres wysyłki i repo rodzica są odrzucane", {
  k <- repo_testowe()
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  gert::git_remote_add("https://login@github.com/owner/praca.git", repo = k)
  expect_true(badaniaZI:::sprawdz_origin(k, "owner/praca"))
  gert::git_remote_set_pushurl("https://github.com/owner/inne.git", "origin", repo = k)
  expect_error(badaniaZI:::sprawdz_origin(k, "owner/praca"), "wysy")
  gert::git_remote_set_pushurl("https://github.com/owner/praca.git", "origin", repo = k)
  expect_true(badaniaZI:::sprawdz_origin(k, "owner/praca"))
  dir.create(file.path(k, "podkatalog"))
  expect_error(badaniaZI:::sprawdz_origin(file.path(k, "podkatalog"), "owner/praca"))
})
