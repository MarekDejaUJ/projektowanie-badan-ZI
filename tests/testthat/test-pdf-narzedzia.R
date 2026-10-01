test_that("TinyTeX spoza PATH jest dopisywany do PATH bieżącej sesji", {
  pusty <- tempfile("bez-latexa-")
  dir.create(pusty)
  root <- tempfile("TinyTeX-")
  bin <- file.path(root, "bin", "platforma")
  dir.create(bin, recursive = TRUE)
  on.exit(unlink(c(pusty, root), recursive = TRUE), add = TRUE)
  for (f in c("xelatex", "xelatex.exe")) { writeLines("", file.path(bin, f)); Sys.chmod(file.path(bin, f), "0755") }
  withr::local_envvar(PATH = pusty)
  local_mocked_bindings(tinytex_root = function(...) root, .package = "tinytex")
  expect_true(badaniaZI:::dodaj_tinytex_do_path())
  expect_true(startsWith(Sys.getenv("PATH"), normalizePath(bin, winslash = "/")))
  expect_false(badaniaZI:::dodaj_tinytex_do_path())
})

test_that("brak TinyTeX i XeLaTeX nie zmienia PATH, a bez pytania nic nie instaluje", {
  pusty <- tempfile("bez-latexa-")
  dir.create(pusty)
  on.exit(unlink(pusty, recursive = TRUE), add = TRUE)
  withr::local_envvar(PATH = pusty)
  local_mocked_bindings(tinytex_root = function(...) "", .package = "tinytex")
  expect_false(badaniaZI:::dodaj_tinytex_do_path())
  expect_identical(Sys.getenv("PATH"), pusty)
  instalacje <- 0L
  local_mocked_bindings(przygotuj_pdf = function(...) { instalacje <<- instalacje + 1L; TRUE },
    zapytaj = function(...) "n", .package = "badaniaZI")
  expect_false(badaniaZI:::zapewnij_narzedzia_pdf(FALSE))
  expect_false(badaniaZI:::zapewnij_narzedzia_pdf(TRUE))
  expect_equal(instalacje, 0L)
  local_mocked_bindings(zapytaj = function(...) "t", .package = "badaniaZI")
  expect_true(badaniaZI:::zapewnij_narzedzia_pdf(TRUE))
  expect_equal(instalacje, 1L)
  expect_error(badaniaZI:::sprawdz_narzedzia_pdf(), "przygotuj_pdf")
})

test_that("lista pakietów LaTeX obejmuje skład polskiego PDF pracy", {
  p <- badaniaZI:::pakiety_latex()
  expect_false(anyDuplicated(p) > 0)
  expect_true(all(c("xetex", "fontspec", "unicode-math", "babel-polish", "hyphen-polish", "framed", "booktabs") %in% p))
})

test_that("próbny skład XeLaTeX działa na przygotowanym stanowisku", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("xelatex")), "Brak XeLaTeX")
  expect_true(badaniaZI:::sprawdz_sklad_latex())
})
