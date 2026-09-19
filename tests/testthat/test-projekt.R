test_that("projekt ma indywidualne dane, konkretne szablony i nie nadpisuje pracy", {
  katalog <- tempfile("moje badania ")
  on.exit(unlink(katalog, recursive = TRUE))
  p <- utworz_projekt("s017", "S03", katalog, repo = "uczelnie/moje-badania-s017")
  expect_true(file.exists(p))
  cfg <- badaniaZI:::czytaj_yaml(file.path(katalog, "kurs.yml"))
  expect_equal(cfg$id, "s017")
  expect_equal(cfg$scenariusz, "S03")
  expect_equal(cfg$repo, "uczelnie/moje-badania-s017")
  for (plik in c("raport.md", "kwestionariusz.md", "analiza.R")) {
    tekst <- readLines(file.path(katalog, "projekty", "ilosciowy", plik), encoding = "UTF-8")
    expect_false(any(grepl("{{", tekst, fixed = TRUE)))
    expect_true(any(grepl("s017", tekst, fixed = TRUE)))
  }
  expect_error(utworz_projekt("s017", "S03", katalog), "istnieje")
  expect_true(file.exists(file.path(katalog, "dane", "manifest.json")))
  expect_false(dir.exists(file.path(katalog, ".github")))
})
