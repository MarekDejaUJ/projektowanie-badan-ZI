test_that("argumenty kontenera nie przekazują sekretów ani zapisu do hosta", {
  k <- tempfile("wejscie-")
  dir.create(k)
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  image <- paste0("sha256:", strrep("a", 64))
  a <- badaniaZI:::argumenty_kontenera(k, image)
  expect_true(all(c("--network=none", "--read-only", "--cap-drop=ALL",
    "--security-opt=no-new-privileges", "--user=65534:65534", "--memory=2g",
    "--pids-limit=128", "--log-driver=none") %in% a))
  expect_equal(sum(a == "--mount"), 1L)
  expect_match(a[match("--mount", a) + 1L], ",target=/input,readonly$", fixed = FALSE)
  expect_false(any(grepl("TOKEN|PAT|--privileged|docker[.]sock|--env|--volume", a)))
  expect_identical(tail(a, 3L), c(image, "--vanilla", "/opt/badaniazi/kontrola.R"))
  expect_error(sprawdz_oddanie(k, "obraz:latest"), "obraz")
  expect_error(sprawdz_oddanie(k, image, timeout = Inf), "Limit")
})

test_that("wynik kontenera jest oddzielny i nie nadpisuje oryginalnego PDF", {
  k <- tempfile("kontrola-")
  dir.create(k)
  on.exit(unlink(k, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(k, "wejscie"))
  paths <- c(badaniaZI:::pliki_wejscia_pracy("Z01"), badaniaZI:::pliki_pdf_pracy("Z01"))
  for (p in paths) {
    dest <- file.path(k, "wejscie", p)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeLines("Pobrana praca", dest)
  }
  proof <- list(format = "odbior-kontrolny-1", sha = strrep("b", 40), zadanie = "Z01",
    id_studenta = "s017", repo = "owner/praca", pliki = as.list(badaniaZI:::hashe_plikow(file.path(k, "wejscie"), paths)))
  badaniaZI:::pisz_linie(jsonlite::toJSON(proof, auto_unbox = TRUE), file.path(k, "odbior.json"))
  expect_identical(badaniaZI:::czytaj_odbior_kontrolny(k), proof)
  local_mocked_bindings(Sys.which = function(...) c(docker = "docker"), .package = "base")
  bytes <- charToRaw(paste0("%PDF-", strrep("TEST", 100)))
  result <- list(format = "kontrola-oddania-1", sha = proof$sha, zadanie = "Z01",
    id_studenta = "s017", ok = TRUE, pdf_base64 = jsonlite::base64_enc(bytes),
    sha256_pdf = digest::digest(bytes, serialize = FALSE, algo = "sha256"))
  local_mocked_bindings(uruchom_kontrole_kontener = function(wejscie, obraz, timeout) {
    expect_identical(badaniaZI:::czytaj_odbior_kontrolny(wejscie), proof)
    list(status = 0L, stdout = jsonlite::toJSON(result, auto_unbox = TRUE))
  }, .package = "badaniaZI")
  got <- sprawdz_oddanie(k, paste0("sha256:", strrep("a", 64)))
  expect_true(got$ok)
  expect_true(file.exists(file.path(got$katalog, "kontrola.pdf")))
  expect_identical(badaniaZI:::czytaj_odbior_kontrolny(k), proof)
  expect_false("pdf_base64" %in% names(got$raport))
  result$sha <- strrep("c", 40)
  expect_error(sprawdz_oddanie(k, paste0("sha256:", strrep("a", 64))), "poprawnego wyniku")
})
