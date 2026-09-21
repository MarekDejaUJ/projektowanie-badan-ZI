# Wyłącznie wejście z prywatnej kopii, wewnątrz kontenera bez sieci i sekretów.
library(badaniaZI)
d <- badaniaZI:::czytaj_odbior_kontrolny("/input")
wynik <- list(format = "kontrola-oddania-1", sha = d$sha, zadanie = d$zadanie,
              id_studenta = d$id_studenta, ok = FALSE)
wynik <- tryCatch({
  dir.create("/work/praca")
  for (p in names(d$pliki)) {
    cel <- file.path("/work/praca", p)
    dir.create(dirname(cel), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path("/input/wejscie", p), cel))
    Sys.chmod(cel, "0600")
  }
  cfg <- badaniaZI:::czytaj_yaml("/work/praca/kurs.yml")
  stopifnot(identical(cfg$id, d$id_studenta), identical(cfg$repo, d$repo))
  # Oryginalny PDF jest dowodem. Nie uznajemy edytowalnej metryki za render.
  kontrola <- sprawdz_zadanie(d$zadanie, "/work/praca", uruchom = TRUE)
  wynik$ok <- isTRUE(kontrola$ok)
  wynik$kontrole <- kontrola$kontrole
  wynik$pakiet <- as.character(packageVersion("badaniaZI"))
  wynik$R <- as.character(getRversion())
  if (wynik$ok) {
    pdf <- file.path("/work/praca", badaniaZI:::pliki_pdf_pracy(d$zadanie)[1L])
    stopifnot(file.info(pdf)$size <= 10e6)
    bajty <- readBin(pdf, "raw", n = file.info(pdf)$size)
    wynik$sha256_pdf <- digest::digest(bajty, algo = "sha256", serialize = FALSE)
    wynik$pdf_base64 <- jsonlite::base64_enc(bajty)
    wynik$interpretacja <- "wymaga oceny merytorycznej"
  }
  wynik
}, error = function(e) {
  wynik$blad <- "Oddanie nie przeszło kontroli struktury, tożsamości lub odtworzenia. Sprawdź Rmd i wersję narzędzi."
  wynik
})
cat(jsonlite::toJSON(wynik, auto_unbox = TRUE, dataframe = "rows", null = "null"))
