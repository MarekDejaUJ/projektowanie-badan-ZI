library(badaniaZI)
image <- Sys.getenv("BADANIAZI_REVIEW_IMAGE")
stopifnot(grepl("^sha256:[0-9a-f]{64}$", image))
run <- function() {
  root <- tempfile("test-kontroli-")
  stopifnot(dir.create(root))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  k <- file.path(root, "praca")
  utworz_projekt("test017", katalog = k, repo = "test/praca")
  for (id in c("Z01", "Z09", "Z10")) {
    rmd <- przygotuj_zadanie(id, k)
    if (id == "Z09") wariant_projektu("Z09", "S02", katalog = k)
    t <- readLines(rmd, encoding = "UTF-8")
    for (s in sprintf("S%02d", 1:5)) t <- badaniaZI:::wstaw_odpowiedz(t, s,
      paste("Test techniczny", s, "— nie jest to przykład ocenionej odpowiedzi. Czas nie zastępuje poprawności."))
    badaniaZI:::pisz_linie(t, rmd)
  }
  rmd <- przygotuj_raport(k)
  t <- badaniaZI:::wstaw_odpowiedz(readLines(rmd, encoding = "UTF-8"), "P11", "Test techniczny. Źródła: materiały kursu.")
  badaniaZI:::pisz_linie(t, rmd)
  make_input <- function(id, label) {
    inp <- file.path(root, label)
    dir.create(file.path(inp, "wejscie"), recursive = TRUE)
    paths <- badaniaZI:::pliki_wejscia_pracy(id)
    for (p in paths) {
      dest <- file.path(inp, "wejscie", p)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      stopifnot(file.copy(file.path(k, p), dest))
    }
    # Celowo nie ufamy gotowemu PDF/metadanym: kontrola musi odtworzyć wynik.
    for (p in badaniaZI:::pliki_pdf_pracy(id)) writeLines("Podmieniony wynik testowy", file.path(inp, "wejscie", p))
    update_proof(inp, id)
    inp
  }
  update_proof <- function(inp, id) {
    paths <- c(badaniaZI:::pliki_wejscia_pracy(id), badaniaZI:::pliki_pdf_pracy(id))
    d <- list(format = "odbior-kontrolny-1", sha = strrep("a", 40), repo = "test/praca",
      zadanie = id, id_studenta = "test017", pliki = as.list(badaniaZI:::hashe_plikow(file.path(inp, "wejscie"), paths)))
    badaniaZI:::pisz_linie(jsonlite::toJSON(d, auto_unbox = TRUE), file.path(inp, "odbior.json"))
  }
  diagnose_synthetic <- function(inp) {
    # Tylko lokalna fikstura tego testu: nigdy pobrane prywatne oddanie.
    args0 <- badaniaZI:::argumenty_kontenera
    code <- paste0("source('/opt/badaniazi/kontrola.R', encoding='UTF-8'); ",
      "w <- badaniaZI:::kontroluj_wejscia_rmd(d$zadanie, '/work/praca'); ",
      "r <- badaniaZI:::uruchom_render_pracy(file.path(badaniaZI:::folder_pracy(d$zadanie), w$rmd), '/work/praca', 240); ",
      "cat('\\nDIAGNOSTYKA SYNTETYCZNA\\n', r$status, '\\n', r$stdout, '\\n', r$stderr)")
    testthat::with_mocked_bindings({
      z <- badaniaZI:::uruchom_kontrole_kontener(inp, image, 360)
      cat(substr(z$stdout, 1L, 20000L), "\n")
    }, argumenty_kontenera = function(wejscie, obraz) {
      c(head(args0(wejscie, obraz), -1L), "-e", code)
    }, .package = "badaniaZI")
  }
  for (id in c("Z01", "Z09", "Z10", "PROJEKT")) {
    inp <- make_input(id, id)
    d <- badaniaZI:::czytaj_odbior_kontrolny(inp)
    x <- sprawdz_oddanie(inp, image)
    if (!isTRUE(x$ok)) {
      print(x$raport)
      diagnose_synthetic(inp)
    }
    stopifnot(x$ok, identical(d, badaniaZI:::czytaj_odbior_kontrolny(inp)),
      file.info(file.path(x$katalog, "kontrola.pdf"))$size > 1000)
    cat(id, "odtworzenie kontenerowe i niezmienne oryginały PASS\n")
  }
  inp <- make_input("Z01", "niedozwolony-kod")
  p <- file.path(inp, "wejscie/zadania/z01/analiza.R")
  cat("\nstop('Nie wykonuj zmienionego pomocnika')\n", file = p, append = TRUE)
  update_proof(inp, "Z01")
  stopifnot(!sprawdz_oddanie(inp, image)$ok)
  cat("Zmieniony kod odrzucony PASS\n")
  # Sprawdzenie ograniczeń na tym samym obrazie i z tymi samymi flagami.
  args0 <- badaniaZI:::argumenty_kontenera
  probe <- paste0("stopifnot(all(Sys.getenv(c('GH_TOKEN','GITHUB_TOKEN','GITHUB_PAT'))==''), ",
    "!file.exists('/var/run/docker.sock'), identical(list.files('/sys/class/net'),'lo')); ",
    "s<-readLines('/proc/self/status'); stopifnot(any(grepl('^Uid:([[:space:]]+65534){4}$',s)), ",
    "any(grepl('^NoNewPrivs:[[:space:]]+1$',s)), ",
    "any(grepl('^CapEff:[[:space:]]+0+$',s))); ",
    "stopifnot(!suppressWarnings(file.create('/etc/test-zapisu')), ",
    "!suppressWarnings(file.create('/input/test-zapisu'))); cat('IZOLACJA PASS')")
  testthat::with_mocked_bindings({
    z <- withr::with_envvar(c(GH_TOKEN = "test-secret", GITHUB_TOKEN = "test-secret", GITHUB_PAT = "test-secret"),
      badaniaZI:::uruchom_kontrole_kontener(inp, image, 30))
    stopifnot(z$status == 0L, identical(z$stdout, "IZOLACJA PASS"))
  }, argumenty_kontenera = function(wejscie, obraz) {
    a <- args0(wejscie, obraz)
    c(head(a, -1L), "-e", probe)
  }, .package = "badaniaZI")
  cat("Brak sekretów, sieci, roota, capabilities, socketu i zapisu do wejścia/hosta PASS\n")
  created <- character()
  run0 <- processx::run
  start <- proc.time()[["elapsed"]]
  testthat::with_mocked_bindings({
    testthat::with_mocked_bindings({
      testthat::expect_error(badaniaZI:::uruchom_kontrole_kontener(inp, image, 1), "limit czasu")
    }, argumenty_kontenera = function(wejscie, obraz) {
      c(head(args0(wejscie, obraz), -1L), "-e", "Sys.sleep(60)")
    }, .package = "badaniaZI")
  }, run = function(command, args, ...) {
    z <- run0(command, args, ...)
    if (identical(command, "docker") && identical(args[1L], "create") && z$status == 0L)
      created <<- c(created, trimws(z$stdout))
    z
  }, .package = "processx")
  stopifnot(length(created) == 1L, grepl("^[0-9a-f]{64}$", created),
    proc.time()[["elapsed"]] - start < 20)
  z <- run0("docker", c("container", "ls", "--all", "--no-trunc", "--filter",
    paste0("id=", created), "--format", "{{.ID}}"), timeout = 30)
  stopifnot(!nzchar(trimws(z$stdout)))
  cat("Limit czasu zatrzymał proces i usunął wyłącznie własny kontener PASS\n")
}
run()
