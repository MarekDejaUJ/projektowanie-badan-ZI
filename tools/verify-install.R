repo <- Sys.getenv("GITHUB_REPOSITORY", "MarekDejaUJ/projektowanie-badan-ZI")
sha <- Sys.getenv("GITHUB_SHA")
if (!grepl("^[0-9a-f]{40}$", sha)) stop("Podaj GITHUB_SHA sprawdzanego wydania.")
biblioteka <- tempfile("biblioteka-")
dir.create(biblioteka)
remotes::install_github(paste0(repo, "@", sha), lib = biblioteka,
                        dependencies = NA, upgrade = "never", build_vignettes = FALSE)
.libPaths(c(biblioteka, .libPaths()))
library(badaniaZI)
stopifnot(startsWith(find.package("badaniaZI"), normalizePath(biblioteka, winslash = "/")))
x <- generuj_dane("instalacja017", "S03")
stopifnot(nrow(x$dane) == 150L, nrow(scenariusze()) == 20L)
k <- tempfile("projekt lokalny ze spacjami-")
utworz_projekt("instalacja017", katalog = k)
stopifnot(file.exists(file.path(k, "moje-badania.Rproj")))
stopifnot(!dir.exists(file.path(k, "dane")), !dir.exists(file.path(k, "projekty")))
for (id in sprintf("Z%02d", 1:9)) {
  p <- przygotuj_zadanie(id, k)
  txt <- readLines(p, encoding = "UTF-8")
  stopifnot(basename(p) == "zadanie.Rmd",
    sum(grepl('^<!-- odpowiedz:S0[1-5] -->$', txt)) == 5L,
    any(grepl('ID <- "TWOJE_ID"', txt, fixed = TRUE)),
    any(grepl('zaloguj_github()', txt, fixed = TRUE)),
    file.exists(file.path(dirname(p), "analiza.R")),
    !file.exists(file.path(dirname(p), "odpowiedzi.md")))
}
a <- wariant_projektu("Z09", "S03", katalog = k)
przygotuj_zadanie("Z10", k)
b <- wariant_projektu("Z10", katalog = k)
stopifnot(identical(a, b))
raport <- przygotuj_raport(k)
stopifnot(file.exists(raport), basename(raport) == "raport.Rmd",
  sum(grepl('^<!-- odpowiedz:P[0-9]{2} -->$', readLines(raport, encoding = "UTF-8"))) == 11L)
katalog_materialow <- materialy()
for (id in katalog_materialow$id) {
  for (f in c("html", "pdf", "Rmd"))
    stopifnot(file.exists(otworz_material(id, format = f, otworz = FALSE)))
  if (katalog_materialow$typ[katalog_materialow$id == id] == "wyklad") {
    for (f in c("html", "pdf", "Rmd", "tex"))
      stopifnot(file.exists(otworz_material(id, format = f, handout = TRUE, otworz = FALSE)))
    if (id != "W01") stopifnot(file.exists(otworz_material(id, format = "R", otworz = FALSE)))
  } else {
    stopifnot(file.exists(otworz_material(id, format = "R", otworz = FALSE)))
    stopifnot(inherits(try(otworz_material(id, handout = TRUE, otworz = FALSE), silent = TRUE), "try-error"))
  }
}
stopifnot(oblicz_ocene(rep(8, 10), 90)$procent == 85)
unlink(k, recursive = TRUE)
cat("Instalacja z konkretnego SHA, jeden Rmd, start każdego spotkania, stały wariant i materiały offline: OK.\n")
