# Instalacja tak jak u studenta: install.packages() z repozytorium pakietu kursu
# (tu lokalnego, zbudowanego z bieżących źródeł) i zależności z CRAN.
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
repo <- file.path(tempfile("repo-kursu-"), "src", "contrib")
dir.create(repo, recursive = TRUE)
wynik <- system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(root)),
                 stdout = TRUE, stderr = TRUE)
paczka <- list.files(".", "^badaniaZI_.*[.]tar[.]gz$")
if (length(paczka) != 1L) stop("Nie zbudowano paczki:\n", paste(wynik, collapse = "\n"))
stopifnot(file.rename(paczka, file.path(repo, paczka)))
tools::write_PACKAGES(repo, type = "source")
biblioteka <- tempfile("biblioteka-")
dir.create(biblioteka)
adres <- paste0("file:///", sub("^/", "", gsub("\\\\", "/", dirname(dirname(repo)))))
install.packages("badaniaZI", lib = biblioteka, repos = c(adres, getOption("repos")), type = "source")
.libPaths(c(biblioteka, .libPaths()))
library(badaniaZI)
stopifnot(startsWith(normalizePath(find.package("badaniaZI"), winslash = "/"), normalizePath(biblioteka, winslash = "/")),
          identical(as.character(packageVersion("badaniaZI")), read.dcf("DESCRIPTION", "Version")[[1]]))
x <- generuj_dane("instalacja017", "S03")
stopifnot(nrow(x$dane) == 150L, nrow(scenariusze()) == 20L)
k <- tempfile("projekt lokalny ze spacjami-")
utworz_projekt("instalacja017", katalog = k, student = "Test Instalacji")
stopifnot(file.exists(file.path(k, "moje-badania.Rproj")))
stopifnot(!dir.exists(file.path(k, "dane")), !dir.exists(file.path(k, "projekty")))
for (id in sprintf("Z%02d", 1:9)) {
  p <- przygotuj_zadanie(id, k)
  txt <- readLines(p, encoding = "UTF-8")
  stopifnot(basename(p) == "zadanie.Rmd",
    sum(grepl('^<!-- odpowiedz:S0[1-5] -->$', txt)) == 5L,
    any(grepl(sprintf('rozpocznij_zajecia("C%s", id_studenta = "TWOJE_ID"', substring(id, 2)), txt, fixed = TRUE)),
    !any(grepl("GitHub|zaloguj_", txt)),
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
  } else {
    stopifnot(inherits(try(otworz_material(id, handout = TRUE, otworz = FALSE), silent = TRUE), "try-error"))
  }
}
stopifnot(nrow(sprawdz_konfiguracje(konfiguracja_kursu())[sprawdz_konfiguracje(konfiguracja_kursu())$waga == "blad", ]) == 0L)
stopifnot(oblicz_ocene(rep(8, 10), 90)$procent == 85)
unlink(k, recursive = TRUE)
cat("Instalacja z repozytorium pakietu, jeden Rmd, start każdego spotkania, stały wariant i materiały offline: OK.\n")
