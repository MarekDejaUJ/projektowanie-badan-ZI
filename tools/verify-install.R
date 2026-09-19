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
utworz_projekt("instalacja017", "S03", k)
stopifnot(file.exists(file.path(k, "moje-badania.Rproj")))
katalog_materialow <- materialy()
for (id in katalog_materialow$id) {
  for (f in c("html", "pdf", "Rmd"))
    stopifnot(file.exists(otworz_material(id, format = f, otworz = FALSE)))
  if (katalog_materialow$typ[katalog_materialow$id == id] == "wyklad") {
    for (f in c("html", "pdf", "Rmd", "tex"))
      stopifnot(file.exists(otworz_material(id, format = f, handout = TRUE, otworz = FALSE)))
  } else {
    stopifnot(file.exists(otworz_material(id, format = "R", otworz = FALSE)))
    stopifnot(inherits(try(otworz_material(id, handout = TRUE, otworz = FALSE), silent = TRUE), "try-error"))
  }
}
stopifnot(oblicz_ocene(rep(8, 10), 90)$procent == 85)
unlink(k, recursive = TRUE)
cat("Instalacja z konkretnego SHA, dane, projekt i materiały offline: OK.\n")
