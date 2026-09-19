out <- "_site"
if (dir.exists(out)) unlink(out, recursive = TRUE)
dir.create(out)

manifest_text <- readLines("inst/materialy/manifest.yml", encoding = "UTF-8", warn = FALSE)
manifest <- yaml::yaml.load(paste(manifest_text, collapse = "\n"))$jednostki
stopifnot(length(manifest) == 15L)

copy_tree <- function(from, to) {
  files <- list.files(from, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  for (src in files) {
    rel <- substring(src, nchar(from) + 2L)
    dst <- file.path(to, rel)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(src, dst, overwrite = TRUE, copy.date = TRUE))
      stop("Nie można skopiować zasobu strony: ", src)
  }
}

copy_tree("inst/materialy", file.path(out, "materialy"))
copy_tree("inst/rubryki", file.path(out, "rubryki"))
copy_tree("inst/scenariusze", file.path(out, "scenariusze"))
copy_tree("inst/szablony", file.path(out, "szablony"))
file.copy("README.md", file.path(out, "README.md"), overwrite = TRUE)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

card <- function(x) {
  id <- x$id
  unit <- tolower(id)
  if (x$typ == "cwiczenie") {
    links <- c("HTML" = paste0("materialy/", unit, "/pelne.html"),
               "PDF" = paste0("materialy/", unit, "/pelne.pdf"),
               "Rmd" = paste0("materialy/", unit, "/pelne.Rmd"),
               "gotowy R" = paste0("materialy/", unit, "/analiza.R"),
               "rubryka" = paste0("rubryki/Z", substring(id, 2), ".md"))
  } else {
    links <- c("wykład HTML" = paste0("materialy/", unit, "/pelne.html"),
               "wykład PDF" = paste0("materialy/", unit, "/pelne.pdf"),
               "handout HTML" = paste0("materialy/", unit, "/handout.html"),
               "handout PDF" = paste0("materialy/", unit, "/handout.pdf"),
               "źródło Rmd" = paste0("materialy/", unit, "/pelne.Rmd"))
  }
  hrefs <- paste(sprintf('<a href="%s">%s</a>', unname(links), names(links)), collapse = " · ")
  sprintf('<article><h3>%s — %s</h3><p>%s</p></article>', id, esc(x$tytul), hrefs)
}

lectures <- manifest[vapply(manifest, function(x) x$typ == "wyklad", logical(1))]
exercises <- manifest[vapply(manifest, function(x) x$typ == "cwiczenie", logical(1))]
scenario_links <- paste(sprintf('<a href="scenariusze/S%02d.md">S%02d</a>', 1:20, 1:20), collapse = " · ")
rubric_links <- paste(c(sprintf('<a href="rubryki/Z%02d.md">Z%02d</a>', 1:10, 1:10),
                        '<a href="rubryki/PROJEKT.md">projekt</a>'), collapse = " · ")

html <- c('<!doctype html>', '<html lang="pl"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Projektowanie badań — Zarządzanie informacją</title>',
  '<style>body{font-family:system-ui,sans-serif;line-height:1.55;color:#1f2933;max-width:1050px;margin:auto;padding:2rem}h1,h2,h3{color:#005c91}article{border:1px solid #cbd8df;border-radius:.5rem;padding:.8rem 1rem;margin:.8rem 0;background:#f8fbfc}code,pre{background:#eef4f8}pre{padding:1rem;overflow:auto}a{color:#005c91}aside{border-left:4px solid #d55e00;padding:.6rem 1rem;background:#fff7ef}</style>',
  '</head><body>', '<h1>Projektowanie badań — część ilościowa</h1>',
  '<p>Zarządzanie informacją, rok 2026/27. Materiały łączą ankietę z obserwowanym zadaniem wyszukiwawczym. Kod analiz jest gotowy; praca studenta polega na wyborze wskazanych parametrów oraz samodzielnej interpretacji.</p>',
  '<aside><strong>Terminy:</strong> Z01–Z09 do początku kolejnych ćwiczeń; Z10 do 26.01.2027, 10:30. Projekt do 27.01.2027. Spóźnione oddanie do 10.02.2027 ma ocenę maksymalną 4,5; poprawa projektu do 24.02.2027.</aside>',
  '<h2>Szybki start w RStudio</h2>',
  '<pre><code>library(badaniaZI)\nsprawdz_srodowisko()\nzaloguj_github()\nrozpocznij_zajecia("C01", "s017",\n  "https://github.com/MarekDejaUJ/ZI-s017.git")</code></pre>',
  '<p>Hasło i drugi składnik wpisuje się wyłącznie na stronie GitHub. Po pracy użyj <code>sprawdz_zadanie()</code>, <code>oddaj_zadanie()</code>, <code>status_oddania()</code> oraz <code>wyloguj_github()</code>.</p>',
  '<h2>Wykłady i handouty</h2>', vapply(lectures, card, character(1)),
  '<h2>Ćwiczenia i gotowe skrypty</h2>', vapply(exercises, card, character(1)),
  '<h2>Projekt indywidualny</h2>',
  '<p><a href="szablony/projekt/raport.md">szablon raportu</a> · <a href="szablony/projekt/kwestionariusz.md">plan narzędzia</a> · <a href="szablony/projekt/analiza.R">gotowa analiza</a> · <a href="rubryki/PROJEKT.md">rubryka</a></p>',
  paste0('<p><strong>Scenariusze:</strong> ', scenario_links, '</p>'),
  paste0('<p><strong>Rubryki zadań:</strong> ', rubric_links, '</p>'),
  '<h2>Instalacja na własnym komputerze</h2>',
  '<pre><code>install.packages("remotes")\nremotes::install_github("MarekDejaUJ/projektowanie-badan-ZI",\n  dependencies = NA, upgrade = "never")</code></pre>',
  '<p>W sali pakiet jest instalowany centralnie. Gotowe PDF i HTML działają również lokalnie z pakietu.</p>',
  '</body></html>')
writeLines(html, file.path(out, "index.html"), useBytes = TRUE)

required <- unlist(lapply(manifest, function(x) {
  base <- file.path(out, "materialy", tolower(x$id))
  if (x$typ == "cwiczenie") file.path(base, c("pelne.html", "pelne.pdf", "pelne.Rmd", "analiza.R"))
  else file.path(base, c("pelne.html", "pelne.pdf", "pelne.Rmd", "handout.html", "handout.pdf", "handout.Rmd", "handout.tex"))
}))
stopifnot(all(file.exists(required)), length(list.files(file.path(out, "scenariusze"), "^S[0-9]{2}[.]md$")) == 20L)
cat("Strona gotowa:", normalizePath(out, winslash = "/"), "\n")
