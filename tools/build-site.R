root <- normalizePath(".", winslash = "/", mustWork = TRUE)
stopifnot(identical(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[[1]], "badaniaZI"))
wydanie <- yaml::read_yaml(file.path(root, "inst/kurs/wydanie.yml"))
stopifnot(identical(wydanie$pakiet, read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[[1]]),
  all(vapply(wydanie[c("pakiet", "materialy", "generator")], function(x)
    is.character(x) && length(x) == 1L && grepl("^[0-9]+[.][0-9]+[.][0-9]+$", x), logical(1))))
# Budujemy obok istniejącej strony; dopiero kompletny wynik zastępuje _site.
out <- tempfile(".site-stage-", tmpdir = root)
stopifnot(dir.create(out))

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
stopifnot(file.copy("NEWS.md", file.path(out, "NEWS.md")))

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
               "rubryka" = paste0("rubryki/Z", substring(id, 2), ".html"))
  } else {
    links <- c("wykład HTML" = paste0("materialy/", unit, "/pelne.html"),
               "wykład PDF" = paste0("materialy/", unit, "/pelne.pdf"),
               "handout HTML" = paste0("materialy/", unit, "/handout.html"),
               "handout PDF" = paste0("materialy/", unit, "/handout.pdf"),
               "źródło Rmd" = paste0("materialy/", unit, "/pelne.Rmd"))
    if (file.exists(file.path("inst/materialy", unit, "analiza.R")))
      links <- c(links, "gotowy R" = paste0("materialy/", unit, "/analiza.R"))
  }
  hrefs <- paste(sprintf('<a href="%s">%s</a>', unname(links), names(links)), collapse = " · ")
  sprintf('<article><h3>%s — %s</h3><p>%s</p></article>', id, esc(x$tytul), hrefs)
}

lectures <- manifest[vapply(manifest, function(x) x$typ == "wyklad", logical(1))]
exercises <- manifest[vapply(manifest, function(x) x$typ == "cwiczenie", logical(1))]
scenario_links <- paste(sprintf('<a href="scenariusze/S%02d.html">S%02d</a> (<a href="scenariusze/S%02d.pdf">PDF</a>)', 1:20, 1:20, 1:20), collapse = " · ")
rubric_links <- paste(c(sprintf('<a href="rubryki/Z%02d.html">Z%02d</a>', 1:10, 1:10),
                        '<a href="rubryki/PROJEKT.html">projekt</a>'), collapse = " · ")

html <- c('<!doctype html>', '<html lang="pl"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Projektowanie badań — Zarządzanie informacją</title>',
  '<style>body{font-family:system-ui,sans-serif;line-height:1.55;color:#1f2933;max-width:1050px;margin:auto;padding:2rem}h1,h2,h3{color:#005c91}article{border:1px solid #cbd8df;border-radius:.5rem;padding:.8rem 1rem;margin:.8rem 0;background:#f8fbfc}code,pre{background:#eef4f8}pre{padding:1rem;overflow:auto}a{color:#005c91}aside{border-left:4px solid #d55e00;padding:.6rem 1rem;background:#fff7ef}</style>',
  '</head><body>', '<h1>Projektowanie badań — część ilościowa</h1>',
  sprintf('<p>Materiały %s · pakiet %s · generator %s. <a href="NEWS.md">Opis zmian i status wydania</a>.</p>',
    esc(wydanie$materialy), esc(wydanie$pakiet), esc(wydanie$generator)),
  '<p>Zarządzanie informacją, rok 2026/27. Materiały łączą ankietę z obserwowanym zadaniem wyszukiwawczym. Kod analiz jest gotowy; praca studenta polega na wyborze wskazanych parametrów oraz samodzielnej interpretacji.</p>',
  '<aside><strong>Terminy:</strong> Z01–Z09 do początku kolejnych ćwiczeń; Z10 do 26.01.2027, 10:30. Projekt do 27.01.2027. Spóźnione oddanie do 10.02.2027 ma ocenę maksymalną 4,5; poprawa projektu do 24.02.2027.</aside>',
  '<h2>Początek każdych ćwiczeń w RStudio</h2>',
  '<pre><code>library(badaniaZI)\nID &lt;- "TWOJE_ID"\nzaloguj_github()\nrozpocznij_zajecia(\n  "C01", id_studenta = ID,\n  repo_url = "ADRES_TWOJEGO_PRYWATNEGO_REPOZYTORIUM"\n)</code></pre>',
  '<p>Wpisz swoje ID, otrzymany adres i numer bieżących ćwiczeń. Pełna instrukcja powtarza się w każdym zestawie. Hasło i drugi składnik wpisuje się wyłącznie na stronie GitHub. Funkcja otwiera własny <code>zadanie.Rmd</code>; nie przełączaj potem projektu przez <code>.Rproj</code>, ponieważ utracisz bieżącą sesję i logowanie.</p>',
  '<p>Uruchom pierwszy chunk i kolejne gotowe analizy. Pięć części LEARN pokazuje, jak odczytywać wyniki; w pięciu CHALLENGE piszesz samodzielne akapity w oznaczonych miejscach Rmd, poza kodem. Pracujesz indywidualnie; w razie trudności pytasz prowadzącego. Orientacyjnie: 5 minut startu, 30 LEARN, 50 CHALLENGE i 5 oddania.</p>',
  '<p>Zapisz Rmd (Ctrl+S; macOS Cmd+S), następnie wykonaj jedno polecenie:</p>',
  '<pre><code>oddaj_zadanie("Z01")</code></pre>',
  '<p>Funkcja sprawdza odpowiedzi, tworzy aktualny PDF i wysyła komplet do prywatnego repozytorium. Prowadzący otrzymuje PDF z wynikami i twoimi akapitami oraz źródła do odtworzenia pracy. Potwierdzenie zawiera zdalne SHA. Sam PDF nie oznacza udanej wysyłki: przy błędzie zachowaj lokalną pracę. <code>status_oddania("Z01")</code> i <code>pobierz_ocene("Z01")</code> służą do późniejszego sprawdzenia odbioru i oceny. Przed wyjściem wykonaj <code>wyloguj_github()</code> i wyloguj przeglądarkę.</p>',
  '<h2>Wykłady i handouty</h2>', vapply(lectures, card, character(1)),
  '<h2>Ćwiczenia i gotowe skrypty</h2>', vapply(exercises, card, character(1)),
  '<h2>Projekt indywidualny</h2>',
  '<p>C01–C08 korzystają z podanych scenariuszy. Projekt zaczyna się na C09: wybierasz scenariusz, zapisujesz syntetyczny wariant i plan. C10 odczytuje te same dane. Po obu zadaniach <code>przygotuj_raport()</code> jednorazowo przenosi dziesięć własnych odpowiedzi do <code>projekty/ilosciowy/raport.Rmd</code>. Otwórz ten plik, uporządkuj tekst, uzupełnij P11 z bibliografią i zakresem wsparcia. Po zapisaniu użyj <code>oddaj_projekt()</code>. Nie generuj nowego wariantu. Poniższe źródła są do wglądu; własny raport przygotowuje funkcja z zachowanych Z09/Z10.</p>',
  '<p><a href="szablony/projekt/raport.Rmd">raport Rmd z planem pomiaru</a> · <a href="szablony/projekt/analiza.R">gotowy silnik analiz</a> · <a href="rubryki/PROJEKT.html">rubryka HTML</a> · <a href="rubryki/PROJEKT.pdf">rubryka PDF</a></p>',
  paste0('<p><strong>Scenariusze:</strong> ', scenario_links, '</p>'),
  paste0('<p><strong>Rubryki zadań:</strong> ', rubric_links, '</p>'),
  '<h2>Instalacja na własnym komputerze</h2>',
  '<pre><code>install.packages("remotes")\nremotes::install_github("MarekDejaUJ/projektowanie-badan-ZI",\n  dependencies = NA, upgrade = "never")</code></pre>',
  '<p>W sali pakiet jest instalowany centralnie. Do własnego PDF przy oddawaniu potrzebne są także Pandoc i XeLaTeX, przygotowane przez informatyka; wysyłka ich nie instaluje. <code>sprawdz_srodowisko()</code> pokazuje dostępne narzędzia. Czytanie gotowych PDF i HTML nie wymaga LaTeX. Instaluj wersję wskazaną dla rocznika; powyższe polecenie pobiera bieżącą gałąź repozytorium. Nie aktualizuj pakietu w trakcie zadania.</p>',
  '</body></html>')
writeLines(html, file.path(out, "index.html"), useBytes = TRUE)

required <- unlist(lapply(manifest, function(x) {
  base <- file.path(out, "materialy", tolower(x$id))
  if (x$typ == "cwiczenie") file.path(base, c("pelne.html", "pelne.pdf", "pelne.Rmd", "analiza.R"))
  else file.path(base, c("pelne.html", "pelne.pdf", "pelne.Rmd", "handout.html", "handout.pdf", "handout.Rmd", "handout.tex"))
}))
stopifnot(all(file.exists(required)), length(list.files(file.path(out, "scenariusze"), "^S[0-9]{2}[.]md$")) == 20L)
href <- regmatches(html, gregexpr('href="[^"]+"', html))
href <- sub('^href="(.*)"$', '\\1', unlist(href))
stopifnot(all(file.exists(file.path(out, href))))
target <- file.path(root, "_site")
backup <- NULL
if (file.exists(target)) {
  resolved <- normalizePath(target, winslash = "/", mustWork = TRUE)
  stopifnot(dir.exists(target), !nzchar(Sys.readlink(target)),
            identical(resolved, target), identical(dirname(resolved), root))
  backup <- tempfile(".site-previous-", tmpdir = root)
  stopifnot(identical(dirname(backup), root), !file.exists(backup))
  if (!file.rename(target, backup)) stop("Nie można zachować poprzedniej strony; nowy wynik pozostał w ", out)
}
if (!file.rename(out, target)) {
  if (!is.null(backup)) file.rename(backup, target)
  stop("Nie można opublikować lokalnego wyniku; zachowano źródła i poprzednią stronę.")
}
if (!is.null(backup)) cat("Poprzednia lokalna strona:", backup, "\n")
cat("Strona gotowa:", normalizePath(target, winslash = "/"), "\n")
