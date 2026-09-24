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
  '<h2>Przed pierwszymi zajęciami</h2>',
  '<ol><li>Załóż konto GitHub i włącz uwierzytelnianie dwuskładnikowe (2FA).</li>',
  '<li>Przekaż prowadzącemu swój login GitHub. Prowadzący przydziela pseudonimowe ID (np. <code>s017</code>) i prywatne repozytorium; przyjmij zaproszenie z e-maila od GitHub albo ze strony powiadomień GitHub.</li>',
  '<li>Zapisz swoje ID i adres repozytorium: wpisujesz je na początku każdych zajęć.</li>',
  '<li>W sali wszystkie programy są zainstalowane. Na własnym komputerze zainstaluj je według sekcji „Własny komputer” na końcu strony.</li></ol>',
  '<h2>Przebieg zajęć</h2>',
  '<ol><li><strong>Start.</strong> W RStudio, w konsoli (dolny panel ze znakiem <code>&gt;</code>), wpisz cztery polecenia z początku materiału ćwiczeń: swoje ID, adres repozytorium i numer bieżących ćwiczeń.',
  '<pre><code>library(badaniaZI)\nID &lt;- "TWOJE_ID"\nzaloguj_github()\nrozpocznij_zajecia(\n  "C01", id_studenta = ID,\n  repo_url = "ADRES_TWOJEGO_PRYWATNEGO_REPOZYTORIUM"\n)</code></pre></li>',
  '<li><strong>Logowanie.</strong> W konsoli pojawia się jednorazowy kod, a przeglądarka otwiera stronę GitHub. Hasło, drugi składnik i kod wpisujesz wyłącznie na stronie GitHub, nigdy w konsoli R.</li>',
  '<li><strong>Praca.</strong> Plik <code>zadanie.Rmd</code> otwiera się automatycznie; plik <code>.Rproj</code> pozostaje zamknięty, bo jego otwarcie uruchamia nową sesję R bez logowania. Uruchamiaj bloki R od pierwszego. LEARN pokazuje odczyt wyników i wzorce zapisu; w pięciu zadaniach CHALLENGE blok R wyświetla potrzebne liczby, a akapit wpisujesz w ramce „Twój akapit”, poza blokami R. Orientacyjnie: 5 minut startu, 30 LEARN, 50 CHALLENGE i 5 oddania.</li>',
  '<li><strong>Wysłanie.</strong> Zapisz plik (Ctrl+S; macOS Cmd+S) i w konsoli wpisz polecenie z numerem bieżącego zadania:',
  '<pre><code>oddaj_zadanie("Z01")</code></pre>',
  'Funkcja sprawdza akapity, tworzy aktualny PDF i wysyła go razem ze źródłami do prywatnego repozytorium. Wysyłkę potwierdza komunikat ze zdalnym SHA; przy błędzie popraw wskazany problem i wyślij ponownie tym samym poleceniem.</li>',
  '<li><strong>Koniec zajęć.</strong> Wpisz <code>wyloguj_github()</code>, wyloguj się z GitHub w przeglądarce i zamknij RStudio, wybierając „Don\'t Save”.</li>',
  '<li><strong>Po zajęciach.</strong> <code>status_oddania("Z01")</code> potwierdza odbiór, a <code>pobierz_ocene("Z01")</code> pobiera ocenę po jej wystawieniu.</li></ol>',
  '<h2>Wykłady i handouty</h2>', vapply(lectures, card, character(1)),
  '<h2>Ćwiczenia i gotowe skrypty</h2>', vapply(exercises, card, character(1)),
  '<h2>Projekt indywidualny</h2>',
  '<p>C01–C08 korzystają z podanych scenariuszy. Projekt zaczyna się na C09: wybierasz scenariusz, zapisujesz syntetyczny wariant i plan. C10 odczytuje te same dane. Po obu zadaniach <code>przygotuj_raport()</code> jednorazowo przenosi dziesięć własnych odpowiedzi do <code>projekty/ilosciowy/raport.Rmd</code>. Otwórz ten plik, uporządkuj tekst, uzupełnij P11 z bibliografią i zakresem wsparcia. Po zapisaniu użyj <code>oddaj_projekt()</code>. Nie generuj nowego wariantu. Poniższe źródła są do wglądu; własny raport przygotowuje funkcja z zachowanych Z09/Z10.</p>',
  '<p><a href="szablony/projekt/raport.Rmd">raport Rmd z planem pomiaru</a> · <a href="szablony/projekt/analiza.R">gotowy silnik analiz</a> · <a href="rubryki/PROJEKT.html">rubryka HTML</a> · <a href="rubryki/PROJEKT.pdf">rubryka PDF</a></p>',
  paste0('<p><strong>Scenariusze:</strong> ', scenario_links, '</p>'),
  paste0('<p><strong>Rubryki zadań:</strong> ', rubric_links, '</p>'),
  '<h2>Własny komputer</h2>',
  '<p>Zainstaluj R w wersji 4.3 lub nowszej (cran.r-project.org), RStudio Desktop (posit.co) i GitHub CLI (cli.github.com). Następnie w konsoli RStudio zainstaluj pakiet kursu w wersji wydania i TinyTeX, czyli LaTeX potrzebny do PDF pracy:</p>',
  sprintf('<pre><code>install.packages("remotes")\nremotes::install_github("MarekDejaUJ/projektowanie-badan-ZI@v%s",\n                        dependencies = TRUE, upgrade = "never")\ninstall.packages("tinytex")\ntinytex::install_tinytex()</code></pre>', esc(wydanie$pakiet)),
  '<p>Po instalacji TinyTeX uruchom ponownie RStudio i doinstaluj pakiety LaTeX:</p>',
  '<pre><code>tinytex::tlmgr_install(c("xetex", "fontspec", "unicode-math", "lm",\n  "lm-math", "amsmath", "amsfonts", "babel", "babel-polish", "hyphen-polish",\n  "latex", "tools", "graphics", "graphics-cfg", "graphics-def", "geometry",\n  "hyperref", "bookmark", "booktabs", "etoolbox", "fancyvrb", "float",\n  "footnotehyper", "framed", "iftex", "l3kernel", "l3packages", "microtype",\n  "parskip", "upquote", "url", "xcolor", "xurl", "bigintcalc", "bitset",\n  "gettitlestring", "hycolor", "infwarerr", "intcalc", "kvdefinekeys",\n  "kvoptions", "kvsetkeys", "ltxcmds", "pdfescape", "pdftexcmds", "refcount",\n  "rerunfilecheck", "stringenc", "uniquecounter"))</code></pre>',
  '<p>Test komputera tworzy próbny PDF pracy bez logowania i bez wysyłania:</p>',
  '<pre><code>library(badaniaZI)\nsprawdz_srodowisko()\nk &lt;- file.path(tempdir(), "test-sali")\nutworz_projekt("test001", katalog = k)\np &lt;- przygotuj_zadanie("Z02", k)\nt &lt;- readLines(p, encoding = "UTF-8")\nt[t %in% sprintf("[UZUPELNIJ_S%02d]", 1:5)] &lt;- "Akapit testowy."\nwriteLines(t, p, useBytes = TRUE)\nsprawdz_zadanie("Z02", k)$ok</code></pre>',
  '<p>Komputer jest gotowy, gdy tabela <code>sprawdz_srodowisko()</code> ma <code>TRUE</code> w każdym wierszu, a ostatnie polecenie zwraca <code>TRUE</code>. Na własnym komputerze praca zostaje także w folderze <code>moje-badania</code>; kolejne zajęcia zaczynaj w tym samym folderze roboczym R, a funkcja <code>rozpocznij_zajecia()</code> odnajdzie istniejącą pracę. Wersja pakietu obowiązuje przez cały semestr; zmieniasz ją wyłącznie na polecenie prowadzącego.</p>',
  '<h2>Dla prowadzącego i informatyka</h2>',
  '<p><a href="README.md">README</a> opisuje przygotowanie komputerów w sali (programy, pakiety R i LaTeX, test stanowiska), komputer prowadzącego (repozytoria studentów, odbiór i ocena) oraz opcjonalną kontrolę oddań w kontenerze Docker. Studenci i komputery w sali pracują bez Dockera.</p>',
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
