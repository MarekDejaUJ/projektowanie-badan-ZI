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

# Strona udostępnia materiały do czytania: HTML, PDF, Rmd i rubryki. Skrypty R
# oraz źródła handoutów zostają w pakiecie, który student instaluje w Posit Cloud.
copy_tree <- function(from, to, pomin = "[.](R|tex)$|_files/") {
  files <- list.files(from, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[!grepl(pomin, files)]
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
dir.create(file.path(out, "szablony", "projekt"), recursive = TRUE)
stopifnot(file.copy("inst/szablony/projekt/raport.Rmd", file.path(out, "szablony", "projekt", "raport.Rmd")))
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
               "rubryka" = paste0("rubryki/Z", substring(id, 2), ".html"))
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
  '<ol><li>Załóż bezpłatne konto w <a href="https://posit.cloud">Posit Cloud</a> (RStudio w przeglądarce) albo dołącz do przestrzeni kursu z zaproszenia prowadzącego. Pracujesz w przeglądarce; w sali nie trzeba niczego instalować, a praca zostaje w projekcie Posit Cloud.</li>',
  '<li>Otwórz projekt kursu z przestrzeni prowadzącego albo utwórz własny projekt RStudio i zainstaluj w nim pakiet kursu według sekcji „Instalacja pakietu” na końcu strony.</li>',
  '<li>Zapisz swoje pseudonimowe ID (np. <code>s017</code>) otrzymane od prowadzącego. ID wyznacza indywidualne dane syntetyczne; przy pierwszym starcie podajesz je razem z imieniem i nazwiskiem.</li></ol>',
  '<h2>Przebieg zajęć</h2>',
  '<ol><li><strong>Start.</strong> W konsoli RStudio (dolny panel ze znakiem <code>&gt;</code>) wpisz dwa polecenia z początku materiału ćwiczeń: numer bieżących ćwiczeń, swoje ID oraz imię i nazwisko.',
  '<pre><code>library(badaniaZI)\nrozpocznij_zajecia("C01", id_studenta = "TWOJE_ID", student = "Imię Nazwisko")</code></pre>',
  'Przy pierwszym starcie funkcja tworzy folder <code>moje-badania</code> i zapisuje w nim ID oraz imię i nazwisko; na kolejnych zajęciach wystarczy <code>rozpocznij_zajecia("C02")</code>. Funkcja otwiera plik <code>zadanie.Rmd</code> bieżących ćwiczeń i kopiuje do <code>moje-badania/materialy</code> wersję HTML i PDF ćwiczenia oraz rubrykę.</li>',
  '<li><strong>Praca.</strong> Uruchamiaj bloki R od pierwszego. LEARN pokazuje odczyt wyników i wzorce zapisu; w pięciu zadaniach CHALLENGE blok R wyświetla potrzebne liczby, a akapit wpisujesz w ramce „Twój akapit”, poza blokami R. Orientacyjnie: 5 minut startu, 30 LEARN, 50 CHALLENGE i 5 oddania.</li>',
  '<li><strong>Oddanie.</strong> Zapisz plik (Ctrl+S; macOS Cmd+S) i w konsoli wpisz polecenie z numerem bieżącego zadania:',
  '<pre><code>oddaj_zadanie("Z01")</code></pre>',
  'Funkcja sprawdza akapity, tworzy aktualny PDF w świeżej sesji R, otwiera go do obejrzenia i po potwierdzeniu literą <code>t</code> wysyła do folderu zadania prowadzącego na serwerze UJ (nc.uj.edu.pl), pod nazwą z Twoim ID oraz nazwiskiem i imieniem, np. <code>s017_kowalska_anna_Z01.pdf</code>. Oddanie potwierdza komunikat z nazwą pliku i czasem; przy błędzie popraw wskazany problem i oddaj ponownie tym samym poleceniem. Nowa wersja trafia do folderu obok poprzedniej.</li>',
  '<li><strong>Po zajęciach.</strong> Posit Cloud zachowuje folder <code>moje-badania</code> do kolejnych zajęć. <code>status_oddania()</code> pokazuje zapis Twoich oddań z tego projektu. Ocenę prowadzący przekazuje osobno.</li></ol>',
  '<h2>Wykłady i handouty</h2>', vapply(lectures, card, character(1)),
  '<h2>Ćwiczenia</h2>', vapply(exercises, card, character(1)),
  '<h2>Projekt indywidualny</h2>',
  '<p>C01–C08 korzystają z podanych scenariuszy. Projekt zaczyna się na C09: wybierasz scenariusz, zapisujesz syntetyczny wariant i plan; karty scenariuszy trafiają wtedy także do <code>moje-badania/materialy/scenariusze</code>. C10 odczytuje te same dane. Po oddaniu Z09 i Z10 polecenie <code>przygotuj_raport()</code> jednorazowo przenosi dziesięć własnych odpowiedzi do <code>moje-badania/projekty/ilosciowy/raport.Rmd</code> razem z gotowym silnikiem analiz i danymi wariantu. Otwórz ten plik, uporządkuj tekst, uzupełnij P11 z bibliografią i zakresem wsparcia. Po zapisaniu użyj <code>oddaj_projekt()</code>: funkcja wysyła do folderu projektu PDF raportu i archiwum ZIP ze źródłami i danymi. Nie generuj nowego wariantu. Raport Rmd poniżej jest do wglądu; własny raport przygotowuje funkcja z zachowanych Z09/Z10.</p>',
  '<p><a href="szablony/projekt/raport.Rmd">raport Rmd z planem pomiaru</a> · <a href="rubryki/PROJEKT.html">rubryka HTML</a> · <a href="rubryki/PROJEKT.pdf">rubryka PDF</a></p>',
  paste0('<p><strong>Scenariusze:</strong> ', scenario_links, '</p>'),
  paste0('<p><strong>Rubryki zadań:</strong> ', rubric_links, '</p>'),
  '<h2>Instalacja pakietu</h2>',
  '<p>W projekcie Posit Cloud albo w RStudio na własnym komputerze (R w wersji 4.3 lub nowszej) zainstaluj pakiet kursu. Polecenie instaluje także wszystkie pakiety używane w ćwiczeniach. Drugie polecenie jednorazowo przygotowuje narzędzia PDF (TinyTeX) i składa próbny dokument; trwa kilka minut. W projekcie kursu udostępnionym przez prowadzącego oba kroki są już wykonane.</p>',
  sprintf('<pre><code>install.packages("badaniaZI", repos = c(\n  "https://marekdejauj.github.io/projektowanie-badan-ZI/pakiet",\n  getOption("repos")\n))\nbadaniaZI::przygotuj_pdf()\npackageVersion("badaniaZI")   # %s</code></pre>', esc(wydanie$pakiet)),
  '<p>Test tworzy próbny PDF pracy bez wysyłania:</p>',
  '<pre><code>library(badaniaZI)\nsprawdz_srodowisko()\nk &lt;- file.path(tempdir(), "test-pracy")\nutworz_projekt("test001", katalog = k, student = "Test Kursu")\np &lt;- przygotuj_zadanie("Z02", k)\nt &lt;- readLines(p, encoding = "UTF-8")\nt[t %in% sprintf("[UZUPELNIJ_S%02d]", 1:5)] &lt;- "Akapit testowy."\nwriteLines(t, p, useBytes = TRUE)\nsprawdz_zadanie("Z02", k)$ok</code></pre>',
  '<p>Stanowisko jest gotowe, gdy tabela <code>sprawdz_srodowisko()</code> ma <code>TRUE</code> w każdym wierszu, a ostatnie polecenie zwraca <code>TRUE</code>. Wersja pakietu obowiązuje przez cały semestr; zmieniasz ją wyłącznie na polecenie prowadzącego.</p>',
  '<h2>Dla prowadzącego</h2>',
  '<p><a href="README.md">README</a> opisuje przygotowanie projektu kursu w przestrzeni Posit Cloud, foldery oddania w Nextcloud, nazwy przyjmowanych plików, odbiór i ocenę według rubryk oraz test stanowiska.</p>',
  '</body></html>')
writeLines(html, file.path(out, "index.html"), useBytes = TRUE)

required <- unlist(lapply(manifest, function(x) {
  base <- file.path(out, "materialy", tolower(x$id))
  if (x$typ == "cwiczenie") file.path(base, c("pelne.html", "pelne.pdf", "pelne.Rmd"))
  else file.path(base, c("pelne.html", "pelne.pdf", "pelne.Rmd", "handout.html", "handout.pdf", "handout.Rmd"))
}))
stopifnot(all(file.exists(required)), length(list.files(file.path(out, "scenariusze"), "^S[0-9]{2}[.]md$")) == 20L,
          !length(list.files(out, "[.](R|tex)$", recursive = TRUE)))

# Repozytorium pakietu na stronie: install.packages() pobiera badaniaZI stąd,
# a zależności z repozytorium CRAN (w Posit Cloud — z Posit Package Manager).
repo <- file.path(out, "pakiet", "src", "contrib")
stopifnot(dir.create(repo, recursive = TRUE))
budowa <- system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(root)),
                  stdout = TRUE, stderr = TRUE)
paczka <- file.path(getwd(), paste0("badaniaZI_", wydanie$pakiet, ".tar.gz"))
if (!file.exists(paczka)) stop("Nie zbudowano paczki pakietu:\n", paste(budowa, collapse = "\n"))
stopifnot(file.rename(paczka, file.path(repo, basename(paczka))))
tools::write_PACKAGES(repo, type = "source")
# Puste indeksy binarne: Windows i macOS nie zgłaszają wtedy ostrzeżenia o brakującym indeksie.
wersje_r <- sprintf("4.%d", 3:7)
for (sciezka in c(file.path("bin", "windows", "contrib", wersje_r),
                  file.path("bin", "macosx", rep(c("big-sur-arm64", "big-sur-x86_64", "sonoma-arm64"), each = length(wersje_r)),
                            "contrib", wersje_r))) {
  d <- file.path(out, "pakiet", sciezka)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # R pobiera przez HTTP najpierw PACKAGES.rds i PACKAGES.gz, dlatego zapisujemy komplet
  # pustych indeksów w formacie indeksu źródłowego.
  file.create(file.path(d, "PACKAGES"))
  close(gzfile(file.path(d, "PACKAGES.gz"), "w"))
  saveRDS(readRDS(file.path(repo, "PACKAGES.rds"))[0, , drop = FALSE], file.path(d, "PACKAGES.rds"))
}
indeks <- read.dcf(file.path(repo, "PACKAGES"))
stopifnot(identical(unname(indeks[, "Package"]), "badaniaZI"), identical(unname(indeks[, "Version"]), wydanie$pakiet))
href <- regmatches(html, gregexpr('href="[^"]+"', html))
href <- sub('^href="(.*)"$', '\\1', unlist(href))
href <- href[!grepl("^https?://", href)]
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
