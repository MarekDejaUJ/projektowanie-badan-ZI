# Projektowanie badań: część ilościowa

Materiały części ilościowej przedmiotu *Projektowanie badań naukowych i analiz instytucjonalnych dla zarządzania informacją* (Zarządzanie informacją, studia II stopnia, UJ, 2026/27): 10 ćwiczeń, 5 wykładów i projekt indywidualny.

Strona kursu: <https://marekdejauj.github.io/projektowanie-badan-ZI/>

## Jak działa kurs

Każde ćwiczenie ma dwie części. **LEARN** jest rozdziałem książki na stronie kursu: pytanie, wzory z opisem symboli, gotowe analizy na danych scenariusza S02 i wzorce zapisu wyników. **CHALLENGE** jest plikiem zadania `zadanie-0n.Rmd`: pięć poleceń z gotowym kodem na danych innego scenariusza i miejscem na odpowiedź. Dane w pliku zadania zależą od ID studenta, więc każda osoba ma własne liczby. Projekt indywidualny powstaje w pliku `projekt.Rmd` na ćwiczeniach C09 i C10.

Student nie instaluje pakietu kursu. Pierwszy blok każdego pliku zadania uruchamia skrypt startowy z adresu kursu:

```r
source("https://marekdejauj.github.io/projektowanie-badan-ZI/materialy/start.R")
```

Skrypt doinstalowuje brakujące pakiety CRAN, ładuje pakiety używane w zadaniach, tworzy dane scenariuszy i udostępnia funkcje `pobierz_szablon()`, `oddaj_zadanie()`, `oddaj_projekt()`, `sprawdz_srodowisko()` i `przygotuj_pdf()`. Oddanie tworzy PDF w nowej sesji R (przy braku LaTeX-a HTML) i wysyła go do folderu ćwiczenia w Nextcloud UJ jako `id_nazwisko_imie_Z0n.pdf`; ponowne oddanie dodaje nowy plik obok poprzedniego.

## Zawartość repozytorium

| Ścieżka | Zawartość |
|:--|:--|
| `ksiazka/` | źródła książki (bookdown, `bs4_book`): strona startowa, instalacja, rozdziały C01–C10, projekt, wykłady, scenariusze, ściąga |
| `materialy/start.R` | skrypt startowy: pakiety, generator danych, funkcje opisu, pobieranie plików zadań i oddawanie do Nextcloud |
| `materialy/scenariusze.yml` | karty scenariuszy S01–S20: problem, grupy, kategoria, pozycje skali, zadanie obserwowane, parametry generatora |
| `materialy/zadanie-01.Rmd` … `zadanie-08.Rmd`, `projekt.Rmd` | pliki studenta (CHALLENGE i projekt) |
| `materialy/wyklady/` | wykłady W01–W05 w PDF i HTML |
| `build.R` | budowa książki do `_book/` i test plików zadań w nowych sesjach R |
| `.github/workflows/pages.yml` | publikacja strony po zmianie gałęzi `main` |

## Sala komputerowa (informatyk)

Na każdym stanowisku: R 4.3 lub nowszy, RStudio, pakiety R i TinyTeX z pakietami LaTeX, dostępne dla wszystkich użytkowników, oraz dostęp do `marekdejauj.github.io` i `nc.uj.edu.pl`.

```r
install.packages(c("dplyr", "ggplot2", "infer", "psych", "rstatix", "effectsize", "car",
                   "rmarkdown", "knitr", "yaml", "curl", "tinytex", "rstudioapi"),
                 repos = "https://cloud.r-project.org")
```

Pakiety LaTeX: `fontspec`, `unicode-math`, `lm`, `lm-math`, `babel-polish`, `hyphen-polish`, `geometry`, `hyperref`, `bookmark`, `booktabs`, `fancyvrb`, `fvextra`, `float`, `framed`, `xcolor`, `upquote`, `microtype`, `parskip`, `xurl`, `lineno`, `etoolbox`. Test stanowiska z konta studenta:

```r
source("https://marekdejauj.github.io/projektowanie-badan-ZI/materialy/start.R")
sprawdz_srodowisko(pdf = TRUE)
```

Komputery w sali usuwają stan pracy po zajęciach, więc studenci oddają pracę przed wyjściem albo zapisują plik zadania poza komputerem. Instrukcję dla studentów (własny komputer, Posit Cloud) zawiera rozdział „Instalacja i środowisko pracy” na stronie kursu.

## Prowadzący

Budowa i test (z katalogu repozytorium; potrzebne pakiety z listy powyżej oraz `bookdown`, `downlit`, `xml2`):

```sh
Rscript build.R            # książka w _book/ i test plików zadań (HTML)
Rscript build.R --pdf      # dodatkowo PDF każdego pliku zadania
Rscript build.R --oddaj    # dodatkowo oddaj_zadanie(..., wyslij = FALSE) dla każdego pliku
```

Test podmienia w kopii pliku zadania adres skryptu startowego na lokalny `materialy/start.R`, więc sprawdza wersję z repozytorium przed publikacją. Zmiana gałęzi `main` uruchamia workflow, który buduje książkę, testuje pliki zadań i publikuje stronę.

Dane studenta odtwarza `dane_zi("S11", id = "s017")`: ten sam scenariusz i ID dają zawsze te same liczby. Zmiana generatora w `start.R` zmienia dane wszystkich osób, dlatego w trakcie semestru zmiany generatora wymagają nowej wersji danych i informacji dla studentów. Linki folderów Nextcloud (Z01–Z10, PROJEKT) zawiera lista `.zi_foldery` w `materialy/start.R`.

## Archiwum

Poprzednia wersja kursu, pakiet R `badaniaZI` 2.5.8 z materiałami C01–C10, W01–W05 i handoutami, jest zachowana w tagu `v2.5.8` i gałęzi `badaniaZI-2.5`.
