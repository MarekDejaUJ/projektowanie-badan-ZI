# Projektowanie badań — Zarządzanie informacją

Pakiet `badaniaZI` obsługuje część ilościową kursu w roku 2026/27: materiały
(5 wykładów i 10 ćwiczeń w HTML i PDF), indywidualne dane syntetyczne, zadania,
rubryki oraz oddawanie prac z konsoli R do folderów prowadzącego w Nextcloud
(nc.uj.edu.pl). Student uruchamia gotowe analizy i pisze własne akapity; kodu
nie pisze.

Materiały online: <https://MarekDejaUJ.github.io/projektowanie-badan-ZI/>
Wydanie: **2.5.7**; zmiany opisuje [NEWS.md](NEWS.md). Wcześniejszy przepływ
z prywatnymi repozytoriami GitHub zachowuje gałąź `gh-workflow`.

Przewodnik ma trzech odbiorców: prowadzącego przygotowującego przestrzeń kursu
w Posit Cloud (część 1), studenta (część 2) i prowadzącego odbierającego prace
(część 3). Część 4 opisuje pracę na własnym komputerze, a część 5 — test
stanowiska.

| Stanowisko | Kto przygotowuje | Co jest potrzebne | Gdzie zostaje praca |
|---|---|---|---|
| Projekt w Posit Cloud (sala i dom) | prowadzący (projekt bazowy) albo student (własny projekt) | przeglądarka i konto Posit Cloud; w projekcie pakiet `badaniaZI` 2.5.7 i TinyTeX | w projekcie Posit Cloud studenta, folder `moje-badania` |
| Własny komputer | student | R 4.3 lub nowszy, RStudio, pakiet `badaniaZI` 2.5.7, TinyTeX | w folderze `moje-badania` |
| Odbiór prac | prowadzący | foldery „File drop” w Nextcloud, opcjonalnie R z pakietem do kontroli projektu | w folderach Nextcloud prowadzącego |

## 1. Posit Cloud — przestrzeń kursu

Posit Cloud udostępnia RStudio w przeglądarce. Komputery w sali potrzebują
tylko przeglądarki, a praca studenta zostaje w jego projekcie także po
wyczyszczeniu komputera.

### Projekt bazowy w przestrzeni prowadzącego

1. W przestrzeni kursu (Space) utwórz nowy projekt RStudio.
2. W konsoli projektu zainstaluj pakiet i narzędzia PDF:

   ```r
   install.packages("badaniaZI", repos = c(
     "https://marekdejauj.github.io/projektowanie-badan-ZI/pakiet",
     getOption("repos")
   ))
   badaniaZI::przygotuj_pdf()
   ```

   `install.packages()` instaluje razem z pakietem wszystkie pakiety używane
   w ćwiczeniach (m.in. `ggplot2`, `readxl`, `rmarkdown`, `curl`, `zip`,
   `tinytex`). `przygotuj_pdf()` instaluje TinyTeX z pakietami LaTeX potrzebnymi
   do PDF pracy i składa próbny dokument; trwa kilka minut.
3. Wykonaj test stanowiska z części 5.
4. Udostępnij projekt jako zadanie przestrzeni (Assignment), jeśli plan Posit
   Cloud na to pozwala: każdy student otrzyma własną kopię z gotowym pakietem
   i TinyTeX. Bez tej funkcji student tworzy własny projekt według części 2.

Wersja 2.5.7 obowiązuje przez cały semestr; zmianę wersji wykonuje się na
prośbę prowadzącego, poza zajęciami.

## 2. Student

Przed pierwszymi zajęciami (C01):

1. **Konto Posit Cloud** (<https://posit.cloud>) i dostęp do przestrzeni kursu
   z zaproszenia prowadzącego.
2. **Projekt kursu**: kopia projektu bazowego z przestrzeni albo własny nowy
   projekt RStudio, w którym w konsoli wpisujesz dwa polecenia instalacji
   z części 1.
3. **Pseudonimowe ID** (np. `s017`) otrzymane od prowadzącego. ID wyznacza
   indywidualne dane syntetyczne; nie zawiera nazwiska ani numeru albumu.

Przebieg każdych zajęć:

1. **Start.** W konsoli RStudio (dolny panel ze znakiem `>`) dwa polecenia
   z początku materiału ćwiczeń:

   ```r
   library(badaniaZI)
   rozpocznij_zajecia("C01", id_studenta = "TWOJE_ID", student = "Imię Nazwisko")
   ```

   Przy pierwszym starcie funkcja tworzy w bieżącym folderze roboczym folder
   `moje-badania` i zapisuje w nim ID oraz imię i nazwisko. Na kolejnych
   zajęciach wystarczy numer ćwiczeń, np. `rozpocznij_zajecia("C02")`.
   Funkcja otwiera plik `zadanie.Rmd` i kopiuje do `moje-badania/materialy`
   HTML i PDF ćwiczenia oraz rubrykę zadania.
2. **Praca.** Student uruchamia bloki R od pierwszego, zielonym trójkątem przy
   bloku. Części LEARN pokazują odczyt wyników i wzorce zapisu; w pięciu
   zadaniach CHALLENGE blok R wyświetla wszystkie potrzebne liczby, a student
   wpisuje akapit w ramce „Twój akapit”, w miejsce znacznika w nawiasie
   kwadratowym, poza blokami R. Orientacyjnie: 5 minut startu, 30 LEARN,
   50 CHALLENGE i 5 oddania.
3. **Oddanie.** Ctrl+S (w macOS Cmd+S), a potem w konsoli:

   ```r
   oddaj_zadanie("Z01")
   ```

   Funkcja sprawdza komplet akapitów, gotowy kod, ID i dane, tworzy aktualny
   PDF z zapisanego pliku w świeżej sesji R, otwiera go do obejrzenia i po
   potwierdzeniu literą `t` wysyła do folderu zadania prowadzącego. Oddanie
   potwierdza komunikat z nazwą pliku, folderem i czasem; plik
   `moje-badania/oddania.csv` zapisuje też sumę SHA-256 wysłanego PDF. Przy
   błędzie komunikat wskazuje problem; po poprawce oddaje się ponownie tym
   samym poleceniem. `sprawdz_zadanie("Z01")` tworzy i sprawdza PDF bez
   wysyłania, a `status_oddania()` pokazuje zapis oddań z tego projektu.
4. **Projekt.** C01–C08 mają podane scenariusze. Na C09 student wybiera jeden
   z 20 scenariuszy i zapisuje własny wariant danych; C10 odczytuje te same dane.
   Po oddaniu Z09 i Z10 polecenie `przygotuj_raport()` tworzy
   `moje-badania/projekty/ilosciowy/raport.Rmd` z gotowym silnikiem analiz
   i danymi wariantu oraz przenosi do niego dziesięć własnych akapitów. Student
   redaguje je w jeden tekst, uzupełnia pole P11 (źródła i zakres wsparcia
   narzędzi SI), zapisuje plik i wysyła go poleceniem `oddaj_projekt()`.

## 3. Odbiór i ocena

Każde zadanie ma folder Nextcloud udostępniony jako „File drop” (tylko
przesyłanie, bez hasła): Z01–Z10 oraz PROJEKT. Adresy folderów zapisuje
konfiguracja rocznika, pole `foldery_oddania` w `inst/kurs/2026-27.yml`;
zmiana adresu wymaga nowego wydania pakietu. Folder nie pokazuje studentom
cudzych plików i nie pozwala ich nadpisać.

| Oddanie | Pliki w folderze |
|---|---|
| Z01–Z10 | `ID_nazwisko_imie_Z01.pdf`, np. `s017_kowalska_anna_Z01.pdf` |
| Projekt | `s017_kowalska_anna_PROJEKT.pdf` i `s017_kowalska_anna_PROJEKT_zrodla.zip` |

Ponowne oddanie trafia obok poprzedniego jako `nazwa (2).pdf`; czas przyjęcia
pliku w Nextcloud rozstrzyga o terminie. Po wygaśnięciu udziału folder
przestaje przyjmować pliki, a funkcja oddania informuje studenta, że minął
termin. PDF zawiera wyniki, akapity i ID w nagłówku, więc do oceny wystarcza
on sam.

Archiwum projektu zawiera `kurs.yml` i komplet folderu
`projekty/ilosciowy` (raport, silnik analiz, dane wariantu, metadane i PDF).
Odtworzenie raportu po rozpakowaniu:

```r
library(badaniaZI)
sprawdz_zadanie("PROJEKT", "ścieżka/do/rozpakowanego/archiwum")$kontrole
```

Ocena według rubryki:

```r
f <- formularz_oceny("Z01")
f                              # kryteria i maksimum punktów z rubryki Z01
f$punkty <- c(2.25, 3, 1.5, 2) # punkty kolejnych kryteriów, na poziomach rubryki
sprawdz_ocene(f, "Z01")
```

Ocenę końcową tej części kursu liczy `oblicz_ocene()` z punktów dziesięciu
zadań i projektu. Terminy podają `konfiguracja_kursu()` i `termin_zadania()`.
Przed C09 `sprawdz_warianty()` sprawdza w prywatnym wykazie ID, czy warianty
danych są unikalne.

## 4. Własny komputer

1. **R** w wersji 4.3 lub nowszej (<https://cran.r-project.org>) i **RStudio
   Desktop** (<https://posit.co/download/rstudio-desktop/>), który zawiera
   Pandoc.
2. W konsoli RStudio dwa polecenia instalacji z części 1. Na komputerze
   z innym LaTeX (TeX Live, MiKTeX) `przygotuj_pdf()` korzysta z niego
   i sprawdza próbny skład; brakujące pakiety LaTeX z listy instaluje się
   wtedy narzędziem tej dystrybucji.
3. Praca zostaje w folderze `moje-badania` utworzonym w bieżącym folderze
   roboczym R; kolejne zajęcia zaczyna się w tym samym folderze.

## 5. Test stanowiska

W konsoli RStudio:

```r
library(badaniaZI)
sprawdz_srodowisko()
k <- file.path(tempdir(), "test-pracy")
utworz_projekt("test001", katalog = k, student = "Test Kursu")
p <- przygotuj_zadanie("Z02", k)
t <- readLines(p, encoding = "UTF-8")
t[t %in% sprintf("[UZUPELNIJ_S%02d]", 1:5)] <- "Akapit testowy."
writeLines(t, p, useBytes = TRUE)
sprawdz_zadanie("Z02", k)$ok
```

Stanowisko jest gotowe, gdy tabela `sprawdz_srodowisko()` ma w kolumnie
`dostepne` wartość `TRUE` w każdym wierszu, a ostatnie polecenie zwraca `TRUE`:
stanowisko utworzyło wtedy próbny PDF pracy studenta. Test niczego nie wysyła.

## Wydanie i strona

Strona i repozytorium pakietu powstają w GitHub Actions z gałęzi `main`
(`tools/build-site.R`): strona zawiera materiały HTML, PDF i Rmd, rubryki,
scenariusze oraz zbudowaną paczkę pakietu w `pakiet/src/contrib`. Sprawdzenie
pakietu obejmuje Windows, macOS i Linux, instalację z repozytorium pakietu
oraz przepływ studenta z TinyTeX, PDF i oddaniem (wysyłka próbna tylko na
żądanie). Ręczne uruchomienie sprawdzenia z parametrem `render` renderuje
wskazane materiały na Windows i udostępnia je jako artefakt.

## Ocena i terminy 2026/27

Projekt indywidualny ilościowy stanowi 50% oceny, dziesięć jednakowo ważonych
zadań łącznie 50% (każde 5%). Projekt obejmuje plan badania i analizę danych.
Egzaminu nie ma; ocena wybranej realizacji jest oceną końcową.

Z01–Z09 są oddawane do rozpoczęcia kolejnych ćwiczeń, Z10 do 26.01.2027
o 10:30. Projekt do 27.01.2027; pierwsze spóźnione oddanie do 10.02.2027
z maksymalną oceną 4,5. Oceniony projekt można poprawiać do 24.02.2027.
Pełny kalendarz spotkań pozostaje do ustalenia. Daty, wagi, progi i rubryki
są wersjonowane w pakiecie; sprawdź `konfiguracja_kursu()`, `termin_zadania()`
i ogłoszenie na platformie UJ.

## Pomoc w kodzie

Generatywna SI może pomagać w zrozumieniu i poprawianiu kodu. Interpretację wyników i wnioski napisz samodzielnie; oznacz użyte fragmenty i sprawdź ich działanie. Korzystanie z SI jest dobrowolne. Obowiązują także [zasady UJ](https://bip.uj.edu.pl/documents/1384597/159749054/zarz_115_2025.pdf).

## Licencja

MIT. Dane demonstracyjne są syntetyczne i nie stanowią dowodu o rzeczywistych
instytucjach ani respondentach.
