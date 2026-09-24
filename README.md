# Projektowanie badań — Zarządzanie informacją

Pakiet `badaniaZI` obsługuje część ilościową kursu w roku 2026/27: materiały
(5 wykładów i 10 ćwiczeń w HTML i PDF), indywidualne dane syntetyczne, zadania,
rubryki oraz oddawanie prac z konsoli R do prywatnych repozytoriów GitHub.
Student uruchamia gotowe analizy i pisze własne akapity; kodu nie pisze.

Materiały online: <https://MarekDejaUJ.github.io/projektowanie-badan-ZI/>
Wydanie: **2.3.5**; zmiany opisuje [NEWS.md](NEWS.md).

Przewodnik ma trzech odbiorców: informatyka przygotowującego komputery w sali
(część 1), prowadzącego (część 2) i studenta pracującego na własnym komputerze
(część 3). Część 4 opisuje przebieg pracy od logowania do wysłania, a część 5 —
rolę Dockera.

| Stanowisko | Kto przygotowuje | Co jest potrzebne | Gdzie zostaje praca |
|---|---|---|---|
| Komputery w sali (czyszczone po zajęciach) | informatyk | R, RStudio, pakiet `badaniaZI` 2.3.5 z pakietami dodatkowymi, GitHub CLI, XeLaTeX z listą pakietów, przeglądarka | w prywatnym repozytorium GitHub studenta; komputer przechowuje pracę tylko w trakcie zajęć |
| Komputer prowadzącego (stały) | prowadzący, ewentualnie informatyk | to samo co w sali; opcjonalnie Docker do niezależnej kontroli oddań | prywatne repozytoria studentów na koncie prowadzącego i prywatny wykaz ID |
| Własny komputer studenta | student | to samo co w sali | w prywatnym repozytorium GitHub i w folderze `moje-badania` |

## 1. Komputery w sali — instrukcja dla informatyka

Komputery są czyszczone po zajęciach, dlatego każdy program instaluje się dla
wszystkich użytkowników, w folderach systemowych, a nie w profilu użytkownika.
Praca studenta przechowywana jest na GitHub: na początku zajęć pakiet pobiera ją
z prywatnego repozytorium, a na końcu wysyła z powrotem.

### Programy

1. **R w wersji 4.3 lub nowszej** (<https://cran.r-project.org>), instalacja dla
   wszystkich użytkowników.
2. **RStudio Desktop** (<https://posit.co/download/rstudio-desktop/>). RStudio
   zawiera Pandoc, którego pakiet używa do tworzenia PDF. Zalecane ustawienie:
   *Tools → Global Options → General → Save workspace to .RData on exit: Never*.
3. **GitHub CLI** (<https://cli.github.com>; w Windows np.
   `winget install --id GitHub.cli`). Polecenie `gh` musi być dostępne w ścieżce
   systemowej PATH: `gh --version` działa w nowym oknie terminala. Program służy
   wyłącznie do logowania i nie wymaga konfiguracji; pakiet zapisuje dane
   logowania w folderze tymczasowym, usuwa go po zalogowaniu, a token przechowuje
   tylko w pamięci bieżącej sesji R.
4. **Dystrybucja LaTeX z XeLaTeX**, dla wszystkich użytkowników: TeX Live
   (schemat pełny zawiera wszystkie potrzebne pakiety) albo MiKTeX z instalacją
   „for all users”. Pakiet kursu tworzy PDF z wyłączonym doinstalowywaniem
   pakietów LaTeX w trakcie pracy, więc wszystkie pakiety z listy muszą być
   zainstalowane wcześniej:

   ```
   xetex fontspec unicode-math lm lm-math amsmath amsfonts babel babel-polish
   hyphen-polish latex tools graphics graphics-cfg graphics-def geometry hyperref
   bookmark booktabs etoolbox fancyvrb float footnotehyper framed iftex l3kernel
   l3packages microtype parskip upquote url xcolor xurl bigintcalc bitset
   gettitlestring hycolor infwarerr intcalc kvdefinekeys kvoptions kvsetkeys
   ltxcmds pdfescape pdftexcmds refcount rerunfilecheck stringenc uniquecounter
   ```

   W TeX Live pakiety instaluje `tlmgr install` z tą listą; w MiKTeX — MiKTeX
   Console, pod tymi samymi nazwami.
5. **Przeglądarka internetowa** do logowania na stronie GitHub.
6. **Dostęp do sieci**: `github.com` i `api.github.com` (HTTPS, port 443).

Komputery w sali działają bez Dockera, bez Gita w wierszu poleceń (pakiet
korzysta z biblioteki libgit2 wbudowanej w pakiet R `gert`) i bez zapisanych
kont lub haseł.

### Pakiety R

W R uruchomionym z uprawnieniami administratora, tak aby pakiety trafiły do
biblioteki systemowej:

```r
install.packages("remotes")
remotes::install_github("MarekDejaUJ/projektowanie-badan-ZI@v2.3.5",
                        dependencies = TRUE, upgrade = "never")
```

Argument `dependencies = TRUE` instaluje także pakiety `readxl` i `rstudioapi`:
materiały używają ich do odczytu arkusza w C02 i do otwierania pliku pracy
w RStudio. Wersja 2.3.5 obowiązuje przez cały semestr; zmianę wersji wykonuje się
na prośbę prowadzącego, poza zajęciami.

### Kontrola stanowiska

Na koncie zwykłego użytkownika, w konsoli RStudio:

```r
library(badaniaZI)
sprawdz_srodowisko()
k <- file.path(tempdir(), "test-sali")
utworz_projekt("test001", katalog = k)
p <- przygotuj_zadanie("Z02", k)
t <- readLines(p, encoding = "UTF-8")
t[t %in% sprintf("[UZUPELNIJ_S%02d]", 1:5)] <- "Akapit testowy."
writeLines(t, p, useBytes = TRUE)
sprawdz_zadanie("Z02", k)$ok
```

Stanowisko jest gotowe, gdy tabela `sprawdz_srodowisko()` ma w kolumnie
`dostepne` wartość `TRUE` w każdym wierszu, a ostatnie polecenie zwraca `TRUE`:
stanowisko utworzyło wtedy próbny PDF pracy studenta. Test działa bez konta
GitHub i niczego nie wysyła.

## 2. Komputer prowadzącego

Komputer prowadzącego nie jest czyszczony. Potrzebuje tych samych programów
i pakietów co komputery w sali (część 1) oraz konta GitHub wskazanego
w konfiguracji kursu jako właściciel repozytoriów (`konfiguracja_kursu()`,
pole `wlasciciel_repozytoriow`), z włączonym uwierzytelnianiem dwuskładnikowym.
Docker jest potrzebny tylko do opcjonalnej niezależnej kontroli oddań (część 5).

Wykaz przydziałów (ID studenta, login GitHub, adres repozytorium) prowadzący
przechowuje prywatnie, poza publicznym repozytorium materiałów.

### Przed pierwszymi zajęciami

1. Każdy student otrzymuje pseudonimowe ID, np. `s017`. ID trafia do nazwy
   repozytorium i do danych syntetycznych, więc nie zawiera nazwiska ani numeru
   albumu.
2. Prowadzący zbiera loginy GitHub studentów.
3. Dla każdego studenta:

   ```r
   library(badaniaZI)
   zaloguj_github()
   przygotuj_repo_studenta("s017", "login-studenta")
   ```

   Funkcja tworzy na koncie prowadzącego prywatne repozytorium `ZI-s017`
   z konfiguracją przestrzeni ćwiczeń i wysyła zaproszenie z prawem zapisu.
   Organizacja GitHub jest zbędna. Student otrzymuje prywatnie dwie informacje:
   swoje ID i adres repozytorium.
4. Przed C09 `sprawdz_warianty()` sprawdza w prywatnym wykazie, czy warianty
   danych są unikalne.

### Odbiór i ocena

Każde oddanie jest commitem „Oddaj Z01” (dla projektu „Oddaj PROJEKT”)
w repozytorium studenta, z zielonym statusem `badaniaZI/odbior/Z01`. PDF pracy
leży w `zadania/z01/zadanie.pdf`, a PDF projektu w
`projekty/ilosciowy/raport.pdf`. Pełny SHA tego commita identyfikuje ocenianą
wersję; student ma ten sam SHA w potwierdzeniu wysyłki.

```r
library(badaniaZI)
zaloguj_github()
f <- formularz_oceny("Z01")
f                              # kryteria i maksimum punktów z rubryki Z01
f$punkty <- c(2.25, 3, 1.5, 2) # punkty kolejnych kryteriów, na poziomach rubryki
wystaw_ocene("MarekDejaUJ/ZI-s017", "PELNY_SHA_ODDANIA", "Z01", f,
             "Ogólny komentarz merytoryczny.")
```

Ocena trafia jako prywatny komentarz do ocenianego commita; student odczytuje
ją poleceniem `pobierz_ocene("Z01")`. Ocenę końcową tej części kursu liczy
`oblicz_ocene()` z punktów dziesięciu zadań i projektu. Terminy podają
`konfiguracja_kursu()` i `termin_zadania()`.

## 3. Własny komputer studenta

Przed pierwszymi zajęciami (C01):

1. **Konto GitHub** z włączonym uwierzytelnianiem dwuskładnikowym (2FA)
   i przyjęte zaproszenie do prywatnego repozytorium: e-mail z GitHub albo
   <https://github.com/notifications>.
2. **R** w wersji 4.3 lub nowszej, **RStudio Desktop** i **GitHub CLI**
   (strony jak w części 1; w macOS np. `brew install gh`).
3. **Pakiet kursu i LaTeX** — w konsoli RStudio:

   ```r
   install.packages("remotes")
   remotes::install_github("MarekDejaUJ/projektowanie-badan-ZI@v2.3.5",
                           dependencies = TRUE, upgrade = "never")
   install.packages("tinytex")
   tinytex::install_tinytex()
   ```

   Po instalacji TinyTeX uruchom ponownie RStudio i doinstaluj pakiety LaTeX:

   ```r
   tinytex::tlmgr_install(c("xetex", "fontspec", "unicode-math", "lm",
     "lm-math", "amsmath", "amsfonts", "babel", "babel-polish", "hyphen-polish",
     "latex", "tools", "graphics", "graphics-cfg", "graphics-def", "geometry",
     "hyperref", "bookmark", "booktabs", "etoolbox", "fancyvrb", "float",
     "footnotehyper", "framed", "iftex", "l3kernel", "l3packages", "microtype",
     "parskip", "upquote", "url", "xcolor", "xurl", "bigintcalc", "bitset",
     "gettitlestring", "hycolor", "infwarerr", "intcalc", "kvdefinekeys",
     "kvoptions", "kvsetkeys", "ltxcmds", "pdfescape", "pdftexcmds", "refcount",
     "rerunfilecheck", "stringenc", "uniquecounter"))
   ```

4. **Test** z części 1 („Kontrola stanowiska”): oba wyniki `TRUE` oznaczają
   gotowy komputer.

Na własnym komputerze praca zostaje także lokalnie, w folderze `moje-badania`
utworzonym w bieżącym folderze roboczym R przy pierwszym uruchomieniu
`rozpocznij_zajecia()`. Kolejne zajęcia zaczyna się w tym samym folderze
roboczym (domyślnie w folderze domowym); funkcja odnajduje wtedy istniejącą
pracę i dopisuje nowe ćwiczenie.

## 4. Przebieg pracy — od logowania do wysłania

1. **Start.** W RStudio, w konsoli (dolny panel ze znakiem `>`), student wpisuje
   cztery polecenia z początku każdego materiału ćwiczeń:

   ```r
   library(badaniaZI)
   ID <- "TWOJE_ID"
   zaloguj_github()
   rozpocznij_zajecia(
     "C01", id_studenta = ID,
     repo_url = "ADRES_TWOJEGO_PRYWATNEGO_REPOZYTORIUM"
   )
   ```

   W miejsce `TWOJE_ID` wpisuje swoje ID, w miejsce adresu — adres repozytorium,
   a `C01` zmienia na numer bieżących ćwiczeń.
2. **Logowanie.** `zaloguj_github()` wyświetla w konsoli jednorazowy kod
   i otwiera stronę GitHub. Hasło, drugi składnik i kod wpisuje się wyłącznie na
   stronie GitHub, nigdy w konsoli R.
3. **Otwarcie pracy.** `rozpocznij_zajecia()` pobiera repozytorium i otwiera
   w RStudio plik `zadanie.Rmd` bieżących ćwiczeń. Plik `.Rproj` pozostaje
   zamknięty: jego otwarcie uruchamia nową sesję R bez logowania.
4. **Praca.** Student uruchamia bloki R od pierwszego, zielonym trójkątem przy
   bloku. Części LEARN pokazują odczyt wyników i wzorce zapisu; w pięciu
   zadaniach CHALLENGE blok R wyświetla wszystkie potrzebne liczby, a student
   wpisuje akapit w ramce „Twój akapit”, w miejsce znacznika w nawiasie
   kwadratowym, poza blokami R. Orientacyjnie: 5 minut startu, 30 LEARN,
   50 CHALLENGE i 5 oddania.
5. **Zapis i wysłanie.** Ctrl+S (w macOS Cmd+S), a potem w konsoli:

   ```r
   oddaj_zadanie("Z01")
   ```

   Funkcja sprawdza komplet akapitów, tworzy aktualny PDF z zapisanego pliku
   i wysyła PDF razem ze źródłami do prywatnego repozytorium. Wysyłkę potwierdza
   komunikat ze zdalnym SHA. Przy błędzie komunikat wskazuje problem; po poprawce
   wysyła się ponownie tym samym poleceniem. `sprawdz_zadanie("Z01")` tworzy
   i sprawdza PDF bez wysyłania.
6. **Koniec zajęć.** `wyloguj_github()` w konsoli, wylogowanie z GitHub
   w przeglądarce i zamknięcie RStudio z wyborem „Don't Save” przy pytaniu
   o `.RData`.
7. **Po zajęciach.** `status_oddania("Z01")` potwierdza odbiór, a
   `pobierz_ocene("Z01")` pobiera ocenę po jej wystawieniu. Brak oceny oznacza
   ocenę jeszcze niewystawioną.
8. **Projekt.** C01–C08 mają podane scenariusze. Na C09 student wybiera jeden
   z 20 scenariuszy i zapisuje własny wariant danych; C10 odczytuje te same dane.
   Po oddaniu Z09 i Z10 polecenie `przygotuj_raport()` tworzy
   `projekty/ilosciowy/raport.Rmd` i przenosi do niego dziesięć własnych akapitów.
   Student redaguje je w jeden tekst, uzupełnia pole P11 (źródła i zakres wsparcia
   narzędzi SI), zapisuje plik i wysyła go poleceniem `oddaj_projekt()`.

## 5. Docker — kto go potrzebuje

- **Student i komputery w sali**: Docker jest zbędny.
- **Prowadzący**: Docker służy wyłącznie do opcjonalnej, niezależnej kontroli
  wybranych oddań. Kontrola uruchamia kod studenta w izolowanym kontenerze Linux
  (bez sieci, bez dostępu do plików prowadzącego, z limitami czasu i pamięci)
  i tworzy osobny PDF kontrolny. Oddany PDF zawiera wyniki i akapity studenta,
  więc do oceny wystarcza on sam.
- **Informatyk** (na komputerze prowadzącego albo osobnym stanowisku kontroli)
  instaluje Docker Desktop z kontenerami Linux (w Windows z WSL 2) albo Docker
  Engine w systemie Linux i buduje obraz kontroli z paczki tej samej wersji
  pakietu:

  ```sh
  git clone --branch v2.3.5 https://github.com/MarekDejaUJ/projektowanie-badan-ZI.git
  cd projektowanie-badan-ZI
  R CMD build --no-build-vignettes .
  Rscript --vanilla tools/kontrola/build.R badaniaZI_2.3.5.tar.gz
  ```

  Ostatnie polecenie wypisuje identyfikator obrazu `sha256:...`, który
  prowadzący podaje funkcji `sprawdz_oddanie()`. Obraz i paczkę zachowuje się
  do końca oceniania rocznika.

Pobranie oddania i kontrolę opisuje [instrukcja kontroli oddań](docs/kontrola-oddan.md).

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
