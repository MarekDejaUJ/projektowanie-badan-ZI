# Projektowanie badań — Zarządzanie informacją

Pakiet `badaniaZI` organizuje część ilościową kursu w roku 2026/27: pracę
lokalną w RStudio, indywidualne dane syntetyczne, materiały, zadania, rubryki
oraz oddawanie z konsoli R do prywatnego repozytorium GitHub.

Katalog `materialy()` wskazuje jednostki dostępne w zainstalowanej wersji.
Kurs obejmuje 5 wykładów i 10 ćwiczeń po 90 minut z pełną treścią HTML/PDF.
Każde ćwiczenie ma co najmniej 15 stron, jeden roboczy Rmd i gotowy silnik
`analiza.R`. Pięć części LEARN pokazuje odczytywanie i opisywanie wyników;
pięć CHALLENGE wymaga własnych pisemnych interpretacji. Student pracuje
indywidualnie przed komputerem, z możliwością konsultacji z prowadzącym.
Osobne handouty HTML/PDF są przeznaczone dla pięciu wykładów.

Materiały online: <https://MarekDejaUJ.github.io/projektowanie-badan-ZI/>

## Instalacja na własnym komputerze

W sali pakiet instaluje informatyk. Na własnym komputerze z R ≥ 4.3:

```r
install.packages("remotes")
remotes::install_github("MarekDejaUJ/projektowanie-badan-ZI",
                      dependencies = NA, upgrade = "never")
```

Publiczny pakiet można pobrać bez logowania do prywatnej pracy.
Instaluj wersję wskazaną dla swojego rocznika; nie aktualizuj pakietu w trakcie
rozwiązywania zadania. Polecenie powyżej pobiera bieżącą gałąź repozytorium.
Do oddania własnego PDF potrzebne są również Pandoc i XeLaTeX; nie instaluje
ich polecenie `oddaj_zadanie()`. W sali cały zestaw przygotowuje informatyk.
Kontrola `sprawdz_srodowisko()` pokazuje dostępne narzędzia, niczego nie instaluje.

## Przygotowanie przez prowadzącego

Prowadzący przygotowuje zwykłe prywatne repozytorium i zaproszenie bez
organizacji GitHub. Po zalogowaniu do własnego konta wykonuje:

```r
przygotuj_repo_studenta("s017", "login-studenta")
```

Funkcja umieszcza w repozytorium konfigurację technicznej przestrzeni ćwiczeń.
Nie losuje danych ani nie rozpoczyna projektu badawczego. Student przyjmuje
zaproszenie przed C01; własny scenariusz projektu poznaje i wybiera na C09.

## Praca i materiały

```r
library(badaniaZI)
sprawdz_srodowisko()
materialy()
otworz_material("C01")
```

Materiały lokalne i obliczenia działają bez sieci. Czytanie gotowych PDF/HTML
nie wymaga LaTeX. Tworzenie własnego PDF przy oddawaniu wymaga narzędzi
wymienionych w instrukcji instalacji.

## Początek każdych ćwiczeń

Przygotuj konto GitHub i 2FA przed C01. Przyjmij zaproszenie do zwykłego
prywatnego repozytorium przygotowanego przez prowadzącego; kurs nie wymaga
organizacji GitHub. Na czyszczonym komputerze użyj otrzymanego adresu:

```r
library(badaniaZI)
ID <- "TWOJE_ID"
zaloguj_github()
rozpocznij_zajecia(
  "C01", id_studenta = ID,
  repo_url = "ADRES_TWOJEGO_PRYWATNEGO_REPOZYTORIUM"
)
```

Hasło oraz drugi składnik podaje się wyłącznie na stronie GitHub. Logowanie
z R korzysta z GitHub CLI zainstalowanego w sali; wariant device wymaga
skonfigurowanej aplikacji. Wpisz własne ID i otrzymany adres, a `C01` zastąp
numerem aktualnych ćwiczeń. Pełna instrukcja powtarza się na początku
każdego zestawu; nie trzeba pamiętać poleceń z poprzedniego spotkania.

Funkcja otwiera własny `zadanie.Rmd` w bieżącej sesji. Nie otwieraj następnie
`.Rproj`, bo przełączenie projektu uruchomi nową sesję i utracisz logowanie.
Uruchom pierwszy chunk, który wczytuje `analiza.R`, a następnie gotowe chunki
LEARN i CHALLENGE. Wynik pojawia się jako tabela lub wykres. Pod każdym
CHALLENGE wpisz swój akapit w oznaczonym miejscu **poza chunkiem**,
zastępując znacznik odpowiedzi. Zmieniaj tylko jawnie wskazane parametry
w Rmd, nie zawartość silnika `analiza.R`.

Orientacyjny podział 90 minut: 5 minut na start, 30 na LEARN, 50 na pięć
CHALLENGE i 5 na oddanie. Dodatkowe przykłady i rozszerzenia nie są
dodatkowymi obowiązkowymi odpowiedziami.

## Oddanie zapisanej pracy

Zapisz Rmd (Ctrl+S; na macOS Cmd+S). W konsoli wystarczy:

```r
oddaj_zadanie("Z01")
```

Funkcja sprawdza komplet odpowiedzi, tworzy świeży PDF z zapisanego Rmd
w osobnym procesie R i wysyła jawny komplet plików do prywatnego repozytorium.
Prowadzący otrzymuje PDF z wynikami i własnymi akapitami studenta oraz źródła
potrzebne do odtworzenia pracy. Nie trzeba osobno eksportować tabel ani
przepisywać odpowiedzi do drugiego formularza.

Pokwitowanie zawiera zdalne SHA i czas serwera. Odbiór i ocena są osobnymi
informacjami: brak oceny nie oznacza zera punktów. Jeśli wysyłka się nie uda,
zachowaj lokalny katalog; sam utworzony PDF nie potwierdza odbioru przez GitHub.
Opcjonalnie `sprawdz_zadanie("Z01")` tworzy i sprawdza PDF bez wysyłania,
`status_oddania("Z01")` sprawdza odbiór, a `pobierz_ocene("Z01")` pobiera feedback.
Przed opuszczeniem sali wykonaj `wyloguj_github()` i wyloguj także przeglądarkę.

## Projekt od C09 do raportu

C01–C08 mają podane scenariusze do nauki interpretacji; nie wymagają pracy
nad nieznanym jeszcze projektem. Na C09 wybierasz scenariusz, generujesz
własny syntetyczny wariant i zapisujesz plan. C10 odczytuje dokładnie te same
dane: nie losuj ich ponownie. Po wykonaniu obu zadań:

```r
przygotuj_raport()
```

Otwórz plik `projekty/ilosciowy/raport.Rmd` w swojej przestrzeni ćwiczeń.
Funkcja tworzy go i jednorazowo przenosi dziesięć własnych odpowiedzi
Z09/Z10 do odpowiednich części. Uporządkuj je w spójny raport, uzupełnij P11
(bibliografia i zakres wsparcia), uruchom gotowe analizy, zapisz plik i użyj
`oddaj_projekt()`. Ponowne przygotowanie nie nadpisuje już istniejącego raportu.

## Ocena i terminy 2026/27

Prowadzący może niezależnie odtworzyć oddanie w izolowanym środowisku;
[instrukcja kontroli](docs/kontrola-oddan.md) opisuje pobranie konkretnego SHA
i osobny PDF kontrolny. Student nie potrzebuje Docker ani dodatkowych poleceń.

Projekt indywidualny ilościowy stanowi 50% oceny, dziesięć jednakowo ważonych
zadań łącznie 50% (każde 5%). Projekt obejmuje plan badania i analizę danych.
Nie ma egzaminu; ocena wybranej realizacji jest oceną końcową.

Z01–Z09 są oddawane do rozpoczęcia kolejnych ćwiczeń, Z10 do 26.01.2027
o 10:30. Projekt do 27.01.2027; pierwsze spóźnione oddanie do 10.02.2027
z maksymalną oceną 4,5. Oceniony projekt można poprawiać do 24.02.2027.
Pełny kalendarz spotkań pozostaje do ustalenia. Daty, wagi, progi i rubryki
są wersjonowane w pakiecie; sprawdź `konfiguracja_kursu()`, `termin_zadania()`
i ogłoszenie na platformie UJ.

MIT. Dane demonstracyjne są syntetyczne i nie stanowią dowodu o rzeczywistych
instytucjach ani respondentach.

## Pomoc w kodzie

Generatywna SI może pomagać w zrozumieniu i poprawianiu kodu. Interpretację wyników i wnioski napisz samodzielnie; oznacz użyte fragmenty i sprawdź ich działanie. Korzystanie z SI jest dobrowolne. Obowiązują także [zasady UJ](https://bip.uj.edu.pl/documents/1384597/159749054/zarz_115_2025.pdf).
