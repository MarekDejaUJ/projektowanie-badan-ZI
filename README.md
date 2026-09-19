# Projektowanie badań — Zarządzanie informacją

Pakiet `badaniaZI` organizuje część ilościową kursu w roku 2026/27: pracę
lokalną w RStudio, indywidualne dane syntetyczne, materiały, zadania, rubryki
oraz oddawanie z konsoli R do prywatnego repozytorium GitHub.

Katalog `materialy()` wskazuje jednostki dostępne w zainstalowanej wersji.
Wydanie obejmuje 5 wykładów i 10 ćwiczeń po 90 minut z pełną treścią HTML/PDF
i gotowym skryptem R. Każdy zestaw ćwiczeniowy ma 15–20 stron i prowadzi od
pytania badawczego przez odczyt wyniku do samodzielnej interpretacji.
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

Prowadzący przygotowuje zwykłe prywatne repozytorium i zaproszenie bez
organizacji GitHub. Po zalogowaniu do własnego konta wykonuje:

```r
przygotuj_repo_studenta("s017", "login-studenta", "S02")
```

Funkcja umieszcza w repozytorium indywidualne dane, gotowe skrypty, szablony
raportu i kontrolę oddania. Student przyjmuje zaproszenie przed C01.

## Praca i materiały

```r
library(badaniaZI)
sprawdz_srodowisko()
materialy()
otworz_material("C01")
utworz_projekt("s017", "S02", katalog = "moje-badania-lokalnie")
```

Przykładowe ID zastąp pseudonimem otrzymanym na zajęciach. Materiały lokalne,
generator i obliczenia działają bez sieci. Gotowe PDF/HTML nie wymagają LaTeX.

## Odtworzenie i oddanie

Przygotuj konto GitHub i 2FA przed C01. Przyjmij zaproszenie do zwykłego
prywatnego repozytorium przygotowanego przez prowadzącego; kurs nie wymaga
organizacji GitHub. Na czyszczonym komputerze użyj otrzymanego adresu:

```r
zaloguj_github()
rozpocznij_zajecia("C01", "s017",
  "https://github.com/MarekDejaUJ/ZI-s017.git")
```

Hasło oraz drugi składnik podaje się wyłącznie na stronie GitHub. Logowanie
z R korzysta z GitHub CLI zainstalowanego w sali; wariant device wymaga
skonfigurowanej aplikacji. Otwórz główny `.Rproj`. W przygotowanym `analiza.R`
zmieniaj wyłącznie wskazane parametry, wykonaj cały skrypt od początku i
uzupełnij interpretację w pliku Markdown. Następnie uruchom:

```r
sprawdz_zadanie("Z01")
oddaj_zadanie("Z01")
status_oddania("Z01")
pobierz_ocene("Z01")
wyloguj_github()
```

Pokwitowanie zawiera zdalne SHA i czas serwera. Odbiór, kontrola kodu i ocena
są osobnymi informacjami. Brak oceny nie oznacza zera punktów. Wyloguj też
przeglądarkę przed opuszczeniem sali.

## Ocena i terminy 2026/27

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
