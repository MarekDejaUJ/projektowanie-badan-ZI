# badaniaZI 2.0.0 — wydanie kursu ZI 2026/27

Wersja 2.0.0 jest przypiętym wydaniem części ilościowej kursu na rok 2026/27.
Wersję do pracy na zajęciach wskazuje prowadzący; nie aktualizuj pakietu
podczas wykonywania zadania.

## Materiały i sposób pracy

- Dziesięć pełnych zestawów ćwiczeń ma po pięć części LEARN i pięć CHALLENGE.
  Student uruchamia gotowe analizy i wpisuje własne krótkie akapity w jednym
  Rmd, poza blokami kodu. Osobny `analiza.R` przechowuje silnik obliczeniowy.
- Pięć pełnych wykładów i ich handouty rozwijają tę samą opowieść o usługach
  informacyjnych, wyszukiwaniu, deklaracjach i obserwowanym wykonaniu zadań.
  Materiały PDF i HTML są dostępne również lokalnie po instalacji pakietu.
- Każde ćwiczenie powtarza instrukcję podania ID, logowania i odtworzenia pracy.
  Start przypomina także uruchamianie bloków R, miejsce odpowiedzi i polecenie
  oddania właściwego zadania.
- Interpretacja obejmuje pytanie badawcze, sens pomiaru, symbole we wzorze,
  liczebność, wielkość efektu i niepewność. W zadaniach inferencyjnych wynik
  klasyczny jest zestawiony z wynikiem permutacyjnym. Dane pozostają syntetyczne.

## Projekt indywidualny

- C01–C08 korzystają z podanych scenariuszy. Własny projekt zaczyna się na C09:
  wybór scenariusza i zapis wariantu poprzedzają jego interpretację.
- C10 i raport korzystają z tego samego zapisanego wariantu, bez ponownego
  losowania danych. Opisy dwudziestu scenariuszy rozróżniają czas, źródło
  pomiaru, znaczenie wyniku 0/1 i treść indeksu.
- `przygotuj_raport()` przenosi dziesięć własnych odpowiedzi z Z09 i Z10 tylko
  przy pierwszym utworzeniu raportu. Student porządkuje tekst i uzupełnia P11
  z bibliografią oraz opisem zakresu wsparcia. Ponowienie nie nadpisuje pracy.

## Oddawanie i informacja zwrotna

- `oddaj_zadanie()` i `oddaj_projekt()` tworzą aktualny PDF z zapisanego Rmd
  przed wysyłką. Prowadzący otrzymuje wyniki i własne akapity studenta oraz
  źródła potrzebne do odtworzenia pracy.
- Kontrola odróżnia kompletność pracy od oceny merytorycznej. Pokwitowanie
  zawiera SHA konkretnej wersji oraz czas odbioru z GitHub; sam lokalny PDF
  lub commit nie potwierdza oddania.
- Ponowna wysyłka identycznych plików zachowuje pierwotne pokwitowanie także
  po oddaniu innych zadań. `status_oddania()` i `pobierz_ocene()` domyślnie
  odnajdują właściwe zadanie po numerze, bez konieczności pamiętania SHA.
  Poprawiona wersja bez oceny nie dziedziczy oceny poprzedniej wersji.
- Wysyłana jest jawna lista plików; sprawdzana jest także niewysłana historia
  Git. Konflikt nie uruchamia wymuszonego nadpisania ani kasowania odpowiedzi.
- Prowadzący może pobrać konkretne oddanie przez `pobierz_oddanie()` i sprawdzić
  je oddzielnie przez `sprawdz_oddanie()` w przygotowanym kontenerze Docker.
  Taka kontrola nie przyznaje punktów za interpretację.

## Zgodność i przygotowanie stanowiska

- Wymagane jest R co najmniej 4.3. Informatyk zapewnia pakiet z zależnościami,
  Pandoc i XeLaTeX. Student nie instaluje tych narzędzi przy oddawaniu zadania.
- Stare formularze Markdown i wcześniejszy tor analiz CSV nie są podstawą
  nowych zadań. Prace z wersji 1.x nie są automatycznie migrowane: zachowaj
  ich kopię i uzgodnij przejście z prowadzącym.
- Generator pozostaje w wersji 1.0.0, opisy scenariuszy mają wersję 1.1.0.
  Wersje materiałów i rubryk są zapisane w konfiguracji rocznika; wagi zadań
  i projektu nie zostały zmienione.
