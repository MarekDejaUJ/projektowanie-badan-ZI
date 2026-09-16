# Projekt indywidualny — plan badania i analiza ilościowa

ID: {{ID}}  
Scenariusz: {{SCENARIUSZ}}  
Rocznik: {{ROCZNIK}}  
Wersja generatora: {{GENERATOR}}

Raport: 1800–2500 słów bez tabel, bibliografii i kwestionariusza. Zastąp każde
`[UZUPELNIJ]` własną odpowiedzią. Odwołuj się do plików rzeczywiście tworzonych
przez `analiza.R`. Nie wklejaj wszystkich komunikatów konsoli.

W `wyniki` zapisz tabele `dziennik.csv`, `opis.csv`, `indeks.csv`, `kanaly.csv`,
`porownanie.csv`, `druga_analiza.csv` oraz wykresy `histogram.png` i `grupy.png`.
Możesz dołączyć dodatkowy wykres rozrzutu lub inne wyniki opisane w tekście.

## 1. Problem instytucji i cel

W 150–250 słowach opisz problem informacyjny i odbiorców. Jakiej decyzji ma
służyć badanie? Co jest planem rzeczywistego badania, a co analizą demonstracyjną?
Zaznacz, że analizowane odpowiedzi są syntetyczne.

[UZUPELNIJ]

## 2. Pytania, hipotezy i plan próby

Podaj 1–2 pytania pasujące do scenariusza. Przynajmniej jedno dotyczy porównania
dwóch grup, drugie związku zmiennych. Dla hipotez wskaż zmienne i przewidywany
kierunek, jeśli masz podstawę do jego przewidzenia. Opisz populację, ramę
doboru, rekrutację, okres odniesienia, możliwą selekcję i ochronę prywatności.
Nie twierdź, że generator stanowi losową próbę rzeczywistych użytkowników.

[UZUPELNIJ]

## 3. Operacjonalizacja i kwestionariusz

Zdefiniuj konstrukt i wskaźniki, ich skale oraz kody. Uzasadnij odwrócenie
pozycji 3 i średni indeks przy minimum pięciu odpowiedziach. Wyjaśnij różnicę
między pojedynczą pozycją a indeksem. Odwołaj się do `kwestionariusz.md`
i `../../dane/slownik.csv`. Jakie ograniczenie ma przykładowa, niewalidowana skala?

[UZUPELNIJ]

## 4. Dane i jawne czyszczenie

Podaj ID, scenariusz, wersję i klucz z manifestu. Podaj N surowe i N po
czyszczeniu. Opisz osobno duplikaty, kod 99, braki i czasy poza udokumentowanym
zakresem. Uzasadnij reguły i wskaż, ile odpowiedzi/komórek każda zmieniła.
Nie zastępuj braków zerami. Surowy plik pozostaw bez zmian.

[UZUPELNIJ]

## 5. Opis próby i pomiaru

Podaj liczebności grup, liczby ważnych odpowiedzi, miary położenia i
zróżnicowania indeksu oraz czasu. Dodaj 2–3 wykresy odpowiednie do skal
z nazwami osi i jednostkami. W pytaniu wielokrotnym podaj mianownik
respondentów i wyjaśnij, dlaczego suma odsetków może przekraczać 100%.
Tabela lub wykres ma podpis i odwołanie w tekście.

[UZUPELNIJ]

## 6. Analiza porównania grup

Uzasadnij test przez pytanie, skalę i niezależność. Podaj N grup, średnie
lub odpowiednie miary, kierunek i wielkość różnicy, przedział ufności,
statystykę, stopnie swobody, jeśli dotyczą metody, oraz p. Wyjaśnij znaczenie
praktyczne w jednostkach indeksu. Brak istotności nie dowodzi braku różnicy.

[UZUPELNIJ]

## 7. Druga analiza

Wykonaj analizę wskazaną w scenariuszu. Dla korelacji podaj wykres, N par,
metodę, współczynnik, p i dostępny przedział; nie twórz fikcyjnych df dla
Spearmana. Dla tabeli podaj liczebności, odsetki w grupach, warunki wyboru
chi-kwadrat/Fishera, wynik testu i V Cramera. Kierunek związku odczytaj
z danych. Wyjaśnij, jak wynik odpowiada drugiemu pytaniu.

[UZUPELNIJ]

## 8. Wnioski, ograniczenia i rekomendacja

Odpowiedz na pytania za pomocą konkretnych liczb. Rozdziel związek,
przewidywanie i przyczynowość. Omów ograniczenie doboru, pomiaru i symulacji.
Zaproponuj jedną warunkową rekomendację i sposób jej sprawdzenia na
rzeczywistych danych; symulacja nie uzasadnia decyzji o instytucji.

[UZUPELNIJ]

## 9. Źródła, wersje i odtworzenie

Podaj wykorzystane źródła z odnośnikami, wersję R, pakietu i generatora.
Napisz, jak otworzyć projekt RStudio i uruchomić zapisany `analiza.R`
w świeżej sesji oraz gdzie powstają wyniki. Nie wymagaj ręcznego
przygotowania obiektów w konsoli. Oddanie identyfikuje pokwitowanie
zdalnego SHA; nie wpisuj tego SHA do pliku, który tworzyłby nowy commit.
Jeśli SI pomagała w zrozumieniu lub poprawieniu kodu, oznacz użyte fragmenty
i opisz zakres pomocy. Interpretacja wyników i wnioski są własną pracą.
Odpowiedź modelu nie zastępuje rzeczywistego źródła.

[UZUPELNIJ]
