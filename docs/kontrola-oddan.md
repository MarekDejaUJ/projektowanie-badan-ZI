# Kontrola oddań przez prowadzącego

Student zapisuje odpowiedzi w jednym Rmd i używa `oddaj_zadanie()` albo
`oddaj_projekt()`. Docker ani opisane niżej polecenia nie są mu potrzebne.
Potwierdzenie odbioru oznacza zapis na GitHub, nie ocenę merytoryczną.

Prowadzący zachowuje oryginalny PDF, ale odtwarza analizę niezależnie.
Nie uruchamiaj otrzymanego Rmd ani `analiza.R` w sesji z poświadczeniami.
Samo `Rscript --vanilla` nie izoluje systemu plików ani sieci.

## Przygotowanie stanowiska kontroli

Informatyk przygotowuje Docker z obsługą kontenerów Linux i zaufaną paczkę
wersji kursu. Z katalogu źródeł tej samej wersji buduje obraz:

```sh
Rscript --vanilla tools/kontrola/build.R /sciezka/do/badaniaZI_2.0.0.tar.gz
```

Budowa ma dostęp do sieci w celu instalacji narzędzi. Jej kontekst obejmuje
wyłącznie wskazaną paczkę oraz Dockerfile, bez prac i poświadczeń. Zanotuj
zwrócony pełny identyfikator `sha256:...`. Funkcja kontroli wymaga tego
identyfikatora, nie ruchomego tagu; nie buduje ani nie pobiera obrazu sama.
Zachowaj obraz i paczkę przez okres oceniania rocznika.

## Pobranie i wykonanie

Użyj własnego konta właściciela prywatnego repozytorium. ID bierzesz z prywatnego
przydziału, a SHA z pokwitowania właściwego zadania. Wybierz nowy katalog poza
publicznym repozytorium materiałów.

```r
library(badaniaZI)
zaloguj_github()
pobierz_oddanie(
  repo = "prowadzacy/ZI-s017", sha = "PELNY_SHA_Z_POKWITOWANIA",
  id = "Z01", id_studenta = "s017", katalog = "prywatna-kontrola-z01"
)
wyloguj_github()

wynik <- sprawdz_oddanie(
  "prywatna-kontrola-z01", obraz = "sha256:IDENTYFIKATOR_OD_INFORMATYKA"
)
wynik$ok
wynik$katalog
```

Pobranie weryfikuje bajty plików z niezmiennego commitu. Nie wykonuje Rmd,
nie otwiera PDF i nie odczytuje RDS. Właściwa kontrola odbywa się w kontenerze:
bez sieci, jako użytkownik bez praw administratora, z wejściem tylko do odczytu,
bez montowania katalogu domowego lub gniazda Docker, z limitami pamięci,
procesów, czasu i odpowiedzi. Nie ma trybu awaryjnego wykonującego kod na hoście.
Kontener nadal współdzieli jądro z maszyną Linux: utrzymuj Docker/system
wspierane i aktualne; nie traktuj go jako ochrony przed każdą luką systemową.

Każda kontrola ma nowy podkatalog z `kontrola.json` i, po powodzeniu,
`kontrola.pdf`. Oryginalne oddanie pozostaje bez zmian. PDF kontrolny z innego
systemu może różnić się bajtami lub składem; identyczność PDF między systemami
nie jest kryterium oceny. Sprawdź zgodność wyników i własnych akapitów.

Wynik `TRUE` potwierdza techniczne odtworzenie zgodnego pliku, nie poprawność
rozumowania statystycznego. Oceń interpretację według `formularz_oceny()`
i rubryki. Dopiero osobne `wystaw_ocene()` publikuje prywatną informację zwrotną
powiązaną z ocenionym SHA. Katalogi kontroli, raporty oraz dane pozostają prywatne;
nie dodawaj ich do materiałów kursu, publicznych logów ani Pages.
