# Gotowy skrypt ćwiczenia C09: wspólny przykład S02 (ID demo001), własny wariant
# projektu, karty planu oraz demonstracje selekcji i precyzji. Bloki Rmd
# przypisują wynik funkcji i wywołują print(wynik).

# ---- Prezentacja: tabele, karty i wykresy ----
ustaw_material <- function() {
  knitr::opts_chunk$set(echo=TRUE,message=FALSE,warning=FALSE,
    results='asis',fig.width=6,fig.height=3.3,fig.align='center',fig.pos='H')
  # Rysunek [H] zostaje przy swoim zadaniu także w PDF bez preambuły kursu.
  if(isTRUE(getOption('knitr.in.progress'))&&knitr::is_latex_output())
    knitr::knit_meta_add(list(rmarkdown::latex_dependency('float')))
  invisible(NULL)
}
wydruk_zi <- function(tabele=list(),wykresy=list(),tekst=NULL,digits=3L,karty=list(),markdown=list()) {
  stopifnot(is.list(tabele),is.list(wykresy),is.list(karty),is.list(markdown),
    is.numeric(digits),length(digits)==1L)
  structure(list(tabele=tabele,wykresy=wykresy,tekst=tekst,digits=digits,karty=karty,
    markdown=markdown),class='zi_wydruk')
}
# Karta to tabela Markdown z dwiema kolumnami; długie treści zawijają się w PDF i HTML.
karta_markdown <- function(karta) {
  znak <- function(s) gsub('|','\\|',s,fixed=TRUE)
  tresc <- vapply(karta,function(z) paste(as.character(z),collapse=' '),character(1))
  c('| Element | Treść |','|:-------------|:---------------------------------------------------|',
    paste0('| ',znak(names(karta)),' | ',znak(tresc),' |'))
}
print.zi_wydruk <- function(x,...) {
  dokument <- isTRUE(getOption('knitr.in.progress'))
  for(nazwa in names(x$karty)) {
    karta <- x$karty[[nazwa]]
    if(dokument) cat('\n\n**',nazwa,'**\n\n',paste(karta_markdown(karta),collapse='\n'),'\n\n',sep='')
    else {
      cat('\n',nazwa,'\n',sep='')
      for(pole in names(karta)) cat(pole,': ',karta[[pole]],'\n',sep='')
    }
  }
  for(nazwa in names(x$tabele)) {
    tab <- x$tabele[[nazwa]]
    if(dokument) {
      format <- if(knitr::is_latex_output()) 'latex' else 'html'
      # Brak środowiska float utrzymuje tabelę bezpośrednio przy swoim zadaniu.
      cat('\n\n**',nazwa,'**\n\n',sep='')
      if(format=='latex') cat('\\begin{center}\n')
      tresc <- as.character(knitr::kable(tab,format=format,caption=NULL,
        digits=x$digits,row.names=FALSE,format.args=list(decimal.mark=',')))
      if(format=='html') tresc <- gsub('$','&#36;',tresc,fixed=TRUE)
      cat(tresc,'\n',sep='')
      if(format=='latex') cat('\\end{center}\n')
      cat('\n\n')
    } else {
      cat('\n',nazwa,'\n',sep='')
      print(tab,row.names=FALSE)
    }
  }
  for(md in x$markdown) cat(paste(as.character(md),collapse='\n'),'\n\n',sep='')
  if(!is.null(x$tekst)) {
    if(dokument) cat('\n\n```text\n',paste(x$tekst,collapse='\n'),'\n```\n\n',sep='')
    else cat(paste(x$tekst,collapse='\n'),'\n')
  }
  for(wykres in x$wykresy) print(wykres)
  invisible(x)
}
pokaz_tabele <- function(tabela,tytul='Wynik analizy',digits=3L) {
  wydruk_zi(tabele=setNames(list(tabela),tytul),digits=digits)
}
pokaz_wykres <- function(wykres) wydruk_zi(wykresy=list(wykres))
liczba <- function(x,cyfry=3L) {
  s <- formatC(x,format='f',digits=cyfry,decimal.mark=',')
  # Wartość zaokrąglona do zera traci znak minus; pozostałe ujemne dostają znak −.
  sub('^-','−',sub('^-(0,0*)$','\\1',s))
}

# ---- Wariant projektu: odczyt, przygotowanie i opis ----
identyfikator_materialu <- function() {
  # Render odtwarza ID zapisane w nagłówku roboczego Rmd; sesja używa ID z początku zajęć.
  if(isTRUE(getOption('knitr.in.progress'))) {
    parametry <- get0('params',envir=knitr::knit_global(),inherits=FALSE)
    id <- if(is.list(parametry)) parametry$id_studenta else NULL
  } else {
    id <- get0('ID',envir=globalenv(),inherits=FALSE)
  }
  if(!is.character(id)||length(id)!=1L||is.na(id)||
     !grepl('^[A-Za-z0-9][A-Za-z0-9_-]{1,39}$',id))
    stop('Brak prawidłowego ID. Wykonaj instrukcję początku zajęć; render wymaga ID zapisanego w pliku.')
  id
}
przygotuj_zapisany_wariant <- function(pakiet) {
  porzadek <- badaniaZI::przygotuj_ankiete(pakiet$dane)
  dane <- porzadek$dane
  pozycje <- dane[paste0('pozycja_',1:6)]
  pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
  dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)
  cfg <- pakiet$scenariusz
  # W całym projekcie kontrast to pierwsza grupa z karty minus druga, jawnie podpisany.
  grupy <- rev(unlist(cfg$grupy))
  opis <- do.call(rbind,lapply(grupy,function(g) {
    z <- dane[dane$grupa==g,,drop=FALSE]
    czas <- z$czas_wyszukiwania[is.finite(z$czas_wyszukiwania)]
    data.frame(grupa=g,N_osob=nrow(z),N_indeks=sum(is.finite(z$indeks)),
      srednia=mean(z$indeks,na.rm=TRUE),SD=sd(z$indeks,na.rm=TRUE),
      mediana=median(z$indeks,na.rm=TRUE),N_czas=length(czas),
      maks_czas=if(length(czas)) max(czas) else NA_real_,
      N_wynik=sum(is.finite(z$powodzenie)),row.names=NULL)
  }))
  tabela <- cfg$druga_analiza=='tabela krzyżowa'
  drugie_n <- if(tabela) sum(complete.cases(dane[c('grupa','powodzenie')])) else
    sum(complete.cases(dane[c('indeks',cfg$zmienna_druga)]))
  bilans <- data.frame(Zakres=c('Rekordy surowe','Osoby po usunięciu duplikatów',
    'Analiza 1: osoby z ważnym indeksem','Osoby z ważnym czasem',
    if(tabela) 'Analiza 2: osoby z wartością 0/1' else
      paste('Analiza 2: kompletne pary — indeks i',nazwa_drugiej(cfg,FALSE))),
    N=c(nrow(pakiet$dane),nrow(dane),sum(is.finite(dane$indeks)),
      sum(is.finite(dane$czas_wyszukiwania)),drugie_n))
  structure(list(pakiet=pakiet,dane=dane,pozycje=pozycje,dziennik=porzadek$dziennik,
    opis=opis,bilans=bilans,odniesienie=grupy[1],porownywana=grupy[2]),
    class='zi_projekt')
}
przygotuj_wariant_projektu <- function(scenariusz=NULL,id=identyfikator_materialu()) {
  # Własna praca odczytuje albo zapisuje wariant Z09; podgląd DEMO_C09 i przykład demo001 generują dane.
  pakiet <- if(missing(id) && !identical(id,'DEMO_C09'))
    badaniaZI::wariant_projektu('Z09',scenariusz,id_studenta=id) else
    badaniaZI::generuj_dane(id,if(is.null(scenariusz)) 'S02' else scenariusz,
                          rocznik='2026-27',n=150L)
  przygotuj_zapisany_wariant(pakiet)
}
skrot_do_odczytu <- function(x) {
  # Spacje tylko w wydruku, oryginalny manifest zachowuje pełny ciąg znaków.
  paste(substring(x,seq(1,nchar(x),8),pmin(seq(1,nchar(x),8)+7,nchar(x))),collapse=' ')
}
pole_opisu <- function(cfg,nazwa,domyslnie) {
  x <- cfg[[nazwa]]
  if(is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x)) x else domyslnie
}
nazwa_indeksu <- function(cfg) {
  pole_opisu(cfg,'nazwa_indeksu',paste('Indeks',cfg$konstrukt))
}
nazwa_drugiej <- function(cfg,jednostka=TRUE) {
  if(cfg$zmienna_druga=='czas_wyszukiwania')
    paste0(tolower(pole_opisu(cfg,'nazwa_czasu','Czas zadania')),if(jednostka) ' [min]' else '') else
    paste0('częstość korzystania',if(jednostka) ' [kategorie 1–5]' else '')
}
etykiety_grup <- function(cfg,kody) {
  etykiety <- unlist(cfg$etykiety_grup,use.names=FALSE)
  if(!length(etykiety)) return(as.character(kody))
  stopifnot(length(etykiety)==length(cfg$grupy))
  pos <- match(as.character(kody),unlist(cfg$grupy,use.names=FALSE))
  stopifnot(!anyNA(pos))
  etykiety[pos]
}
kontrast_do_druku <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  paste(etykiety_grup(cfg,projekt$porownywana),'minus',
    etykiety_grup(cfg,projekt$odniesienie))
}
print.zi_projekt <- function(x,...) {
  cfg <- x$pakiet$scenariusz
  m <- x$pakiet$manifest
  karta <- list('Scenariusz'=paste(cfg$id,'—',cfg$tytul),
    'Problem usługi'=cfg$problem,
    'Projekt badania'=pole_opisu(cfg,'projekt_badania','Badanie obserwacyjne dwóch grup.'),
    'Konstrukt indeksu'=nazwa_indeksu(cfg),
    'Pierwsze porównanie'=kontrast_do_druku(x),
    'Druga relacja'=cfg$pytanie_drugie,
    'Klucz wariantu (grupy po 8 znaków)'=skrot_do_odczytu(m$klucz_wariantu),
    'Hash SHA-256 danych (grupy po 8 znaków)'=skrot_do_odczytu(m$hash_danych))
  meta <- data.frame(Pole=c('ID','Scenariusz','Rocznik','Wersja generatora',
    'Rekordy surowe','Wersja R','Dane syntetyczne'),
    Wartosc=c(m$id,m$scenariusz,m$rocznik,m$wersja_generatora,m$n,
      m$wersja_R,if(isTRUE(m$syntetyczne)) 'tak' else 'nie'))
  names(meta)[2] <- 'Wartość'
  print(wydruk_zi(karty=list('Karta wybranego wariantu'=karta),
    tabele=list('Manifest wariantu: pochodzenie danych'=meta)))
  invisible(x)
}
pomiar_wariantu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  pozycje <- as.list(unlist(cfg$pozycje))
  names(pozycje) <- paste('Pozycja',1:6)
  karta <- c(pozycje,list(
    'Kontekst pozycji'=pole_opisu(cfg,'kontekst_pozycji','Wspólny punkt odniesienia dla obu grup.'),
    'Odpowiedzi na pozycje'='1 zdecydowanie nie, 2 raczej nie, 3 ani tak, ani nie, 4 raczej tak, 5 zdecydowanie tak.',
    'Kierunek i kompletność'='Pozycja 3 po odwróceniu 6 − x; indeks jest średnią przy co najmniej 5 z 6 ważnych pozycji.',
    'Definicja grup'=pole_opisu(cfg,'opis_grup',paste(unlist(cfg$grupy),collapse=' / ')),
    'Zadanie obserwacyjne'=pole_opisu(cfg,'zadanie_obserwacyjne','Zadanie opisane w karcie scenariusza.'),
    'Czas'=pole_opisu(cfg,'nazwa_czasu','Czas zadania'),
    'Protokół czasu'=pole_opisu(cfg,'protokol_czasu','Minuty od udostępnienia zadania do zgłoszenia odpowiedzi.'),
    'Częstość korzystania'=pole_opisu(cfg,'opis_czestosci',
      '1 nigdy, 2 rzadko, 3 czasami, 4 często, 5 bardzo często; kategorie porządkowe.'),
    'Treść pola 0/1'=cfg$pytanie_binarne,
    'Źródło pola 0/1'=pole_opisu(cfg,'rodzaj_binarnego','zapisane w karcie scenariusza'),
    'Kod 1'=pole_opisu(cfg,'kod_1','odpowiedź „tak” na pytanie pola 0/1'),
    'Kod 0'=pole_opisu(cfg,'kod_0','odpowiedź „nie” na pytanie pola 0/1'),
    'Granica odczytu pola 0/1'=pole_opisu(cfg,'ograniczenie_binarnego',
      'Wynik dotyczy jednego zadania w określonych warunkach.'),
    'Kanały (odpowiedź wielokrotna)'=paste(unlist(cfg$kanaly),collapse=', ')))
  wydruk_zi(karty=list('Kwestionariusz i pomiary w wybranym scenariuszu'=karta))
}
plan_wariantu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  a <- etykiety_grup(cfg,projekt$porownywana)
  b <- etykiety_grup(cfg,projekt$odniesienie)
  pierwsza <- list('Pomiar'=paste0(nazwa_indeksu(cfg),': średnia sześciu pozycji, skala 1–5'),
    'Grupy i kolejność'=paste(a,'minus',b),
    'Estymanda'=paste0('różnica średnich indeksu w populacji: średnia (',a,') minus średnia (',b,')'),
    'Jednostka efektu'='punkty indeksu 1–5',
    'Hipoteza zerowa'=paste0('H0: średnie w populacji są równe, różnica wynosi 0; test dwustronny'),
    'Tor klasyczny'='test t Welcha z 95% CI różnicy średnich',
    'Tor losowany'='tasowanie etykiet grup (B = 1999) przy wymienialności; bootstrap osób w grupach daje drugi 95% CI',
    'Liczebność'='osoby ze znaną grupą i ważnym indeksem; osobne n każdej grupy')
  druga <- if(cfg$druga_analiza=='tabela krzyżowa')
    list('Pomiar'=paste0('pole 0/1 — ',cfg$pytanie_binarne,' Kod 1: ',
        pole_opisu(cfg,'kod_1','odpowiedź „tak”'),'.'),
      'Grupy i kolejność'=paste(a,'minus',b),
      'Estymanda'=paste0('różnica odsetków kodu 1 w populacji: odsetek (',a,') minus odsetek (',b,')'),
      'Jednostka efektu'='punkty procentowe (pp)',
      'Hipoteza zerowa'='H0: grupa i pole 0/1 są niezależne, różnica odsetków wynosi 0',
      'Tor klasyczny'='test chi-kwadrat (przy liczebności oczekiwanej poniżej 5: test Fishera) i 95% CI różnicy odsetków',
      'Tor losowany'='tabele Monte Carlo przy stałych marginesach (B = 1999); bootstrap osób w grupach daje drugi 95% CI',
      'Miara uzupełniająca'='V Craméra: natężenie związku bez kierunku',
      'Liczebność'='osoby ze znaną grupą i wartością 0/1; odsetki liczone w każdej grupie osobno') else
    list('Pomiar'=paste0('indeks oraz ',nazwa_drugiej(cfg)),
      'Estymanda'=paste0('korelacja rang Spearmana indeksu i zmiennej „',nazwa_drugiej(cfg,FALSE),'” w populacji'),
      'Jednostka efektu'='współczynnik bez jednostki, od −1 do 1',
      'Hipoteza zerowa'=paste0('H0: indeks i ',nazwa_drugiej(cfg,FALSE),' są niezależne, korelacja rang wynosi 0'),
      'Tor klasyczny'='test Spearmana: statystyka S i przybliżone p',
      'Tor losowany'='tasowanie jednej zmiennej względem drugiej (B = 1999); bootstrap całych par daje 95% CI',
      'Liczebność'='kompletne pary: osoby z obiema wartościami')
  wydruk_zi(karty=list('Analiza 1 — porównanie grup'=pierwsza,
    'Analiza 2 — relacja ze scenariusza'=druga))
}
przygotowanie_wariantu <- function(projekt) {
  dziennik <- projekt$dziennik
  dziennik$regula <- gsub('--','–',dziennik$regula,fixed=TRUE)
  names(dziennik) <- c('Reguła','Liczba')
  reguly <- list('Identyczne duplikaty'='z kilku identycznych rekordów zostaje jeden; liczba osób maleje',
    'Kod 99'='w pozycjach ankiety oznacza brak odpowiedzi i zmienia komórkę na NA; osoba zostaje w danych',
    'Czas poza 0–120 min'='zmienia komórkę czasu na NA; osoba zostaje w analizach bez czasu',
    'Długi czas w zakresie 0–120 min'='zostaje jako ważna obserwacja',
    'Pozycja 3'='sformułowana odwrotnie; odwrócenie 6 − x nadaje jej kierunek pozostałych pozycji',
    'Indeks'='średnia sześciu pozycji przy co najmniej 5 ważnych odpowiedziach; przy 4 lub mniej indeks jest brakiem')
  wydruk_zi(karty=list('Reguły gotowego skryptu'=reguly),
    tabele=list('Dziennik przygotowania: wykonane zmiany'=dziennik),digits=0L)
}
bilans_wariantu <- function(projekt) {
  tab <- projekt$opis[c('grupa','N_osob','N_indeks','N_czas','maks_czas','N_wynik')]
  tab$grupa <- etykiety_grup(projekt$pakiet$scenariusz,tab$grupa)
  names(tab) <- c('Grupa','Osoby','Ważny indeks','Ważny czas','Najdłuższy ważny czas [min]','Wartość 0/1')
  wydruk_zi(tabele=list('Bilans liczebności'=projekt$bilans,
    'Dostępne pomiary w grupach'=tab),digits=1L)
}
wykres_indeksu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  d <- projekt$dane[projekt$dane$grupa %in% c(projekt$odniesienie,projekt$porownywana),]
  d <- d[order(match(d$grupa,c(projekt$odniesienie,projekt$porownywana))),]
  badaniaZI::wykres_pudelkowy(d$indeks,etykiety_grup(cfg,d$grupa),
    os=paste0('Indeks [pkt 1–5]'),granica=FALSE,
    tytul=paste(strwrap(paste('Indeks w grupach:',nazwa_indeksu(cfg)),width=52),collapse='\n'))
}
pierwszy_opis_wariantu <- function(projekt,z_wykresem=TRUE) {
  cfg <- projekt$pakiet$scenariusz
  opis <- projekt$opis[c('grupa','N_osob','N_indeks','srednia','SD','mediana')]
  opis$grupa <- etykiety_grup(cfg,opis$grupa)
  names(opis) <- c('Grupa','Osoby','n (ważny indeks)','Średnia','SD','Mediana')
  roznica <- data.frame(Kontrast=kontrast_do_druku(projekt),
    'Różnica średnich [pkt]'=projekt$opis$srednia[2]-projekt$opis$srednia[1],check.names=FALSE)
  wydruk_zi(tabele=list('Opis indeksu w grupach [pkt 1–5]'=opis,
      'Różnica średnich w kolejności kontrastu'=roznica),
    karty=list('Źródło do uzasadnienia planu'=list('Lektura scenariusza'=pole_opisu(cfg,'inspiracja',
      'Wybierz źródło z bibliografii kursu.'))),
    wykresy=if(z_wykresem) list(wykres_indeksu(projekt)) else list())
}

# ---- Karty planu i demonstracje LEARN ----
katalog_projektow <- function() {
  tab <- badaniaZI::scenariusze()
  tematy <- c('Wsparcie biblioteki w wyszukiwaniu','Użyteczność katalogu',
    'Deklarowana znajomość otwartego dostępu','Informacja o wydarzeniach',
    'Przejrzystość procedur w portalu','Przydatność newslettera',
    'Zrozumiałość opisów archiwum','Informacja w kolekcji muzealnej',
    'Nawigacja w BIP','Dokumentacja danych badawczych',
    'Instrukcje korzystania z e-zasobów','Materiały na platformie nauczania',
    'Adekwatność FAQ','Kontrola wyszukiwania przez filtry',
    'Aktualność instrukcji w bazie wiedzy','Spójność informacji miejskiej',
    'Czytelność opisów otwartych danych','Jasność zasad dostępu',
    'Trafność rekomendacji lektur','Informacja o konferencji')
  wiersze <- vapply(seq_len(nrow(tab)),function(i) {
    cfg <- badaniaZI::scenariusz(tab$id[i])
    druga <- if(cfg$druga_analiza=='tabela krzyżowa') 'tabela 2 × 2: grupa i pole 0/1' else
      paste('Spearman: indeks i',nazwa_drugiej(cfg,FALSE))
    paste0('| ',cfg$id,' | ',tematy[i],' | ',paste(unlist(cfg$etykiety_grup),collapse=' / '),
      ' | ',druga,' |')
  },character(1))
  wydruk_zi(markdown=list(c('**Dwadzieścia scenariuszy projektu**','',
    '| ID | Temat | Grupy porównania | Druga analiza |',
    '|:----|:---------------------------|:---------------------------|:--------------------------|',
    wiersze)))
}
karta_pytan_do_odczytu <- function() {
  wiersze <- c('| Pytanie | O ile ocena katalogu nowych różni się od oceny doświadczonych? | O ile odsetek znalezionych dokumentów u nowych różni się od odsetka u doświadczonych? |',
    '| Pomiar | Indeks użyteczności katalogu (deklaracja, 1–5) | Znalezienie wskazanego dokumentu (obserwacja, 0/1) |',
    '| Kolejność grup | nowi minus doświadczeni | nowi minus doświadczeni |',
    '| Estymanda | różnica średnich indeksu w populacji | różnica odsetków kodu 1 w populacji |',
    '| Jednostka efektu | punkty indeksu | punkty procentowe |',
    '| Liczebność | ważny indeks i grupa | ważna wartość 0/1 i grupa |',
    '| Tor klasyczny | test Welcha i CI różnicy | chi-kwadrat albo Fisher i CI różnicy odsetków |',
    '| Tor losowany | tasowanie etykiet grup | tabele przy stałych marginesach |')
  wydruk_zi(markdown=list(c('**Karta pytań S02**','',
    '| Element | Pytanie 1 | Pytanie 2 |',
    '|:-------------|:------------------------------|:------------------------------|',wiersze)))
}
protokol_do_odczytu <- function() {
  wydruk_zi(karty=list('Protokół obserwacji w planowanym badaniu S02'=list(
    'Start pomiaru czasu'='po odczytaniu instrukcji zadania',
    'Koniec pomiaru czasu'='zgłoszenie wyniku albo zakończenie próby',
    'Kryterium kodu 1'='wskazany właściwy dokument i jego lokalizacja lub dostęp',
    'Pomoc'='zapis rodzaju i momentu pomocy obserwatora',
    'Przerwanie próby'='oddzielny kod przerwania; kod 0 oznacza zakończenie bez poprawnego wskazania',
    'Limit czasu'='ustalony przed badaniem; wynik przy limicie oznacza czas co najmniej równy limitowi',
    'Warunki'='urządzenie i przeglądarka odnotowane dla każdej osoby',
    'Jednostka analizy'='osoba z jednym zadaniem')))
}
etapy_projektu <- function() {
  badaniaZI::diagram_etapow(
    c('Problem instytucji','Dwa pytania','Estymandy i hipotezy','Pomiar i próba',
      'Dane i manifest','Dwie analizy','Wnioski i ograniczenia'),
    c('Jaką decyzję ma wesprzeć badanie?','Co dokładnie porównujemy lub wiążemy?',
      'Jaka wielkość w populacji odpowiada pytaniu?','Kogo, jak i czym mierzymy?',
      'Które dane analizujemy i jak je przygotowano?','Efekt, przedział i dwa tory oceny',
      'Co wynika dla instytucji i gdzie jest granica?'),
    c(rep('plan przyszłego badania',4),rep('analiza danych syntetycznych',3)),
    tytul='Tok projektu: plan badania i analiza wariantu')
}
mapa_projektu <- function() {
  mapa <- yaml::read_yaml(system.file('szablony','projekt','przeniesienie.yml',package='badaniaZI'))
  rozdzialy <- c(P01='1. Problem informacyjny i cel',P02='2. Pytania, estymandy i plan analiz',
    P03='3. Populacja, rekrutacja i operacjonalizacja',P04='4. Dane i reguły przygotowania',
    P05='4. Dane i reguły przygotowania',P06='5. Opis próby i pomiarów',
    P07='6. Pierwsza analiza',P08='7. Druga analiza',
    P09='8. Wnioski i ograniczenia',P10='8. Rekomendacja',P11='9. Źródła i odtworzenie')
  zrodlo <- vapply(mapa$odpowiedzi,function(w) paste0(w$zadanie,' ',w$odpowiedz),character(1))
  names(zrodlo) <- vapply(mapa$odpowiedzi,function(w) w$pole,character(1))
  zrodlo['P11'] <- 'nowe pole raportu'
  tab <- data.frame(Pole=names(rozdzialy),'Rozdział raportu'=unname(rozdzialy),
    'Akapit źródłowy'=unname(zrodlo[names(rozdzialy)]),check.names=FALSE)
  pokaz_tabele(tab,'Raport projektu: jedenaście pól i ich źródła',0L)
}
ocena_projektu <- function() {
  tab <- data.frame(Kryterium=c('A01 Problem, pytania, plan próby','A02 Pomiar i indeks',
    'A03 Przygotowanie danych','A04 Opis próby i wykresy','A05 Dwie analizy',
    'A06 Wnioski i rekomendacja','A07 Odtwarzalność i źródła'),
    Pola=c('P01–P03','P03','P04–P05','P06','P02, P07–P08','P09–P10','P11 i pliki pracy'),
    Punkty=c(15,15,15,10,20,15,10))
  pokaz_tabele(tab,'Projekt końcowy: 100 punktów',0L)
}
selekcja <- data.frame(grupa=c('Nowi','Doświadczeni'),N_pop=c(700,300),
  N_zaproszonych=c(100,100),N_odpowiedzi=c(20,80),srednia=c(3,4))
selekcja_do_odczytu <- function() {
  tab <- selekcja
  tab$odsetek <- 100*tab$N_odpowiedzi/tab$N_zaproszonych
  names(tab) <- c('Grupa','Populacja','Zaproszeni','Odpowiedzi','Średnia grupy','Odpowiedzi [% zaproszonych]')
  srednie <- data.frame(Zbiór=c('Cała hipotetyczna populacja','Odpowiadający'),check.names=FALSE,
    Średnia=c(weighted.mean(selekcja$srednia,selekcja$N_pop),
      weighted.mean(selekcja$srednia,selekcja$N_odpowiedzi)))
  wydruk_zi(tabele=list('Hipotetyczna rekrutacja'=tab,'Średnia całości przy dwóch składach'=srednie),
    wykresy=list(badaniaZI::wykres_selekcja(selekcja$grupa,selekcja$N_pop,selekcja$N_odpowiedzi,
      selekcja$srednia,tytul='Te same średnie grup, inny skład, inna średnia całości')),digits=1L)
}
precyzja_do_odczytu <- function(sd=0.8,n=c(25,50,100,200)) {
  tab <- data.frame(n=n,SE=sd*sqrt(2/n),polowa=1.96*sd*sqrt(2/n),razem=2*n)
  names(tab) <- c('n w grupie','SE różnicy [pkt]','Połowa szerokości CI [pkt]','N razem')
  wydruk_zi(tabele=list('Przybliżenie przy SD = 0,8'=tab),
    wykresy=list(badaniaZI::wykres_precyzja(sd,n,os='Połowa szerokości CI [pkt]',
      tytul='Precyzja różnicy średnich a liczebność grupy')),digits=3L)
}

wariant_demo <- przygotuj_wariant_projektu('S02',id='demo001')
