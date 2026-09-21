dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_',1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)
cfg <- badaniaZI::scenariusz('S02')
opis_grup <- do.call(rbind,lapply(c('doświadczeni','nowi'),function(g) {
  z <- dane[dane$grupa==g,]
  data.frame(grupa=g,N_osob=nrow(z),N_indeks=sum(!is.na(z$indeks)),
    srednia_indeks=mean(z$indeks,na.rm=TRUE),SD_indeks=sd(z$indeks,na.rm=TRUE),
    N_czas=sum(!is.na(z$czas_wyszukiwania)),N_sukces=sum(!is.na(z$powodzenie)))
}))
karta_pytan <- data.frame(element=c('Pytanie','Wynik','Porównanie','Estymanda',
  'Jednostka efektu','N analizy','Klasyczny wynik','Losowanie'),
  P1=c('Różnica oceny katalogu','Indeks użyteczności','Nowi minus doświadczeni',
    'Różnica średnich','Punkty indeksu','Ważny indeks i grupa',
    'Welch i CI różnicy','Tasowanie etykiet grup'),
  P2=c('Różnica skuteczności','Sukces 0/1','Nowi minus doświadczeni',
    'Różnica proporcji','Punkty procentowe','Ważny sukces i grupa',
    'Chi-kwadrat / Fisher','Tabele przy stałych marginesach'))
# Przybliżenia do dyskusji planistycznej; nie są analizą mocy ani obietnicą dokładnego CI.
precyzja <- data.frame(N_grupy=c(25,50,100,200))
precyzja$SE_roznicy <- .8*sqrt(2/precyzja$N_grupy)
precyzja$polszerokosc_95 <- 1.96*precyzja$SE_roznicy
precyzja$N_razem <- 2*precyzja$N_grupy
wykres_precyzji <- ggplot2::ggplot(precyzja,ggplot2::aes(N_razem,polszerokosc_95))+
  ggplot2::geom_line(colour='#0072B2')+ggplot2::geom_point(size=2,colour='#D55E00')+
  ggplot2::labs(x='Łączne N dwóch równych grup',y='Przybliżona połowa szerokości CI [pkt]',
    title='Planowanie precyzji przy założonym SD=0,8')+badaniaZI::theme_zi()
selekcja <- data.frame(grupa=c('Nowi','Doświadczeni'),N_pop=c(700,300),
  N_zaproszonych=c(100,100),N_odpowiedzi=c(20,80),srednia_przyjeta=c(3,4))
selekcja$odsetek_odpowiedzi <- 100*selekcja$N_odpowiedzi/selekcja$N_zaproszonych
srednie_selekcji <- data.frame(zakres=c('Cała hipotetyczna populacja','Próba odpowiadających'),
  srednia=c(weighted.mean(selekcja$srednia_przyjeta,selekcja$N_pop),
    weighted.mean(selekcja$srednia_przyjeta,selekcja$N_odpowiedzi)))
protokol <- data.frame(element=c('Start','Koniec','Sukces','Pomoc','Przerwa',
  'Limit czasu','Urządzenie','Jednostka'),
  zapis=c('Po odczytaniu instrukcji','Zgłoszenie wyniku lub zakończenia próby',
    'Wskazany dokument i poprawna lokalizacja','Zapis rodzaju i momentu',
    'Oddzielny kod, nie sukces=0 bez sprawdzenia','Jawny przed badaniem; cenzorowanie',
    'Warunki odnotowane','Osoba, jedno zadanie'))
reguly <- data.frame(problem=c('Identyczny duplikat','Kod 99','Czas poza zakresem',
  'Długi prawidłowy czas','Brak pozycji','Brak sukcesu','Wielokrotne kanały'),
  decyzja=c('Zachowaj jeden rekord zgodnie z regułą',
    'NA tylko w polu objętym słownikiem','Oznacz i udokumentuj jako brak',
    'Zachowaj; sprawdź protokół','Indeks przy minimum 5 z 6',
    'Wyłącz z tabeli, nie zamieniaj na zero','Jawny wspólny mianownik osób'))
budzet <- data.frame(element=c('Plan i pytania','Pomiar','Czyszczenie','Opis',
  'Dwie analizy','Wnioski','Odtworzenie'),punkty=c(15,15,15,10,20,15,10))
wykres_grup <- ggplot2::ggplot(dane,ggplot2::aes(grupa,indeks,fill=grupa))+
  ggplot2::geom_boxplot(width=.55,alpha=.7,na.rm=TRUE)+
  ggplot2::scale_fill_manual(values=c('#0072B2','#D55E00'))+
  ggplot2::labs(x='Grupa',y='Indeks użyteczności [1–5]',
    title='Pierwszy opis S02, jeszcze nie odpowiedź o przyczynach')+
  badaniaZI::theme_zi()+ggplot2::theme(legend.position='none')

# Warstwa prezentacji: w Rmd wystarczy przypisanie oraz print(wynik).
ustaw_material <- function() {
  knitr::opts_chunk$set(echo=TRUE,message=FALSE,warning=FALSE,
    results='asis',fig.width=6,fig.height=3.3,fig.align='center')
  invisible(NULL)
}
wydruk_zi <- function(tabele=list(),wykresy=list(),tekst=NULL,digits=3L,karty=list()) {
  stopifnot(is.list(tabele),is.list(wykresy),is.numeric(digits),length(digits)==1L)
  structure(list(tabele=tabele,wykresy=wykresy,tekst=tekst,digits=digits,karty=karty),
    class='zi_wydruk')
}
print.zi_wydruk <- function(x,...) {
  dokument <- isTRUE(getOption('knitr.in.progress'))
  for(nazwa in names(x$karty)) {
    if(dokument) cat('\n\n**',nazwa,'**\n\n',sep='') else cat('\n',nazwa,'\n',sep='')
    karta <- x$karty[[nazwa]]
    for(pole in names(karta)) {
      if(dokument) cat('**',pole,':** ',karta[[pole]],'\n\n',sep='')
      else cat(pole,': ',karta[[pole]],'\n',sep='')
    }
  }
  for(nazwa in names(x$tabele)) {
    tab <- x$tabele[[nazwa]]
    if(dokument) {
      format <- if(knitr::is_latex_output()) 'latex' else 'html'
      # Podpis jest Markdownem, więc znaki takie jak % są escapowane przez Pandoc.
      # Brak środowiska float utrzymuje tabelę bezpośrednio przy swoim zadaniu.
      cat('\n\n**',nazwa,'**\n\n',sep='')
      if(format=='latex') cat('\\begin{center}\n')
      tresc <- as.character(knitr::kable(tab,format=format,caption=NULL,
        digits=x$digits,row.names=FALSE))
      if(format=='html') tresc <- gsub('$','&#36;',tresc,fixed=TRUE)
      cat(tresc,'\n',sep='')
      if(format=='latex') cat('\\end{center}\n')
      cat('\n\n')
    } else {
      cat('\n',nazwa,'\n',sep='')
      print(tab,row.names=FALSE)
    }
  }
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
przygotuj_wariant_projektu <- function(scenariusz='S02',id=identyfikator_materialu()) {
  pakiet <- if(missing(id) && !identical(id,'DEMO_C09'))
    badaniaZI::wariant_projektu('Z09',scenariusz,id_studenta=id) else
    badaniaZI::generuj_dane(id,scenariusz,rocznik='2026-27',n=150L)
  porzadek <- badaniaZI::przygotuj_ankiete(pakiet$dane)
  dane <- porzadek$dane
  pozycje <- dane[paste0('pozycja_',1:6)]
  pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
  dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)
  # W całym projekcie: pierwsza grupa z karty minus druga, jawnie podpisane.
  grupy <- rev(unlist(pakiet$scenariusz$grupy))
  opis <- do.call(rbind,lapply(grupy,function(g) {
    z <- dane[dane$grupa==g,,drop=FALSE]
    data.frame(grupa=g,N_osob=nrow(z),N_indeks=sum(is.finite(z$indeks)),
      srednia=mean(z$indeks,na.rm=TRUE),SD=sd(z$indeks,na.rm=TRUE),
      N_czas=sum(is.finite(z$czas_wyszukiwania)),
      N_wynik=sum(is.finite(z$powodzenie)),row.names=NULL)
  }))
  drugie_n <- if(pakiet$scenariusz$druga_analiza=='tabela krzyżowa')
    sum(complete.cases(dane[c('grupa','powodzenie')])) else
    sum(complete.cases(dane[c('indeks',pakiet$scenariusz$zmienna_druga)]))
  bilans <- data.frame(zakres=c('Rekordy surowe','Osoby po usunięciu duplikatów',
    'Ważny indeks','Ważny czas','Dane do drugiego pytania'),
    N=c(nrow(pakiet$dane),nrow(dane),sum(is.finite(dane$indeks)),
      sum(is.finite(dane$czas_wyszukiwania)),drugie_n))
  structure(list(pakiet=pakiet,dane=dane,pozycje=pozycje,dziennik=porzadek$dziennik,
    opis=opis,bilans=bilans,odniesienie=grupy[1],porownywana=grupy[2]),
    class='zi_projekt')
}
skrot_do_odczytu <- function(x) {
  # Spacje tylko w wydruku, oryginalny manifest zachowuje pełny SHA-256.
  paste(substring(x,seq(1,nchar(x),8),pmin(seq(1,nchar(x),8)+7,nchar(x))),collapse=' ')
}
print.zi_projekt <- function(x,...) {
  cfg <- x$pakiet$scenariusz
  m <- x$pakiet$manifest
  karta <- list('Scenariusz'=paste(cfg$id,cfg$tytul),
    'Problem usługi'=cfg$problem,
    'Konstrukt indeksu'=nazwa_indeksu(cfg),
    'Kontrast pierwszego porównania'=kontrast_do_druku(x),
    'Druga relacja'=cfg$pytanie_drugie,
    'Wersja opisu pomiaru'=pole_opisu(cfg,'wersja_opisu','starsza karta bez odrębnego numeru'),
    'Klucz wariantu (grupy znaków)'=skrot_do_odczytu(m$klucz_wariantu),
    'SHA-256 danych (grupy znaków)'=skrot_do_odczytu(m$hash_danych))
  meta <- data.frame(pole=c('ID','Scenariusz','Rocznik','Generator',
    'Rekordy surowe','Wersja R','Syntetyczne'),
    wartosc=c(m$id,m$scenariusz,m$rocznik,m$wersja_generatora,m$n,
      m$wersja_R,as.character(m$syntetyczne)))
  print(wydruk_zi(karty=list('Karta wybranego wariantu'=karta),
    tabele=list('Manifest: pochodzenie, nie jakość pomiaru'=meta)))
  invisible(x)
}
katalog_projektow <- function() {
  tab <- badaniaZI::scenariusze()
  tab$tytul <- c('Wsparcie biblioteki w wyszukiwaniu','Użyteczność katalogu',
    'Deklarowana znajomość otwartego dostępu','Informacja o wydarzeniach',
    'Przejrzystość procedur w portalu','Przydatność newslettera',
    'Zrozumiałość opisów archiwum','Informacja w kolekcji muzealnej',
    'Nawigacja w BIP','Dokumentacja danych badawczych',
    'Instrukcje korzystania z e-zasobów','Materiały na platformie nauczania',
    'Adekwatność FAQ','Kontrola wyszukiwania przez filtry',
    'Aktualność instrukcji w bazie wiedzy','Spójność informacji miejskiej',
    'Czytelność opisów otwartych danych','Jasność zasad dostępu',
    'Trafność rekomendacji lektur','Informacja o konferencji')
  tab$druga_analiza <- ifelse(tab$druga_analiza=='tabela krzyżowa','Tabela 2 na 2','Spearman')
  names(tab) <- c('ID','Temat','Druga analiza')
  pokaz_tabele(tab,'Dwadzieścia scenariuszy; w każdym pierwsza analiza porównuje indeks')
}
pole_opisu <- function(cfg,nazwa,domyslnie) {
  x <- cfg[[nazwa]]
  if(is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x)) x else domyslnie
}
nazwa_indeksu <- function(cfg) {
  pole_opisu(cfg,'nazwa_indeksu',paste('Indeks:',cfg$konstrukt,'(deklaracje)'))
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
grupy_do_druku <- function(projekt,tabela) {
  tabela$grupa <- etykiety_grup(projekt$pakiet$scenariusz,tabela$grupa)
  tabela
}
pomiar_wariantu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  pozycje <- as.list(unlist(cfg$pozycje))
  names(pozycje) <- paste('Pozycja',1:6)
  karta <- c(pozycje,list(
    'Kontekst pozycji'=pole_opisu(cfg,'kontekst_pozycji','Ustal wspólny punkt odniesienia dla obu grup.'),
    'Odpowiedzi na pozycje'='1 zdecydowanie nie, 2 raczej nie, 3 ani tak, ani nie, 4 raczej tak, 5 zdecydowanie tak.',
    'Kierunek i kompletność'='Pozycja 3: 6-x po obsłudze braków. Średnia przy minimum 5 z 6 ważnych pozycji.',
    'Definicja grup'=pole_opisu(cfg,'opis_grup',paste(unlist(cfg$grupy),collapse=' / ')),
    'Częstość korzystania'=pole_opisu(cfg,'opis_czestosci',
      '1 nigdy, 2 rzadko, 3 czasami, 4 często, 5 bardzo często; kategorie porządkowe.'),
    'Czas'=pole_opisu(cfg,'nazwa_czasu','Czas zadania'),
    'Protokół czasu'=pole_opisu(cfg,'protokol_czasu',
      'Minuty; techniczny zakres 0–120 nie definiuje startu, końca ani postępowania przy przerwie.'),
    'Treść pola 0/1'=cfg$pytanie_binarne,
    'Źródło pola 0/1'=pole_opisu(cfg,'rodzaj_binarnego','Odczytaj źródło w zapisanej karcie scenariusza.'),
    'Kod 1'=pole_opisu(cfg,'kod_1','Tak dla treści pytania; nie zawsze oznacza sukces.'),
    'Kod 0'=pole_opisu(cfg,'kod_0','Nie dla treści pytania; nie oznacza braku odpowiedzi.'),
    'Granica odczytu'=pole_opisu(cfg,'ograniczenie_binarnego',
      'Deklaracja nie jest obserwacją, a nieznany wynik nie jest kodem 0.'),
    'Kanały'=paste(unlist(cfg$kanaly),collapse=', ')))
  wydruk_zi(karty=list('Kwestionariusz i pomiary w wybranym scenariuszu'=karta))
}
plan_wariantu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  pierwsza <- list('Wynik'=nazwa_indeksu(cfg),
    'Kontrast'=kontrast_do_druku(projekt),
    'Efekt i CI'='Różnica średnich w punktach indeksu; CI Welcha oraz bootstrapu osób w grupach.',
    'Dwa tory'='Test Welcha; tasowanie etykiet grup z jawnym założeniem wymienialności.',
    'N'='Osoby ze znaną grupą i ważnym indeksem, osobne N grup.')
  druga <- if(cfg$druga_analiza=='tabela krzyżowa')
    list('Zmienne'=paste('Grupa i odpowiedź 0/1:',cfg$pytanie_binarne),
      'Efekt i CI'='Różnica proporcji kodu 1 w punktach procentowych i CI; V osobno, bez kierunku.',
      'Dwa tory'='Chi-kwadrat albo Fisher według oczekiwań; tabele Monte Carlo o stałych marginesach.',
      'N'='Osoby ze znaną grupą i wynikiem 0/1; procenty liczone w grupach.') else
    list('Zmienne'=paste('Indeks oraz',cfg$zmienna_druga),
      'Efekt i CI'='Korelacja rangowa Spearmana, bez jednostki; CI bootstrapu całych par.',
      'Dwa tory'='Przybliżony test Spearmana oraz tasowanie jednej zmiennej pod niezależnością i wymienialnością.',
      'N'='Kompletne pary należące do tych samych osób.')
  wydruk_zi(karty=list('Plan analizy 1 — porównanie grup'=pierwsza,
    'Plan analizy 2 — relacja ze scenariusza'=druga))
}
przygotowanie_wariantu <- function(projekt) {
  pokaz_tabele(projekt$dziennik,'Reguły przygotowania wykonane przez gotowy skrypt',0L)
}
bilans_wariantu <- function(projekt) {
  tab <- grupy_do_druku(projekt,projekt$opis[c('grupa','N_osob','N_indeks','N_czas','N_wynik')])
  wydruk_zi(tabele=list('Bilans analiz'=projekt$bilans,'Dostępne pomiary w grupach'=tab),digits=0L)
}
pierwszy_opis_wariantu <- function(projekt,z_wykresem=TRUE) {
  plot <- ggplot2::ggplot(projekt$dane,ggplot2::aes(grupa,indeks,fill=grupa))+
    ggplot2::geom_boxplot(width=.55,alpha=.7,na.rm=TRUE)+
    ggplot2::scale_fill_manual(values=c('#0072B2','#D55E00'))+
    ggplot2::scale_x_discrete(labels=function(x)
      vapply(etykiety_grup(projekt$pakiet$scenariusz,x),function(z)
        paste(strwrap(z,width=22),collapse='\n'),character(1)))+
    ggplot2::labs(x='Grupa',y='Indeks [pkt 1–5]',
      title=paste(strwrap(nazwa_indeksu(projekt$pakiet$scenariusz),width=48),collapse='\n'))+
    badaniaZI::theme_zi()+ggplot2::theme(legend.position='none')
  wydruk_zi(tabele=list('Opis indeksu w grupach'=
    grupy_do_druku(projekt,projekt$opis[c('grupa','N_osob','N_indeks','srednia','SD')])),
    wykresy=if(z_wykresem) list(plot) else list())
}
karta_pytan_do_odczytu <- function() {
  pierwsza <- as.list(karta_pytan$P1)
  druga <- as.list(karta_pytan$P2)
  names(pierwsza) <- names(druga) <- karta_pytan$element
  wydruk_zi(karty=list('S02 — pytanie 1'=pierwsza,'S02 — pytanie 2'=druga))
}
protokol_do_odczytu <- function() {
  karta <- as.list(protokol$zapis)
  names(karta) <- protokol$element
  wydruk_zi(karty=list('Elementy planowanego protokołu obserwacji'=karta))
}
reguly_do_odczytu <- function() {
  karta <- as.list(reguly$decyzja)
  names(karta) <- reguly$problem
  wydruk_zi(karty=list('Reguły przed obejrzeniem p'=karta))
}
selekcja_do_odczytu <- function() {
  tab <- selekcja
  names(tab) <- c('Grupa','Populacja','Zaproszeni','Odpowiedzi','Średnia','Odsetek [%]')
  wydruk_zi(tabele=list('Hipotetyczna rekrutacja'=tab,
    'Średnia zależy także od składu grup'=srednie_selekcji),digits=2L)
}
precyzja_do_odczytu <- function() {
  pokaz <- precyzja
  names(pokaz) <- c('N grupy','SE różnicy','Połowa CI','N razem')
  wydruk_zi(tabele=list('Przybliżenie przy SD=0,8'=pokaz),
    wykresy=list(wykres_precyzji))
}
wariant_demo <- przygotuj_wariant_projektu('S02',id='demo001')
