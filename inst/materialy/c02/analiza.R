dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_', 1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
B <- 1999L
set.seed(202627)
formatuj_wynik <- function(x) {
  wynik <- x
  kolumny_p <- names(wynik)[grepl('^p($|_)', names(wynik))]
  for (nazwa in kolumny_p) if (is.numeric(wynik[[nazwa]]))
    wynik[[nazwa]] <- format.pval(wynik[[nazwa]], digits = 3, eps = 0.001)
  wynik
}
tabela_glowna <- przygotowane$dziennik
wynik_glowny <- data.frame(N_surowe = nrow(dane_surowe), N_analityczne = nrow(dane), braki_czasu = sum(is.na(dane$czas_wyszukiwania)))
wynik_klasyczny <- przygotowane$dziennik
wynik_permutacyjny <- data.frame(status = 'permutacja nast\u0119puje po czyszczeniu')
efekt <- data.frame(zmienione_elementy = sum(przygotowane$dziennik$liczba[2:4]))
wykres_glowny <- ggplot2::ggplot(dane, ggplot2::aes(x = czas_wyszukiwania)) + ggplot2::geom_histogram(bins = 18, fill = '#56B4E9', colour = 'white') + ggplot2::labs(x = 'Czas [min]', y = 'Liczba os\u00F3b', title = 'Czas po zastosowaniu regu\u0142') + badaniaZI::theme_zi()

# Tekst demonstracyjny pozostaje oddzielony od surowego pliku kursowego.
eksport_demo <- paste(
  'id;grupa;czas_min;ocena;powodzenie',
  'u01;nowi;6,5;4;1',
  'u02;nowi;8,0;99;0',
  'u02;nowi;8,0;99;0',
  'u03;doświadczeni;4,5;5;1',
  'u04;doświadczeni;;3;',
  'u05;nowi;-2,0;2;0',
  'u06;doświadczeni;35,0;4;1', sep = '\n'
)
import_demo <- function(separator = ';', dziesietny = ',') {
  utils::read.table(text = eksport_demo, header = TRUE, sep = separator,
                    dec = dziesietny, na.strings = '', stringsAsFactors = FALSE,
                    check.names = FALSE)
}
surowe_demo <- import_demo()
unikalne_demo <- unique(surowe_demo)
czyste_demo <- unikalne_demo
czyste_demo$ocena[czyste_demo$ocena == 99 & !is.na(czyste_demo$ocena)] <- NA_real_
czyste_demo$czas_min[which(czyste_demo$czas_min < 0 | czyste_demo$czas_min > 120)] <- NA_real_

tabela_klas <- function(tabela) {
  data.frame(kolumna = names(tabela),
             klasa_R = vapply(tabela, function(x) paste(class(x), collapse = '/'), character(1)),
             braki = vapply(tabela, function(x) sum(is.na(x)), integer(1)))
}
porownaj_etapy <- function(...) {
  etapy <- list(...)
  do.call(rbind, lapply(names(etapy), function(nazwa) {
    x <- etapy[[nazwa]]
    data.frame(etap = nazwa, rekordy = nrow(x), wazny_czas = sum(!is.na(x$czas_min)),
               wazna_ocena = sum(!is.na(x$ocena)), wazny_wynik = sum(!is.na(x$powodzenie)))
  }))
}
etapy_demo <- porownaj_etapy(surowe = surowe_demo, unikalne = unikalne_demo,
                            oczyszczone = czyste_demo)
roznice_komorek <- data.frame(
  id = czyste_demo$id, czas_przed = unikalne_demo$czas_min,
  czas_po = czyste_demo$czas_min, ocena_przed = unikalne_demo$ocena,
  ocena_po = czyste_demo$ocena
)
ocena_bez_kodu <- unikalne_demo$ocena[unikalne_demo$ocena != 99]
skutek_kodu <- data.frame(
  wariant = c('Kod 99 błędnie traktowany jak odpowiedź', 'Wyłącznie ważne odpowiedzi 1–5'),
  N = c(nrow(unikalne_demo), sum(!is.na(czyste_demo$ocena))),
  srednia = c(mean(unikalne_demo$ocena), mean(czyste_demo$ocena, na.rm = TRUE))
)
konflikt_demo <- data.frame(id = c('u02', 'u02'), czas_min = c(8, 18), powodzenie = c(0, 1))
braki_w_zbiorze <- data.frame(
  zmienna = c('czas_wyszukiwania', 'pozycja_2', 'powodzenie'),
  N_przed = vapply(dane_surowe[c('czas_wyszukiwania', 'pozycja_2', 'powodzenie')],
                    function(x) sum(!is.na(x)), integer(1)),
  N_po = vapply(dane[c('czas_wyszukiwania', 'pozycja_2', 'powodzenie')],
                 function(x) sum(!is.na(x)), integer(1))
)
plik_csv <- system.file('extdata', 'przyklad', 'surowe.csv', package = 'badaniaZI')
plik_xlsx <- system.file('extdata', 'przyklad', 'ankieta.xlsx', package = 'badaniaZI')
odczyt_csv <- utils::read.csv(plik_csv, encoding = 'UTF-8', stringsAsFactors = FALSE)
odczytaj_arkusz <- function() {
  if (!requireNamespace('readxl', quietly = TRUE))
    stop('Odczyt XLSX wymaga pakietu readxl przygotowanego w środowisku kursu.')
  as.data.frame(readxl::read_excel(plik_xlsx))
}
czas_demo_do_wykresu <- czyste_demo[is.finite(czyste_demo$czas_min), ]
wykres_decyzji <- ggplot2::ggplot(czas_demo_do_wykresu,
                                  ggplot2::aes(x = id, y = czas_min)) +
  ggplot2::geom_point(size = 3, colour = '#0072B2') +
  ggplot2::labs(x = 'Osoba', y = 'Czas [min]',
                 title = 'Długi czas pozostaje obserwacją',
                 caption = 'Przykład syntetyczny. 35 minut mieści się w zadanej regule 0–120.') +
  badaniaZI::theme_zi()

log_demo <- data.frame(id = c('u01', 'u01', 'u01', 'u02', 'u02'),
                       minuta = c(0, 1, 6.5, 0, 8),
                       zdarzenie = c('start', 'filtr', 'koniec', 'start', 'koniec'))
agregat_demo <- aggregate(minuta ~ id, log_demo, function(x) max(x) - min(x))
names(agregat_demo)[2] <- 'czas_min'
pary_demo <- data.frame(id = czyste_demo$id,
                        czas = czyste_demo$czas_min, ocena = czyste_demo$ocena,
                        kompletna_para = complete.cases(czyste_demo[c('czas_min', 'ocena')]))

czas_pelny_demo <- czyste_demo$czas_min[is.finite(czyste_demo$czas_min)]
czas_bez_dl_demo <- czas_pelny_demo[czas_pelny_demo != 35]
wrazliwosc_czasu <- data.frame(
  wariant = c('Wszystkie ważne pomiary', 'Po usunięciu poprawnego czasu 35 min'),
  N = c(length(czas_pelny_demo), length(czas_bez_dl_demo)),
  srednia_min = c(mean(czas_pelny_demo), mean(czas_bez_dl_demo)),
  mediana_min = c(median(czas_pelny_demo), median(czas_bez_dl_demo))
)
braki_grupy <- do.call(rbind, lapply(split(dane, dane$grupa), function(z) {
  data.frame(grupa = z$grupa[1], osoby = nrow(z),
             wazne_czasy = sum(!is.na(z$czas_wyszukiwania)),
             braki_czasu = sum(is.na(z$czas_wyszukiwania)),
             procent_brakow = 100 * mean(is.na(z$czas_wyszukiwania)))
}))
rownames(braki_grupy) <- NULL

# Nowa mała historia: syntetyczny eksport badania cyfrowego archiwum.
eksport_archiwum <- paste(
  'id;grupa;czas_min;ocena;powodzenie',
  'a01;nowi;0,0;3;0',
  'a02;nowi;7,5;4;1',
  'a03;doświadczeni;12,0;99;1',
  'a03;doświadczeni;12,0;99;1',
  'a04;nowi;;2;',
  'a05;doświadczeni;121,0;5;0',
  'a06;doświadczeni;35,0;4;1',sep='\n')
archiwum_import <- function(separator=';',dziesietny=',') {
  utils::read.table(text=eksport_archiwum,header=TRUE,sep=separator,dec=dziesietny,
    na.strings='',stringsAsFactors=FALSE,check.names=FALSE)
}
archiwum_surowe <- archiwum_import()
archiwum_unikalne <- unique(archiwum_surowe)
archiwum_czyste <- archiwum_unikalne
archiwum_czyste$ocena[which(archiwum_czyste$ocena==99)] <- NA_real_
archiwum_czyste$czas_min[which(archiwum_czyste$czas_min<0|
  archiwum_czyste$czas_min>120)] <- NA_real_
archiwum_etapy <- porownaj_etapy(surowe=archiwum_surowe,
  bez_duplikatu=archiwum_unikalne,po_regulach=archiwum_czyste)
archiwum_konflikt <- data.frame(id=c('a03','a03'),czas_min=c(12,18),
  powodzenie=c(1,0))
archiwum_opis <- data.frame(N_osob=nrow(archiwum_czyste),
  N_czas=sum(!is.na(archiwum_czyste$czas_min)),
  srednia_min=mean(archiwum_czyste$czas_min,na.rm=TRUE),
  mediana_min=median(archiwum_czyste$czas_min,na.rm=TRUE),
  N_ocena=sum(!is.na(archiwum_czyste$ocena)),
  N_sukces=sum(!is.na(archiwum_czyste$powodzenie)),
  sukces_procent=100*mean(archiwum_czyste$powodzenie,na.rm=TRUE))
archiwum_wrazliwosc <- data.frame(
  wariant=c('Wszystkie ważne czasy','Bez prawidłowych 35 minut'),
  N=c(sum(!is.na(archiwum_czyste$czas_min)),
    sum(!is.na(archiwum_czyste$czas_min)&archiwum_czyste$czas_min!=35)),
  srednia_min=c(mean(archiwum_czyste$czas_min,na.rm=TRUE),
    mean(archiwum_czyste$czas_min[which(archiwum_czyste$czas_min!=35)])))
archiwum_pary <- data.frame(id=archiwum_czyste$id,czas=archiwum_czyste$czas_min,
  ocena=archiwum_czyste$ocena,
  para=complete.cases(archiwum_czyste[c('czas_min','ocena')]))
archiwum_wykres <- ggplot2::ggplot(archiwum_czyste,ggplot2::aes(id,czas_min))+
  ggplot2::geom_point(colour='#0072B2',size=3,na.rm=TRUE)+
  ggplot2::labs(x='Osoba',y='Czas [min]',title='Zero, długi czas i brak nie są tym samym')+
  badaniaZI::theme_zi()

# Warstwa prezentacji: w Rmd wystarczy przypisanie oraz print(wynik).
ustaw_material <- function() {
  knitr::opts_chunk$set(echo=TRUE,message=FALSE,warning=FALSE,
    results='asis',fig.width=6,fig.height=3.3,fig.align='center')
  invisible(NULL)
}
wydruk_zi <- function(tabele=list(),wykresy=list(),tekst=NULL,digits=3L) {
  stopifnot(is.list(tabele),is.list(wykresy),is.numeric(digits),length(digits)==1L)
  structure(list(tabele=tabele,wykresy=wykresy,tekst=tekst,digits=digits),
    class='zi_wydruk')
}
print.zi_wydruk <- function(x,...) {
  dokument <- isTRUE(getOption('knitr.in.progress'))
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


tekst_eksportu <- function(eksport) wydruk_zi(tekst=strsplit(eksport,'\n',fixed=TRUE)[[1]])
kontrola_typow <- function(tabela) pokaz_tabele(tabela_klas(tabela),'Typ zapisu i liczba braków')
pokaz_etapy <- function(etapy) {
  tab <- etapy
  names(tab) <- c('Etap','Rekordy','Czas','Ocena','Powodzenie')
  pokaz_tabele(tab,'Rekordy oraz niebrakujące wartości na kolejnych etapach',0L)
}
porownanie_wrazliwosci <- function(tabela) {
  tab <- tabela
  names(tab) <- c('Wariant','N','Średnia [min]','Mediana [min]')[seq_len(ncol(tab))]
  pokaz_tabele(tab,'Konsekwencja wykluczenia prawidłowego pomiaru',3L)
}
podsumowanie_przygotowania <- function(tabela,etapy) {
  x <- tabela$czas_min
  y <- tabela$ocena
  z <- tabela$powodzenie
  bilans <- data.frame(
    miara=c('Osoby po przygotowaniu','Ważne czasy','Średnia czasu [min]',
      'Mediana czasu [min]','Ważne oceny','Ważne wyniki zadania',
      'Powodzenia','Powodzenia [% ważnych wyników]','Pary czasu i oceny'),
    wartosc=c(nrow(tabela),sum(!is.na(x)),mean(x,na.rm=TRUE),median(x,na.rm=TRUE),
      sum(!is.na(y)),sum(!is.na(z)),sum(z,na.rm=TRUE),
      100*mean(z,na.rm=TRUE),sum(complete.cases(tabela[c('czas_min','ocena')]))))
  tab <- etapy
  names(tab) <- c('Etap','Rekordy','Czas','Ocena','Powodzenie')
  wydruk_zi(tabele=list('Etapy przygotowania'=tab,'Dane do krótkiej notatki'=bilans))
}
bilans_s02 <- function() {
  wydruk_zi(tabele=list('Rekordy S02'=wynik_glowny,
    'Niepuste wartości przed regułami i ważne po regułach'=braki_w_zbiorze))
}
porownanie_formatow <- function() {
  arkusz <- odczytaj_arkusz()
  tab <- data.frame(zrodlo=c('CSV','Arkusz ćwiczeniowy'),
    wiersze=c(nrow(odczyt_csv),nrow(arkusz)),
    kolumny=c(ncol(odczyt_csv),ncol(arkusz)))
  pokaz_tabele(tab,'Lokalne pliki dwóch formatów')
}
podglad_arkusza <- function() {
  arkusz <- odczytaj_arkusz()
  tab <- head(arkusz[c('id_odpowiedzi','grupa','czas_wyszukiwania','powodzenie')],6)
  names(tab) <- c('ID','Grupa','Czas [min]','Powodzenie')
  pokaz_tabele(tab,'Wybrane kolumny arkusza',2L)
}
braki_w_grupach <- function() {
  tab <- braki_grupy
  names(tab) <- c('Grupa','Osoby','Ważne czasy','Braki','Braki [%]')
  pokaz_tabele(tab,'Mianownik odsetka braków to liczba osób w danej grupie',1L)
}
przyklad_transformacji <- function() {
  czas <- c(2,4,4,8)
  tab <- data.frame(ID=c('a','b','c','d'),minuty=czas,sekundy=60*czas,
    ranga=rank(czas),min_max=(czas-min(czas))/diff(range(czas)),
    z=(czas-mean(czas))/sd(czas),log2=log2(czas))
  pokaz_tabele(tab,'Cztery osoby: ta sama informacja, różne przekształcenia',3L)
}
kontrola_importu_archiwum <- function() {
  # Surowy tekst pozwala sprawdzić separator i znak dziesiętny, nie zgadywać ich z tabeli.
  wydruk_zi(tabele=list('Import archiwum'=archiwum_surowe,
    'Typy kolumn'=tabela_klas(archiwum_surowe)),
    tekst=strsplit(eksport_archiwum,'\n',fixed=TRUE)[[1]])
}
kontrola_powtorzen_archiwum <- function() {
  tab <- archiwum_etapy
  names(tab) <- c('Etap','Rekordy','Czas','Ocena','Powodzenie')
  wydruk_zi(tabele=list('Etapy przygotowania'=tab,
    'Odrębny przykład konfliktu'=archiwum_konflikt))
}
kontrola_regul_archiwum <- function() {
  wydruk_zi(tabele=list('Po zastosowaniu reguł'=archiwum_czyste,
    'Kompletność par czasu i oceny'=archiwum_pary))
}
wrazliwosc_archiwum <- function() {
  tab <- archiwum_wrazliwosc
  names(tab) <- c('Wariant','N','Średnia [min]')
  wydruk_zi(tabele=list('Z prawidłowym długim czasem i bez niego'=tab),
    wykresy=list(archiwum_wykres))
}
