# C02: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po trzech regułach funkcji przygotuj_ankiete(): usunięcie
# identycznych duplikatów, kod 99 w pozycjach jako brak i czas spoza
# 0–120 minut jako brak.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_', 1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
kolory <- badaniaZI::paleta_zi()
wynik_glowny <- data.frame(N_surowe = nrow(dane_surowe), N_analityczne = nrow(dane),
                           braki_czasu = sum(is.na(dane$czas_wyszukiwania)))

# Histogram ważnych czasów S02 o jawnej szerokości przedziału 2,5 minuty.
wykres_glowny <- badaniaZI::wykres_histogram(dane$czas_wyszukiwania, szerokosc = 2.5,
  os = 'Czas wyszukiwania [min]', tytul = 'Czas S02 po zastosowaniu reguł')

# Przepływ rekordów S02 od surowego eksportu do ważnych czasów.
unikalne_s02 <- unique(dane_surowe)
przeplyw_s02 <- data.frame(
  etap = c('Rekordy surowego eksportu', 'Rekordy po usunięciu duplikatów',
           'Rekordy z zapisanym czasem', 'Czasy w zakresie 0–120 min'),
  n = c(nrow(dane_surowe), nrow(dane), sum(!is.na(unikalne_s02$czas_wyszukiwania)),
        sum(!is.na(dane$czas_wyszukiwania))))
wykres_przeplywu <- badaniaZI::wykres_przeplyw_proby(przeplyw_s02$etap, przeplyw_s02$n,
  os = 'Liczba rekordów', tytul = 'Od surowego eksportu do ważnych czasów S02')

# Odsetek braków trzech zmiennych w dwóch grupach S02.
wykres_brakow <- badaniaZI::wykres_braki(dane, c('czas_wyszukiwania', 'pozycja_2', 'powodzenie'),
  'grupa', etykiety = c('Czas wyszukiwania', 'Pozycja 2', 'Powodzenie'),
  tytul = 'Braki trzech zmiennych w dwóch grupach S02')

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

# Ważne czasy demonstracji: punkty osób i wykres pudełkowy z granicą wąsa.
czas_demo_do_wykresu <- czyste_demo[is.finite(czyste_demo$czas_min), ]
wykres_decyzji <- ggplot2::ggplot(czas_demo_do_wykresu,
                                  ggplot2::aes(x = id, y = czas_min)) +
  ggplot2::geom_point(size = 3, colour = kolory[['primary']]) +
  ggplot2::geom_text(ggplot2::aes(label = chartr('.', ',', as.character(czas_min))),
                     vjust = -1, size = 3.2) +
  ggplot2::scale_y_continuous(limits = c(0, 40)) +
  ggplot2::labs(x = 'Osoba', y = 'Czas [min]',
                 title = 'Długi czas pozostaje obserwacją',
                 caption = 'Przykład syntetyczny. Cztery ważne czasy; u04 ma puste pole, a czas u05 (−2 min) oznaczono jako brak.') +
  badaniaZI::theme_zi()
czas_pelny_demo <- czas_demo_do_wykresu$czas_min
wykres_pudelka <- badaniaZI::wykres_pudelkowy(czas_pelny_demo, os = 'Czas [min]',
  tytul = 'Cztery ważne czasy na wykresie pudełkowym')

log_demo <- data.frame(id = c('u01', 'u01', 'u01', 'u02', 'u02'),
                       minuta = c(0, 1, 6.5, 0, 8),
                       zdarzenie = c('start', 'filtr', 'koniec', 'start', 'koniec'))
agregat_demo <- aggregate(minuta ~ id, log_demo, function(x) max(x) - min(x))
names(agregat_demo)[2] <- 'czas_min'
wykres_logu <- badaniaZI::wykres_log(log_demo, tytul = 'Dwie sesje w logu zdarzeń')
pary_demo <- data.frame(id = czyste_demo$id,
                        czas = czyste_demo$czas_min, ocena = czyste_demo$ocena,
                        kompletna_para = complete.cases(czyste_demo[c('czas_min', 'ocena')]))

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

# Cztery czasy spoza danych zadania w pięciu reprezentacjach.
czas_transformacji <- c(2, 4, 4, 8)
wykres_transformacji <- badaniaZI::wykres_transformacje(czas_transformacji, c('a', 'b', 'c', 'd'),
  tytul = 'Cztery czasy w pięciu skalach')

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
archiwum_wrazliwosc <- data.frame(
  wariant=c('Wszystkie ważne czasy','Bez prawidłowych 35 minut'),
  N=c(sum(!is.na(archiwum_czyste$czas_min)),
    sum(!is.na(archiwum_czyste$czas_min)&archiwum_czyste$czas_min!=35)),
  srednia_min=c(mean(archiwum_czyste$czas_min,na.rm=TRUE),
    mean(archiwum_czyste$czas_min[which(archiwum_czyste$czas_min!=35)])))
archiwum_pary <- data.frame(id=archiwum_czyste$id,czas=archiwum_czyste$czas_min,
  ocena=archiwum_czyste$ocena,
  para=complete.cases(archiwum_czyste[c('czas_min','ocena')]))
archiwum_wykres <- ggplot2::ggplot(archiwum_czyste[!is.na(archiwum_czyste$czas_min), ],
    ggplot2::aes(id,czas_min))+
  ggplot2::geom_point(colour=kolory[['primary']],size=3)+
  ggplot2::labs(x='Osoba',y='Czas [min]',title='Zero, długi czas i brak: trzy różne zapisy',
    caption='Cztery ważne czasy po regułach.\na04 ma puste pole, a05 czas 121 min spoza zakresu: obie osoby mają brak czasu.')+
  badaniaZI::theme_zi()

# Warstwa prezentacji: w Rmd wystarczy przypisanie oraz print(wynik).
ustaw_material <- function() {
  knitr::opts_chunk$set(echo=TRUE,message=FALSE,warning=FALSE,
    results='asis',fig.width=6,fig.height=3.3,fig.align='center',fig.pos='H')
  # Rysunek [H] zostaje przy swoim zadaniu także w PDF bez preambuły kursu.
  if(isTRUE(getOption('knitr.in.progress'))&&knitr::is_latex_output())
    knitr::knit_meta_add(list(rmarkdown::latex_dependency('float')))
  invisible(NULL)
}
wydruk_zi <- function(tabele=list(),wykresy=list(),tekst=NULL,digits=3L,markdown=list()) {
  stopifnot(is.list(tabele),is.list(wykresy),is.list(markdown),is.numeric(digits),length(digits)==1L)
  structure(list(tabele=tabele,wykresy=wykresy,tekst=tekst,digits=digits,markdown=markdown),
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
  # Długie tabele tekstowe (Markdown) zawijają się w PDF i HTML.
  for(md in x$markdown) cat(as.character(md), '\n\n', sep = '')
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
dziennik_s02 <- function() {
  tab <- przygotowane$dziennik
  tab$regula <- gsub('--', '–', tab$regula, fixed = TRUE)
  names(tab) <- c('Reguła lub stan', 'Liczba')
  pokaz_tabele(tab, 'Reguły w pełnym zbiorze S02', 0L)
}
bilans_s02 <- function() {
  rekordy <- wynik_glowny
  names(rekordy) <- c('Rekordy surowe', 'Rekordy po przygotowaniu', 'Braki czasu')
  wartosci <- braki_w_zbiorze
  names(wartosci) <- c('Zmienna', 'Niepuste przed regułami', 'Ważne po regułach')
  wydruk_zi(tabele=list('Rekordy S02'=rekordy,
    'Niepuste wartości przed regułami i ważne po regułach'=wartosci), digits = 0L)
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
  wydruk_zi(tabele=list('Mianownik odsetka braków to liczba osób w danej grupie'=tab),
    wykresy=list(wykres_brakow),digits=1L)
}
przyklad_transformacji <- function() {
  czas <- czas_transformacji
  tab <- data.frame(ID=c('a','b','c','d'),minuty=czas,sekundy=60*czas,
    ranga=rank(czas),min_max=(czas-min(czas))/diff(range(czas)),
    z=(czas-mean(czas))/sd(czas),log2=log2(czas))
  pokaz_tabele(tab,'Cztery osoby: ta sama informacja, różne przekształcenia',3L)
}
kontrola_importu_archiwum <- function() {
  # Surowy tekst pozwala odczytać separator i znak dziesiętny wprost z eksportu.
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
  # Cztery zapisy obok siebie: student porównuje wartość przed regułą i po niej bez wracania do importu.
  zapisy <- data.frame(Osoba=c('a01','a03','a05','a04'),Kolumna=c('czas_min','ocena','czas_min','czas_min'),
    'Przed regułą'=c('0','99','121','puste pole'),'Po regule'=c('0','NA','NA','NA'),
    'Reguła'=c('zarejestrowane zero zostaje','kod braku oceny','czas poza 0–120 min','brak zapisu zostaje brakiem'),
    check.names=FALSE)
  wydruk_zi(tabele=list('Cztery zapisy przed regułami i po nich'=zapisy,
    'Po zastosowaniu reguł'=archiwum_czyste,'Kompletność par czasu i oceny'=archiwum_pary))
}
wrazliwosc_archiwum <- function() {
  tab <- archiwum_wrazliwosc
  czasy <- archiwum_czyste$czas_min[!is.na(archiwum_czyste$czas_min)]
  tab$mediana <- c(median(czasy),median(czasy[czasy!=35]))
  names(tab) <- c('Wariant','N','Średnia [min]','Mediana [min]')
  wydruk_zi(tabele=list('Z prawidłowym długim czasem i bez niego'=tab),
    wykresy=list(archiwum_wykres))
}
