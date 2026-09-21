# Przygotowanie wspólnego syntetycznego przykładu S02.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
nazwy_pozycji <- paste0('pozycja_', 1:6)
pozycje_surowe <- dane[nazwy_pozycji]
pozycje <- pozycje_surowe
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
tabela_glowna <- dane[c('id_odpowiedzi', 'liczba_pozycji', 'indeks')]
kanaly <- badaniaZI::odpowiedzi_wielokrotne(dane[paste0('kanal_', 1:4)])

# Funkcje odczytu mają własne małe przykłady, niezależne od S02.
mini <- data.frame(
  osoba = letters[1:6],
  p1 = c(4,4,5,1,3,5), p2 = c(5,4,NA,2,3,1),
  p3 = c(1,2,1,5,3,1), p4 = c(4,4,NA,1,3,5),
  p5 = c(4,NA,5,2,3,1), p6 = c(5,4,5,1,3,5)
)
mini_ukierunkowane <- mini
mini_ukierunkowane$p3 <- badaniaZI::odwroc_pozycje(mini$p3)
audyt_indeksu <- function(tabela, minimum = 5L) {
  stopifnot(all(paste0('p', 1:6) %in% names(tabela)))
  x <- tabela[paste0('p', 1:6)]
  m <- rowSums(!is.na(x))
  data.frame(osoba = tabela$osoba, wazne = m,
             suma_waznych = rowSums(x, na.rm = TRUE),
             indeks = badaniaZI::indeks_ankiety(x, minimum = minimum))
}
mini_wynik <- audyt_indeksu(mini_ukierunkowane)
kierunek_demo <- data.frame(
  osoba = mini$osoba,
  bez_odwrocenia = badaniaZI::indeks_ankiety(mini[paste0('p',1:6)]),
  po_odwroceniu = mini_wynik$indeks
)
odwrocenie <- data.frame(odpowiedz = 1:5,
                         po_odwroceniu = badaniaZI::odwroc_pozycje(1:5))
liczba_odpowiedzi <- as.data.frame(table(factor(dane$liczba_pozycji, levels=0:6)))
names(liczba_odpowiedzi) <- c('wazne_pozycje', 'osoby')
rozklad_pozycji <- function(x) {
  stopifnot(is.numeric(x), all(is.na(x) | x %in% 1:5))
  n <- sum(!is.na(x))
  data.frame(odpowiedz = 1:5,
             liczba = as.integer(table(factor(x, levels = 1:5))),
             N = n, braki = sum(is.na(x)),
             procent = if(n) 100 * as.integer(table(factor(x, levels=1:5))) / n else NA_real_)
}
pozycja_3_opis <- rozklad_pozycji(pozycje_surowe$pozycja_3)
wykres_pozycji <- ggplot2::ggplot(pozycja_3_opis,
    ggplot2::aes(x = factor(odpowiedz), y = procent)) +
  ggplot2::geom_col(fill = '#56B4E9', width = .7) +
  ggplot2::scale_y_continuous(limits = c(0,100)) +
  ggplot2::labs(x = 'Oryginalna odpowiedź 1–5', y = 'Procent ważnych odpowiedzi',
                 title = 'Pozycja 3: odczytanie rekordu wymaga zgadywania') +
  badaniaZI::theme_zi()
wykres_indeksu <- ggplot2::ggplot(dane, ggplot2::aes(x = indeks)) +
  ggplot2::geom_histogram(binwidth = .25, boundary = 1, fill = '#009E73', colour = 'white') +
  ggplot2::scale_x_continuous(breaks = 1:5, limits = c(.9,5.1)) +
  ggplot2::labs(x = 'Indeks deklarowanej użyteczności [1–5]', y = 'Liczba osób',
                 title = 'Indeks po ukierunkowaniu pozycji i regule 5/6') +
  badaniaZI::theme_zi()
porownaj_minimum <- function(minimum) {
  z <- badaniaZI::indeks_ankiety(pozycje, minimum = minimum)
  data.frame(minimum = minimum, N = sum(!is.na(z)), braki = sum(is.na(z)),
             srednia = mean(z, na.rm = TRUE), SD = sd(z, na.rm = TRUE))
}
wrazliwosc_minimum <- rbind(porownaj_minimum(5L), porownaj_minimum(6L))
profile <- data.frame(pozycja=rep(1:6,2),
  osoba=rep(c('Jednolite odpowiedzi','Zróżnicowane odpowiedzi'),each=6),
  odpowiedz=c(3,3,3,3,3,3,1,5,3,5,1,3))
wykres_profili <- ggplot2::ggplot(profile,
    ggplot2::aes(x = pozycja, y = odpowiedz, colour = osoba, group = osoba)) +
  ggplot2::geom_line(linewidth = .7) + ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = 1:6) +
  ggplot2::scale_y_continuous(breaks = 1:5, limits = c(1,5)) +
  ggplot2::scale_colour_manual(values = c('#0072B2','#D55E00')) +
  ggplot2::labs(x = 'Numer ukierunkowanej pozycji', y = 'Odpowiedź', colour = NULL,
                 title = 'Dwa profile o takim samym indeksie równym 3') +
  badaniaZI::theme_zi() + ggplot2::theme(legend.position = 'bottom')
kanaly_demo <- data.frame(
  www = c(1,0,0,1,NA,1), email = c(0,1,0,NA,NA,1),
  media = c(1,0,0,0,NA,0), kontakt = c(0,0,0,0,NA,1))
kanaly_demo_wynik <- badaniaZI::odpowiedzi_wielokrotne(kanaly_demo)
kanaly_demo_wynik$procent_wskazan <- 100 * kanaly_demo_wynik$liczba /
  sum(kanaly_demo_wynik$liczba)
wykres_kanalow <- ggplot2::ggplot(kanaly,
    ggplot2::aes(x = opcja, y = procent_respondentow)) +
  ggplot2::geom_col(fill = '#0072B2', width = .6) +
  ggplot2::scale_y_continuous(limits = c(0,100)) +
  ggplot2::labs(x = 'Kanał: 1 WWW, 2 e-mail, 3 media, 4 kontakt',
                 y = 'Procent kompletnych respondentów',
                 title = 'Wielokrotny wybór — osoba może wskazać kilka kanałów') +
  badaniaZI::theme_zi()


# Syntetyczna ocena interfejsu cyfrowego archiwum; p3 ma przeciwny kierunek.
archiwum_pozycje <- data.frame(osoba=paste0('a',1:8),
  p1=c(4,5,3,2,4,1,5,3),p2=c(4,4,3,2,NA,2,5,3),
  p3=c(2,1,3,4,2,5,1,3),p4=c(5,5,3,2,4,1,NA,3),
  p5=c(4,4,3,1,NA,2,5,3),p6=c(4,5,3,2,5,1,5,3))
archiwum_kierunek <- archiwum_pozycje
archiwum_kierunek$p3 <- badaniaZI::odwroc_pozycje(archiwum_pozycje$p3)
archiwum_indeks <- audyt_indeksu(archiwum_kierunek)
archiwum_porownanie <- data.frame(osoba=archiwum_pozycje$osoba,
  bez_odwrocenia=audyt_indeksu(archiwum_pozycje)$indeks,
  poprawny=archiwum_indeks$indeks,
  przy_minimum_6=audyt_indeksu(archiwum_kierunek,6L)$indeks)
archiwum_kanaly <- data.frame(www=c(1,1,0,0,1,NA,0,1),
  email=c(1,0,0,1,NA,NA,0,1),media=c(0,1,0,0,0,NA,1,0),
  kontakt=c(0,0,0,1,0,NA,0,0))
archiwum_kanaly_opis <- badaniaZI::odpowiedzi_wielokrotne(archiwum_kanaly)
archiwum_kanaly_opis$procent_wskazan <-
  100*archiwum_kanaly_opis$liczba/sum(archiwum_kanaly_opis$liczba)
archiwum_pomiar <- data.frame(osoba=archiwum_pozycje$osoba,
  indeks=archiwum_indeks$indeks,
  czas_min=c(6,12,8,4,9,3,15,7),sukces=c(1,0,1,0,1,0,1,1))

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


# Dodatkowa obserwacja w tej samej małej demonstracji, nie dane badania rzeczywistego.
mini_pomiar <- data.frame(osoba=mini$osoba,indeks=mini_wynik$indeks,
  czas_min=c(6,12,8,4,10,7),powodzenie=c(1,0,1,0,1,1))

podglad_indeksu <- function() {
  tab <- head(tabela_glowna,8)
  names(tab) <- c('ID','Ważne pozycje','Indeks [1–5]')
  pokaz_tabele(tab,'Pierwsze osiem rekordów S02')
}
dane_kanalow <- function(tabela,osoby=seq_len(nrow(tabela))) {
  pokaz_tabele(cbind(osoba=osoby,tabela),'Wybory osób: 0, 1 oraz brak',0L)
}
opis_kanalow <- function(tabela) {
  stopifnot(length(unique(tabela$mianownik))==1L,
    length(unique(tabela$pominiete_niepelne))==1L)
  suma_wskazan <- sum(tabela$liczba)
  bilans <- data.frame(
    miara=c('Kompletni respondenci','Pominięte niepełne odpowiedzi','Suma wskazań'),
    liczba=c(tabela$mianownik[1],tabela$pominiete_niepelne[1],suma_wskazan))
  tab <- data.frame(Kanal=tabela$opcja,Wybrali=tabela$liczba,
    osoby_procent=tabela$procent_respondentow,
    wskazania_procent=if(suma_wskazan>0) 100*tabela$liczba/suma_wskazan else NA_real_)
  names(tab) <- c('Kanał','Wybrali','Osoby [%]','Wskazania [%]')
  wydruk_zi(tabele=list('Mianowniki i kompletność'=bilans,
    'Odsetki osób oraz udział w puli wskazań'=tab),digits=2L)
}
deklaracja_i_obserwacja <- function() {
  pokaz_tabele(mini_pomiar,'Dwa pomiary tych samych osób a–f')
}
opis_pozycji_archiwum <- function() {
  wydruk_zi(tabele=list('Pierwotne pozycje cyfrowego archiwum'=archiwum_pozycje,
    'Rozkład p2: etykiety filtrów są zrozumiałe'=rozklad_pozycji(archiwum_pozycje$p2)),
    digits=2L)
}
konstrukcja_indeksu_archiwum <- function() {
  porownanie <- archiwum_porownanie[c('osoba','bez_odwrocenia','poprawny')]
  names(porownanie) <- c('Osoba','Bez odwrócenia','Po odwróceniu')
  wydruk_zi(tabele=list('Audyt poprawnego indeksu'=archiwum_indeks,
    'Błędny oraz poprawny kierunek pozycji'=porownanie))
}
kompletnosc_archiwum <- function() {
  tab <- archiwum_porownanie[c('osoba','poprawny','przy_minimum_6')]
  names(tab) <- c('Osoba','Minimum 5','Minimum 6')
  bilans <- data.frame(minimum=c(5,6),
    wazne_indeksy=c(sum(!is.na(tab[[2]])),sum(!is.na(tab[[3]]))))
  wydruk_zi(tabele=list('Wynik każdej osoby w dwóch wariantach'=tab,
    'Liczby ważnych indeksów'=bilans))
}
kanaly_pomocy_archiwum <- function() {
  wynik <- opis_kanalow(archiwum_kanaly_opis)
  surowe <- cbind(osoba=archiwum_pozycje$osoba,archiwum_kanaly)
  wynik$tabele <- c(list('Surowe wybory kanałów'=surowe),wynik$tabele)
  wynik
}
