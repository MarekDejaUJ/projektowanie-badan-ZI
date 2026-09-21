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
x <- dane$czas_wyszukiwania[is.finite(dane$czas_wyszukiwania)]
tabela_glowna <- data.frame(N = length(x), srednia = mean(x), mediana = median(x), SD = sd(x), IQR = IQR(x), min = min(x), max = max(x))
wynik_glowny <- tabela_glowna
wynik_klasyczny <- tabela_glowna
wynik_permutacyjny <- data.frame(status = 'bez testu hipotezy na C03')
efekt <- data.frame(roznica_srednia_mediana = mean(x) - median(x))
wykres_glowny <- ggplot2::ggplot(dane, ggplot2::aes(x = czas_wyszukiwania)) + ggplot2::geom_histogram(bins = 20, fill = '#56B4E9', colour = 'white') + ggplot2::geom_vline(xintercept = median(x), colour = '#D55E00', linewidth = 1) + ggplot2::labs(x = 'Obserwowany czas [min]', y = 'Liczba os\u00F3b', title = 'Rozk\u0142ad czasu; linia oznacza median\u0119') + badaniaZI::theme_zi()

# Wynik opisowy zachowuje N i braki właściwe dla wybranej zmiennej.
opis_czasu <- function(x) {
  stopifnot(is.numeric(x), sum(is.finite(x)) >= 2)
  y <- x[is.finite(x)]
  data.frame(N = length(y), braki = sum(!is.finite(x)),
             srednia = mean(y), mediana = median(y), SD = sd(y),
             Q1 = unname(quantile(y, .25, type = 7)),
             Q3 = unname(quantile(y, .75, type = 7)),
             IQR = IQR(y), P90 = unname(quantile(y, .9, type = 7)))
}
serie_demo <- list(A = c(2, 4, 6, 8, 10), B = c(2, 4, 6, 8, 30))
opis_demo <- do.call(rbind, lapply(names(serie_demo), function(nazwa) {
  cbind(seria = nazwa, opis_czasu(serie_demo[[nazwa]]))
}))
kwadraty_demo <- data.frame(czas = serie_demo$A,
                            odchylenie = serie_demo$A - mean(serie_demo$A),
                            kwadrat = (serie_demo$A - mean(serie_demo$A))^2)
opis_pionowy <- function(x) {
  out <- opis_czasu(x)
  data.frame(miara = names(out), wartosc = as.numeric(out[1, ]))
}
opis_grup <- do.call(rbind, lapply(split(dane, dane$grupa), function(z) {
  cbind(grupa = z$grupa[1], opis_czasu(z$czas_wyszukiwania))
}))
rownames(opis_grup) <- NULL
udzial_do_progu <- function(tabela, prog_min = 10) {
  stopifnot(is.numeric(prog_min), length(prog_min) == 1L, is.finite(prog_min))
  t <- tabela$czas_wyszukiwania
  s <- tabela$powodzenie
  para <- is.finite(t) & !is.na(s)
  data.frame(
    pytanie = c('Czas nie przekracza progu', 'Powodzenie i czas nie przekracza progu'),
    prog_min = prog_min,
    licznik = c(sum(t <= prog_min, na.rm = TRUE), sum(t[para] <= prog_min & s[para] == 1)),
    mianownik = c(sum(is.finite(t)), sum(para))
  )
}
progi <- udzial_do_progu(dane, 10)
progi$procent <- 100 * progi$licznik / progi$mianownik
powodzenie_grup <- do.call(rbind, lapply(split(dane, dane$grupa), function(z) {
  data.frame(grupa = z$grupa[1], wazne = sum(!is.na(z$powodzenie)),
             sukcesy = sum(z$powodzenie == 1, na.rm = TRUE),
             procent = 100 * mean(z$powodzenie == 1, na.rm = TRUE))
}))
rownames(powodzenie_grup) <- NULL
histogram_czasu <- function(liczba_przedzialow = 12L) {
  stopifnot(liczba_przedzialow >= 3L, liczba_przedzialow <= 40L)
  ggplot2::ggplot(dane, ggplot2::aes(x = czas_wyszukiwania)) +
    ggplot2::geom_histogram(bins = liczba_przedzialow, fill = '#56B4E9', colour = 'white') +
    ggplot2::labs(x = 'Czas [min]', y = 'Liczba osób',
                   title = paste('Histogram:', liczba_przedzialow, 'przedziałów')) +
    badaniaZI::theme_zi()
}
wykres_ecdf <- ggplot2::ggplot(dane[is.finite(dane$czas_wyszukiwania), ],
                               ggplot2::aes(x = czas_wyszukiwania)) +
  ggplot2::stat_ecdf(geom = 'step', colour = '#0072B2', linewidth = .8) +
  ggplot2::geom_vline(xintercept = 10, linetype = 2, colour = '#D55E00') +
  ggplot2::scale_y_continuous(labels = function(x) paste0(round(100*x), '%')) +
  ggplot2::labs(x = 'Czas [min]', y = 'Udział czasów nie większych niż x',
                 title = 'Dystrybuanta empiryczna czasu') + badaniaZI::theme_zi()
wykres_grup <- ggplot2::ggplot(dane, ggplot2::aes(x = grupa, y = czas_wyszukiwania)) +
  ggplot2::geom_boxplot(fill = '#56B4E9', width = .45, outlier.colour = '#D55E00') +
  ggplot2::labs(x = 'Grupa', y = 'Czas [min]', title = 'Położenie i zróżnicowanie czasu') +
  badaniaZI::theme_zi()
wykres_powodzen <- ggplot2::ggplot(powodzenie_grup,
                                   ggplot2::aes(x = grupa, y = procent)) +
  ggplot2::geom_col(fill = '#009E73', width = .55) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(sukcesy, '/', wazne)), vjust = -.4) +
  ggplot2::scale_y_continuous(limits = c(0, 100)) +
  ggplot2::labs(x = 'Grupa', y = 'Powodzenia [% ważnych wyników]',
                 title = 'Skuteczność z własnym mianownikiem każdej grupy') +
  badaniaZI::theme_zi()
zakresy_demo <- data.frame(
  seria = c('A', 'B'),
  srednia = c(mean(serie_demo$A), mean(serie_demo$B)),
  mediana = c(median(serie_demo$A), median(serie_demo$B))
)
skalowanie_demo <- data.frame(
  jednostka = c('minuty', 'sekundy'),
  srednia = c(mean(x), mean(60*x)), SD = c(sd(x), sd(60*x)),
  CV = c(sd(x)/mean(x), sd(60*x)/mean(60*x))
)
miary_wszystkie <- opis_czasu(dane$czas_wyszukiwania)
miary_sukces <- opis_czasu(dane$czas_wyszukiwania[which(dane$powodzenie == 1)])

# Osobna demonstracja: skład zadań może odwrócić porównanie ogółem.
sklad_demo <- data.frame(
  wersja = rep(c('A', 'B'), each = 2),
  zadanie = rep(c('łatwe', 'trudne'), 2),
  sukces = c(81, 2, 9, 27), N = c(90, 10, 10, 90)
)
sklad_demo$procent <- 100 * sklad_demo$sukces / sklad_demo$N
sklad_ogolem <- aggregate(cbind(sukces, N) ~ wersja, sklad_demo, sum)
sklad_ogolem$procent <- 100 * sklad_ogolem$sukces / sklad_ogolem$N
sklad_wspolny <- aggregate(procent ~ wersja, sklad_demo, mean)
sklad_wspolny$udzial_latwych <- .5
sklad_wspolny$udzial_trudnych <- .5

cztery_profile <- data.frame(
  osoba = c('a', 'b', 'c', 'd'), czas = c(2, 3, 14, 16),
  powodzenie = c(1, 0, 1, 0)
)
wykres_profili <- ggplot2::ggplot(cztery_profile,
    ggplot2::aes(x = czas, y = powodzenie, label = osoba)) +
  ggplot2::geom_point(size = 3, colour = '#0072B2') +
  ggplot2::geom_text(nudge_y = .1) +
  ggplot2::scale_y_continuous(breaks = c(0, 1), labels = c('Nie', 'Tak'),
                             limits = c(-.2, 1.3)) +
  ggplot2::labs(x = 'Czas [min]', y = 'Powodzenie',
                 title = 'Cztery profile wykonania — osobna demonstracja') +
  badaniaZI::theme_zi()


# Osobny eksport po sprawdzonym przygotowaniu: jedna osoba, jedno zadanie.
repozytorium <- data.frame(
  osoba=sprintf('r%02d',1:12),
  czas_wyszukiwania=c(2,3,4,5,6,7,8,9,10,12,24,NA),
  powodzenie=c(0,1,1,1,0,1,1,1,1,0,1,1))
repo_opis <- opis_pionowy(repozytorium$czas_wyszukiwania)
repo_kwantyle <- data.frame(
  miara=c('Q1','Mediana','Q3','P90'),
  minuty=unname(quantile(repozytorium$czas_wyszukiwania,
    c(.25,.5,.75,.9),na.rm=TRUE,type=7)))
repo_wrazliwosc <- rbind(
  cbind(zakres='Wszystkie ważne czasy',opis_czasu(repozytorium$czas_wyszukiwania)),
  cbind(zakres='Bez potwierdzonych 24 minut',
    opis_czasu(repozytorium$czas_wyszukiwania[-11])))
repo_progi <- udzial_do_progu(repozytorium,10)
repo_progi$procent <- 100*repo_progi$licznik/repo_progi$mianownik
repo_sukces <- data.frame(sukcesy=sum(repozytorium$powodzenie),
  N=nrow(repozytorium),procent=100*mean(repozytorium$powodzenie))
repo_wykres <- ggplot2::ggplot(repozytorium,
    ggplot2::aes(x=czas_wyszukiwania,y=osoba,shape=factor(powodzenie)))+
  ggplot2::geom_point(size=2.6,na.rm=TRUE)+
  ggplot2::scale_shape_manual(values=c('0'=1,'1'=16))+
  ggplot2::labs(x='Czas [min]',y='Osoba',shape='Sukces',
    title='Odnalezienie wskazanej publikacji w repozytorium')+badaniaZI::theme_zi()
repo_ecdf <- ggplot2::ggplot(repozytorium[!is.na(repozytorium$czas_wyszukiwania),],
    ggplot2::aes(x=czas_wyszukiwania))+
  ggplot2::stat_ecdf(geom='step',colour='#0072B2')+
  ggplot2::geom_vline(xintercept=10,linetype=2)+
  ggplot2::labs(x='Czas [min]',y='Udział czasów nie większych niż x')+
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


opis_czasu_do_raportu <- function(czas) {
  pokaz_tabele(opis_pionowy(czas),'Miary czasu [min], liczebność i braki',3L)
}
polozenie_serii <- function() {
  pokaz_tabele(opis_demo[c('seria','N','srednia','mediana')],
    'Średnia i mediana dwóch serii [min]',2L)
}
rozrzut_serii <- function() {
  pokaz_tabele(opis_demo[c('seria','srednia','SD','Q1','Q3','IQR')],
    'Położenie i rozrzut tych samych czasów [min]',2L)
}
kwantyle_serii <- function() {
  pokaz_tabele(opis_demo[c('seria','N','Q1','mediana','Q3','IQR','P90')],
    'Kwartyle, IQR i percentyl 90. [min]',2L)
}
opis_czasu_grup <- function() {
  pokaz_tabele(opis_grup[c('grupa','N','srednia','mediana','SD','IQR')],
    'Opis czasu w grupach [min]',2L)
}
porownanie_histogramow <- function() {
  wydruk_zi(wykresy=list(histogram_czasu(8),histogram_czasu(24)))
}
tabela_progow <- function(tabela,prog_min=10) {
  tab <- udzial_do_progu(tabela,prog_min)
  tab$procent <- 100*tab$licznik/tab$mianownik
  # Pełne definicje znajdują się w opisie; krótkie nazwy mieszczą się na A4.
  tab$pytanie <- c('Sam czas','Sukces i czas')
  names(tab) <- c('Warunek','Próg [min]','Licznik','Mianownik','Procent')
  tab
}
opis_progow <- function(tabela,prog_min=10) {
  pokaz_tabele(tabela_progow(tabela,prog_min),'Dwa pytania o czas nie większy niż próg',2L)
}
czas_wszystkich_i_udanych <- function() {
  tab <- rbind(
    cbind(zakres='Wszystkie ważne czasy',miary_wszystkie[c('N','srednia','mediana','SD')]),
    cbind(zakres='Wyłącznie udane próby',miary_sukces[c('N','srednia','mediana','SD')]))
  pokaz_tabele(tab,'Zmiana analizowanego zbioru; czas w minutach',2L)
}
portret_repozytorium <- function() {
  wydruk_zi(tabele=list('Dwanaście osób w repozytorium'=repozytorium,
    'Opis ważnych czasów [min]'=repo_opis))
}
kwantyle_i_dystrybuanta <- function() {
  wydruk_zi(tabele=list('Kwantyle czasu [min]'=repo_kwantyle),
    wykresy=list(repo_ecdf),digits=2L)
}
wrazliwosc_repozytorium <- function() {
  tab <- repo_wrazliwosc[c('zakres','N','srednia','mediana','SD')]
  tab$zakres <- c('Z prawidłowym czasem 24 min','Bez czasu 24 min')
  names(tab) <- c('Wariant','N','Średnia','Mediana','SD')
  wydruk_zi(tabele=list('Wrażliwość opisu czasu [min]'=tab),digits=3L)
}
skutecznosc_repozytorium <- function(prog_min=10) {
  wydruk_zi(tabele=list('Powodzenie niezależnie od czasu'=repo_sukces,
    'Warunki czasu i powodzenia'=tabela_progow(repozytorium,prog_min)),digits=2L)
}
