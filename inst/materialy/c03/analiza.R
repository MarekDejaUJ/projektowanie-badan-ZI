# C03: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po trzech regułach ćwiczenia C02: usunięcie identycznych
# duplikatów, kod 99 w pozycjach jako brak, czas spoza 0–120 minut jako brak.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_', 1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
kolory <- badaniaZI::paleta_zi()
x <- dane$czas_wyszukiwania[is.finite(dane$czas_wyszukiwania)]

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
nazwy_miar <- c(N = 'N (ważne czasy)', braki = 'Braki czasu', srednia = 'Średnia', mediana = 'Mediana',
                SD = 'SD', Q1 = 'Q1', Q3 = 'Q3', IQR = 'IQR', P90 = 'P90')
opis_pionowy <- function(x) {
  out <- opis_czasu(x)
  data.frame(Miara = unname(nazwy_miar[names(out)]), `Wartość` = as.numeric(out[1, ]), check.names = FALSE)
}
serie_demo <- list(A = c(2, 4, 6, 8, 10), B = c(2, 4, 6, 8, 30))
opis_demo <- do.call(rbind, lapply(names(serie_demo), function(nazwa) {
  cbind(seria = nazwa, opis_czasu(serie_demo[[nazwa]]))
}))
kwadraty_demo <- data.frame(`Czas [min]` = c(serie_demo$A, 30),
                            `Odchylenie od 6` = c(serie_demo$A - 6, 0),
                            `Kwadrat odchylenia` = c((serie_demo$A - 6)^2, 40), check.names = FALSE)
kwadraty_demo <- cbind(Wiersz = c(paste0('Osoba ', 1:5), 'Suma'), kwadraty_demo)
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
powodzenie_grup <- do.call(rbind, lapply(split(dane, dane$grupa), function(z) {
  data.frame(grupa = z$grupa[1], wazne = sum(!is.na(z$powodzenie)),
             sukcesy = sum(z$powodzenie == 1, na.rm = TRUE),
             procent = 100 * mean(z$powodzenie == 1, na.rm = TRUE))
}))
rownames(powodzenie_grup) <- NULL
miary_wszystkie <- opis_czasu(dane$czas_wyszukiwania)
miary_sukces <- opis_czasu(dane$czas_wyszukiwania[which(dane$powodzenie == 1)])
skalowanie_demo <- data.frame(Jednostka = c('minuty', 'sekundy'),
                              `Średnia` = c(mean(x), mean(60 * x)), SD = c(sd(x), sd(60 * x)),
                              CV = c(sd(x) / mean(x), sd(60 * x) / mean(60 * x)), check.names = FALSE)
w_przedziale_sd <- mean(abs(x - mean(x)) <= sd(x))
srednia_geometryczna <- exp(mean(log(x)))

# Wykresy S02.
wykres_glowny <- badaniaZI::wykres_histogram(x, 2.5, os = 'Czas wyszukiwania [min]', tytul = 'Rozkład czasu S02') +
  ggplot2::geom_vline(xintercept = median(x), linetype = 'dashed', colour = kolory[['accent']], linewidth = 0.9) +
  ggplot2::annotate('text', x = median(x), y = Inf, label = 'mediana 8,25', hjust = -0.08, vjust = 1.5, size = 3.3)
wykres_ecdf <- badaniaZI::wykres_dystrybuanta(x, prog = 10, os = 'Czas wyszukiwania [min]',
  tytul = 'Dystrybuanta empiryczna czasu S02')
wykres_ecdf_grup <- badaniaZI::wykres_dystrybuanta(dane$czas_wyszukiwania, prog = 10, grupa = dane$grupa,
  os = 'Czas wyszukiwania [min]', tytul = 'Dystrybuanty czasu w dwóch grupach')
wykres_grup <- badaniaZI::wykres_pudelkowy(dane$czas_wyszukiwania, dane$grupa, os = 'Czas wyszukiwania [min]',
  tytul = 'Położenie i zróżnicowanie czasu w grupach')
wykres_powodzen <- badaniaZI::wykres_licznosci_odsetki(powodzenie_grup$grupa, powodzenie_grup$sukcesy,
  powodzenie_grup$wazne, tytul = 'Sukcesy w grupach: liczba i odsetek')
wykres_normalny <- badaniaZI::wykres_histogram(x, 2.5, skala = 'gestosc', dopasuj = 'normalny',
  os = 'Czas wyszukiwania [min]', tytul = 'Histogram gęstości i krzywa normalna')
wykres_gestosci_grup <- badaniaZI::wykres_histogram_panele(split(x, dane$grupa[is.finite(dane$czas_wyszukiwania)]),
  2.5, poczatek = 0, skala = 'gestosc', os = 'Czas wyszukiwania [min]', tytul = 'Histogramy gęstości dwóch grup')
histogram_czasu <- function(szerokosc = 2.5) {
  stopifnot(szerokosc > 0)
  badaniaZI::wykres_histogram(x, szerokosc, os = 'Czas wyszukiwania [min]',
    tytul = paste0('Przedziały po ', format(szerokosc, decimal.mark = ','), ' min'))
}
wykres_przedzialow <- badaniaZI::wykres_histogram_panele(
  list(`Szerokość 5 min` = x, `Szerokość 1,25 min` = x), c(5, 1.25), poczatek = 0,
  os = 'Czas wyszukiwania [min]', tytul = 'Ten sam czas przy dwóch szerokościach przedziału')

# Osobna demonstracja: skład zadań może odwrócić porównanie ogółem (paradoks Simpsona).
sklad_demo <- data.frame(wersja = rep(c('A', 'B'), each = 2), zadanie = rep(c('łatwe', 'trudne'), 2),
                         sukces = c(81, 2, 9, 27), N = c(90, 10, 10, 90))
sklad_demo$procent <- 100 * sklad_demo$sukces / sklad_demo$N
sklad_ogolem <- aggregate(cbind(sukces, N) ~ wersja, sklad_demo, sum)
sklad_ogolem$procent <- 100 * sklad_ogolem$sukces / sklad_ogolem$N
sklad_wspolny <- aggregate(procent ~ wersja, sklad_demo, mean)
simpson <- rbind(data.frame(panel = paste('Zadania', sklad_demo$zadanie), wersja = sklad_demo$wersja,
                            procent = sklad_demo$procent, etykieta = paste0(sklad_demo$sukces, '/', sklad_demo$N)),
                 data.frame(panel = 'Ogółem', wersja = sklad_ogolem$wersja, procent = sklad_ogolem$procent,
                            etykieta = paste0(sklad_ogolem$sukces, '/', sklad_ogolem$N)))
simpson$panel <- factor(simpson$panel, levels = c('Zadania łatwe', 'Zadania trudne', 'Ogółem'))
wykres_simpson <- ggplot2::ggplot(simpson, ggplot2::aes(wersja, procent)) +
  ggplot2::geom_col(fill = kolory[['secondary']], width = 0.55) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(etykieta, '\n', round(procent), '%')), vjust = -0.2, size = 3) +
  ggplot2::facet_wrap(~panel, nrow = 1) +
  ggplot2::scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
  ggplot2::labs(x = 'Wersja usługi', y = 'Sukces [%]', title = 'Skład zadań odwraca porównanie ogółem') +
  badaniaZI::theme_zi()

cztery_profile <- data.frame(osoba = c('a', 'b', 'c', 'd'), czas = c(2, 3, 14, 16), powodzenie = c(1, 0, 1, 0))
wykres_profili <- ggplot2::ggplot(cztery_profile, ggplot2::aes(x = czas, y = powodzenie, label = osoba)) +
  ggplot2::geom_point(size = 3, colour = kolory[['primary']]) +
  ggplot2::geom_text(nudge_y = .12) +
  ggplot2::scale_y_continuous(breaks = c(0, 1), labels = c('niepowodzenie', 'sukces'), limits = c(-.2, 1.3)) +
  ggplot2::labs(x = 'Czas [min]', y = 'Wynik zadania', title = 'Cztery profile wykonania') +
  badaniaZI::theme_zi()

# Osobny eksport po przygotowaniu: jedna osoba, jedno zadanie w repozytorium.
repozytorium <- data.frame(osoba = sprintf('r%02d', 1:12),
                           czas_wyszukiwania = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 24, NA),
                           powodzenie = c(0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1))
repo_opis <- opis_pionowy(repozytorium$czas_wyszukiwania)
repo_kwantyle <- data.frame(Miara = c('Q1', 'Mediana', 'Q3', 'P90'),
  Minuty = unname(quantile(repozytorium$czas_wyszukiwania, c(.25, .5, .75, .9), na.rm = TRUE, type = 7)))
repo_wrazliwosc <- rbind(
  cbind(zakres = 'Wszystkie ważne czasy', opis_czasu(repozytorium$czas_wyszukiwania)),
  cbind(zakres = 'Bez potwierdzonych 24 minut', opis_czasu(repozytorium$czas_wyszukiwania[-11])))
repo_sukces <- data.frame(Sukcesy = sum(repozytorium$powodzenie), N = nrow(repozytorium),
                          `Procent` = 100 * mean(repozytorium$powodzenie), check.names = FALSE)
repo_do_wykresu <- repozytorium[!is.na(repozytorium$czas_wyszukiwania), ]
repo_do_wykresu$wynik <- factor(ifelse(repo_do_wykresu$powodzenie == 1, 'sukces', 'niepowodzenie'),
                                levels = c('sukces', 'niepowodzenie'))
repo_wykres <- ggplot2::ggplot(repo_do_wykresu, ggplot2::aes(x = czas_wyszukiwania, y = osoba, shape = wynik)) +
  ggplot2::geom_point(size = 2.6, colour = kolory[['primary']]) +
  ggplot2::scale_shape_manual(values = c(sukces = 16, niepowodzenie = 1)) +
  ggplot2::labs(x = 'Czas [min]', y = 'Osoba', shape = 'Wynik zadania',
                title = 'Odnalezienie wskazanej publikacji w repozytorium',
                caption = 'Jedenaście osób z zapisanym czasem; osoba r12 ma nieznany czas.') +
  badaniaZI::theme_zi()
repo_ecdf <- badaniaZI::wykres_dystrybuanta(repozytorium$czas_wyszukiwania, prog = 10, os = 'Czas [min]',
  tytul = 'Dystrybuanta czasu w repozytorium')

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

opis_czasu_do_raportu <- function(czas) {
  pokaz_tabele(opis_pionowy(czas),'Miary czasu [min], liczebność i braki',3L)
}
polozenie_serii <- function() {
  tab <- opis_demo[c('seria','N','srednia','mediana')]
  names(tab) <- c('Seria','N','Średnia','Mediana')
  pokaz_tabele(tab,'Średnia i mediana dwóch serii [min]',2L)
}
rozrzut_serii <- function() {
  tab <- opis_demo[c('seria','srednia','SD','Q1','Q3','IQR')]
  names(tab) <- c('Seria','Średnia','SD','Q1','Q3','IQR')
  pokaz_tabele(tab,'Położenie i rozrzut tych samych czasów [min]',2L)
}
kwantyle_serii <- function() {
  tab <- opis_demo[c('seria','N','Q1','mediana','Q3','IQR','P90')]
  names(tab) <- c('Seria','N','Q1','Mediana','Q3','IQR','P90')
  pokaz_tabele(tab,'Kwartyle, IQR i percentyl 90. [min]',2L)
}
opis_czasu_grup <- function() {
  tab <- opis_grup[c('grupa','N','srednia','mediana','SD','Q1','Q3','IQR')]
  names(tab) <- c('Grupa','N','Średnia','Mediana','SD','Q1','Q3','IQR')
  pokaz_tabele(tab,'Opis czasu w grupach [min]',3L)
}
porownanie_histogramow <- function() pokaz_wykres(wykres_przedzialow)
tabela_progow <- function(tabela,prog_min=10) {
  tab <- udzial_do_progu(tabela,prog_min)
  tab$procent <- 100*tab$licznik/tab$mianownik
  tab$pytanie <- c('Sam czas','Sukces i czas')
  names(tab) <- c('Warunek','Próg [min]','Licznik','Mianownik','Procent')
  tab
}
opis_progow <- function(tabela,prog_min=10) {
  pokaz_tabele(tabela_progow(tabela,prog_min),'Dwa pytania o czas nie większy niż próg',2L)
}
tabela_powodzen <- function() {
  tab <- powodzenie_grup
  names(tab) <- c('Grupa','Znane wyniki','Sukcesy','Sukcesy [%]')
  pokaz_tabele(tab,'Powodzenie w każdej grupie',1L)
}
czas_wszystkich_i_udanych <- function() {
  tab <- rbind(
    cbind(zakres='Wszystkie ważne czasy',miary_wszystkie[c('N','srednia','mediana','SD')]),
    cbind(zakres='Wyłącznie udane próby',miary_sukces[c('N','srednia','mediana','SD')]))
  names(tab) <- c('Zakres','N','Średnia','Mediana','SD')
  pokaz_tabele(tab,'Wszystkie i udane próby; czas w minutach',2L)
}
portret_repozytorium <- function() {
  tab <- repozytorium
  names(tab) <- c('Osoba','Czas [min]','Powodzenie (0/1)')
  wydruk_zi(tabele=list('Dwanaście osób w repozytorium'=tab,
    'Opis ważnych czasów [min]'=repo_opis))
}
kwantyle_i_dystrybuanta <- function() {
  czas <- repozytorium$czas_wyszukiwania[!is.na(repozytorium$czas_wyszukiwania)]
  tab <- rbind(repo_kwantyle,data.frame(Miara='IQR = Q3 − Q1',Minuty=repo_kwantyle$Minuty[3]-repo_kwantyle$Minuty[1]))
  udzial <- data.frame('Ważne czasy'=length(czas),'Czasy do 10 min'=sum(czas<=10),
    'Udział [%]'=100*mean(czas<=10),check.names=FALSE)
  wydruk_zi(tabele=list('Kwantyle czasu [min]'=tab,'Dystrybuanta przy 10 minutach'=udzial),
    wykresy=list(repo_ecdf),digits=2L)
}
wniosek_repozytorium <- function(prog_min=10) {
  # Komplet liczb do akapitu dla instytucji: położenie, rozrzut i trzy wskaźniki skuteczności.
  czas <- repozytorium$czas_wyszukiwania
  wazne <- !is.na(czas)
  para <- wazne & repozytorium$powodzenie==1 & czas<=prog_min
  miary <- data.frame(Miara=c('Osoby','Ważne czasy','Średnia [min]','Mediana [min]','SD [min]',
      'IQR [min]','P90 [min]'),
    'Wartość'=c(nrow(repozytorium),sum(wazne),mean(czas,na.rm=TRUE),median(czas,na.rm=TRUE),
      sd(czas,na.rm=TRUE),IQR(czas,na.rm=TRUE),unname(quantile(czas,.9,na.rm=TRUE))),check.names=FALSE)
  skutecznosc <- data.frame(Wskaźnik=c('Sukces (wszystkie osoby)',paste('Sukces w najwyżej',prog_min,'min')),
    Licznik=c(sum(repozytorium$powodzenie),sum(para)),Mianownik=c(nrow(repozytorium),sum(wazne)),
    check.names=FALSE)
  skutecznosc$'Procent [%]' <- 100*skutecznosc$Licznik/skutecznosc$Mianownik
  wydruk_zi(tabele=list('Miary czasu do akapitu'=miary,'Skuteczność do akapitu'=skutecznosc),
    wykresy=list(repo_wykres),digits=2L)
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
jednostki_czasu <- function() pokaz_tabele(skalowanie_demo,'Ten sam czas w dwóch jednostkach',3L)
sklad_zadan <- function() {
  tab <- sklad_demo
  names(tab) <- c('Wersja','Zadanie','Sukcesy','N','Sukces [%]')
  pokaz_tabele(tab,'Wyniki wewnątrz rodzaju zadania',1L)
}
sklad_polaczony <- function() {
  tab <- sklad_ogolem
  names(tab) <- c('Wersja','Sukcesy','N','Sukces [%]')
  pokaz_tabele(tab,'Wyniki po połączeniu zadań',1L)
}
sklad_wazony <- function() {
  tab <- sklad_wspolny
  names(tab) <- c('Wersja','Sukces przy wagach 50/50 [%]')
  pokaz_tabele(tab,'Hipotetyczna mieszanka pół na pół',1L)
}
