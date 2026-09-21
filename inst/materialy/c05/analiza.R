# S02: zachowujemy reguły czyszczenia i indeksu z poprzednich spotkań.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_',1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)

analiza_jednej_sredniej <- function(x, mu0=3, B=1999L, ziarno=202627L) {
  stopifnot(is.numeric(x), length(mu0)==1L, is.finite(mu0),
            length(B)==1L, B>=99L, B==as.integer(B))
  wazne <- x[is.finite(x)]
  stopifnot(length(wazne)>1L, sd(wazne)>0)
  n <- length(wazne)
  test <- t.test(wazne,mu=mu0)
  delta <- mean(wazne)-mu0
  set.seed(ziarno)
  boot <- replicate(B,mean(sample(wazne,size=n,replace=TRUE)))
  # Dokładne uzasadnienie zmian znaków wymaga symetrii odchyleń pod H0.
  zerowy <- replicate(B,mean((wazne-mu0)*sample(c(-1,1),n,replace=TRUE)))
  skrajne <- sum(abs(zerowy)>=abs(delta)-1e-12)
  p_mc <- (1+skrajne)/(B+1)
  ci_boot <- unname(quantile(boot,c(.025,.975),type=7))
  list(
    dane=wazne, bootstrap=boot, zerowy=zerowy, test=test,
    opis=data.frame(N=n,braki=sum(!is.finite(x)),srednia=mean(wazne),
                     SD=sd(wazne),SE=sd(wazne)/sqrt(n),odniesienie=mu0,
                     roznica=delta,d=delta/sd(wazne)),
    klasyczny=data.frame(t=unname(test$statistic),df=unname(test$parameter),
                          p=test$p.value),
    losowanie=data.frame(T_obserwowane=delta,skrajne=skrajne,B=B,p_MC=p_mc),
    przedzialy=data.frame(
      metoda=rep(c('t','Bootstrap percentylowy'),each=2),
      parametr=rep(c('Średnia','Różnica od odniesienia'),2),
      estymata=rep(c(mean(wazne),delta),2),
      dol=c(test$conf.int[1],test$conf.int[1]-mu0,ci_boot[1],ci_boot[1]-mu0),
      gora=c(test$conf.int[2],test$conf.int[2]-mu0,ci_boot[2],ci_boot[2]-mu0)),
    SE_boot=sd(boot)
  )
}
wynik <- analiza_jednej_sredniej(dane$indeks)
formatuj_wynik <- function(x) {
  out <- x
  for (j in names(out)[grepl('^p($|_)',names(out))])
    if(is.numeric(out[[j]])) out[[j]] <- format.pval(out[[j]],digits=3,eps=.0001)
  out
}
pionowo <- function(x) {
  data.frame(wielkosc=names(x),wartosc=vapply(x,function(z) {
    if(is.numeric(z)) format(round(z,4),trim=TRUE) else as.character(z)
  },character(1)),row.names=NULL)
}
wykres_danych <- ggplot2::ggplot(data.frame(indeks=wynik$dane),
    ggplot2::aes(x=indeks)) +
  ggplot2::geom_histogram(binwidth=.25,boundary=1,fill='#56B4E9',colour='white') +
  ggplot2::geom_vline(xintercept=3,linetype=2,colour='#D55E00') +
  ggplot2::labs(x='Indeks deklarowanej użyteczności [1–5]',y='Liczba osób',
                 title='Dane osób; linia to umowne odniesienie 3') +
  badaniaZI::theme_zi()
wykres_bootstrap <- ggplot2::ggplot(data.frame(srednia=wynik$bootstrap),
    ggplot2::aes(x=srednia)) +
  ggplot2::geom_histogram(bins=30,fill='#0072B2',colour='white') +
  ggplot2::geom_vline(xintercept=mean(wynik$dane),colour='#D55E00') +
  ggplot2::labs(x='Średnia w replice bootstrapowej [pkt]',y='Liczba replik',
                 title='Bootstrap: niepewność średniej, nie rozkład zerowy') +
  badaniaZI::theme_zi()
wykres_zerowy <- ggplot2::ggplot(data.frame(T=wynik$zerowy),
    ggplot2::aes(x=T)) +
  ggplot2::geom_histogram(bins=30,fill='#009E73',colour='white') +
  ggplot2::geom_vline(xintercept=c(-1,1)*abs(wynik$opis$roznica),
                     colour='#D55E00',linewidth=.8) +
  ggplot2::labs(x='Średnia odchyleń po zmianie znaków [pkt]',y='Liczba losowań',
                 title='Rozkład zerowy; linie wyznaczają skrajność dwustronną') +
  badaniaZI::theme_zi()
ci_mean <- subset(wynik$przedzialy,parametr=='Średnia')
wykres_ci <- ggplot2::ggplot(ci_mean,
    ggplot2::aes(x=estymata,y=metoda)) +
  ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=metoda),linewidth=.9) +
  ggplot2::geom_point(size=3,colour='#0072B2') +
  ggplot2::geom_vline(xintercept=3,linetype=2,colour='#D55E00') +
  ggplot2::labs(x='Średnia i 95% przedział [pkt]',y=NULL,
                 title='Ten sam parametr, dwie metody przedziału') +
  badaniaZI::theme_zi()
male_x <- c(2,3,3,4,4,5)
indeksy_boot <- c(2,2,6,1,4,4)
jedna_replika <- data.frame(miejsce=1:6,osoba_zrodlowa=indeksy_boot,
                            indeks=male_x[indeksy_boot])
male_odchylenia <- c(-1,1,2)
wszystkie_znaki <- expand.grid(z1=c(-1,1),z2=c(-1,1),z3=c(-1,1))
# Każdy wiersz zawiera znaki przypisane trzem obserwowanym odchyleniom.
wszystkie_znaki$T <- as.vector(as.matrix(wszystkie_znaki[1:3]) %*% male_odchylenia)/3
wszystkie_znaki$skrajne <- abs(wszystkie_znaki$T)>=abs(mean(male_odchylenia))-1e-12
p_dokladne <- mean(wszystkie_znaki$skrajne)
wplyw_n <- do.call(rbind,lapply(c(25,100,400),function(n) {
  SE <- .8/sqrt(n)
  stat <- .2/SE
  data.frame(N=n,srednia=3.2,SD=.8,roznica=.2,d=.25,SE=SE,t=stat,df=n-1,
             p=2*pt(-abs(stat),n-1))
}))
wplyw_odniesienia <- do.call(rbind,lapply(c(3,3.2,3.5),function(ref) {
  test <- t.test(wynik$dane,mu=ref)
  data.frame(odniesienie=ref,roznica=mean(wynik$dane)-ref,
             CI_sredniej_dol=test$conf.int[1],CI_sredniej_gora=test$conf.int[2],
             t=unname(test$statistic),p=test$p.value)
}))
monte_carlo <- data.frame(B=c(199,1999,19999))
monte_carlo$najmniejsze_p <- 1/(monte_carlo$B+1)
monte_carlo$przyblizone_SE_p_przy_005 <- sqrt(.05*.95/monte_carlo$B)
binom <- binom.test(26,40,p=.5)
przyklad_proporcji <- data.frame(sukcesy=26,N=40,proporcja=26/40,
  odniesienie=.5,CI_dol=binom$conf.int[1],CI_gora=binom$conf.int[2],p=binom$p.value)

# Osobny syntetyczny scenariusz; indeks z sześciu ukierunkowanych pozycji 1–5.
portal_indeksy <- c(rep(2,2),rep(2.5,3),rep(3,6),rep(3.5,6),rep(4,5),rep(4.5,2),NA)
portal_wynik <- analiza_jednej_sredniej(portal_indeksy,mu0=3,ziarno=202628L)
portal_ci_sredniej <- subset(portal_wynik$przedzialy,parametr=='Średnia')
portal_wykres_ci <- ggplot2::ggplot(portal_ci_sredniej,
    ggplot2::aes(x=estymata,y=metoda))+
  ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=metoda),linewidth=.8)+
  ggplot2::geom_point(size=3,colour='#0072B2')+
  ggplot2::geom_vline(xintercept=c(3,3.5),linetype=c(2,3))+
  ggplot2::labs(x='Średnia deklarowanej przejrzystości [pkt 1–5]',y=NULL,
    title='95% przedziały; odniesienie 3 i umowny cel 3,5')+badaniaZI::theme_zi()
portal_wykres_boot <- ggplot2::ggplot(data.frame(srednia=portal_wynik$bootstrap),
    ggplot2::aes(x=srednia))+
  ggplot2::geom_histogram(bins=25,fill='#0072B2',colour='white')+
  ggplot2::labs(x='Średnia w replice [pkt]',y='Liczba replik',
    title='Bootstrap: losowanie osób ze zwracaniem')+badaniaZI::theme_zi()
portal_wykres_zerowy <- ggplot2::ggplot(data.frame(T=portal_wynik$zerowy),
    ggplot2::aes(x=T))+
  ggplot2::geom_histogram(bins=25,fill='#009E73',colour='white')+
  ggplot2::geom_vline(xintercept=c(-1,1)*abs(portal_wynik$opis$roznica),
    colour='#D55E00')+
  ggplot2::labs(x='Średnia odchyleń po zmianie znaków [pkt]',y='Liczba losowań',
    title='Model zerowy przy symetrii odchyleń')+badaniaZI::theme_zi()

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


opis_sredniej <- function(analiza) {
  pokaz_tabele(pionowo(analiza$opis),'Osoby, średnia, niepewność i efekt')
}
test_sredniej <- function(analiza) {
  pokaz_tabele(formatuj_wynik(analiza$klasyczny),'Dwustronny test t jednej średniej',4L)
}
oryginal_testu_sredniej <- function(analiza) {
  wydruk_zi(tekst=trimws(capture.output(print(analiza$test)),which='right'))
}
przedzialy_sredniej <- function(analiza,metoda=c('obie','t','bootstrap')) {
  metoda <- match.arg(metoda)
  tab <- analiza$przedzialy
  if(metoda=='t') tab <- tab[tab$metoda=='t',,drop=FALSE]
  if(metoda=='bootstrap') tab <- tab[tab$metoda!='t',,drop=FALSE]
  # Osobne tabele utrzymują jawny parametr i mieszczą się na A4.
  wydruk_zi(tabele=list(
    '95% CI średniej [pkt]'=tab[tab$parametr=='Średnia',c('metoda','estymata','dol','gora')],
    '95% CI różnicy od odniesienia [pkt]'=tab[tab$parametr!='Średnia',c('metoda','estymata','dol','gora')]),
    digits=4L)
}
test_znakow_sredniej <- function(analiza) {
  pokaz_tabele(formatuj_wynik(analiza$losowanie),'Zmiana znaków: T w punktach, p Monte Carlo',4L)
}
dwa_testy_sredniej <- function(analiza) {
  wydruk_zi(tabele=list(
    'Dwustronny test t'=formatuj_wynik(analiza$klasyczny),
    'Losowe zmiany znaków'=formatuj_wynik(analiza$losowanie)),digits=4L)
}
podsumowanie_sredniej <- function(analiza) {
  efekt <- analiza$opis[c('N','roznica','d')]
  nazwy <- przedzialy_sredniej(analiza)$tabele
  wydruk_zi(tabele=c(list('Efekt i N'=efekt,
    'Wynik klasyczny'=formatuj_wynik(analiza$klasyczny),
    'Wynik zmian znaków'=formatuj_wynik(analiza$losowanie)),nazwy),digits=4L)
}
dokladnosc_losowania <- function() {
  tab <- monte_carlo
  names(tab) <- c('B','Najmniejsze p','Przybliżone SE udziału przy p=0,05')
  pokaz_tabele(tab,'Dokładność Monte Carlo, nie liczba osób',5L)
}
efekt_i_liczebnosc <- function() {
  tab <- wplyw_n[c('N','roznica','d','SE','t','df','p')]
  pokaz_tabele(formatuj_wynik(tab),'Ten sam efekt, różne liczebności',4L)
}
zmiana_odniesienia <- function() {
  ci <- unique(wplyw_odniesienia[c('CI_sredniej_dol','CI_sredniej_gora')])
  names(ci) <- c('Dolna granica','Górna granica')
  wydruk_zi(tabele=list('Niezmieniony 95% CI średniej'=ci,
    'Różne pytania zerowe'=formatuj_wynik(wplyw_odniesienia[c('odniesienie','roznica','t','p')])),
    digits=4L)
}
przyklad_proporcji_do_raportu <- function() {
  pokaz_tabele(pionowo(formatuj_wynik(przyklad_proporcji)),
    'Odrębne pytanie o proporcję: dokładny test dwumianowy')
}
przedzialy_portalu_do_raportu <- function() {
  wynik <- przedzialy_sredniej(portal_wynik)
  wynik$wykresy <- list(portal_wykres_ci)
  wynik
}
rozklady_portalu_do_odczytu <- function() {
  wydruk_zi(wykresy=list(portal_wykres_boot,portal_wykres_zerowy))
}
efekt_i_cel_portalu <- function() {
  tab <- portal_wynik$opis[c('srednia','odniesienie','roznica','d')]
  wydruk_zi(tabele=list('Średnia i efekt od 3 [pkt]; d standaryzowane'=tab,
    '95% CI średniej względem odniesień 3 i 3,5'=portal_ci_sredniej[c('metoda','estymata','dol','gora')]),
    digits=4L)
}
