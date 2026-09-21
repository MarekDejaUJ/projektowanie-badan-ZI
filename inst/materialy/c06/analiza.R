dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_',1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)

porownaj_grupy <- function(dane, zmienna='indeks', grupa_1='doświadczeni',
                           grupa_2='nowi', B=1999L, ziarno=202627L) {
  stopifnot(zmienna %in% names(dane),is.numeric(dane[[zmienna]]),
            grupa_1!=grupa_2,length(B)==1L,B>=99L,B==as.integer(B))
  komplet <- !is.na(dane$grupa) & is.finite(dane[[zmienna]])
  x <- dane[komplet & dane$grupa %in% c(grupa_1,grupa_2),c('grupa',zmienna)]
  names(x)[2] <- 'wynik'
  x$grupa <- factor(x$grupa,levels=c(grupa_1,grupa_2))
  a <- x$wynik[x$grupa==grupa_1]
  b <- x$wynik[x$grupa==grupa_2]
  stopifnot(length(a)>1L,length(b)>1L,sd(a)>0,sd(b)>0)
  # Jawna kolejność: grupa 2 minus grupa 1, także dla t i CI.
  test <- t.test(b,a)
  delta <- mean(b)-mean(a)
  se <- sqrt(var(a)/length(a)+var(b)/length(b))
  sp <- sqrt(((length(a)-1)*var(a)+(length(b)-1)*var(b))/(length(a)+length(b)-2))
  J <- 1-3/(4*(length(a)+length(b))-9)
  set.seed(ziarno)
  zerowy <- replicate(B,{
    g <- sample(x$grupa)
    mean(x$wynik[g==grupa_2])-mean(x$wynik[g==grupa_1])
  })
  boot <- replicate(B,mean(sample(b,replace=TRUE))-mean(sample(a,replace=TRUE)))
  skrajne <- sum(abs(zerowy)>=abs(delta)-1e-12)
  ci <- unname(quantile(boot,c(.025,.975)))
  opis <- do.call(rbind,lapply(c(grupa_1,grupa_2),function(g) {
    y <- x$wynik[x$grupa==g]
    data.frame(grupa=g,N=length(y),
      braki_wyniku=sum(dane$grupa==g & !is.finite(dane[[zmienna]]),na.rm=TRUE),
      srednia=mean(y),SD=sd(y),mediana=median(y),IQR=IQR(y))
  }))
  list(dane=x,test=test,opis=opis,zerowy=zerowy,bootstrap=boot,
       kierunek=paste(grupa_2,'minus',grupa_1),
       klasyczny=data.frame(roznica=delta,SE=se,t=unname(test$statistic),
                             df=unname(test$parameter),p=test$p.value),
       efekt=data.frame(roznica=delta,SD_laczone=sp,d=delta/sp,J=J,Hedges_g=J*delta/sp),
       losowanie=data.frame(T_obserwowane=delta,skrajne=skrajne,B=B,
                             p_perm=(1+skrajne)/(B+1)),
       przedzialy=data.frame(metoda=c('Welch','Bootstrap w grupach'),
         estymata=delta,dol=c(test$conf.int[1],ci[1]),gora=c(test$conf.int[2],ci[2])))
}
wynik <- porownaj_grupy(dane)
wynik_czas <- porownaj_grupy(dane,'czas_wyszukiwania')
formatuj_wynik <- function(x) {
  for(j in names(x)[grepl('^p($|_)',names(x))])
    if(is.numeric(x[[j]])) x[[j]] <- format.pval(x[[j]],digits=3,eps=.0001)
  x
}
pionowo <- function(x) {
  y <- formatuj_wynik(x)
  data.frame(wielkosc=names(y),wartosc=vapply(y,function(z) {
    if(is.numeric(z)) format(round(z,4),trim=TRUE) else as.character(z)
  },character(1)),row.names=NULL)
}
wykres_grup <- ggplot2::ggplot(wynik$dane,ggplot2::aes(x=grupa,y=wynik)) +
  ggplot2::geom_boxplot(fill='#56B4E9',width=.45,outlier.colour='#D55E00') +
  ggplot2::stat_summary(fun=mean,geom='point',shape=18,size=3,colour='#0072B2') +
  ggplot2::labs(x='Grupa',y='Indeks deklarowanej użyteczności [1–5]',
                 title='Pudełko pokazuje rozrzut; romb oznacza średnią') +
  badaniaZI::theme_zi()
wykres_zerowy <- ggplot2::ggplot(data.frame(delta=wynik$zerowy),
    ggplot2::aes(x=delta)) +
  ggplot2::geom_histogram(bins=30,fill='#009E73',colour='white') +
  ggplot2::geom_vline(xintercept=c(-1,1)*abs(wynik$klasyczny$roznica),
                     colour='#D55E00',linewidth=.8) +
  ggplot2::labs(x='Różnica średnich po tasowaniu [pkt]',y='Liczba losowań',
                 title='Etykiety przestawione, wyniki osób zachowane') +
  badaniaZI::theme_zi()
wykres_boot <- ggplot2::ggplot(data.frame(delta=wynik$bootstrap),
    ggplot2::aes(x=delta)) +
  ggplot2::geom_histogram(bins=30,fill='#0072B2',colour='white') +
  ggplot2::geom_vline(xintercept=wynik$klasyczny$roznica,colour='#D55E00') +
  ggplot2::labs(x='Różnica średnich w bootstrapie [pkt]',y='Liczba replik',
                 title='Losowanie osobno w grupach zachowuje ich znaczenie') +
  badaniaZI::theme_zi()
wykres_ci <- ggplot2::ggplot(wynik$przedzialy,
    ggplot2::aes(x=estymata,y=metoda)) +
  ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=metoda),linewidth=.9) +
  ggplot2::geom_point(size=3,colour='#0072B2') +
  ggplot2::geom_vline(xintercept=0,linetype=2,colour='#D55E00') +
  ggplot2::labs(x='Nowi minus doświadczeni [pkt]',y=NULL,
                 title='Niepewność tej samej różnicy, dwie procedury') +
  badaniaZI::theme_zi()
mini <- data.frame(osoba=letters[1:4],indeks=c(1,2,4,5),
                    grupa=c('doświadczeni','doświadczeni','nowi','nowi'))
kombinacje <- combn(1:4,2)
mini_permutacje <- do.call(rbind,lapply(seq_len(ncol(kombinacje)),function(j) {
  k <- kombinacje[,j]
  data.frame(nowi=paste(mini$osoba[k],collapse=', '),
    doswiadczeni=paste(mini$osoba[-k],collapse=', '),
    roznica=mean(mini$indeks[k])-mean(mini$indeks[-k]))
}))
mini_permutacje$skrajna <- abs(mini_permutacje$roznica)>=3
odwrocenie_kierunku <- data.frame(
  kierunek=c('Nowi minus doświadczeni','Doświadczeni minus nowi'),
  roznica=c(wynik$klasyczny$roznica,-wynik$klasyczny$roznica),
  t=c(wynik$klasyczny$t,-wynik$klasyczny$t),
  CI_dol=c(wynik$przedzialy$dol[1],-wynik$przedzialy$gora[1]),
  CI_gora=c(wynik$przedzialy$gora[1],-wynik$przedzialy$dol[1]))
pary <- data.frame(osoba=letters[1:6],przed=c(6,8,10,12,14,16),po=c(5,6,9,9,12,12))
pary$zmiana <- pary$po-pary$przed
test_par <- t.test(pary$po,pary$przed,paired=TRUE)
podsumowanie_par <- data.frame(N_par=6,zmiana=mean(pary$zmiana),
  SD_zmian=sd(pary$zmiana),SE=sd(pary$zmiana)/sqrt(6),
  CI_dol=test_par$conf.int[1],CI_gora=test_par$conf.int[2],
  t=unname(test_par$statistic),df=unname(test_par$parameter),p=test_par$p.value)
ci_grup_demo <- data.frame(grupa=c('A','B'),N=100,srednia=c(3,3.3),SD=1)
ci_grup_demo$dol <- ci_grup_demo$srednia-qt(.975,99)/10
ci_grup_demo$gora <- ci_grup_demo$srednia+qt(.975,99)/10
ci_roznicy_demo <- data.frame(roznica=.3,SE=sqrt(2/100),
  dol=.3-qt(.975,198)*sqrt(2/100),gora=.3+qt(.975,198)*sqrt(2/100))
wykres_nakladania <- ggplot2::ggplot(ci_grup_demo,
    ggplot2::aes(x=srednia,y=grupa)) +
  ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=grupa),linewidth=.9) +
  ggplot2::geom_point(size=3,colour='#0072B2') +
  ggplot2::labs(x='Średnia i jej 95% CI [pkt]',y='Hipotetyczna grupa',
                 title='Przedziały średnich mogą się nakładać') +
  badaniaZI::theme_zi()

repozytorium <- data.frame(osoba=sprintf('r%02d',1:24),
  grupa=rep(c('doświadczeni','nowi'),each=12),
  czas_min=c(7,8,10,11,6,9,8,13,5,11,10,10,
    9,11,12,14,8,13,10,16,7,12,14,9))
repo_wynik <- porownaj_grupy(repozytorium,'czas_min',ziarno=202628L)
repo_opis <- repo_wynik$opis
repo_opis$minimum <- vapply(c('doświadczeni','nowi'),function(g)
  min(repozytorium$czas_min[repozytorium$grupa==g]),numeric(1))
repo_opis$maksimum <- vapply(c('doświadczeni','nowi'),function(g)
  max(repozytorium$czas_min[repozytorium$grupa==g]),numeric(1))
repo_wykres <- ggplot2::ggplot(repo_wynik$dane,ggplot2::aes(grupa,wynik))+
  ggplot2::geom_boxplot(fill='#56B4E9',width=.45)+
  ggplot2::stat_summary(fun=mean,geom='point',shape=18,size=3,colour='#0072B2')+
  ggplot2::labs(x='Doświadczenie',y='Czas [min]',
    title='Repozytorium: rozrzut osób i średnie grup')+badaniaZI::theme_zi()
repo_wykres_ci <- ggplot2::ggplot(repo_wynik$przedzialy,
    ggplot2::aes(estymata,metoda))+
  ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=metoda),linewidth=.8)+
  ggplot2::geom_point(size=3,colour='#0072B2')+
  ggplot2::geom_vline(xintercept=0,linetype=2)+
  ggplot2::labs(x='Nowi minus doświadczeni: różnica średnich [min]',y=NULL)+
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
      cat(as.character(knitr::kable(tab,format=format,caption=NULL,
        digits=x$digits,row.names=FALSE)),'\n',sep='')
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
opis_porownania <- function(analiza,jednostka='pkt 1–5',wykres=FALSE) {
  tab <- analiza$opis[,c('grupa','N','srednia','SD','mediana','IQR')]
  if(wykres) {
    tab$minimum <- vapply(levels(analiza$dane$grupa),function(g)
      min(analiza$dane$wynik[analiza$dane$grupa==g]),numeric(1))
    tab$maksimum <- vapply(levels(analiza$dane$grupa),function(g)
      max(analiza$dane$wynik[analiza$dane$grupa==g]),numeric(1))
    tab <- tab[,c('grupa','N','srednia','SD','minimum','maksimum')]
  }
  wyk <- ggplot2::ggplot(analiza$dane,ggplot2::aes(grupa,wynik))+
    ggplot2::geom_boxplot(fill='#56B4E9',width=.45)+
    ggplot2::stat_summary(fun=mean,geom='point',shape=18,size=3,colour='#0072B2')+
    ggplot2::labs(x='Grupa',y=paste0('Wynik [',jednostka,']'))+badaniaZI::theme_zi()
  wydruk_zi(tabele=setNames(list(tab),paste0('Opis grup [',jednostka,']')),
    wykresy=if(wykres) list(wyk) else list())
}
test_porownania <- function(analiza) {
  pokaz_tabele(pionowo(analiza$klasyczny),'Test Welcha — różnica grupy 2 minus grupy 1',4L)
}
oryginal_testu <- function(analiza) wydruk_zi(tekst=capture.output(print(analiza$test)))
przedzial_porownania <- function(analiza) {
  pokaz_tabele(analiza$przedzialy,'95% przedziały różnicy średnich',4L)
}
efekt_porownania <- function(analiza) {
  pokaz_tabele(pionowo(analiza$efekt),'Efekt surowy i standaryzowany',4L)
}
permutacja_porownania <- function(analiza) {
  pokaz_tabele(formatuj_wynik(analiza$losowanie),'Tasowanie etykiet grup',4L)
}
podsumowanie_porownania <- function(analiza,jednostka='pkt 1–5') {
  ci <- ggplot2::ggplot(analiza$przedzialy,ggplot2::aes(estymata,metoda))+
    ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=metoda),linewidth=.8)+
    ggplot2::geom_point(size=3,colour='#0072B2')+
    ggplot2::geom_vline(xintercept=0,linetype=2)+
    ggplot2::labs(x=paste0(analiza$kierunek,' [',jednostka,']'),y=NULL)+
    badaniaZI::theme_zi()
  wydruk_zi(tabele=list('Efekty'=pionowo(analiza$efekt),
    '95% przedziały różnicy'=analiza$przedzialy),wykresy=list(ci),digits=4L)
}
wynik_do_raportu <- function(analiza) {
  wydruk_zi(tabele=list('Test Welcha'=pionowo(analiza$klasyczny),
    'Klasyczny 95% przedział różnicy'=analiza$przedzialy[1,]),digits=4L)
}
