dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane

statystyka_chi <- function(tab) {
  if(any(rowSums(tab)==0) || any(colSums(tab)==0)) return(NA_real_)
  E <- outer(rowSums(tab),colSums(tab))/sum(tab)
  sum((tab-E)^2/E)
}
analiza_tabeli <- function(tab, B=1999L, ziarno=202627L) {
  tab <- as.matrix(tab)
  stopifnot(all(dim(tab)==c(2,2)),all(is.finite(tab)),all(tab>=0),
            all(tab==round(tab)),all(rowSums(tab)>0),all(colSums(tab)>0),
            length(B)==1L,B>=99L,B==as.integer(B))
  # Kolumny 0 i 1; kontrast proporcji sukcesu: wiersz 2 minus wiersz 1.
  N <- sum(tab)
  E <- outer(rowSums(tab),colSums(tab))/N
  dimnames(E) <- dimnames(tab)
  chi <- statystyka_chi(tab)
  pchi <- pchisq(chi,df=1,lower.tail=FALSE)
  fisher <- fisher.test(tab)
  set.seed(ziarno)
  tabele_zerowe <- r2dtable(B,rowSums(tab),colSums(tab))
  zerowy <- vapply(tabele_zerowe,statystyka_chi,numeric(1))
  k <- sum(zerowy>=chi-1e-12)
  y <- lapply(1:2,function(i) rep(0:1,times=tab[i,]))
  prop <- tab[,2]/rowSums(tab)
  delta <- prop[2]-prop[1]
  # Ten prosty CI różnicy jest przybliżeniem dużopróbkowym, nie metodą dla rzadkich komórek.
  test_prop <- suppressWarnings(prop.test(tab[2:1,2],rowSums(tab)[2:1],correct=FALSE))
  boot <- replicate(B,{
    b1 <- sample(y[[1]],replace=TRUE)
    b2 <- sample(y[[2]],replace=TRUE)
    tb <- rbind(tabulate(b1+1,nbins=2),tabulate(b2+1,nbins=2))
    c(roznica=mean(b2)-mean(b1),V=sqrt(statystyka_chi(tb)/N))
  })
  ci_delta <- unname(quantile(boot['roznica',],c(.025,.975)))
  vboot <- boot['V',is.finite(boot['V',])]
  ci_v <- unname(quantile(vboot,c(.025,.975)))
  list(tab=tab,E=E,wklady=(tab-E)^2/E,reszty=(tab-E)/sqrt(E),
    opis=data.frame(grupa=rownames(tab),N=as.integer(rowSums(tab)),
      sukcesy=as.integer(tab[,2]),procent=100*prop,row.names=NULL),
    klasyczny=data.frame(N=N,chi2=chi,df=1,p_chi2=pchi,min_E=min(E),
      komorki_E_mniejsze_5=sum(E<5)),
    fisher=data.frame(OR_warunkowe=unname(fisher$estimate),
      CI_OR_dol=fisher$conf.int[1],CI_OR_gora=fisher$conf.int[2],
      p_Fisher=fisher$p.value),
    wybor=if(any(E<5)) 'Fisher dla tabeli 2 x 2; chi-kwadrat opisowo' else
      'Chi-kwadrat bez korekty ciągłości',
    losowanie=data.frame(chi2=chi,skrajne=k,B=B,p_MC=(1+k)/(1+B)),
    zerowy=zerowy,przykladowa_tabela=tabele_zerowe[[1]],
    efekty=data.frame(roznica_proporcji=unname(delta),
      roznica_pp=100*unname(delta),
      iloraz_proporcji=unname(prop[2]/prop[1]),
      OR_surowe=unname((tab[2,2]/tab[2,1])/(tab[1,2]/tab[1,1])),
      V=sqrt(chi/N)),
    przedzialy=data.frame(metoda=c('Przybliżenie bez korekty','Bootstrap w grupach'),
      estymata=unname(delta),dol=c(test_prop$conf.int[1],ci_delta[1]),
      gora=c(test_prop$conf.int[2],ci_delta[2])),
    bootstrap_roznicy=boot['roznica',],bootstrap_V=vboot,
    opis_boot_V=data.frame(V=sqrt(chi/N),percentyl_025=ci_v[1],
      percentyl_975=ci_v[2],B_wazne=length(vboot),B_nieokreslone=B-length(vboot)))
}
tab <- table(factor(dane$grupa,levels=c('doświadczeni','nowi')),
             factor(dane$powodzenie,levels=0:1))
dimnames(tab) <- list(grupa=c('doświadczeni','nowi'),powodzenie=c('0','1'))
wynik <- analiza_tabeli(tab)
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
procent_wiersz <- 100*prop.table(tab,1)
procent_kolumna <- 100*prop.table(tab,2)
plot_data <- as.data.frame(prop.table(tab,1))
names(plot_data) <- c('grupa','powodzenie','udzial')
wykres_proporcji <- ggplot2::ggplot(plot_data,
    ggplot2::aes(x=grupa,y=udzial,fill=powodzenie)) +
  ggplot2::geom_col(width=.6) +
  ggplot2::scale_y_continuous(labels=function(x) paste0(round(100*x),'%')) +
  ggplot2::scale_fill_manual(values=c('0'='#D55E00','1'='#009E73'),
                              labels=c('0: niepowodzenie','1: sukces')) +
  ggplot2::labs(x='Grupa',y='Odsetek w grupie',fill=NULL,
                 title='Powodzenie i niepowodzenie mają wspólny mianownik') +
  badaniaZI::theme_zi() + ggplot2::theme(legend.position='bottom')
komorki <- as.data.frame(tab)
komorki$E <- as.vector(wynik$E)
komorki$reszta <- as.vector(wynik$reszty)
wykres_reszt <- ggplot2::ggplot(komorki,
    ggplot2::aes(x=powodzenie,y=grupa,fill=reszta)) +
  ggplot2::geom_tile(colour='white') +
  ggplot2::geom_text(ggplot2::aes(label=paste0('O=',Freq,'\nE=',round(E,1))),size=4) +
  ggplot2::scale_fill_gradient2(low='#D55E00',mid='white',high='#56B4E9',midpoint=0) +
  ggplot2::labs(x='Powodzenie: 0 nie, 1 tak',y=NULL,fill='Reszta\nPearsona',
                 title='Kierunek odchylenia komórki od niezależności') +
  badaniaZI::theme_zi()
wykres_zerowy <- ggplot2::ggplot(data.frame(chi2=wynik$zerowy),
    ggplot2::aes(x=chi2)) +
  ggplot2::geom_histogram(bins=30,fill='#009E73',colour='white') +
  ggplot2::geom_vline(xintercept=wynik$klasyczny$chi2,colour='#D55E00',linewidth=.8) +
  ggplot2::labs(x='Chi-kwadrat w losowej tabeli',y='Liczba losowań',
                 title='Rozkład zerowy przy ustalonych marginesach') +
  badaniaZI::theme_zi()
wykres_ci <- ggplot2::ggplot(wynik$przedzialy,
    ggplot2::aes(x=100*estymata,y=metoda)) +
  ggplot2::geom_segment(ggplot2::aes(x=100*dol,xend=100*gora,yend=metoda),linewidth=.8) +
  ggplot2::geom_point(size=3,colour='#0072B2') +
  ggplot2::geom_vline(xintercept=0,linetype=2) +
  ggplot2::labs(x='Różnica nowi minus doświadczeni [punkty procentowe]',y=NULL,
                 title='Niepewność różnicy skuteczności') +
  badaniaZI::theme_zi()
# Osobne przykłady, poza S02.
rzadka <- matrix(c(3,7,9,1),2,byrow=TRUE,dimnames=dimnames(tab))
wynik_rzadki <- analiza_tabeli(rzadka)
blisko_zero <- matrix(c(50,47,53,50),2,byrow=TRUE,dimnames=dimnames(tab))
wynik_blisko_zero <- analiza_tabeli(blisko_zero)
wykres_V <- ggplot2::ggplot(data.frame(V=wynik_blisko_zero$bootstrap_V),
    ggplot2::aes(x=V)) +
  ggplot2::geom_histogram(bins=30,fill='#56B4E9',colour='white') +
  ggplot2::geom_vline(xintercept=wynik_blisko_zero$efekty$V,colour='#D55E00') +
  ggplot2::labs(x='V w bootstrapie tabeli bliskiej niezależności',y='Liczba replik',
                 title='Nieujemna miara: percentyle nie są testem zera') +
  badaniaZI::theme_zi()
skalowanie_N <- do.call(rbind,lapply(c(1,10),function(k) {
  tb <- k*tab
  ch <- statystyka_chi(tb)
  data.frame(mnoznik=k,N=sum(tb),chi2=ch,p=pchisq(ch,1,lower.tail=FALSE),
             V=sqrt(ch/sum(tb)))
}))
przed_po <- matrix(c(20,10,2,18),2,byrow=TRUE,
  dimnames=list(przed=c('0','1'),po=c('0','1')))
mcn <- mcnemar.test(przed_po,correct=TRUE)
dokladny_zmian <- binom.test(10,12,p=.5)
wynik_zmian <- data.frame(N_osob=50,poprawa=10,pogorszenie=2,
  zmiana_proporcji=.16,chi2_McNemara=unname(mcn$statistic),df=1,
  p_z_korekta=mcn$p.value,p_dokladne=dokladny_zmian$p.value)

# CHALLENGE: odrębne syntetyczne badanie dostępu do archiwum cyfrowego.
archiwum_bilans <- data.frame(grupa=c('powracający','pierwsza wizyta'),
  zarejestrowani=c(42L,84L),brak_wyniku=c(2L,4L),w_tabeli=c(40L,80L))
archiwum_tab <- matrix(c(10L,30L,32L,48L),nrow=2,byrow=TRUE,
  dimnames=list(grupa=archiwum_bilans$grupa,powodzenie=c('0','1')))
archiwum_wynik <- analiza_tabeli(archiwum_tab,ziarno=202628L)

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


# Macierz staje się tabelą z jawnymi etykietami wierszy, nie samymi liczbami.
tabela_z_etykietami <- function(tab,nazwa='Grupa') {
  out <- data.frame(rownames(tab),as.data.frame.matrix(tab),check.names=FALSE,
    row.names=NULL)
  names(out)[1] <- nazwa
  out
}
liczebnosci_tabeli <- function(analiza,marginesy=FALSE) {
  tab <- analiza$tab
  if(marginesy) tab <- addmargins(tab)
  pokaz_tabele(tabela_z_etykietami(tab),'Liczebności: 0 = niepowodzenie, 1 = sukces',0L)
}
odsetki_tabeli <- function(analiza,mianownik=c('grupa','wynik')) {
  mianownik <- match.arg(mianownik)
  margin <- if(mianownik=='grupa') 1 else 2
  tab <- 100*prop.table(analiza$tab,margin)
  tytul <- if(mianownik=='grupa') 'Odsetki w grupach [%]' else 'Skład kategorii wyniku [%]'
  pokaz_tabele(tabela_z_etykietami(tab),tytul,2L)
}
oczekiwania_tabeli <- function(analiza) {
  pokaz_tabele(tabela_z_etykietami(analiza$E),'E przy niezależności',2L)
}
wklady_tabeli <- function(analiza) {
  pokaz_tabele(tabela_z_etykietami(analiza$wklady),'Wkłady komórek do chi-kwadrat',4L)
}
test_tabeli <- function(analiza) {
  pokaz_tabele(pionowo(analiza$klasyczny),'Chi-kwadrat bez korekty ciągłości')
}
losowa_tabela <- function(analiza) {
  tb <- analiza$przykladowa_tabela
  dimnames(tb) <- dimnames(analiza$tab)
  pokaz_tabele(tabela_z_etykietami(addmargins(tb)),'Jedna tabela z modelu zerowego',0L)
}
monte_carlo_tabeli <- function(analiza) {
  pokaz_tabele(formatuj_wynik(analiza$losowanie),'Warunkowy test Monte Carlo',4L)
}
efekty_tabeli <- function(analiza,rozszerzone=FALSE) {
  tab <- analiza$efekty
  if(!rozszerzone) tab <- tab[c('roznica_proporcji','roznica_pp','V')]
  pokaz_tabele(pionowo(tab),'Efekt: sukces wiersza 2 minus sukces wiersza 1')
}
przedzialy_tabeli <- function(analiza,jednostka=c('proporcja','pp')) {
  jednostka <- match.arg(jednostka)
  tab <- analiza$przedzialy
  if(jednostka=='pp') tab[c('estymata','dol','gora')] <-
    100*tab[c('estymata','dol','gora')]
  pokaz_tabele(tab,paste0('95% CI różnicy [',jednostka,']'),4L)
}
proporcje_na_wykresie <- function(analiza) {
  tab <- as.data.frame(as.table(prop.table(analiza$tab,1)))
  names(tab) <- c('grupa','wynik','udzial')
  ns <- rowSums(analiza$tab)
  plot <- ggplot2::ggplot(tab,ggplot2::aes(x=grupa,y=udzial,fill=wynik))+
    ggplot2::geom_col(width=.6)+
    ggplot2::scale_y_continuous(labels=function(x) paste0(round(100*x),'%'))+
    ggplot2::scale_x_discrete(labels=function(x) paste0(x,'\nN = ',ns[x]))+
    ggplot2::scale_fill_manual(values=c('0'='#D55E00','1'='#009E73'),
      labels=c('0: niepowodzenie','1: sukces'))+
    ggplot2::labs(x=NULL,y='Odsetek w grupie',fill=NULL,
      title='Skuteczność w tym samym zadaniu')+badaniaZI::theme_zi()+
    ggplot2::theme(legend.position='bottom')
  pokaz_wykres(plot)
}
dwa_testy_tabeli <- function(analiza) {
  wydruk_zi(tabele=list('Chi-kwadrat bez korekty'=pionowo(analiza$klasyczny),
    'Warunkowe Monte Carlo'=formatuj_wynik(analiza$losowanie)),digits=4L)
}
rzadka_tabela_do_odczytu <- function() {
  wydruk_zi(tabele=list('Obserwacje'=tabela_z_etykietami(rzadka),
    'Oczekiwania'=tabela_z_etykietami(wynik_rzadki$E)),digits=2L)
}
rzadkie_testy_do_odczytu <- function() {
  wydruk_zi(tabele=list('Przybliżenie chi-kwadrat'=pionowo(wynik_rzadki$klasyczny),
    'Dokładny test Fishera'=pionowo(wynik_rzadki$fisher),
    'Warunkowe Monte Carlo'=pionowo(wynik_rzadki$losowanie)))
}
v_przy_zerze <- function() {
  wydruk_zi(tabele=list('Tabela bliska niezależności'=tabela_z_etykietami(blisko_zero),
    'Test niezależności'=pionowo(wynik_blisko_zero$klasyczny),
    'Opis rozkładu bootstrapowego V'=pionowo(wynik_blisko_zero$opis_boot_V)))
}
pomiar_sparowany_do_odczytu <- function() {
  wydruk_zi(tabele=list('Przed (wiersze) i po (kolumny)'=tabela_z_etykietami(przed_po,'Przed'),
    'Zmiany wśród tych samych osób'=pionowo(wynik_zmian)))
}
bilans_archiwum <- function() {
  wydruk_zi(tabele=list('Od rejestracji do ważnego wyniku'=archiwum_bilans,
    'Tabela wyników zadania'=tabela_z_etykietami(archiwum_tab)),digits=0L)
}
mianowniki_archiwum <- function() {
  wydruk_zi(tabele=list('N grup i sukcesy'=archiwum_wynik$opis,
    'Odsetki w grupach [%]'=tabela_z_etykietami(100*prop.table(archiwum_tab,1)),
    'Skład kategorii wyniku [%]'=tabela_z_etykietami(100*prop.table(archiwum_tab,2))),
    wykresy=proporcje_na_wykresie(archiwum_wynik)$wykresy,digits=2L)
}
testy_archiwum <- function() {
  out <- dwa_testy_tabeli(archiwum_wynik)
  out$tabele <- c(list('E przy niezależności'=tabela_z_etykietami(archiwum_wynik$E),
    'Wkłady do chi-kwadrat'=tabela_z_etykietami(archiwum_wynik$wklady)),out$tabele)
  out
}
efekt_archiwum <- function() {
  out <- efekty_tabeli(archiwum_wynik)
  out$tabele <- c(out$tabele,przedzialy_tabeli(archiwum_wynik,'pp')$tabele)
  out
}
