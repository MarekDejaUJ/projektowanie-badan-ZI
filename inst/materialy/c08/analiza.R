dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_',1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)

analizuj_zwiazek <- function(x,y,B=1999L,ziarno=202627L) {
  stopifnot(is.numeric(x),is.numeric(y),length(x)==length(y),B>=99L,B==as.integer(B))
  ok <- is.finite(x)&is.finite(y)
  pary <- data.frame(x=x[ok],y=y[ok])
  n <- nrow(pary)
  stopifnot(n>=4L,sd(pary$x)>0,sd(pary$y)>0)
  pear <- cor.test(pary$x,pary$y,method='pearson')
  spear <- cor.test(pary$x,pary$y,method='spearman',exact=FALSE)
  obs <- c(Pearson=unname(pear$estimate),Spearman=unname(spear$estimate))
  set.seed(ziarno)
  perm <- replicate(B,{
    yy <- sample(pary$y)
    c(Pearson=cor(pary$x,yy),Spearman=cor(pary$x,yy,method='spearman'))
  })
  k <- rowSums(abs(perm)>=abs(obs)-1e-12)
  boot <- replicate(B,{
    i <- sample.int(n,replace=TRUE)
    if(sd(pary$x[i])==0||sd(pary$y[i])==0) return(c(Pearson=NA_real_,Spearman=NA_real_))
    c(Pearson=cor(pary$x[i],pary$y[i]),
      Spearman=cor(pary$x[i],pary$y[i],method='spearman'))
  })
  ci_boot <- t(apply(boot,1,quantile,probs=c(.025,.975),na.rm=TRUE))
  model <- lm(y~x,data=pary)
  sm <- summary(model)
  cf <- coef(sm)
  ci_b <- confint(model)
  list(pary=pary,N_wejscie=length(x),N_par=n,N_brak=length(x)-n,
    klasyczny=data.frame(metoda=c('Pearson','Spearman, przybliżenie'),
      N=n,wspolczynnik=obs,statystyka=c(unname(pear$statistic),unname(spear$statistic)),
      symbol=c('t','S'),df=c(n-2,NA),p=c(pear$p.value,spear$p.value),row.names=NULL),
    przedzialy=data.frame(metoda=c('Pearson: transformacja Fishera',
      'Pearson: bootstrap par','Spearman: bootstrap par'),
      dol=c(pear$conf.int[1],ci_boot[1,1],ci_boot[2,1]),
      gora=c(pear$conf.int[2],ci_boot[1,2],ci_boot[2,2])),
    losowanie=data.frame(metoda=names(obs),wspolczynnik=obs,skrajne=k,
      B=B,p_perm=(k+1)/(B+1),row.names=NULL),
    bootstrap_info=data.frame(metoda=rownames(boot),
      B_wazne=rowSums(is.finite(boot)),B_nieokreslone=rowSums(!is.finite(boot))),
    perm=perm,boot=boot,model=model,
    regresja=data.frame(parametr=c('Wyraz wolny','Nachylenie'),
      estymata=cf[,1],SE=cf[,2],t=cf[,3],df=df.residual(model),p=cf[,4],
      CI_dol=ci_b[,1],CI_gora=ci_b[,2],row.names=NULL),
    dopasowanie=data.frame(N=n,R2=sm$r.squared,SD_reszt=sm$sigma,
      SSE=sum(resid(model)^2),SST=sum((pary$y-mean(pary$y))^2)),
    diagnostyka=data.frame(x=pary$x,y=pary$y,przewidywany=fitted(model),
      reszta=resid(model),dzwignia=hatvalues(model),Cook=cooks.distance(model)))
}
wynik <- analizuj_zwiazek(dane$indeks,dane$czas_wyszukiwania)
pary <- wynik$pary
bilans <- data.frame(zakres=c('Osoby po czyszczeniu','Ważny indeks',
  'Ważny czas','Kompletne pary'),N=c(nrow(dane),sum(!is.na(dane$indeks)),
  sum(!is.na(dane$czas_wyszukiwania)),wynik$N_par))
formatuj_wynik <- function(x) {
  for(nm in names(x)[grepl('^p($|_)',names(x))])
    if(is.numeric(x[[nm]])) x[[nm]] <- format.pval(x[[nm]],digits=3,eps=.0001)
  x
}
pionowo <- function(x) data.frame(wielkosc=names(x),
  wartosc=vapply(x,function(z) if(is.numeric(z)) format(signif(z[1],5),trim=TRUE)
    else as.character(z[1]),character(1)),row.names=NULL)
wykres_par <- ggplot2::ggplot(pary,ggplot2::aes(x,y))+
  ggplot2::geom_point(alpha=.55,colour='#0072B2')+
  ggplot2::labs(x='Deklarowana użyteczność katalogu [1–5]',y='Czas [min]',
    title='Jedna kropka to jedna kompletna para')+badaniaZI::theme_zi()
wykres_regresji <- wykres_par+
  ggplot2::geom_smooth(method='lm',se=TRUE,colour='#D55E00')+
  ggplot2::labs(title='Prosta i 95% CI średniego czasu')
wykres_perm <- ggplot2::ggplot(data.frame(r=wynik$perm['Spearman',]),ggplot2::aes(r))+
  ggplot2::geom_histogram(bins=30,fill='#56B4E9',colour='white')+
  ggplot2::geom_vline(xintercept=c(-1,1)*abs(wynik$klasyczny$wspolczynnik[2]),
    colour='#D55E00',linewidth=.8)+
  ggplot2::labs(x='Korelacja Spearmana po tasowaniu y',y='Liczba losowań',
    title='Rozkład zerowy: dwa kierunki skrajności')+badaniaZI::theme_zi()
wykres_reszt <- ggplot2::ggplot(wynik$diagnostyka,ggplot2::aes(przewidywany,reszta))+
  ggplot2::geom_point(alpha=.6,colour='#0072B2')+
  ggplot2::geom_hline(yintercept=0,colour='#D55E00')+
  ggplot2::labs(x='Przewidywany czas [min]',y='Reszta [min]',
    title='Czy błąd modelu ma widoczny wzorzec?')+badaniaZI::theme_zi()
wykres_qq <- ggplot2::ggplot(wynik$diagnostyka,ggplot2::aes(sample=reszta))+
  ggplot2::stat_qq(colour='#0072B2')+ggplot2::stat_qq_line(colour='#D55E00')+
  ggplot2::labs(x='Kwantyl normalny',y='Kwantyl reszt [min]',
    title='Q–Q reszt: ocena przybliżenia, nie certyfikat')+badaniaZI::theme_zi()
siatka <- data.frame(x=seq(min(pary$x),max(pary$x),length.out=80))
cm <- predict(wynik$model,newdata=siatka,interval='confidence')
pr <- predict(wynik$model,newdata=siatka,interval='prediction')
pasma <- data.frame(x=siatka$x,y=cm[,1],CI_dol=cm[,2],CI_gora=cm[,3],
  PI_dol=pr[,2],PI_gora=pr[,3])
wykres_predykcji <- ggplot2::ggplot(pasma,ggplot2::aes(x,y))+
  ggplot2::geom_ribbon(ggplot2::aes(ymin=PI_dol,ymax=PI_gora),fill='#56B4E9',alpha=.2)+
  ggplot2::geom_ribbon(ggplot2::aes(ymin=CI_dol,ymax=CI_gora),fill='#0072B2',alpha=.5)+
  ggplot2::geom_line(colour='#D55E00')+
  ggplot2::labs(x='Indeks [1–5]',y='Przewidywany czas [min]',
    title='Wąskie pasmo: średnia; szerokie: nowa osoba')+badaniaZI::theme_zi()
prognoza <- do.call(rbind,lapply(c('confidence','prediction'),function(typ) {
  z <- predict(wynik$model,newdata=data.frame(x=c(2,3,4)),interval=typ)
  data.frame(typ=typ,indeks=c(2,3,4),czas=z[,1],dol=z[,2],gora=z[,3],row.names=NULL)
}))
mini <- data.frame(osoba=letters[1:6],x=c(1,2,2,3,4,5),y=c(15,12,14,10,9,6))
mini$dx <- mini$x-mean(mini$x)
mini$dy <- mini$y-mean(mini$y)
mini$iloczyn <- mini$dx*mini$dy
mini$ranga_x <- rank(mini$x)
mini$ranga_y <- rank(mini$y)
mini_r <- cor(mini$x,mini$y)
mini_rho <- cor(mini$x,mini$y,method='spearman')
mini_model <- lm(y~x,data=mini)
mini_podsumowanie <- data.frame(r=mini_r,rho=mini_rho,
  b0=coef(mini_model)[1],b1=coef(mini_model)[2],
  R2=summary(mini_model)$r.squared,row.names=NULL)
mini_losowanie <- data.frame(osoba=mini$osoba,x=mini$x,y=mini$y,
  y_permutacja=mini$y[c(3,6,1,5,2,4)])
mini_boot <- mini[c(2,2,6,1,4,4),c('osoba','x','y')]
rownames(mini_boot) <- NULL
ksztalty <- rbind(
  data.frame(przyklad='Liniowy',x=1:9,y=c(17,16,14,13,11,10,8,7,5)),
  data.frame(przyklad='Monotoniczny, zakrzywiony',x=1:9,y=30/(1:9)),
  data.frame(przyklad='Kształt U',x=1:9,y=(1:9-5)^2+2))
ksztalty_wynik <- do.call(rbind,lapply(split(ksztalty,ksztalty$przyklad),function(z)
  data.frame(przyklad=z$przyklad[1],r=cor(z$x,z$y),
    rho=cor(z$x,z$y,method='spearman'))))
wykres_ksztaltow <- ggplot2::ggplot(ksztalty,ggplot2::aes(x,y))+
  ggplot2::geom_point(colour='#0072B2')+ggplot2::geom_line(colour='#56B4E9')+
  ggplot2::facet_wrap(~przyklad,nrow=1,scales='free_y')+
  ggplot2::labs(x='Wskaźnik x [jednostki umowne]',y='Czas [min]',
    title='Trzy hipotetyczne wzorce, różne pytania')+badaniaZI::theme_zi()
wplyw <- data.frame(x=c(1:8,9),y=c(2,3,3,5,4,6,6,8,30))
wplyw_wynik <- data.frame(wariant=c('Wszystkie 9','Pierwsze 8: analiza wrażliwości'),
  r=c(cor(wplyw$x,wplyw$y),cor(wplyw$x[1:8],wplyw$y[1:8])),
  b1=c(coef(lm(y~x,wplyw))[2],coef(lm(y~x,wplyw[1:8,]))[2]),row.names=NULL)
wykres_wplywu <- ggplot2::ggplot(wplyw,ggplot2::aes(x,y))+
  ggplot2::geom_point(colour='#0072B2')+
  ggplot2::geom_smooth(method='lm',se=FALSE,colour='#D55E00')+
  ggplot2::geom_smooth(data=wplyw[1:8,],method='lm',se=FALSE,colour='#009E73')+
  ggplot2::labs(x='x [jednostki umowne]',y='Czas [min]',
    title='Pomarańczowa: 9 osób; zielona: 8 osób')+badaniaZI::theme_zi()
kalibracja <- data.frame(osoba=letters[1:5],wynik_obserwowany=c(20,30,40,50,60),
  samoocena=c(40,50,60,70,80))
kalibracja$blad <- kalibracja$samoocena-kalibracja$wynik_obserwowany
kalibracja_wynik <- data.frame(r=cor(kalibracja$wynik_obserwowany,kalibracja$samoocena),
  srednie_zawyzenie_pp=mean(kalibracja$blad))
warunki <- data.frame(grupa=rep(c('Łatwe zadanie','Trudne zadanie'),each=5),
  x=c(1,2,3,4,5,4,5,6,7,8),y=c(10,9,8,7,6,20,19,18,17,16))
warunki_wynik <- data.frame(zakres=c('Razem','Łatwe','Trudne'),
  r=c(cor(warunki$x,warunki$y),cor(warunki$x[1:5],warunki$y[1:5]),
    cor(warunki$x[6:10],warunki$y[6:10])))
wykres_grup <- ggplot2::ggplot(warunki,ggplot2::aes(x,y,colour=grupa))+
  ggplot2::geom_point(size=2)+ggplot2::geom_smooth(method='lm',se=FALSE)+
  ggplot2::scale_colour_manual(values=c('#0072B2','#D55E00'))+
  ggplot2::labs(x='Doświadczenie [liczba wcześniejszych zadań]',y='Czas [min]',
  colour=NULL,title='Wewnątrz grup i po ich połączeniu')+badaniaZI::theme_zi()

# CHALLENGE: samoocena sprawności weryfikacji źródeł i czas zadania.
# Celowo odrębny syntetyczny przykład, nie podzbiór S02.
portal_dane <- data.frame(osoba=sprintf('w%02d',1:30),
  samoocena=c(rep(seq(1.5,4.5,.5),each=4),NA,3.5),
  czas=c(15,12,19,10,14,18,8,12,11,16,13,9,14,7,12,10,
    10,13,6,11,12,8,9,5,7,11,9,6,12,NA))
portal_wynik <- analizuj_zwiazek(portal_dane$samoocena,portal_dane$czas,ziarno=202628L)
portal_bilans <- data.frame(zakres=c('Zarejestrowane osoby','Ważna samoocena',
  'Ważny czas','Kompletne pary','Brak przynajmniej jednej wartości'),
  N=c(nrow(portal_dane),sum(is.finite(portal_dane$samoocena)),
    sum(is.finite(portal_dane$czas)),portal_wynik$N_par,portal_wynik$N_brak))

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


korelacja_do_odczytu <- function(analiza,miara=c('obie','Pearson','Spearman')) {
  miara <- match.arg(miara)
  wiersze <- if(miara=='obie') 1:2 else match(miara,c('Pearson','Spearman'))
  # df Spearmana jest niepodane, nie dopisujemy liczby z testu Pearsona.
  tab <- formatuj_wynik(analiza$klasyczny[wiersze,,drop=FALSE])
  tab$metoda <- sub(', przybliżenie','',tab$metoda,fixed=TRUE)
  tab$df <- ifelse(is.na(tab$df),'—',as.character(tab$df))
  names(tab) <- c('Miara','N','Współczynnik','Statystyka','Symbol','df','p')
  tytul <- if(miara=='Pearson') 'Pearson: klasyczny test zerowej korelacji' else
    if(miara=='Spearman') 'Spearman: przybliżony test korelacji rangowej' else
      'Klasyczny odczyt; p Spearmana przybliżone'
  pokaz_tabele(tab,tytul,4L)
}
przedzialy_korelacji <- function(analiza,miara=c('obie','Pearson','Spearman')) {
  miara <- match.arg(miara)
  wiersze <- if(miara=='obie') 1:3 else if(miara=='Pearson') 1:2 else 3
  pokaz_tabele(analiza$przedzialy[wiersze,,drop=FALSE],'95% CI współczynnika',4L)
}
losowanie_korelacji <- function(analiza) {
  pokaz_tabele(formatuj_wynik(analiza$losowanie),'Tasowanie y: dwustronne p Monte Carlo',4L)
}
regresja_do_odczytu <- function(analiza) {
  tab <- formatuj_wynik(analiza$regresja)
  # Dwa wąskie widoki tej samej tabeli mieszczą etykiety i jednostki na A4.
  wydruk_zi(tabele=list('Współczynniki modelu: y w minutach, x w punktach'=
    tab[c('parametr','estymata','SE','CI_dol','CI_gora')],
    'Testy parametrów równych zero'=tab[c('parametr','t','df','p')]),digits=4L)
}
dopasowanie_do_odczytu <- function(analiza) {
  pokaz_tabele(analiza$dopasowanie,'Dopasowanie: R2 bez jednostki; SD reszt w minutach',4L)
}
rozrzut_do_odczytu <- function(analiza,opis_x,prosta=FALSE) {
  wykres <- ggplot2::ggplot(analiza$pary,ggplot2::aes(x,y))+
    ggplot2::geom_point(alpha=.65,colour='#0072B2')+
    ggplot2::labs(x=opis_x,y='Czas [min]',
      title=paste('Jedna osoba — jedna para; N =',analiza$N_par))+badaniaZI::theme_zi()
  if(prosta) wykres <- wykres+
    ggplot2::geom_smooth(method='lm',se=TRUE,colour='#D55E00',formula=y~x)+
    ggplot2::labs(title='Prosta oraz 95% CI średniego czasu')
  pokaz_wykres(wykres)
}
diagnostyka_do_odczytu <- function(analiza) {
  wykres <- ggplot2::ggplot(analiza$diagnostyka,ggplot2::aes(przewidywany,reszta))+
    ggplot2::geom_point(alpha=.65,colour='#0072B2')+
    ggplot2::geom_hline(yintercept=0,colour='#D55E00')+
    ggplot2::labs(x='Przewidywany czas [min]',y='Reszta [min]',
      title='Reszty: kształt, rozrzut i odległe punkty')+badaniaZI::theme_zi()
  pokaz_wykres(wykres)
}
przewidywanie_czasu <- function(analiza,x=3) {
  stopifnot(length(x)==1,is.finite(x))
  typy <- c('confidence','prediction')
  wynik <- do.call(rbind,lapply(typy,function(typ) {
    tab <- predict(analiza$model,newdata=data.frame(x=x),interval=typ)
    data.frame(przedmiot=if(typ=='confidence') 'Średnia warunkowa' else 'Nowa osoba',
      x=x,czas=tab[1,1],dol=tab[1,2],gora=tab[1,3],row.names=NULL)
  }))
  pokaz_tabele(wynik,'Dwa 95% przedziały: średnia i nowa osoba [min]',3L)
}
rachunek_korelacji <- function() {
  wydruk_zi(tabele=list('Odchylenia i iloczyny'=mini[c('osoba','x','y','dx','dy','iloczyn')],
    'Pearson i Spearman'=mini_podsumowanie[c('r','rho')]),digits=4L)
}
ksztalty_do_odczytu <- function() {
  wydruk_zi(tabele=list('Współczynniki opisują określony kształt'=ksztalty_wynik),
    wykresy=list(wykres_ksztaltow))
}
wplyw_do_odczytu <- function() {
  wydruk_zi(tabele=list('Porównanie wrażliwości'=wplyw_wynik),
    wykresy=list(wykres_wplywu))
}
kalibracja_do_odczytu <- function() {
  wydruk_zi(tabele=list('Dwie wartości na porównywalnej skali'=kalibracja,
    'Związek a systematyczny błąd samooceny'=kalibracja_wynik))
}
grupy_do_odczytu <- function() {
  wydruk_zi(tabele=list('Korelacja łączna i wewnątrz grup'=warunki_wynik),
    wykresy=list(wykres_grup))
}
pary_portalu <- function() {
  out <- rozrzut_do_odczytu(portal_wynik,'Samoocena sprawności weryfikacji [pkt 1–5]')
  out$tabele <- list('Bilans osób i par'=portal_bilans)
  out
}
wynik_korelacji_portalu <- function(miara=c('Pearson','Spearman')) {
  miara <- match.arg(miara)
  out <- korelacja_do_odczytu(portal_wynik,miara)
  out$tabele <- c(out$tabele,przedzialy_korelacji(portal_wynik,miara)$tabele)
  out
}
losowania_portalu <- function() {
  out <- losowanie_korelacji(portal_wynik)
  out$tabele <- c(out$tabele,list('Kontrola replik bootstrapu'=portal_wynik$bootstrap_info))
  out
}
model_portalu <- function() {
  out <- regresja_do_odczytu(portal_wynik)
  out$tabele <- c(out$tabele,dopasowanie_do_odczytu(portal_wynik)$tabele)
  out$wykresy <- diagnostyka_do_odczytu(portal_wynik)$wykresy
  out
}
