# Gotowy silnik raportu: od zapisanych danych do tabel i wykresów.
# Nie generuje nowego wariantu i nie zapisuje odpowiedzi za studenta.
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


przygotuj_zapisany_wariant <- function(pakiet) {
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

formatuj_wynik <- function(x) {
  for(nm in names(x)[grepl('^p($|_)',names(x))])
    if(is.numeric(x[[nm]])) x[[nm]] <- format.pval(x[[nm]],digits=3,eps=.0001)
  x
}
pionowo <- function(x) {
  y <- formatuj_wynik(x)
  data.frame(wielkosc=names(y),wartosc=vapply(y,function(z)
    if(is.numeric(z)) format(signif(z[1],5),trim=TRUE) else as.character(z[1]),
    character(1)),row.names=NULL)
}
analizy_wariantu <- function(projekt,B=1999L,ziarno=202627L) {
  stopifnot(inherits(projekt,'zi_projekt'))
  # Zachowaj stan sesji; powtórne wydrukowanie wyniku nie zmienia losowania danych.
  rng_istnial <- exists('.Random.seed',envir=globalenv(),inherits=FALSE)
  if(rng_istnial) rng <- get('.Random.seed',envir=globalenv(),inherits=FALSE)
  on.exit(if(rng_istnial) assign('.Random.seed',rng,envir=globalenv()) else
    if(exists('.Random.seed',envir=globalenv(),inherits=FALSE))
      rm('.Random.seed',envir=globalenv()),add=TRUE)
  cfg <- projekt$pakiet$scenariusz
  a <- porownaj_grupy(projekt$dane,grupa_1=projekt$odniesienie,
    grupa_2=projekt$porownywana,B=B,ziarno=ziarno)
  tabelaryczna <- cfg$druga_analiza=='tabela krzyżowa'
  if(tabelaryczna) {
    tab <- table(factor(projekt$dane$grupa,
      levels=c(projekt$odniesienie,projekt$porownywana)),
      factor(projekt$dane$powodzenie,levels=0:1))
    b <- analiza_tabeli(tab,B=B,ziarno=ziarno)
  } else {
    b <- analizuj_zwiazek(projekt$dane$indeks,
      projekt$dane[[cfg$zmienna_druga]],B=B,ziarno=ziarno)
  }
  structure(list(projekt=projekt,pierwsza=a,druga=b,
    rodzaj=if(tabelaryczna) 'tabela' else 'Spearman',B=B,ziarno=ziarno),
    class='zi_analizy')
}
wynik_pierwszej_analizy <- function(analizy) {
  a <- analizy$pierwsza
  opis <- grupy_do_druku(analizy$projekt,a$opis[c('grupa','N','srednia','SD')])
  wydruk_zi(karty=list('Analiza 1'=list('Kontrast'=kontrast_do_druku(analizy$projekt),
    'Jednostka'='Punkty indeksu 1–5, nie punkty procentowe.')),
    tabele=list('Opis grup'=opis,'Welch — oszacowanie i test'=pionowo(a$klasyczny),
      '95% CI różnicy średnich'=a$przedzialy,
      'Etykiety grup tasowane pod wymienialnością'=formatuj_wynik(a$losowanie),
      'Standaryzowany efekt opisowy'=a$efekt[c('roznica','Hedges_g')]))
}
wynik_drugiej_analizy <- function(analizy) {
  p <- analizy$projekt
  cfg <- p$pakiet$scenariusz
  b <- analizy$druga
  if(analizy$rodzaj=='tabela') {
    opis <- grupy_do_druku(p,b$opis)
    names(opis) <- c('Grupa','N','Kod 1','Odsetek [%]')
    liczby <- data.frame(Grupa=etykiety_grup(cfg,rownames(b$tab)),'Kod 0'=b$tab[,1],
      'Kod 1'=b$tab[,2],check.names=FALSE,row.names=NULL)
    ci <- b$przedzialy
    ci[c('estymata','dol','gora')] <- 100*ci[c('estymata','dol','gora')]
    test <- b$klasyczny
    if(any(b$E<5)) test <- cbind(test,b$fisher)
    return(wydruk_zi(karty=list('Analiza 2 — odpowiedź binarna'=list(
      'Pytanie'=cfg$pytanie_binarne,
      'Źródło i kod 1'=paste(pole_opisu(cfg,'rodzaj_binarnego','Odczytaj kartę'),
        '—',pole_opisu(cfg,'kod_1','Tak dla treści pytania; nie zawsze sukces.')),
      'Kontrast'=kontrast_do_druku(p),
      'Metoda klasyczna'=b$wybor,
      'Przedział różnicy'=if(any(b$E<5))
        'Rzadkie komórki: CI różnicy jest tylko przybliżeniem; odczytaj także dokładny CI ilorazu szans Fishera.' else
        'Dużopróbkowy CI różnicy proporcji; drugi CI z bootstrapu w grupach.')),
      tabele=list('Liczby odpowiedzi 0 i 1'=liczby,'Mianowniki i procenty w grupach'=opis,
        'Test i ocena liczebności oczekiwanych'=pionowo(test),
        'Tabele Monte Carlo przy stałych marginesach'=formatuj_wynik(b$losowanie),
        '95% CI różnicy [punkty procentowe]'=ci,
        'Efekt kierunkowy i V bez kierunku'=b$efekty[c('roznica_pp','V')])))
  }
  kl <- b$klasyczny[2,c('N','wspolczynnik','statystyka','symbol','p')]
  rownames(kl) <- NULL
  ci <- b$przedzialy[3,,drop=FALSE]
  mc <- b$losowanie[2,,drop=FALSE]
  rownames(ci) <- rownames(mc) <- NULL
  wydruk_zi(karty=list('Analiza 2 — korelacja rang'=list(
    'Zmienne'=paste('Indeks oraz',cfg$zmienna_druga),
    'Metoda'='Spearman: rho bez jednostki; S i przybliżone p, bez dopisywania df.',
    'Przedział'='95% CI bootstrapu kompletnych par; nie CI Pearsona.',
    'Tasowanie'='Jedna zmienna przestawiana względem drugiej pod niezależnością i wymienialnością par.')),
    tabele=list('Kompletność'=data.frame(N_osob=b$N_wejscie,N_par=b$N_par,
      N_niekompletne=b$N_brak),
      'Spearman — odczyt wyniku'=pionowo(kl),'95% CI współczynnika'=ci,
      'Permutacje jednej zmiennej'=formatuj_wynik(mc),
      'Ważne replikacje bootstrapu'=b$bootstrap_info[2,,drop=FALSE]))
}
wykresy_wariantu <- function(analizy) {
  p <- analizy$projekt
  pierwszy <- pierwszy_opis_wariantu(p)$wykresy[[1]]
  b <- analizy$druga
  if(analizy$rodzaj=='tabela') {
    drugi <- ggplot2::ggplot(b$opis,ggplot2::aes(grupa,procent,fill=grupa))+
      ggplot2::geom_col(width=.55)+ggplot2::scale_y_continuous(limits=c(0,100))+
      ggplot2::scale_fill_manual(values=c('#0072B2','#D55E00'))+
      ggplot2::scale_x_discrete(labels=function(x)
        vapply(etykiety_grup(p$pakiet$scenariusz,x),function(z)
          paste(strwrap(z,width=22),collapse='\n'),character(1)))+
      ggplot2::labs(x='Grupa',y='Odpowiedzi z kodem 1 [% w grupie]',
        title='Druga analiza: mianownik właściwy każdej grupie')+
      badaniaZI::theme_zi()+ggplot2::theme(legend.position='none')
  } else {
    etykieta <- if(p$pakiet$scenariusz$zmienna_druga=='czas_wyszukiwania')
      paste0(pole_opisu(p$pakiet$scenariusz,'nazwa_czasu','Czas zadania'),' [min]') else
      'Częstość korzystania [kategoria 1–5]'
    drugi <- ggplot2::ggplot(b$pary,ggplot2::aes(x,y))+
      ggplot2::geom_point(alpha=.5,colour='#0072B2')+
      ggplot2::labs(x='Indeks deklaracji [pkt 1–5]',y=etykieta,
        title=paste('Druga analiza:',b$N_par,'kompletnych par'))+badaniaZI::theme_zi()
  }
  wydruk_zi(wykresy=list(pierwszy,drugi))
}
slad_analizy <- function(analizy) {
  p <- analizy$projekt
  m <- p$pakiet$manifest
  wydruk_zi(karty=list('Odtworzenie tego wyniku'=list(
    'Wariant'=paste(m$id,m$scenariusz,m$rocznik,sep=' / '),
    'Generator'=m$wersja_generatora,
    'Hash surowych danych'=skrot_do_odczytu(m$hash_danych),
    'Reguły'='Identyczne duplikaty; 99 jako NA; czas poza 0–120 jako NA; pozycja 3 odwrócona; indeks minimum 5/6.',
    'Losowania'=paste('B =',analizy$B,'; zapisane ziarno =',analizy$ziarno),
    'Źródło obliczeń'='Zapisany Rmd i towarzyszący analiza.R; bez obiektów z poprzedniej sesji.',
    'Wersja wysłana'='Odrębne pokwitowanie zdalnego SHA po oddaniu; nie jest hashem danych.')))
}

parametry_raportu <- function(plik=badaniaZI::plik_pracy('PROJEKT')) {
  if(isTRUE(getOption('knitr.in.progress'))) {
    parametry <- get0('params',envir=knitr::knit_global(),inherits=FALSE)
  } else {
    if(!file.exists(plik)) stop('Otwórz raport.Rmd i uruchom jego pierwszy chunk.',call.=FALSE)
    tekst <- readLines(plik,encoding='UTF-8',warn=FALSE)
    granice <- which(tekst=='---')
    if(length(granice)<2L||granice[1]!=1L)
      stop('Nagłówek raportu jest niekompletny. Odtwórz kopię pliku, nie zmieniaj ID.',call.=FALSE)
    parametry <- yaml::yaml.load(paste(tekst[2:(granice[2]-1)],collapse='\n'),
      eval.expr=FALSE)$params
    id_sesji <- get0('ID',envir=globalenv(),inherits=FALSE)
    if(!is.null(id_sesji)&&!identical(id_sesji,parametry$id_studenta))
      stop('ID sesji różni się od ID raportu. Otwórz własną pracę.',call.=FALSE)
  }
  pola <- c('id_studenta','scenariusz','rocznik','wersja_generatora','hash_danych')
  if(!is.list(parametry)||!all(pola %in% names(parametry))||
     !all(vapply(parametry[pola],function(x) is.character(x)&&length(x)==1L&&
       !is.na(x)&&nzchar(x),logical(1))))
    stop('Brak zapisanych metadanych raportu. Odtwórz projekt ze swojej pracy Z09/Z10.',call.=FALSE)
  if(!grepl('^[A-Za-z0-9][A-Za-z0-9_-]{1,39}$',parametry$id_studenta)||
     !grepl('^S(0[1-9]|1[0-9]|20)$',parametry$scenariusz)||
     !grepl('^[a-f0-9]{64}$',parametry$hash_danych))
    stop('Metadane raportu nie identyfikują poprawnego wariantu danych.',call.=FALSE)
  parametry
}
wczytaj_projekt_raportu <- function(katalog_danych=badaniaZI::plik_pracy('PROJEKT','dane'),parametry=parametry_raportu()) {
  wymagane <- c('surowe.rds','surowe.csv','slownik.csv','manifest.json','scenariusz.yml')
  sciezki <- file.path(katalog_danych,wymagane)
  if(!all(file.exists(sciezki)))
    stop('Brak kompletu zapisanych danych raportu: ',
      paste(wymagane[!file.exists(sciezki)],collapse=', '),
      '. Odtwórz pliki swojego wariantu; nie generuj innego zbioru.',call.=FALSE)
  m <- jsonlite::read_json(file.path(katalog_danych,'manifest.json'),simplifyVector=TRUE)
  zgodne <- identical(m$id,parametry$id_studenta)&&
    identical(m$scenariusz,parametry$scenariusz)&&identical(m$rocznik,parametry$rocznik)&&
    identical(m$wersja_generatora,parametry$wersja_generatora)&&
    identical(m$hash_danych,parametry$hash_danych)&&isTRUE(m$syntetyczne)
  if(!zgodne) stop('Manifest i raport wskazują inne dane lub ID. Sprawdź własną pracę Z09/Z10.',call.=FALSE)
  for(ext in c('csv','rds')) {
    hash <- digest::digest(file=file.path(katalog_danych,paste0('surowe.',ext)),algo='sha256')
    if(!identical(hash,m[[paste0('sha256_',ext)]]))
      stop('Zapisane dane surowe zmieniły się: surowe.',ext,
        '. Przywróć potwierdzoną wersję; nie edytuj manifestu.',call.=FALSE)
  }
  metadane <- c(slownik='slownik.csv',scenariusz='scenariusz.yml')
  for(pole in names(metadane)) {
    hash <- digest::digest(file=file.path(katalog_danych,metadane[[pole]]),algo='sha256')
    if(!identical(hash,m[[paste0('sha256_',pole)]]))
      stop('Słownik lub karta scenariusza różni się od zapisanej wersji: ',
        metadane[[pole]],'. Odtwórz komplet plików projektu.',call.=FALSE)
  }
  dane <- readRDS(file.path(katalog_danych,'surowe.rds'))
  if(!is.data.frame(dane)) stop('Plik surowe.rds nie zawiera tabeli danych.',call.=FALSE)
  hash <- digest::digest(enc2utf8(as.character(jsonlite::toJSON(dane,
    dataframe='rows',na='null',digits=NA))),algo='sha256',serialize=FALSE)
  if(!identical(hash,m$hash_danych)||nrow(dane)!=m$n)
    stop('Zawartość lub liczba rekordów jest niezgodna z manifestem.',call.=FALSE)
  cfg <- yaml::read_yaml(file.path(katalog_danych,'scenariusz.yml'),eval.expr=FALSE)
  if(!is.list(cfg)||!identical(cfg$id,m$scenariusz)||length(cfg$pozycje)!=6L||
     length(cfg$grupy)!=2L||anyDuplicated(cfg$grupy)||
     !all(unique(dane$grupa) %in% unlist(cfg$grupy))||
     !cfg$druga_analiza %in% c('tabela krzyżowa','Spearman'))
    stop('Zapisana karta scenariusza nie odpowiada danym i planowi analiz.',call.=FALSE)
  slownik <- utils::read.csv(file.path(katalog_danych,'slownik.csv'),
    fileEncoding='UTF-8',stringsAsFactors=FALSE,check.names=FALSE)
  if(!all(c('zmienna','opis','skala','kodowanie','braki','odwrocona') %in% names(slownik))||
     !setequal(slownik$zmienna,names(dane))||anyDuplicated(slownik$zmienna))
    stop('Słownik nie opisuje kompletu kolumn zapisanych danych.',call.=FALSE)
  pakiet <- structure(list(dane=dane,slownik=slownik,scenariusz=cfg,manifest=m),
    class='badaniaZI_dane')
  przygotuj_zapisany_wariant(pakiet)
}
opis_raportu <- function(projekt) {
  czas <- do.call(rbind,lapply(c(projekt$odniesienie,projekt$porownywana),function(g) {
    y <- projekt$dane$czas_wyszukiwania[projekt$dane$grupa==g]
    y <- y[is.finite(y)]
    data.frame(grupa=g,N=length(y),srednia=mean(y),SD=sd(y),
      mediana=median(y),IQR=IQR(y))
  }))
  tabele <- list('Indeks w grupach [pkt 1–5]'=
    grupy_do_druku(projekt,projekt$opis[c('grupa','N_osob','N_indeks','srednia','SD')]),
    grupy_do_druku(projekt,czas))
  names(tabele)[2] <- paste0(pole_opisu(projekt$pakiet$scenariusz,
    'nazwa_czasu','Czas zadania'),' w grupach [min]')
  wydruk_zi(tabele=tabele)
}
kanaly_raportu <- function(projekt) {
  dane <- projekt$dane[paste0('kanal_',1:4)]
  wynik <- badaniaZI::odpowiedzi_wielokrotne(dane)
  wynik$opcja <- paste0('K',1:4)
  names(wynik) <- c('Kanał','Liczba','Mianownik','Procent [%]','Pominięte')
  karta <- as.list(unlist(projekt$pakiet$scenariusz$kanaly))
  names(karta) <- paste0('K',1:4)
  wydruk_zi(karty=list('Odpowiedzi wielokrotne — etykiety kanałów'=karta),
    tabele=list('Kanały: wspólny mianownik osób z kompletem czterech kodów'=wynik))
}
wykres_raportu <- function(analizy,numer=1L) {
  stopifnot(length(numer)==1L,numer %in% 1:2)
  wydruk_zi(wykresy=wykresy_wariantu(analizy)$wykresy[numer])
}
wersje_raportu <- function(analizy) {
  meta <- data.frame(element=c('R','badaniaZI','knitr','rmarkdown','Generator danych',
    'B losowań','Ziarno analiz'),
    wartosc=c(as.character(getRversion()),as.character(utils::packageVersion('badaniaZI')),
      as.character(utils::packageVersion('knitr')),as.character(utils::packageVersion('rmarkdown')),
      analizy$projekt$pakiet$manifest$wersja_generatora,
      as.character(analizy$B),as.character(analizy$ziarno)))
  pokaz_tabele(meta,'Wersje i parametry tego wykonania')
}
