# Gotowy silnik raportu: od zapisanych danych wariantu do tabel i wykresów.
# Odczytuje dane zapisane na C09 i C10 bez nowego losowania; akapity pisze autor raportu.

# ---- Część wspólna z raportem projektu: początek ----
# ---- Prezentacja: tabele, karty i wykresy ----
ustaw_material <- function() {
  knitr::opts_chunk$set(echo=TRUE,message=FALSE,warning=FALSE,
    results='asis',fig.width=6,fig.height=3.3,fig.align='center',fig.pos='H')
  # Rysunek [H] zostaje przy swoim zadaniu także w PDF bez preambuły kursu.
  if(isTRUE(getOption('knitr.in.progress'))&&knitr::is_latex_output())
    knitr::knit_meta_add(list(rmarkdown::latex_dependency('float')))
  invisible(NULL)
}
wydruk_zi <- function(tabele=list(),wykresy=list(),tekst=NULL,digits=3L,karty=list(),markdown=list()) {
  stopifnot(is.list(tabele),is.list(wykresy),is.list(karty),is.list(markdown),
    is.numeric(digits),length(digits)==1L)
  structure(list(tabele=tabele,wykresy=wykresy,tekst=tekst,digits=digits,karty=karty,
    markdown=markdown),class='zi_wydruk')
}
# Karta to tabela Markdown z dwiema kolumnami; długie treści zawijają się w PDF i HTML.
karta_markdown <- function(karta) {
  znak <- function(s) gsub('|','\\|',s,fixed=TRUE)
  tresc <- vapply(karta,function(z) paste(as.character(z),collapse=' '),character(1))
  c('| Element | Treść |','|:-------------|:---------------------------------------------------|',
    paste0('| ',znak(names(karta)),' | ',znak(tresc),' |'))
}
print.zi_wydruk <- function(x,...) {
  dokument <- isTRUE(getOption('knitr.in.progress'))
  for(nazwa in names(x$karty)) {
    karta <- x$karty[[nazwa]]
    if(dokument) cat('\n\n**',nazwa,'**\n\n',paste(karta_markdown(karta),collapse='\n'),'\n\n',sep='')
    else {
      cat('\n',nazwa,'\n',sep='')
      for(pole in names(karta)) cat(pole,': ',karta[[pole]],'\n',sep='')
    }
  }
  for(nazwa in names(x$tabele)) {
    tab <- x$tabele[[nazwa]]
    if(dokument) {
      format <- if(knitr::is_latex_output()) 'latex' else 'html'
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
  for(md in x$markdown) cat(paste(as.character(md),collapse='\n'),'\n\n',sep='')
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
liczba <- function(x,cyfry=3L) {
  s <- formatC(x,format='f',digits=cyfry,decimal.mark=',')
  # Wartość zaokrąglona do zera traci znak minus; pozostałe ujemne dostają znak −.
  sub('^-','−',sub('^-(0,0*)$','\\1',s))
}
formatuj_p <- function(p) ifelse(p<0.0001,'< 0,0001',formatC(p,format='f',digits=4,decimal.mark=','))

# ---- Wariant projektu: przygotowanie, karty i opis ----
przygotuj_zapisany_wariant <- function(pakiet) {
  porzadek <- badaniaZI::przygotuj_ankiete(pakiet$dane)
  dane <- porzadek$dane
  pozycje <- dane[paste0('pozycja_',1:6)]
  pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
  dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)
  cfg <- pakiet$scenariusz
  # W całym projekcie kontrast to pierwsza grupa z karty minus druga, jawnie podpisany.
  grupy <- rev(unlist(cfg$grupy))
  opis <- do.call(rbind,lapply(grupy,function(g) {
    z <- dane[dane$grupa==g,,drop=FALSE]
    czas <- z$czas_wyszukiwania[is.finite(z$czas_wyszukiwania)]
    data.frame(grupa=g,N_osob=nrow(z),N_indeks=sum(is.finite(z$indeks)),
      srednia=mean(z$indeks,na.rm=TRUE),SD=sd(z$indeks,na.rm=TRUE),
      mediana=median(z$indeks,na.rm=TRUE),N_czas=length(czas),
      maks_czas=if(length(czas)) max(czas) else NA_real_,
      N_wynik=sum(is.finite(z$powodzenie)),row.names=NULL)
  }))
  tabela <- cfg$druga_analiza=='tabela krzyżowa'
  drugie_n <- if(tabela) sum(complete.cases(dane[c('grupa','powodzenie')])) else
    sum(complete.cases(dane[c('indeks',cfg$zmienna_druga)]))
  bilans <- data.frame(Zakres=c('Rekordy surowe','Osoby po usunięciu duplikatów',
    'Analiza 1: osoby z ważnym indeksem','Osoby z ważnym czasem',
    if(tabela) 'Analiza 2: osoby z wartością 0/1' else
      paste('Analiza 2: kompletne pary — indeks i',nazwa_drugiej(cfg,FALSE))),
    N=c(nrow(pakiet$dane),nrow(dane),sum(is.finite(dane$indeks)),
      sum(is.finite(dane$czas_wyszukiwania)),drugie_n))
  structure(list(pakiet=pakiet,dane=dane,pozycje=pozycje,dziennik=porzadek$dziennik,
    opis=opis,bilans=bilans,odniesienie=grupy[1],porownywana=grupy[2]),
    class='zi_projekt')
}
skrot_do_odczytu <- function(x) {
  # Spacje tylko w wydruku, oryginalny manifest zachowuje pełny ciąg znaków.
  paste(substring(x,seq(1,nchar(x),8),pmin(seq(1,nchar(x),8)+7,nchar(x))),collapse=' ')
}
pole_opisu <- function(cfg,nazwa,domyslnie) {
  x <- cfg[[nazwa]]
  if(is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x)) x else domyslnie
}
nazwa_indeksu <- function(cfg) {
  pole_opisu(cfg,'nazwa_indeksu',paste('Indeks',cfg$konstrukt))
}
nazwa_drugiej <- function(cfg,jednostka=TRUE) {
  if(cfg$zmienna_druga=='czas_wyszukiwania')
    paste0(tolower(pole_opisu(cfg,'nazwa_czasu','Czas zadania')),if(jednostka) ' [min]' else '') else
    paste0('częstość korzystania',if(jednostka) ' [kategorie 1–5]' else '')
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
print.zi_projekt <- function(x,...) {
  cfg <- x$pakiet$scenariusz
  m <- x$pakiet$manifest
  karta <- list('Scenariusz'=paste(cfg$id,'—',cfg$tytul),
    'Problem usługi'=cfg$problem,
    'Projekt badania'=pole_opisu(cfg,'projekt_badania','Badanie obserwacyjne dwóch grup.'),
    'Konstrukt indeksu'=nazwa_indeksu(cfg),
    'Pierwsze porównanie'=kontrast_do_druku(x),
    'Druga relacja'=cfg$pytanie_drugie,
    'Klucz wariantu (grupy po 8 znaków)'=skrot_do_odczytu(m$klucz_wariantu),
    'Hash SHA-256 danych (grupy po 8 znaków)'=skrot_do_odczytu(m$hash_danych))
  meta <- data.frame(Pole=c('ID','Scenariusz','Rocznik','Wersja generatora',
    'Rekordy surowe','Wersja R','Dane syntetyczne'),
    Wartosc=c(m$id,m$scenariusz,m$rocznik,m$wersja_generatora,m$n,
      m$wersja_R,if(isTRUE(m$syntetyczne)) 'tak' else 'nie'))
  names(meta)[2] <- 'Wartość'
  print(wydruk_zi(karty=list('Karta wybranego wariantu'=karta),
    tabele=list('Manifest wariantu: pochodzenie danych'=meta)))
  invisible(x)
}
pomiar_wariantu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  pozycje <- as.list(unlist(cfg$pozycje))
  names(pozycje) <- paste('Pozycja',1:6)
  karta <- c(pozycje,list(
    'Kontekst pozycji'=pole_opisu(cfg,'kontekst_pozycji','Wspólny punkt odniesienia dla obu grup.'),
    'Odpowiedzi na pozycje'='1 zdecydowanie nie, 2 raczej nie, 3 ani tak, ani nie, 4 raczej tak, 5 zdecydowanie tak.',
    'Kierunek i kompletność'='Pozycja 3 po odwróceniu 6 − x; indeks jest średnią przy co najmniej 5 z 6 ważnych pozycji.',
    'Definicja grup'=pole_opisu(cfg,'opis_grup',paste(unlist(cfg$grupy),collapse=' / ')),
    'Zadanie obserwacyjne'=pole_opisu(cfg,'zadanie_obserwacyjne','Zadanie opisane w karcie scenariusza.'),
    'Czas'=pole_opisu(cfg,'nazwa_czasu','Czas zadania'),
    'Protokół czasu'=pole_opisu(cfg,'protokol_czasu','Minuty od udostępnienia zadania do zgłoszenia odpowiedzi.'),
    'Częstość korzystania'=pole_opisu(cfg,'opis_czestosci',
      '1 nigdy, 2 rzadko, 3 czasami, 4 często, 5 bardzo często; kategorie porządkowe.'),
    'Treść pola 0/1'=cfg$pytanie_binarne,
    'Źródło pola 0/1'=pole_opisu(cfg,'rodzaj_binarnego','zapisane w karcie scenariusza'),
    'Kod 1'=pole_opisu(cfg,'kod_1','odpowiedź „tak” na pytanie pola 0/1'),
    'Kod 0'=pole_opisu(cfg,'kod_0','odpowiedź „nie” na pytanie pola 0/1'),
    'Granica odczytu pola 0/1'=pole_opisu(cfg,'ograniczenie_binarnego',
      'Wynik dotyczy jednego zadania w określonych warunkach.'),
    'Kanały (odpowiedź wielokrotna)'=paste(unlist(cfg$kanaly),collapse=', ')))
  wydruk_zi(karty=list('Kwestionariusz i pomiary w wybranym scenariuszu'=karta))
}
plan_wariantu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  a <- etykiety_grup(cfg,projekt$porownywana)
  b <- etykiety_grup(cfg,projekt$odniesienie)
  pierwsza <- list('Pomiar'=paste0(nazwa_indeksu(cfg),': średnia sześciu pozycji, skala 1–5'),
    'Grupy i kolejność'=paste(a,'minus',b),
    'Estymanda'=paste0('różnica średnich indeksu w populacji: średnia (',a,') minus średnia (',b,')'),
    'Jednostka efektu'='punkty indeksu 1–5',
    'Hipoteza zerowa'='H0: średnie w populacji są równe, różnica wynosi 0; test dwustronny',
    'Tor klasyczny'='test t Welcha z 95% CI różnicy średnich',
    'Tor losowany'='tasowanie etykiet grup (B = 1999) przy wymienialności; bootstrap osób w grupach daje drugi 95% CI',
    'Liczebność'='osoby ze znaną grupą i ważnym indeksem; osobne n każdej grupy')
  druga <- if(cfg$druga_analiza=='tabela krzyżowa')
    list('Pomiar'=paste0('pole 0/1 — ',cfg$pytanie_binarne,' Kod 1: ',
        pole_opisu(cfg,'kod_1','odpowiedź „tak”'),'.'),
      'Grupy i kolejność'=paste(a,'minus',b),
      'Estymanda'=paste0('różnica odsetków kodu 1 w populacji: odsetek (',a,') minus odsetek (',b,')'),
      'Jednostka efektu'='punkty procentowe (pp)',
      'Hipoteza zerowa'='H0: grupa i pole 0/1 są niezależne, różnica odsetków wynosi 0',
      'Tor klasyczny'='test chi-kwadrat (przy liczebności oczekiwanej poniżej 5: test Fishera) i 95% CI różnicy odsetków',
      'Tor losowany'='tabele Monte Carlo przy stałych marginesach (B = 1999); bootstrap osób w grupach daje drugi 95% CI',
      'Miara uzupełniająca'='V Craméra: natężenie związku bez kierunku',
      'Liczebność'='osoby ze znaną grupą i wartością 0/1; odsetki liczone w każdej grupie osobno') else
    list('Pomiar'=paste0('indeks oraz ',nazwa_drugiej(cfg)),
      'Estymanda'=paste0('korelacja rang Spearmana indeksu i zmiennej „',nazwa_drugiej(cfg,FALSE),'” w populacji'),
      'Jednostka efektu'='współczynnik bez jednostki, od −1 do 1',
      'Hipoteza zerowa'=paste0('H0: indeks i ',nazwa_drugiej(cfg,FALSE),' są niezależne, korelacja rang wynosi 0'),
      'Tor klasyczny'='test Spearmana: statystyka S i przybliżone p',
      'Tor losowany'='tasowanie jednej zmiennej względem drugiej (B = 1999); bootstrap całych par daje 95% CI',
      'Liczebność'='kompletne pary: osoby z obiema wartościami')
  wydruk_zi(karty=list('Analiza 1 — porównanie grup'=pierwsza,
    'Analiza 2 — relacja ze scenariusza'=druga))
}
przygotowanie_wariantu <- function(projekt) {
  dziennik <- projekt$dziennik
  dziennik$regula <- gsub('--','–',dziennik$regula,fixed=TRUE)
  names(dziennik) <- c('Reguła','Liczba')
  reguly <- list('Identyczne duplikaty'='z kilku identycznych rekordów zostaje jeden; liczba osób maleje',
    'Kod 99'='w pozycjach ankiety oznacza brak odpowiedzi i zmienia komórkę na NA; osoba zostaje w danych',
    'Czas poza 0–120 min'='zmienia komórkę czasu na NA; osoba zostaje w analizach bez czasu',
    'Długi czas w zakresie 0–120 min'='zostaje jako ważna obserwacja',
    'Pozycja 3'='sformułowana odwrotnie; odwrócenie 6 − x nadaje jej kierunek pozostałych pozycji',
    'Indeks'='średnia sześciu pozycji przy co najmniej 5 ważnych odpowiedziach; przy 4 lub mniej indeks jest brakiem')
  wydruk_zi(karty=list('Reguły gotowego skryptu'=reguly),
    tabele=list('Dziennik przygotowania: wykonane zmiany'=dziennik),digits=0L)
}
bilans_wariantu <- function(projekt) {
  tab <- projekt$opis[c('grupa','N_osob','N_indeks','N_czas','maks_czas','N_wynik')]
  tab$grupa <- etykiety_grup(projekt$pakiet$scenariusz,tab$grupa)
  names(tab) <- c('Grupa','Osoby','Ważny indeks','Ważny czas','Najdłuższy ważny czas [min]','Wartość 0/1')
  wydruk_zi(tabele=list('Bilans liczebności'=projekt$bilans,
    'Dostępne pomiary w grupach'=tab),digits=1L)
}
wykres_indeksu <- function(projekt) {
  cfg <- projekt$pakiet$scenariusz
  d <- projekt$dane[projekt$dane$grupa %in% c(projekt$odniesienie,projekt$porownywana),]
  d <- d[order(match(d$grupa,c(projekt$odniesienie,projekt$porownywana))),]
  badaniaZI::wykres_pudelkowy(d$indeks,etykiety_grup(cfg,d$grupa),
    os='Indeks [pkt 1–5]',granica=FALSE,
    tytul=paste(strwrap(paste('Indeks w grupach:',nazwa_indeksu(cfg)),width=52),collapse='\n'))
}
pierwszy_opis_wariantu <- function(projekt,z_wykresem=TRUE) {
  cfg <- projekt$pakiet$scenariusz
  opis <- projekt$opis[c('grupa','N_osob','N_indeks','srednia','SD','mediana')]
  opis$grupa <- etykiety_grup(cfg,opis$grupa)
  names(opis) <- c('Grupa','Osoby','n (ważny indeks)','Średnia','SD','Mediana')
  roznica <- data.frame(Kontrast=kontrast_do_druku(projekt),
    'Różnica średnich [pkt]'=projekt$opis$srednia[2]-projekt$opis$srednia[1],check.names=FALSE)
  wydruk_zi(tabele=list('Opis indeksu w grupach [pkt 1–5]'=opis,
      'Różnica średnich w kolejności kontrastu'=roznica),
    karty=list('Źródło do uzasadnienia planu'=list('Lektura scenariusza'=pole_opisu(cfg,'inspiracja',
      'Wybierz źródło z bibliografii kursu.'))),
    wykresy=if(z_wykresem) list(wykres_indeksu(projekt)) else list())
}

# ---- Silniki analiz: porównanie grup, tabela 2 × 2, korelacja rang ----
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
  # Kolumny 0 i 1; kontrast proporcji kodu 1: wiersz 2 minus wiersz 1.
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
  # Przedział dużopróbkowy; przy rzadkich komórkach wynik czyta się razem z testem Fishera.
  test_prop <- suppressWarnings(prop.test(tab[2:1,2],rowSums(tab)[2:1],correct=FALSE))
  boot <- replicate(B,{
    b1 <- sample(y[[1]],replace=TRUE)
    b2 <- sample(y[[2]],replace=TRUE)
    c(roznica=mean(b2)-mean(b1))
  })
  ci_delta <- unname(quantile(boot,c(.025,.975)))
  list(tab=tab,E=E,
    opis=data.frame(grupa=rownames(tab),N=as.integer(rowSums(tab)),
      sukcesy=as.integer(tab[,2]),procent=100*prop,row.names=NULL),
    klasyczny=data.frame(N=N,chi2=chi,df=1,p_chi2=pchi,min_E=min(E),
      komorki_E_mniejsze_5=sum(E<5)),
    fisher=data.frame(OR_warunkowe=unname(fisher$estimate),
      CI_OR_dol=fisher$conf.int[1],CI_OR_gora=fisher$conf.int[2],
      p_Fisher=fisher$p.value),
    rzadkie=any(E<5),
    losowanie=data.frame(chi2=chi,skrajne=k,B=B,p_MC=(1+k)/(1+B)),
    zerowy=zerowy,
    efekty=data.frame(roznica_pp=100*unname(delta),V=sqrt(chi/N)),
    przedzialy=data.frame(metoda=c('Przybliżenie dużopróbkowe','Bootstrap w grupach'),
      estymata=unname(delta),dol=c(test_prop$conf.int[1],ci_delta[1]),
      gora=c(test_prop$conf.int[2],ci_delta[2])),
    bootstrap_roznicy=boot)
}
analizuj_spearmana <- function(x,y,B=1999L,ziarno=202627L) {
  stopifnot(is.numeric(x),is.numeric(y),length(x)==length(y),B>=99L,B==as.integer(B))
  ok <- is.finite(x)&is.finite(y)
  pary <- data.frame(x=x[ok],y=y[ok])
  n <- nrow(pary)
  stopifnot(n>=4L,sd(pary$x)>0,sd(pary$y)>0)
  test <- cor.test(pary$x,pary$y,method='spearman',exact=FALSE)
  r <- unname(test$estimate)
  set.seed(ziarno)
  perm <- replicate(B,cor(pary$x,sample(pary$y),method='spearman'))
  k <- sum(abs(perm)>=abs(r)-1e-12)
  boot <- replicate(B,{
    i <- sample.int(n,replace=TRUE)
    if(sd(pary$x[i])==0||sd(pary$y[i])==0) NA_real_ else
      cor(pary$x[i],pary$y[i],method='spearman')
  })
  ci <- unname(quantile(boot,c(.025,.975),na.rm=TRUE))
  list(pary=pary,N_wejscie=length(x),N_par=n,N_brak=length(x)-n,
    klasyczny=data.frame(N=n,r_S=r,S=unname(test$statistic),p=test$p.value),
    przedzialy=data.frame(metoda='Bootstrap par',estymata=r,dol=ci[1],gora=ci[2],
      B_wazne=sum(is.finite(boot))),
    losowanie=data.frame(r_S=r,skrajne=k,B=B,p_perm=(k+1)/(B+1)),
    perm=perm,boot=boot)
}
analizy_wariantu <- function(projekt,B=1999L,ziarno=202627L) {
  stopifnot(inherits(projekt,'zi_projekt'))
  # Zachowaj stan sesji; powtórne wydrukowanie wyniku zostawia generator liczb losowych bez zmian.
  rng_istnial <- exists('.Random.seed',envir=globalenv(),inherits=FALSE)
  if(rng_istnial) rng <- get('.Random.seed',envir=globalenv(),inherits=FALSE)
  on.exit(if(rng_istnial) assign('.Random.seed',rng,envir=globalenv()) else
    if(exists('.Random.seed',envir=globalenv(),inherits=FALSE))
      rm('.Random.seed',envir=globalenv()),add=TRUE)
  cfg <- projekt$pakiet$scenariusz
  a <- porownaj_grupy(projekt$dane,grupa_1=projekt$odniesienie,
    grupa_2=projekt$porownywana,B=B,ziarno=ziarno)
  tabelaryczna <- cfg$druga_analiza=='tabela krzyżowa'
  b <- if(tabelaryczna) {
    tab <- table(factor(projekt$dane$grupa,levels=c(projekt$odniesienie,projekt$porownywana)),
      factor(projekt$dane$powodzenie,levels=0:1))
    analiza_tabeli(tab,B=B,ziarno=ziarno)
  } else analizuj_spearmana(projekt$dane$indeks,projekt$dane[[cfg$zmienna_druga]],B=B,ziarno=ziarno)
  structure(list(projekt=projekt,pierwsza=a,druga=b,
    rodzaj=if(tabelaryczna) 'tabela' else 'Spearman',B=B,ziarno=ziarno),
    class='zi_analizy')
}

# ---- Wyniki do akapitów: tabele, wykresy i karta odtworzenia ----
wynik_pierwszej_analizy <- function(analizy) {
  p <- analizy$projekt
  a <- analizy$pierwsza
  cfg <- p$pakiet$scenariusz
  opis <- data.frame(Grupa=etykiety_grup(cfg,a$opis$grupa),n=a$opis$N,
    'Średnia'=liczba(a$opis$srednia),SD=liczba(a$opis$SD),Mediana=liczba(a$opis$mediana),
    check.names=FALSE)
  k <- a$klasyczny
  welch <- data.frame('Różnica [pkt]'=liczba(k$roznica),SE=liczba(k$SE),t=liczba(k$t),
    df=liczba(k$df,2L),p=formatuj_p(k$p),check.names=FALSE)
  ci <- data.frame(Metoda=a$przedzialy$metoda,'Różnica [pkt]'=liczba(a$przedzialy$estymata),
    'Dolna granica'=liczba(a$przedzialy$dol),'Górna granica'=liczba(a$przedzialy$gora),check.names=FALSE)
  l <- a$losowanie
  perm <- data.frame('Różnica obserwowana [pkt]'=liczba(l$T_obserwowane),k=l$skrajne,B=l$B,
    p_perm=formatuj_p(l$p_perm),check.names=FALSE)
  g <- data.frame('Różnica [pkt]'=liczba(a$efekt$roznica),'SD łączone'=liczba(a$efekt$SD_laczone),
    'Hedges g'=liczba(a$efekt$Hedges_g),check.names=FALSE)
  wydruk_zi(karty=list('Analiza 1: kontrast i jednostka'=list('Pomiar'=nazwa_indeksu(cfg),
      'Kontrast'=kontrast_do_druku(p),'Jednostka różnicy'='punkty indeksu 1–5')),
    tabele=list('Opis grup [pkt 1–5]'=opis,'Test Welcha'=welch,
      '95% CI różnicy średnich [pkt]'=ci,'Tasowanie etykiet grup'=perm,
      'Efekt standaryzowany'=g))
}
wykres_dwa_tory <- function(analizy) {
  a <- analizy$pierwsza
  delta <- a$klasyczny$roznica
  szer <- signif(diff(range(c(a$zerowy,a$bootstrap)))/40,1)
  boot <- badaniaZI::wykres_bootstrap(a$bootstrap,szer,os='Różnica średnich [pkt]',
    tytul='Bootstrap osób w grupach: rozkład różnicy')
  zero <- badaniaZI::wykres_rozklad_zerowy(a$zerowy,delta,szer,os='Różnica średnich po tasowaniu [pkt]',
    tytul='Tasowanie etykiet: rozkład różnicy przy H0')
  wydruk_zi(wykresy=list(boot,zero))
}
wynik_drugiej_analizy <- function(analizy) {
  p <- analizy$projekt
  cfg <- p$pakiet$scenariusz
  b <- analizy$druga
  if(analizy$rodzaj=='tabela') {
    grupy <- etykiety_grup(cfg,rownames(b$tab))
    liczby <- data.frame(Grupa=grupy,'Kod 0'=b$tab[,1],'Kod 1'=b$tab[,2],n=rowSums(b$tab),
      'Kod 1 [% grupy]'=liczba(b$opis$procent,1L),check.names=FALSE,row.names=NULL)
    k <- b$klasyczny
    test <- data.frame(N=k$N,'Chi-kwadrat'=liczba(k$chi2),df=k$df,p=formatuj_p(k$p_chi2),
      'Najmniejsza oczekiwana'=liczba(k$min_E,1L),'Komórki z oczekiwaną < 5'=k$komorki_E_mniejsze_5,
      check.names=FALSE)
    l <- b$losowanie
    mc <- data.frame('Chi-kwadrat obserwowane'=liczba(l$chi2),k=l$skrajne,B=l$B,
      p_MC=formatuj_p(l$p_MC),check.names=FALSE)
    ci <- data.frame(Metoda=b$przedzialy$metoda,'Różnica [pp]'=liczba(100*b$przedzialy$estymata,2L),
      'Dolna granica'=liczba(100*b$przedzialy$dol,2L),'Górna granica'=liczba(100*b$przedzialy$gora,2L),
      check.names=FALSE)
    tabele <- list('Liczby i odsetki kodu 1 w grupach'=liczby,'Test chi-kwadrat bez korekty ciągłości'=test)
    if(isTRUE(b$rzadkie)) tabele[['Test Fishera: iloraz szans z 95% CI']] <- data.frame(
      'Iloraz szans'=liczba(b$fisher$OR_warunkowe),'Dolna granica'=liczba(b$fisher$CI_OR_dol),
      'Górna granica'=liczba(b$fisher$CI_OR_gora),p=formatuj_p(b$fisher$p_Fisher),check.names=FALSE)
    tabele <- c(tabele,list('Tabele Monte Carlo przy stałych marginesach'=mc,
      '95% CI różnicy odsetków kodu 1 [pp]'=ci,
      'V Craméra: natężenie bez kierunku'=data.frame(V=liczba(b$efekty$V))))
    return(wydruk_zi(karty=list('Analiza 2: pole 0/1'=list(
      'Pytanie'=cfg$pytanie_binarne,
      'Kod 1'=pole_opisu(cfg,'kod_1','odpowiedź „tak”'),
      'Źródło pola'=pole_opisu(cfg,'rodzaj_binarnego','zapisane w karcie scenariusza'),
      'Kontrast'=kontrast_do_druku(p),
      'Metoda główna'=if(isTRUE(b$rzadkie)) 'test Fishera (liczebność oczekiwana poniżej 5); chi-kwadrat opisowo' else
        'test chi-kwadrat bez korekty ciągłości (wszystkie liczebności oczekiwane co najmniej 5)')),
      tabele=tabele))
  }
  k <- b$klasyczny
  test <- data.frame('N par'=k$N,rS=liczba(k$r_S),S=liczba(k$S,0L),p=formatuj_p(k$p),check.names=FALSE)
  ci <- data.frame(Metoda=b$przedzialy$metoda,rS=liczba(b$przedzialy$estymata),
    'Dolna granica'=liczba(b$przedzialy$dol),'Górna granica'=liczba(b$przedzialy$gora),
    'Ważne repliki'=b$przedzialy$B_wazne,check.names=FALSE)
  l <- b$losowanie
  perm <- data.frame(rS=liczba(l$r_S),k=l$skrajne,B=l$B,p_perm=formatuj_p(l$p_perm),check.names=FALSE)
  wydruk_zi(karty=list('Analiza 2: korelacja rang'=list(
      'Zmienne'=paste('indeks oraz',nazwa_drugiej(cfg)),
      'Współczynnik'='rS Spearmana: liczba bez jednostki od −1 do 1',
      'Przedział'='95% CI z bootstrapu całych par',
      'Tasowanie'='jedna zmienna przestawiana względem drugiej przy niezależności')),
    tabele=list('Kompletność par'=data.frame(Osoby=b$N_wejscie,'Kompletne pary'=b$N_par,
        'Pary niekompletne'=b$N_brak,check.names=FALSE),
      'Test Spearmana'=test,'95% CI współczynnika rS'=ci,'Tasowanie jednej zmiennej'=perm))
}
tabela_efektow <- function(analizy) {
  a <- analizy$pierwsza
  b <- analizy$druga
  druga <- if(analizy$rodzaj=='tabela')
    data.frame(Analiza='2: odsetek kodu 1',Efekt='różnica odsetków',Jednostka='pp',
      Estymata=100*b$przedzialy$estymata[1],dol=100*b$przedzialy$dol[1],gora=100*b$przedzialy$gora[1]) else
    data.frame(Analiza='2: korelacja rang',Efekt='rS Spearmana',Jednostka='bez jednostki',
      Estymata=b$przedzialy$estymata,dol=b$przedzialy$dol,gora=b$przedzialy$gora)
  rbind(data.frame(Analiza='1: indeks',Efekt='różnica średnich',Jednostka='pkt',
    Estymata=a$przedzialy$estymata[1],dol=a$przedzialy$dol[1],gora=a$przedzialy$gora[1]),druga)
}
efekty_projektu <- function(analizy) {
  e <- tabela_efektow(analizy)
  tab <- data.frame(Analiza=e$Analiza,Efekt=e$Efekt,Jednostka=e$Jednostka,Estymata=liczba(e$Estymata,2L),
    'Dolna granica'=liczba(e$dol,2L),'Górna granica'=liczba(e$gora,2L),check.names=FALSE)
  e$panel <- factor(paste0(e$Analiza,' [',e$Jednostka,']'),levels=paste0(e$Analiza,' [',e$Jednostka,']'))
  e$etykieta <- paste0(liczba(e$Estymata,2L),' [',liczba(e$dol,2L),'; ',liczba(e$gora,2L),']')
  kol <- badaniaZI::paleta_zi()
  wykres <- ggplot2::ggplot(e,ggplot2::aes(y=0)) +
    ggplot2::geom_vline(xintercept=0,linetype=2,colour='gray40') +
    ggplot2::geom_segment(ggplot2::aes(x=.data$dol,xend=.data$gora,yend=0),linewidth=1.1,colour=kol[['primary']]) +
    ggplot2::geom_point(ggplot2::aes(x=.data$Estymata),shape=18,size=4.5,colour=kol[['accent']]) +
    ggplot2::geom_text(ggplot2::aes(x=.data$Estymata,label=.data$etykieta),vjust=-1.3,size=3.2) +
    ggplot2::facet_wrap(~panel,ncol=1,scales='free_x') +
    ggplot2::scale_y_continuous(breaks=NULL,limits=c(-0.6,0.9)) +
    ggplot2::scale_x_continuous(labels=function(x) sub('^-','−',format(x,decimal.mark=',',trim=TRUE,drop0trailing=TRUE))) +
    ggplot2::labs(x='Efekt z 95% CI (linia przerywana: zero)',y=NULL,title='Efekty obu analiz w ich jednostkach') +
    badaniaZI::theme_zi()
  wydruk_zi(tabele=list('Efekty obu analiz z 95% CI'=tab),wykresy=list(wykres))
}
wykresy_wariantu <- function(analizy) {
  p <- analizy$projekt
  cfg <- p$pakiet$scenariusz
  b <- analizy$druga
  drugi <- if(analizy$rodzaj=='tabela')
    badaniaZI::wykres_licznosci_odsetki(etykiety_grup(cfg,rownames(b$tab)),b$tab[,2],rowSums(b$tab),
      zdarzenie='kod 1',tytul='Analiza 2: kod 1 w grupach') else {
    czestosc <- cfg$zmienna_druga!='czas_wyszukiwania'
    ggplot2::ggplot(b$pary,ggplot2::aes(.data$x,.data$y))+
      ggplot2::geom_point(alpha=.55,colour=badaniaZI::paleta_zi()[['primary']],
        position=ggplot2::position_jitter(width=0,height=if(czestosc) 0.15 else 0,seed=2026))+
      ggplot2::labs(x='Indeks [pkt 1–5]',y=paste0(toupper(substring(nazwa_drugiej(cfg),1,1)),substring(nazwa_drugiej(cfg),2)),
        title=paste0('Analiza 2: ',b$N_par,' kompletnych par'))+badaniaZI::theme_zi()
  }
  wydruk_zi(wykresy=list(wykres_indeksu(p),drugi))
}
slad_analizy <- function(analizy) {
  p <- analizy$projekt
  m <- p$pakiet$manifest
  karta <- list('Wariant danych'=paste(m$id,m$scenariusz,m$rocznik,sep=' / '),
    'Wersja generatora'=m$wersja_generatora,
    'Hash danych (pierwsza grupa)'=substr(m$hash_danych,1,8),
    'Reguły przygotowania'='identyczne duplikaty usunięte; 99 w pozycjach jako brak; czas poza 0–120 min jako brak; pozycja 3 odwrócona; indeks przy co najmniej 5 z 6 pozycji',
    'Losowania'=paste0('B = ',analizy$B,'; ziarno = ',analizy$ziarno),
    'Źródła obliczeń'='zapisany plik Rmd i towarzyszący mu analiza.R',
    'Wersja wysłana'='zdalne SHA z potwierdzenia oddania')
  wersje <- data.frame(Element=c('R','badaniaZI','knitr','rmarkdown'),
    Wersja=c(as.character(getRversion()),as.character(utils::packageVersion('badaniaZI')),
      as.character(utils::packageVersion('knitr')),as.character(utils::packageVersion('rmarkdown'))))
  wydruk_zi(karty=list('Odtworzenie wyniku'=karta),tabele=list('Wersje środowiska tego wykonania'=wersje))
}
# ---- Część wspólna z raportem projektu: koniec ----

# ---- Raport: odczyt zapisanych danych i tabele opisu ----
parametry_raportu <- function(plik=badaniaZI::plik_pracy('PROJEKT')) {
  if(isTRUE(getOption('knitr.in.progress'))) {
    parametry <- get0('params',envir=knitr::knit_global(),inherits=FALSE)
  } else {
    if(!file.exists(plik)) stop('Otwórz raport.Rmd i uruchom jego pierwszy chunk.',call.=FALSE)
    tekst <- readLines(plik,encoding='UTF-8',warn=FALSE)
    granice <- which(tekst=='---')
    if(length(granice)<2L||granice[1]!=1L)
      stop('Nagłówek raportu jest niekompletny. Odtwórz kopię pliku z zachowanym ID.',call.=FALSE)
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
      '. Odtwórz pliki swojego wariantu z pomocą prowadzącego.',call.=FALSE)
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
        '. Przywróć potwierdzoną wersję danych; manifest pozostaje bez zmian.',call.=FALSE)
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
  cfg <- projekt$pakiet$scenariusz
  indeks <- projekt$opis
  tab_indeks <- data.frame(Grupa=etykiety_grup(cfg,indeks$grupa),Osoby=indeks$N_osob,
    'n (ważny indeks)'=indeks$N_indeks,'Średnia'=liczba(indeks$srednia),SD=liczba(indeks$SD),
    Mediana=liczba(indeks$mediana),check.names=FALSE)
  czas <- do.call(rbind,lapply(c(projekt$odniesienie,projekt$porownywana),function(g) {
    y <- projekt$dane$czas_wyszukiwania[projekt$dane$grupa==g]
    y <- y[is.finite(y)]
    data.frame(Grupa=etykiety_grup(cfg,g),n=length(y),'Średnia'=liczba(mean(y),1L),
      SD=liczba(sd(y),1L),Mediana=liczba(median(y),1L),IQR=liczba(IQR(y),1L),check.names=FALSE)
  }))
  tabele <- list('Indeks w grupach [pkt 1–5]'=tab_indeks,czas)
  names(tabele)[2] <- paste0(pole_opisu(cfg,'nazwa_czasu','Czas zadania'),' w grupach [min]')
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
    tabele=list('Kanały: wspólny mianownik osób z kompletem czterech kodów'=wynik),digits=1L)
}
wykres_raportu <- function(analizy,numer=1L) {
  stopifnot(length(numer)==1L,numer %in% 1:2)
  wydruk_zi(wykresy=wykresy_wariantu(analizy)$wykresy[numer])
}
