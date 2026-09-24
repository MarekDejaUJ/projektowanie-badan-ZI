# C08: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po regułach ćwiczenia C02; indeks sześciu pozycji według reguły 5/6 z C04.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_',1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)

# Związek dwóch zmiennych na kompletnych parach: Pearson, Spearman, tasowanie y,
# bootstrap całych par, regresja prosta i diagnostyka (B powtórzeń, stałe ziarno).
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
kolory <- badaniaZI::paleta_zi()
bilans <- data.frame(Zakres=c('Osoby po przygotowaniu','Ważny indeks','Ważny czas','Kompletne pary'),
  N=c(nrow(dane),sum(!is.na(dane$indeks)),sum(!is.na(dane$czas_wyszukiwania)),wynik$N_par))

# Wartość p po polsku: cztery miejsca albo „< 0,0001”.
formatuj_p <- function(p) ifelse(p < 0.0001, '< 0,0001', formatC(p, format = 'f', digits = 4, decimal.mark = ','))
liczba <- function(x, cyfry = 3L) sub('^-', '−', formatC(x, format = 'f', digits = cyfry, decimal.mark = ','))

# Wykresy S02: pary, galeria r, rozkłady losowań, prosta, reszty i pasma.
os_indeksu <- 'Deklarowana użyteczność katalogu [pkt 1–5]'
wykres_par <- ggplot2::ggplot(pary, ggplot2::aes(x, y)) +
  ggplot2::geom_point(alpha = 0.55, colour = kolory[['primary']]) +
  ggplot2::labs(x = os_indeksu, y = 'Czas [min]', title = 'Indeks i czas: 141 kompletnych par') +
  badaniaZI::theme_zi()
wykres_galerii <- badaniaZI::wykres_galeria_r(c(-0.13, -0.5, -0.9), n = nrow(pary), dane = pary,
  etykieta_danych = 'S02', tytul = 'Jak wygląda r = −0,13 obok silniejszych związków')
wykres_boot_pearson <- badaniaZI::wykres_bootstrap(wynik$boot['Pearson', ], 0.02,
  os = 'r Pearsona w próbie bootstrapowej', tytul = 'Bootstrap par: r Pearsona')
wykres_boot_spearman <- badaniaZI::wykres_bootstrap(wynik$boot['Spearman', ], 0.02,
  os = 'r Spearmana w próbie bootstrapowej', tytul = 'Bootstrap par: r Spearmana')
wykres_zerowy_pearson <- badaniaZI::wykres_rozklad_zerowy(wynik$perm['Pearson', ],
  wynik$klasyczny$wspolczynnik[1], 0.02, os = 'r Pearsona po tasowaniu y', tytul = 'Tasowanie y: r Pearsona')
wykres_zerowy_spearman <- badaniaZI::wykres_rozklad_zerowy(wynik$perm['Spearman', ],
  wynik$klasyczny$wspolczynnik[2], 0.02, os = 'r Spearmana po tasowaniu y', tytul = 'Tasowanie y: r Spearmana')
wykres_regresji <- badaniaZI::wykres_pasma_regresji(pary$x, pary$y, os_x = os_indeksu, os_y = 'Czas [min]',
  predykcja = FALSE, tytul = 'Prosta regresji i 95% przedział średniego czasu')
wykres_predykcji <- badaniaZI::wykres_pasma_regresji(pary$x, pary$y, x0 = 3, os_x = os_indeksu,
  os_y = 'Czas [min]', predykcja = TRUE, tytul = 'Średnia warunkowa i czas nowej osoby')
wykres_reszt <- ggplot2::ggplot(wynik$diagnostyka, ggplot2::aes(przewidywany, reszta)) +
  ggplot2::geom_point(alpha = 0.6, colour = kolory[['primary']]) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = kolory[['dark']]) +
  ggplot2::labs(x = 'Przewidywany czas [min]', y = 'Reszta [min]', title = 'Reszty wobec przewidywanego czasu') +
  badaniaZI::theme_zi()
wykres_hist_reszt <- badaniaZI::wykres_histogram(wynik$diagnostyka$reszta, 2.5, poczatek = -10, skala = 'gestosc',
  dopasuj = 'normalny', os = 'Reszta [min]', tytul = 'Rozkład reszt i krzywa normalna')
wykres_qq <- ggplot2::ggplot(wynik$diagnostyka, ggplot2::aes(sample = reszta)) +
  ggplot2::stat_qq(colour = kolory[['primary']]) + ggplot2::stat_qq_line(colour = kolory[['dark']], linetype = 'dashed') +
  ggplot2::labs(x = 'Kwantyl rozkładu normalnego', y = 'Kwantyl reszt [min]', title = 'Wykres Q–Q reszt') +
  badaniaZI::theme_zi()

# Miniatura sześciu par z wykładu W05: odchylenia, iloczyny i rangi.
mini <- data.frame(osoba=letters[1:6],x=c(1,2,2,3,4,5),y=c(15,12,14,10,9,6))
mini$dx <- mini$x-mean(mini$x)
mini$dy <- mini$y-mean(mini$y)
mini$iloczyn <- mini$dx*mini$dy
mini$ranga_x <- rank(mini$x)
mini$ranga_y <- rank(mini$y)
mini_r <- cor(mini$x,mini$y)
mini_rho <- cor(mini$x,mini$y,method='spearman')
mini_losowanie <- data.frame(Osoba=mini$osoba,x=mini$x,y=mini$y,`y po tasowaniu`=mini$y[c(3,6,1,5,2,4)],
  check.names=FALSE)
mini_boot <- mini[c(2,2,6,1,4,4),c('osoba','x','y')]
names(mini_boot) <- c('Osoba','x','y')
rownames(mini_boot) <- NULL

# Hipotetyczne kształty, punkt wpływowy, kalibracja i połączenie grup.
ksztalty <- rbind(
  data.frame(przyklad='Liniowy',x=1:9,y=c(17,16,14,13,11,10,8,7,5)),
  data.frame(przyklad='Monotoniczny, zakrzywiony',x=1:9,y=30/(1:9)),
  data.frame(przyklad='Kształt U',x=1:9,y=(1:9-5)^2+2))
ksztalty_wynik <- do.call(rbind,lapply(split(ksztalty,ksztalty$przyklad),function(z)
  data.frame(przyklad=z$przyklad[1],r=cor(z$x,z$y),rho=cor(z$x,z$y,method='spearman'))))
wykres_ksztaltow <- ggplot2::ggplot(ksztalty,ggplot2::aes(x,y))+
  ggplot2::geom_point(colour=kolory[['primary']])+ggplot2::geom_line(colour=kolory[['secondary']])+
  ggplot2::facet_wrap(~przyklad,nrow=1,scales='free_y')+
  ggplot2::labs(x='Wskaźnik x [jednostki umowne]',y='Czas [min]',title='Trzy hipotetyczne kształty zależności')+
  badaniaZI::theme_zi()
wplyw <- data.frame(x=c(1:8,9),y=c(2,3,3,5,4,6,6,8,30))
wplyw_wynik <- data.frame(Wariant=c('Wszystkie 9 osób','Pierwsze 8 osób'),
  r=c(cor(wplyw$x,wplyw$y),cor(wplyw$x[1:8],wplyw$y[1:8])),
  `Nachylenie [min/jedn.]`=c(coef(lm(y~x,wplyw))[2],coef(lm(y~x,wplyw[1:8,]))[2]),
  row.names=NULL, check.names=FALSE)
wykres_wplywu <- badaniaZI::wykres_wplyw(wplyw$x[1:8], wplyw$y[1:8], c(9, 30), os_x = 'x [jednostki umowne]',
  os_y = 'Czas [min]', tytul = 'Jeden punkt zmienia prostą')
kalibracja <- data.frame(osoba=letters[1:5],wynik_obserwowany=c(20,30,40,50,60),samoocena=c(40,50,60,70,80))
kalibracja$blad <- kalibracja$samoocena-kalibracja$wynik_obserwowany
kalibracja_wynik <- data.frame(r=cor(kalibracja$wynik_obserwowany,kalibracja$samoocena),
  srednie_zawyzenie_pp=mean(kalibracja$blad))
wykres_kalibracji <- badaniaZI::wykres_kalibracja(kalibracja$samoocena, kalibracja$wynik_obserwowany,
  tytul = 'Samoocena i wynik: idealna korelacja, stały błąd')
warunki <- data.frame(grupa=rep(c('Łatwe zadanie','Trudne zadanie'),each=5),
  x=c(1,2,3,4,5,4,5,6,7,8),y=c(10,9,8,7,6,20,19,18,17,16))
warunki_wynik <- data.frame(Zakres=c('Razem','Łatwe zadanie','Trudne zadanie'),
  r=c(cor(warunki$x,warunki$y),cor(warunki$x[1:5],warunki$y[1:5]),cor(warunki$x[6:10],warunki$y[6:10])))
wykres_grup <- ggplot2::ggplot(warunki,ggplot2::aes(x,y,shape=grupa,linetype=grupa))+
  ggplot2::geom_point(size=2.6,colour=kolory[['primary']])+
  ggplot2::geom_smooth(method='lm',formula=y~x,se=FALSE,colour=kolory[['primary']])+
  ggplot2::geom_smooth(data=warunki,ggplot2::aes(x,y),inherit.aes=FALSE,method='lm',formula=y~x,se=FALSE,colour=kolory[['accent']],
    linetype='dotted',linewidth=0.9)+
  ggplot2::scale_shape_manual(values=c(16,17))+
  ggplot2::labs(x='Doświadczenie [liczba wcześniejszych zadań]',y='Czas [min]',shape=NULL,linetype=NULL,
    title='Wewnątrz grup i po połączeniu grup')+
  badaniaZI::theme_zi()+ggplot2::theme(legend.position='bottom')

# CHALLENGE: samoocena sprawności weryfikacji źródeł i czas zadania.
# Odrębny syntetyczny przykład, niezależny od S02.
portal_dane <- data.frame(osoba=sprintf('w%02d',1:30),
  samoocena=c(rep(seq(1.5,4.5,.5),each=4),NA,3.5),
  czas=c(15,12,19,10,14,18,8,12,11,16,13,9,14,7,12,10,
    10,13,6,11,12,8,9,5,7,11,9,6,12,NA))
portal_wynik <- analizuj_zwiazek(portal_dane$samoocena,portal_dane$czas,ziarno=202628L)
portal_bilans <- data.frame(Zakres=c('Zarejestrowane osoby','Ważna samoocena',
  'Ważny czas','Kompletne pary','Brak przynajmniej jednej wartości'),
  N=c(nrow(portal_dane),sum(is.finite(portal_dane$samoocena)),
    sum(is.finite(portal_dane$czas)),portal_wynik$N_par,portal_wynik$N_brak))
portal_braki <- portal_dane[!stats::complete.cases(portal_dane[c('samoocena','czas')]),]
portal_zmienne <- data.frame(Zmienna=c('samoocena','czas'),
  Znaczenie=c('Średnia czterech stwierdzeń o weryfikacji źródła',
    'Od rozpoczęcia zadania do przesłania odpowiedzi'),
  `Skala i źródło`=c('1–5 pkt; deklaracja','minuty; obserwacja'), check.names=FALSE)

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

# Tabele wyników z polskimi nagłówkami.
tabela_korelacji <- function(analiza, wiersze = 1:2) {
  k <- analiza$klasyczny[wiersze, , drop = FALSE]
  # df Spearmana jest niepodane; kolumna df pozostaje pusta dla Spearmana.
  data.frame(Miara = sub(', przybliżenie', '', k$metoda, fixed = TRUE), N = k$N,
             `Współczynnik` = liczba(k$wspolczynnik), Statystyka = liczba(k$statystyka), Symbol = k$symbol,
             df = ifelse(is.na(k$df), '—', as.character(k$df)), p = formatuj_p(k$p), check.names = FALSE)
}
tabela_przedzialow_r <- function(analiza, wiersze = 1:3) {
  tab <- analiza$przedzialy[wiersze, , drop = FALSE]
  data.frame(Metoda = tab$metoda, `Dolna granica` = liczba(tab$dol), `Górna granica` = liczba(tab$gora),
             check.names = FALSE)
}
tabela_losowan <- function(analiza) {
  l <- analiza$losowanie
  data.frame(Miara = l$metoda, `Współczynnik` = liczba(l$wspolczynnik), k = l$skrajne, B = l$B,
             p_perm = formatuj_p(l$p_perm), check.names = FALSE)
}
tabela_regresji <- function(analiza) {
  r <- analiza$regresja
  list(wspolczynniki = data.frame(Parametr = r$parametr, Estymata = liczba(r$estymata), SE = liczba(r$SE),
                                  `Dolna granica CI` = liczba(r$CI_dol), `Górna granica CI` = liczba(r$CI_gora),
                                  check.names = FALSE),
       testy = data.frame(Parametr = r$parametr, t = liczba(r$t), df = r$df, p = formatuj_p(r$p)))
}
tabela_dopasowania <- function(analiza) {
  d <- analiza$dopasowanie
  data.frame(N = d$N, `R²` = liczba(d$R2, 4L), `SD reszt [min]` = liczba(d$SD_reszt, 2L),
             SSE = liczba(d$SSE, 1L), SST = liczba(d$SST, 1L), check.names = FALSE)
}
bilans_do_odczytu <- function() pokaz_tabele(bilans, 'Bilans ważnych pomiarów i par', 0L)
rozrzut_do_odczytu <- function(analiza, opis_x) {
  wykres <- ggplot2::ggplot(analiza$pary, ggplot2::aes(x, y)) +
    ggplot2::geom_point(alpha = 0.65, colour = kolory[['primary']], size = 2.2) +
    ggplot2::labs(x = opis_x, y = 'Czas [min]', title = paste('Jedna osoba — jedna para; N =', analiza$N_par)) +
    badaniaZI::theme_zi()
  pokaz_wykres(wykres)
}
korelacja_do_odczytu <- function(analiza, miara = c('obie', 'Pearson', 'Spearman')) {
  miara <- match.arg(miara)
  wiersze <- if (miara == 'obie') 1:2 else match(miara, c('Pearson', 'Spearman'))
  tytul <- if (miara == 'Pearson') 'Pearson: klasyczny test zerowej korelacji' else
    if (miara == 'Spearman') 'Spearman: przybliżony test korelacji rangowej' else
      'Klasyczny odczyt; p Spearmana przybliżone'
  pokaz_tabele(tabela_korelacji(analiza, wiersze), tytul)
}
przedzialy_korelacji <- function(analiza, miara = c('obie', 'Pearson', 'Spearman')) {
  miara <- match.arg(miara)
  wiersze <- if (miara == 'obie') 1:3 else if (miara == 'Pearson') 1:2 else 3
  pokaz_tabele(tabela_przedzialow_r(analiza, wiersze), '95% CI współczynnika')
}
losowanie_korelacji <- function(analiza) {
  pokaz_tabele(tabela_losowan(analiza), 'Tasowanie y: dwustronne p Monte Carlo')
}
regresja_do_odczytu <- function(analiza) {
  t <- tabela_regresji(analiza)
  wydruk_zi(tabele = list('Współczynniki modelu: y w minutach, x w punktach' = t$wspolczynniki,
    'Testy parametrów równych zero' = t$testy))
}
dopasowanie_do_odczytu <- function(analiza) {
  pokaz_tabele(tabela_dopasowania(analiza), 'Dopasowanie: R² bez jednostki; SD reszt w minutach')
}
diagnostyka_do_odczytu <- function(analiza) {
  wykres <- ggplot2::ggplot(analiza$diagnostyka, ggplot2::aes(przewidywany, reszta)) +
    ggplot2::geom_point(alpha = 0.65, colour = kolory[['primary']], size = 2.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = kolory[['dark']]) +
    ggplot2::labs(x = 'Przewidywany czas [min]', y = 'Reszta [min]',
                  title = 'Reszty: kształt, rozrzut i odległe punkty') + badaniaZI::theme_zi()
  pokaz_wykres(wykres)
}
przewidywanie_czasu <- function(analiza, x = 3) {
  stopifnot(length(x) == 1, is.finite(x))
  typy <- c('confidence', 'prediction')
  wynik_p <- do.call(rbind, lapply(typy, function(typ) {
    tab <- predict(analiza$model, newdata = data.frame(x = x), interval = typ)
    data.frame(przedmiot = if (typ == 'confidence') 'Średnia warunkowa' else 'Nowa osoba',
               x = x, czas = tab[1, 1], dol = tab[1, 2], gora = tab[1, 3], row.names = NULL)
  }))
  names(wynik_p) <- c('Przedmiot', 'Indeks x', 'Czas [min]', 'Dolna granica', 'Górna granica')
  pokaz_tabele(wynik_p, 'Dwa 95% przedziały: średnia i nowa osoba [min]', 2L)
}
rachunek_korelacji <- function() {
  tab <- mini[c('osoba', 'x', 'y', 'dx', 'dy', 'iloczyn')]
  names(tab) <- c('Osoba', 'x', 'y', 'x − średnia', 'y − średnia', 'Iloczyn')
  wynik_r <- data.frame(`r Pearsona` = liczba(mini_r), `r Spearmana` = liczba(mini_rho), check.names = FALSE)
  wydruk_zi(tabele = list('Odchylenia i iloczyny' = tab, 'Pearson i Spearman' = wynik_r), digits = 3L)
}
rangi_do_odczytu <- function() {
  tab <- mini[c('osoba', 'x', 'ranga_x', 'y', 'ranga_y')]
  names(tab) <- c('Osoba', 'x', 'Ranga x', 'y', 'Ranga y')
  pokaz_tabele(tab, 'Średnie rangi przy remisach', 1L)
}
ksztalty_do_odczytu <- function() {
  tab <- ksztalty_wynik
  names(tab) <- c('Kształt', 'r Pearsona', 'r Spearmana')
  wydruk_zi(tabele = list('Współczynniki trzech kształtów' = tab), wykresy = list(wykres_ksztaltow))
}
wplyw_do_odczytu <- function() {
  wydruk_zi(tabele = list('Porównanie wrażliwości' = wplyw_wynik), wykresy = list(wykres_wplywu))
}
kalibracja_do_odczytu <- function() {
  tab <- kalibracja
  names(tab) <- c('Osoba', 'Wynik obserwowany [%]', 'Samoocena [%]', 'Błąd [pp]')
  wyn <- data.frame(r = liczba(kalibracja_wynik$r), `Średni błąd [pp]` = kalibracja_wynik$srednie_zawyzenie_pp,
                    check.names = FALSE)
  wydruk_zi(tabele = list('Samoocena i wynik na tej samej skali' = tab, 'Korelacja i średni błąd samooceny' = wyn),
    wykresy = list(wykres_kalibracji))
}
grupy_do_odczytu <- function() {
  wydruk_zi(tabele = list('Korelacja łączna i wewnątrz grup' = warunki_wynik), wykresy = list(wykres_grup))
}

# Funkcje bloków CHALLENGE: każdy blok drukuje komplet liczb potrzebnych do akapitu.
pary_portalu <- function() {
  zakres <- data.frame(Zmienna = c('samoocena [pkt]', 'czas [min]'),
    Minimum = c(min(portal_wynik$pary$x), min(portal_wynik$pary$y)),
    Mediana = c(median(portal_wynik$pary$x), median(portal_wynik$pary$y)),
    Maksimum = c(max(portal_wynik$pary$x), max(portal_wynik$pary$y)))
  braki <- portal_braki
  names(braki) <- c('Osoba', 'Samoocena', 'Czas [min]')
  out <- rozrzut_do_odczytu(portal_wynik, 'Samoocena sprawności weryfikacji [pkt 1–5]')
  out$tabele <- list('Opis zmiennych' = portal_zmienne, 'Bilans osób i par' = portal_bilans,
    'Osoby z brakiem' = braki, 'Zakres wartości w kompletnych parach' = zakres)
  out$digits <- 1L
  out
}
wynik_korelacji_portalu <- function(miara = c('Pearson', 'Spearman')) {
  miara <- match.arg(miara)
  out <- korelacja_do_odczytu(portal_wynik, miara)
  out$tabele <- c(out$tabele, przedzialy_korelacji(portal_wynik, miara)$tabele)
  out
}
losowania_portalu <- function() {
  info <- portal_wynik$bootstrap_info
  names(info) <- c('Miara', 'Prawidłowe repliki', 'Repliki nieokreślone')
  wydruk_zi(tabele = list('Tasowanie y: dwustronne p Monte Carlo' = tabela_losowan(portal_wynik),
    'Klasyczny odczyt tych samych par' = tabela_korelacji(portal_wynik),
    'Kontrola replik bootstrapu' = info))
}
model_portalu <- function() {
  t <- tabela_regresji(portal_wynik)
  d <- portal_wynik$diagnostyka
  d$osoba <- portal_dane$osoba[stats::complete.cases(portal_dane[c('samoocena', 'czas')])]
  najwieksze <- d[order(-abs(d$reszta)), ][1:3, c('osoba', 'x', 'y', 'przewidywany', 'reszta')]
  names(najwieksze) <- c('Osoba', 'Samoocena', 'Czas [min]', 'Przewidywany [min]', 'Reszta [min]')
  out <- wydruk_zi(tabele = list('Współczynniki modelu: y w minutach, x w punktach' = t$wspolczynniki,
    'Testy parametrów równych zero' = t$testy, 'Dopasowanie modelu' = tabela_dopasowania(portal_wynik),
    'Trzy największe reszty' = najwieksze), digits = 2L)
  out$wykresy <- diagnostyka_do_odczytu(portal_wynik)$wykresy
  out
}
prognoza_portalu <- function(x = 3) {
  out <- przewidywanie_czasu(portal_wynik, x)
  r <- portal_wynik$regresja[2, ]
  nachylenie <- data.frame(`Nachylenie [min/pkt]` = liczba(r$estymata, 2L), `Dolna granica CI` = liczba(r$CI_dol, 2L),
                           `Górna granica CI` = liczba(r$CI_gora, 2L), check.names = FALSE)
  out$tabele <- c(out$tabele, list('Nachylenie modelu z 95% CI' = nachylenie))
  out
}
