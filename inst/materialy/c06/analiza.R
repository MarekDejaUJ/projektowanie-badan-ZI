# C06: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po regułach ćwiczenia C02; indeks sześciu pozycji według reguły 5/6 z C04.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_',1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje,minimum=5L)

# Porównanie dwóch niezależnych grup: różnica grupa_2 minus grupa_1, test Welcha,
# g Hedgesa, bootstrap osobno w grupach i tasowanie etykiet (B powtórzeń, stałe ziarno).
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
kolory <- badaniaZI::paleta_zi()

# Prawdopodobieństwo przewagi: udział par (nowy, doświadczony) z wyższym wynikiem nowego plus połowa remisów.
theta_przewagi <- function(analiza) {
  a <- analiza$dane$wynik[analiza$dane$grupa == levels(analiza$dane$grupa)[1]]
  b <- analiza$dane$wynik[analiza$dane$grupa == levels(analiza$dane$grupa)[2]]
  mean(outer(b, a, '>')) + 0.5 * mean(outer(b, a, '=='))
}
theta_indeksu <- theta_przewagi(wynik)

# Wartość p po polsku: cztery miejsca albo „< 0,0001”.
formatuj_p <- function(p) ifelse(p < 0.0001, '< 0,0001', formatC(p, format = 'f', digits = 4, decimal.mark = ','))

# Wykresy S02: indeks w grupach, rozkład t Welcha, bootstrap, tasowanie i przedziały.
wykres_z_srednia <- function(analiza, os, tytul, granica = TRUE) {
  badaniaZI::wykres_pudelkowy(analiza$dane$wynik, as.character(analiza$dane$grupa), os = os, tytul = tytul,
                              granica = granica) +
    ggplot2::stat_summary(fun = mean, geom = 'point', shape = 18, size = 5, colour = kolory[['accent']],
                          orientation = 'y')
}
wykres_grup <- wykres_z_srednia(wynik, 'Indeks deklarowanej użyteczności [1–5]',
  'Indeks w dwóch grupach: osoby, pudełka i średnie', granica = FALSE)
wykres_gestosci <- badaniaZI::wykres_histogram_panele(split(wynik$dane$wynik, wynik$dane$grupa), 0.25,
  poczatek = 1, skala = 'gestosc', srednia = TRUE, os = 'Indeks deklarowanej użyteczności [1–5]',
  os_y = 'Gęstość [1/pkt]', tytul = 'Histogramy gęstości indeksu w dwóch grupach')
wykres_t <- badaniaZI::wykres_ogony(wynik$klasyczny$t, 't', wynik$klasyczny$df, os = 'Statystyka t Welcha',
  tytul = 'Rozkład t(144,98) i obserwowane t = −3,822')
wykres_boot <- badaniaZI::wykres_bootstrap(wynik$bootstrap, 0.025,
  os = 'Nowi minus doświadczeni w próbie bootstrapowej [pkt]', tytul = 'Bootstrap różnicy średnich w grupach')
wykres_zerowy <- badaniaZI::wykres_rozklad_zerowy(wynik$zerowy, wynik$klasyczny$roznica, 0.025,
  os = 'Różnica średnich po tasowaniu etykiet [pkt]', tytul = 'Rozkład zerowy tasowania etykiet: indeks')
wykres_ci <- badaniaZI::wykres_przedzialy(wynik$przedzialy$metoda, wynik$przedzialy$estymata,
  wynik$przedzialy$dol, wynik$przedzialy$gora, odniesienie = 0, os = 'Nowi minus doświadczeni [pkt]',
  cyfry = 3L, tytul = 'Dwa przedziały tej samej różnicy')

# Czas S02: dystrybuanty grup i rozkład zerowy tasowania etykiet.
wykres_ecdf_czasu <- badaniaZI::wykres_dystrybuanta(dane$czas_wyszukiwania, prog = 10, grupa = dane$grupa,
  os = 'Czas wyszukiwania [min]', tytul = 'Dystrybuanty czasu w dwóch grupach')
# Szerokość przedziału to jedna trzecia |Δ|, więc granice słupków wypadają dokładnie przy ±|Δ|.
krok_czasu <- abs(wynik_czas$klasyczny$roznica) / 3
poczatek_czasu <- -abs(wynik_czas$klasyczny$roznica) -
  krok_czasu * ceiling((max(abs(wynik_czas$zerowy)) - abs(wynik_czas$klasyczny$roznica)) / krok_czasu)
wykres_zerowy_czasu <- badaniaZI::wykres_rozklad_zerowy(wynik_czas$zerowy, wynik_czas$klasyczny$roznica,
  krok_czasu, poczatek = poczatek_czasu, os = 'Różnica średnich czasu po tasowaniu [min]',
  tytul = 'Rozkład zerowy tasowania etykiet: czas')

# Cztery osoby: wszystkie sześć podziałów na dwie grupy po dwie osoby.
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
  Kierunek=c('Nowi minus doświadczeni','Doświadczeni minus nowi'),
  `Różnica [pkt]`=c(wynik$klasyczny$roznica,-wynik$klasyczny$roznica),
  t=c(wynik$klasyczny$t,-wynik$klasyczny$t),
  `Dolna granica`=c(wynik$przedzialy$dol[1],-wynik$przedzialy$gora[1]),
  `Górna granica`=c(wynik$przedzialy$gora[1],-wynik$przedzialy$dol[1]), check.names = FALSE)

# Sześć osób przed instruktażem i po nim (dane z wykładu W04).
pary <- data.frame(osoba=letters[1:6],przed=c(6,8,10,12,14,16),po=c(5,6,9,9,12,12))
pary$zmiana <- pary$po-pary$przed
test_par <- t.test(pary$po,pary$przed,paired=TRUE)
podsumowanie_par <- data.frame(N_par=6,zmiana=mean(pary$zmiana),
  SD_zmian=sd(pary$zmiana),SE=sd(pary$zmiana)/sqrt(6),
  CI_dol=test_par$conf.int[1],CI_gora=test_par$conf.int[2],
  t=unname(test_par$statistic),df=unname(test_par$parameter),p=test_par$p.value)
wykres_par <- badaniaZI::wykres_pary(pary$przed, pary$po, pary$osoba, os = 'Czas [min]',
  tytul = 'Sześć par: czas przed instruktażem i po nim')

# Hipotetyczne grupy A i B: osobne przedziały średnich i przedział różnicy.
ci_grup_demo <- data.frame(grupa=c('A','B'),N=100,srednia=c(3,3.3),SD=1)
ci_grup_demo$dol <- ci_grup_demo$srednia-qt(.975,99)/10
ci_grup_demo$gora <- ci_grup_demo$srednia+qt(.975,99)/10
ci_roznicy_demo <- data.frame(roznica=.3,SE=sqrt(2/100),
  dol=.3-qt(.975,198)*sqrt(2/100),gora=.3+qt(.975,198)*sqrt(2/100))
wykres_nakladania <- badaniaZI::wykres_przedzialy(c('Grupa A', 'Grupa B'), ci_grup_demo$srednia,
  ci_grup_demo$dol, ci_grup_demo$gora, os = 'Średnia i 95% przedział [pkt]', cyfry = 3L,
  tytul = 'Przedziały średnich grup A i B')

# Mała, zróżnicowana grupa i duża, jednorodna: SE różnicy w dwóch modelach.
se_demo <- data.frame(Model = c('Welch: osobne wariancje', 'Wspólna wariancja'),
  `SE różnicy [min]` = c(sqrt(10^2 / 10 + 2^2 / 100),
                         sqrt((9 * 10^2 + 99 * 2^2) / 108 * (1 / 10 + 1 / 100))), check.names = FALSE)

# Osobny syntetyczny scenariusz repozytorium: czas 24 osób w dwóch grupach.
repozytorium <- data.frame(osoba=sprintf('r%02d',1:24),
  grupa=rep(c('doświadczeni','nowi'),each=12),
  czas_min=c(7,8,10,11,6,9,8,13,5,11,10,10,
    9,11,12,14,8,13,10,16,7,12,14,9))
repo_wynik <- porownaj_grupy(repozytorium,'czas_min',ziarno=202628L)

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
tabela_przedzialow <- function(analiza) {
  tab <- analiza$przedzialy
  names(tab) <- c('Metoda', 'Estymata', 'Dolna granica', 'Górna granica')
  tab
}
tabela_welcha <- function(analiza) {
  data.frame(`Różnica` = analiza$klasyczny$roznica, SE = analiza$klasyczny$SE, t = analiza$klasyczny$t,
             df = analiza$klasyczny$df, p = formatuj_p(analiza$klasyczny$p), check.names = FALSE)
}
tabela_efektu <- function(analiza) {
  data.frame(Miara = c('Różnica średnich', 'Łączone SD s_p', 'd = różnica / s_p', 'Korekta J', 'g Hedgesa = J · d'),
             `Wartość` = as.numeric(analiza$efekt[1, ]), check.names = FALSE)
}
tabela_tasowania <- function(analiza) {
  tab <- data.frame(analiza$losowanie$T_obserwowane, analiza$losowanie$skrajne, analiza$losowanie$B,
                    formatuj_p(analiza$losowanie$p_perm))
  names(tab) <- c('Różnica obserwowana', 'Tasowania co najmniej tak skrajne (k)', 'B', 'p_perm')
  tab
}
opis_porownania <- function(analiza, jednostka = 'pkt 1–5', wykres = FALSE) {
  tab <- analiza$opis[, c('grupa', 'N', 'braki_wyniku', 'srednia', 'SD', 'mediana', 'IQR')]
  names(tab) <- c('Grupa', 'N', 'Braki', 'Średnia', 'SD', 'Mediana', 'IQR')
  if (wykres) {
    tab <- analiza$opis[, c('grupa', 'N', 'srednia', 'SD')]
    tab$minimum <- vapply(levels(analiza$dane$grupa), function(g)
      min(analiza$dane$wynik[analiza$dane$grupa == g]), numeric(1))
    tab$maksimum <- vapply(levels(analiza$dane$grupa), function(g)
      max(analiza$dane$wynik[analiza$dane$grupa == g]), numeric(1))
    names(tab) <- c('Grupa', 'N', 'Średnia', 'SD', 'Minimum', 'Maksimum')
  }
  wyk <- wykres_z_srednia(analiza, paste0('Wynik [', jednostka, ']'), 'Czas w dwóch grupach repozytorium')
  wydruk_zi(tabele = setNames(list(tab), paste0('Opis grup [', jednostka, ']')),
    wykresy = if (wykres) list(wyk) else list(), digits = 3L)
}
test_porownania <- function(analiza) {
  pokaz_tabele(tabela_welcha(analiza), 'Test Welcha — różnica grupy 2 minus grupy 1', 3L)
}
oryginal_testu <- function(analiza) wydruk_zi(tekst = capture.output(print(analiza$test)))
przedzial_porownania <- function(analiza) {
  pokaz_tabele(tabela_przedzialow(analiza), '95% przedziały różnicy średnich', 3L)
}
kierunek_porownania <- function() pokaz_tabele(odwrocenie_kierunku, 'Dwa kierunki tej samej różnicy', 3L)
efekt_porownania <- function(analiza) {
  pokaz_tabele(tabela_efektu(analiza), 'Efekt surowy i standaryzowany', 3L)
}
permutacja_porownania <- function(analiza) {
  pokaz_tabele(tabela_tasowania(analiza), 'Tasowanie etykiet grup', 3L)
}
tasowanie_i_welch <- function(analiza) {
  # Wynik tasowania obok klasycznego p tego samego pytania: porównanie bez wracania do poprzedniego zadania.
  wydruk_zi(tabele = list('Tasowanie etykiet grup' = tabela_tasowania(analiza),
    'Test Welcha tego samego pytania' = tabela_welcha(analiza)), digits = 3L)
}
podsumowanie_porownania <- function(analiza, jednostka = 'pkt 1–5') {
  ci <- badaniaZI::wykres_przedzialy(analiza$przedzialy$metoda, analiza$przedzialy$estymata,
    analiza$przedzialy$dol, analiza$przedzialy$gora, odniesienie = 0,
    os = paste0(analiza$kierunek, ' [', jednostka, ']'), cyfry = 2L, tytul = 'Dwa przedziały różnicy')
  wydruk_zi(tabele = list('Efekty' = tabela_efektu(analiza), '95% przedziały różnicy' = tabela_przedzialow(analiza)),
    wykresy = list(ci), digits = 3L)
}
wynik_do_raportu <- function(analiza) {
  # Komplet liczb do akapitu: grupy, efekt, dwa przedziały i dwa tory testu.
  opis <- analiza$opis[, c('grupa', 'N', 'srednia', 'SD')]
  names(opis) <- c('Grupa', 'N', 'Średnia', 'SD')
  g <- data.frame(`g Hedgesa` = analiza$efekt$Hedges_g, check.names = FALSE)
  wydruk_zi(tabele = list('Opis grup [min]' = opis, 'Test Welcha' = tabela_welcha(analiza),
    '95% przedziały różnicy' = tabela_przedzialow(analiza), 'Tasowanie etykiet grup' = tabela_tasowania(analiza),
    'Efekt standaryzowany' = g), digits = 3L)
}
czas_do_raportu <- function(analiza) {
  wydruk_zi(tabele = list('Test Welcha' = tabela_welcha(analiza),
    '95% przedziały różnicy' = tabela_przedzialow(analiza), 'Tasowanie etykiet grup' = tabela_tasowania(analiza)),
    digits = 3L)
}
podzialy_czterech_osob <- function() {
  tab <- mini_permutacje
  tab$skrajna <- ifelse(tab$skrajna, 'tak', 'nie')
  names(tab) <- c('Nowi', 'Doświadczeni', 'Różnica [pkt]', 'Różnica co najmniej 3 od zera')
  pokaz_tabele(tab, 'Sześć podziałów czterech osób', 2L)
}
przedzialy_grup_demo <- function() {
  grupy <- ci_grup_demo
  names(grupy) <- c('Grupa', 'N', 'Średnia', 'SD', 'Dolna granica', 'Górna granica')
  roznica <- ci_roznicy_demo
  names(roznica) <- c('Różnica B − A', 'SE', 'Dolna granica', 'Górna granica')
  wydruk_zi(tabele = list('95% przedziały średnich grup A i B' = grupy, '95% przedział różnicy B − A' = roznica),
    digits = 3L)
}
dane_par <- function() {
  tab <- pary
  names(tab) <- c('Osoba', 'Przed [min]', 'Po [min]', 'Zmiana po − przed [min]')
  pokaz_tabele(tab, 'Sześć par pomiarów', 0L)
}
test_par_do_raportu <- function() {
  tab <- data.frame(Miara = c('N par', 'Średnia zmiana [min]', 'SD zmian [min]', 'SE średniej zmiany [min]',
                              'Dolna granica 95% CI', 'Górna granica 95% CI', 't', 'df', 'p'),
                    `Wartość` = c(formatC(as.numeric(podsumowanie_par[1, 1:8]), format = 'f', digits = 3,
                                          decimal.mark = ','), formatuj_p(podsumowanie_par$p)), check.names = FALSE)
  tab$`Wartość`[c(1, 8)] <- c('6', '5')
  pokaz_tabele(tab, 'Test par: średnia zmian po − przed')
}
se_dwoch_modeli <- function() pokaz_tabele(se_demo, 'SE różnicy przy nierównych wariancjach', 2L)
