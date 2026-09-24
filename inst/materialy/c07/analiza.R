# C07: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po regułach ćwiczenia C02: grupa i powodzenie zadania.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane

# Statystyka chi-kwadrat Pearsona bez korekty ciągłości.
statystyka_chi <- function(tab) {
  if(any(rowSums(tab)==0) || any(colSums(tab)==0)) return(NA_real_)
  E <- outer(rowSums(tab),colSums(tab))/sum(tab)
  sum((tab-E)^2/E)
}
# Tabela 2 x 2 (wiersze: grupy, kolumny: 0 i 1): oczekiwania, chi-kwadrat, Fisher,
# Monte Carlo przy ustalonych marginesach, różnica proporcji z dwoma CI i V.
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
  # Przedział Walda jest przybliżeniem dużopróbkowym; rzadkie komórki wymagają metod dokładnych.
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
    przedzialy=data.frame(metoda=c('Wald bez korekty','Bootstrap w grupach'),
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
kolory <- badaniaZI::paleta_zi()
nazwy_wyniku <- c('0: niepowodzenie', '1: sukces')

# Wartość p po polsku: cztery miejsca albo „< 0,0001”.
formatuj_p <- function(p) ifelse(p < 0.0001, '< 0,0001', formatC(p, format = 'f', digits = 4, decimal.mark = ','))

# Test z dla dwóch proporcji: z^2 równa się chi-kwadrat tabeli 2 x 2.
p_wspolne <- sum(tab[, 2]) / sum(tab)
se_zerowe <- sqrt(p_wspolne * (1 - p_wspolne) * sum(1 / rowSums(tab)))
z_proporcji <- wynik$efekty$roznica_proporcji / se_zerowe

# Wykresy S02: mozaika, reszty, rozkłady chi-kwadrat, Monte Carlo, z i przedziały.
wykres_mozaiki <- badaniaZI::wykres_mozaika(unclass(tab), nazwy_wyniku, tytul = 'Sukces i niepowodzenie w grupach S02')
komorki <- as.data.frame(tab)
komorki$E <- as.vector(wynik$E)
komorki$reszta <- as.vector(wynik$reszty)
komorki$powodzenie <- factor(nazwy_wyniku[as.integer(as.character(komorki$powodzenie)) + 1], levels = nazwy_wyniku)
wykres_reszt <- ggplot2::ggplot(komorki, ggplot2::aes(x = powodzenie, y = grupa, fill = reszta)) +
  ggplot2::geom_tile(colour = 'white') +
  ggplot2::geom_text(ggplot2::aes(label = paste0('O = ', Freq, ', E = ', format(E, decimal.mark = ','),
    '\nr = ', sub('^-', '−', formatC(reszta, format = 'f', digits = 3, decimal.mark = ',')))), size = 3.6) +
  ggplot2::scale_fill_gradient2(low = kolory[['accent']], mid = 'white', high = kolory[['secondary']], midpoint = 0,
                                limits = c(-1, 1), labels = function(v) sub('^-', '−', format(v, decimal.mark = ','))) +
  ggplot2::labs(x = NULL, y = NULL, fill = 'Reszta r', title = 'Reszty Pearsona w tabeli S02') +
  badaniaZI::theme_zi()
wykres_chi2_df <- badaniaZI::wykres_chi2(1:3, tytul = 'Rozkłady χ² dla df = 1, 2 i 3')
zerowy_wartosci <- as.data.frame(table(round(wynik$zerowy, 2)), stringsAsFactors = FALSE)
names(zerowy_wartosci) <- c('chi2', 'liczba')
zerowy_wartosci$chi2 <- as.numeric(zerowy_wartosci$chi2)
zerowy_wartosci$skrajna <- zerowy_wartosci$chi2 >= round(wynik$klasyczny$chi2, 2)
wykres_zerowy <- ggplot2::ggplot(zerowy_wartosci, ggplot2::aes(x = chi2, y = liczba, fill = skrajna)) +
  ggplot2::geom_col(width = 0.25, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = liczba), vjust = -0.4, size = 3) +
  ggplot2::geom_vline(xintercept = wynik$klasyczny$chi2, linetype = 'dashed', colour = kolory[['dark']]) +
  ggplot2::scale_fill_manual(values = c(`FALSE` = kolory[['secondary']], `TRUE` = kolory[['warning']])) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
  ggplot2::labs(x = 'Chi-kwadrat w losowej tabeli', y = 'Liczba tabel',
                title = 'Rozkład zerowy przy ustalonych marginesach',
                subtitle = paste0('B = 1999; χ² ≥ 1,466: ', wynik$losowanie$skrajne, ' tabel')) +
  badaniaZI::theme_zi()
wykres_ogona <- badaniaZI::wykres_chi2_mc(wynik$zerowy, wynik$klasyczny$chi2, 1,
  tytul = 'Ogon Monte Carlo i ogon χ²(1)')
wykres_z <- badaniaZI::wykres_ogony(z_proporcji, 'normalny', os = 'Statystyka z',
  tytul = 'Rozkład N(0, 1) i z = −1,211')
wykres_chi2_obs <- badaniaZI::wykres_ogony(wynik$klasyczny$chi2, 'chi2', 1, os = 'Statystyka χ²',
  tytul = 'Rozkład χ²(1) i χ² = z² = 1,466')
wykres_ci <- badaniaZI::wykres_przedzialy(wynik$przedzialy$metoda, 100 * wynik$przedzialy$estymata,
  100 * wynik$przedzialy$dol, 100 * wynik$przedzialy$gora, odniesienie = 0,
  os = 'Nowi minus doświadczeni [punkty procentowe]', cyfry = 1L, tytul = 'Niepewność różnicy skuteczności')

# Osobne przykłady: tabela rzadka, tabela bliska niezależności, skalowanie N, pomiar przed–po.
rzadka <- matrix(c(3,7,9,1),2,byrow=TRUE,dimnames=dimnames(tab))
wynik_rzadki <- analiza_tabeli(rzadka)
wykres_hipergeometryczny <- badaniaZI::wykres_rozklad_dyskretny(0:8, stats::dhyper(0:8, 8, 12, 10),
  os = 'Sukcesy doświadczonych przy ustalonych marginesach', zaznacz = c(0, 1, 7, 8), dystrybuanta = FALSE,
  tytul = 'Rozkład hipergeometryczny tabeli rzadkiej')
blisko_zero <- matrix(c(50,47,53,50),2,byrow=TRUE,dimnames=dimnames(tab))
wynik_blisko_zero <- analiza_tabeli(blisko_zero)
wykres_V <- ggplot2::ggplot(data.frame(V = wynik_blisko_zero$bootstrap_V), ggplot2::aes(x = V)) +
  ggplot2::geom_histogram(binwidth = 0.01, boundary = 0, fill = kolory[['secondary']], colour = 'white') +
  ggplot2::geom_vline(xintercept = wynik_blisko_zero$efekty$V, linetype = 'dashed', colour = kolory[['accent']]) +
  ggplot2::labs(x = 'V w próbie bootstrapowej', y = 'Liczba prób',
                title = 'V jest nieujemne: bootstrap tabeli bliskiej niezależności') +
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
wykres_przejsc <- badaniaZI::wykres_przejscia(unclass(przed_po), tytul = 'Te same 50 osób przed szkoleniem i po nim')

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

# Macierz staje się tabelą z jawnymi etykietami wierszy i kolumn.
tabela_z_etykietami <- function(tab, nazwa = 'Grupa') {
  out <- data.frame(rownames(tab), as.data.frame.matrix(tab), check.names = FALSE, row.names = NULL)
  names(out)[1] <- nazwa
  names(out)[names(out) == '0'] <- '0: niepowodzenie'
  names(out)[names(out) == '1'] <- '1: sukces'
  names(out)[names(out) == 'Sum'] <- 'Suma'
  out[[1]][out[[1]] == 'Sum'] <- 'Suma'
  out
}
tabela_miar <- function(nazwy, wartosci) data.frame(Miara = nazwy, `Wartość` = wartosci, check.names = FALSE)
tabela_chi <- function(analiza) {
  k <- analiza$klasyczny
  tabela_miar(c('N', 'Chi-kwadrat', 'df', 'p', 'Najmniejsze E', 'Komórki z E < 5'),
    c(k$N, formatC(k$chi2, format = 'f', digits = 3, decimal.mark = ','), k$df, formatuj_p(k$p_chi2),
      formatC(k$min_E, format = 'f', digits = 2, decimal.mark = ','), k$komorki_E_mniejsze_5))
}
tabela_mc <- function(analiza) {
  l <- analiza$losowanie
  data.frame(`Chi-kwadrat obserwowane` = l$chi2, `Tabele z χ²* ≥ χ² (k)` = l$skrajne, B = l$B,
             p_MC = formatuj_p(l$p_MC), check.names = FALSE)
}
tabela_przedzialow <- function(analiza) {
  tab <- analiza$przedzialy
  tab[c('estymata', 'dol', 'gora')] <- 100 * tab[c('estymata', 'dol', 'gora')]
  names(tab) <- c('Metoda', 'Estymata [pp]', 'Dolna granica [pp]', 'Górna granica [pp]')
  tab
}
tabela_efektow <- function(analiza, rozszerzone = FALSE) {
  e <- analiza$efekty
  nazwy <- c('Różnica proporcji', 'Różnica [punkty procentowe]', 'V Craméra')
  wartosci <- c(e$roznica_proporcji, e$roznica_pp, e$V)
  if (rozszerzone) {
    nazwy <- c(nazwy, 'Iloraz proporcji RR', 'Iloraz szans OR')
    wartosci <- c(wartosci, e$iloraz_proporcji, e$OR_surowe)
  }
  tabela_miar(nazwy, wartosci)
}
liczebnosci_tabeli <- function(analiza, marginesy = FALSE) {
  tb <- analiza$tab
  if (marginesy) tb <- addmargins(tb)
  pokaz_tabele(tabela_z_etykietami(tb), 'Liczebności: 0 = niepowodzenie, 1 = sukces', 0L)
}
odsetki_tabeli <- function(analiza, mianownik = c('grupa', 'wynik')) {
  mianownik <- match.arg(mianownik)
  margin <- if (mianownik == 'grupa') 1 else 2
  tb <- 100 * prop.table(analiza$tab, margin)
  tytul <- if (mianownik == 'grupa') 'Odsetki w grupach [%]' else 'Skład kategorii wyniku [%]'
  pokaz_tabele(tabela_z_etykietami(tb), tytul, 2L)
}
oczekiwania_tabeli <- function(analiza) {
  pokaz_tabele(tabela_z_etykietami(analiza$E), 'E przy niezależności', 2L)
}
wklady_tabeli <- function(analiza) {
  pokaz_tabele(tabela_z_etykietami(analiza$wklady), 'Wkłady komórek do chi-kwadrat', 3L)
}
test_tabeli <- function(analiza) pokaz_tabele(tabela_chi(analiza), 'Chi-kwadrat bez korekty ciągłości')
losowa_tabela <- function(analiza) {
  tb <- analiza$przykladowa_tabela
  dimnames(tb) <- dimnames(analiza$tab)
  pokaz_tabele(tabela_z_etykietami(addmargins(tb)), 'Jedna tabela z modelu zerowego', 0L)
}
monte_carlo_tabeli <- function(analiza) pokaz_tabele(tabela_mc(analiza), 'Warunkowy test Monte Carlo', 3L)
efekty_tabeli <- function(analiza, rozszerzone = FALSE) {
  pokaz_tabele(tabela_efektow(analiza, rozszerzone), 'Efekt: sukces wiersza 2 minus sukces wiersza 1', 4L)
}
przedzialy_tabeli <- function(analiza) {
  pokaz_tabele(tabela_przedzialow(analiza), '95% CI różnicy [punkty procentowe]', 2L)
}
mozaika_do_odczytu <- function(analiza, tytul) {
  pokaz_wykres(badaniaZI::wykres_mozaika(unclass(analiza$tab), nazwy_wyniku, tytul = tytul))
}
dwa_testy_tabeli <- function(analiza) {
  wydruk_zi(tabele = list('Chi-kwadrat bez korekty' = tabela_chi(analiza),
    'Warunkowe Monte Carlo' = tabela_mc(analiza)), digits = 3L)
}
rzadka_tabela_do_odczytu <- function() {
  wydruk_zi(tabele = list('Obserwacje' = tabela_z_etykietami(rzadka),
    'Oczekiwania' = tabela_z_etykietami(wynik_rzadki$E)), digits = 2L)
}
rzadkie_testy_do_odczytu <- function() {
  f <- wynik_rzadki$fisher
  fisher <- tabela_miar(c('OR warunkowe', 'Dolna granica 95% CI', 'Górna granica 95% CI', 'p Fishera'),
    c(formatC(c(f$OR_warunkowe, f$CI_OR_dol, f$CI_OR_gora), format = 'f', digits = 3, decimal.mark = ','),
      formatuj_p(f$p_Fisher)))
  wydruk_zi(tabele = list('Przybliżenie chi-kwadrat' = tabela_chi(wynik_rzadki),
    'Dokładny test Fishera' = fisher, 'Warunkowe Monte Carlo' = tabela_mc(wynik_rzadki)), digits = 3L)
}
v_przy_zerze <- function() {
  o <- wynik_blisko_zero$opis_boot_V
  opis <- tabela_miar(c('V obserwowane', 'Percentyl 2,5%', 'Percentyl 97,5%', 'B'),
    c(formatC(c(o$V, o$percentyl_025, o$percentyl_975), format = 'f', digits = 4, decimal.mark = ','), o$B_wazne))
  wydruk_zi(tabele = list('Tabela bliska niezależności' = tabela_z_etykietami(blisko_zero),
    'Test niezależności' = tabela_chi(wynik_blisko_zero), 'Opis rozkładu bootstrapowego V' = opis))
}
bootstrap_V_s02 <- function() {
  o <- wynik$opis_boot_V
  pokaz_tabele(tabela_miar(c('V obserwowane', 'Percentyl 2,5%', 'Percentyl 97,5%', 'B'),
    c(formatC(c(o$V, o$percentyl_025, o$percentyl_975), format = 'f', digits = 3, decimal.mark = ','), o$B_wazne)),
    'Bootstrap V w S02: opis zmienności')
}
skalowanie_do_odczytu <- function() {
  tb <- skalowanie_N
  tb$p <- formatuj_p(tb$p)
  names(tb) <- c('Mnożnik', 'N', 'Chi-kwadrat', 'p', 'V')
  pokaz_tabele(tb, 'Te same proporcje, różne N', 3L)
}
pomiar_sparowany_do_odczytu <- function() {
  w <- wynik_zmian
  zmiany <- tabela_miar(c('N osób', 'Poprawa (0 → 1)', 'Pogorszenie (1 → 0)', 'Zmiana proporcji',
                          'Chi-kwadrat McNemara', 'df', 'p z korektą', 'p dokładne'),
    c(w$N_osob, w$poprawa, w$pogorszenie, formatC(c(w$zmiana_proporcji, w$chi2_McNemara), format = 'f', digits = 3,
      decimal.mark = ','), w$df, formatuj_p(w$p_z_korekta), formatuj_p(w$p_dokladne)))
  wydruk_zi(tabele = list('Przed (wiersze) i po (kolumny)' = tabela_z_etykietami(przed_po, 'Przed'),
    'Zmiany wśród tych samych osób' = zmiany))
}
bilans_archiwum <- function() {
  bilans <- archiwum_bilans
  names(bilans) <- c('Grupa', 'Zarejestrowani', 'Brak wyniku', 'W tabeli')
  wydruk_zi(tabele = list('Od rejestracji do ważnego wyniku' = bilans,
    'Tabela wyników zadania' = tabela_z_etykietami(archiwum_tab)), digits = 0L)
}
mianowniki_archiwum <- function() {
  opis <- archiwum_wynik$opis
  names(opis) <- c('Grupa', 'N', 'Sukcesy', 'Sukcesy [%]')
  wydruk_zi(tabele = list('N grup i sukcesy' = opis,
    'Odsetki w grupach [%]' = tabela_z_etykietami(100 * prop.table(archiwum_tab, 1)),
    'Skład kategorii wyniku [%]' = tabela_z_etykietami(100 * prop.table(archiwum_tab, 2))),
    wykresy = list(badaniaZI::wykres_mozaika(archiwum_tab, nazwy_wyniku,
      tytul = 'Sukces w archiwum: szerokość słupka proporcjonalna do N')), digits = 2L)
}
testy_archiwum <- function() {
  out <- dwa_testy_tabeli(archiwum_wynik)
  out$tabele <- c(list('E przy niezależności' = tabela_z_etykietami(archiwum_wynik$E),
    'Wkłady do chi-kwadrat' = tabela_z_etykietami(archiwum_wynik$wklady)), out$tabele)
  out
}
efekt_archiwum <- function() {
  wydruk_zi(tabele = list('Efekt: sukces wiersza 2 minus sukces wiersza 1' = tabela_efektow(archiwum_wynik),
    '95% CI różnicy [punkty procentowe]' = tabela_przedzialow(archiwum_wynik)), digits = 3L)
}
