# C05: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po regułach ćwiczenia C02; indeks sześciu pozycji według reguły 5/6 z C04.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_', 1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)

# Jedna średnia wobec odniesienia mu0: test t, bootstrap percentylowy
# i losowe zmiany znaków odchyleń (B powtórzeń, stałe ziarno).
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
kolory <- badaniaZI::paleta_zi()

# Wartość p po polsku: cztery miejsca albo „< 0,0001”.
formatuj_p <- function(p) ifelse(p < 0.0001, '< 0,0001', formatC(p, format = 'f', digits = 4, decimal.mark = ','))
nazwy_opisu <- c(N = 'N (ważne indeksy)', braki = 'Braki indeksu', srednia = 'Średnia [pkt]', SD = 'SD [pkt]',
                 SE = 'SE średniej [pkt]', odniesienie = 'Odniesienie μ0 [pkt]',
                 roznica = 'Różnica od odniesienia [pkt]', d = 'd (bez jednostki)')

# Wykresy S02: osoby, rozkład t, bootstrap, rozkład zerowy i dwa przedziały.
wykres_danych <- badaniaZI::wykres_histogram(wynik$dane, 0.25, poczatek = 1, jednostka = 'pkt',
    os = 'Indeks deklarowanej użyteczności [1–5]', tytul = 'Indeksy osób i odniesienie 3') +
  ggplot2::geom_vline(xintercept = 3, linetype = 'dashed', colour = kolory[['accent']], linewidth = 0.9) +
  ggplot2::annotate('text', x = 3, y = Inf, label = 'odniesienie 3', hjust = 1.08, vjust = 1.5, size = 3.3)
wykres_t <- badaniaZI::wykres_ogony(wynik$klasyczny$t, 't', wynik$klasyczny$df, os = 'Statystyka t',
  tytul = 'Rozkład t(146) i obserwowane t = 4,876')
wykres_bootstrap <- badaniaZI::wykres_bootstrap(wynik$bootstrap, 0.02,
  os = 'Średnia w próbie bootstrapowej [pkt]', tytul = 'Bootstrap: niepewność średniej')
wykres_zerowy <- badaniaZI::wykres_rozklad_zerowy(wynik$zerowy, wynik$opis$roznica, 0.02,
  os = 'Średnia odchyleń po zmianie znaków [pkt]', tytul = 'Rozkład zerowy zmian znaków')
ci_sredniej <- subset(wynik$przedzialy, parametr == 'Średnia')
wykres_ci <- badaniaZI::wykres_przedzialy(ci_sredniej$metoda, ci_sredniej$estymata, ci_sredniej$dol,
  ci_sredniej$gora, odniesienie = 3, os = 'Średnia i 95% przedział [pkt]', cyfry = 3L,
  tytul = 'Ten sam parametr, dwie metody przedziału')
trzy_rozklady <- rbind(
  data.frame(panel = 'Indeksy osób (N = 147)', wartosc = wynik$dane),
  data.frame(panel = 'Średnie prób bootstrapowych (B = 1999)', wartosc = wynik$bootstrap),
  data.frame(panel = 'Statystyki po zmianie znaków (B = 1999)', wartosc = wynik$zerowy))
trzy_rozklady$panel <- factor(trzy_rozklady$panel, levels = unique(trzy_rozklady$panel))
wykres_trzy <- ggplot2::ggplot(trzy_rozklady, ggplot2::aes(x = wartosc)) +
  ggplot2::geom_histogram(bins = 30, fill = kolory[['secondary']], colour = 'white') +
  ggplot2::facet_wrap(~panel, ncol = 1, scales = 'free') +
  ggplot2::labs(x = 'Wartość w punktach indeksu', y = 'Liczba elementów',
                title = 'Trzy rozkłady, trzy jednostki') +
  badaniaZI::theme_zi()

# Mała demonstracja bootstrapu: sześć indeksów i jedno losowanie ze zwracaniem.
male_x <- c(2, 3, 3, 4, 4, 5)
indeksy_boot <- c(2, 2, 6, 1, 4, 4)
jedna_replika <- data.frame(`Miejsce w próbie` = 1:6, `Wylosowana osoba` = indeksy_boot,
                            Indeks = male_x[indeksy_boot], check.names = FALSE)

# Pełne wyliczenie 2^3 = 8 układów znaków dla odchyleń −1, 1 i 2.
male_odchylenia <- c(-1, 1, 2)
wszystkie_znaki <- expand.grid(z1 = c(-1, 1), z2 = c(-1, 1), z3 = c(-1, 1))
wszystkie_znaki$T <- as.vector(as.matrix(wszystkie_znaki[1:3]) %*% male_odchylenia) / 3
wszystkie_znaki$skrajne <- abs(wszystkie_znaki$T) >= abs(mean(male_odchylenia)) - 1e-12
p_dokladne <- mean(wszystkie_znaki$skrajne)

# Trzy hipotetyczne badania: średnia 3,2, SD 0,8, odniesienie 3, różne N.
wplyw_n <- do.call(rbind, lapply(c(25, 100, 400), function(n) {
  SE <- 0.8 / sqrt(n)
  stat <- 0.2 / SE
  kryt <- stats::qt(0.975, n - 1)
  data.frame(N = n, srednia = 3.2, SD = 0.8, roznica = 0.2, d = 0.25, SE = SE, t = stat, df = n - 1,
             p = 2 * stats::pt(-abs(stat), n - 1), dol = 3.2 - kryt * SE, gora = 3.2 + kryt * SE)
}))
wykres_n <- badaniaZI::wykres_przedzialy(paste('N =', wplyw_n$N), wplyw_n$srednia, wplyw_n$dol, wplyw_n$gora,
  odniesienie = 3, os = 'Średnia i 95% przedział [pkt]', cyfry = 2L, tytul = 'Ten sam efekt, trzy liczebności')
wplyw_odniesienia <- do.call(rbind, lapply(c(3, 3.2, 3.5), function(ref) {
  test <- t.test(wynik$dane, mu = ref)
  data.frame(odniesienie = ref, roznica = mean(wynik$dane) - ref,
             CI_dol = test$conf.int[1], CI_gora = test$conf.int[2],
             t = unname(test$statistic), p = test$p.value)
}))
monte_carlo <- data.frame(B = c(199, 1999, 19999))
monte_carlo$najmniejsze_p <- 1 / (monte_carlo$B + 1)
monte_carlo$SE_przy_005 <- sqrt(0.05 * 0.95 / monte_carlo$B)

# Osobne pytanie o proporcję: 26 sukcesów na 40 prób, odniesienie 0,5.
binom <- binom.test(26, 40, p = 0.5)
przyklad_proporcji <- data.frame(sukcesy = 26, N = 40, proporcja = 26 / 40,
  odniesienie = 0.5, CI_dol = binom$conf.int[1], CI_gora = binom$conf.int[2], p = binom$p.value)
rozklad_dwumianowy <- data.frame(k = 0:40, p = stats::dbinom(0:40, 40, 0.5))
rozklad_dwumianowy$skrajne <- rozklad_dwumianowy$p <= stats::dbinom(26, 40, 0.5) * (1 + 1e-7)
wykres_dwumianowy <- ggplot2::ggplot(rozklad_dwumianowy, ggplot2::aes(x = k, y = p, fill = skrajne)) +
  ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
  ggplot2::geom_vline(xintercept = 26, linetype = 'dashed', colour = kolory[['dark']]) +
  ggplot2::scale_fill_manual(values = c(`FALSE` = kolory[['secondary']], `TRUE` = kolory[['warning']])) +
  ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
  ggplot2::labs(x = 'Liczba sukcesów k wśród 40 prób', y = 'Prawdopodobieństwo P(K = k)',
                title = 'Rozkład dwumianowy Bin(40; 0,5)',
                subtitle = paste0('P(K ≤ 14) + P(K ≥ 26) = ',
                                  format(round(sum(rozklad_dwumianowy$p[rozklad_dwumianowy$skrajne]), 3),
                                         decimal.mark = ','))) +
  badaniaZI::theme_zi()

# Osobny syntetyczny scenariusz portalu; indeks z sześciu ukierunkowanych pozycji 1–5.
portal_indeksy <- c(rep(2, 2), rep(2.5, 3), rep(3, 6), rep(3.5, 6), rep(4, 5), rep(4.5, 2), NA)
portal_wynik <- analiza_jednej_sredniej(portal_indeksy, mu0 = 3, ziarno = 202628L)
portal_ci_sredniej <- subset(portal_wynik$przedzialy, parametr == 'Średnia')
portal_wykres_ci <- badaniaZI::wykres_przedzialy(portal_ci_sredniej$metoda, portal_ci_sredniej$estymata,
  portal_ci_sredniej$dol, portal_ci_sredniej$gora, odniesienie = c(3, 3.5),
  os = 'Średnia deklarowanej przejrzystości [pkt 1–5]', cyfry = 3L,
  tytul = '95% przedziały średniej; odniesienie 3 i cel 3,5')
portal_wykres_boot <- badaniaZI::wykres_bootstrap(portal_wynik$bootstrap, 0.05,
  os = 'Średnia w próbie bootstrapowej [pkt]', tytul = 'Portal: bootstrap średniej')
portal_wykres_zerowy <- badaniaZI::wykres_rozklad_zerowy(portal_wynik$zerowy, portal_wynik$opis$roznica, 0.05,
  os = 'Średnia odchyleń po zmianie znaków [pkt]', tytul = 'Portal: rozkład zerowy zmian znaków')

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

# Tabele wyników z polskimi nagłówkami.
tabela_klasyczna <- function(analiza) {
  data.frame(t = analiza$klasyczny$t, df = analiza$klasyczny$df, p = formatuj_p(analiza$klasyczny$p))
}
tabela_losowania <- function(analiza) {
  tab <- data.frame(analiza$losowanie$T_obserwowane, analiza$losowanie$skrajne, analiza$losowanie$B,
                    formatuj_p(analiza$losowanie$p_MC))
  names(tab) <- c('T obserwowane [pkt]', 'Repliki |T*| ≥ |T|', 'B', 'p_MC')
  tab
}
tabela_przedzialow <- function(tab) {
  tab <- tab[c('metoda', 'estymata', 'dol', 'gora')]
  names(tab) <- c('Metoda', 'Estymata', 'Dolna granica', 'Górna granica')
  tab
}
opis_sredniej <- function(analiza) {
  tab <- data.frame(Miara = unname(nazwy_opisu[names(analiza$opis)]),
                    `Wartość` = as.numeric(analiza$opis[1, ]), check.names = FALSE)
  pokaz_tabele(tab, 'Osoby, średnia, niepewność i efekt', 3L)
}
test_sredniej <- function(analiza) {
  pokaz_tabele(tabela_klasyczna(analiza), 'Dwustronny test t jednej średniej', 3L)
}
oryginal_testu_sredniej <- function(analiza) {
  wydruk_zi(tekst=trimws(capture.output(print(analiza$test)),which='right'))
}
przedzialy_sredniej <- function(analiza, metoda = c('obie', 't', 'bootstrap')) {
  metoda <- match.arg(metoda)
  tab <- analiza$przedzialy
  if (metoda == 't') tab <- tab[tab$metoda == 't', , drop = FALSE]
  if (metoda == 'bootstrap') tab <- tab[tab$metoda != 't', , drop = FALSE]
  # Osobne tabele utrzymują jawny parametr i mieszczą się na A4.
  wydruk_zi(tabele = list(
    '95% CI średniej [pkt]' = tabela_przedzialow(tab[tab$parametr == 'Średnia', ]),
    '95% CI różnicy od odniesienia [pkt]' = tabela_przedzialow(tab[tab$parametr != 'Średnia', ])),
    digits = 3L)
}
test_znakow_sredniej <- function(analiza) {
  pokaz_tabele(tabela_losowania(analiza), 'Zmiana znaków: T w punktach, p Monte Carlo', 3L)
}
dwa_testy_sredniej <- function(analiza) {
  wydruk_zi(tabele = list('Dwustronny test t' = tabela_klasyczna(analiza),
    'Losowe zmiany znaków' = tabela_losowania(analiza)), digits = 3L)
}
podsumowanie_sredniej <- function(analiza) {
  efekt <- analiza$opis[c('N', 'roznica', 'd')]
  names(efekt) <- c('N', 'Różnica od 3 [pkt]', 'd')
  nazwy <- przedzialy_sredniej(analiza)$tabele
  wydruk_zi(tabele = c(list('Efekt i N' = efekt, 'Wynik klasyczny' = tabela_klasyczna(analiza),
    'Wynik zmian znaków' = tabela_losowania(analiza)), nazwy), digits = 3L)
}
jedna_replika_do_odczytu <- function() pokaz_tabele(jedna_replika, 'Jedno losowanie sześciu osób ze zwracaniem', 0L)
uklady_znakow <- function() {
  tab <- wszystkie_znaki
  tab$skrajne <- ifelse(tab$skrajne, 'tak', 'nie')
  names(tab) <- c('Znak 1', 'Znak 2', 'Znak 3', 'T [pkt]', '|T| ≥ 2/3')
  pokaz_tabele(tab, 'Osiem układów znaków', 3L)
}
dokladnosc_losowania <- function() {
  tab <- monte_carlo
  names(tab) <- c('B', 'Najmniejsze p_MC', 'SE p_MC przy p = 0,05')
  pokaz_tabele(tab, 'Dokładność Monte Carlo i liczba replik B', 5L)
}
efekt_i_liczebnosc <- function() {
  tab <- wplyw_n[c('N', 'roznica', 'd', 'SE', 't', 'df')]
  tab$p <- formatuj_p(wplyw_n$p)
  names(tab) <- c('N', 'Różnica [pkt]', 'd', 'SE [pkt]', 't', 'df', 'p')
  pokaz_tabele(tab, 'Ten sam efekt, różne liczebności', 3L)
}
zmiana_odniesienia <- function() {
  ci <- unique(wplyw_odniesienia[c('CI_dol', 'CI_gora')])
  names(ci) <- c('Dolna granica', 'Górna granica')
  tab <- wplyw_odniesienia[c('odniesienie', 'roznica', 't')]
  tab$p <- formatuj_p(wplyw_odniesienia$p)
  names(tab) <- c('Odniesienie μ0', 'Różnica [pkt]', 't', 'p')
  wydruk_zi(tabele = list('Ten sam 95% CI średniej [pkt]' = ci, 'Trzy pytania zerowe' = tab), digits = 3L)
}
przyklad_proporcji_do_raportu <- function() {
  tab <- data.frame(Miara = c('Sukcesy', 'N', 'Proporcja', 'Odniesienie π0', 'Dolna granica 95% CI',
                              'Górna granica 95% CI', 'p (test dwumianowy)'),
                    `Wartość` = c(format(c(26, 40)), formatC(c(przyklad_proporcji$proporcja, 0.5,
                                  przyklad_proporcji$CI_dol, przyklad_proporcji$CI_gora), format = 'f',
                                  digits = 3, decimal.mark = ','), formatuj_p(przyklad_proporcji$p)),
                    check.names = FALSE)
  pokaz_tabele(tab, 'Pytanie o proporcję: dokładny test dwumianowy')
}
przedzialy_portalu_do_raportu <- function() {
  wynik_portalu <- przedzialy_sredniej(portal_wynik)
  wynik_portalu$wykresy <- list(portal_wykres_ci)
  wynik_portalu
}
rozklady_portalu_do_odczytu <- function() {
  wydruk_zi(wykresy = list(portal_wykres_boot, portal_wykres_zerowy))
}
efekt_i_cel_portalu <- function() {
  tab <- portal_wynik$opis[c('srednia', 'odniesienie', 'roznica', 'd')]
  names(tab) <- c('Średnia [pkt]', 'Odniesienie [pkt]', 'Różnica [pkt]', 'd')
  wydruk_zi(tabele = list('Średnia i efekt od 3 [pkt]; d standaryzowane' = tab,
    '95% CI średniej względem odniesień 3 i 3,5' = tabela_przedzialow(portal_ci_sredniej)), digits = 3L)
}
