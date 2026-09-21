# Przykłady liczbowe W03: obliczenia z jawnych statystyk i symulacja dydaktyczna.
wynik_z_opisu <- function(n, srednia, SD, odniesienie, jednostka) {
  stopifnot(n >= 2, SD > 0)
  SE <- SD / sqrt(n)
  t <- (srednia - odniesienie) / SE
  margines <- qt(.975, df=n-1) * SE
  data.frame(N=n, srednia=srednia, SD=SD, SE=SE,
             odniesienie=odniesienie, roznica=srednia-odniesienie,
             CI_srednia_dol=srednia-margines, CI_srednia_gora=srednia+margines,
             CI_roznica_dol=srednia-odniesienie-margines,
             CI_roznica_gora=srednia-odniesienie+margines,
             t=t, df=n-1, p=2*pt(-abs(t),df=n-1),
             d=(srednia-odniesienie)/SD, jednostka=jednostka)
}
przyklad_indeks <- wynik_z_opisu(100,3.24,.8,3,'punkty')
przyklad_czas <- wynik_z_opisu(16,11.2,6,10,'minuty')
przeloz_wynik <- function(wynik) {
  data.frame(wielkosc=names(wynik), wartosc=vapply(wynik,function(x) {
    if(is.numeric(x)) format(round(x,4),trim=TRUE) else x
  },character(1)),row.names=NULL)
}
se_i_n <- data.frame(N=c(25,100,400), SD=4, SE=4/sqrt(c(25,100,400)))
przyklad_prawdopodobienstwa <- function() {
  k <- 0:3
  data.frame(k=k,punktowe=dbinom(k,size=3,prob=.5),
    skumulowane=pbinom(k,size=3,prob=.5))
}
przyklad_czasu_modelowego <- function() {
  data.frame(wielkosc=c('Gęstość f(5) [1/min]','P(T <= 5)',
    'P(5 < T <= 10)','Mediana czasu [min]'),
    wartosc=c(dexp(5,rate=.1),pexp(5,rate=.1),
      pexp(10,rate=.1)-pexp(5,rate=.1),qexp(.5,rate=.1)))
}
set.seed(202627)
# Dodatnia populacja modelowa: średnia 10 minut, SD 4 minuty.
sdlog <- sqrt(log(1+(4/10)^2))
meanlog <- log(10)-sdlog^2/2
losuj_czasy <- function(n) rlnorm(n,meanlog=meanlog,sdlog=sdlog)
jedna_proba <- losuj_czasy(40)
powtorzenia <- t(replicate(2000, {
  x <- losuj_czasy(40)
  SE <- sd(x)/sqrt(length(x))
  c(srednia=mean(x),SD=sd(x),SE=SE,
    dol=mean(x)-qt(.975,39)*SE,gora=mean(x)+qt(.975,39)*SE)
}))
powtorzenia <- as.data.frame(powtorzenia)
powtorzenia$obejmuje <- powtorzenia$dol <= 10 & powtorzenia$gora >= 10
podsumowanie_symulacji <- data.frame(
  liczba_prob=nrow(powtorzenia), osob_w_probie=40,
  parametr_mu=10, srednia_estymat=mean(powtorzenia$srednia),
  SD_estymat=sd(powtorzenia$srednia),
  pokrycie_procent=100*mean(powtorzenia$obejmuje))
rozklady <- rbind(data.frame(wartosc=jedna_proba,typ='Czasy osób w jednej próbie'),
                  data.frame(wartosc=powtorzenia$srednia,typ='Średnie w 2000 próbach'))
wykres_rozkladow <- ggplot2::ggplot(rozklady,ggplot2::aes(x=wartosc)) +
  ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)),
                         bins=20,fill='#56B4E9',colour='white') +
  ggplot2::facet_wrap(~typ,ncol=1,scales='free_y') +
  ggplot2::geom_vline(xintercept=10,colour='#D55E00') +
  ggplot2::labs(x='Minuty',y='Gęstość',title='Rozrzut osób i rozrzut średnich') +
  badaniaZI::theme_zi()
odcinki <- head(powtorzenia,40)
odcinki$proba <- seq_len(nrow(odcinki))
wykres_przedzialow <- ggplot2::ggplot(odcinki,
    ggplot2::aes(x=srednia,y=proba,colour=obejmuje)) +
  ggplot2::geom_segment(ggplot2::aes(x=dol,xend=gora,yend=proba)) +
  ggplot2::geom_point(size=1.4) +
  ggplot2::geom_vline(xintercept=10,linetype=2) +
  ggplot2::scale_colour_manual(values=c('FALSE'='#D55E00','TRUE'='#0072B2'),
                               labels=c('Nie obejmuje','Obejmuje')) +
  ggplot2::labs(x='Przedział średniego czasu [min]',y='Kolejna próba',
                 colour='Parametr 10',title='40 przedziałów z powtarzanych prób') +
  badaniaZI::theme_zi() + ggplot2::theme(legend.position='bottom')
null_t <- data.frame(t=seq(-4.5,4.5,length.out=901))
null_t$gestosc <- dt(null_t$t,df=99)
wykres_t <- ggplot2::ggplot(null_t,ggplot2::aes(x=t,y=gestosc)) +
  ggplot2::geom_area(data=subset(null_t,t<=-3),fill='#D55E00') +
  ggplot2::geom_area(data=subset(null_t,t>=3),fill='#D55E00') +
  ggplot2::geom_line(colour='#0072B2',linewidth=.7) +
  ggplot2::geom_vline(xintercept=c(-3,3),linetype=2) +
  ggplot2::labs(x='Statystyka t przy H0',y='Gęstość',
                 title='Dwustronne p: oba ogony dla |t| co najmniej 3') +
  badaniaZI::theme_zi()
