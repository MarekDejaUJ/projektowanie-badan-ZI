# Osobne małe demonstracje W04, nie rekordy S02.
nowi <- c(8,10,12,14,16)
doswiadczeni <- c(4,6,8,10,12)
test_w <- t.test(nowi,doswiadczeni)
delta <- mean(nowi)-mean(doswiadczeni)
opis_grup <- data.frame(grupa=c('Nowi','Doświadczeni'),N=c(5,5),
  srednia=c(mean(nowi),mean(doswiadczeni)),SD=c(sd(nowi),sd(doswiadczeni)))
sp <- sqrt(((length(nowi)-1)*var(nowi)+(length(doswiadczeni)-1)*var(doswiadczeni))/8)
welch <- data.frame(roznica_nowi_minus_doswiadczeni=delta,
  SE=sqrt(var(nowi)/length(nowi)+var(doswiadczeni)/length(doswiadczeni)),
  CI_dol=test_w$conf.int[1],CI_gora=test_w$conf.int[2],
  t=unname(test_w$statistic),df=unname(test_w$parameter),p=test_w$p.value,
  Hedges_g=(1-3/(4*10-9))*delta/sp)
punkty <- data.frame(czas=c(nowi,doswiadczeni),
                     grupa=rep(c('Nowi','Doświadczeni'),each=5))
wykres_grup <- ggplot2::ggplot(punkty,ggplot2::aes(x=czas,y=grupa)) +
  ggplot2::geom_point(size=3,colour='#0072B2') +
  ggplot2::labs(x='Czas [min]',y=NULL,title='Czasy w dwóch niezależnych grupach') +
  badaniaZI::theme_zi()
pary <- data.frame(osoba=letters[1:6],przed=c(6,8,10,12,14,16),po=c(5,6,9,9,12,12))
pary$roznica_po_minus_przed <- pary$po-pary$przed
test_par <- t.test(pary$po,pary$przed,paired=TRUE)
wynik_par <- data.frame(N_par=nrow(pary),zmiana=mean(pary$roznica_po_minus_przed),
  SD_roznic=sd(pary$roznica_po_minus_przed),
  CI_dol=test_par$conf.int[1],CI_gora=test_par$conf.int[2],
  t=unname(test_par$statistic),df=unname(test_par$parameter),p=test_par$p.value)
dlugie_pary <- data.frame(osoba=rep(pary$osoba,2),
  moment=factor(rep(c('Przed','Po'),each=6),levels=c('Przed','Po')),
  czas=c(pary$przed,pary$po))
wykres_par <- ggplot2::ggplot(dlugie_pary,
  ggplot2::aes(x=moment,y=czas,group=osoba)) +
  ggplot2::geom_line(colour='#0072B2',alpha=.6) + ggplot2::geom_point(size=2) +
  ggplot2::labs(x=NULL,y='Czas [min]',title='Każda linia łączy dwa pomiary jednej osoby') +
  badaniaZI::theme_zi()
tab <- matrix(c(18,12,32,8),nrow=2,byrow=TRUE,
              dimnames=list(c('Nowi','Doświadczeni'),c('Sukces','Niepowodzenie')))
chi <- chisq.test(tab,correct=FALSE)
set.seed(202627)
mc <- chisq.test(tab,simulate.p.value=TRUE,B=1999)
fisher <- fisher.test(tab)
oczekiwane <- chi$expected
wklady <- (tab-oczekiwane)^2/oczekiwane
proporcje <- data.frame(grupa=rownames(tab),sukcesy=tab[,1],N=rowSums(tab),
  procent=100*tab[,1]/rowSums(tab),row.names=NULL)
test_prop <- prop.test(tab[,1],rowSums(tab),correct=FALSE)
efekty_kategorii <- data.frame(
  roznica_proporcji=.6-.8,CI_dol=test_prop$conf.int[1],CI_gora=test_prop$conf.int[2],
  iloraz_proporcji=.6/.8,iloraz_szans=(18/12)/(32/8),
  V=sqrt(unname(chi$statistic)/sum(tab)))
testy_kategorii <- data.frame(chi2=unname(chi$statistic),df=unname(chi$parameter),
  p_chi2=chi$p.value,p_MC=mc$p.value,p_Fisher=fisher$p.value,B=1999)
rzadka <- matrix(c(1,9,7,3),2,byrow=TRUE,dimnames=dimnames(tab))
fisher_rzadka <- fisher.test(rzadka)
wykres_proporcji <- ggplot2::ggplot(proporcje,ggplot2::aes(x=grupa,y=procent)) +
  ggplot2::geom_col(fill='#009E73',width=.55) +
  ggplot2::geom_text(ggplot2::aes(label=paste0(sukcesy,'/',N)),vjust=-.4) +
  ggplot2::scale_y_continuous(limits=c(0,100)) +
  ggplot2::labs(x=NULL,y='Powodzenia [%]',title='Porównanie udziałów z jawnymi mianownikami') +
  badaniaZI::theme_zi()
trzy_grupy <- data.frame(wersja=rep(c('A','B','C'),each=3),
                        czas=c(4,5,6,7,8,9,4,6,8))
model_aov <- aov(czas~wersja,data=trzy_grupy)
anova_tabela <- as.data.frame(summary(model_aov)[[1]])
rownames(anova_tabela) <- c('Między grupami','Wewnątrz grup')
eta2 <- anova_tabela$'Sum Sq'[1]/sum(anova_tabela$'Sum Sq')
pionowo <- function(x) {
  data.frame(wielkosc=names(x),wartosc=vapply(seq_along(x),function(j) {
    z <- x[[j]]
    if(grepl('^p($|_)',names(x)[j])) format.pval(z,digits=3,eps=.0001)
    else if(is.numeric(z)) format(round(z,4),trim=TRUE) else as.character(z)
  },character(1)),row.names=NULL)
}
