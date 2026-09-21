# Małe, jawnie hipotetyczne dane do wspólnego odczytu na wykładzie.
mini <- data.frame(osoba=letters[1:6],x=c(1,2,2,3,4,5),y=c(15,12,14,10,9,6))
mini$dx <- mini$x-mean(mini$x)
mini$dy <- mini$y-mean(mini$y)
mini$iloczyn <- mini$dx*mini$dy
mini$ranga_x <- rank(mini$x)
mini$ranga_y <- rank(mini$y)
test <- cor.test(mini$x,mini$y)
model <- lm(y~x,mini)
sm <- summary(model)
cf <- coef(sm)
ci <- confint(model)
korelacja <- data.frame(N=6,r=unname(test$estimate),t=unname(test$statistic),df=4,
  p=test$p.value,CI_dol=test$conf.int[1],CI_gora=test$conf.int[2],
  rho=cor(mini$x,mini$y,method='spearman'))
regresja <- data.frame(parametr=c('Wyraz wolny','Nachylenie'),
  b=cf[,1],SE=cf[,2],t=cf[,3],df=4,p=cf[,4],dol=ci[,1],gora=ci[,2],row.names=NULL)
dopasowanie <- data.frame(R2=sm$r.squared,SD_reszt=sm$sigma,
  SSE=sum(resid(model)^2),SST=sum((mini$y-mean(mini$y))^2))
# Pełna enumeracja 6! permutacji indeksów, bez korekty Monte Carlo +1.
permutacje <- function(x) {
  if(length(x)==1L) return(matrix(x,nrow=1))
  do.call(rbind,lapply(seq_along(x),function(i)
    cbind(x[i],permutacje(x[-i]))))
}
indeksy <- permutacje(1:6)
zerowy <- apply(indeksy,1,function(i) cor(mini$x,mini$y[i]))
k <- sum(abs(zerowy)>=abs(unname(test$estimate))-1e-12)
dokladny <- data.frame(uklady=nrow(indeksy),skrajne=k,p_perm=k/nrow(indeksy))
pred <- do.call(rbind,lapply(c('confidence','prediction'),function(typ) {
  z <- predict(model,data.frame(x=3),interval=typ)
  data.frame(typ=typ,x=3,przewidywany=z[,1],dol=z[,2],gora=z[,3])
}))
wykres <- ggplot2::ggplot(mini,ggplot2::aes(x,y))+
  ggplot2::geom_point(size=2,colour='#0072B2')+
  ggplot2::geom_smooth(method='lm',se=TRUE,colour='#D55E00')+
  ggplot2::labs(x='Deklarowana użyteczność katalogu [1–5]',y='Czas [min]',
    title='Sześć par do nauki odczytu, nie wynik badania')+badaniaZI::theme_zi()
wykres_perm <- ggplot2::ggplot(data.frame(r=zerowy),ggplot2::aes(r))+
  ggplot2::geom_histogram(bins=25,fill='#56B4E9',colour='white')+
  ggplot2::geom_vline(xintercept=c(-1,1)*abs(unname(test$estimate)),colour='#D55E00')+
  ggplot2::labs(x='r po zmianie parowania',y='Liczba układów',
    title='Wszystkie 720 permutacji indeksów')+badaniaZI::theme_zi()
ksztalty <- rbind(data.frame(typ='Monotoniczny',x=1:9,y=30/(1:9)),
  data.frame(typ='Kształt U',x=1:9,y=(1:9-5)^2+2))
wykres_ksztaltow <- ggplot2::ggplot(ksztalty,ggplot2::aes(x,y))+
  ggplot2::geom_point(colour='#0072B2')+ggplot2::geom_line(colour='#56B4E9')+
  ggplot2::facet_wrap(~typ,scales='free_y')+
  ggplot2::labs(x='x [jednostki umowne]',y='Czas [min]',
    title='Nieliniowy nie zawsze oznacza monotoniczny')+badaniaZI::theme_zi()
kalibracja <- data.frame(wynik_obserwowany=c(20,30,40,50,60),
  samoocena=c(40,50,60,70,80))
formatuj_wynik <- function(x) {
  for(nm in names(x)[grepl('^p($|_)',names(x))])
    if(is.numeric(x[[nm]])) x[[nm]] <- format.pval(x[[nm]],digits=3,eps=.0001)
  x
}

# Rozwinięcia zwracają wyniki do odczytu, nie gotowe odpowiedzi studenta.
wynik_w05 <- function(tabele=list(),wykresy=list()) {
  structure(list(tabele=tabele,wykresy=wykresy),class='wynik_w05')
}
print.wynik_w05 <- function(x,...) {
  dokument <- isTRUE(getOption('knitr.in.progress'))
  etykiety <- c(parametr='Parametr',estymata='Estymata',b='Estymata',
    proby='Wcześniejsze próby',przewidywany_czas='Średni czas [min]',
    bariera='Bariera',liczba='Liczba',skumulowany_procent='Narastająco [%]',
    urzadzenie='Urządzenie',udzial='Udział [%]',grupa='Grupa',
    zadanie='Zadanie',powodzenia='Powodzenia',procent='Powodzenia [%]',
    rownoczesne='Równoczesne żądania',czas_s='Mediana [s]',
    na_sekunde='Żądania/s',praktyka='Próby',trudnosc='Trudność',
    czas='Czas [min]',czastkowe='Reszta cząstkowa [min]')
  for(nazwa in names(x$tabele)) {
    tab <- x$tabele[[nazwa]]
    zmien <- names(tab) %in% names(etykiety)
    names(tab)[zmien] <- unname(etykiety[names(tab)[zmien]])
    if(dokument) {
      cat('\n\n**',nazwa,'**\n\n',sep='')
      format <- if(knitr::is_latex_output()) 'latex' else 'html'
      cat(as.character(knitr::kable(tab,format=format,
        row.names=FALSE,digits=3)),'\n\n')
    } else {
      cat('\n',nazwa,'\n',sep='')
      print(tab,row.names=FALSE)
    }
  }
  for(wykres in x$wykresy) print(wykres)
  invisible(x)
}

model_nieliniowy <- function() {
  d <- data.frame(proby=0:8)
  d$czas <- 3+12*exp(-.4*d$proby)+c(0,.3,-.4,.2,-.2,.25,-.1,.15,-.2)
  model <- stats::nls(czas~a+b*exp(-c*proby),data=d,
    start=list(a=3,b=12,c=.4))
  siatka <- data.frame(proby=seq(0,8,length.out=101))
  siatka$czas <- predict(model,newdata=siatka)
  cf <- coef(model)
  przyklady <- data.frame(proby=c(0,1,2,4,8))
  przyklady$przewidywany_czas <- predict(model,newdata=przyklady)
  wykres <- ggplot2::ggplot(d,ggplot2::aes(proby,czas))+
    ggplot2::geom_point(colour='#0072B2',size=2)+
    ggplot2::geom_line(data=siatka,colour='#D55E00')+
    ggplot2::labs(x='Wcześniejsze próby [liczba]',y='Czas [min]',
      title='Model nieliniowy: odczyt krzywej, nie dowód uczenia się',
      subtitle='Dziewięć hipotetycznych osób, po jednym pomiarze')+
    badaniaZI::theme_zi()
  wynik_w05(tabele=list(
    'Parametry krzywej'=data.frame(parametr=names(cf),estymata=unname(cf)),
    'Przewidywane czasy w zakresie przykładu'=przyklady),
    wykresy=list(wykres))
}

galeria_wiolinowa <- function() {
  d <- data.frame(grupa=rep(c('Nowi (N=12)','Doświadczeni (N=12)'),each=12),
    czas=c(2,3,4,5,6,6,7,8,10,12,15,18,2,3,3,4,4,5,5,6,6,7,8,12))
  wykres <- ggplot2::ggplot(d,ggplot2::aes(grupa,czas,fill=grupa))+
    ggplot2::geom_violin(trim=TRUE,bw=1.6,alpha=.55,show.legend=FALSE)+
    ggplot2::geom_boxplot(width=.12,fill='white',outlier.shape=NA,
      show.legend=FALSE)+
    ggplot2::geom_point(position=ggplot2::position_nudge(x=.13),size=1,
      show.legend=FALSE)+
    ggplot2::scale_fill_manual(values=c('#56B4E9','#E69F00'))+
    ggplot2::labs(x=NULL,y='Czas [min]',title='Wiolina, kwartyle i obserwacje',
      subtitle='Syntetyczne czasy, skala pola przed przycięciem ogonów')+
    badaniaZI::theme_zi()
  wynik_w05(wykresy=list(wykres))
}

galeria_pareto <- function() {
  d <- data.frame(bariera=c('Logowanie','Link','Filtr','Format','Inne'),
    liczba=c(12,9,5,3,1))
  d$bariera <- factor(d$bariera,levels=d$bariera)
  d$skumulowany_procent <- 100*cumsum(d$liczba)/sum(d$liczba)
  czestosci <- ggplot2::ggplot(d,ggplot2::aes(bariera,liczba))+
    ggplot2::geom_col(fill='#56B4E9')+
    ggplot2::scale_y_continuous(breaks=seq(0,12,3))+
    ggplot2::labs(x=NULL,y='Zgłoszenia [liczba]',
      title='Pareto: bariery od najczęstszej',
      subtitle='30 syntetycznych zgłoszeń, jedna kategoria na zgłoszenie')+
    badaniaZI::theme_zi()
  kumulacja <- ggplot2::ggplot(d,
      ggplot2::aes(bariera,skumulowany_procent,group=1))+
    ggplot2::geom_line(colour='#D55E00')+
    ggplot2::geom_point(colour='#D55E00')+
    ggplot2::scale_y_continuous(limits=c(0,100),breaks=seq(0,100,25))+
    ggplot2::labs(x='Główna bariera zgłoszenia',y='Skumulowany udział [%]',
      title='Te same kategorie: narastający udział zgłoszeń')+
    badaniaZI::theme_zi()
  wynik_w05(tabele=list('Licznik i kumulacja'=d[1:3]),
    wykresy=list(czestosci,kumulacja))
}

galeria_kolowa <- function() {
  d <- data.frame(urzadzenie=c('Komputer','Telefon','Tablet'),N=c(24,16,8))
  d$udzial <- 100*d$N/sum(d$N)
  d$etykieta <- paste0(d$urzadzenie,'\n',round(d$udzial,1),'%')
  wykres <- ggplot2::ggplot(d,ggplot2::aes(x='',y=N,fill=urzadzenie))+
    ggplot2::geom_col(width=1,colour='white')+
    ggplot2::geom_text(ggplot2::aes(label=etykieta),
      position=ggplot2::position_stack(vjust=.5),size=3)+
    ggplot2::coord_polar(theta='y')+
    ggplot2::scale_fill_manual(values=c('#56B4E9','#E69F00','#009E73'))+
    ggplot2::labs(title='Jedno urządzenie bieżącej sesji',
      subtitle='N=48 hipotetycznych osób, rozłączne kategorie')+
    ggplot2::theme_void()+ggplot2::theme(legend.position='none')
  wynik_w05(tabele=list('Podział całej próby'=d[1:3]),wykresy=list(wykres))
}

galeria_mapy <- function() {
  d <- data.frame(grupa=rep(c('Nowi','Doświadczeni'),each=3),
    zadanie=rep(c('Katalog','Repozytorium','Weryfikacja'),2),
    N=c(15,20,10,12,20,15),powodzenia=c(9,12,8,9,18,9))
  d$procent <- 100*d$powodzenia/d$N
  d$etykieta <- paste0(d$powodzenia,'/',d$N,'\n',round(d$procent),'%')
  wykres <- ggplot2::ggplot(d,ggplot2::aes(zadanie,grupa,fill=procent))+
    ggplot2::geom_tile(colour='white')+
    ggplot2::geom_text(ggplot2::aes(label=etykieta),size=3.5)+
    ggplot2::scale_fill_gradient(low='#FFFFFF',high='#56B4E9',limits=c(0,100))+
    ggplot2::labs(x='Rodzaj zadania',y=NULL,fill='Powodzenie [%]',
      title='Mapa cieplna: udział i mianownik w każdej komórce',
      subtitle='92 hipotetyczne osoby, każda wykonuje jedno zadanie')+
    badaniaZI::theme_zi()+ggplot2::theme(legend.position='bottom')
  wynik_w05(tabele=list('Mianowniki sześciu komórek'=d[1:5]),wykresy=list(wykres))
}

galeria_wydajnosci <- function() {
  d <- data.frame(rownoczesne=c(20,40,60,80,100),czas_s=c(.35,.42,.8,1.6,3.2),
    na_sekunde=c(15,28,40,44,43))
  dlugie <- rbind(
    data.frame(obciazenie=d$rownoczesne,wynik=d$czas_s,
      miara='Mediana czasu odpowiedzi [s]'),
    data.frame(obciazenie=d$rownoczesne,wynik=d$na_sekunde,
      miara='Przepustowość [żądania/s]'))
  wykres <- ggplot2::ggplot(dlugie,ggplot2::aes(obciazenie,wynik))+
    ggplot2::geom_line(colour='#0072B2')+
    ggplot2::geom_point(colour='#D55E00',size=2)+
    ggplot2::facet_wrap(~miara,ncol=1,scales='free_y')+
    ggplot2::labs(x='Równoczesne żądania [liczba]',y=NULL,
      title='Wydajność techniczna nie jest kompetencją użytkownika',
      subtitle='Pięć hipotetycznych ustawień testu, nie pomiary studentów')+
    badaniaZI::theme_zi()
  wynik_w05(tabele=list('Obciążenie i dwa wyniki'=d),wykresy=list(wykres))
}

galeria_reszt_czastkowych <- function() {
  d <- data.frame(praktyka=1:12,trudnosc=rep(c(1,3,2),4))
  d$czas <- 18-.5*d$praktyka+2*d$trudnosc+
    c(.5,-.2,.3,-.4,.1,-.2,.4,-.3,.2,-.1,.3,-.5)
  m <- lm(czas~praktyka+trudnosc,data=d)
  b <- unname(coef(m)['praktyka'])
  d$czastkowe <- resid(m)+b*d$praktyka
  wykres <- ggplot2::ggplot(d,ggplot2::aes(praktyka,czastkowe))+
    ggplot2::geom_point(colour='#0072B2',size=2)+
    ggplot2::geom_abline(intercept=0,slope=b,colour='#D55E00')+
    ggplot2::scale_x_continuous(breaks=c(1,3,6,9,12))+
    ggplot2::labs(x='Wcześniejsze próby [liczba]',
      y='Reszta + składnik praktyki [min]',
      title='Reszty cząstkowe dla praktyki',
      subtitle='N=12 hipotetycznych osób; model uwzględnia też trudność zadania')+
    badaniaZI::theme_zi()
  wynik_w05(tabele=list('Współczynniki modelu'=data.frame(
    parametr=names(coef(m)),b=unname(coef(m))),
    'Pierwsze cztery obserwacje'=head(d,4)),wykresy=list(wykres))
}
