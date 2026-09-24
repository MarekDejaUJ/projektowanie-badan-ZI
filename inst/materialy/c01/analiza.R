# C01: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po regułach przygotowania: usunięcie identycznych duplikatów,
# kod 99 jako brak i czas spoza 0–120 minut jako brak (ćwiczenie C02).
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
pozycje <- dane[paste0('pozycja_', 1:6)]
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
tabela_glowna <- head(dane[c('id_odpowiedzi', 'grupa', 'czas_wyszukiwania', 'powodzenie')], 6)
wynik_glowny <- data.frame(N_surowe = nrow(dane_surowe), N_po = nrow(dane), kolumny = ncol(dane))
kolory <- badaniaZI::paleta_zi()

# Wszystkie ważne czasy S02 na jednej osi, z kwartylami.
wykres_glowny <- badaniaZI::wykres_punkty_os(dane$czas_wyszukiwania,
  os = 'Czas wyszukiwania [min]', tytul = 'Czasy wszystkich osób S02')

# Mały, odrębny przykład umożliwia sprawdzenie każdego licznika i mianownika.
mini <- data.frame(
  id = paste0('u0', 1:6),
  grupa = c('nowi', 'nowi', 'doświadczeni', 'nowi', 'doświadczeni', 'nowi'),
  czas_min = c(6, 8, 12, NA, 9, 5),
  powodzenie = c(1, 1, 0, NA, 1, 0)
)
czasy <- mini$czas_min

# Funkcja pokazuje, które wiersze faktycznie wchodzą do danego podsumowania.
odczytaj_mianownik <- function(tabela, zmienna) {
  stopifnot(is.data.frame(tabela), zmienna %in% names(tabela))
  x <- tabela[[zmienna]]
  data.frame(zmienna = zmienna, rekordy = length(x),
             wazne = sum(!is.na(x)), braki = sum(is.na(x)))
}

czytaj_rekord <- function(tabela, numer = 1L) {
  stopifnot(numer >= 1L, numer <= nrow(tabela))
  data.frame(pole = names(tabela),
             wartosc = unname(vapply(tabela, function(x) as.character(x[numer]), character(1))))
}

opis_mini <- data.frame(
  miara = c('Średni czas [min]', 'Mediana czasu [min]', 'Znane wyniki zadania',
            'Liczba powodzeń', 'Proporcja powodzeń', 'Odsetek powodzeń [%]'),
  wartosc = c(mean(czasy, na.rm = TRUE), median(czasy, na.rm = TRUE),
              sum(!is.na(mini$powodzenie)), sum(mini$powodzenie, na.rm = TRUE),
              mean(mini$powodzenie, na.rm = TRUE),
              100 * mean(mini$powodzenie, na.rm = TRUE))
)
kontrola_braku <- data.frame(
  postepowanie = c('Pięć zmierzonych czasów', 'Brak błędnie zastąpiony zerem'),
  suma_min = c(sum(czasy, na.rm = TRUE), sum(czasy, na.rm = TRUE)),
  mianownik = c(5, 6), srednia_min = c(40/5, 40/6)
)
odczyt_polecen <- data.frame(
  polecenie = c('czasy[3]', 'mini[3, "czas_min"]', 'mini$czas_min',
                'nrow(mini)', 'sum(is.na(czasy))', 'mean(czasy, na.rm = TRUE)'),
  sens = c('Trzecia wartość wektora', 'Czas z trzeciego wiersza tabeli',
            'Cała kolumna czasu', 'Liczba wierszy, także z brakami',
            'Liczba braków czasu', 'Średnia wyłącznie zmierzonych czasów')
)
wykres_mini <- ggplot2::ggplot(mini[!is.na(mini$czas_min), ],
                               ggplot2::aes(x = id, y = czas_min)) +
  ggplot2::geom_point(size = 3, colour = kolory[['primary']]) +
  ggplot2::geom_hline(yintercept = mean(czasy, na.rm = TRUE),
                     linetype = 2, colour = kolory[['accent']]) +
  ggplot2::annotate('text', x = 0.6, y = mean(czasy, na.rm = TRUE), label = 'średnia 8 min',
                    vjust = -0.6, hjust = 0, size = 3.3) +
  ggplot2::labs(x = 'Identyfikator osoby', y = 'Czas [min]',
                 title = 'Pięć zmierzonych czasów',
                 caption = 'Odrębny przykład syntetyczny; osoba u04 ma nieznany czas.') +
  badaniaZI::theme_zi()

# Zakres odsetka sukcesów sześciu osób zależny od nieznanego wyniku u04.
wykres_zakresu <- badaniaZI::wykres_przedzialy(
  'Sześć osób: zakres przy nieznanym wyniku u04', 60, 50, 400 / 6,
  os = 'Odsetek powodzeń [%]', cyfry = 1L,
  tytul = 'Zakres odsetka przy nieznanym wyniku')

# Wpływ zmiany wyniku jednej osoby na odsetek w próbie 5 i 50 osób.
wykres_czulosci <- badaniaZI::wykres_przedzialy(
  c('Próba 5 osób: 4 sukcesy', 'Próba 50 osób: 40 sukcesów'), c(80, 80), c(60, 78), c(100, 82),
  os = 'Odsetek powodzeń [%]; odcinek: zmiana wyniku jednej osoby', cyfry = 0L,
  tytul = 'Wrażliwość odsetka na jedną osobę')

# Oddzielna, syntetyczna historia do pięciu samodzielnych odpowiedzi.
portal <- data.frame(
  id=c('p01','p02','p03','p04','p05','p06','p07','p08'),
  rok=c('pierwszy','pierwszy','kolejny','pierwszy','kolejny','kolejny','pierwszy','kolejny'),
  czas_min=c(4,7,11,NA,6,14,9,5),
  sukces=c(1,1,0,NA,1,0,1,1),
  pewnosc=c(4,5,4,3,2,5,4,3)
)
opis_portalu <- data.frame(
  wskaznik=c('Wiersze','Ważny czas','Brak czasu','Średnia czasu [min]',
    'Mediana czasu [min]','Ważny sukces','Sukcesy','Sukces [% ważnych]'),
  wartosc=c(nrow(portal),sum(!is.na(portal$czas_min)),sum(is.na(portal$czas_min)),
    mean(portal$czas_min,na.rm=TRUE),median(portal$czas_min,na.rm=TRUE),
    sum(!is.na(portal$sukces)),sum(portal$sukces,na.rm=TRUE),
    100*mean(portal$sukces,na.rm=TRUE)))
wybor_portalu <- data.frame(wyrazenie=c('portal$czas_min[3]','portal[3, "czas_min"]',
  'nrow(portal)','sum(!is.na(portal$czas_min))','portal$id[is.na(portal$czas_min)]'),
  wynik=c(portal$czas_min[3],portal[3,'czas_min'],nrow(portal),
    sum(!is.na(portal$czas_min)),portal$id[is.na(portal$czas_min)]))
# Znaki potrzebne do odczytu wyrażeń w CHALLENGE 2.
znaki_wyrazen <- data.frame(
  Znak = c('[3]', '[3, "czas_min"]', '$', 'nrow()', 'is.na()', '!'),
  Znaczenie = c('Trzecia pozycja wektora', 'Wiersz 3 (przed przecinkiem), kolumna czas_min (po przecinku)',
                'Kolumna tabeli wybrana po nazwie', 'Liczba wierszy tabeli, także z brakami',
                'TRUE dla brakującej wartości', 'Zaprzeczenie: !is.na() daje TRUE dla wartości ważnej'))
# Źródło informacji w każdej kolumnie portalu: rejestr, cecha osoby, obserwacja albo deklaracja.
zrodla_portalu <- data.frame(
  Kolumna = c('id', 'rok', 'czas_min', 'sukces', 'pewnosc'),
  Znaczenie = c('Identyfikator osoby', 'Rok studiów', 'Czas szukania terminu [min]',
                '1 = poprawny termin, 0 = niepoprawny termin', 'Pewność korzystania z portalu, 1–5'),
  `Źródło` = c('rejestr', 'cecha osoby', 'obserwacja', 'obserwacja', 'deklaracja osoby'),
  check.names = FALSE)
blad_portalu <- data.frame(wariant=c('Brak pozostaje brakiem','Brak zastąpiony zerem'),
  N=c(7,8),suma_czasu=c(sum(portal$czas_min,na.rm=TRUE),sum(portal$czas_min,na.rm=TRUE)),
  srednia=c(mean(portal$czas_min,na.rm=TRUE),sum(portal$czas_min,na.rm=TRUE)/8))
portal_wykres <- portal[!is.na(portal$czas_min), ]
portal_wykres$wynik <- factor(ifelse(portal_wykres$sukces == 1, 'sukces', 'niepowodzenie'),
                              levels = c('sukces', 'niepowodzenie'))
wykres_portalu <- ggplot2::ggplot(portal_wykres, ggplot2::aes(czas_min, pewnosc, shape = wynik)) +
  ggplot2::geom_point(size = 3, colour = kolory[['primary']]) +
  ggplot2::geom_text(ggplot2::aes(label = id), vjust = -1, size = 3) +
  ggplot2::scale_shape_manual(values = c(sukces = 16, niepowodzenie = 1)) +
  ggplot2::scale_y_continuous(limits = c(1, 5.5), breaks = 1:5) +
  ggplot2::labs(x = 'Obserwowany czas [min]', y = 'Deklarowana pewność [1–5]',
    shape = 'Wynik zadania', title = 'Portal: deklaracja i wynik zadania',
    caption = 'Siedem osób z zapisanym czasem; osoba p04 ma nieznany czas i wynik.') +
  badaniaZI::theme_zi()

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

slownik_obserwacji <- function() {
  tab <- data.frame(
    kolumna=c('id','grupa','czas_min','powodzenie'),
    znaczenie=c('Identyfikator osoby','Wcześniejsze doświadczenie z katalogiem',
      'Czas obserwowanego zadania [min]','0 = niepowodzenie, 1 = powodzenie'),
    skala=c('identyfikator','nominalna','ilorazowa','nominalna'))
  pokaz_tabele(tab,'Definicje czterech kolumn')
}
odczyt_rekordu <- function(tabela,numer=1L) {
  pokaz_tabele(czytaj_rekord(tabela,numer),'Odczyt jednej osoby')
}
mianowniki_obserwacji <- function(tabela) {
  wydruk_zi(tabele=list('Czas'=odczytaj_mianownik(tabela,'czas_min'),
    'Powodzenie'=odczytaj_mianownik(tabela,'powodzenie')))
}
slownik_pelnego_badania <- function() {
  # Słownik z pakietu: brzmienie pozycji, skala i kodowanie każdej kolumny S02.
  s <- badaniaZI::slownik_zmiennych(c('id_odpowiedzi', 'grupa', 'czestosc_korzystania',
    'czas_wyszukiwania', 'pozycja_1', 'pozycja_3', 'powodzenie', 'kanal_1'))
  s$kodowanie[grepl('^pozycja_', s$zmienna)] <- '1–5: od 1 = zdecydowanie nie do 5 = zdecydowanie tak'
  s$kodowanie <- gsub('--', '–', s$kodowanie, fixed = TRUE)
  s$kodowanie[s$odwrocona] <- paste0(s$kodowanie[s$odwrocona], '; pozycja odwrócona')
  tab <- data.frame(Kolumna = gsub('_', '\\_', s$zmienna, fixed = TRUE), Opis = s$opis,
                    Skala = s$skala, Kodowanie = s$kodowanie)
  wydruk_zi(markdown = list(badaniaZI::tabela_markdown(tab, c(24, 34, 13, 29),
    'Wybrane kolumny zbioru S02: opis, skala pomiaru i kodowanie')))
}
podglad_badania <- function() {
  tab <- tabela_glowna
  names(tab) <- c('ID','Grupa','Czas [min]','Powodzenie')
  pokaz_tabele(tab,'Sześć pierwszych rekordów S02',2L)
}
slownik_odczytu <- function() pokaz_tabele(odczyt_polecen,'Polecenie i jego sens')
odczyt_portalu <- function(numer=6L) {
  wydruk_zi(tabele=list('Portal studencki — osiem osób'=portal,
    'Źródło informacji w kolumnach'=zrodla_portalu,
    'Rekord wybranej osoby'=czytaj_rekord(portal,numer)))
}
tabela_adresow <- function() {
  tab <- wybor_portalu
  names(tab) <- c('Wyrażenie R', 'Wynik')
  wydruk_zi(tabele=list('Polecenia wskazujące źródło wyniku'=tab, 'Znaki w wyrażeniach R'=znaki_wyrazen))
}
brak_portalu <- function() {
  tab <- blad_portalu
  names(tab) <- c('Reguła', 'N', 'Suma czasu [min]', 'Średnia [min]')
  wydruk_zi(tabele=list('Dwie reguły potraktowania braku'=tab,
    'Rekord osoby p04'=czytaj_rekord(portal,4L)), digits=2L)
}
opis_portalu_do_raportu <- function() {
  # Liczebności bez miejsc po przecinku, minuty z dwoma, odsetek z jednym.
  cyfry <- c(0, 0, 0, 2, 2, 0, 0, 1)
  wartosc <- vapply(seq_along(cyfry), function(i)
    formatC(opis_portalu$wartosc[i], format = 'f', digits = cyfry[i], decimal.mark = ','), character(1))
  tab <- data.frame(`Wskaźnik` = opis_portalu$wskaznik, `Wartość` = wartosc, check.names = FALSE)
  pokaz_tabele(tab, 'Opis portalu studenckiego')
}
deklaracja_i_wynik <- function() {
  tab <- portal[order(-portal$pewnosc, portal$czas_min), c('id', 'pewnosc', 'czas_min', 'sukces')]
  tab$sukces <- ifelse(is.na(tab$sukces), 'nieznany', ifelse(tab$sukces == 1, 'sukces', 'niepowodzenie'))
  names(tab) <- c('Osoba', 'Pewność [1–5]', 'Czas [min]', 'Wynik zadania')
  wydruk_zi(tabele=list('Osoby według deklarowanej pewności'=tab), wykresy=list(wykres_portalu))
}
