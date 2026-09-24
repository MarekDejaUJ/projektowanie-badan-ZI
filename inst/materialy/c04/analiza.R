# C04: gotowe dane, tabele i wykresy ćwiczenia. Student uruchamia bloki
# w zadanie.Rmd; obliczenia i wygląd wyników pozostają w tym skrypcie.

# Zbiór S02 po regułach ćwiczenia C02; pozycja 3 po odwróceniu kierunku.
dane_surowe <- badaniaZI::dane_przykladowe()
przygotowane <- badaniaZI::przygotuj_ankiete(dane_surowe)
dane <- przygotowane$dane
nazwy_pozycji <- paste0('pozycja_', 1:6)
pozycje_surowe <- dane[nazwy_pozycji]
pozycje <- pozycje_surowe
pozycje$pozycja_3 <- badaniaZI::odwroc_pozycje(pozycje$pozycja_3)
dane$liczba_pozycji <- rowSums(!is.na(pozycje))
dane$indeks <- badaniaZI::indeks_ankiety(pozycje, minimum = 5L)
indeks_6 <- badaniaZI::indeks_ankiety(pozycje, minimum = 6L)
kolory <- badaniaZI::paleta_zi()
etykiety_pozycji <- c('1 Pole wyszukiwania', '2 Nazwy filtrów', '3 Rekord bez zgadywania (odwr.)',
                      '4 Lokalizacja dokumentu', '5 Komunikaty katalogu', '6 Wsparcie zadania')
nazwy_odpowiedzi <- c('1 zdecydowanie nie', '2 raczej nie', '3 ani tak, ani nie', '4 raczej tak',
                      '5 zdecydowanie tak')
nazwy_kanalow <- c(kanal_1 = 'WWW', kanal_2 = 'e-mail', kanal_3 = 'media społecznościowe',
                   kanal_4 = 'kontakt bezpośredni', www = 'WWW', email = 'e-mail',
                   media = 'media społecznościowe', kontakt = 'kontakt bezpośredni')
kanaly <- badaniaZI::odpowiedzi_wielokrotne(dane[paste0('kanal_', 1:4)])

# Rozkład jednej pozycji porządkowej: liczba, odsetek i odsetek skumulowany.
rozklad_pozycji <- function(x) {
  stopifnot(is.numeric(x), all(is.na(x) | x %in% 1:5))
  liczba <- as.integer(table(factor(x, levels = 1:5)))
  n <- sum(liczba)
  data.frame(odpowiedz = 1:5, liczba = liczba, N = n, braki = sum(is.na(x)),
             procent = if (n) 100 * liczba / n else NA_real_,
             skumulowany = if (n) 100 * cumsum(liczba) / n else NA_real_)
}
pozycja_3_opis <- rozklad_pozycji(pozycje_surowe$pozycja_3)

# Sześć małych rekordów a–f, niezależnych od S02.
mini <- data.frame(
  osoba = letters[1:6],
  p1 = c(4, 4, 5, 1, 3, 5), p2 = c(5, 4, NA, 2, 3, 1),
  p3 = c(1, 2, 1, 5, 3, 1), p4 = c(4, 4, NA, 1, 3, 5),
  p5 = c(4, NA, 5, 2, 3, 1), p6 = c(5, 4, 5, 1, 3, 5)
)
mini_ukierunkowane <- mini
mini_ukierunkowane$p3 <- badaniaZI::odwroc_pozycje(mini$p3)
audyt_indeksu <- function(tabela, minimum = 5L) {
  stopifnot(all(paste0('p', 1:6) %in% names(tabela)))
  x <- tabela[paste0('p', 1:6)]
  data.frame(osoba = tabela$osoba, wazne = rowSums(!is.na(x)),
             suma_waznych = rowSums(x, na.rm = TRUE),
             indeks = badaniaZI::indeks_ankiety(x, minimum = minimum))
}
naglowki_audytu <- c('Osoba', 'Ważne pozycje m', 'Suma ważnych', 'Indeks I')
mini_wynik <- audyt_indeksu(mini_ukierunkowane)
kierunek_demo <- data.frame(Osoba = mini$osoba,
                            `Bez odwrócenia (błąd)` = badaniaZI::indeks_ankiety(mini[paste0('p', 1:6)]),
                            `Po odwróceniu p3` = mini_wynik$indeks, check.names = FALSE)
odwrocenie <- data.frame(`Odpowiedź x` = 1:5, `Po odwróceniu 6 − x` = badaniaZI::odwroc_pozycje(1:5),
                         check.names = FALSE)
liczba_odpowiedzi <- as.data.frame(table(factor(dane$liczba_pozycji, levels = 0:6)))
names(liczba_odpowiedzi) <- c('Ważne pozycje', 'Osoby')
porownaj_minimum <- function(minimum) {
  z <- badaniaZI::indeks_ankiety(pozycje, minimum = minimum)
  data.frame(`Minimum pozycji` = minimum, N = sum(!is.na(z)), `Braki indeksu` = sum(is.na(z)),
             `Średnia` = mean(z, na.rm = TRUE), SD = stats::sd(z, na.rm = TRUE), check.names = FALSE)
}
wrazliwosc_minimum <- rbind(porownaj_minimum(5L), porownaj_minimum(6L))

# Alfa Cronbacha i korelacje pozycji na osobach z kompletem sześciu odpowiedzi.
kompletne <- pozycje[stats::complete.cases(pozycje), ]
K <- ncol(kompletne)
wariancje_pozycji <- vapply(kompletne, stats::var, numeric(1))
wariancja_sumy <- stats::var(rowSums(kompletne))
alfa <- K / (K - 1) * (1 - sum(wariancje_pozycji) / wariancja_sumy)
korelacje <- stats::cor(kompletne)
r_srednia <- mean(korelacje[upper.tri(korelacje)])
alfa_std <- K * r_srednia / (1 + (K - 1) * r_srednia)

# Wykresy S02.
wykres_pozycji <- badaniaZI::wykres_czestosci(1:5, pozycja_3_opis$liczba,
  os = 'Oryginalna odpowiedź na pozycję 3 (1–5)', tytul = 'Pozycja 3: odczytanie rekordu wymaga zgadywania')
wykres_szesciu_pozycji <- badaniaZI::wykres_likert(pozycje, etykiety_pozycji,
  nazwy_kategorii = nazwy_odpowiedzi, tytul = 'Sześć pozycji S02 po ukierunkowaniu')
wykres_indeksu <- badaniaZI::wykres_histogram(dane$indeks, 0.25, poczatek = 1, jednostka = 'pkt',
  os = 'Indeks deklarowanej użyteczności [1–5]', tytul = 'Indeks po ukierunkowaniu pozycji i regule 5/6')
wykres_regul <- badaniaZI::wykres_histogram_panele(list(`Reguła 5/6` = dane$indeks, `Reguła 6/6` = indeks_6),
  0.25, poczatek = 1, srednia = TRUE, os = 'Indeks deklarowanej użyteczności [1–5]', os_y = 'Liczba osób',
  tytul = 'Indeks przy dwóch regułach kompletności')
wykres_korelacji <- badaniaZI::wykres_macierz_korelacji(pozycje, paste('Pozycja', 1:6),
  tytul = 'Korelacje sześciu pozycji S02')
kanaly_do_wykresu <- data.frame(kanal = factor(unname(nazwy_kanalow[kanaly$opcja]),
                                               levels = unname(nazwy_kanalow[kanaly$opcja])),
                                n = kanaly$liczba, N = kanaly$mianownik, procent = kanaly$procent_respondentow)
wykres_kanalow <- ggplot2::ggplot(kanaly_do_wykresu, ggplot2::aes(x = kanal, y = procent)) +
  ggplot2::geom_col(fill = kolory[['primary']], width = 0.6) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(n, ' z ', N, '\n', format(round(procent, 1), decimal.mark = ','), '%')),
                     vjust = -0.3, size = 3.2) +
  ggplot2::scale_x_discrete(labels = function(k) sub(' ', '\n', k)) +
  ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  ggplot2::labs(x = NULL, y = 'Procent kompletnych respondentów [%]',
                title = 'Kanały informacji wybrane przez respondentów S02',
                subtitle = paste0('N = ', kanaly$mianownik[1], ' kompletnych odpowiedzi; suma odsetków ',
                                  format(round(sum(kanaly$procent_respondentow), 1), decimal.mark = ','), '%')) +
  badaniaZI::theme_zi()
wykres_wspolnych <- badaniaZI::wykres_wspolne_wybory(30, 25, 50, c('e-mail', 'WWW'),
  tytul = 'Granice wspólnych wyborów e-maila i WWW')

# Dwa profile o tym samym indeksie 3; kształt punktu i typ linii rozróżniają osoby.
profile <- data.frame(pozycja = rep(1:6, 2),
  osoba = rep(c('Profil jednolity', 'Profil zróżnicowany'), each = 6),
  odpowiedz = c(3, 3, 3, 3, 3, 3, 1, 5, 3, 5, 1, 3))
wykres_profili <- ggplot2::ggplot(profile,
    ggplot2::aes(x = pozycja, y = odpowiedz, linetype = osoba, shape = osoba, group = osoba)) +
  ggplot2::geom_line(linewidth = 0.7, colour = kolory[['primary']]) +
  ggplot2::geom_point(size = 2.6, colour = kolory[['primary']]) +
  ggplot2::scale_x_continuous(breaks = 1:6) +
  ggplot2::scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  ggplot2::scale_linetype_manual(values = c('solid', 'dashed')) +
  ggplot2::scale_shape_manual(values = c(16, 17)) +
  ggplot2::labs(x = 'Numer ukierunkowanej pozycji', y = 'Odpowiedź', linetype = NULL, shape = NULL,
                title = 'Dwa profile o takim samym indeksie równym 3') +
  badaniaZI::theme_zi() + ggplot2::theme(legend.position = 'bottom')

# Kanały sześciu osób demonstracji; osoba 3 ma cztery zera, osoby 4 i 5 braki.
kanaly_demo <- data.frame(
  www = c(1, 0, 0, 1, NA, 1), email = c(0, 1, 0, NA, NA, 1),
  media = c(1, 0, 0, 0, NA, 0), kontakt = c(0, 0, 0, 0, NA, 1))
kanaly_demo_wynik <- badaniaZI::odpowiedzi_wielokrotne(kanaly_demo)

# Dwóch obserwatorów ocenia te same dziesięć prób: 1 = sukces, 0 = niepowodzenie.
oceny_obserwatorow <- data.frame(A = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0), B = c(1, 1, 1, 1, 1, 0, 1, 0, 0, 0))
p_o <- mean(oceny_obserwatorow$A == oceny_obserwatorow$B)
p_A <- mean(oceny_obserwatorow$A)
p_B <- mean(oceny_obserwatorow$B)
p_e <- p_A * p_B + (1 - p_A) * (1 - p_B)
kappa <- (p_o - p_e) / (1 - p_e)

# Syntetyczna ocena interfejsu cyfrowego archiwum; p3 ma przeciwny kierunek.
archiwum_pozycje <- data.frame(osoba = paste0('a', 1:8),
  p1 = c(4, 5, 3, 2, 4, 1, 5, 3), p2 = c(4, 4, 3, 2, NA, 2, 5, 3),
  p3 = c(2, 1, 3, 4, 2, 5, 1, 3), p4 = c(5, 5, 3, 2, 4, 1, NA, 3),
  p5 = c(4, 4, 3, 1, NA, 2, 5, 3), p6 = c(4, 5, 3, 2, 5, 1, 5, 3))
archiwum_kierunek <- archiwum_pozycje
archiwum_kierunek$p3 <- badaniaZI::odwroc_pozycje(archiwum_pozycje$p3)
archiwum_indeks <- audyt_indeksu(archiwum_kierunek)
archiwum_porownanie <- data.frame(osoba = archiwum_pozycje$osoba,
  bez_odwrocenia = audyt_indeksu(archiwum_pozycje)$indeks,
  poprawny = archiwum_indeks$indeks,
  przy_minimum_6 = audyt_indeksu(archiwum_kierunek, 6L)$indeks)
archiwum_kanaly <- data.frame(www = c(1, 1, 0, 0, 1, NA, 0, 1),
  email = c(1, 0, 0, 1, NA, NA, 0, 1), media = c(0, 1, 0, 0, 0, NA, 1, 0),
  kontakt = c(0, 0, 0, 1, 0, NA, 0, 0))
archiwum_kanaly_opis <- badaniaZI::odpowiedzi_wielokrotne(archiwum_kanaly)
archiwum_pomiar <- data.frame(osoba = archiwum_pozycje$osoba,
  indeks = archiwum_indeks$indeks,
  czas_min = c(6, 12, 8, 4, 9, 3, 15, 7), sukces = c(1, 0, 1, 0, 1, 0, 1, 1))

# Dodatkowa obserwacja w tej samej małej demonstracji a–f.
mini_pomiar <- data.frame(osoba = mini$osoba, indeks = mini_wynik$indeks,
  czas_min = c(6, 12, 8, 4, 10, 7), powodzenie = c(1, 0, 1, 0, 1, 1))

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

# Funkcje bloków: jedna funkcja wyświetla jeden wynik z polskimi nagłówkami.
opis_pozycji <- function(opis, tytul) {
  tab <- opis[c('odpowiedz', 'liczba', 'procent', 'skumulowany')]
  names(tab) <- c('Odpowiedź', 'Liczba', 'Procent [%]', 'Skumulowany [%]')
  pokaz_tabele(tab, tytul, 1L)
}
odpowiedzi_osob <- function(tabela, tytul) {
  tab <- tabela
  names(tab)[1] <- 'Osoba'
  pokaz_tabele(tab, tytul, 0L)
}
indeks_osob <- function(wynik, tytul) {
  tab <- wynik
  names(tab) <- naglowki_audytu
  pokaz_tabele(tab, tytul, 2L)
}
podglad_indeksu <- function() {
  tab <- utils::head(dane[c('id_odpowiedzi', 'liczba_pozycji', 'indeks')], 8)
  names(tab) <- c('ID', 'Ważne pozycje', 'Indeks [1–5]')
  pokaz_tabele(tab, 'Pierwsze osiem rekordów S02', 2L)
}
dane_kanalow <- function(tabela, osoby = seq_len(nrow(tabela))) {
  tab <- cbind(Osoba = osoby, tabela)
  names(tab)[-1] <- unname(nazwy_kanalow[names(tabela)])
  pokaz_tabele(tab, 'Wybory osób: 0, 1 oraz brak', 0L)
}
opis_kanalow <- function(tabela) {
  stopifnot(length(unique(tabela$mianownik)) == 1L, length(unique(tabela$pominiete_niepelne)) == 1L)
  suma_wskazan <- sum(tabela$liczba)
  bilans <- data.frame(Miara = c('Kompletni respondenci', 'Pominięte niepełne odpowiedzi', 'Suma wskazań'),
                       Liczba = c(tabela$mianownik[1], tabela$pominiete_niepelne[1], suma_wskazan))
  tab <- data.frame(Kanal = unname(nazwy_kanalow[tabela$opcja]), Wybrali = tabela$liczba,
                    osoby = tabela$procent_respondentow,
                    wskazania = if (suma_wskazan > 0) 100 * tabela$liczba / suma_wskazan else NA_real_)
  names(tab) <- c('Kanał', 'Wybrali', 'Osoby [%]', 'Wskazania [%]')
  wydruk_zi(tabele = list('Mianowniki i kompletność' = bilans,
    'Odsetki osób oraz udział w puli wskazań' = tab), digits = 1L)
}
tabela_zgodnosci <- function() {
  tab <- data.frame(Obserwator_A = c('A: sukces', 'A: niepowodzenie'),
    sukces = c(sum(oceny_obserwatorow$A == 1 & oceny_obserwatorow$B == 1),
               sum(oceny_obserwatorow$A == 0 & oceny_obserwatorow$B == 1)),
    niepowodzenie = c(sum(oceny_obserwatorow$A == 1 & oceny_obserwatorow$B == 0),
                      sum(oceny_obserwatorow$A == 0 & oceny_obserwatorow$B == 0)))
  names(tab) <- c('Ocena A', 'B: sukces', 'B: niepowodzenie')
  pokaz_tabele(tab, 'Zgodność dwóch obserwatorów w dziesięciu próbach', 0L)
}
deklaracja_i_obserwacja <- function() {
  tab <- mini_pomiar
  names(tab) <- c('Osoba', 'Indeks [1–5]', 'Czas [min]', 'Powodzenie (0/1)')
  pokaz_tabele(tab, 'Dwa pomiary tych samych osób a–f', 2L)
}
alfa_cronbacha <- function() {
  tab <- data.frame(Miara = c('Osoby z kompletem pozycji', 'Liczba pozycji K', 'Suma wariancji pozycji',
                              'Wariancja sumy pozycji', 'Alfa Cronbacha', 'Średnia korelacja pozycji',
                              'Alfa standaryzowana'),
                    `Wartość` = c(nrow(kompletne), K, sum(wariancje_pozycji), wariancja_sumy, alfa,
                                  r_srednia, alfa_std), check.names = FALSE)
  pokaz_tabele(tab, 'Alfa Cronbacha sześciu pozycji S02', 3L)
}
opis_pozycji_archiwum <- function() {
  tab <- archiwum_pozycje
  names(tab)[1] <- 'Osoba'
  opis <- rozklad_pozycji(archiwum_pozycje$p2)[c('odpowiedz', 'liczba', 'N', 'braki', 'procent')]
  names(opis) <- c('Odpowiedź', 'Liczba', 'N', 'Braki', 'Procent [%]')
  p2 <- archiwum_pozycje$p2[!is.na(archiwum_pozycje$p2)]
  zgoda <- data.frame(Odpowiedzi = '4 lub 5 (zgoda)', Liczba = sum(p2 >= 4), N = length(p2),
    `Procent [%]` = 100 * mean(p2 >= 4), check.names = FALSE)
  wydruk_zi(tabele = list('Pierwotne pozycje cyfrowego archiwum' = tab,
    'Rozkład p2: etykiety filtrów są zrozumiałe' = opis, 'Zgoda z p2' = zgoda), digits = 1L)
}
konstrukcja_indeksu_archiwum <- function() {
  audyt <- archiwum_indeks
  names(audyt) <- naglowki_audytu
  porownanie <- archiwum_porownanie[c('osoba', 'bez_odwrocenia', 'poprawny')]
  names(porownanie) <- c('Osoba', 'Bez odwrócenia', 'Po odwróceniu')
  a1 <- rbind(archiwum_pozycje[1, -1], archiwum_kierunek[1, -1])
  a1 <- cbind(Zapis = c('Pierwotne odpowiedzi a1', 'Po odwróceniu p3 (6 − x)'), a1)
  wydruk_zi(tabele = list('Rekord a1 przed odwróceniem p3 i po nim' = a1, 'Audyt poprawnego indeksu' = audyt,
    'Błędny oraz poprawny kierunek pozycji' = porownanie), digits = 2L)
}
kompletnosc_archiwum <- function() {
  tab <- archiwum_porownanie[c('osoba', 'poprawny', 'przy_minimum_6')]
  names(tab) <- c('Osoba', 'Minimum 5', 'Minimum 6')
  bilans <- data.frame(`Minimum pozycji` = c(5, 6),
                       `Ważne indeksy` = c(sum(!is.na(tab[[2]])), sum(!is.na(tab[[3]]))), check.names = FALSE)
  wydruk_zi(tabele = list('Wynik każdej osoby w dwóch wariantach' = tab,
    'Liczby ważnych indeksów' = bilans), digits = 2L)
}
kanaly_pomocy_archiwum <- function() {
  wynik <- opis_kanalow(archiwum_kanaly_opis)
  surowe <- cbind(Osoba = archiwum_pozycje$osoba, archiwum_kanaly)
  names(surowe)[-1] <- unname(nazwy_kanalow[names(archiwum_kanaly)])
  wynik$tabele <- c(list('Surowe wybory kanałów' = surowe), wynik$tabele)
  wynik
}
pomiar_archiwum <- function() {
  tab <- archiwum_pomiar
  names(tab) <- c('Osoba', 'Indeks [1–5]', 'Czas [min]', 'Sukces (0/1)')
  pokaz_tabele(tab, 'Indeks deklaracji i wynik zadania', 2L)
}
