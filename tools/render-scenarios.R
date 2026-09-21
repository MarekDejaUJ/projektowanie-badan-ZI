# Opisy scenariuszy powstają z tej samej karty co słownik i wydruk analiz.
# Przykład: Rscript tools/render-scenarios.R S10 S18; bez argumentów: cały katalog.
katalog <- yaml::read_yaml('inst/scenariusze/katalog.yml',eval.expr=FALSE)
ids <- toupper(commandArgs(trailingOnly=TRUE))
wszystkie <- vapply(katalog$scenariusze,function(x) x$id,character(1))
stopifnot(identical(wszystkie,sprintf('S%02d',1:20)))
if(!length(ids)) ids <- wszystkie
stopifnot(!anyDuplicated(ids),all(ids %in% wszystkie))
zapisz <- function(txt,path) {
  con <- file(path,open='wb')
  on.exit(close(con),add=TRUE)
  writeLines(enc2utf8(txt),con,useBytes=TRUE)
}
for(id in ids) {
  s <- katalog$scenariusze[[match(id,wszystkie)]]
  wymagane <- c('wersja_opisu','nazwa_indeksu','etykiety_grup','opis_grup',
    'zadanie_obserwacyjne','kontekst_pozycji','rodzaj_binarnego','kod_0','kod_1',
    'ograniczenie_binarnego','nazwa_czasu','protokol_czasu','opis_czestosci','inspiracja')
  stopifnot(all(wymagane %in% names(s)),length(s$pozycje)==6L,
    length(s$grupy)==2L,length(s$etykiety_grup)==2L,
    s$rodzaj_binarnego %in% c('obserwacja','deklaracja'))
  etykiety <- unlist(s$etykiety_grup)
  kontrast <- paste(etykiety[1],'minus',etykiety[2])
  binarne <- s$druga_analiza=='tabela krzyżowa'
  stopifnot(s$zmienna_druga %in% if(binarne) 'powodzenie' else
    c('czestosc_korzystania','czas_wyszukiwania'))
  md <- c(paste('#',id,'—',s$tytul),'',
    paste('Wersja opisu:',s$wersja_opisu,'. Dane do analizy są syntetyczne.'),'',
    s$problem,'','## Plan badania','',
    'Na C09 wybierasz scenariusz i zapisujesz pięć odpowiedzi w swoim pełnym Rmd. Określ populację, okres, ramę dotarcia, sposób zaproszenia i możliwy mechanizm selekcji. Zaplanuj ograniczenie zbierania informacji identyfikujących osoby. Jest to plan przyszłego badania: na potrzeby tego projektu nie rekrutujesz uczestników ani nie zbierasz ich danych.','',
    paste('Grupy:',paste(etykiety,collapse=' / '),'.',s$opis_grup),'',
    s$projekt_badania,'',
    paste('Indeks:',s$nazwa_indeksu,
      '. To wynik z sześciu deklarowanych ocen, nie test wszystkich kompetencji ani niezależny audyt usługi.'),'',
    paste('Pierwsze pytanie: jaka jest różnica średnich indeksu w porównaniu',
      paste0('„',kontrast,'”?'),
      'Estymandą jest różnica średnich w populacjach określonych w planie; estymatę obliczamy z dostępnej próby. Jednostka to punkt indeksu. H0 zakłada różnicę równą zero.'),'',
    paste('Druga relacja:',paste0(s$pytanie_drugie,'.')),
    if(binarne) paste('Drugie pytanie dotyczy różnicy prawdopodobieństw kodu 1:',
      paste0('„',kontrast,'”.'),
      'H0 zakłada równe prawdopodobieństwa. Zanim nazwiesz odsetek sukcesem, przeczytaj definicję kodów poniżej.') else
      paste('Drugie pytanie dotyczy monotonicznego związku indeksu z',
        paste0('`',s$zmienna_druga,'`.'),
        'Parametrem jest korelacja rangowa Spearmana; H0 w przybliżonym teście dotyczy rho=0. Prosty test tasowania wymaga ponadto niezależności i wymienialności par pod przyjętym modelem, a nie jedynie zerowej korelacji.'),'',
    'Możesz doprecyzować adresatów i warunki przyszłego badania. Zachowaj znaczenie dostarczonych zmiennych, sześć pozycji i dwie relacje rdzenia. Jeśli proponujesz zmianę pomiaru, nazwij ją planem przyszłej weryfikacji; nie udawaj, że nowy pomiar znajduje się już w wygenerowanych danych.','',
    '## Proponowany protokół i gotowy kwestionariusz','',
    paste('Zadanie:',s$zadanie_obserwacyjne),'',
    paste('Czas:',s$nazwa_czasu,'.',s$protokol_czasu),'',
    'Zakres 0–120 minut jest techniczną regułą kontroli tego zbioru, nie ustalonym limitem powodzenia. W realnym planie rozróżnij zakończoną próbę, przerwę zewnętrzną, utratę zapisu i osiągnięcie limitu. Osiągnięcie limitu może oznaczać pomiar ucięty; nie wolno bez uzasadnienia traktować go jak zwykłego czasu zakończenia, zera ani automatycznie usuwać. Gotowe dane i analizy nie obejmują modelu takich czasów.','',
    s$kontekst_pozycji,'',
    'Dla każdej pozycji: 1 — zdecydowanie nie, 2 — raczej nie, 3 — ani tak, ani nie, 4 — raczej tak, 5 — zdecydowanie tak. Brak nie jest oceną 3 ani oceną negatywną.','',
    paste0(seq_along(s$pozycje),'. ',unlist(s$pozycje)),'',
    paste('Pole `powodzenie` zachowuje wspólną nazwę techniczną. Treść pytania:',
      paste0('„',s$pytanie_binarne,'”')),'',
    paste('Źródło wartości:',if(s$rodzaj_binarnego=='obserwacja')
      'zapis obserwatora według podanego kryterium.' else
      'odpowiedź ankietowa — deklaracja respondenta, nie obserwacja wykonania.'),'',
    paste('Kod 1:',paste0(s$kod_1,'.')),
    paste('Kod 0:',paste0(s$kod_0,'.')),
    'NA oznacza nieznany wynik lub brak odpowiedzi, a nie kod 0.','',
    s$ograniczenie_binarnego,'',s$opis_czestosci,'',
    paste('Kanały informacji — deklarowane użycie w ostatnich czterech tygodniach:',
      paste(unlist(s$kanaly),collapse=', '),
      '. Można wskazać kilka. Cztery zera oznaczają brak wskazań; pominięte pytanie oznacza cztery braki. Procent respondentów ma mianownik osób z kompletem odpowiedzi na to pytanie, nie wszystkich zaznaczeń.'),
    'Odpowiedzi wielokrotne i pole grupa są osobnymi pomiarami. Nie wyznaczaj grup automatycznie z liczby zaznaczeń.','',
    '## Dane, reguły i dwie analizy','',
    'Korzystasz z ID wpisanego na początku sesji, nie tworzysz nowego pseudonimu dla scenariusza. Na C09 wybierasz kod we wskazanym wywołaniu gotowej funkcji. Wariant ma być zapisany z danymi surowymi, słownikiem, kartą i manifestem. Na C10 oraz w raporcie kontynuujesz ten sam wariant; nie losujesz nowego zbioru po obejrzeniu wyniku ani po zmianie komputera.','',
    'Domyślny zestaw ma 150 surowych rekordów, w tym dwa identyczne powtórzenia. Gotowy skrypt odtwarza przygotowanie: usuwa wyłącznie identyczne powtórzenia, rozpoznaje udokumentowane kody braków oraz oznacza czasy spoza 0–120 jako NA. Długi ważny czas pozostaje w analizie. Odczytaj rzeczywisty dziennik i N w swoim wariancie; zmiana komórki na NA nie usuwa całej osoby.','',
    s$regula_indeksu,'',
    'Uruchamiasz gotowe chunki, czytasz tabele i wpisujesz własne krótkie akapity poza kodem. Nie programujesz czyszczenia ani testów. W każdej analizie podajesz właściwe N, wielkość efektu i niepewność, a nie tylko etykietę istotności.','',
    paste('Pierwsza analiza porównuje indeks:',paste0(kontrast,'.'),
      'Odczytaj N i średnie grup, różnicę, SE, t Welcha, df i p. Podaj 95% CI różnicy Welcha i bootstrapu osób w grupach oraz Hedgesa g jako efekt bez jednostki. Przedział różnicy w punktach nie jest przedziałem dla g. Porównaj z p po tasowaniu etykiet przy wymienialności pod modelem zerowym; podaj B i wyjaśnij ocenę skrajności.'),'',
    if(binarne) paste('Druga analiza używa grupy i pola 0/1 w znaczeniu podanym w tej karcie. Odczytaj liczebności, N i procenty w grupach, oczekiwane liczebności oraz uzasadnienie chi-kwadrat albo Fishera. Podaj p klasyczne i p Monte Carlo przy stałych marginesach, k i B. Efekt kierunkowy to różnica proporcji kodu 1,',
      paste0(kontrast,','),
      'z 95% CI w punktach procentowych. V Cramera opisuje siłę związku bez znaku; nie stosuj jego percentyli bootstrapowych jako automatycznego testu zera. Przy rzadkich komórkach dużopróbkowe przybliżenie CI różnicy może być słabe; rozdziel je od dokładnego CI ilorazu szans Fishera.') else
      paste('Druga analiza wykorzystuje kompletne pary indeksu i',
        paste0('`',s$zmienna_druga,'`.'),
        'Odczytaj wykres, N par, rho Spearmana, statystykę S, przybliżone p klasyczne, 95% CI bootstrapu całych par oraz p z tasowania jednej zmiennej. Podaj k i B. Nie dopisuj df z Pearsona ani jednostki minut do współczynnika rangowego. Korelacja nie dowodzi przyczyny.'),'',
    'Klasyczne i losowane wyniki jednej relacji to dwa sposoby wnioskowania, nie dwie osobne analizy merytoryczne. Przy Monte Carlo odczytaj (k+1)/(B+1). Nie zwiększa to liczby osób. Przedział obejmujący zero nie jest dowodem równości; rozbieżność procedur wymaga odczytu ich założeń, nie wyboru wygodniejszego p.','',
    '## Wynik pracy','',
    'Z09 zachowuje plan i pierwszy opis; Z10 dodaje dwie analizy oraz własną interpretację. Punktem wyjścia raportu są twoje odpowiedzi z obu spotkań, które dopracowujesz w jednej całości bez ponownego przepisywania ich do dodatkowych formularzy.','',
    'Oddaj jeden projekt indywidualny: `raport.Rmd` i utworzony z niego PDF, gotowy `analiza.R` oraz zapisane surowe dane, słownik, manifest i kartę scenariusza. Plan pomiaru, tabele, dwa wymagane wykresy i własne akapity znajdują się w raporcie; nie wypełniasz osobnego formularza kwestionariusza. Zapisz pracę przed wywołaniem funkcji oddania w konsoli. Potwierdzenie zawiera zdalne SHA oddanej wersji; samo zapisanie PDF lub lokalny commit nie jest odbiorem pracy.','',
    'Obowiązuje wspólna rubryka projektu: plan i pomiar 30 pkt, przygotowanie danych 15 pkt, opis 10 pkt, dwie analizy 20 pkt, wnioski 15 pkt, odtwarzalność 10 pkt. Nie ma limitu słów. Wynik nieistotny pozwala uzyskać pełną ocenę.','',
    '## Literatura i granice wnioskowania','',
    s$inspiracja,'',s$ograniczenia,'',
    'Wyniki symulacji nie są wynikami cytowanej publikacji ani dowodem o rzeczywistej instytucji. Różnica między grupami nie dowodzi działania interwencji. Rekomendację formułuj warunkowo jako propozycję sprawdzenia w rzeczywistym badaniu, uwzględniając pomiar, dobór i niepewność.')
  md <- gsub(' +\\.', '.', md)
  path <- file.path('inst/scenariusze',paste0(id,'.md'))
  zapisz(md,path)
  rmarkdown::render(path,output_format=rmarkdown::html_document(self_contained=TRUE,
    mathjax=NULL,pandoc_args=c('--metadata',paste0('pagetitle=',id,' — ',s$tytul))),
    quiet=TRUE,envir=new.env(parent=globalenv()))
  rmarkdown::render(path,output_format=rmarkdown::pdf_document(latex_engine='xelatex',
    pandoc_args=c('-V','geometry:margin=2cm','-V','fontsize:11pt','-V','papersize:a4')),
    quiet=TRUE,envir=new.env(parent=globalenv()))
  cat(id,': karta → MD/HTML/PDF OK\n')
}
