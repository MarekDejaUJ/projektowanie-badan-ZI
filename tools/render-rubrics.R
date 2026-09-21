# Budowanie wersji studenckich z jednego źródła rubryki YAML.
# Przykład: Rscript tools/render-rubrics.R PROJEKT Z10
ids <- toupper(commandArgs(trailingOnly=TRUE))
dozwolone <- c(sprintf('Z%02d',1:10),'PROJEKT')
if(!length(ids)) ids <- dozwolone
stopifnot(all(ids %in% dozwolone),!anyDuplicated(ids))
tekst_tabeli <- function(x) {
  x <- gsub('\n',' ',x,fixed=TRUE)
  gsub('|','\\|',x,fixed=TRUE)
}
write_md <- function(rubryka,path) {
  md <- c(paste0('# Rubryka ',rubryka$id),'',
    paste('Wersja',rubryka$version,'— rocznik',rubryka$rocznik),'',
    paste('Maksimum:',rubryka$max_points,
      'punktów. Wagi i terminy określa konfiguracja rocznika.'),'',
    'Kontrola techniczna sprawdza pliki i odtworzenie; dobór, interpretację i uzasadnienie ocenia osoba prowadząca. Brak istotności nie obniża punktacji. Nie wymaga się napisania własnego programu analiz ani osiągnięcia limitu słów.')
  if(rubryka$id!='PROJEKT') md <- c(md,'',
    'Dowodem jest pięć odpowiedzi CHALLENGE (S01–S05) wpisanych poza chunkami do tego samego Rmd, jego wyniki i zgodny PDF. Pola przypisane do kryterium wskazują, gdzie szukać dowodu; nie są dodatkowymi zadaniami. LEARN dostarcza przykładów i nie wymaga kolejnych odpowiedzi do oceny.')
  for(k in rubryka$criteria) {
    md <- c(md,'',paste0('## ',k$id,' — ',k$max_points,' pkt'),'',k$title,'')
    if(!identical(k$evidence,k$title)) md <- c(md,paste('Dowód:',k$evidence),'')
    if(length(k$answer_fields)) md <- c(md,
      paste('Pola odpowiedzi:',paste(k$answer_fields,collapse=', ')), '')
    md <- c(md,'| Punkty | Obserwowalny dowód |','|--------:|---------------------------------------------------------------|',
      vapply(k$levels,function(l) paste0('| ',l$points,' | ',
        tekst_tabeli(l$observable_description),' |'),character(1)))
  }
  con <- file(path,open='wb')
  on.exit(close(con),add=TRUE)
  writeLines(enc2utf8(md),con,useBytes=TRUE)
}
for(id in ids) {
  path <- file.path('inst','rubryki',paste0(id,'.yml'))
  rub <- yaml::read_yaml(path,eval.expr=FALSE)
  stopifnot(identical(rub$id,id),length(rub$criteria)==if(id=='PROJEKT') 7L else 4L,
    sum(vapply(rub$criteria,function(k) k$max_points,numeric(1)))==rub$max_points)
  for(k in rub$criteria) {
    stopifnot(identical(as.numeric(vapply(k$levels,function(l) l$points,numeric(1))),
      k$max_points*c(0,.25,.5,.75,1)),length(unique(vapply(k$levels,
        function(l) l$observable_description,character(1))))==5L)
    if(id!='PROJEKT') stopifnot(length(k$answer_fields)>0L,
      !anyDuplicated(k$answer_fields), all(k$answer_fields %in% sprintf('S%02d',1:5)))
  }
  if(id!='PROJEKT') stopifnot(setequal(unlist(lapply(rub$criteria,
    function(k) k$answer_fields)),sprintf('S%02d',1:5)))
  md <- file.path('inst','rubryki',paste0(id,'.md'))
  write_md(rub,md)
  rmarkdown::render(md,output_format=rmarkdown::html_document(self_contained=TRUE,
    mathjax=NULL,pandoc_args=c('--metadata',paste0('pagetitle=Rubryka ',id))),
    quiet=TRUE,envir=new.env(parent=globalenv()))
  rmarkdown::render(md,output_format=rmarkdown::pdf_document(latex_engine='xelatex',
    pandoc_args=c('-V','geometry:margin=2cm','-V','fontsize:11pt','-V','papersize:a4')),
    quiet=TRUE,envir=new.env(parent=globalenv()))
  cat(id,': YAML → MD/HTML/PDF OK\n')
}
