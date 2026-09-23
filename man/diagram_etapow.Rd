\name{diagram_etapow}
\alias{diagram_etapow}
\title{Diagram etapow rozumowania badawczego}
\usage{
diagram_etapow(etapy, opisy = NULL, grupy = NULL, tytul = NULL)
}
\arguments{
\item{etapy}{Nazwy etapow w kolejnosci rozumowania.}

\item{opisy}{Krotkie pytania lub opisy etapow; domyslnie puste.}

\item{grupy}{Nazwy grup etapow; domyslnie jedna grupa bez nazwy.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot.
}
\description{
Rysuje etapy od gory do dolu, polaczone strzalkami. Kazdy etap moze miec
krotkie pytanie oraz grupe (np. plan badania i analiza danych), zaznaczona
odcieniem tla i nazwa w nawiasie.
}
\examples{
diagram_etapow(c("Problem", "Pomiar", "Wniosek"),
  c("Jaka decyzja?", "Co obserwujemy?", "Co wolno stwierdzic?"))
}
