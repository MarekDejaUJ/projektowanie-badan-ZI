\name{tabela_markdown}
\alias{tabela_markdown}
\title{Tabela Markdown z zawijaniem tekstu}
\usage{
tabela_markdown(x, szerokosci, podpis)
}
\arguments{
\item{x}{Ramka danych.}

\item{szerokosci}{Wzgledne szerokosci kolumn, np. c(1, 3, 2).}

\item{podpis}{Podpis tabeli.}
}
\value{
Obiekt knit_asis z tekstem tabeli.
}
\description{
Tworzy tabele w skladni pipe z podpisem. Liczba kresek w linii naglowka
wyznacza wzgledne szerokosci kolumn, wiec dlugi tekst zawija sie w PDF
i w HTML. Wynik drukuje sie w dokumencie knitr jako gotowy Markdown.
}
\examples{
tabela_markdown(data.frame(Kod = "W01", Opis = "Wyklad"), c(1, 4), "Przyklad")
}
