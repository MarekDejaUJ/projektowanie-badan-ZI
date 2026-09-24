\name{wykres_histogram_panele}
\alias{wykres_histogram_panele}
\title{Histogramy w panelach o wspolnej osi}
\usage{
wykres_histogram_panele(
  dane,
  szerokosc,
  poczatek = NULL,
  os = "Wartość",
  srednia = FALSE,
  tytul = NULL,
  odniesienie = NULL,
  etykieta_odniesienia = "parametr",
  os_y = NULL
)
}
\arguments{
\item{dane}{Nazwana lista wektorow liczbowych.}

\item{szerokosc}{Szerokosc przedzialu; jedna liczba albo wartosc na panel.}

\item{poczatek}{Poczatek pierwszego przedzialu; domyslnie wielokrotnosc
szerokosci ponizej minimum.}

\item{os}{Podpis osi wartosci.}

\item{srednia}{Czy oznaczyc srednia panelu linia przerywana.}

\item{tytul}{Tytul wykresu.}

\item{odniesienie}{Opcjonalna wartosc odniesienia (np. parametr populacji)
oznaczona we wszystkich panelach linia kropkowana z opisem.}

\item{etykieta_odniesienia}{Opis linii odniesienia.}

\item{os_y}{Podpis osi liczebnosci; domyslnie liczba osob.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazdy element listy tworzy jeden panel; nazwa elementu i N sa tytulem
panelu. Szerokosc przedzialu moze byc wspolna albo rozna dla paneli, co
pozwala porownac ten sam zbior przy kilku szerokosciach.
}
\examples{
p <- wykres_histogram_panele(list(A = c(1, 2, 2, 3), B = c(1, 1, 4, 4)), 1)
}
