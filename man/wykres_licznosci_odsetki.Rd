\name{wykres_licznosci_odsetki}
\alias{wykres_licznosci_odsetki}
\title{Liczebnosci i odsetki w grupach o roznych mianownikach}
\usage{
wykres_licznosci_odsetki(
  grupy,
  licznik,
  mianownik,
  zdarzenie = "sukcesy",
  tytul = NULL
)
}
\arguments{
\item{grupy}{Nazwy grup.}

\item{licznik}{Liczby zdarzen w grupach.}

\item{mianownik}{Liczebnosci grup.}

\item{zdarzenie}{Nazwa zdarzenia w liczbie mnogiej, np. "sukcesy".}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Dwa panele: liczba zdarzen (np. sukcesow) i odsetek zdarzen w grupie.
Napisy podaja licznik z mianownikiem oraz odsetek.
}
\examples{
p <- wykres_licznosci_odsetki(c("Nowi", "Doswiadczeni"), c(16, 60), c(20, 100))
}
