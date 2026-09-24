\name{wykres_selekcja}
\alias{wykres_selekcja}
\title{Sklad grup w populacji i w probie odpowiadajacych}
\usage{
wykres_selekcja(
  grupy,
  populacja,
  odpowiedzi,
  srednie,
  cyfry = 1L,
  tytul = NULL
)
}
\arguments{
\item{grupy}{Nazwy grup.}

\item{populacja}{Liczebnosci grup w populacji.}

\item{odpowiedzi}{Liczby odpowiedzi w grupach.}

\item{srednie}{Srednie grupowe (te same w populacji i wsrod odpowiadajacych).}

\item{cyfry}{Liczba miejsc po przecinku w srednich.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Dwa slupki skumulowane przedstawiaja udzialy grup [%] w populacji i wsrod
osob, ktore odpowiedzialy. Napis w segmencie podaje udzial i srednia grupy,
a podpis slupka srednia calosci wazona liczebnosciami: przy tych samych
srednich grupowych srednia calosci zmienia sie wraz ze skladem.
}
\examples{
p <- wykres_selekcja(c("Nowi", "Doswiadczeni"), c(700, 300), c(20, 80), c(3, 4))
}
