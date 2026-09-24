\name{wykres_kalibracja}
\alias{wykres_kalibracja}
\title{Kalibracja deklaracji wzgledem wyniku}
\usage{
wykres_kalibracja(deklaracja, wynik, jednostka = "\%", tytul = NULL)
}
\arguments{
\item{deklaracja, wynik}{Pomiary w tej samej jednostce.}

\item{jednostka}{Jednostka w podpisach osi.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Lewy panel: deklaracja wobec wyniku z linia zgodnosci y = x (przerywana).
Prawy panel (Bland-Altman): roznica deklaracja minus wynik wobec sredniej
obu pomiarow; linia ciagla to sredni blad, linie kropkowane to granice
zgodnosci sredni blad +/- 1,96 SD roznic.
}
\examples{
p <- wykres_kalibracja(c(40, 50, 60), c(20, 30, 40))
}
