\name{wykres_precyzja}
\alias{wykres_precyzja}
\title{Polowa szerokosci przedzialu ufnosci roznicy srednich wobec liczebnosci}
\usage{
wykres_precyzja(
  sd,
  n,
  poziom = 0.95,
  os = "Połowa szerokości CI",
  tytul = NULL
)
}
\arguments{
\item{sd}{Robocze odchylenie standardowe w obu grupach.}

\item{n}{Liczebnosci jednej grupy opisane punktami.}

\item{poziom}{Poziom przedzialu.}

\item{os}{Podpis osi polowy szerokosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywa z * SD * pierwiastek(2 / n) dla dwoch rownych, niezaleznych grup
o wspolnym odchyleniu standardowym; punkty z napisami oznaczaja wskazane
liczebnosci grupy.
}
\examples{
p <- wykres_precyzja(0.8, c(25, 50, 100, 200))
}
