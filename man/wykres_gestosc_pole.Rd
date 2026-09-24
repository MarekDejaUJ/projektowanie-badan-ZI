\name{wykres_gestosc_pole}
\alias{wykres_gestosc_pole}
\title{Gestosc z polem miedzy dwiema wartosciami}
\usage{
wykres_gestosc_pole(
  gestosc,
  od,
  do,
  a = NULL,
  b = NULL,
  linie = NULL,
  etykiety_linii = NULL,
  os = "Wartość",
  os_y = "Gęstość",
  tytul = NULL
)
}
\arguments{
\item{gestosc}{Funkcja gestosci jednej zmiennej.}

\item{od, do}{Zakres osi.}

\item{a, b}{Granice wyroznionego pola; NULL pomija pole.}

\item{linie}{Opcjonalne polozenia pionowych linii.}

\item{etykiety_linii}{Opisy linii.}

\item{os}{Podpis osi wartosci.}

\item{os_y}{Podpis osi gestosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Rysuje krzywa gestosci na zadanym zakresie, wyroznia pole miedzy a i b
(podtytul podaje jego wartosc) i opcjonalnie pionowe linie z opisem.
}
\examples{
p <- wykres_gestosc_pole(function(x) stats::dexp(x, 0.1), 0, 40, a = 5, b = 10)
}
