\name{wykres_rozklad_dyskretny}
\alias{wykres_rozklad_dyskretny}
\title{Rozklad dyskretny: prawdopodobienstwa i dystrybuanta}
\usage{
wykres_rozklad_dyskretny(
  k,
  p,
  os = "Liczba sukcesów k",
  tytul = NULL,
  zaznacz = NULL,
  dystrybuanta = TRUE
)
}
\arguments{
\item{k}{Wartosci zmiennej losowej.}

\item{p}{Prawdopodobienstwa wartosci k.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}

\item{zaznacz}{Opcjonalne wartosci k wyroznione kolorem slupka; podtytul
podaje ich laczne prawdopodobienstwo.}

\item{dystrybuanta}{Czy dodac panel dystrybuanty.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Lewy panel: slupki P(K = k) z wartosciami; prawy panel: schodkowa
dystrybuanta F(k) = P(K <= k) z punktami przy kazdym k.
}
\examples{
p <- wykres_rozklad_dyskretny(0:3, stats::dbinom(0:3, 3, 0.5))
}
