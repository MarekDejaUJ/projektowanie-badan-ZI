\name{wykres_bledy_testu}
\alias{wykres_bledy_testu}
\title{Bledy decyzji testu: alfa, beta i moc}
\usage{
wykres_bledy_testu(przesuniecie, df, alfa = 0.05, tytul = NULL)
}
\arguments{
\item{przesuniecie}{Srodek rozkladu statystyki przy H1 (np. d razy
pierwiastek z n).}

\item{df}{Stopnie swobody rozkladu t.}

\item{alfa}{Poziom istotnosci testu dwustronnego.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Krzywa ciagla: rozklad statystyki t przy H0 (srodek 0); krzywa
przerywana: rozklad przy H1 przesuniety o `przesuniecie`. Pole alfa lezy
w ogonach H0 poza wartosciami krytycznymi, pole beta pod krzywa H1
miedzy wartosciami krytycznymi. Podtytul podaje alfa, beta i moc.
}
\examples{
p <- wykres_bledy_testu(3, 99)
}
