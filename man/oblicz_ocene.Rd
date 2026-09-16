\name{oblicz_ocene}
\alias{oblicz_ocene}
\title{Wynik tej realizacji kursu}
\usage{
oblicz_ocene(
  punkty_zadan,
  punkty_projektu,
  spozniony = FALSE,
  konfiguracja = konfiguracja_kursu()
)
}
\arguments{
\item{punkty_zadan}{Dziesięć ocen Z01--Z10 od 0 do 10; NA oznacza brak oceny.}

\item{punkty_projektu}{Ocena projektu od 0 do 100 lub NA.}

\item{spozniony}{Czy pierwsze kwalifikujące się oddanie projektu było po terminie.}

\item{konfiguracja}{Konfiguracja rocznika z wagami i progami.}
}
\value{
Lista: stan, wynik procentowy i ocena; przy braku ocen wynik i ocena są NA.
}
\description{
Dziesięć zadań ma jednakową wagę w części zadaniowej. Za spóźniony projek
maksymalna ocena końcowa wynosi 4,5. Brak punktacji nie oznacza zera.
}
\examples{
oblicz_ocene(rep(8, 10), 90)
oblicz_ocene(rep(10, 10), 100, spozniony = TRUE)
}
