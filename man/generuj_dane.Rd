\name{generuj_dane}
\alias{generuj_dane}
\title{Indywidualne syntetyczne dane ankietowe i obserwacyjne}
\usage{
generuj_dane(id, scenariusz = "S01", rocznik = "2026-27", n = 150L)
}
\arguments{
\item{id}{Pseudonim studenta, np. "s017".}

\item{scenariusz}{Identyfikator S01--S20.}

\item{rocznik}{Rocznik kursu.}

\item{n}{Liczba wierszy surowych danych, od 120 do 180.}
}
\value{
Lista z danymi, słownikiem, opisem scenariusza i manifestem.
}
\description{
Dane służą do ćwiczenia analizy, a nie opisania rzeczywistej instytucji.
Łączą samoocenę z obserwowanym wykonaniem krótkiego zadania wyszukiwawczego.
Zawierają kontrolowane braki, dwa duplikaty i dwa nietypowe czasy.
Identyfikator jest pseudonimem przydzielonym na zajęciach, nie numerem albumu.
}
\examples{
x <- generuj_dane("s017", "S03")
head(x$dane)
}
