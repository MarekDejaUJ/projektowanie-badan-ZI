\name{sprawdz_warianty}
\alias{sprawdz_warianty}
\title{Kontrola unikalności wariantów w prywatnym rosterze}
\usage{
sprawdz_warianty(id, scenariusze, rocznik = "2026-27")
}
\arguments{
\item{id}{Wektor pseudonimów.}

\item{scenariusze}{Wektor scenariuszy tej samej długości albo jeden scenariusz.}

\item{rocznik}{Rocznik.}
}
\value{
Prywatna tabela kluczy; błąd przy powtórzeniu ID lub kolizji RNG.
}
\description{
Kontrola unikalności wariantów w prywatnym rosterze
}
\examples{
sprawdz_warianty(c("s001", "s002"), "S01")
}
