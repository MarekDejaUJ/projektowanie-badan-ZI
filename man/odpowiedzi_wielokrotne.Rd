\name{odpowiedzi_wielokrotne}
\alias{odpowiedzi_wielokrotne}
\title{Odsetki odpowiedzi wielokrotnych}
\usage{
odpowiedzi_wielokrotne(dane)
}
\arguments{
\item{dane}{Tabela kolumn 0/1, z dopuszczalnym NA.}
}
\value{
Liczebności, mianowniki i procent respondentów. Suma procentów może przekraczać 100.
}
\description{
Odsetki odpowiedzi wielokrotnych
}
\examples{
odpowiedzi_wielokrotne(data.frame(www = c(1, 0, NA), email = c(1, 1, NA)))
}
