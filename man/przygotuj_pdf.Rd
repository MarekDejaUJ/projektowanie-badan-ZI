\name{przygotuj_pdf}
\alias{przygotuj_pdf}
\title{Jednorazowe przygotowanie narzędzi PDF}
\usage{
przygotuj_pdf(test = TRUE)
}
\arguments{
\item{test}{Czy złożyć próbny dokument XeLaTeX po instalacji.}
}
\value{
TRUE, niewidocznie, gdy XeLaTeX z kompletem pakietów działa.
}
\description{
Instaluje TinyTeX, jeśli brakuje XeLaTeX (np. w nowym projekcie Posit Cloud),
doinstalowuje pakiety LaTeX używane przez PDF pracy i składa próbny dokument.
Ponowne wywołanie tylko sprawdza gotowe narzędzia. Wymaga internetu przy
pierwszej instalacji; trwa kilka minut.
}
\examples{
\dontrun{ przygotuj_pdf() }
}
