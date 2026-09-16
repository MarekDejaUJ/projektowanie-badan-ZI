\name{sprawdz_konfiguracje}
\alias{sprawdz_konfiguracje}
\title{Sprawdzenie kompletności zasad rocznika}
\usage{
sprawdz_konfiguracje(konfiguracja, publikacja = FALSE)
}
\arguments{
\item{konfiguracja}{Lista zwrócona przez konfiguracja_kursu().}

\item{publikacja}{Czy błędy i brak zasad oceny mają blokować publikację.
Nieogłoszony pełny kalendarz pozostaje jawną informacją do ustalenia.}
}
\value{
Ramka problemów i ich wagi (blad albo informacja).
}
\description{
Sprawdzenie kompletności zasad rocznika
}
\examples{
sprawdz_konfiguracje(konfiguracja_kursu())
}
