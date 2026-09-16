\name{termin_zadania}
\alias{termin_zadania}
\title{Termin zadania wynikający z kalendarza rocznika}
\usage{
termin_zadania(id, konfiguracja = konfiguracja_kursu())
}
\arguments{
\item{id}{Z01--Z10.}

\item{konfiguracja}{Konfiguracja rocznika.}
}
\value{
Lista: stan, zasada, data ISO 8601 lub NULL i identyfikator kolejnego ćwiczenia.
}
\description{
Z01--Z09 są oddawane do rozpoczęcia kolejnego ćwiczenia. Z10 wymaga osobnej
daty. Nieznane daty są zwracane jako do_ustalenia, bez wymyślania kalendarza.
}
\examples{
termin_zadania("Z01")
}
