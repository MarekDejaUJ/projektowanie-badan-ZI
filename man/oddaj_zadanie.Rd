\name{oddaj_zadanie}
\alias{oddaj_zadanie}
\title{Oddanie zadania z konsoli R}
\usage{
oddaj_zadanie(id, katalog = ".")
}
\arguments{
\item{id}{Z01--Z10.}

\item{katalog}{Katalog projektu.}
}
\value{
Lista z repo, SHA, ID odbioru i czasem serwera.
}
\description{
Wysyła wyłącznie jawną listę plików. Potwierdzeniem jest zdalny SHA oraz
rekord odbioru utworzony przez GitHub, z czasem serwera. Zielony stan
odbioru nie jest oceną merytoryczną ani wynikiem kontroli kodu.
}
\examples{
\dontrun{ oddaj_zadanie("Z01", "moje-badania") }
}
