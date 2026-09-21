\name{oddaj_zadanie}
\alias{oddaj_zadanie}
\title{Oddanie zadania z konsoli R}
\usage{
oddaj_zadanie(id, katalog = NULL)
}
\arguments{
\item{id}{Z01--Z10.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}
}
\value{
Lista z repo, SHA, ID odbioru i czasem serwera.
}
\description{
Z zapisanego Rmd tworzy aktualny PDF i wysyła go razem ze źródłami.
Sprawdza własne odpowiedzi, gotowy kod i dane; nie przyznaje punktów.
Wysyła wyłącznie jawną listę plików. Potwierdzeniem jest zdalny SHA oraz
rekord odbioru utworzony przez GitHub, z czasem serwera. Zielony stan
odbioru nie jest oceną merytoryczną ani wynikiem kontroli kodu.
}
\examples{
\dontrun{ oddaj_zadanie("Z01", "moje-badania") }
}
