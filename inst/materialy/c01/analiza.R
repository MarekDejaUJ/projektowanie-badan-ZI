## ----kalkulator---------------------------------------------------------------
# Dodawanie dwóch liczb.
5 + 3

# Potęgowanie: 2 do czwartej potęgi.
2^4

# Pierwiastek kwadratowy ze 144.
sqrt(144)

# Logarytm naturalny z 10.
log(10)


## ----typy-danych--------------------------------------------------------------
# Liczba: można ją dodawać, odejmować i uśredniać.
czas <- 12
class(czas)
typeof(czas)

# Tekst: opisuje kategorię, ale nie nadaje się do obliczania średniej.
kanal <- "WWW"
class(kanal)
typeof(kanal)

# Wartość logiczna: TRUE albo FALSE.
czy_www <- kanal == "WWW"
class(czy_www)
czy_www


## ----obiekty------------------------------------------------------------------
liczba_dokumentow <- 45
liczba_dokumentow
po_dodaniu <- liczba_dokumentow + 5
po_dodaniu


## ----wektory------------------------------------------------------------------
czasy_szukania <- c(15, 22, 45, 12, 60)
czasy_szukania[3]
czasy_szukania + 5


## ----warunki------------------------------------------------------------------
czasy_szukania > 30
(czasy_szukania > 20) & (czasy_szukania < 50)
z_brakiem <- c(15, NA, 45)
is.na(z_brakiem)
mean(z_brakiem)
mean(z_brakiem, na.rm = TRUE)


## ----tabela-------------------------------------------------------------------
uzytkownicy <- data.frame(
  id = c("o001", "o002", "o003"),
  kanal = c("WWW", "e-mail", "WWW"),
  czas = c(12, 8, 15)
)
uzytkownicy
str(uzytkownicy)
nrow(uzytkownicy)
uzytkownicy$czas
uzytkownicy[2, "kanal"]
