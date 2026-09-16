# Czas porównujemy w UTC; przesunięcie w konfiguracji musi odpowiadać strefie kursu.
czas_iso <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[+-][0-9]{2}:[0-9]{2})$", x))
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  v <- as.integer(substring(x, c(1, 6, 9, 12, 15, 18), c(4, 7, 10, 13, 16, 19)))
  rok <- v[1]
  miesiac <- v[2]
  dzien <- v[3]
  dni <- c(31, if (rok %% 4 == 0 && (rok %% 100 != 0 || rok %% 400 == 0)) 29 else 28,
           31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (rok < 1900 || miesiac < 1 || miesiac > 12 || dzien < 1 || dzien > dni[miesiac] ||
      v[4] > 23 || v[5] > 59 || v[6] > 59)
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  przesuniecie <- 0
  if (!endsWith(x, "Z")) {
    h <- as.integer(substr(x, 21, 22))
    m <- as.integer(substr(x, 24, 25))
    if (h > 14 || m > 59 || (h == 14 && m != 0))
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    przesuniecie <- (h * 60 + m) * 60 * if (substr(x, 20, 20) == "+") 1 else -1
  }
  as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") - przesuniecie
}
