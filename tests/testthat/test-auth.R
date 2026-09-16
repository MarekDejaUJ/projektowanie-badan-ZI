test_that("device flow respektuje pending, slow_down, odmowę i wygaśnięcie", {
  t <- 0
  odstepy <- numeric()
  czekaj <- function(s) { t <<- t + s; odstepy <<- c(odstepy, s) }
  i <- 0L
  odpowiedzi <- list(list(error = "authorization_pending"), list(error = "slow_down"), list(access_token = "test-token"))
  post <- function(...) { i <<- i + 1L; odpowiedzi[[i]] }
  x <- list(interval = 5, device_code = "test-code")
  expect_equal(badaniaZI:::czekaj_device(x, "app-test", 60, post, czekaj, function() t), "test-token")
  expect_equal(odstepy, c(5, 5, 10))
  expect_error(badaniaZI:::czekaj_device(x, "app-test", 60, function(...) list(error = "access_denied"), czekaj, function() t), "anulowano")
  expect_error(badaniaZI:::czekaj_device(x, "app-test", 4, post, czekaj, function() t), "wygas")
})

test_that("wylogowanie usuwa poświadczenia pakietu bez zmiany globalnego Git", {
  stan <- Sys.getenv(c("GH_TOKEN", "GH_CONFIG_DIR", "GITHUB_TOKEN"))
  sesja <- get("sesja_github", envir = asNamespace("badaniaZI"))
  sesja$token <- "test-token"
  sesja$login <- "test-student"
  wyloguj_github(FALSE)
  expect_error(badaniaZI:::token_sesji(), "zaloguj_github")
  expect_identical(Sys.getenv(c("GH_TOKEN", "GH_CONFIG_DIR", "GITHUB_TOKEN")), stan)
})
