# Uruchamiaj z katalogu źródeł kursu. Kontekst Docker zawiera tylko paczkę i Dockerfile.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, file.exists(args[1L]), nzchar(Sys.which("docker")))
context <- tempfile("obraz-kontroli-")
dir.create(context)
stopifnot(file.copy(args[1L], file.path(context, "badaniaZI.tar.gz")),
  file.copy("tools/kontrola/Dockerfile", file.path(context, "Dockerfile")))
result <- processx::run("docker", c("build", "--iidfile", file.path(context, "image.id"), context),
  timeout = 1800, error_on_status = FALSE, echo = TRUE, windows_hide_window = TRUE)
if (result$status != 0L) stop("Budowa obrazu nieudana. Kontekst: ", context)
image <- trimws(readLines(file.path(context, "image.id")))
stopifnot(grepl("^sha256:[0-9a-f]{64}$", image))
cat("Zaufany obraz kontroli:", image, "\n")
if (nzchar(Sys.getenv("GITHUB_ENV")))
  cat(paste0("BADANIAZI_REVIEW_IMAGE=", image, "\n"), file = Sys.getenv("GITHUB_ENV"), append = TRUE)
