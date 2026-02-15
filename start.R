options(repos = c(CRAN = "https://cloud.r-project.org"))

auto_update_rainette <- tolower(Sys.getenv("AUTO_UPDATE_RAINETTE", "true")) %in% c("1", "true", "yes")

if (auto_update_rainette) {
  message("AUTO_UPDATE_RAINETTE=true -> tentative de mise à jour de rainette")
  try(install.packages("rainette", quiet = TRUE), silent = TRUE)
}

port <- as.integer(Sys.getenv("PORT", "7860"))
shiny::runApp("/home/user/app", host = "0.0.0.0", port = port)
