if (!require("librarian")) install.packages("librarian")
if (!require("miniUI")) install.packages("miniUI")
if (!require("styler")) install.packages("styler")
if (!require("usethis")) install.packages("usethis")
if (!require("todor")) install.packages("todor")
if (!require("shrtcts")) remotes::install_github("gadenbuie/shrtcts")
if (interactive() && requireNamespace("shrtcts", quietly = TRUE)) {
  shrtcts::add_rstudio_shortcuts(set_keyboard_shortcuts = TRUE)
}

# Check for what to do in the beginning of a session
if (interactive()) {
  suppressMessages(require(usethis))
  suppressMessages(require(todor))
  options(todor_extra = "qmd")
  todor::todor()
}
