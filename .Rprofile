if (!require("librarian")) install.packages("librarian")
if(interactive()){
  librarian::shelf(usethis, todor, gadenbuie/shrtcts, miniUI)
}

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
