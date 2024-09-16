if (!require("librarian")) install.packages("librarian")
if (!require("miniUI")) install.packages("miniUI")
if (!require("styler")) install.packages("styler")
if (!require("usethis")) install.packages("usethis")
if (!require("todor")) install.packages("todor")
if (!require("remotes")) install.packages("remotes")
if (!require("shrtcts")) remotes::install_github("gadenbuie/shrtcts")

# Basic packages
librarian::shelf(knitr, rmarkdown)



# Function to check and install/update TinyTeX
install_or_update_tinytex <- function() {
  # Use `echo Y` to pass "yes" to the update prompt
  message("Checking and updating TinyTeX if necessary...")

  # Execute the command and pass "Y" automatically if update is required
  system("quarto install tinytex --no-prompt")
  system("quarto install --no-prompt")

  message("TinyTeX installation/update process completed.")
}

install_or_update_tinytex()
