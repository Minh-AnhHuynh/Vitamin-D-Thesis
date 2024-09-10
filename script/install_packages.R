if (!require("librarian")) install.packages("librarian")
if (!require("miniUI")) install.packages("miniUI")
if (!require("styler")) install.packages("styler")
if (!require("usethis")) install.packages("usethis")
if (!require("todor")) install.packages("todor")
if (!require("remotes")) install.packages("remotes")
if (!require("shrtcts")) remotes::install_github("gadenbuie/shrtcts")

# Basic packages
librarian::shelf(knitr, rmarkdown)

# If needed
system("quarto install tinytex")

# Function to check installed TinyTeX version
check_tinytex_version <- function() {
  tinytex_info <- system("quarto check", intern = TRUE)

  # Look for the TinyTeX version line
  tinytex_line <- grep("TinyTeX version", tinytex_info, value = TRUE)

  if (length(tinytex_line) > 0) {
    # Extract version number from the line (assuming version is in the format x.y.z)
    installed_version <- sub(".*TinyTeX version (.*)", "\\1", tinytex_line)
    return(installed_version)
  } else {
    return(NULL)
  }
}

# Function to install or update TinyTeX
install_or_update_tinytex <- function() {
  installed_version <- check_tinytex_version()

  if (is.null(installed_version)) {
    message("TinyTeX is not installed. Installing the latest TinyTeX via Quarto...")
    system("quarto install tinytex")
  } else {
    message(paste("Current installed TinyTeX version:", installed_version))

    # Optionally, compare with the latest version (can add external version checks here)
    # For simplicity, let's assume we always install the latest TinyTeX
    message("Updating to the latest TinyTeX version...")
    system("quarto install tinytex")
  }
}

# Run the update or installation process
install_or_update_tinytex()
