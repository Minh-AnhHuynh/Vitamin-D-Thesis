sort_acronyms <- function(file_name) {
  # Read the contents of your R script file
  librarian::shelf(dplyr, tidyr, stringr, magrittr, naturalsort)
  acronym_file <- read.delim(file_name, header = FALSE)
  names(acronym_file) <- "code"
  acronym_file$row_id <- row.names(acronym_file)

  # Extract words from each line
  acronym_file %<>% mutate(
    acronyms = str_replace(code, ".*\\\\acro\\{([^\\}]+)\\}.*", "\\1"),
    acronym_exist = str_detect(code, "\\\\acro\\{([^\\}]+)\\}"),
    acronyms_lower = tolower(acronyms)
  )

  # Get the first row_id of acronym_exist == TRUE
  first_row_id <-
    acronym_file$row_id[which(acronym_file$acronym_exist)[1]]

  # We need to use naturalorder to make with this syntax to make sure the
  # acronyms are sorted according to the index given by natural order, dplyr
  # syntax with mutate doesn't appear to work
  acronym_sorted <- acronym_file %>%
    filter(acronym_exist) %>%
    .[naturalsort::naturalorder(.$acronyms_lower), ] %>%
    mutate(sort_order = seq(from = first_row_id, length.out = nrow(.)))

  final_df <-
    bind_rows(acronym_sorted, filter(acronym_file, !acronym_exist)) %>%
    select(-acronyms_lower)

  # Copy the row_id to sort_order for non-acronym lines
  final_df$sort_order[is.na(final_df$sort_order)] <-
    final_df$row_id[is.na(final_df$sort_order)]

  # Sort by natural numeric order
  final_df <-
    final_df[naturalsort::naturalorder(final_df$sort_order), ]

  # Write the results of final_df$code to a file
  writeLines(final_df$code, file_name)
}
