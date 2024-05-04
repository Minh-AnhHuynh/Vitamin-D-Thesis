#' Add acronym macro
#' Add "\ac{}"
#' @interactive
#' @shortcut Alt+a
function() {
  # Gets the active document
  ctx <- rstudioapi::getActiveDocumentContext()

  # Checks that a document is active
  if (!is.null(ctx)) {
    # Extracts selection as a string
    selected_text <- ctx$selection[[1]]$text

    # Modify string
    selected_text <- glue::glue("\\ac{{{selected_text}}}", escape = "\\")

    # Replaces selection with string
    rstudioapi::modifyRange(ctx$selection[[1]]$range, selected_text)
  }
}
