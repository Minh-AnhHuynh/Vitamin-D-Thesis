# Read the file
lines <- readLines("acronyms.tex")

# Regular expression to match acronyms
pattern <- "\\\\acro\\{(.+?)\\}\\[(.+?)\\]\\{(.+?)\\}"

# Parse and sort acronyms
acronyms <- lapply(lines, function(x) {
  matches <- regmatches(x, regexec(pattern, x))[[1]]
  if(length(matches) > 0) list(matches[2], matches[3], matches[4]) else NULL
})
acronyms <- acronyms[!sapply(acronyms, is.null)]
acronyms <- acronyms[order(sapply(acronyms, function(x) x[[1]]))]

# Write sorted acronyms back to the file
writeLines("\\begin{acronym}", "sorted_acronym.tex")
for(acronym in acronyms) {
  writeLines(paste0("\\acro{", acronym[[1]], "} [", acronym[[2]], "] {", acronym[[3]], "}"), "sorted_acronym.tex")
}
writeLines("\\end{acronym}", "script/sorted_acronym.tex")
