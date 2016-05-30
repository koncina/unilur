# answer.rmd function
# A function using a regular expression replacing the solution chunks by empty response chunks

answer.rmd = function(inputFile, outputFile) {
  input <- paste(readLines(inputFile), collapse = "\n")
  # The regex pattern: searching for code chunks containing solution = TRUE (case insensitive)
  pattern <- "\\n *``` *{.*(?i)solution(?-i) *= *(?i)true(?-i).*} *\\n[\\s\\S]*?\\n *``` *"
  replacement <- "\n```{r, answer = TRUE}\n# Write your answer here\n```\n"
  # the pattern and replacement below reuses the original chunk type (r, asis etc). But the comment in r would result in a markdown header
  # and should be adjusted too...
  #pattern <- "\\n *``` *{ *([[:alpha:]]+) *,.*(?i)solution(?-i) *= *(?i)true(?-i).*} *\\n[\\s\\S]*?\\n *``` *"
  #replacement <- "\n```{\\1, answer = TRUE}\n# Write your answer here\n```\n"
  
  output <- gsub(pattern, replacement, input, perl = TRUE)
  # Removing the chunks with either echo or eval set to FALSE
  pattern <- "\\n *``` *{.*(?i)(eval|echo|include)(?-i) *= *(?i)false(?-i).*} *\\n[\\s\\S]*?\\n *``` *"
  replacement <- ""
  output <- gsub(pattern, replacement, output, perl = TRUE)
  # Replacing the original header by a custom one...
  pattern <- "^--- *\\n[\\s\\S]*?\\n *--- *"
  #header <- "---\ntitle: \"My answers\"\nauthor: \"My name\"\ndate: `r format(Sys.time(), \"%d %B, %Y\")`\noutput:\n\tunilur::tutorial_pdf:\n\t\tanswer: yes\n---"
  # Tab character seems not accepted by the custom yaml parser... Try to use the rmarkdown parser?
  header <- "---\ntitle: \"My answers\"\nauthor: \"My name\"\ndate: '`r format(Sys.time(), \"%d %B, %Y\")`'\noutput: unilur::tutorial\n---"
  output <- gsub(pattern, header, output, perl = TRUE)
  file.create(outputFile)
  fileConn <- file(outputFile)
  writeLines(output, fileConn)
  close(fileConn)
}