tutorial_knit <- (function(inputFile, encoding) {
  # http://stackoverflow.com/questions/32944715/conditionally-display-block-of-markdown-text-using-knitr
  # must include this fix before knitr 1.12 uis released
  knitr::knit_engines$set(asis = function(options) {
    if (options$echo && options$eval) paste(options$code, collapse = '\n')
  })
  # Defining a new solution engine based on the asis engine and wrapping the code
  # with the solution latex environment (see header.tex file)
  knitr::knit_engines$set(solution = function(options) {
    if (solution && options$echo && options$eval) paste(c("\\begin{solution}",options$code,"\\end{solution}"), collapse = '\n')
  })
  header <- system.file("rmarkdown", "templates", "tutorial", "resources", "header.tex",
                          package = "unilur")
  for (solution in c(TRUE, FALSE)) {
    rmarkdown::render(inputFile, encoding = encoding,
                      output_file = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", inputFile), ifelse(solution, "_solution", ""), ".pdf"),
                      output_format = "pdf_document",
                      output_options=list(
                        includes = list(
                          in_header = header)
                        )
                      )
  };
})
