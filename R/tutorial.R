# Reading the yaml header of fileName
# Adapted from http://stackoverflow.com/questions/23622307/strip-yaml-from-child-docs-in-knitr

extract_yaml = function(fileName) {
  require(yaml)
  file_content <- readLines(con = fileName, encoding = "UTF-8")
  yaml_pattern <- "[-]{3}|[.]{3}"
  limits_yaml <- grep(pattern = yaml_pattern, x = file_content)[1:2]
  indices_yaml <- seq(
    from = limits_yaml[1] + 1,
    to = limits_yaml[2] - 1
  )
  yaml <- mapply(
    FUN = function(i) {yaml.load(string = i)},
    file_content[indices_yaml],
    USE.NAMES = FALSE
  )
  return(yaml)
}

# knit replacement to enable the rendering of two files in 1 click
# (with and without solution chunks).
# Insert a call to knit in the yaml header of the Rmd file (below the title)
# knit: unilur::knit
# Depending on the variable "solution" in the yaml header it will render:
#  "solution: yes" = Only the file with solution chunks
#  "solution: no" = Only the file without solution chunks
#  "solution: wow" = Both files (with and without) showing the first rendered without
#  "solution: wwo" = Both files (with and without) showing the first rendered with
# A suffix defined in the yaml header is added to the filename with solution chunks
#  (suffix: "_solution" is the default)

#' @export
knit <- (function(inputFile, encoding) {
  # Maybe there is a cleaner way to pass variables to knit?
  yamlHeader <- extract_yaml(inputFile)

  # Rendering a "response" Rmarkdown file (Rmd) omitting the solution chunks
  if (isTRUE(yamlHeader$answer)) answer.rmd(inputFile, paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", inputFile), "_answer", ".Rmd"))
  
  # Rendering pdf output (with and/or without solution chunks)
  if (is.null(yamlHeader$solution) || isTRUE(yamlHeader$solution)) tut_opt <- list(m = "file with", s = TRUE)
  else if (yamlHeader$solution == "wwo") tut_opt <- list(m = "files with and without", s = c(TRUE, FALSE))
  else if (yamlHeader$solution == "wow") tut_opt <- list(m = "files with and without", s = c(FALSE, TRUE))
  else tut_opt <- list(m = "file without", s = FALSE)

  message(paste("Rendering", tut_opt$m, "solutions"))

  if (!is.null(yamlHeader$suffix)) tut_opt$suffix <- yamlHeader$suffix
  else tut_opt$suffix <- "_solution"

  for (s in tut_opt$s) {
    rmarkdown::render(inputFile, encoding = encoding,
                      output_file = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", inputFile), ifelse(s, tut_opt$suffix, ""), ".pdf"),
                      output_format = "unilur::tutorial",
                      output_options=list(
                       solution = s
                      ), clean = TRUE
    )
  };
})

#' @export
tutorial <- function( keep_tex = TRUE,
                      includes = NULL,
                      fig_width = 6,
                      fig_height = 4,
                      fig_crop = TRUE,
                      solution = TRUE,     # Turn ON or OFF the rendering of solution chunks
                                           # Values in the yaml header are:
                                           # yes, no, wwo, wow (wwo = with and without
                                           #                    wow = without and with
                                           #                    first is shown by RStudio)
                      suffix = "_solution",# Used in the knit function to add a suffix to the pdf name
                      exam = FALSE,        # list(mcq = "oneparchoices") See options in the itemize2mcq function
                      credit = FALSE,      # show a link to the unilur homepage
                      answer = FALSE       # Generate answer Rmd (removing the solution chunks from the Rmd)
                      ) {
  # exam can be either TRUE (yes) or a list to set some options
  # We will use the exam class if isTRUE or is.list is met:
  if (isTRUE(exam) || (is.list(exam))) examen <- TRUE
  else examen <- FALSE

  header <- system.file("rmarkdown", "templates", "tutorial", "resources", "header.tex",
                        package = "unilur")
  header_credit <- system.file("rmarkdown", "templates", "tutorial", "resources", "header_credit.tex",
                        package = "unilur")
  
  # Disabling credit when exam is used:
  # credit would interfere with the exam class which already uses fancyhdr
  if (isTRUE(credit) && !isTRUE(examen)) includes = list(in_header = c(header, header_credit))
  else includes = list(in_header = header)
  
  if (isTRUE(examen)) {
    template <- system.file("rmarkdown", "templates", "tutorial", "resources", "template.tex",
                            package = "unilur")
    args <- c()
    args <- c(args, "--variable", "geometry:margin=1in")  # Adjusts the margin
    args <- c(args, "--variable", "graphics=yes")         # Enables rescaling of too big graphics
    # Enables the rendering of the identification box (first and last name) 
    if (isTRUE(as.list(exam)$id) || is.null(as.list(exam)$id)) args <- c(args, "--variable", "idbox=yes")
    # Using the exam class and passing an additional exam variable to the pandoc template
    args <- c(args, "--variable", "documentclass=exam", "--variable", "exam=yes")
  } else {
    template <- "default"
    args <- NULL
  }
  
  format <- rmarkdown::pdf_document(template = template,
                                    keep_tex = keep_tex,
                                    includes = includes,
                                    pandoc_args = args)

  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution pdf and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options$solution) && !isTRUE(solution)) {
      if (isTRUE(examen) && is.numeric(options$response.space)) return(paste0("\n\\fillwithdottedlines{", options$response.space, "in}\n"))
      else return("")
    } 
    
    # If examen mode is enabled and the "mcq" option is set we override the itemize environment
    if (isTRUE(examen) && isTRUE(options$mcq)) x <- itemize2mcq(x, mcq.option = as.list(exam)$mcq, ifelse(is.numeric(options$mcq.n), options$mcq.n, 3))

    # If "box" is set, we draw a frame around the chunk. 
    if (!is.null(options$box)) {
      BoxBegin <- sprintf("\n\\cboxs[%s]{%s}\n", ifelse(is.null(options$boxtitle), "", options$boxtitle), options$box)
      x <- paste0(c(BoxBegin, x, "\n\\cboxe\n"), collapse = "\n")
    }
    
    # If the solution pdf is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) return(paste0(c("\n\\solutions\n", x, "\n\\solutione\n"), collapse = "\n"))

        # If the solution pdf is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$answer)) return(paste0(c("\n\\answers\n", x, "\n\\answere\n"), collapse = "\n"))
    
    # If no condition has been met before, we are returning the chunk without changing it...
    return(x)
  }

  hook_input <- function(x, options) {
    paste0(c("\n\\begin{mdframed}[style = input]", "\\begin{Verbatim}[commandchars=\\\\\\{\\}]", knitr:::hilight_source(x, "latex", options), "\\end{Verbatim}", "\\end{mdframed}\n"), collapse = "\n")
  }

  hook_output <- function(x, options) {
    # Using trimws to remove the last newline character
    # which is messing up page breaks in latex...
    OutputBegin <- sprintf("\n\\begin{Verbatim}[%scommandchars=\\\\\\{\\}]", ifelse(isTRUE(options$samepage), "samepage, ", ""))
    OutputEnd <- "\\end{Verbatim}\n"
    x <- paste0(c(OutputBegin, trimws(x, which = "right"), OutputEnd), collapse = "\n")
    return(x)
  }

  hook_plot <- function(x, options) {
    # determine caption (if any)
    caption <- ifelse(is.null(options$fig.cap),
                      "",
                      paste("\\captionof{figure}{", options$fig.cap, "}\n", sep = ""))
    # return the latex
    paste(c("\\begin{center}", sprintf("\\includegraphics[trim=0 0 0 -2mm]{%s}\n%s\n", gsub("\\\\", "/", x), caption), "\\end{center}"), collapse = "\n")
  }

  format$knitr$knit_hooks$source  <- hook_input
  format$knitr$knit_hooks$output  <- hook_output
  format$knitr$knit_hooks$plot <- hook_plot
  format$knitr$knit_hooks$chunk  <- hook_chunk
  format
}

# "itemize2mcq" function
# Called by hook_chunk if the mcq option is set
# Wraps the chunk with a start and stop macro to enable/disable an alternative itemize environment

itemize2mcq <- function(x, mcq.option = c("oneparchoices", "oneparchoicesalt", "oneparcheckboxesalt", "oneparcheckboxes"), per.line = 3) {
  # mcq.options contains a list of the yaml mcq options and the corresponding
  # latex macros
  mcq.option <- match.arg(mcq.option)
  
  if (is.numeric(per.line)) MCQMacroOption <- paste0("[", per.line, "]")
  
  MCQMacros <- list(oneparchoices = c("opc", ""),
                    oneparchoicesalt = c("opcalt", MCQMacroOption),
                    oneparcheckboxesalt = c("opcbalt", MCQMacroOption),
                    oneparcheckboxes = c("opcb", "")) #,
                    #checkboxes = c("cb", ""), # Disabling the two last options as they generate latex errors...
                    #choices = c("ch", ""))    # TODO: Fix it!

  mcq.start <- paste0("\n\\", MCQMacros[[mcq.option]][1], MCQMacros[[mcq.option]][2], "{on}\n")
  mcq.end <- paste0("\n\\", MCQMacros[[mcq.option]][1], "{off}\n")
  return(paste(c(mcq.start, x, mcq.end), collapse = "\n"))
}

# answer.rmd function
# A simple function using a regular expression replacing the solution chunks by empty response chunks

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
  #header <- "---\ntitle: \"My answers\"\nauthor: \"My name\"\nknit: unilur::knit\ndate: `r format(Sys.time(), \"%d %B, %Y\")`\noutput:\n\tunilur::tutorial:\n\t\tanswer: yes\n---"
  # Tab character seems not accepted by the custom yaml parser... Try to use the rmarkdown parser?
  header <- "---\ntitle: \"My answers\"\nauthor: \"My name\"\nknit: unilur::knit\ndate: '`r format(Sys.time(), \"%d %B, %Y\")`'\noutput: unilur::tutorial\n---"
  output <- gsub(pattern, header, output, perl = TRUE)
  file.create(outputFile)
  fileConn <- file(outputFile)
  writeLines(output, fileConn)
  close(fileConn)
}