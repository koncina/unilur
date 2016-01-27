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

  if (isTRUE(yamlHeader$solution)) tut_opt <- list(m = "file with", s = TRUE)
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
                      ), clean = FALSE
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
                      exam = FALSE         # list(mcq = "oneparchoices") See options in the itemize2mcq function
                      ) {
  # exam can be either TRUE (yes) or a list to set some options
  # We will use the exam class if isTRUE or is.list is met:
  if (isTRUE(exam) || (is.list(exam))) examen <- TRUE
  else examen <- FALSE

  header <- system.file("rmarkdown", "templates", "tutorial", "resources", "header.tex",
                        package = "unilur")

  includes = list(in_header = header)

  if (isTRUE(examen)) {
    template <- system.file("rmarkdown", "templates", "tutorial", "resources", "template.tex",
                            package = "unilur")
    pandoc_args = c()
    if (isTRUE(as.list(exam)$id) || is.null(as.list(exam)$id)) pandoc_args = c(pandoc_args, "--variable", "idbox=TRUE")
    pandoc_args = c(pandoc_args, "--variable", "documentclass=exam", "--variable", "exam=TRUE")
  } else {
    template <- "default"
    pandoc_args <- NULL #c("--variable", "geometry:margin=1in")
  }
  
  format <- rmarkdown::pdf_document(template = template,
                                    keep_tex = keep_tex,
                                    includes = includes,
                                    pandoc_args = pandoc_args)

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
      if (is.null(options$boxtitle)) {
        beginbox <- paste0("\n\\cboxs{", options$box, "}\n")
      } else {
        beginbox <- paste0("\n\\cboxs[", options$boxtitle, "]{", options$box, "}\n")
      }
      x <- paste0(c(beginbox, x, "\n\\cboxe\n"), collapse = "\n")
    }
    
    # If the solution pdf is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) return(paste0(c("\n\\solutions\n", x, "\n\\solutione\n"), collapse = "\n"))
    
    # If no condition has been met before, we are returning the chunk without changing it...
    return(x)
  }

  hook_input <- function(x, options) {
    paste0(c("\n\\begin{mdframed}[style = input]", "\\begin{Verbatim}[commandchars=\\\\\\{\\}]", knitr:::hilight_source(x, "latex", options), "\\end{Verbatim}", "\\end{mdframed}\n"), collapse = "\n")
  }

  hook_output <- function(x, options) {
    # Using trimws to remove the last newline character
    # which is messing up page breaks in latex...
    x <- paste0(c("\n\\begin{Verbatim}[commandchars=\\\\\\{\\}]", trimws(x, which = "right"), "\\end{Verbatim}\n"), collapse = "\n")
    if (isTRUE(options$samepage)) paste0(c("\n\\begin{samepage}", x, "\\end{samepage}\n"), collapse = "\n")
    else return(x)
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

