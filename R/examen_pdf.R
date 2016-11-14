#' @export
examen_pdf <- function(
  solution = FALSE,
  id = FALSE,
  mcq = "oneparchoices",
  includes = NULL,
  pandoc_args = NULL,
  ...
) {
  template <- system.file("rmarkdown", "templates", "tutorial", "resources", "template.tex",
                          package = "unilur")

  # Enables the rendering of the identification box (first and last name)
  if (isTRUE(id)) pandoc_args <- c(pandoc_args, "--variable", "idbox=yes")
  
  # Using the exam class and passing an additional exam variable to the pandoc template
  pandoc_args <- c(pandoc_args, "--variable", "exam=yes")
  
  header_examen <- system.file("rmarkdown", "templates", "tutorial", "resources", "header_examen.tex",
                               package = "unilur")
  
  format <- tutorial_pdf_base(solution = solution,
                              credit = FALSE,
                              pandoc_args = pandoc_args,
                              latex_class = "exam",
                              includes = merge.list(includes, list(in_header = header_examen)),
                              ...)
  
  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution pdf and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options$solution) && !isTRUE(solution)) {
      if (is.numeric(options$response.space)) return(paste0("\n\\fillwithdottedlines{", options$response.space, "in}\n"))
      else return("")
    } 
    
    # If examen mode is enabled and the "mcq" option is set we override the itemize environment
    if (isTRUE(options$mcq)) x <- itemize2mcq(x, mcq.option = mcq, ifelse(is.numeric(options$mcq.n), options$mcq.n, 3))
    
    # If "box" is set, we draw a frame around the chunk. 
    if (!is.null(options$box)) {
      BoxBegin <- sprintf("\n\\cboxs[%s]{%s}\n", ifelse(is.null(options$boxtitle), "", options$boxtitle), options$box)
      x <- paste0(c(BoxBegin, x, "\n\\cboxe\n"), collapse = "\n")
    }
    
    # If the solution pdf is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) return(paste0(c("\n\\solutions\n", x, "\n\\solutione\n"), collapse = "\n"))
    
    # If no condition has been met before, we are returning the chunk without changing it...
    return(x)
  }
  
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

#' @export
examen_solution_pdf <- function(...) {
  examen_pdf(solution = TRUE, ...)
}