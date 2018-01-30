#' @importFrom grDevices col2rgb

tutorial_pdf_base <- function(solution = FALSE,               # Turn ON or OFF the rendering of solution chunks
                              solution_suffix = "_solution",
                              question_suffix = "_question",
                              credit = FALSE,                 # Show a link to the unilur homepage
                              latex_class = "article",
                              pandoc_args = NULL,
                              ...
) {

  template <- system.file("rmarkdown", "templates", "tutorial", "resources", "template.tex",
                          package = "unilur")
  pandoc_args <- c(pandoc_args, "--variable", "geometry:margin=1in")  # Adjusts the margin
  pandoc_args <- c(pandoc_args, "--variable", "graphics=yes")         # Enables rescaling of too big graphics
  pandoc_args <- c(pandoc_args, "--variable", paste0("documentclass=", latex_class))
  if (isTRUE(credit)) pandoc_args <- c(pandoc_args, "--variable", "credit=yes")
  
  format <- rmarkdown::pdf_document(pandoc_args = pandoc_args, template = template, ...)
  
  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution pdf and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options$solution) && !isTRUE(solution)) return("")
    
    if (!is_box(options)) return(x) # Not a box: return the chunk without changing it...

    # If the solution pdf is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) return(paste0(c("\n\\solutions\n", x, "\n\\solutione\n"), collapse = "\n"))
    
    colour_def <- sprintf("\n\\definecolor{color-%s}{RGB}{%s}\n", options$label, paste(box_colour(options), collapse = ", "))
    
    box_begin <- sprintf("\n\\cboxs[%s]{color-%s}\n", ifelse(is.null(options$box.title), "", options$box.title), options$label)
    paste0(c(colour_def, box_begin, x, "\n\\cboxe\n"), collapse = "\n")
  }
  
  hook_input <- function(x, options) {
    InputBegin <- sprintf("\n\\begin{mdframed}[style = input%s]", ifelse(isTRUE(options$samepage), ", nobreak = true", ""))
    paste0(c(InputBegin,
             "\\begin{Verbatim}[commandchars=\\\\\\{\\}]",
             knitr:::hilight_source(x, "latex", options),
             "\\end{Verbatim}",
             "\\end{mdframed}\n"),
           collapse = "\n")
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
  
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    new_name = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), ifelse(isTRUE(solution), solution_suffix, question_suffix), ".pdf")
    file.rename(output_file, new_name)
    return(new_name)
  }
  
  format$pre_knit <-  function(input, ...) {
    knitr::opts_hooks$set(solution = function(options) {
      if (!isTRUE(solution)) options$eval <- FALSE
      options
    })
  }
  
  format$knitr$knit_hooks$source  <- hook_input
  format$knitr$knit_hooks$output  <- hook_output
  format$knitr$knit_hooks$plot <- hook_plot
  format$knitr$knit_hooks$chunk  <- hook_chunk
  format
}

#' Convert to a tutorial PDF document
#'
#' Format for converting from R Markdown to a tutorial PDF document.
#' 
#' @inheritParams rmarkdown::pdf_document
#' 
#' @details See the inherited `rmarkdown::pdf_document` help page for additional arguments.
#' 
#' @param solution Turn ON or OFF the rendering of solution chunks (default is \code{FALSE})
#' 
#' @param solution_suffix Suffix which is added to the filename when \code{solution = TRUE} (default is '_solution')
#' 
#' @param question_suffix Suffix which is added to the filename when \code{solution = FALSE} (default is '_question')
#' 
#' @param credit Turn ON or OFF the footer showing a link to the unilur homepage (default is \code{FALSE})
#' 
#' @return R Markdown output format to pass to \code{\link{render}}
#' 
#' @export
tutorial_pdf <- function(...) {
  tutorial_pdf_base(...)
}

#' @rdname tutorial_pdf
#' @export
tutorial_pdf_solution <- function(...) {
  tutorial_pdf_base(solution = TRUE, ...)
}