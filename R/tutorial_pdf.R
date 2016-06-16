
tutorial_pdf_base <- function( solution = FALSE,               # Turn ON or OFF the rendering of solution chunks
                               solution_suffix = "_solution",
                               question_suffix = "_question",
                               answer = FALSE,                 # Generate answer Rmd (removing solution chunks from the Rmd)
                               credit = FALSE,                 # Show a link to the unilur homepage
                               ...
) {
  Check <- ArgumentCheck::newArgCheck()
  if (solution %in% c("wow", "wwo"))
    ArgumentCheck::addWarning(
      msg = "wow or wwo values for solution have been deprecated!",
      argcheck = Check
    )
  if (!is.logical(solution)) 
    ArgumentCheck::addError(
      msg = "solution must be either yes (TRUE) or no (FALSE)",
      argcheck = Check
    )
  if (nchar(solution_suffix) == 0 || nchar(question_suffix) == 0 || solution_suffix == question_suffix)
    ArgumentCheck::addError(
      msg = "solution_suffix and question_suffix must be different and contain at least a character",
      argcheck = Check
    )
  if (!is.logical(answer)) 
    ArgumentCheck::addError(
      msg = "answer must be either yes (TRUE) or no (FALSE)",
      argcheck = Check
    )
  if (!is.logical(credit)) 
    ArgumentCheck::addError(
      msg = "credit must be either yes (TRUE) or no (FALSE)",
      argcheck = Check
    )
  ArgumentCheck::finishArgCheck(Check)
  
  header <- system.file("rmarkdown", "templates", "tutorial", "resources", "header.tex",
                        package = "unilur")
  header_credit <- system.file("rmarkdown", "templates", "tutorial", "resources", "header_credit.tex",
                               package = "unilur")
  
  if (isTRUE(credit)) includes = list(in_header = c(header, header_credit))
  else includes = list(in_header = header)
  
  format <- rmarkdown::pdf_document(includes = includes, ...)
  
  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution pdf and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options$solution) && !isTRUE(solution)) return("")
    
    # If "box" is set, we draw a frame around the chunk. 
    if (!is.null(options$box)) {
      c <- col2rgb(options$box)
      ColorDef <- sprintf("\n\\definecolor{color-%s}{RGB}{%s}\n", options$label, paste(c, collapse = ", ")) 
      BoxBegin <- sprintf("\n\\cboxs[%s]{color-%s}\n", ifelse(is.null(options$boxtitle), "", options$boxtitle), options$label)
      x <- paste0(c(ColorDef, BoxBegin, x, "\n\\cboxe\n"), collapse = "\n")
    }
    
    # If the solution pdf is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) return(paste0(c("\n\\solutions\n", x, "\n\\solutione\n"), collapse = "\n"))
    
    # If no condition has been met before, we are returning the chunk without changing it...
    return(x)
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
    print(new_name)
    # Creating the Rmd to be filled by the students (solutions were removed and replaced by empty answer chunks)
    # Not very clean: input_file is an intermediate already knitted file. I will use output_file and replace pdf with Rmd again (might not be reliable...)
    if (isTRUE(answer)) answer.rmd(paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), ".Rmd"), paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), "_answer", ".Rmd"))
    file.rename(output_file, new_name)
    return(new_name)
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
#' @param answer Turn ON or OFF the rendering of a Rmarkdown file (Rmd) without the solutions (default is \code{FALSE}). It will create a <filename>_answer.Rmd file.
#' 
#' @param credit Turn ON or OFF the footer showing a link to the unilur homepage (default is \code{FALSE})
#' 
#' @return R Markdown output format to pass to \code{\link{render}}
#' 
#' @export
tutorial_pdf <- function(...) {
  tutorial_pdf_base(...)
}


#' @export
tutorial_solution_pdf <- function(...) {
  tutorial_pdf_base(solution = TRUE, ...)
}