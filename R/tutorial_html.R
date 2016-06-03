#' Convert to a tutorial PDF document
#'
#' Format for converting from R Markdown to a tutorial HTML document.
#' 
#' @inheritParams rmarkdown::html_document
#' 
#' @details See the inherited `rmarkdown::html_document` help page for additional arguments.
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
tutorial_html <- function( solution = FALSE,
                           solution_suffix = "_solution",
                           question_suffix = "_question",
                           answer = FALSE,                 # Generate answer Rmd (removing solution chunks from the Rmd)
                           credit = FALSE,                 # Show a link to the unilur homepage
                           includes = NULL,
                           ...
) {
  css <- system.file("rmarkdown", "templates", "tutorial", "resources", "style.css",
                     package = "unilur")
  
  credit.footer <- system.file("rmarkdown", "templates", "tutorial", "resources", "credit.html",
                               package = "unilur")
  
  if (isTRUE(credit)) includes = list(after_body = credit.footer)
  
  format <- rmarkdown::html_document(css = css,
                                     includes = includes,
                                     ...)
  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution html and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options$solution) && !isTRUE(solution)) {
      return("")
    }
    
    # If the solution html is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) {
      BoxContent <- sprintf("\n<div class=\"box-content\">\n%s\n</div>", paste0(x, collapse = "\n"))
      return(sprintf("\n<div class=\"box solution\">\n<h2>Solution</h2>\n%s\n</div>\n", BoxContent))
    }
    
    # If "box" is set, we draw a frame around the chunk. 
    if (!is.null(options$box)) {
      # Setting the color (Only colors listed by colors() are supported: Caution with documents for pdf using dvipsnames latex colors!)
      c <- col2rgb(options$box)
      BoxColor <-  sprintf("rgba(%s, %s, %s, 0.3)", c[1], c[2], c[3])
      BoxTitleColor <- sprintf("rgba(%s, %s, %s, 1)", c[1], c[2], c[3])
      
      BoxContent <- sprintf("\n<div class=\"box-content\">\n%s\n</div>", paste0(x, collapse = "\n"))
      if (!is.null(options$boxtitle)) {
        BoxContent <- sprintf("\n<h2 style=\"background-color:%s;\">%s</h2>\n%s\n", BoxTitleColor, options$boxtitle, BoxContent)
      } 
      x <- sprintf("\n<div class=\"box\" style=\"background-color:%s;border:2px solid %s;\">\n%s\n</div>\n", BoxColor, BoxTitleColor, BoxContent)
    }
    
    # If no condition has been met before, we are returning the chunk without changing it...
    return(x)
  }
  
   format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
     #output_file <- rmarkdown::html_document$post_processor(metadata, input_file, output_file, clean, verbose)
     new_name = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), ifelse(solution, solution_suffix, question_suffix), ".html")
     file.rename(output_file, new_name)
     return(new_name)
  }
  
  format$knitr$knit_hooks$chunk  <- hook_chunk
  format
}