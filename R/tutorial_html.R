#' Convert to a tutorial HTML document
#'
#' Format for converting from R Markdown to a tutorial HTML document.
#' 
#' @inheritParams rmarkdown::html_document
#' 
#' @details See the inherited `rmarkdown::html_document` help page for additional arguments.
#' 
#' @param solution Turn ON or OFF the rendering of solution chunks (default is \code{FALSE})
#' 
#' @param collapse Turn ON or OFF the rendering with collapsed solution chunks (default is \code{FALSE})
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
tutorial_html <- function(solution = FALSE,
                          collapse = FALSE,
                          solution_suffix = "_solution",
                          question_suffix = "_question",
                          credit = FALSE,                 # Show a link to the unilur homepage
                          includes = NULL,
                          css = NULL,
                          extra_dependencies = NULL,
                          ...
) {

  credit.footer <- system.file("rmarkdown", "templates", "tutorial", "resources", "credit.html",
                               package = "unilur")
  
  if (isTRUE(credit)) includes = list(after_body = credit.footer)
  
  extra_dependencies <- append(extra_dependencies,
                               list(html_dependency_tutorial()))
  
  format <- rmarkdown::html_document(css = css,
                                     includes = includes,
                                     extra_dependencies = extra_dependencies,
                                     ...)
  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution html and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options$solution) && !isTRUE(solution)) {
      return("")
    }

    panel_header <- ""
    panel_body <- sprintf("<div class=\"panel-body\">%s</div>", paste0(x, collapse = "\n"))
    panel_class <- ""
    
    # If the solution html is being rendered and the chunk is a solution, we are drawing a green box around it.
    if (isTRUE(options$solution) && isTRUE(solution)) {
      panel_header <-  sprintf("<div class=\"panel-heading\"><h4 class=\"panel-title\"><a class = \"%s\" data-toggle=\"collapse\" href=\"#%s\">Solution</a></h4></div>", ifelse(collapse, "collapsed", ""), options$label)
      panel_body <- sprintf("<div id=\"%s\" class=\"panel-collapse collapse%s\">%s</div>", options$label, ifelse(collapse, "", " in"), panel_body)
      panel_class <- "class = \"panel solution\""
    } else if (!is.null(options$box)) { # If "box" is set, we draw a frame around the chunk. 
      # Setting the colour (Only colours listed by colors() are supported: Caution with documents for pdf using dvipsnames latex colours!)
      c <- col2rgb(options$box)
      panel_colour <-  sprintf("rgba(%s, %s, %s, 0.3)", c[1], c[2], c[3])
      header_colour <- sprintf("rgba(%s, %s, %s, 1)", c[1], c[2], c[3])
      panel_class <- sprintf("class = \"panel\" style = \"background-color:%s; border:2px solid %s;\"", panel_colour, header_colour)
      if (!is.null(options$boxtitle)) {
        panel_header <- sprintf("<div class=\"panel-heading\" style=\"background-color:%s;\"><h4 class=\"panel-title\">%s</h4></div>", header_colour, options$boxtitle)
      } 
    } else {
      # If no condition has been met before, we are returning the chunk without changing it...
      return(x)
    }
    
    box_content <-  sprintf("\n\n<div class=\"panel-group\"><div %s>%s%s</div></div>\n\n", panel_class, panel_header, panel_body)
  }
  
   format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
     new_name = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), ifelse(solution, solution_suffix, question_suffix), ".html")
     file.rename(output_file, new_name)
     return(new_name)
  }
  
  format$pre_knit <-  function(input, ...) {
    knitr::opts_hooks$set(solution = function(options) {
      if (!isTRUE(solution)) options$eval <- FALSE
      options
    })
  }
  
  format$knitr$knit_hooks$chunk  <- hook_chunk
  format
}

#' @export
#' 
#' @rdname tutorial_html
tutorial_html_solution <- function(...) {
  tutorial_html(solution = TRUE, ...)
}

html_dependency_tutorial <- function() {
  htmltools::htmlDependency(
    name = "tutorial",
    version = "0.1",
    src = system.file("rmarkdown", "templates", "tutorial", "resources", package = "unilur"),
    stylesheet = c(
      "css/style.css"
    )
  )
}