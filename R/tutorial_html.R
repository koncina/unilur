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
    
    if (!(isTRUE(options$solution) || !is.null(options$box))) { 
      # Not a box: return the chunk without changing it...
      return(x)
    }
    
    # We need to draw a box: solution or custom
    if (isTRUE(options$solution)) {
      # If it's a solution, draw a green box around it.
      colour <- c(172, 225, 175)
      box_title <- "Solution"
    } else {
      colour <- col2rgb(options$box)
      box_title <- options$boxtitle
    }
    
    panel_colour <-  do.call(sprintf, as.list(c("rgba(%s, %s, %s, 0.3)", colour)))
    header_colour <- do.call(sprintf, as.list(c("rgba(%s, %s, %s, 1)", colour)))
    title_colour <- do.call(sprintf, as.list(c("rgba(%s, %s, %s, 1)", round(colour * 0.4))))
    
    panel_class <- sprintf("class = \"panel\" style = \"background-color:%s; border:2px solid %s;\"", panel_colour, header_colour)
    
    panel_body <- sprintf("<div class=\"panel-body\">%s</div>", paste0(x, collapse = "\n"))
    
    if (!is.null(options$box.collapse)) {
      panel_body <- sprintf("<div id=\"%s\" class=\"panel-collapse collapse%s\">%s</div>", options$label, ifelse(isTRUE(options$box.collapse), "", " in"), panel_body)
      box_title = sprintf("<a class = \"%s\" style=\"color: %s;\" data-toggle=\"collapse\" href=\"#%s\">%s</a>", ifelse(isTRUE(options$box.collapse), "collapsed", ""), title_colour, options$label, box_title)
    }
    
    panel_header <- sprintf("<div class=\"panel-heading\" style=\"background-color:%s; color:%s!important;\"><h4 class=\"panel-title\">%s</h4></div>",
                            header_colour, title_colour, box_title)
    
    
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
    version = packageVersion("unilur"),
    src = system.file("rmarkdown", "templates", "tutorial", "resources", package = "unilur"),
    stylesheet = c(
      "css/style.css"
    )
  )
}