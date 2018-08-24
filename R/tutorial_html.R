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
#' @param solution_suffix Suffix which is added to the filename when \code{solution = TRUE} (default is '_solution')
#' 
#' @param question_suffix Suffix which is added to the filename when \code{solution = FALSE} (default is '_question')
#' 
#' @return R Markdown output format to pass to \code{\link{render}}
#' 
#' @export
tutorial_html <- function(solution = FALSE,
                          solution_suffix = "_solution",
                          question_suffix = "_question",
                          includes = NULL,
                          css = NULL,
                          extra_dependencies = NULL,
                          ...
) {

  extra_dependencies <- append(extra_dependencies,
                               list(html_dependency_tutorial()))
  
  format <- rmarkdown::html_document(css = css,
                                     includes = includes,
                                     extra_dependencies = extra_dependencies,
                                     ...)
  
  default_box_themes <- list(
    default = set_box_theme(body = list(fill = "#F2F2F2", colour = "blue"), header = list(fill = "#D3D3D3FF", colour = "black")),
    solution = set_box_theme(title = "solution", body = list(fill = "#ACFFAF4C", colour = "black"), header = list(fill = "#ACFFAFFF", colour = "#456646FF"), collapse = FALSE)
    )
  
  

  hook_chunk <- function(x, options) {
    # If we are NOT rendering the solution html and the chunk is a solution one, we are
    #  returning an empty string to hide the chunk
    if (isTRUE(options[["solution"]]) && !isTRUE(solution)) return("")

    if (!is_box(options)) return(x) # Not a box: return the chunk without changing it...
    
    box_type <- ifelse(is.null(options[["box.type"]]), ifelse(isTRUE(options[["solution"]]), "solution", "default"), options[["box.type"]])
    
    box_theme <- options[["box.types.list"]][[box_type]]
    
    box_theme[c("body", "header")] <- lapply(box_theme[c("body", "header")], to_css_colour)
    
    box_title <- options[["box.title"]]
    if (is.null(box_title)) box_title <- box_theme[["title"]]

    panel_class <- sprintf("class = \"panel\" style = \"background-color:%s; border:2px solid %s;\"", box_theme[["body"]][["fill"]], box_theme[["header"]][["fill"]])
    
    panel_body <- sprintf("<div class=\"panel-body\" style = \"color:%s!important;\">%s</div>", box_theme[["body"]][["colour"]], paste0(x, collapse = "\n"))

    box_icon <- box_theme[["icon"]]

    box_collapse <- options$box.collapse
    
    if (is.null(box_collapse)) {
      box_collapse <- box_theme[["collapse"]]
    }
    
    if (!is.null(box_collapse)) {
      panel_body <- sprintf("<div id=\"%s\" class=\"panel-collapse collapse%s\">%s</div>", options$label, ifelse(isTRUE(box_collapse), "", " in"), panel_body)
      box_title <- sprintf("<a class = \"%s\" style=\"color: %s;\" data-toggle=\"collapse\" href=\"#%s\">%s</a>", ifelse(isTRUE(box_collapse), "collapsed", ""), box_theme[["header"]][["colour"]], options$label, box_title)
    }
    
    if (is.null(box_title)) panel_header <- ""
    else panel_header <- sprintf("<div class=\"panel-heading\" style=\"background-color:%s; color:%s!important;\"><h4 class=\"panel-title\">%s%s</h4></div>",
                                 box_theme[["header"]][["fill"]], box_theme[["header"]][["colour"]], box_icon, box_title)

    sprintf("\n\n<div class=\"panel-group\"><div %s>%s%s</div></div>\n\n", panel_class, panel_header, panel_body)

  }
  
  orig_processor <- format$post_processor
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    orig_processor(metadata, input_file, output_file, clean, verbose)
    new_name = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), ifelse(solution, solution_suffix, question_suffix), ".html")
    file.rename(output_file, new_name)
    new_name
  }
  
  format$pre_knit <-  function(input, ...) {
    knitr::opts_chunk$set(box.types.list = default_box_themes)
    knitr::opts_hooks$set(solution = function(options) {
      if (!isTRUE(solution)) {
        options$eval <- FALSE
      } 
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