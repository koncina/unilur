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

  is_solution_format <- isTRUE(solution)
  
  hook_chunk <- function(x, options) {
    is_solution_chunk <- isTRUE(options[["solution"]])

    if (is_solution_chunk && !is_solution_format) return("")

    box_options <- c("box.title", "box.body", "box.header", "box.icon", "box.collapse")
    
    if (!any(names(options) %in% box_options)) return(x) # Not a box: return the chunk without changing it...
    # Get the box theme
    
    box_options <- setNames(options[box_options], gsub("^box\\.", "", box_options))
    
    box_theme <- do.call(set_box_theme, box_options)
    
    box_theme[c("body", "header")] <- lapply(box_theme[c("body", "header")], to_css_colour)
    
    # Extract the title of the box: chunk option or default in the theme
    box_title <- options[["box.title"]] %n% box_theme[["title"]]
    is_box_collapsed <- options[["box.collapse"]] %n% box_theme[["collapse"]]
    
    # Extract icon
    # TODO: this might be enhanced
    # Use the knitr print method to avoid loosing the library dependencies
    invisible(capture.output({box_icon <- knitr::knit_print(box_theme[["icon"]])}))
    knitr::knit_meta_add(meta = attr(box_icon, "knit_meta"), label = options[["label"]])
    if (! "knit_asis" %in% class(box_icon)) box_icon <- paste0("<i class=\"icon\">", box_icon, "</i>")
    
    panel_class <- sprintf("class = \"panel\" style = \"background-color:%s; border:2px solid %s;\"",
                           box_theme[["body"]][["fill"]],
                           box_theme[["header"]][["fill"]])
    panel_body <- sprintf("<div class=\"panel-body\" style = \"color:%s!important;\">%s</div>",
                          box_theme[["body"]][["colour"]],
                          paste0(x, collapse = "\n"))
    
    # If box is collapsed add the bootstrap code
    if (!is.null(is_box_collapsed)) {
      panel_body <- sprintf("<div id=\"%s\" class=\"panel-collapse collapse%s\">%s</div>",
                            options[["label"]],
                            ifelse(is_box_collapsed, "", " in"),
                            panel_body)
      box_title <- sprintf("<a class = \"%s\" style=\"color: %s;\" data-toggle=\"collapse\" href=\"#%s\">%s</a>",
                           ifelse(is_box_collapsed, "collapsed", ""),
                           box_theme[["header"]][["colour"]],
                           options[["label"]],
                           box_title)
    }
    
    html_box_header <- {
      if (is.null(box_title))
        ""
      else
        sprintf("<div class=\"panel-heading\" style=\"background-color:%s; color:%s!important;\"><h4 class=\"panel-title\">%s%s</h4></div>",
              box_theme[["header"]][["fill"]], box_theme[["header"]][["colour"]], box_icon, box_title)
      }

    # Return the HTML code
    out <- sprintf("\n\n<div class=\"panel-group\"><div %s>%s%s</div></div>\n\n",
            panel_class, 
            html_box_header,
            panel_body)
    
    knitr::asis_output(out)
  }
  
  orig_processor <- format$post_processor
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    orig_processor(metadata, input_file, output_file, clean, verbose)
    new_name = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", output_file), ifelse(solution, solution_suffix, question_suffix), ".html")
    file.rename(output_file, new_name)
    new_name
  }
  
  format$pre_knit <-  function(input, ...) {
    knitr::opts_hooks$set(solution = function(options) {
      # Using merge_list: opts.label will not work here
      options <- knitr:::merge_list(knitr::opts_template$get("solution"), options)
      # No need to evaluate solutions if we don't show them...
      # TODO: might cause troubles on dependencies: not showing the solution but using it even for questions...
      if (!isTRUE(solution)) {
        options$eval <- FALSE
      } 
      options
    })
    
  }
  
  format$knitr$knit_hooks$chunk  <- hook_chunk
  
  format$knitr$opts_template <- list(solution = list(box.title = "solution", box.body = list(fill = "#ACFFAF4C", colour = "black"), box.header = list(fill = "#ACFFAFFF", colour = "#456646FF"), box.collapse = FALSE)
  )
  
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
