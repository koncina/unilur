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
#' @param suffix Suffix which is added to the filename (default is '_question' for 'unilur::tutorial_html' and '_solution' for 'unilur::tutorial_html_solution')
#' 
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' 
#' @export
tutorial_html <- function(solution = FALSE,
                          suffix = "_question",
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
  
  format <- unilur_base(format, isTRUE(solution), suffix)
  
  format
}

#' @export
#' 
#' @rdname tutorial_html
#' 
tutorial_html_solution <- function(suffix = "_solution", ...) {
  tutorial_html(solution = TRUE, suffix = suffix, ...)
}

boxify_html <- function(x, options, box_theme) {
  box_theme[c("body", "header")] <- lapply(box_theme[c("body", "header")], to_css_colour)
  is_box_collapsed <- options[["box.collapse"]] %n% box_theme[["collapse"]]
  box_title <- box_theme[["title"]]
  
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

to_css_colour <- function(x) {
  stopifnot(class(x) == c("box", "box_colour"))
  x["alpha",] <- round(x["alpha",] / 255, 1)
  apply(x, 2, function(i) do.call(sprintf, as.list(c("rgba(%s, %s, %s, %s)", i))))
}

html_dependency_tutorial <- function() {
  htmltools::htmlDependency(
    name = "tutorial",
    version = packageVersion("unilur"),
    src = system.file("rmd", "tutorial_html", "libs", package = "unilur"),
    stylesheet = c(
      "css/unilur.css"
    )
  )
}
