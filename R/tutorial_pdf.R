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
#' @param suffix Suffix which is added to the filename (default is '_question' for 'unilur::tutorial_pdf' and '_solution' for 'unilur::tutorial_pdf_solution')
#' 
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' 
#' @export
tutorial_pdf <- function(solution = FALSE,
                         suffix = "_question",
                         pandoc_args = NULL,
                         ...
) {
  
  template <- system.file("rmd", "tutorial_pdf", "template.tex",
                          package = "unilur")
  pandoc_args <- c(pandoc_args, "--variable", "geometry:margin=1in")  # Adjusts the margin
  pandoc_args <- c(pandoc_args, "--variable", "graphics=yes")         # Enables rescaling of too big graphics

  format <- rmarkdown::pdf_document(pandoc_args = pandoc_args, template = template, ...)
  
  format <- unilur_base(format, isTRUE(solution), suffix)
  format
}


#' @rdname tutorial_pdf
#' @export
tutorial_pdf_solution <- function(suffix = "_solution", ...) {
  tutorial_pdf(solution = TRUE, suffix = suffix, ...)
}

boxify_latex <- function(x, options, box_theme) {
  colour_names <- c("body.fill", "body.colour", "header.fill", "header.colour")
  xcolor_def <- rapply(box_theme,
                       function(i) apply(i, 2, to_latex_rgb),
                       classes = "box_colour",
                       how = "unlist")[colour_names]
  xcolor_def <- paste0("\\definecolor{", options[["label"]], ".", colour_names, "}", xcolor_def)
  
  alpha <- round(100 * rapply(box_theme, function(i) i["alpha",], classes = "box_colour", how = "unlist")[colour_names] / 255, 0)
  colour_names <- paste0(options[["label"]], ".", colour_names, "!", alpha)
  
  stopifnot(length(colour_names) == 4)
  
  box_begin <- do.call(sprintf, as.list(c("\n\\cboxs[%s]{%s}{%s}{%s}{%s}\n", paste0(box_theme[["title"]], ""), colour_names)))
  
  paste0(c("\n", xcolor_def, "\n", box_begin, x, "\n\\cboxe\n"), collapse = "\n")
}

#' Helper function to construct the colour definition in latex (xcolor)
#' 
#' Builds up the string `\{RGB\}\{r, g, b\}` which needs to be completed: `\\definecolor\{name\}\{RGB\}\{r, g, b\}`
#' 
#' @param x a vector containing the rgb colour values
to_latex_rgb <- function(x) {
  do.call(sprintf, as.list(c("{RGB}{%s, %s, %s}", x)))
}


