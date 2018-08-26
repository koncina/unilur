NULL

set_box_colour <- function(fill = NULL, colour = NULL, color = NULL) {
   # from the ggplot2 element function: we can use colour or color
   if (!is.null(color))
     colour <- color
   if (is.null(fill)) fill <- "white"
   if (is.null(colour)) colour <- "black"
   structure(col2rgb(c(fill = fill, colour = colour), alpha = TRUE), 
             class = c("box", "box_colour"))
   
 }

set_box_theme <- function(title = NULL, body = NULL, header = NULL, icon = NULL, collapse = NULL) {

  if (is.null(body)) body <- list(fill = "#F2F2F2")
  body <- do.call("set_box_colour", body)
  if (is.null(header)) {
    header <- body
    header[,"fill"][1:3] <- round(header[,"fill"][1:3]*0.8)
  } else {
    header <- do.call("set_box_colour", header)
  }
  
  check_box_colours(body, header)
  
  if (is.null(icon)) icon <- ""
  
  structure(list(
    body = body,
    header = header,
    title = title,
    icon = icon,
    collapse = collapse),
  class = c("box", "box_theme"))
}


check_box_colours <- function(...) {
  lapply(list(...), function(x) stopifnot(class(x) == c("box", "box_colour")))
}

to_css_colour <- function(x) {
  stopifnot(class(x) == c("box", "box_colour"))
  x["alpha",] <- round(x["alpha",] / 255, 1)
  apply(x, 2, function(i) do.call(sprintf, as.list(c("rgba(%s, %s, %s, %s)", i))))
}

#' Add a new unilur box theme
#'
#' Define custom body and/or header colours, collasible status and icons for new box types.
#' 
#' @param type_name The name of the type to be used in the `box.type` chunk option
#' 
#' @param title The default title of the box. Setting `box.title` chunk option overrides this setting. If `NULL`, no header will be rendered.
#' 
#' @param body a `list` to define the colours of the body. The list should contain up to two elements: `fill` and `colour` adjusted to a colour name, hexadecimal string or a positive integer as supported by the `col2rgb()` function.
#' 
#' @param header a `list` to define the colours of the header. The list should contain up to two elements: `fill` and `colour` adjusted to a colour name, hexadecimal string or a positive integer as supported by the `col2rgb()` function.
#' 
#' @param icon icon appearing in the box header.
#' 
#' @param collapse `logical` or `NULL`. If `NULL` a non collapsible box is drawn. If `logical`, the box is collapsible: `TRUE` it is collapsed and `FALSE` uncollapsed.
#' 
#' @return Returns `NULL` while adding the definition to your `knitr` options.
#' 
#' @export
add_box_type <- function(type_name, title = NULL, body = NULL, header = NULL, icon = NULL, collapse = NULL) {
  stopifnot(is.character(type_name))
  box_style <- set_box_theme(title, body, header, icon, collapse)
  stopifnot(class(box_style) == c("box", "box_theme"))
  box_types <- knitr::opts_knit$get("box.types.list")
  box_types[type_name] <- list(box_style)
  knitr::opts_knit$set(box.types.list = box_types)
}

