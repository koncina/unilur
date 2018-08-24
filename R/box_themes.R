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

#' @export
add_box_type <- function(type_name, ...) {
  stopifnot(is.character(type_name))
  box_style <- set_box_theme(...)
  stopifnot(class(box_style) == c("box", "box_theme"))
  box_types <- knitr::opts_chunk$get("box.types.list")
  box_types[type_name] <- list(box_style)
  knitr::opts_chunk$set(box.types.list = box_types)
}

