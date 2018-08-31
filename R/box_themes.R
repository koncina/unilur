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

# Generate code to integrate icons
icon_fa <- function(icon_name) {
  knitr::asis_output(paste0("<i class=\"fa fa-", icon_name, "\"></i>"),
                     meta = list(rmarkdown::html_dependency_font_awesome()))
}


get_box_icon <- function (x, ...) {
  UseMethod("get_box_icon", x)
}

get_box_icon.character <- function(x, ...) {
  x <- switch(stringr::str_extract(x, "^[^-]+-"),
              `fa-` = list(paste0("<i class=\"fa ", x, "\"></i>"),
                           meta = list(rmarkdown::html_dependency_font_awesome())),
              `ion-` = list(paste0("<i class=\"ion ", x, "\"></i>"),
                            meta = list(rmarkdown::html_dependency_ionicons())),
              stop("icon name must start with 'fa-' or 'ion-'", call. = FALSE))
  do.call(knitr::asis_output, x)
}

# TODO: knit_asis is very permissive while knit_icon might only be relevant for the icon package...
# Should I support emoji?
get_box_icon.knit_asis <- function(x, ...) {
  x
}

get_box_icon.NULL <- function(x, ...) {
  ""
}
  
