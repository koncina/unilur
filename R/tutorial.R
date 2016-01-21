# Reading the yaml header of fileName
# Adapted from http://stackoverflow.com/questions/23622307/strip-yaml-from-child-docs-in-knitr

extract_yaml = function(fileName) {
  require(yaml)
  file_content <- readLines(con = fileName, encoding = "UTF-8")
  yaml_pattern <- "[-]{3}|[.]{3}"
  limits_yaml <- grep(pattern = yaml_pattern, x = file_content)[1:2]
  indices_yaml <- seq(
    from = limits_yaml[1] + 1,
    to = limits_yaml[2] - 1
  )
  yaml <- mapply(
    FUN = function(i) {yaml.load(string = i)},
    file_content[indices_yaml],
    USE.NAMES = FALSE
  )
  return(yaml)
}

# knit replacement to enable the rendering of two files in 1 click
# (with and without solution chunks).
# Insert a call to knit in the yaml header of the Rmd file (below the title)
# knit: unilur::knit
# Depending on the variable "solution" in the yaml header it will render:
#  "solution: yes" = Only the file with solution chunks
#  "solution: no" = Only the file without solution chunks
#  "solution: wow" = Both files (with and without) showing the first rendered without
#  "solution: wwo" = Both files (with and without) showing the first rendered with
# A suffix defined in the yaml header is added to the filename with solution chunks
#  (suffix: "_solution" is the default)

#' @export
knit <- (function(inputFile, encoding, test = FALSE) {

  # Maybe there is a cleaner way to pass variables to knit?
  yamlHeader <- extract_yaml(inputFile)

  if (isTRUE(yamlHeader$solution)) tut_opt <- list(m = "file with", s = TRUE)
  else if (yamlHeader$solution == "wwo") tut_opt <- list(m = "files with and without", s = c(TRUE, FALSE))
  else if (yamlHeader$solution == "wow") tut_opt <- list(m = "files with and without", s = c(FALSE, TRUE))
  else tut_opt <- list(m = "file without", s = FALSE)

  message(paste("Rendering", tut_opt$m, "solutions"))

  if (!is.null(yamlHeader$suffix)) tut_opt$suffix <- yamlHeader$suffix
  else tut_opt$suffix <- "_solution"

  for (s in tut_opt$s) {
    rmarkdown::render(inputFile, encoding = encoding,
                      output_file = paste0(gsub("(.*)(\\.[[:alnum:]]+$)", "\\1", inputFile), ifelse(s, tut_opt$suffix, ""), ".pdf"),
                      output_format = "unilur::tutorial",
                      output_options=list(
                       solution = s
                      )
    )
  };
})

#' @export
tutorial <- function( keep_tex = TRUE,
                      includes = NULL,
                      fig_width = 6,
                      fig_height = 4,
                      fig_crop = TRUE,
                      solution = TRUE,     # Turn ON or OFF the rendering of solution chunks
                                           # Values in the yaml header are:
                                           # yes, no, wwo, wow (wwo = with and without
                                           #                    wow = without and with
                                           #                    first is shown by RStudio)
                      only_asis = FALSE,   # Restrict the solution rendering to the asis boxes
                                           # to force the use of the original MD to latex engine
                                           # Pandoc?
                      suffix = "_solution" # Used in the knit function to add a suffix to the pdf name
) {
  header <- system.file("rmarkdown", "templates", "tutorial", "resources", "header.tex",
                        package = "unilur")

  includes = list(in_header = header)

  format <- rmarkdown::pdf_document(keep_tex = keep_tex,
                                    includes = includes)

  hook_chunk <- function(x, options) {
    if (isTRUE(options$solution) && !isTRUE(solution)) return("")
    if (isTRUE(only_asis) && options$engine != "asis") return(paste0(x, "\n", collapse = "\n"))
    if (!is.null(options$box)) {
      if (is.null(options$boxtitle)) {
        beginbox <- paste0("\\begin{cbox}{", options$box, "}")
      } else {
        beginbox <- paste0("\\begin{cbox}[", options$boxtitle, "]{", options$box, "}")
      }
      x <- paste0(c(beginbox, x, "\\end{cbox}\n"), collapse = "\n")
    }
    if (isTRUE(options$solution) && isTRUE(solution)) return(paste0(c("\\begin{solution}", x, "\\end{solution}\n"), collapse = "\n"))
    return(x)
  }

  hook_input <- function(x, options) {
    paste0(c("\n\\begin{mdframed}[style = input]", "\\begin{Verbatim}[commandchars=\\\\\\{\\}]", knitr:::hilight_source(x, "latex", options), "\\end{Verbatim}", "\\end{mdframed}\n"), collapse = "\n")
  }

  hook_output <- function(x, options) {
    # Using trimws to remove the last newline character
    # which is messing up page breaks in latex...
    paste0(c("\n\\begin{Verbatim}[commandchars=\\\\\\{\\}]", trimws(x, which = "right"), "\\end{Verbatim}\n"), collapse = "\n")
  }

  hook_plot <- function(x, options) {
    if (isTRUE(only_asis)) return(x)
    # determine caption (if any)
    caption <- ifelse(is.null(options$fig.cap),
                      "",
                      paste("\\captionof{figure}{", options$fig.cap, "}\n", sep = ""))
    # return the latex
    paste(sprintf("\\includegraphics{%s}\n%s\n", gsub("\\\\", "/", x), caption))
  }

  if (!isTRUE(only_asis)) {
    format$knitr$knit_hooks$source  <- hook_input
    format$knitr$knit_hooks$output  <- hook_output
    format$knitr$knit_hooks$plot <- hook_plot
  }
  format$knitr$knit_hooks$chunk  <- hook_chunk
  format
}
