#' Convert to a tutorial answer Rmarkdown file
#'
#' Format to generate a Rmd file with solutions chunks being replaced by empty answer chunks.
#' 
#' @param yaml a list to override the \code{title}, \code{author} and \code{output} of the answer Rmd file.
#' 
#' @param suffix Suffix which is added to the filename (default is '_answer')
#' 
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' 
#' @export
answer_rmd <- function(yaml = NULL, suffix = "_answer") {
  # Here we implement a fake knitting: I don't think it's possible to avoid using pandoc
  # We will generate a simple md document (that will be deleted) and use a chunk hook
  # to disable any code evaluation (to speed up the unnecessary knitting)
  # We generate the Rmd file using a simple regex rule in the post-processor
  e <- new.env()
  e$default_yaml <- list(
    title = "My answers",
    author = "My name",
    output = "html_document"
  )
  yaml <- check.options(yaml, name.opt = "default_yaml", envir = e)
  
  format <- rmarkdown::md_document()
  
  answer_output <- NULL
  
  format$pre_knit <-  function(input, ...) {
    # Creating answer Rmd
    rmd <- paste(readLines(input), collapse = "\n")
    pattern <- "\\n *``` *{.*(?i)solution(?-i) *= *(?i)true(?-i).*} *\\n[\\s\\S]*?\\n *``` *"
    replacement <- "\n```{r}\n# Write your answer here\n```\n"
    rmd <- gsub(pattern, replacement, rmd, perl = TRUE)
    # Removing the chunks with either echo or eval set to FALSE
    pattern <- "\\n *``` *{.*(?i)(eval|echo|include)(?-i) *= *(?i)false(?-i).*} *\\n[\\s\\S]*?\\n *``` *"
    replacement <- ""
    rmd <- gsub(pattern, replacement, rmd, perl = TRUE)
    # Replacing the original header by a custom one...
    pattern <- "^--- *\\n[\\s\\S]*?\\n *--- *"
    # header <- "---\ntitle: \"My answers\"\nauthor: \"My name\"\ndate: `r format(Sys.time(), \"%d %B, %Y\")`\noutput:\n\tunilur::tutorial_pdf:\n\t\tanswer: yes\n---"
    # Tab character seems not accepted by the custom yaml parser... Try to use the rmarkdown parser?
    #header <- sprintf("---\ntitle: \"%s\"\nauthor: \"%s\"\ndate: '`r format(Sys.time(), \"%%d %%B, %%Y\")`'\noutput: %s\n---",
    #                  yaml[["title"]], yaml[["author"]], yaml[["output"]])
    
    header <- do.call(sprintf, c(fmt = "---\ntitle: \"%s\"\nauthor: \"%s\"\ndate: '`r format(Sys.time(), \"%%d %%B, %%Y\")`'\noutput: %s\n---",
                                 yaml[c("title", "author", "output")]))
    
    rmd <- gsub(pattern, header, rmd, perl = TRUE)
    
    answer_output <<- paste0(tools::file_path_sans_ext(input), suffix, ".Rmd")
    writeLines(rmd, answer_output)
    
    knitr::opts_hooks$set(eval = function(options) {
      options$eval <- FALSE
      options
    })
  }
  
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # Removing knitted output_file we don't want and using the already created answer_output as the generated output_file
    unlink(output_file)
    answer_output
  }
  format
}
