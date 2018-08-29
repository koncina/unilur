#' Define common settings for pdf and HTML output format
#' 
#' Set the post_processor, pre_knit as well as the solution theme in opts_template (which are common to all unilur formats)
#'
#' @param format a format to adapt (rmarkdown::html_document or rmarkdown::pdf_document)
#' 
#' @param suffix Suffix which is added to the filename (to generate two separate files from the same source without overwriting)
#' 
#' @return R Markdown output format
#' 
unilur_base <- function(format, is_solution_format, suffix) {
  
  format$knitr$knit_hooks$chunk <- function(x, options) {
    
    is_solution_chunk <- isTRUE(options[["solution"]])
    
    if (is_solution_chunk && !is_solution_format) {
      #TODO: add argument or try to handle this better
      if ("exam=yes" %in% format$pandoc$args && is.numeric(options[["answer.lines"]])) {
        return(paste0("\n\\fillwithdottedlines{", options[["answer.lines"]], "in}\\vspace{2em}\n"))
      } else {
        return("")
      }
    }
    
    box_options <- c("box.title", "box.body", "box.header", "box.icon", "box.collapse")
    
    if (!any(names(options) %in% box_options)) return(x) # Not a box: return the chunk without changing it...
    
    # Get the box theme
    box_theme <- do.call(set_box_theme, setNames(options[box_options], gsub("^box\\.", "", box_options)))
    
    # Extract the title of the box: chunk option or default in the theme
    box_theme[["title"]] <- options[["box.title"]] %n% box_theme[["title"]]
    
    switch (format[["pandoc"]][["to"]],
            html = boxify_html(x, options, box_theme),
            latex = boxify_latex(x, options, box_theme),
            x
    )
  }
  
  
  orig_processor <- format$post_processor
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (is.function(orig_processor)) orig_processor(metadata, input_file, output_file, clean, verbose)
    new_name = paste0(tools::file_path_sans_ext(output_file), suffix, ".", tools::file_ext(output_file))
    print("aaaaaaaaaaaaa")
    print(output_file)
    print(new_name)
    file.rename(output_file, new_name)
    new_name
  }
  
  format$pre_knit <-  function(input, ...) {
    knitr::opts_hooks$set(solution = function(options) {
      # Using merge_list: opts.label will not work here
      options <- knitr:::merge_list(knitr::opts_template$get("solution"), options)
      # No need to evaluate solutions if we don't show them...
      # TODO: might cause troubles on dependencies: not showing the solution but using it even for questions...
      if (!is_solution_format) {
        options$eval <- FALSE
      } 
      options
    })
  }
  
  format$knitr$opts_template <- list(solution = list(box.title = "solution",
                                                     box.body = list(fill = "#ACFFAF4C",
                                                                     colour = "black"),
                                                     box.header = list(fill = "#ACFFAFFF",
                                                                       colour = "#456646FF"),
                                                     box.collapse = FALSE
  )
  )
  
  format
}