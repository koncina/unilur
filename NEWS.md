# Unilur 0.3.0.9000

## Changes

- All boxes in `tutorial_html` are now collapsible. Adjust the chunk option `box.collapse` to `TRUE` (collapsed), `FALSE` (collapsible but uncollapsed) or `NULL` (non collapsible box).
- **breaking change:** Removed the collapse setting from the yaml header: The option should be adjusted for each chunk (use `knitr::opts_chunk$set(collapse = TRUE)` to define the default setting).
- **breaking change:** Changed chunk options to adjust a custom box colour and title: use `box.colour` and `box.title` (instead of `box` and `boxtitle`)

# Unilur 0.3.0

- `unilur::tutorial_html` and `unilur::tutorial_html_solution` now renders solution and coloured boxes as [bootstrap panels](https://www.w3schools.com/bootstrap/bootstrap_panels.asp). In addition, solution boxes can be collapsed and expanded. The initial state can be adjusted using the `collapse` yaml option.
- Added `unilur::answer_rmd` to create a Rmd file with solutions being removed. This file can be used by students to fill in their answers.
- Improved knitting of files with `solution = FALSE` by disabling the evaluation of these chunks.

# Unilur 0.2.0

- `unilur::tutorial` was replaced by `unilur::tutorial_pdf` and `unilur::tutorial_html`
- Removed `unilur::knit`:
    * Output filename is defined in the `unilur::tutorial_pdf` or `unilur::tutorial_html` format (`<basename>_question.<ext>` or `<basename>_solution.<ext>`).
    * Allows to use the interactive `knit` menu in Rstudio in particular with custom formats now showing up in Knit menu (tested in Rstudio 0.99.1197 preview)
- Isolated the `unilur::tutorial` option `exam` and created a new `unilur::examen_pdf` format
