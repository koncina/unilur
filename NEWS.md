# Unilur 0.4.0.9000

## Changes

- add an option `exclude_chunk` to remove from `answer_rmd` specific chunks

## Breaking changes

- replace `question_suffix` and `solution_suffix` by `suffix` in `unilur::tutorial_*` or `unilur::tutorial_*_solution` to change the suffix appended to the rendered file.
- remove `box.colour` chunk option (replaced by `box.body` and `box.header` which accepts a list to define `fill` and `colour`)
- The automatic adjustement of header colour changes: `box.colour` was used to define the header colour and the colour of the body was derived from this value. From now, if `box.header` is omitted, the `fill` colour is derived from the `box.body` fill colour.
- rename chunk option `response.space` to `answer.lines` in `unilur::examen_pdf`.

## Changes

- use latex template shipped with rmarkdown and use includes to integrate unilur customisations.
- rewrite the format functions to deduplicate the source code.
- remove output and source hooks in pdf output: might again produce troubles with unexpected page breaks using boxes.
- redefine box chunk options: `box.title`, `box.body`, `box.header` and `box.collapse`. Colours for background and text can be set for the body and the header using a list (to provide the colours for `fill` and `colour`) 
- add support for icons in HTML output using the `box.icon` chunk option. Use font awesome names ("fa-*") or ionicon names ("ion-*") to specify the desired icon. Alternatively you can use the output of packages like [`icon`](https://github.com/ropenscilabs/icon).

# Unilur 0.4.0

## Breaking changes

- removed the collapse setting from the yaml header: The option should be adjusted for each chunk (use `knitr::opts_chunk$set(collapse = TRUE)` to define the default setting).
- changed chunk options to adjust a custom box colour and title: use `box.colour` and `box.title` (instead of `box` and `boxtitle`)

## Changes

- all boxes in `tutorial_html` are now collapsible. Adjust the chunk option `box.collapse` to `TRUE` (collapsed), `FALSE` (collapsible but uncollapsed) or `NULL` (non collapsible box).

## Bug fixes

- HTML widgets were not rendered

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
