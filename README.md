
# unilur

## Overview

**`unilur`** is a R package to help writing tutorials, practicals or
examination papers with [rmarkdown](http://rmarkdown.rstudio.com/).

With `unilur` you can render the following outputs from a single
rmarkdown file:

  - a PDF or HTML file with the exam or tutorial questions (answers
    remaining hidden) as a PDF or HTML file.
  - the same output as above but with the answers appearing in coloured
    boxes.
  - a cleaned up Rmarkdown file (without answers) to be filled by the
    candidate.

In addition, you will be able to:

  - Create coloured boxes to highlight some markdown or *R* content.
  - Create examination papers with
      - multiple choice questions
      - a candidate identification form
      - dotted lines placeholders to fill in answers

## Installation

The source code is hosted on
[github](https://github.com/koncina/unilur). To install the *R* package,
run the following code:

``` r
devtools::install_github("koncina/unilur")
```

The package contains example templates illustrating some of the
possibilities.

## Usage

Use one of the following rmarkdown output formats to start writing your
tutorials or examination papers:

  - `unilur::tutorial_html`
  - `unilur::tutorial_pdf`
  - `unilur::tutorial_html_solution`
  - `unilur::tutorial_pdf_solution`
  - `unilur::answer_rmd`

## Articles

You can read [these
articles](http://koncina.github.io/unilur/articles/index.html) to find
more informations on how to use the `unilur` formats:

  - Writing tutorials or practicals
  - Writing examination papers
  - Create custom box themes
