# Release Notes

## Version 0.2

- `unilur::tutorial` was replaced by `unilur::tutorial_pdf` and `unilur::tutorial_html`
- Removed `unilur::knit`:
    * Output filename is defined in the `unilur::tutorial_pdf` or `unilur::tutorial_html` format (`<basename>_question.<ext>` or `<basename>_solution.<ext>`).
    * Allows to use the interactive `knit` menu in Rstudio in particular with custom formats now showing up in Knit menu (tested in Rstudio 0.99.1197 preview)
- Isolated the `unilur::tutorial` option `exam` and created a new `unilur::examen_pdf` format
