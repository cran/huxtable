# huxtable 0.3.0.9000 (development version on github)

* New convenience functions `insert_row` and `insert_column`.
* `latex_float` property allows you to change positioning in LaTeX.

# huxtable 0.3.0

* New borders argument for huxreg, gives borders in sensible places.
* Allow more flexible caption positioning with `caption_pos`.
* New `set_default_properties` function to set default properties for new huxtables.
* Fix compatibility with dplyr 0.6.0.

# huxtable 0.2.2

* Fix a bug that could lead to wrong significance stars in `huxreg`.

# huxtable 0.2.1

* Compatibility with dplyr 0.6.0.
* Use ~ for decimal padding in LaTeX.

# huxtable 0.2.0

* New `huxreg` function to convert a list of models to a huxtable.
* New set_* interface allowing column ranges, expressions a la `subset`, and filling in values by row.
* Replacement methods `$<-`, `[<-` and `[[<-` now work better.
* New function `set_cell_properties` to set multiple properties on cells.
* `evens`, `odds`, `everywhere`, `every(n, from)`, `final(n)`, `where(cond)`: 
  convenience functions to select rows, columns and cells.
* Export to Word/Powerpoint via `ReporteRs`.
* Huxtable now supports dplyr verbs like `filter` and `select`.
* Exported function `guess_knitr_output_format`.
* Ability to set border colors.
* Prevent overlapping row/colspans.
* Expanded introduction and new vignette for `huxreg`.
* Numerous bugs have been fixed and replaced with new, more advanced bugs.

## Breaking changes

* `theme_minimal` has been renamed `theme_basic` to avoid a name clash with `ggplot2`.

# huxtable 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* First CRAN release.



