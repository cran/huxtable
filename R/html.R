

#' @export
#'
#' @rdname to_html
#'
print_html <- function(ht, ...) cat(to_html(ht, ...))


#' Create HTML representing a huxtable
#'
#' These functions print or return an HTML table.
#'
#' @param ht A huxtable.
#' @param ... Arguments to pass to methods. Not currently used.
#'
#' @return `to_html` returns an HTML string. `print_html` prints the string and returns `NULL`.
#' @export
#'
#' @family printing functions
#'
#' @examples
#' ht <- hux(a = 1:3, b = letters[1:3])
#' to_html(ht)
to_html <- function (ht, ...) UseMethod("to_html")


#' @export
#'
#' @rdname to_html
#'
#' @return `print_notebook` prints HTML output suitable for use in an
#' RStudio interactive notebook.
print_notebook <- function(ht, ...) {
  print(rmarkdown::html_notebook_output_html(to_html(ht)))
}


#' @export
#' @rdname to_html
to_html.huxtable <- function(ht, ...) {
  check_positive_dims(ht)

  ## TABLE START ----------
  width <- width(ht)
  width_string <- if (is.na(width)) "" else {
    if (is.numeric(width)) width <- paste0(width * 100, "%")
    paste0("width: ", width)
  }

  margin_string <- switch(position(ht),
          "wrapleft"  = "margin-left: 0%; margin-right: 2em;",
          "wrapright" = "margin-left: 2em; margin-right: 0%;",
          "left"      = "margin-left: 0%; margin-right: auto;",
          "right"     = "margin-left: auto; margin-right: 0%;",
          "center"    = "margin-left: auto; margin-right: auto;"
        )

  height <- height(ht)
  height_string <- blank_where({
    h <- if (is.numeric(height)) paste0(height * 100, "%") else height
    sprintf("height: %s;", h)
  }, is.na(height))

  float_string <- switch(position(ht),
          "wrapleft"  = "float: left;",
          "wrapright" = "float: right;",
          ""
        )

  lab <- make_label(ht)
  id_string <- if (is.na(lab)) "" else sprintf(" id=\"%s\"", lab)

  quarto_attribute <- if (getOption("huxtable.quarto_process", FALSE)) {
    ""
  } else {
    "data-quarto-disable-processing=\"true\" "
  }
  table_start <- sprintf(paste0(
        '<table class="huxtable" %s',
        'style="border-collapse: collapse; border: 0px; ',
        'margin-bottom: 2em; margin-top: 2em; %s; %s %s %s"%s>\n'),
        quarto_attribute,
        width_string, margin_string, height_string, float_string, id_string)

  if (! is.na(cap <- make_caption(ht, lab, "html"))) {
    vpos <- if (grepl("top", caption_pos(ht))) "top" else "bottom"
    hpos <- get_caption_hpos(ht)

    if (! is.na(cap_width <- caption_width(ht))) {
      if (! is.na(as.numeric(cap_width))) {
        cap_width <- paste0(as.numeric(cap_width) * 100, "%")
      }
      cap_width <- sprintf("width: %s;", cap_width)
    } else {
      cap_width <- ""
    }
    cap <- sprintf('<caption style="caption-side: %s; text-align: %s;%s">%s</caption>',
          vpos, hpos, cap_width, cap)
    table_start <- paste0(table_start, cap)
  }

  # COLUMN TAGS -----------
  col_widths <- col_width(ht)
  # NAs become empty strings
  empty_cw <- is.na(col_widths)
  if (is.numeric(col_widths)) col_widths <- sprintf("%s%%", col_widths * 100)
  cols_html <- sprintf(' style="width: %s"', col_widths)
  cols_html <- blank_where(cols_html, empty_cw)
  cols_html <- sprintf("<col%s>", cols_html)
  cols_html <- paste0(cols_html, collapse = "")

  ## CELLS ----------------
  display_cells <- display_cells(ht)

  rowspan <- rowspan(ht)
  rowspan <- blank_where(sprintf(' rowspan="%s"', rowspan), rowspan == 1)
  colspan <- colspan(ht)
  colspan <- blank_where(sprintf(' colspan="%s"', colspan), colspan == 1)

  valign  <- sprintf("vertical-align: %s;", valign(ht))
  align   <- sprintf(" text-align: %s;", real_align(ht))
  wrap    <- sprintf(" white-space: %s;", ifelse(wrap(ht), "normal", "nowrap"))

  # get_visible_borders() data is in "real cell" position.
  # But we just want to grab the original data
  # and apply it
  border_css <- compute_border_css(ht)

  add_pts <- function (x) if (is.numeric(x)) sprintf("%.4gpt", x) else x
  padding <- sprintf(" padding: %s %s %s %s;",
          add_pts(top_padding(ht)),
          add_pts(right_padding(ht)),
          add_pts(bottom_padding(ht)),
          add_pts(left_padding(ht))
        )

  bg_color <- background_color(ht)
  bg_color <- format_color(bg_color) # NA becomes white, as it happens
  bg_color <- sprintf(" background-color: rgb(%s);", bg_color)
  bg_color <- blank_where(bg_color, is.na(background_color(ht)))

  bold <- ifelse(bold(ht), " font-weight: bold;", " font-weight: normal;")
  italic <- ifelse(italic(ht), " font-style: italic;", "")

  font <- sprintf(" font-family: %s;", font(ht))
  font <- blank_where(font, is.na(font(ht)))
  font_size <- sprintf(" font-size: %.4gpt;", font_size(ht))
  font_size <- blank_where(font_size, is.na(font_size(ht)))

  style   <- paste0("style=\"", valign, align, wrap, border_css,
        padding, bg_color, bold, italic, font, font_size, "\"")
  th_td <- matrix("td", nrow(ht), ncol(ht))
  th_td[header_rows(ht), ] <- "th"
  th_td[, header_cols(ht)] <- "th"
  cell_start <- sprintf("<%s%s%s %s>", th_td, rowspan, colspan, style)
  cell_end   <- sprintf("</%s>", th_td)
  contents <- clean_contents(ht, output_type = "html")

  rot <- rotation(ht)
  rot <- (rot %% 360) * -1 # HTML goes anticlockwise
  rot_div <- sprintf('<div style="transform: rotate(%.4gdeg); white-space: nowrap;">', rot)
  # special-case straight up/down to be handled by writing-mode.
  # this will probably break on non-LTR text, but before it was hard to use anyway.
  rot_div[rot == -270] <- sprintf('<div style="writing-mode: vertical-rl;">')
  rot_div[rot == -90]  <- sprintf(
        '<div style="writing-mode: vertical-rl; transform: rotate(180deg);">')

  rot_div_end <- rep("</div>", length(rot_div))
  rot_div <- blank_where(rot_div, rot == 0)
  rot_div_end <- blank_where(rot_div_end, rot == 0)

  color <- text_color(ht)
  color <- format_color(color)
  color_span <- sprintf('<span style="color: rgb(%s);">', color)
  color_span <- blank_where(color_span, is.na(text_color(ht)))
  color_span_end <- rep("</span>", length(color))
  color_span_end <- blank_where(color_span_end, is.na(text_color(ht)))

  cells_html <- paste0(cell_start, rot_div, color_span, contents,
                  color_span_end, rot_div_end, cell_end)
  cells_html <- blank_where(cells_html, display_cells$shadowed)

  # add in row tags
  dim(cells_html) <- dim(ht)
  cells_html <- apply(cells_html, 1, paste0, collapse = "")
  row_heights <- row_height(ht)
  if (is.numeric(row_heights)) {
    row_heights <- 100 * row_heights / sum(row_heights)
    row_heights <- sprintf("%.3g%%", row_heights) # %3g prints max 1 decimal place
  }
  row_heights <- sprintf(' style="height: %s;"', row_heights)
  row_heights <- blank_where(row_heights, is.na(row_height(ht)))
  tr <- sprintf("<tr%s>\n", row_heights)
  cells_html <- paste0(tr, cells_html, rep("</tr>\n", length(tr)))
  cells_html <- paste0(cells_html, collapse = "")

  res <- paste0(table_start, cols_html, cells_html, "</table>\n")
  return(res)
}


#' Create border css for each cell
#'
#' This returns a matrix of border CSS. Cells with spans > 1 get the
#' borders from the correct position.
#'
#' @param ht A huxtable.
#'
#' @return A character array of border CSS, ending in a semicolon
#' @noRd
compute_border_css <- function (ht) {
  top_row <- c(row(ht))
  bottom_row <- top_row + c(rowspan(ht)) - 1
  left_col <- c(col(ht))
  right_col <- left_col + c(colspan(ht)) - 1

  dc <- display_cells(ht)
  dc <- as.matrix(dc[, c("row", "col", "end_row", "end_col")])
  # we don't use display_row because shadowed cells will be blanked anyway.
  top_matrix    <- dc[, c("row", "col"), drop = FALSE]
  left_matrix   <- top_matrix
  bottom_matrix <- dc[, c("end_row", "col"), drop = FALSE]
  right_matrix  <- dc[, c("row", "end_col"), drop = FALSE]

  # We don't use get_visible_borders, because borders in the middle of a
  # span won't be used anyway.
  tb <- brdr_thickness(top_border(ht))   [top_matrix]
  rb <- brdr_thickness(right_border(ht)) [right_matrix]
  bb <- brdr_thickness(bottom_border(ht))[bottom_matrix]
  lb <- brdr_thickness(left_border(ht))  [left_matrix]

  tbs <- top_border_style(ht)   [top_matrix]
  rbs <- right_border_style(ht) [right_matrix]
  bbs <- bottom_border_style(ht)[bottom_matrix]
  lbs <- left_border_style(ht)  [left_matrix]

  tbc <- top_border_color(ht)   [top_matrix]
  rbc <- right_border_color(ht) [right_matrix]
  bbc <- bottom_border_color(ht)[bottom_matrix]
  lbc <- left_border_color(ht)  [left_matrix]

  format_border_color_css <- function (col, pos) {
    x <- sprintf(" border-%s-color: rgb(%s);", pos, format_color(col))
    blank_where(x, is.na(col))
  }
  tbc <- format_border_color_css(tbc, "top")
  rbc <- format_border_color_css(rbc, "right")
  bbc <- format_border_color_css(bbc, "bottom")
  lbc <- format_border_color_css(lbc, "left")

  if (any(tbs == "double" & tb > 0 & tb < 3) ||
      any(rbs == "double" & rb > 0 & rb < 3) ||
      any(bbs == "double" & bb > 0 & bb < 3) ||
      any(lbs == "double" & lb > 0 & lb < 3)
  ) {
    warning("border_style set to \"double\" but border less than 3 points")
  }

  border_css <- sprintf(
    " border-style: %s %s %s %s; border-width: %.4gpt %.4gpt %.4gpt %.4gpt;",
    tbs, rbs, bbs, lbs, tb, rb, bb, lb
  )
  border_css <- paste0(border_css, sprintf("%s %s %s %s", tbc, rbc, bbc, lbc))

  return(border_css)
}
