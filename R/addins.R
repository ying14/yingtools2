

convert_winpath <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (text=="") {
    text <- suppressMessages(read.clipboard())
  }
  newtext <- gsub("\\\\","/",text)
  rstudioapi::insertText(newtext)
}

set_line_break_after_comma_if_4096_chars <- function(pd_flat) {
  pd2 <- pd_flat %>%
    mutate(
      is_comma = token == "','",
      row = row_number(),
      group = cumsum(lag(is_comma, default = TRUE))
    )
  pd3 <- pd2 %>%
    group_by(group) %>%
    summarize(
      nchar = sum(nchar(text)),
      first_row = first(row),
      last_row = last(row),
      text.all = paste(text, collapse = "")
    ) %>%
    ungroup() %>%
    mutate(
      cumsum = accumulate(nchar, ~ ifelse(.x + .y < 3096, .x + .y, .y)),
      newgroup = c(FALSE, diff(cumsum) <= 0)
    )
  rows_that_need_newline <- pd3$first_row[pd3$newgroup]
  pd_flat$lag_newlines <- 0L
  pd_flat$lag_newlines[rows_that_need_newline] <- 1L
  return(pd_flat)
}

set_line_break_after_comma_if_4096_chars_style <- function() {
  styler::create_style_guide(
    line_break = tibble::lst(set_line_break_after_comma_if_4096_chars),
    style_guide_name = "yingtools2::set_line_break_after_comma_if_4096_chars_style@https://github.com/ying14/yingtools2",
    style_guide_version = read.dcf(here::here("DESCRIPTION"))[, "Version"]
  )
}


#' Formats the highlighted selection such that no line exceeds 4096 characters.
#'
#' @return
#' @export
style_selection_4096 <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (all(nchar(text) == 0)) {
    abort("No code selected")
  }
  out <- styler::style_text(text, style = set_line_break_after_comma_if_4096_chars_style)

  rstudioapi::modifyRange(context$selection[[1]]$range,
    paste0(c(
      out,
      if (context$selection[[1]]$range$end[2] == 1) ""
    ), collapse = "\n"),
    id = context$id
  )
}
