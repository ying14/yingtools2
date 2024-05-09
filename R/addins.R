


# custom Rstudio addins.
# these need to be declared in /inst/rstudio/addins.dcf.
# run the following to edit....
# rstudioapi::navigateToFile('inst/rstudio/addins.dcf')




convert_winpath <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (text=="") {
    text <- suppressMessages(read.clipboard())
  }
  newtext <- gsub("\\\\","/",text)
  rstudioapi::insertText(newtext)
}




selection_try_catch <- function()  {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (text=="") {
    text <- suppressMessages(read.clipboard())
  }
  newtext <- str_glue("tryCatch({{
  {text}
}},error=function(e) {{
  message('Error here!')
  print(e)
})")
  rstudioapi::insertText(newtext)
}


selection_benchmark <- function()  {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (text=="") {
    text <- suppressMessages(read.clipboard())
  }
  newtext <- str_glue("rbenchmark::benchmark(
  test1={{
    {text}
  }},
  test2={{

  }},
  replications = 1)")
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







remove_comments <- function(pd) {
  is_comment <- pd$token == "COMMENT"
  pd[!is_comment, ]
}
add_semi_colon <- function(pd) {
  is_curly_expr <- oneliner:::is_curly_brace_expr(pd)
  if (is_curly_expr || all(pd$token %in% c("expr", "expr_or_assign_or_help"))) {
    conditions <- list(
      if (is_curly_expr) {which(pd$token != "'}'")},
      if (is_curly_expr) {which(styler:::lag(pd$token != "'{'", n = 1))},
      which(pd$lag_newlines > 0L)) %>% compact()
    needs_semicolon <- reduce(conditions, intersect, .init = which(styler:::lag(pd$token == "expr")))
    if (length(needs_semicolon) < 1L) {
      return(pd)
    }
    # browser()
    semicolumn_pds <- styler:::create_tokens(rep("';'", length(needs_semicolon)),  rep(";", length(needs_semicolon)),
                                             pos_ids = map_dbl(needs_semicolon, styler:::create_pos_ids, pd = pd),
                                             indents=pd$indent, stylerignore = pd$stylerignore[1])
    pd <- styler:::bind_rows(pd, semicolumn_pds) %>% styler:::arrange(pos_id)
  }
  pd
}
remove_all_line_breaks <- function(pd) {
  pd$lag_newlines <- rep(0L, nrow(pd))
  pd
}
oneline_style <- function() {
  styler::create_style_guide(
    initialize = styler::default_style_guide_attributes,
    line_break = lst(identity),
    space = lst(oneliner:::remove_all_spaces, add_semi_colon),
    token = lst(remove_comments,add_semi_colon,remove_all_line_breaks),
    indention = lst(identity),
    use_raw_indention = TRUE,
    reindention = tidyverse_reindention(),
    style_guide_name = "oneliner::one_line_style@https://github.com/lorenzwalthert",
    style_guide_version = version
  )
}


#' Formats the highlighted selection into one line
#'
#' @return
#' @export
style_selection_oneline <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (all(nchar(text) == 0)) {
    abort("No code selected")
  }
  out <- styler::style_text(text, style = oneline_style)
  rstudioapi::modifyRange(context$selection[[1]]$range,
                          paste0(c(
                            out,
                            if (context$selection[[1]]$range$end[2] == 1) ""
                          ), collapse = "\n"),
                          id = context$id
  )
}









modify_selected_package_function <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text

  parts <- str_split(text,":::|::") %>% simplify()
  if (length(parts)==2) {
    ns <- parts[1]
    fn_name <- parts[2]
  } else if (length(parts)==1) {
    fn_name <- parts[1]
    ns <- find(fn_name) %>% str_replace("package:","")
  } else {
    message("nothing to modify")
    return()
  }
  fn <- getFromNamespace(fn_name,ns)
  fn_code <- fn %>% deparse1(collapse="\n")

  new.fn <- str_glue("customized_{fn_name}")
  orig.fn <- str_glue("original_{fn_name}")
  modtext <- str_glue("

## customized function
{new.fn} <- {fn_code}

## assign above function to replace in package {ns}
if (!exists('{orig.fn}')) {orig.fn} <- {text}
environment({new.fn}) <- asNamespace('{ns}')
assignInNamespace('{fn_name}', {new.fn}, ns = '{ns}')

## to reset back to normal:
#assignInNamespace('{fn_name}', {orig.fn}, ns = '{ns}')

")
  message(str_glue("Setting up code for re-assigning function {fn_name} in package {ns}"))
  rstudioapi::documentNew(modtext)
}



