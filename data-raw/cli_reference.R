




library(tidyverse)
library(yingtools2)
library(cli)


{
  cli_text("variables: {.var variable}")
  cli_text("functions: {.fn mutate}")
  cli_text("code: {.code x <- 1+2+3}")
  cli_text("field: {.field field}")
  cli_text("value: {.val {12}} or {.val {'text'}}")
  cli_text("packages: {.pkg yingtools2}")
  cli_text("class: {.cls {class(1234)}}")
  cli_text("file: {.file readme.txt}")
  cli_text("filepath: {.path readme.txt}")
  cli_text("Keyboard: press {.kbd ctrl+spacebar}")
  cli_text("environment variable: {.envvar BLASTDB}")
  cli_text("italics/emphasis: {.emph italics here}")
  cli_text("Bold: {.strong Strong} importance")
  cli_text("Bold: {style_bold('bold text here')}")
  cli_text("Inverse: {style_inverse('inverese text here')}")
  cli_text("Color: {col_br_magenta('magenta text here')}")
  cli_text("Put something in quotes: He said, {.str where are you?}")
  cli_text("Help page of a topic (clickable): check out the {.topic mutate} command")
  cli_text("URLs: {.url http://www.google.com}")
  cli_text("Hyperlinks: {.href http://www.google.com}")
}


# object types and description
objlist <- list(
  1,
  c(1,2,3),
  1.1,
  TRUE,
  c(TRUE,FALSE,TRUE),
  "text",
  LETTERS[1:3],
  factor("text"),
  Sys.Date(),
  Sys.time(),
  list(1,2,3),
  as.difftime(1,units="secs"),
  mtcars,
  function(x) {x}
)
for (obj in objlist) {
  cli_text(c("This object is {.type {obj}}.",
             "Its class is {.cls {class(obj)}}.",
             "It has value {.val {obj}}"))
}



# error/warning messages
stop(format_error("Error with cli formatting: {1:3}"))
rlang::abort(format_error("Error with cli formatting: {1:3}"))
cli_abort("Error with cli formatting: {1:3}")

warning(format_warning("Warning with cli formatting: {1:3}"))
rlang::warn(format_warning("Warning with cli formatting: {1:3}"))
cli_warn("Warning with cli formatting: {1:3}")




# pluralization
cli_text("Counting to 1000: {1:1000}.")

x <- 0:6
for (i in x) {
  # {?x} adds x to everything but 1
  cli_text("Found {i} file{?s}.")
}
for (i in x) {
  # {no(i): if i is 0 then print 'no'}
  cli_text("Found {no(i)} file{?s}.")
}
for (i in x) {
  # {?x/y} adds x if 1, y otherwise
  cli_text("Found {i} director{?y/ies}.")
}
for (i in x) {
  # {qty(i)} to avoid confusion on what to pluralize
  ntotal <- 10
  cli_text("{i}/{ntotal} {qty(i)} file{?s} {?needs/need} updates")
}

for (i in x) {
  p <- str_c("pkg",LETTERS[0:i])
  # {?x/y/z} adds x if 0, y if 1, z otherwise
  cli_text("Will remove {?no/the/the} {.pkg {p}} package{?s}.")
}

for (x in 0:3) {
  for (y in 0:3) {
    cli_text("we have {x}/{10} {qty(x)} value{?s} of X, and {y}/{10} {qty(y)} value{?s} of Y")
  }
}





pkgs <- list(
  character(),
  c("pkg1"),
  c("pkg1","pkg2"),
  c("pkg1","pkg2","pkg3")
)
for (p in pkgs) {
  cli_text("Will remove {?no/the/the} {.pkg {p}} package{?s}.")
}


# custom pluralization
x <- cli_vec(names(mtcars), list("vec-trunc" = 3))
cli_text("Column names: {x}.")


cli_alert("Alert text")
cli_alert_danger("Danger text")
cli_alert_info("Info text")
cli_alert_success("Success text")
cli_alert_warning("Warning text")
cli_h1("Heading 1")
cli_h2("Heading 2")
cli_h3("Heading 3")

cli_text(c("Downloaded {prettyunits::pretty_bytes(123143123)} in ",
           "{prettyunits::pretty_sec(1.3454)}"))

# definition list
cli_dl(c(foo = "one", bar = "two", baz = "three"))
# numbered list
cli_ol(c("one", "two", "three"))
# bulleted list
cli_ul(c("one", "two", "three"))
# varying bullets list
cli_bullets(c(
  "noindent",
  " " = "indent",
  "*" = "bullet",
  ">" = "arrow",
  "v" = "success",
  "x" = "danger",
  "!" = "warning",
  "i" = "info"
))
# ruler line
cli_rule("Summary of results")


# progress bar
cli_progress_bar("Cleaning data", total = 100)
for (i in 1:100) {
  Sys.sleep(5/100)
  cli_progress_update()
}
# blockquote
cli_blockquote("Revenge is a dish best served cold.", citation = "Pierre Choderlos de Laclos")


# divs
fun <- function() {
  cli_div(theme = list(ol = list("margin-left" = 2)))
  cli_ul()
  cli_li("one")
  cli_ol(c("foo", "bar", "foobar"))
  cli_li("two")
  cli_end()
  cli_end()
}
fun()

fun <- function() {
  cli_ol()
  cli_li("Item 1")
  ulid <- cli_ul()
  cli_li("Subitem 1")
  cli_li("Subitem 2")
  cli_end(ulid)
  cli_li("Item 2")
  cli_end()
}
fun()

# paragraph
clifun <- function() {
  cli_par()
  cli_text(cli:::lorem_ipsum())
}
clifun()



