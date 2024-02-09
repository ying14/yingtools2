
#' Useful R Functions by Ying
#'
#' Ying's assorted collection of R functions. Primarily useful for manipulation, analysis, and plotting of data, particularly microbiome data.
#'
#' To install/update this package, run: \code{devtools::install_github("ying14/yingtools2")}
#'
#' @author
#' Ying Taur
#'
#' Maintainer: Ying Taur
#' @name yingtools2
#' @docType package
#' @rawNamespace import(dplyr, except=recode)
#' @import tidyr
#' @import tibble
#' @import ggplot2
#' @import forcats
#' @import stringr
#' @import phyloseq
#' @import cli
#' @rawNamespace import(purrr, except=c(invoke,splice,as_function,%@%))
#' @rawNamespace import(rlang, except=c(invoke,flatten,flatten_raw,flatten_dbl,flatten_chr,flatten_lgl,flatten_int,splice))
#' @import grid
#' @importFrom lubridate is.Date is.POSIXct is.POSIXlt is.POSIXt
#' @importFrom scales percent trans_new pvalue rescale
#' @importFrom reshape2 dcast melt
#' @importFrom plyr adply
#' @importFrom gridExtra arrangeGrob grid.arrange arrangeGrob marrangeGrob
NULL



#' Pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL






if (FALSE) {


  #' ASDF function 2
  #'
  #' @description
  #' Here is a description of the function.
  #'
  #' @details
  #' Here are some details.
  #'
  #' `code here` = \code{code here}
  #'
  #' _italics_ = \emph{italics}
  #'
  #' **bold** = \strong{bold}
  #'
  #' \pkg{package_name}
  #'
  #' Bullet list:
  #' * bullet 1
  #' * bullet 2
  #'
  #' Number list:
  #' 1. number uno.
  #' continue discussing uno
  #' 2. number dos
  #' continue discussing dos
  #'
  #' link to another function in this package or another: [age.years()] or [tidyselect::all_of()]
  #'
  #' same as above but with different text: [ageyears][age.years()] or [tidyselect_stuff][tidyselect::all_of()]
  #'
  #' package: [`ggplot2`]
  #'
  #' package topic: [`<tidy-select>`][`tidyr::tidyr_tidy_select`]
  #'
  #' hyperlink: [Commonmark web site](http://commonmark.org/help)
  #'
  #' ## Subsection within details
  #' ### Sub-subsection
  #' ... text ...
  #'
  #' The main R web site is at <https://r-project.org>.
  #'
  #' here is a calculated number: `r 2+2`
  #'
  #' ```{r}
  #' #| echo: false
  #' knitr::kable(mtcars[1:5,1:5])
  #' ```
  #'
  #'
  #'
  #' @param a the a variable
  #' @param b the b variable
  #' @param c the c variable
  #' @param d the d variable
  #' @param ... ellipsis
  #'
  #' @return a list of everything.
  #' @export
  #'
  #' @examples
  #' asdf("a", "b")
  asdf <- function(a, b, c=1, d=list("a","b"), ...) {
    # ctrl-shift-D
    # rstudioapi::previewRd("man/asdf.Rd")

    # rstudioapi::previewRd("man/test_if_nonvarying_by_group.Rd")

    list(a,b,c,d,...)
  }

}





if (FALSE) {
  # find functions used by the package
  scripts <- list.files(path=".",pattern="\\.R$",recursive=TRUE,ignore.case=TRUE)
  parsedata  <- lapply(scripts,function(x) {
    getParseData(parse(file=x)) %>% mutate(file=x)
  }) %>% bind_rows()

  fundata <- parsedata %>%
    filter(token=="SYMBOL_FUNCTION_CALL") %>%
    group_by(text) %>%
    mutate(pkg=paste(find(first(text)),collapse="|")) %>%
    ungroup() %>% mutate(pkg=gsub("package:","",pkg)) %>%
    select(pkg,text,everything())

  fundata %>% filter(pkg %!in% c("base","yingtools2")) %>% group_by(pkg) %>% dt()


  # missing
  fundata %>% filter(pkg=="") %>% dt()

  fundata %>% filter(text=="read_excel")



  fundata %>% filter(grepl("readxl",pkg)) %>% count(pkg,text) %>%
    pull(text) %>% paste(collapse=" ") %>% copy.to.clipboard()

}




### DESCRIPTION file:
## suggests:
## imports:
## depends: just like loads the package, similar to library(xxxx) [don't do this].

### __imports.R file:
## @import xxxx: import package xxxx
## @importFrom xxxx f1 f2: import specific functions f1 and f2 from xxxx
## @rawNamespace import(xxxx, except=yyyy): import all functions except recode
## (alternatively, just use the :: operator)



## to force a fresh pull from github:
# git fetch --all
# git reset --hard origin/master (or main)
# git pull origin master


## git remote add origin git@github.com:ying14/yingtools2.git
## git push -u origin master
#" ...Title...
#"
#" ...Description...
#"
#" @usage ...usage.code...
#"
#" ...details...
#"
#" @param .param1. ...param1.description...
#" @param .param2. ...param2.description...
#" @return ...description.of.data.returned...
#" @examples
#" ...examples.here....
#" @keywords keyword1 keyword2 ...
#" @seealso \code{\link{cdiff.method}}
#" @author Ying Taur
#" @export

