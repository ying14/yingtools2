
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
#' @import ggplot2
#' @import forcats
#' @import stringr
#' @importFrom lubridate is.Date is.POSIXct is.POSIXlt is.POSIXt
#' @importFrom reshape2 dcast melt
#' @importFrom readxl read_excel
#' @importFrom scales percent
#' @importFrom plyr adply
NULL



#' Pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL


#check:

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
# git reset --hard origin/master
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

