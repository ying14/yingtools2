
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
#' @importFrom reshape2 dcast melt
#' @importFrom readxl read_excel
#' @importFrom scales percent
NULL




#' Pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL



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

