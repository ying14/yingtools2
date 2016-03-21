## git remote add origin git@github.com:ying14/yingtools2.git
## git push -u origin master
##' ...Title...
##'
##' ...Description...
##'
##' @usage ...usage.code...
##'
##' ...details...
##'
##' @param .param1. ...param1.description...
##' @param .param2. ...param2.description...
##' @return ...description.of.data.returned...
##' @examples
##' ...examples.here....
##' @keywords keyword1 keyword2 ...
##' @seealso \code{\link{cdiff.method}}
##' @author Ying Taur
##' @export


#' @export
"%!in%" = function(x,y) {
  !(x %in% y)
}



#' Pretty Numeric Format (Non-scientific)
#'
#' Use to format axes in ggplot with non-scientific notation. Good for abundances!
#' @param l number vector to be formatted.
#' @return Expression for l, in non-scientific notation.
#' @examples
#' pretty_number(c(111e-12,230000022.11111,0.1234567))
#' ggplot(mtcars,aes(mpg*1e-6)) + geom_bar() + scale_x_continuous(label=pretty_number)
#' @export
pretty_number <- function(l,digits=2) {
  sapply(l,function(x) format(x,scientific=FALSE,trim=TRUE,big.mark=",",digits=digits))
}


#' Fancy Scientific Notation
#'
#' Use to format axes in ggplot with scientific notation.
#' @param l number vector to be formatted.
#' @return Expression for l, in scientific notation.
#' @examples
#' pretty_scientific(c(111e-12,230000022.11111,0.1234567))
#' ggplot(mtcars,aes(mpg*1e-6)) + geom_bar() + scale_x_continuous(label=pretty_scientific)
#' @export
pretty_scientific <- function(l,parse=TRUE) {
  l <- format(l,scientific=TRUE)
  l <- gsub("^(.*)e","'\\1'e",l)
  l <- gsub("e","%*%10^",l)
  if (parse) {
    return(parse(text=l))
  } else {
    return(l)
  }
}


#' Log Epsilon Tranformation
#'
#' Use this transformation for plotting log data including 0. You can't use regular log transformation because it can't take zero.
#'
#' The transformation used is y=log(x+epsilon/8)-log(epsilon/8), where epsilon is the parameter controlling the scale. The 1/8 portion is to make distances between ticks equal, so it's visually pleasing.
#' @param epsilon This parameter controls scaling. Think of this as the value of the first axis tick after zero. Default is 0.001.
#' @return Tranformation function to be plugged into ggplot.
#' @examples
#' values <- c(0,10^(-10:0))
#' d <- data.frame(x=1:length(values),y=values)
#' g <- ggplot(d,aes(x=x,y=y,label=y)) + geom_point() + geom_line() + geom_text()
#' g1 <- g + scale_y_continuous(breaks=values) + ggtitle("untransformed")
#' g2 <- g + scale_y_continuous(trans=log1_trans(0.0001)) + ggtitle("scale_trans, epsilon=0.0001")
#' g3 <- g + scale_y_continuous(trans=log1_trans(10^-6.)) + ggtitle("scale_trans, epsilon=0.0000001")
#' g4 <- g + scale_y_continuous(trans=log1_trans(10^-10)) + ggtitle("scale_trans, epsilon=0.0000000001")
#' print(arrangeGrob(g1,g2,g3,g4,nrow=2))
#' @author Ying Taur
#' @export
log_epsilon_trans <- function(epsilon=0.001) {
  ep8 <- epsilon/8
  trans <- function(x) sign(x)*(log(abs(x)+epsilon/8)-log(epsilon/8))
  inv <- function(y) sign(y)*epsilon/8*(exp(abs(y))-1)
  trans_new(paste0("log_epsilon-",format(epsilon)),trans,inv,
            breaks=log_epsilon_trans_breaks(epsilon),
            format=pretty_number,
            domain=c(-Inf,Inf))
}


#' Breaks for Log Epsilon Tranformation
#'
#' This is used by scant_trans as default method for breaks. Will fill in logs of 10.
#' @param epsilon
#' @return break function returning break values.
#' @export
log_epsilon_trans_breaks <- function(epsilon) {
  function(x) {
    firsttick <- round(log(epsilon,10))
    lasttick <- floor(log(x[2],10))
    c(0,10^(firsttick:lasttick))
  }
}



