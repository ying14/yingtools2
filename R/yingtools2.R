




# custom operators --------------------------------------------------------

#' Not In
#'
#' Convenience function. `a %!in% b` is equivalent to `!(a %in% b)`
#' @export
`%!in%` = function(x,y) {
  !(x %in% y)
}

#' All In
#'
#' Convenience function. `a %allin% b` is equivalent to `all(a %in% b, na.rm=FALSE)`
#' @export
`%allin%` = function(x,y) {
  all(x %in% y)
}

#' Subtract Dates
#'
#' Returns number of days.
#'
#' @param x vector to subtract from
#' @param y vector to subtract
#' @return vector representing `x - y`
#'
#'
#' @export
#' @examples
#' Sys.Date() %-% as.Date("1975-02-21")
`%-%` = function(x,y) {
  as.numeric(difftime(x,y,units="days"))
}

#' Regular Expression Operator
#'
#' Shorthand operator for regular expression.
#' @export
#' @examples
#' sentences %like% "fish"
`%like%` = function(x,y) {
  grepl(y,x,ignore.case=TRUE)
}

#' Find Regular Expression Operator
#'
#' Shorthand operator for finding a regex pattern.
#' @export
#' @examples
#' sentences %find% "fish"
`%find%` <- function(x,...) {
  UseMethod("%find%",x)
}



#' @export
`%find%.default` <- function(x,pattern,name=NULL,maxhits=10) {
  requireNamespace(c("cli","pillar"),quietly = TRUE)
  pattern <- regex(pattern,ignore_case = TRUE)
  if (is.null(name)) {
    name <- deparse1(substitute(x))
  }
  width <- cli::console_width()
  which.hits <- str_which(x,pattern=pattern)
  n.hits <- length(which.hits)
  hits <- x[which.hits]
  if (n.hits>0) {
    cli::cli_text("{name}: {n.hits} {ifelse(n.hits==1,'hit','hits')}")
    sub.hits <- hits %>% head(n=maxhits)
    sub.which.hits <- which.hits %>% head(n=maxhits)
    bullet <- str_glue("[{sub.which.hits}]") %>% pillar::align()
    loc <- str_locate_all(sub.hits,pattern)
    text <- stringi::stri_sub_all(sub.hits,loc)
    emphtext <- text %>% map(cli::col_red)
    newhits <- stringi::stri_sub_replace_all(sub.hits,loc,replacement=emphtext)
    first.hit.loc <- loc %>% map_int(~.x[1,1])
    add.middle.ellipsis <- function(string,loc.first,pre.ellipsis=5,post.ellipsis=5,padded.criteria=5,
                                    ellipsis=cli::symbol$ellipsis) {
      part1 <- cli::ansi_substr(newhits,1,pre.ellipsis)
      part2 <- cli::ansi_substr(newhits,loc.first-post.ellipsis,cli::ansi_nchar(newhits))
      length.cutoff <- nchar(ellipsis) + pre.ellipsis + post.ellipsis + padded.criteria
      if_else(loc.first>length.cutoff,str_c(part1,ellipsis,part2),newhits)
    }
    # ellipsis <- symbol$ellipsis
    ellipsis <- cli::symbol$ellipsis
    pre.ellipsis = floor(width / 6)
    post.ellipsis = floor(width / 6)
    padded.criteria = floor(width * 1/3)
    newhits2 <- add.middle.ellipsis(newhits,loc=first.hit.loc,
                                    pre.ellipsis = pre.ellipsis,
                                    post.ellipsis = post.ellipsis,
                                    padded.criteria = padded.criteria,
                                    ellipsis=ellipsis)

    report <- str_glue("{bullet}: {newhits2}") %>%
      cli::ansi_strtrim(width = width, ellipsis = ellipsis)
    cli::cat_line(report)
    if (n.hits>maxhits) {
      cli::cli_text("... [truncated]")
    }

  }
  invisible(hits)
}

#' @export
`%find%.data.frame` <- function(x,pattern,name=NULL,maxhits=5) {
  hitlist <- x %>% imap(~`%find%.default`(.x,pattern=pattern,maxhits=5,name=paste0("$ ",.y)))
  invisible(hitlist)
}



# simple vector operations ------------------------------------------------


#' Age in years
#'
#' Calculates REAL age in years.
#'
#' Some people calculate age by taking age in days and dividing by 365.25. That approach can be inaccurate
#' because of leap years (that's weak!).
#' @param bdate `Date`, vector of birthdays.
#' @param now, `Date`, vector representing the time by which to calculate age.
#' @return Returns the age in years.
#' @examples
#' age.years(as.Date("1975-02-21"),Sys.Date())
#' @author Ying Taur
#' @export
age.years <- function(bdate,now) {
  #given the two dates, calculate whole age
  this.years.bday.char <- ifelse(is.na(bdate)|is.na(now),NA,
                                 paste0(format(now,format="%Y"),"-",format(bdate,format="%m-%d")))
  #this.years.bday.char <- paste0(format(now,format="%Y"),"-",format(bdate,format="%m-%d"))
  leap.years <- is.na(as.Date(this.years.bday.char)) & grepl("-02-29$",this.years.bday.char)
  this.years.bday.char[leap.years] <- sub("-02-29","-03-01",this.years.bday.char[leap.years])
  this.years.bday <- as.Date(this.years.bday.char)
  age <- as.numeric(format(now,format="%Y")) - as.numeric(format(bdate,format="%Y"))
  age[which(this.years.bday>now)] <- age[which(this.years.bday>now)] - 1
  return(age)
}


#' Middle Pattern
#'
#' Creates a regular expression to extract text in between two patterns. This utilizes Perl-style lookahead and lookbehind assertions.
#'
#' @param start Expression prior to middle expression.
#' @param middle Middle expression to be found.
#' @param end Expression after middle expression.
#' @return A regular expression (Perl-style).
#' @examples
#' text <- c("start[target]end","start[target 2]end","start[target 3]")
#' stringr::str_extract(text,middle.pattern("start",".+","end"))
#' stringr::str_extract(text,middle.pattern("start",".+"))
#' @export
middle.pattern <- function(start="",middle=".+",end="") {
  if (start!="") {
    start <- paste0("(?<=",start,")")
  }
  if (end!="") {
    end <- paste0("(?=",end,")")
  }
  paste0(start,middle,end)
}



#' Paste 2
#'
#' Similar to `paste` command, except that `NA`s are not converted to text.
#' If all fields are `NA`, then return `NA` if collapse if specified.
#'
#' This is useful when dealing with `NA` values. `paste` produces character `"NA"` values,
#' of course I could just convert those to actual `NA` values afterwards. However, if I use `collapse` option,
#' I can frequently get a lot of character combinations with `NA` in it, where it can be hard to remove.
#' This function comes in handy when using the `collapse` option and I need to guarantee a single character value.
#' An example is the `values_fn` argument for [tidyr::pivot_wider()], which requires a function that returns a single value.
#'
#' @param ... one or more `R` objects, to be converted to character vectors (same as `paste`).
#' @param sep a character string to separate the terms (same as `paste`).
#' @param an optional character string to separate the results (same as `paste`).
#' @return A character vector of the concatenated values.
#' @examples
#' a <- c(1,2,3)
#' b <- c(4,5,NA)
#' c <- c(NA,NA,NA)
#'
#' # Produces same output
#' paste(a,collapse=",") #produces "1,2,3"
#' paste2(a,collapse=",") #produces "1,2,3"
#'
#' # Output is different for these
#' paste(b,collapse=",") #produces "4,5,NA"
#' paste2(b,collapse=",") #produces "4,5"
#'
#' paste(c,collapse=",") #produces "NA,NA,NA"
#' paste2(c,collapse=",") #produces NA
#'
#' paste(a,b,collapse=",") #produces "1 4,2 5,3 NA"
#' paste2(a,b,collapse=",") #produces "1 4,2 5,3"
#' @author Ying Taur
#' @export
paste2 <- function(...,sep=" ",collapse=NULL) {
  #similar to paste except that NAs are not converted to text.
  #if all fields are NA, then return NA if collapse is specified.
  #data=list(as.character(x$Value));sep=" ";collapse=";"
  # data <- lapply(varlist,as.character)
  data <- lapply(list(...),as.character)
  data <- do.call(cbind,data)
  p.text <- apply(data,1,function(x) {
    x <- x[!is.na(x)]
    paste(x,collapse=sep)
  })
  p.text[p.text==""] <- NA_character_
  if (is.null(collapse)) {
    #no collapse
    return(p.text)
  } else {
    p.text <- p.text[!is.na(p.text)]
    if (length(p.text)==0) {
      #collapse: all NA, so return NA.
      return(NA_character_)
    } else {
      return(paste(p.text,collapse=collapse))
    }
  }
}





#' Ying's Min/Max
#'
#' Similar to `min`/`max` command, except that if the data is empty, the function returns
#' `NA` instead of `Inf`.
#'
#' This is useful when using the function repetitively and it's possible that everything can be `NA`.
#' This might come in handy if running min/max functions across an `apply` or `ddply` command.
#'
#' @param ... numeric or character arguments.
#' @param na.rm a logical indicating whether missing values should be removed.
#' @return A length-one vector representing max or min.
#' @describeIn max2 `max2` maximum value.
#' @examples
#' a <- c(1,2,3)
#' b <- c(1,2,NA)
#' c <- c(NA,NA,NA)
#'
#' # produces same answer
#' max(a)
#' max2(a)

#' max(b)
#' max2(b)
#'
#' max(b,na.rm=TRUE)
#' max2(b,na.rm=TRUE)
#'
#' max(c)
#' max2(c)
#'
#' #Produces different answer
#' max(c,na.rm=TRUE)
#' max2(c,na.rm=TRUE)
#' @author Ying Taur
#' @export
max2 <- function(...,na.rm=FALSE) {
  suppressWarnings({
    val <-   max(...,na.rm=na.rm)
  })
  val[is.infinite(val)] <- NA
  return(val)
}



#' @describeIn max2 `max2` minimum value.
#' @export
min2 <- function(...,na.rm=FALSE) {
  suppressWarnings({
    val <-   min(...,na.rm=na.rm)
  })
  val[is.infinite(val)] <- NA
  return(val)

}


#' Ying's Cut 2
#'
#' Similar to cut, but with several options for grouping.
#' @param x a numeric vector (or Date) to be converted to factor by cutting.
#' @param lower a vector of lower bounds
#' @param upper a vector of upper bounds
#' @param percentiles a vector of percentile breakpoints
#' @param n.quantiles an integer specifying number of quantiles.
#' @param n.splits split range of x into equal sized parts.
#' @param lvls optional vector for renaming the levels
#' @return a factor derived from grouping of x
#' @export
cut2 <- function(x,lower,upper,n.quantiles,percentiles,n.splits,lvls,rename.lvls=TRUE) {
  requireNamespace("cli",quietly = TRUE)
  if (!missing(lower)) {
    breaks <- unique(c(-Inf,lower,Inf))
    right=FALSE
  } else if (!missing(upper)) {
    breaks <- unique(c(-Inf,upper,Inf))
    right=TRUE
  } else if (!missing(percentiles)) {
    probs <- percentiles %>% unique()
    breaks <- quantile(x,probs=probs,na.rm=TRUE) %>% unname() %>% c(-Inf,.,Inf) %>% unique()
    right=TRUE
  } else if (!missing(n.quantiles)) {
    if (n.quantiles<=1) {
      stop("YTError: n.quantiles needs two or more cutoffs")
    }
    probs <- seq(0,1,length.out=n.quantiles+1)
    probs <- probs[c(-1,-length(probs))]
    quantiles <- quantile(x,probs=probs,na.rm=TRUE)
    breaks <- unname(quantiles) %>% c(-Inf,.,Inf) %>% unique()
    right=TRUE
  } else if (!missing(n.splits)) {
    if (n.splits<=1) {
      stop("YTError: n.splits needs two or more cutoffs")
    }
    seq0 <- seq(from=min(x,na.rm=TRUE),to=max(x,na.rm=TRUE),length.out=n.splits+1)
    breaks <- c(-Inf,seq0[c(-1,-length(seq0))],Inf)
    right <- FALSE
  } else {
    print("Error, need parameters!")
    return(NULL)
  }
  new.x <- cut(x,breaks,right=right,include.lowest=TRUE)
  if (!rename.lvls) {
    return(new.x)
  }
  if (class(x)=="Date") {
    #don't change anything
  } else if (is.wholenumber(x)) {
    levels(new.x) <- sapply(levels(new.x),function(y) {
      num <- as.numeric(unlist(str_extract_all(y,"-?[0-9.Inf]+")))
      num[1] <- ifelse(grepl("\\[",y),num[1],num[1]+1)
      num[2] <- ifelse(grepl("\\]",y),num[2],num[2]-1)
      newlvl <- paste0(num[1],"-",num[2])
      newlvl <- gsub("^-Inf-","<=",newlvl)
      newlvl <- gsub("^-?[0-9.]+-Inf$",paste0(">=",num[1]),newlvl)
      newlvl
    })
  } else { #e.g. 20<=X<30
    parts <- levels(new.x) %>% str_match("^([\\[\\(])([^,]+),([^,]+)([\\]\\)])$")
    if (any(is.na(parts))) {
      stop("YTError: not sure how to read levels!")
    }
    colnames(parts) <- c("all","l.bracket","lower","upper","u.bracket")
    parts <- as_tibble(parts,.name_repair = "check_unique") %>%
      mutate(l.bound=if_else(l.bracket=="[",cli::symbol$leq,"<"),
                     u.bound=if_else(u.bracket=="]",cli::symbol$leq,"<"),
                     lower.text=na_if(str_trim(lower),"-Inf"),
                     upper.text=na_if(str_trim(upper),"Inf"),
                     lower.text=str_c(lower.text,l.bound),
                     upper.text=str_c(u.bound,upper.text),
                     text=paste2(lower.text,"X",upper.text,sep=""))
    levels(new.x) <- parts$text
    # levels(new.x) <- sapply(levels(new.x),function(y) {
    #   b1 <- ifelse(grepl("\\[",y),symbol$leq,"<")
    #   b2 <- ifelse(grepl("\\]",y),symbol$leq,"<")
    #   num <- unlist(str_extract_all(y,"-?(Inf|[0-9.]+)"))
    #   newlvl <- paste0(num[1],b1,"X",b2,num[2])
    #   newlvl
    # })
    # levels(new.x) <- gsub("^-Inf<=?|<=?Inf$","",levels(new.x))
  }
  if (!missing(lvls)) {
    levels(new.x) <- lvls
  }
  return(new.x)
}




#' Split String Into Approximately Equal Sized Parts
#'
#' Split a string vector into separate lines of approximately equal size.
#' Typical use for this is formatting labels for plotting.
#' @param char character vector to be split.
#' @param nparts number of parts to split into. Default is `2`.
#' @param sep separator to split by. Default is a space, `" "`
#' @param collapse character to collapse by, after splitting. Default is `"\n"`. Specify `NULL` to keep as a list.
#'
#' @return a character vector (or list) which has been split.
#' @export
#'
#' @examples
#' str_split_equal_parts(sentences,4)
str_split_equal_parts <- function(char,nparts=2,sep=" ",collapse="\n")  {
  # declare.args(char="Staphylococcus aureus",str_split_equal_parts)
  # declare.args(char="S taphylococcus aureus",str_split_equal_parts)

  npartitions <- nparts -1
  locs <- str_locate_all(char,sep)
  lens <- nchar(char)
  split_char  <- pmap(list(char,locs,lens),function(chr,loc,len) {
    if (is.na(chr)) {
      return(NULL)
    }
    pos <- loc[,"start"]
    npartitions <- min(npartitions,length(pos))

    if (length(pos)==1) {
      # in combn(x,m), if x is a single integer, it converts it to  seq_len(x).
      combn.positions <- list(pos)
    } else {
      combn.positions <- combn(pos,npartitions,simplify=FALSE)
    }

    lengths <- combn.positions %>% map(~{
      x <- c(.x,len)
      x-lag(x,default=0)
    })
    max.lengths <- map_dbl(lengths,max)
    max.winners <- which(max.lengths==min(max.lengths))
    min.winner <- lengths[max.winners] %>% map_dbl(min) %>% which.max()
    final.winner <- max.winners[min.winner]
    combn.positions.winner <- combn.positions[[final.winner]]
    end <- c(combn.positions.winner-1,len)

    start <- c(1,combn.positions.winner+1)
    str_sub(chr,start=start,end=end)
  })
  if (!is.null(collapse)) {
    split_char <- map_chr(split_char,~paste2(.x,collapse="\n"))
  }
  return(split_char)
}







#' Determines if a numeric is all whole numbers
#'
#' Uses machine precision to determine if a numeric vector is all whole numbers.
#'
#' @param x numeric vector to be analyzed.
#' @param tol tolerance for whole number calling. Usually no need to change this.
#' @return Returnes logical value of whether or not all values are whole numbers.
#' @examples
#' a <- c(1,2,3)
#' b <- c(1,2,3.00001)
#' c <- c(1,2,3.00000000001)
#' is.wholenumber(a)
#' is.wholenumber(b)
#' is.wholenumber(c)
#' @author Ying Taur
#' @export
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  all(abs(x-round(x))<tol | is.na(x))
}


#' Cumulative Max
#'
#' Adds date functionality to \code{base::cummax} function.
#' @param x vector, numeric or Date.
#' @return Cumulative max of the vector
#' @author Ying Taur
#' @export
cummax <- function(x,...) {
  UseMethod("cummax")
}

#' @rdname cummax
#' @export
cummax.default <- base::cummax

#' @rdname cummax
#' @export
cummax.Date <- function(x) {
  new.x <- as.numeric(x)
  cu <- base::cummax(new.x)
  as.Date(cu,origin="1970-01-01")
}

# data inspection tools --------------------------------------------------------


#' Compare objects
#'
#' Compare two objects (vector or data frame), and displays a summary of similarities and differences.
#'
#' @param x object (vector or data frame) to be compared
#' @param y object (vector or data frame) to be compared
#' @param by variable(s) to join by and compare (for data frames only)
#' @return displays a report of the comparison, and invisibly returns a table of comparison details.
#' @examples
#' library(tidyverse)
#' vec <- stringr::sentences[1:100]
#' compare(vec,vec)
#' compare(vec,rev(vec))
#' compare(vec,rep(vec,2))
#' compare(rep(vec,2),vec)
#' compare(vec,rep(vec[1:10],2))
#' compare(vec,sample(vec,size=200,replace=TRUE))
#' compare(vec[1:50],vec[51:100])
#'
#' m <- mtcars %>% rownames_to_column("car")
#' m2 <- bind_rows(m,m)
#' m3 <- bind_rows(m,m,m)
#' m.sort <- m %>% arrange(mpg)
#' m.single <- m; m.single[4,4] <- 200
#' m.col <- m; m.col$disp <- 101
#' m.top <- m %>% slice(1:25)
#' m.bottom <- m %>% slice(10:n())
#' m.extracol <- m %>% mutate(mpg2=mpg+1,hp2=hp*100)
#' compare(m,m)
#' compare(m,m,by="car")
#' compare(m,m2,by="car")
#' compare(m2,m3,by="car")
#' compare(m,m,by="cyl")
#' compare(m,m.sort,by="car")
#' compare(m,m.single,by="car")
#' compare(m,m.single)
#' compare(m,m.col,by="car")
#' compare(m,m.top,by="car")
#' compare(m.top,m.bottom,by="car")
#' @export
compare <- function(x,...) {
  UseMethod("compare",x)
}


#' @rdname compare
#' @export
compare.default <- function(x,y) {
  if (is.atomic(x) && is.atomic(y)) {
    compare.character(as.character(x),as.character(y))
  } else {
    stop("YTError: not sure how to compare these.")
  }
}

#' @rdname compare
#' @export
compare.character <- function(x,y) {
  #using deparse1(substitute) because as_label doesn't seem to work in UseMethod situations, for some reason.
  x.name <- deparse1(substitute(x))
  y.name <- deparse1(substitute(y))
  x.type <- format(pillar::new_pillar_type(x))
  y.type <- format(pillar::new_pillar_type(y))

  are.equal <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }
  x.length <- length(x)
  y.length <- length(y)
  x.ndistinct <- n_distinct(x)
  y.ndistinct <- n_distinct(y)
  x.is.distinct <- x.length==x.ndistinct
  y.is.distinct <- y.length==y.ndistinct
  xy.samelength <- x.length==y.length
  xy.identical <- xy.samelength && all(are.equal(x,y))
  xy.identical.difforder <- xy.samelength && all(are.equal(sort(x),sort(y)))
  x.not.y <- setdiff(x,y)
  y.not.x <- setdiff(y,x)
  x.and.y <- intersect(x,y)
  setequal.xy <- length(x.not.y)==0 && length(y.not.x)==0
  x.subsetof.y <-length(x.not.y)==0 && length(y.not.x)>0
  y.subsetof.x <-length(y.not.x)==0 && length(x.not.y)>0
  xy.nooverlap <- length(x.and.y)==0

  cli_text("X ",
           col_cyan("<{x.name}>"),
           x.type,
           " vs. ",
           "Y ",
           col_cyan("<{y.name}>"),
           y.type)

  if (x.is.distinct) {
    cli_text("X is distinct ",style_italic(col_grey("(N={x.length})")))
  } else {
    cli_text("X is non-distinct ",style_italic(col_grey("(N={x.length}, {x.ndistinct} distinct)")))
  }
  if (y.is.distinct) {
    cli_text("Y is distinct ",style_italic(col_grey("(N={y.length})")))
  } else {
    cli_text("Y is non-distinct ",style_italic(col_grey("(N={y.length}, {y.ndistinct} distinct)")))
  }

  report.sets <- FALSE

  if (xy.identical) {
    cli_text("X and Y are identical")
  } else if (xy.identical.difforder) {
    cli_text("X and Y are identical, but in different order")
  } else if (setequal.xy) {
    if (!x.is.distinct && y.is.distinct) {
      cli_text("X and Y are equal sets, where X has duplicates")
    } else if (x.is.distinct && !y.is.distinct) {
      cli_text("X and Y are equal sets, where Y has duplicates")
    } else if (!x.is.distinct && !y.is.distinct) {
      cli_text("X and Y are equal sets, but with different freqs")
    } else {
      cli_text("?????")
    }
  } else if (x.subsetof.y) {
    # cli_text("X is a subset of Y ",style_italic(col_grey("({x.ndistinct} out of {y.ndistinct})")))
    cli_text("X is a subset of Y")
    report.sets <- TRUE
  } else if (y.subsetof.x) {
    # cli_text("Y is a subset of X ",style_italic(col_grey("({y.ndistinct} out of {x.ndistinct})")))
    cli_text("Y is a subset of X")
    report.sets <- TRUE
  } else if (xy.nooverlap) {
    cli_text("X and Y do not overlap")
    report.sets <- TRUE
  } else {
    # cli_text(str_glue("X and Y partially overlap: {length(x.not.y)} values both X and Y, {length(y.not.x)} values in Y only, {length(x.and.y)} values in X only"))
    cli_text("X and Y partially overlap")
    report.sets <- TRUE
  }
  if (report.sets) {
    header <- c("X and Y",
                "X only",
                "Y only") %>% pillar::align()
    items <- list(x.and.y,x.not.y,y.not.x)
    show <- items %>% map_lgl(~length(.x)>0)
    counts <- items %>% map_int(length) %>% str_c("[",.,"]") %>%
      col_grey() %>% pillar::align()
    body <- items %>% map_chr(pillar:::format_glimpse_1)
    report <- str_c(header,counts,body,sep=" ") %>% ansi_strtrim()
    cat_line(report[show])
  }

  tbl <- list(x.and.y=x.and.y,
              x.only=x.not.y,
              y.only=y.not.x)

  invisible(tbl)
}






#' @rdname compare
#' @export
compare.data.frame <- function(x,y,by=NULL) {
  x.name <- deparse1(substitute(x))
  y.name <- deparse1(substitute(y))
  x.type <- format(pillar::new_pillar_type(x))
  y.type <- format(pillar::new_pillar_type(y))
  cli_text("X ",
           col_cyan("<{x.name}>"),
           x.type,
           " vs. ",
           "Y ",
           col_cyan("<{y.name}>"),
           y.type)
  # message(str_glue("X <{x.name}> vs. Y <{y.name}>"))

  xy.cols <- intersect(names(x),names(y))
  x.cols <- setdiff(names(x),names(y))
  y.cols <- setdiff(names(y),names(x))
  if (is.null(by)) {
    by <- xy.cols
  }
  by.x <- (names(by) %||% by) %>% if_else(.=="",by,.) %>% unname()  # similar to coalesce; if names(by) is NULL, then =by.
  by.y <- unname(by)
  # are by vars distinct?
  x.is.distinct <- x %>% is.distinct(!!!syms(by.x))
  y.is.distinct <- y %>% is.distinct(!!!syms(by.y))


  if (setequal(by.x,names(x))) {
    by.x.label <- str_glue("all {length(by.x)} vars")
  } else {
    by.x.label <- paste(by.x,collapse=",")
  }
  if (setequal(by.y,names(y))) {
    by.y.label <- str_glue("all {length(by.y)} vars")
  } else {
    by.y.label <- paste(by.y,collapse=",")
  }
  cli_text("-X: {pretty_number(nrow(x))} rows ({ifelse(x.is.distinct,'distinct','not distinct')} across {by.x.label})")
  cli_text("-Y: {pretty_number(nrow(y))} rows ({ifelse(y.is.distinct,'distinct','not distinct')} across {by.y.label})")
  # cols
  column.report <- "Columns: "
  if (length(x.cols)==0 && length(y.cols)==0) {
    column.report <- c(column.report, str_glue("-X and Y have same {length(xy.cols)} columns"))
  } else {
    if (length(x.cols)>0) {
      column.report <- c(column.report, str_glue("-X has {length(x.cols)} cols not in Y: {paste(x.cols,collapse=', ')}"))
    }
    if (length(y.cols)>0) {
      column.report <- c(column.report, str_glue("-Y has {length(y.cols)} cols not in X: {paste(y.cols,collapse=', ')}"))
    }
  }
  # message(paste(column.report,collapse="\n"))
  cat_line(column.report)


  if (length(by)==0) {
    cli_text("-No cols to compare.")
    return(invisible(NULL))
  }
  #compare x and y
  different.by <- by.x!=by.y
  if (any(different.by)) {
    #rename vars such that by.x==by.y
    old.x <- by.x[different.by]
    old.y <- by.y[different.by]
    new.xy <- paste(old.x,old.y,sep="___")
    x <- x %>% rename(!!!(set_names(syms(old.x),new.xy)))
    y <- y %>% rename(!!!(set_names(syms(old.y),new.xy)))
    by.x <- ifelse(different.by,new.xy,by.x)
    by.y <- ifelse(different.by,new.xy,by.y)
    if (!all(by.x==by.y)) {
      stop("YTError: by.x and by.y are different!")
    }
  }
  x.byvals <- x %>% transmute(x.vals=paste(!!!syms(by.x),sep="__")) %>% pull(x.vals)
  y.byvals <- y %>% transmute(y.vals=paste(!!!syms(by.y),sep="__")) %>% pull(y.vals)
  byvals.setequal <- setequal(x.byvals,y.byvals)
  byvals.xnoty <- setdiff(x.byvals,y.byvals)
  byvals.ynotx <- setdiff(y.byvals,x.byvals)
  byvals.y.subsetof.x <- length(byvals.xnoty)>0 && length(byvals.ynotx)==0
  byvals.x.subsetof.y <- length(byvals.ynotx)>0 && length(byvals.xnoty)==0
  byvals.samelength <- length(x.byvals)==length(y.byvals)
  byvals.identical.diff.order <- byvals.samelength && byvals.setequal && all(sort(x.byvals)==sort(y.byvals))
  byvals.identical <- byvals.samelength && byvals.setequal && all(x.byvals==y.byvals)
  byvals.relationship <- str_glue("{if_else(x.is.distinct,'1','many')}-to-{if_else(y.is.distinct,'1','many')}")

  cli_text("Joining:")
  if (byvals.setequal) {
    cli_text("-X and Y join completely, {byvals.relationship}")
  } else if (byvals.y.subsetof.x) {
    cli_text("-Y is a subset of X, {byvals.relationship}")
  } else if (byvals.x.subsetof.y) {
    cli_text("-X is a subset of Y, {byvals.relationship}")
  } else {
    cli_text("-X and Y partially overlap, {byvals.relationship}")
  }
  # recalculate in case of weirdness with col names
  compare.vars <- intersect(names(x),names(y))
  # add _x and _y to ends
  names(x) <- paste0(names(x),"_x")
  names(y) <- paste0(names(y),"_y")
  by.x <- paste0(by.x,"_x")
  by.y <- paste0(by.y,"_y")
  compare.x <- paste0(compare.vars,"_x")
  compare.y <- paste0(compare.vars,"_y")
  by <- set_names(by.y,by.x)
  if (length(intersect(names(x),names(y)))>0) {
    #should never happen
    stop("YTError: there's still name overlap after renaming!")
  }
  all <- full_join(x,y,by=by,keep=TRUE) %>%
    mutate(.status=case_when(
      !is.na(!!sym(by.x[1])) & !is.na(!!sym(by.y[1])) ~ "both X and Y rows",
      !is.na(!!sym(by.x[1])) & is.na(!!sym(by.y[1])) ~ "X only rows",
      is.na(!!sym(by.x[1])) & !is.na(!!sym(by.y[1])) ~ "Y only rows"
    ))
  diff <- map2(compare.x,compare.y,~{
    xx <- all[[.x]]
    yy <- all[[.y]]
    xy.diff <- xx != yy
    xy.both.na <- is.na(xx) & is.na(yy)
    xy.diff | !xy.both.na
  }) %>% setNames(compare.vars) %>% as_tibble()
  alldiff <- all %>% select(.status) %>% cbind(diff)
  alldiff.summary <- alldiff %>%
    group_by(.status) %>%
    summarize(n.rows=n(),
              across(all_of(compare.vars),.fns = ~{
                diffcount <- sum(.x,na.rm=TRUE)
                ifelse(diffcount>0,diffcount,NA_integer_)
                # pct <- diffcount/n.rows
                # ifelse(pct>0,pct,NA_integer_)
              }),.groups="drop") %>%
    mutate(n.diffs=coalesce_values(!!!syms(compare.vars),omit.na=TRUE))

  alldiffs <- alldiff.summary %>% filter(.status=="both X and Y rows") %>% pull(n.diffs)
  n.compared.rows <- alldiff.summary %>% filter(.status=="both X and Y rows") %>% pull(n.rows)
  if (is.na(alldiffs)) {
    cli_text("-no mismatches")
  } else {
    cli_text("-mismatched values in col(s): {alldiffs}")
  }
  invisible(all)
}


#' Tabulate
#'
#' Tabulates frequencies of vectors. By default, sorts by frequency.
#'
#' @param var the vector to be tabulated
#' @param sort whether to sort results. Default `TRUE`
#' @param pct whether to display percents. Default `TRUE`
#' @param as.char logical specifying whether to return tabulation as a single character. Useful for summarizing data within grouping commands such as `ddply` or `group_by`/`summarize`
#' @param collapse if `as.char=TRUE`, how to collapse. Default `"\n"`
#'
#' @return Returns a data frame with tabulations.
#' @examples
#' tab(cid.patients$sex)
#' @author Ying Taur
#' @export
tab <- function(var,sort=TRUE,pct=TRUE,as.char=FALSE,collapse="\n") {
  tbl <- data.frame(var=var) %>% count(var)
  if (pct) {
    tbl <- tbl %>% mutate(pct=percent(prop.table(n)))
  }
  if (sort) {
    tbl <- tbl %>% arrange(desc(n))
  }
  if (as.char) {

    if (pct) {
      char <- paste0(tbl$var," (n=",tbl$n,",",tbl$pct,")",collapse=collapse)
    } else {
      char <- paste0(tbl$var," (n=",tbl$n,")",collapse=collapse)
    }
    return(char)
  } else {
    return(tbl)
  }
}



#' Search data columns for a Regex pattern
#'
#' Use to search columns for a pattern.
#' @param data data to be searched
#' @param pattern regex pattern to be searched
#' @param ignore.case whether to ignore case, default is `TRUE`
#'
#' @return a summary table of column hits
#' @export
#'
#' @examples
search.data.columns <- function(data,pattern,ignore.case=TRUE) {
  # data=cell.infusion.rows;pattern="boost";ignore.case=T
  d <- data %>%
    mutate(across(.fns=as.character)) %>%
    mutate(row=row_number()) %>%
    pivot_longer(-row,names_to="column") %>%
    count(column,value) %>%
    mutate(hit=grepl(pattern,value,ignore.case=ignore.case))

  dd <- d %>%
    group_by(column) %>%
    summarize(n.hits=sum(n[hit]),
              pct=n.hits/sum(n),
              values=paste2(value[hit],collapse="\n"),
              .groups="drop") %>%
    arrange(desc(n.hits)) %>%
    filter(n.hits>0)
  dd
}

#' Ying's DT view
#'
#' Use to peruse a dataframe within RStudio. Utilizes `DT` package.
#'
#' If data frame is grouped (i.e. `group_by` in dplyr), the rows will be sorted and shaded by group.
#'
#' @param data dataframe to be viewed.
#' @param fontsize numeric controlling font size in the table, measured in px. Default is 11.
#' @param maxchars max number of characters before adding an ellipsis `...`. Default is 250.
#' @param whiteSpace CSS property sets how white space inside an element is handled. Default is "pre-wrap".
#' @param pageLength number of rows to display per page (Default `Inf`, show all rows)
#' @param maxrows numeric controlling max number of rows to display. The purpose is to prevent `DT` from handling excessively large data frames. Default is 1000.
#' @param rownames whether or not to show row names (passed directly to [DT::datatable()]).
#' @param class the CSS class(es) of the table (passed directly to [DT::datatable()]).
#' @param escape whether to escape HTML entities in the table (passed directly to [DT::datatable()]).
#'
#' @return A javascript-style datatable, which displays in the Rstudio viewer.
#' @examples
#' library(dplyr)
#' mtcars %>% dt()
#' mtcars %>% group_by(cyl) %>% dt()
#' @author Ying Taur
#' @export
dt <- function(data,fontsize=14,pageLength=Inf,maxchars=250,maxrows=500,rownames=FALSE,escape=FALSE,class="compact cell-border stripe",whiteSpace="pre-wrap") {
  requireNamespace(c("DT","forcats"),quietly=TRUE)
  fontsize <- paste0(fontsize,"px")
  n.cols <- ncol(data)
  index_col <- n.cols + rownames

  pal <- c("white","seashell","aliceblue")
  indices <- seq_along(pal)
  clrs.rgb <- paste0("rgb(",apply(col2rgb(pal),2,function(x) paste(x,collapse=",")),")")
  data$index_ <- data %>% group_indices() %>% factor() %>% forcats::fct_inorder() %>% as.numeric()
  data <- data %>% arrange(index_) %>%
    mutate(index_=((index_-1) %% length(pal)) + 1) %>%
    select(!!!groups(.),-index_,everything()) %>% ungroup()
  add <- function(l,...) {
    if (is.list(l)) {
      c(l,list(...))
    } else {
      c(l,...)
    }
  }
  plugins <- c()
  options <- list()
  columnDefs <- list()
  ## ellipsis
  plugins <- add(plugins,"ellipsis")
  columnDefs <- add(columnDefs,list(
    targets = 1:n.cols,
    render = DT::JS("$.fn.dataTable.render.ellipsis( ",maxchars," ,true, true)")
  ))
  ## header font size
  options <- add(options,initComplete=DT::JS(paste0("function(settings, json) {$(this.api().table().header()).css({'font-size':'",fontsize,"'});}")))
  options <- add(options,searchHighlight=TRUE)
  options <- add(options,paging=!is.infinite(pageLength),
                 pageLength=pmin(pageLength,maxrows))
  ## make index invisible
  columnDefs <- add(columnDefs,list(
    targets = index_col,
    visible = FALSE
  ))
  options <- add(options,columnDefs=columnDefs)
  output <- data %>%
    filter(row_number()<=maxrows) %>%
    # mutate(across(where(is.character),~str_replace_all(.,c("<"="&lt",">"="&gt","&"="&amp","\""="&quot","'"="&#39")))) %>%
    DT::datatable(plugins=plugins,class=class,options=options,escape=escape,rownames=rownames) %>%
    DT::formatStyle(0:length(data),fontSize=fontsize,lineHeight="95%",whiteSpace=whiteSpace)
  if (nrow(data)>0) {
    output <- output %>%
      DT::formatStyle("index_",target="row",backgroundColor=DT::styleEqual(indices,clrs.rgb))
  }
  return(output)
}



#' Make Table
#'
#' Creates a summary table (data frame) variables from the data.
#'
#' This was written to create a "Table 1" of a manuscript.
#'
#' @param data Data frame containing data to be described.
#' @param ... column names (bare) within `data` to be summarized.
#' @param denom whether to show the denominator in the summary
#' @param maxgroups max number of groups before collapsing into an "Other" category.
#' @param by optional variable name (bare) by which to summarize the data. Each separate value will be a column of data in the table.
#' @param fisher whether or not to perform Fisher test. Performed if `by=...` is specified.
#'
#' @return Returns a data frame formatted to be summary table.
#' @examples
#' make_table(mtcars,cyl,gear)
#' @author Ying Taur
#' @export
make_table <- function(data,...,by=NULL,denom=FALSE,maxgroups=10,accuracy=0.1,fisher=TRUE) {
  requireNamespace(c("rlang","purrr"),quietly=TRUE)
  vars <- enquos(...)
  by <- enquo(by)
  totalvar <- quo(total_)
  allvars <- append(vars,totalvar)

  vars.by <- vars %>% append(by) %>% map(as_label)
  is.td <- vars.by %>% paste0("_day") %>% has_name(data,.)
  if (any(is.td)) {
    warning("YTWarning: possible time-dependent variables detected: ",paste(vars.by[is.td],collapse=","),". Consider carefully before incorporating these.")
  }
  d <- data %>% ungroup() %>%
    mutate(!!totalvar:="") %>%
    select(!!!allvars,!!by) %>%
    mutate(across(where(is.character),fct_infreq),
           across(where(~!is.character(.) & !is.factor(.)),as.character),
           across(where(~n_distinct(.)>maxgroups),~fct_lump_n(.,n=maxgroups,ties.method="first")))
  tbl <- lapply(allvars,function(var) {
    d %>% count(value=!!var) %>%
      complete(value,fill=list(n=0)) %>%
      mutate(sum=sum(n),
             pct=n/sum,
             percent=scales::label_percent(accuracy=accuracy)(pct),
             var=as_name(var),
             text=if (denom) str_glue("{n}/{sum} ({percent})") else str_glue("{n} ({percent})")) %>%
      select(var,value,all=text)
  }) %>% bind_rows()

  if (!quo_is_null(by)) {
    by.table <- lapply(allvars,function(var) {

      d %>% count(value=!!var,col=paste0(as_name(by),"=",!!by)) %>%
        complete(value,col,fill=list(n=0)) %>%
        group_by(col) %>%
        mutate(sum=sum(n),
               pct=n/sum,
               percent=scales::label_percent(accuracy=accuracy)(pct),
               var=as_name(var),
               text=if (denom) str_glue("{n}/{sum} ({percent})") else str_glue("{n} ({percent})")) %>%
        ungroup() %>%
        pivot_wider(id_cols=c(var,value),names_from=col,values_from=text)
    }) %>% bind_rows()
    tbl <- full_join(by.table,tbl,by=c("var","value"))
  }
  if (fisher & !quo_is_null(by)) {
    f.tbl <- lapply(vars,function(var) {
      x <- pull(d,!!var)
      y <- pull(d,!!by)
      pval <- tryCatch({
        f <- fisher.test(x,y)
        scales::pvalue(f$p.value)
      },error=function(e) {
        warning(e)
        return("error")
      })
      tibble(var=as_name(var),fisher.pvalue=pval)
    }) %>% bind_rows()
    tbl <- tbl %>% left_join(f.tbl,by="var") %>%
      mutate(fisher.pvalue=if_else(duplicated(var),NA_character_,fisher.pvalue))
  }
  return(tbl)
}


#' Find All Distinct Variables
#'
#' Find Distinct
#'
#' @param data data frame to be analyzed
#' @param ... grouping variables that define data units.
#' @return prints whether variables matching the groups or not.
#' @export
find.all.distinct.vars <- function(data, ...) {
  id.vars <- ensyms(...)
  id.varnames <- id.vars %>% map_chr(as_name)
  other.varnames <- setdiff(names(data),id.varnames)
  other.vars <- syms(other.varnames)

  data2 <- data %>% group_by(...) %>% summarize_all(function(x) length(unique(x))) %>% ungroup()
  data3 <- data2 %>% select(!!!other.vars) %>% summarize_all(function(x) all(x==1))

  distinct.vars <- names(data3)[t(data3)]
  non.distinct.vars <- names(data3)[!t(data3)]
  distinct.vars.text <- paste0("[",paste(id.varnames,collapse=","),"],",paste(distinct.vars,collapse=","))
  non.distinct.vars.text <- paste0(non.distinct.vars,collapse=",")
  message("distinct: ",distinct.vars.text,"\n\nnot distinct: ",non.distinct.vars.text,"\n")

}


#' Is Distinct
#'
#' Determine if specified columns within data are distinct for individual rows.
#' @param data data frame to be analyzed
#' @param ... grouping variables that define data units.
#' @param add.group.vars if `TRUE`, add any grouping variables.
#' @return Logical indicating whether or not columns are distinct.
#' @export
#' @examples
#' cid.patients %>% is.distinct(Patient_ID)
#' cid.cdiff %>% is.distinct(Patient_ID)
is.distinct <- function(data, ..., add.group.vars=TRUE) {
  gvars <- data %>%
    group_by(..., .add=add.group.vars) %>%
    groups()
  anydup <- data %>% select(!!!gvars) %>% anyDuplicated()
  return(anydup==0)
}







#' Test the relationship between X and Y
#'
#' @param x first vector
#' @param y second vector
#'
#' @return
#' @export
#'
#' @examples
compare_relationship <- function(x,y) {
  t <- tibble(x=x,y=y)
  tx <- t %>% group_by(x) %>%
    summarize(n.y=n_distinct(y),.groups="drop") %>%
    summarize(max.n.y=max(n.y),.groups="drop") %>% pull(max.n.y)
  ty <- t %>% group_by(y) %>%
    summarize(n.x=n_distinct(x),.groups="drop") %>%
    summarize(max.n.x=max(n.x),.groups="drop") %>% pull(max.n.x)

  if (tx==1 && ty==1) {
    relationship <- "one-to-one"
  } else if (tx==1 && ty>1) {
    relationship <- "many-to-one"
  } else if (tx>1 && ty==1) {
    relationship <- "one-to-many"
  } else {
    relationship <- "many-to-many"
  }
  message(relationship)
  invisible(relationship)
}


#' Test if X and Y are one-to-one
#'
#' @param x first vector
#' @param y second vector
#'
#' @return logical value as to whether or not the vectors are one-to-one.
#' @export
#'
#' @examples
is.one.to.one <- function(x,y) {
  rel <- compare_relationship(x,y)
  return(rel=="one-to-one")
}


#' Regular Expression Widget
#'
#' A widget for exploring regular expressions on a vector
#' @param vec a character vector where regex searches will be applied.
#' @return
#' @export
#' @examples
regex.widget <- function(vec,port=4567) {
  library(shiny);library(stringi)
  if (!is.atomic(vec)) {
    stop("YTError: vec is not an atomic vector!")
  }
  app <- shinyApp(ui=fluidPage(
    tagList(
      div(textInput("textinput","Reg Ex #1",value="",width="100%"),
          textInput("textinput2","Reg Ex #2",value="",width="100%"),style="height: 17 px;"),
      fluidRow(column(3,actionButton("go","Run")),
               column(3,checkboxInput("grouphits","Group by Hit",TRUE))),
      div(DT::dataTableOutput("datatable",width="100%",height="400px"),style="font-size:80%"),
      div(DT::dataTableOutput("datatable2",width="100%",height="400px"),style="font-size:80%")
    )
  ),server=function(input, output) {
    tbl.all <- reactive({
      input$go
      tbl <- isolate({
        re1 <- regex(input$textinput,ignore_case=TRUE)
        re2 <- regex(input$textinput2,ignore_case=TRUE)
        tibble(vec=tolower(vec)) %>%
          mutate(match.re1=str_detect(vec,re1),
                 match.re2=!match.re1 & str_detect(vec,re2),
                 match.re1=coalesce(match.re1,FALSE),
                 match.re2=coalesce(match.re2,FALSE)) %>%
          filter(match.re1|match.re2) %>%
          mutate(loc1=str_locate_all(vec,re1),
                 loc2=str_locate_all(vec,re2))
      })
      tbl
    })
    output$datatable <- DT::renderDataTable({
      tbl1 <- tbl.all() %>% filter(match.re1) %>%
        unnest(loc1) %>%
        mutate(hits1=stringi::stri_sub(vec,loc1),
               hilight1=paste0("<b><font color=\"red\">",hits1,"</font></b>"),
               repl1=stringi::stri_sub_replace(vec,loc1,replacement=hilight1)) %>%
        select(hits1,repl1)

      if (input$grouphits) {
        tbl1 <- tbl1 %>%
          group_by(hits1) %>%
          summarize(n=n(),repl1=repl1[1]) %>%
          ungroup() %>% arrange(desc(n))
      } else {
        tbl1 <- tbl1 %>%
          group_by(repl1,hits1) %>%
          summarize(n=n()) %>%
          ungroup() %>% arrange(desc(n))
      }
      tbl1 <- tbl1 %>% filter(row_number()<=500)
      tbl1 %>% dt()
    })
    output$datatable2 <- DT::renderDataTable({
      tbl2 <- tbl.all() %>% filter(match.re2) %>%
        unnest(loc2) %>%
        mutate(hits2=stringi::stri_sub(vec,loc2),
               hilight2=paste0("<b><font color=\"red\">",hits2,"</font></b>"),
               repl2=stringi::stri_sub_replace(vec,loc2,replacement=hilight2)) %>%
        select(hits2,repl2)

      if (input$grouphits) {
        tbl2 <- tbl2 %>%
          group_by(hits2) %>%
          summarize(n=n(),repl2=repl2[1]) %>%
          ungroup() %>% arrange(desc(n))
      } else {
        tbl2 <- tbl2 %>%
          group_by(repl2,hits2) %>%
          summarize(n=n()) %>%
          ungroup() %>% arrange(desc(n))
      }
      tbl2 <- tbl2 %>% filter(row_number()<=500)
      tbl2 %>% dt()
    })
  })
  runGadget(app)
}




#' Coalesce indicator variables into one summary variable.
#'
#' After providing multiple indicator variables, summarize them by creating a character vector.
#' @param ... indicator variables to coalesce together. Should be all logical.
#' @param else.value The character value if there are no hits. Default is `NA`
#' @param first.hit.only If `TRUE`, will only show first hit (which is a true coalesce). Default is `FALSE`, which concatenates all hits.
#' @return A vector of same length as the indicators, displaying variable names that were `TRUE`
#' @examples
#' cid.patients %>%
#'   mutate(antibiotics=coalesce_indicators(vanco_iv,betalactam,fluoroquinolone,metronidazole)) %>%
#'   count(antibiotics,sort=TRUE)
#' @author Ying Taur
#' @export
coalesce_indicators <- function(...,else.value=NA_character_,first.hit.only=FALSE) {
  vars <- enquos(..., .named=TRUE)

  if (packageVersion("purrr") <"1.0.0") {
    do.simplify.list <- purrr::simplify_all
  } else {
    do.simplify.list <- function(x) {
      map(x, purrr::list_simplify, strict=FALSE)
    }
  }
  text <- vars %>% map(eval_tidy) %>%
    imap(function(value,lbl) {
      keep <- !is.na(value) & value
      if_else(keep,lbl,NA_character_)
    }) %>% transpose() %>%
    do.simplify.list() %>%
    # simplify_all() %>%
    # map(list_simplify, strict = FALSE) %>%
    map_chr(~{
      .x <- .x[!is.na(.x)]
      if (first.hit.only) {
        .x <- .x[1]
      }
      paste2(.x,collapse="|")
    })
  text[is.na(text)] <- else.value
  return(text)
}



#' Coalesce values into one summary variable.
#'
#' Summarize the value of several variables in a single character vector by concatenating variable name and values.
#' @param ... variables to coalesce together.
#' @param sep character string separating variable name and value. Default is "="
#' @param collapse character string separating variable/value pairs. Default is "|"
#' @param omit.na whether or not to remove variable in the case of NA.
#' @return A character vector of same length as the variables, displaying variable names plus values.
#' @export
#' @examples
#' cid.patients %>%
#'   mutate(demographics=coalesce_values(agebmt>60,sex,race)) %>%
#'   count(demographics,sort=TRUE)
coalesce_values <- function(...,sep="=",collapse="|",omit.na=FALSE) {
  vars <- enquos(..., .named=TRUE)

  if (packageVersion("purrr") <"1.0.0") {
    do.simplify.list <- purrr::simplify_all
  } else {
    do.simplify.list <- function(x) {
      map(x, purrr::list_simplify, strict=FALSE)
    }
  }

  # values <- vars %>% map(eval_tidy)
  text <- vars %>% imap(function(quo,lbl) {
    value <- eval_tidy(quo) %>% as.character()
    if (omit.na) {
      ifelse(!is.na(value),paste0(lbl,sep,value),NA_character_)
    } else {
      paste0(lbl,sep,value)
    }
  }) %>% transpose() %>%
    do.simplify.list() %>%
    # map(list_simplify, strict = FALSE) %>%
    map_chr(~paste2(.x,collapse=collapse))
  return(text)
}



# data recoding ----------------------------------------------------------------


#' Ying's Recode
#'
#' Recode a variable
#'
#' @param var the vector to be recoded.
#' @param recodes typically, a named vector specifying recodes. Note that order matters; first matching recode will apply
#' (assuming `multi.hits=FALSE`). As an additional option, a named list of vectors can also be used, where each vector of
#' values will be recoded to its corresponding name.
#' @param else.value the value to be used if a value is not recoded. Default is the old value.
#' @param as.factor whether or not to code as a factor. The levels will ordered based on `recodes`. Default is to base it on whether original vector is a factor.
#' @param regexp if `TRUE`, use regular expressions. Default is `FALSE`, which performs exact matching.
#' @param replace if `TRUE`, replace the hit (using `gsub`)instead of replacing the entire field. Note that `regexp` and `multi.hits` should be `TRUE`, if not it will be changed. Default is `FALSE`.
#' @param multi.hits if `TRUE`, will evaluate every value for every recode. So values can be recoded more than one time.
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is `TRUE`
#' @param perl whether to use perl-style regular expressions. Default is `FALSE`
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for `grep`)
#' @return A vector consisting of the recoded values of `var`
#' @examples
#' # Recode if field matches exactly.
#' recodes1 <- c("Pseudomonas aeruginosa"="P. aeruginosa","Staphylococcus aureus oxacillin resistant"="MRSA")
#' bsi$org.short.1 <- recode2(cid.bsi$org.short,recodes1)
#' bsi$org.short.1
#'
#' # Recode if there is a regular expression pattern match.
#' recodes2 <- c("Pseud.+aerug"="P. aeruginosa","oxacill.+resist"="MRSA")
#' bsi$org.short.2 <- recode2(cid.$org.short,recodes2,regexp=TRUE)
#' bsi$org.short.2
#'
#' # Instead of recoding, find and replace text.
#' recodes3 <- c("Pseudomonas"="P.","Staphylococcus"="S.")
#' bsi$org.short.3 <- recode2(cid.$org.short,recodes3,replace=TRUE,multi.hits=TRUE)
#' bsi$org.short.3
#'
#' # Recode via regular expressions to merge groups.
#' recodes4 <- c("enterococcus|staph|streptococcus|cnst|vre"="gram positive",
#'               "klebs|coli|serrat|pseudo|steno|citro|acinet|enterobact"="gram negative")
#' bsi$org.short.4 <- recode2(cid.$org.short,recodes4,regexp=TRUE,else.value="Other Bacteria")
#' bsi$org.short.4
#'
#' # if recodes are in list object, vectors of possible hits can be listed. This will do the same thing as #4.
#' recodes5 <- list("gram positive"=c("entero","staph","strep","cnst","vre"),
#'                  "gram negative"=c("klebs","coli","serrat","pseudo","steno","citro","acinet","enterobact"))
#' bsi$org.short.5 <- recode2(cid.$org.short,recodes5,regexp=TRUE,else.value="Other Bacteria")
#' bsi$org.short.5
#' @author Ying Taur
#' @export
recode2 <- function(var,recodes,else.value=NULL,as.factor=NULL,regexp=FALSE,replace=FALSE,multi.hits=FALSE,ignore.case=TRUE,perl=FALSE,useBytes=TRUE) {

  if (is.null(names(recodes)) & length(recodes)>0) {
    stop("Variable recodes needs to be a named vector or list")
  }
  if (replace & (!multi.hits | !regexp)) {
    warning("You specified replace=TRUE, setting regexp and multi.hits to TRUE.")
    multi.hits <- TRUE
    regexp <- TRUE
  }
  if (is.list(recodes)) { #if recodes is list, convert it to vector
    recodes <- unlist(lapply(names(recodes),function(n) {
      re <- recodes[[n]]
      structure(rep(n,length(re)),names=re)
    }))
  }
  if (is.null(else.value)) {
    else.class <- mode(var)
  } else {
    else.class <- mode(else.value)
  }
  rclass <- c(sapply(recodes,mode),else.class) %>% unique()
  if (length(rclass)==1) {
    na_value <- as(NA,rclass)
  } else {
    warning("YTWarning: different classes were specified in recodes and else.value. Result will be character.")
    mode(recodes) <- "character"
    if (!is.null(else.value)) {
      mode(else.value) <- "character"
    }
    na_value <- NA_character_
  }
  var.recode <- rep(na_value,length(var)) #create NA vector

  for (i in seq_along(recodes)) {
    pattern <- names(recodes)[i]
    newname <- recodes[i]
    if (multi.hits) { #which indices to evaluate.
      evals <- 1:length(var)
    } else { #normally: eval NAs
      evals <- is.na(var.recode)
    }
    if (regexp) { #regexp matching
      hit <- grepl(pattern,var[evals],ignore.case=ignore.case,perl=perl,useBytes=useBytes)
    } else { #exact matching
      hit <- pattern==var[evals]
    }
    if (replace & regexp & multi.hits) { #replace, then substitute
      var.recode <- if_else(is.na(var.recode),as.character(var),var.recode) #should only run first time through
      var.recode[evals][hit] <- gsub(pattern,newname,var.recode[evals][hit],ignore.case=ignore.case,perl=perl,useBytes=useBytes)
    } else { #normal replacing
      var.recode[evals][hit] <- newname
    }
  }
  #NA values kept as NA
  var.recode[is.na(var)] <- na_value

  # names(var.recode) <- var #add names to result.
  #handling non-matches
  if (is.null(else.value)) {
    #default is to use old value
    var.recode <- if_else(is.na(var.recode),as.character(var),var.recode)
  } else {
    var.recode <- if_else(is.na(var.recode),else.value,var.recode)
  }
  if (is.null(as.factor)) {
    as.factor <- is.factor(var)
  }
  if (as.factor) {
    old.lvls <- var %>% as.factor() %>% levels
    kept.lvls <- intersect(old.lvls,var.recode)
    new.lvls <- unique(c(recodes,kept.lvls,else.value))
    var.recode <- factor(var.recode,levels=new.lvls)
  }
  return(var.recode)
}



#' @rdname recode2
#' @export
recode.grep <- function(...) {
  recode2(regexp=TRUE,...)
}


#' Ying's Replace Grep
#'
#' Perform multiple text replacements at once using regular expressions. Similar in form to [recode()] and [recode.grep()]
#'
#' @param var the character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, the names are the regular expression, and the value is the replacement text.
#' @param result.as.list if `TRUE`, returns a 2-vector list containing replaced text and text hits. Default is `FALSE`.
#' @param replace.text text to replace hits with. Default is `""`
#' @param collapse.hits the separator with which all hits are pasted together. If `NULL`, hits will remain as an uncollapsed list. Default is `"|"`. Note that this parameter is not relevant unless `result.as.list=TRUE`
#' @param recode.hits whether to recode the hits into the with the replacement. Default if `FALSE`. This is relevant if `result.as.list=TRUE`.
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is `TRUE`
#' @param perl whether to use perl-style regular expressions. Default is `TRUE`
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for `grep`)
#'
#' @return By default, returns `var`, but with all regular expression hits replaced. If `result.as.list=TRUE` is specified, the hits themselves are also returned, within a 2-vector list.
#' @export
replace.grep <- function(var,recodes,result.as.list=FALSE,replace.text="",collapse.hits="|",recode.hits=FALSE,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) {
  # declare.args(var=sentences, recodes=c("the"="[THE]","[.]$"="!!"), replace.grep)
  message("YTNote: replace.grep() and replace.grep.data() are deprecated. Try using replace_grep_data.")
  if (is.null(collapse.hits) & !result.as.list) {
    stop("YTWarning: hits.collapse=NULL (hits displayed as list), but report.as.list=FALSE.")
  }
  if (!is.null(names(recodes))) {
    patterns <- names(recodes)
    replacements <- unlist(recodes)
  } else {
    patterns <- recodes
    replacements <- rep(replace.text,length.out=length(recodes))
  }
  newvar <- var
  hits <- rep(list(character(0)),length.out=length(var))
  for (i in 1:length(recodes)) {
    pattern <- patterns[i]
    replacetext <- replacements[i]
    if (result.as.list) {
      newhits <- str_extract_all(newvar,regex(pattern,ignore_case=ignore.case))
      hits <- mapply(c,hits,newhits)
    }
    newvar <- gsub(pattern, replacetext, newvar, ignore.case = ignore.case, perl = perl, useBytes = useBytes)
  }
  if (recode.hits) {
    if (length(hits)>0) {
      hits <- lapply(hits,function(hit) {
        recode.grep(hit,recodes=setNames(replacements,patterns),else.value=NA,perl=perl,ignore.case=ignore.case)
      })
    }
  }
  if (!is.null(collapse.hits)) {
    hits <- sapply(hits,function(x) paste2(x,collapse=collapse.hits))
  }
  if (result.as.list) {
    list(replace.text=newvar,hits=hits)
  } else {
    return(newvar)
  }
}




#' Ying's Replace Grep for Data Frames
#'
#' Perform multiple text replacements for a character vector in a data frame.
#'
#' Uses `replace.grep`.
#'
#' @param data the data frame to be manipulated.
#' @param var the bare character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, the names are the regular expression, and the value is the replacement text.
#' @param newvar bare name of column to hold the replaced version of `var`. If `NULL` (default), `var` will be overwritten.
#' @param replace.text text to replace hits with. Default is `""`
#' @param hits.var bare name of column to hold the text hits. If `NULL` (default), hits are not stored.
#' @param collapse.hits the separator with which all hits are pasted together. If `NULL`, hits will remain as an uncollapsed list. Default is `"|"`. Note that this parameter is not relevant unless `hits.var` is specified.
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is `TRUE`
#' @param perl whether to use perl-style regular expressions. Default is `TRUE`
#' @param recode.hits whether to recode the hits into the with the replacement. Default if `FALSE`. This is relevant if `result.as.list=TRUE`.
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for `grep`)
#'
#' @return By default, returns `var`, but with all regular expression hits replaced. If `result.as.list=TRUE` is specified, the hits themselves are also returned, within a 2-vector list.
#' @export
replace.grep.data <- function(data,var,recodes,newvar=NULL,replace.text="",hits.var=NULL,collapse.hits="|",recode.hits=FALSE,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) {
  newvar <- as.character(substitute(newvar))
  hits.var <- as.character(substitute(hits.var))
  if (length(newvar)==0) {
    newvar <- as.character(substitute(var))
  }
  store.hits <- length(hits.var)>0
  oldvar <- lazyeval::lazy_eval(lazyeval::lazy(var),data)
  results <- replace.grep(var=oldvar,recodes=recodes,result.as.list=store.hits,
                          replace.text=replace.text,collapse.hits=collapse.hits,recode.hits=recode.hits,ignore.case=ignore.case,perl=perl,useBytes=useBytes)
  if (store.hits) {
    newdata <- data
    newdata[[newvar]] <- results$replace.text
    newdata[[hits.var]] <- results$hits
  } else {
    newdata <- data
    newdata[[newvar]] <- results
    # newdata[[hits.var]] <- results$hits
  }
  return(newdata)
}

#' Replace and Extract Regular Expression Patterns for Data Frames
#'
#' For a given column of text, search for list of Regex patterns. Perform replacements and save the hits in another column.
#' This is roughly equivalent to repeatedly running '`stringr::str_replace_all()` and/or `stringr::str_extract_all()` on the same
#' column of text.
#'
#' This function attempts to perform multiple text manipulations (replacements and/or extractions) in an easy and efficient way.
#' It can be faster than manually running '`stringr::str_replace_all()` and/or `stringr::str_extract_all()` for a few
#' reasons: (1) it performs one search for both replacement and extraction, (2) it performs an initial search and ignores any rows
#' that didn't match, which saves time especially if most rows are not hits.
#'
#' Note that if you specify `newvar=NULL`, the original column `var` is searched for all patterns.
#' However, if you specify a value for `newvar`, that new column will be used for searching (and replacing) for each pattern.
#' @param data the data frame to be manipulated.
#' @param var the bare character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, will replace as: \code{c("replacement1"="pattern1", "replacement2"="pattern2", ...)}.
#' If unnamed, will replace `c("pattern1","pattern2", ...)` with `""`.
#' @param newvar bare name of column to hold the replaced version of `var`. If `NULL` (default), no replacement is performed.
#' Note: whether you specify this leads to slightly different behavior.
#' @param hits bare name of column to hold the text hits. If `NULL` (default), hits are not stored. This will store a list of extracted text, similar to the output of `str_extract_all()`
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is `TRUE`
#' @param collapse.fn optional function to apply to each element of `hits`, to create an atomic vector Non-hits are ignored.
#' @return returns the data with the above replacement text and stored hits.
#' @examples
#' library(stringr)
#' data <- tibble(text=stringr::sentences)
#' recodes <- c("<s-word>"="\\bs[a-z]+","<r-word>"="\\br[a-z]+")
#' data %>%
#'   replace_grep_data(var = text,
#'                     recodes = recodes,
#'                     newvar = new.sentence,
#'                     hits = hits)
#' data %>%
#'   replace_grep_data(var = text,
#'                     recodes = recodes,
#'                     newvar = new.text,
#'                     hits = hits,
#'                     collapse.fn = ~ paste(names(.), "=", ., collapse = "; "))
#' @export
replace_grep_data <- function(data,var,recodes,newvar=NULL,hits=NULL,ignore.case=TRUE,collapse.fn=NULL) {
  requireNamespace(c("rlang","stringi","purrr"),quietly=TRUE)
  var <- enquo(var)
  newvar <- enquo(newvar)
  hits <- enquo(hits)
  get.hits <- !quo_is_null(hits)
  get.replace <- !quo_is_null(newvar)
  if (!get.hits & !get.replace) {stop("YTError: you should specify a variable name for newvar, hits, or both.")}
  if (!get.hits & !is.null(collapse.fn)) {warning("YTWarning: you specified collapse.fn, but hits are not being stored. The collapse.fn will not be used.")}

  if (is.null(names(recodes))) {
    replacements=rep("",length.out=length(recodes))
  } else {
    replacements <- purrr::imap(recodes,~rep(.y,length.out=length(.x))) %>% unlist() %>% unname()
  }
  patterns <- recodes %>% unlist() %>% unname()
  var <- data %>% pull(!!var)
  hitlist <- vector(mode="list",length=length(var))
  for (i in 1:length(recodes)) {
    pattern <- patterns[i]
    replacement <- replacements[i]
    re <- regex(pattern,ignore_case=ignore.case)
    has.backslash <- str_detect(replacement,"\\\\[0-9]+")
    detected <- str_detect(var,re) & !is.na(var)
    subvar <- var[detected]
    subloc <- stringr::str_locate_all(subvar,re)
    if (get.hits|has.backslash) {
      subnewhits <- stringi::stri_sub_all(subvar,subloc)
      if (has.backslash) {
        subnewhits <- subnewhits %>% map(~.[!is.na(.)]) %>% map(~set_names(.,str_replace(.,re,replacement)))
        replacement <- lapply(subnewhits,names)
      } else {
        subnewhits <- subnewhits %>% map(~.[!is.na(.)]) %>% map(~set_names(.,rep(replacement,length.out=length(.))))
      }
      hitlist[detected] <- purrr::map2(hitlist[detected],subnewhits,c)
    }
    if (get.replace) {
      var[detected] <- stringi::stri_sub_replace_all(subvar,subloc,replacement=replacement)
    }
  }
  if (get.hits & !is.null(collapse.fn)) {
    nohit <- sapply(hitlist,is.null)
    newhits <- rep(NA_character_,length.out=length(var))
    newhits[!nohit] <- hitlist[!nohit] %>% map_chr(collapse.fn)
    hitlist <- newhits
  }
  if (get.hits) {
    data[[as_name(hits)]] <- hitlist
  }
  if (get.replace) {
    data[[as_name(newvar)]] <- var
  }
  return(data)

}




# coding shortcut functions -----------------------------------------------------

#' Convert object to R-code.
#'
#' Produces R-code that would create the object inputted. I use this if I have some data object that I obtained
#' somehow but just want to declare it in the code.
#'
#' This is similar to `deparse()`, except output looks a little bit more normal, and you can specify `width=Inf`
#' @param x object to be converted to R-code. Can be vector or data frame.
#' @param width max character width of each line. Set to `Inf` to avoid text-wrapping.
#' @return Returns the R-code.
#' @examples
#' x <- c("a","b","c")
#' copy.as.Rcode(x) %>% cat()
#' x <- tibble("a"=1:4,"_b"=c(T,F,T,F),"c c"=Sys.Date()+4:1,"d"=factor(LETTERS[1:4]))
#' deparse2(x) %>% cat()
#' @author Ying Taur
#' @export
deparse2 <- function(x,width=Inf) {
  r.version <- strsplit(version[['version.string']], ' ')[[1]][3]
  r.too.low <- utils::compareVersion(r.version,"4.0")==-1
  if (r.too.low) {
    deparse1 <- function (expr, collapse = " ", width.cutoff = 500L, ...) {
      paste(deparse(expr, width.cutoff, ...), collapse = collapse)
    }
  }
  deparse0 <- function(x) {
    if (is.infinite(width)) {
      deparse1(x)
    } else {
      deparse(x,width.cutoff=width)
    }
  }
  #converts x to R-code.
  if (is.atomic(x)) {
    if (is.Date(x)) {
      x.char <- deparse2(as.character(x),width=width)
      rcode <- paste0("as.Date(",x.char,")")
    } else if (is.POSIXlt(x)) { #these need to come before list, since these are lists.
      x.char <- deparse2(as.character(x,usetz=TRUE),width=width)
      rcode <- paste0("as.POSIXlt(",x.char,")")
    } else if (is.POSIXct(x)) {
      x.char <- deparse2(as.character(x,usetz=TRUE),width=width)
      rcode <- paste0("as.POSIXct(",x.char,")")
    } else if (is.factor(x)) {
      x.char <- deparse2(as.character(x),width=width)
      x.lvls <- deparse2(as.character(levels(x)),width=width)
      rcode <- paste0("factor(",x.char,",levels=",x.lvls,")")
    } else if (is.logical(x) | is.numeric(x) | is.character(x)){
      rcode <- deparse0(x) %>% paste(collapse="\n")
    }
  } else { ##### not atomic
    if (is.data.frame(x)) {
      df_names <- names(x)
      df_names <- if_else(df_names==make.names(df_names),df_names,paste0("\"",df_names,"\""))
      x.cols <- sapply(x,deparse2,width=width)
      x.cols <- mapply(function(varname,var) paste0(varname,"=",var),df_names,x.cols)
      rcode <- paste(x.cols,collapse=",\n")
      rcode <- paste0("tibble(",rcode,")")
    } else if (is.list(x)) {
      x.cols <- sapply(x,deparse2,width=width)
      x.cols <- mapply(function(varname,var) paste0("\"",varname,"\"=",var),names(x),x.cols)
      rcode <- paste(x.cols,collapse=",\n")
      rcode <- paste0("list(",rcode,")")
    } else {
      rcode <- deparse0(x)
    }
  }
  return(rcode)
}

#' @rdname deparse2
#' @param copy.clipboard whether or not to copy to clipboard. Default is `TRUE`
#' @export
copy.as.Rcode <- function(x,width=getOption("width")-15,copy.clipboard=TRUE) {
  #converts x to R-code.
  rcode <- deparse2(x,width=width)
  if (copy.clipboard) {
    copy.to.clipboard(rcode)
  }
  return(rcode)
}



#' Convert vector to SQL code.
#'
#' Produces SQL code for a vector of values.
#'
#' @param x vector to be converted to SQL code.
#' @param copy.clipboard logical, if `TRUE`, will copy the SQL code to the Clipboard.
#' @return Returns the SQL code.
#' @examples
#' values <- c("35171234","35507574")
#' copy.as.sql(values) %>% cat()
#' @author Ying Taur
#' @export
copy.as.sql <- function(x,copy.clipboard=TRUE,fit=TRUE,width=getOption("width")-15) {
  if (is.atomic(x)) {
    if (is.numeric(x)) {
      sql <- as.character(x)
    } else if (is.character(x) | is.factor(x)) {
      sql <- str_glue("'{x}'")
    } else if (lubridate::is.Date(x)) {
      sql <- str_glue("date('{x}')")
    }
    if (length(x)>1) {
      sql <- str_glue("({paste(sql,collapse=',')})")
    }
  } else if (is.data.frame(x)) {
    tbl <- x %>%
      rowwise() %>%
      mutate(across(.fns=~copy.as.sql2(.x,copy.clipboard=FALSE))) %>%
      ungroup()
    for (var in names(tbl)) {
      tbl[[var]] <- str_glue("{tbl[[var]]} as {var}")
    }
    sql.rows <- apply(tbl,1,function(x) {
      paste(x,collapse=",")
    })
    sql.rows2 <- str_glue("(select {sql.rows} from idb.oms_ord_catalog fetch first 1 rows only)")
    sql <- paste(sql.rows2,collapse=" union all\n")
    sql <- str_glue("({sql})")
  }

  if (copy.clipboard) {
    copy.to.clipboard(sql)
  }
  return(sql)
}

#' Copy to clipboard as tribble
#'
#' @param tbl a data frame to be copied
#' @param spaces number of spaces between columns
#'
#' @return (invisibly) returns R code statement that creates the tribble data frame, and copies to clipboard.
#' @export
#'
#' @examples
#' copy.as.tribble(head(mtcars)) %>% cat()
copy.as.tribble <- function(tbl,spaces=1) {
  if (!is.data.frame(tbl)) {
    stop("YTError: tbl is not a data frame")
  }
  convert <- function(x,name) {
    header <- paste0("~",name)
    values <- map_chr(x,deparse2)
    col <- paste0(c(header,values),",")
    char.width <- max(nchar(col)) + spaces
    fmtcol <- str_pad(col,char.width,side="right")
    return(fmtcol)
  }
  tab <- "\t"
  fmttbl <- tbl %>% imap(convert)
  lastcol <- fmttbl[[length(fmttbl)]] %>% str_trim()
  lastcol[length(lastcol)] <- lastcol[length(lastcol)] %>% str_replace(",$","")
  fmttbl[[length(fmttbl)]] <- lastcol
  fmttbl.transpose <- fmttbl %>% transpose()

  fmttbl.lines <- fmttbl.transpose %>% map_chr(~paste(.x,collapse="")) %>% paste0(tab,.,collapse="\n")
  rcode <- paste("tribble(",fmttbl.lines,")",sep="\n")
  rcode %>% copy.to.clipboard()
  invisible(rcode)
}


#' Get Code Info
#'
#' Read code (as an expression, text, or function) and provide information on
#' functions, package dependencies
#' @param expr expression to be evaluated.
#' @param text character, code can alternatively input as a string.
#' @param fn function, use this to evaluate code within a function.
#' @param recursive whether to examine code within functions
#' @return list containing information such as packages, functions, parsedata.
#' @examples
#' get.code.info(overlaps(1,2,3,sqrt(44)))
#'
#' get.code.info(text="log_epsilon_trans(0.001)")
#' yingtools2::age.years()
#'
#' fun <- function() {
#'   age.years(as.Date("1975-02-21"),Sys.Date())
#' }
#' get.code.info(fn=fun)
#' @export
get.code.info <- function(expr,text=NULL,fn=NULL,envir=parent.frame()) {
  expr <- enquo(expr)
  if (is.null(text)) {
    text <- quo_text(expr)
  }
  if (!is.null(fn)) {
    text <- deparse(body(fn))
  }
  exists2 <- function(x,envir) {
    tryCatch(exists(x,envir=envir,inherits=FALSE),error=function(e) FALSE)
  }
  parsedata <- getParseData(parse(text=text,keep.source=TRUE)) %>%
    mutate(src=sapply(text,function(x) find(x)[1]),
           exists=sapply(text,exists2,envir=envir),
           is.function=token=="SYMBOL_FUNCTION_CALL",
           is.symbol=token=="SYMBOL",
           # is.local=src==".GlobalEnv",
           is.local=exists & (is.symbol|is.function),
           pkg=str_extract(src,"(?<=package:).*$"),
           explicit=pkg!="base" & !is.na(pkg) & (is.function|is.symbol),
           explicit.text=ifelse(explicit,paste0(pkg,"::",text),text))
  explicit.code <- parsedata %>% group_by(line1) %>%
    summarize(code=paste(explicit.text,collapse="")) %>%
    ungroup() %>% pull(code) %>% paste(collapse="\n")
  all.fns <- parsedata %>% filter(is.function) %>% pull(explicit.text)
  locals <- parsedata %>% filter(is.local) %>% pull(text) %>% unique()
  # local.fns <- parsedata %>% filter(is.local,is.function) %>% pull(text) %>% unique()
  library.pkgs <- parsedata %>% filter(!is.na(pkg),pkg!="base",!explicit) %>% pull(pkg) %>% unique()
  all.pkgs <- parsedata %>% filter(!is.na(pkg),pkg!="base") %>% pull(pkg) %>% unique()
  results <- list(code=text,
                  explicit.code=explicit.code,
                  parsedata=parsedata,
                  locals=locals,
                  all.fns=all.fns,
                  all.pkgs=all.pkgs,
                  library.pkgs=library.pkgs)
  results
}




#' Declare arguments in a function
#'
#' Convenience function used when creating or modifying function code.
#'
#' Using this is like debug trace, except more unofficial. If anything is handled as quosure, you'll probably
#' need to put `quo()` around it. Also does not handle ellipses.
#' @param ... either named arguments or a function by which to assign defaults.
#' @param envir_ Environment to declare the arguments. Default is calling environment.
#' @return (nothing)
#' @export
#'
#' @examples
#' declare.args(var=sentences, recodes=c("the"="[THE]","[.]$"="!!"), replace.grep)
declare.args <- function(..., envir_=parent.frame()) {
  elist <- enexprs(...)
  imap(elist,function(exp,varname) {
    message(paste(varname,"=",as_label(exp)))
    obj <- eval(exp,envir=envir_)
    if (varname!="") {
      assign(varname,obj,envir=envir_)
    }
    if (is_function(obj)) {
      fmls <- formals(obj)
      # all args with defaults
      defaults <- fmls[!map_lgl(fmls,is.name)]
      current.vars <- ls(envir=envir_)
      # defaults that are not declared previously.
      defaults.to.assign <- defaults[!(names(defaults) %in% current.vars)]
      # defaults <- fmls[!(names(fmls) %in% declared.vars)]
      n.assign <- length(defaults.to.assign)
      if (n.assign>0) {
        for (j in 1:n.assign) {
          dvar <- names(defaults.to.assign)[j]
          dobj <- defaults.to.assign[[j]] %>% eval_tidy()
          assign(dvar,dobj,envir = envir_)
        }
      }
    }
  })
  invisible(NULL)
}


# date/time/timeline-related functions ------------------------------------------------------


#' Determines if 2 sets of time intervals overlap.
#'
#' @param start1 start times for interval 1
#' @param stop1 stop times for interval 1
#' @param start2 start times for interval 2
#' @param stop2 stop times for interval 2
#' @param check whether to check if start comes before stop.
#' @param start_NA start value if NA. Default is to leave as NA.
#' @param stop_NA stop value if NA. Default is to leave as NA.
#'
#' @export
#' @examples
#' times <- tribble(
#'   ~subj, ~startA, ~stopA, ~startB, ~stopB,
#'   1,     1,       2,      3,       4,
#'   2,     1,       3,      2,       4,
#'   3,     1,       4,      2,       3,
#'   4,     2,       3,      1,       4,
#'   5,     2,       4,      1,       3,
#'   6,     3,       4,      1,       2
#' )
#' times <- times %>%
#'   mutate(overlaps=overlaps(startA,stopA,startB,stopB))
#' times
#' ggplot() +
#'   geom_segment(data=times,aes(x=startA,xend=stopA,y=subj,yend=subj,color="A"),size=1) +
#'   geom_segment(data=times,aes(x=startB,xend=stopB,y=subj+0.1,yend=subj+0.1,color="B"),size=1) +
#'   geom_text(data=times,aes(x=0.5,y=subj,label=overlaps))
overlaps <- function(start1,stop1,start2,stop2,check=TRUE,start_NA=NA,stop_NA=NA) {
  if (!is.na(start_NA)) {
    start1 <- coalesce(start1,start_NA)
    start2 <- coalesce(start2,start_NA)
  }
  if (!is.na(stop_NA)) {
    stop1 <- coalesce(stop1,stop_NA)
    stop2 <- coalesce(stop2,stop_NA)
  }
  if (check) {
    error1 <- any(start1>stop1,na.rm=TRUE)
    error2 <- any(start2>stop2,na.rm=TRUE)
    if (error1) {stop("YTError: start1 is greater than stop1")}
    if (error2) {stop("YTError: start2 is greater than stop2")}
  }
  stop1>=start2 & stop2>=start1
}



#' Any overlap
#'
#' For a given set of intervals, determine whether any interval is overlapping.
#'
#' @param start vector specifying the start of the intervals
#' @param stop vector specifying the end of the intervals
#' @param check whether to check if start is greater than stop (default is `TRUE`)
#' @param na.rm whether to remove NA values (default is TRUE)
#'
#' @return whether or not at least one interval is overlapping.
#' @export
#' @examples
#' times1 <- tribble(
#'   ~start, ~stop,
#'   1,      2,
#'   3,      4,
#'   4,      6
#' )
#' any.overlap(times1$start,times1$stop)
#'
#' times2 <- tribble(
#'   ~start, ~stop,
#'   1,      2,
#'   3,      5,
#'   4,      6
#' )
#' any.overlap(times2$start,times2$stop)
any.overlap <- function(start,stop,check=TRUE,na.rm=TRUE) {

  if (check && any(start>stop,na.rm=TRUE)) {
    stop("YTError: start is greater than stop!")
  }
  t <- tibble(start=start,stop=stop) %>%
    arrange(start) %>%
    mutate(diff = start - lag(stop))
  return(any(t$diff[-1]<0,na.rm=na.rm))
}


#' Determines if x is between start and stop/
#'
#' Similar to [dplyr::between()], except that the vectors are recycled, so x can be a fixed value.
#' @param x vector of values to be checked
#' @param start vector of start time(s)
#' @param stop vector of stop time(s)
#' @param check whether to check if start comes before stop.
#'
#' @export
is.between <- function(x,start,stop,check=TRUE) {
  if (check) {
    if (any(start>stop,na.rm=TRUE)) {stop("YTError: start is greater than stop")}
  }
  overlaps(x,x,start,stop,check=check)
}





#' Get Rows (optimized for timeline plots)
#'
#' Given timeline event data with event type labels and start/stop times, calculate rows.
#' If requested, this will attempt to save vertical plot space by placing two event types on the same row, where possible.
#'
#' Note that `get.row()` is used in [geom_timeline()].
#' @param start vector of event start times (numeric or Date).
#' @param stop vector of event stop times (numeric or Date).
#' @param row vector of event types. Can be a list of more than one vector.
#' @param by optional grouping variable (vector or list of vectors), where events of the same group will be kept to together. Default is `NULL`
#' @param row.overlap whether or not the same row value can overlap. `TRUE`: each row value is always one row and can overlap, FALSE: rows do not overlap and can occupy several rows if necessary
#' @param min.gap the minimum gap allowed before 2 different row values can be combined. `Inf`: different row values can never share the same row position. `0`: fit different rows as much as possible.
#' @return Returns a vector of row number assignments for each time event.
#' @examples
#' library(tidyverse)
#' library(gridExtra)
#'
#' plot.meds <- function(data,title) {
#'   ggplot(data) +
#'     geom_rect(aes(xmin=startday-0.45,xmax=endday+0.45,ymin=row-0.45,ymax=row+0.45,fill=med.class),alpha=0.7) +
#'     geom_text(aes(x=midpoint(startday,endday),y=row,label=med.clean)) +
#'     ggtitle(title)
#' }
#' pt.meds <- cid.meds %>% filter(Patient_ID=="157")
#'
#' # strictly one row per med (row.overlap=TRUE and min.gap=Inf), arranged by class
#' pt.meds %>%
#'   mutate(row=get.row(startday,endday,row=med.clean,by=med.class)) %>%
#'   plot.meds("strictly one row per med\n(row.overlap=TRUE and min.gap=Inf), arranged by class")
#' # same meds are in different rows, if they overlap\nthe same time (row.overlap=TRUE)
#' pt.meds %>%
#'   mutate(row=get.row(startday,endday,row=med.clean,by=med.class,row.overlap=TRUE)) %>%
#'   plot.meds("same meds are in different rows, if they overlap\nthe same time (row.overlap=TRUE)")
#' # To save space, different meds can be in the same row,\nas long as they are sufficiently separated (min.gap=1)
#' pt.meds %>%
#'   mutate(row=get.row(startday,endday,row=med.clean,min.gap=1,by=med.class)) %>%
#'   plot.meds("To save space, different meds can be in the same row,\nas long as they are sufficiently separated (min.gap=1)")
#' # Arrange everything in as few rows as possible\n(row=NULL, by=NULL, row.overlap=FALSE)
#' pt.meds %>%
#'   mutate(row=get.row(startday,endday,row.overlap=FALSE)) %>%
#'   plot.meds("Arrange everything in as few rows as possible\n(row=NULL, by=NULL, row.overlap=FALSE)")
#' @export
get.row <- function(start,stop,row=NULL,by=NULL,row.overlap=TRUE,min.gap=Inf) {
  requireNamespace("IRanges",quietly=TRUE)
  if (any(start>stop,na.rm=TRUE)) {stop("YTError: start is greater than stop")}
  if (length(start)==0) {return(integer())}
  get.distinct.row <- function(start,stop) {
    if (any(is.infinite(start))|any(is.infinite(stop))) {return(seq_along(start))}
    ir <- IRanges::IRanges(as.numeric(start),as.numeric(stop))
    return(IRanges::disjointBins(ir))
  }
  combine.one.factor <- function(var) {
    if (is.null(var)) {return(1)}
    if (!is.list(var)) {
      return(factor(var))
    } else {
      by.combined <- do.call(paste,c(var,list(sep="||")))
      by.order <- do.call(order,var)
      return(factor(by.combined,levels=unique(by.combined[by.order])))
    }
  }
  by <- combine.one.factor(by)
  row <- combine.one.factor(row)
  t <- data.frame(start,stop,row,by) %>% mutate(i=row_number())
  #arrange elements within each row (minimize number of rows)
  if (!row.overlap) {
    t2 <- t %>% group_by(row,by) %>%
      mutate(row1=get.distinct.row(start,stop)) %>%
      ungroup()
  } else {
    t2 <- t %>% mutate(row1=1)
  }
  #now arrange rows within each by-value
  pad <- min.gap / 2
  t.row <- t %>% group_by(row,by) %>%
    summarize(start=min(start),stop=max(stop),.groups="drop") %>%
    group_by(by) %>%
    mutate(row2=get.distinct.row(start-pad,stop+pad)) %>%
    ungroup() %>%
    select(-start,-stop)
  # join together and create final row
  t3 <- t2 %>% left_join(t.row,by=c("row","by")) %>%
    # arrange(by,row2,row1) %>%
    mutate(finalrow=paste(by,row2,row1,sep=";;"),
           finalrow=fct_reordern(finalrow,by,row2,row1),
           finalrow=as.numeric(finalrow)) %>%
    arrange(i)
  return(t3$finalrow)
}



#' Convert Date format to regular expression
#' @param format character designating date formatting, following [base::strptime()] convention.
#' @return regular expression corresponding to the format
#' @export
#' @examples
#' date.regex("%Y-%m-%d")
#' date.regex("%a %b %e %H:%M:%S %Y")
date.regex <- function(format) {
  date.recodes <- c("%m"="(0?[1-9]|1[0-2])", #month 01-12
                    "%d"="(0?[1-9]|[1-2][0-9]|3[0-1])", #day of month
                    "%a"="(Mon|Tues?|Wed|Thu(rs)?|Fri|Sat|Sun)",
                    "%A"="(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)",
                    "%b"="(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
                    "%B"="(January|February|March|April|May|June|July|August|September|October|November|December)",
                    "%y"="([0-9]{2})", #2-digit year
                    "%Y"="((18|19|20)[0-9]{2})", #4 digit year
                    "%H"="([0-1][0-9]|2[0-3])", #hours as decimal 00-23
                    "%I"="(0?[1-9]|1[0-2])", #hours as decimal 01-12
                    "%M"="([0-5][0-9])", #minute 00-59
                    "%S"="(0?[0-9]|[1-5][0-9])(\\.[0-9]+)?", #second 00-59 (can have up to 6 decimals)
                    "%p"="(AM|PM)",
                    "\\."="\\.")
  regex <- recode2(format,date.recodes,regexp=TRUE,replace=TRUE,multi.hits=TRUE,ignore.case=FALSE)
  return(paste0("^",regex,"$"))
}


#' Convert to Date (Ying's version)
#'
#' Examines strings and convert to Date format if necessary.
#'
#' @param vec The vector to be converted.
#' @return Returns vector converted to Date. If date+time, convert to POSIXct.
#' @examples
#' #Access dates read by RODBC:
#' access.dates <- c("2009-012-03 00:00:00","2010-08-21 00:00:00","2013-07-01 00:00:00","2014-10-31 00:00:00")
#' as.Date2(access.dates)
#' @export
as.Date2 <- function(vec) {
  if (is.POSIXt(vec)) {
    if (all(format(vec,"%H:%M:%S")=="00:00:00" | is.na(vec))) {
      return(as.Date(vec))
    } else {
      return(vec)
    }
  } else if (!is.character(vec) & !is.factor(vec)) {
    return(vec)
  } else if (all(is.na(vec) | vec=="")) {
    return(vec)
  }
  date.formats <- c("%m-%d-%Y", #"12-23-2013","2-4-2014"
                    "%m/%d/%Y", #"12/23/2013","2/4/2014"
                    "%Y-%m-%d", #"2014-01-21","2014-02-04"
                    "%Y/%m/%d", #"2014/01/21","2014/02/04"
                    "%d-%b-%y", #"21-Jan-14","04-Feb-14"
                    "%d-%b-%Y", #"21-Jan-2014","04-Feb-2014"
                    "%m-%d-%y", #"01-14-14","01-21-14"
                    "%m/%d/%y", #"01/14/14","01/21/14"
                    "%Y-%m-%d 00:00:00", #"2004-02-21 00:00:00","1999-07-20 00:00:00" Access dates, read in by RODBC
                    "%Y-%m-%d 00:00:00.0", #"2004-02-21 00:00:00.0","1999-07-20 00:00:00.0" Access dates, read in by RODBC
                    "%Y-%m-%d 00:00:00.000000") #"2015-08-26 00:00:00.000000","2016-02-04 00:00:00.000000" SQL dates.
  datetime.formats <- c("%Y-%m-%d %H:%M:%S", #"1999-07-20 14:25:29","1999-07-20 14:25:29"
                        #"%Y-%m-%d %H:%M:%S", #"1999-07-20 14:25:29.3","1999-07-20 14:25:29"
                        "%Y-%m-%d %H:%M:%S", #"1999-07-20 14:25:29","1999-07-20 14:25:29"
                        "%Y-%m-%d-%H.%M.%S") #"1999-07-27-10.55.27","1999-07-27-10.55.27"

  vec2 <- vec[!is.na(vec) & vec!=""]
  for (df in date.formats) {
    #use of useBytes is to avoid warnings about locale
    if (all.grepl(date.regex(df),vec2,useBytes=TRUE)) {
      return(as.Date(vec,format=df))
    }
  }
  for (dtf in datetime.formats) {
    #use of useBytes is to avoid warnings about locale
    if (all.grepl(date.regex(dtf),vec2,useBytes=TRUE)) {
      return(as.POSIXct(vec,format=dtf))
    }
  }
  return(vec)
}


#' Create Date-Time (POSIXct object)
#'
#' Note: The particular use is in [ytdata::visits()] function.
#' @param date Date object
#' @param time character with time in it
#'
#' @return Returns POSIXct object with date and time combined.
#' @export
make.datetime <- function(date,time) {
  if (all(is.na(date))) {
    return(as.POSIXct(date))
  }
  if (!is.Date(date)) {
    stop("YTError: date is not a Date object!")
  }
  if (!is.character(time)) {
    stop("YTError: time should be character!")
  }
  dt <- as.POSIXct(rep(NA,length(date)))
  convert <- !is.na(date) & !is.na(time)
  dt[convert] <- as.POSIXct(paste(date[convert],time[convert]))
  dt <- lubridate::with_tz(dt,"UTC")
  return(dt)
}






#' Convert POSIXct to a fractional date
#'
#' This creates a fractional date. If you do `as.Date(datetime)`, it removes the time part.
#' @param datetime a POSIXct vector to be converted
#' @return a Date column that contains fractional values.
#' @export
as_date_fractional <- function(datetime) {
  date <- as.Date(datetime)
  time <- difftime(datetime,date,units="days")
  date+as.numeric(time)
}



#' Get Midpoint
#'
#' For 2 vectors, get the center. Works with numeric, Date, POSIXct.
#'
#' This is useful when dealing with Date and POSIXct, since you can't just add them together and divide by 2.
#' @param tstart the start value
#' @param tstop the stop value
#' @return a vector of values represent the midpoint between `tstart` and `tstop`.
#' @export
midpoint <- function(tstart,tstop) {
  if (lubridate::is.Date(tstart) | lubridate::is.POSIXct(tstart)) {
    tstart+as.numeric(tstop-tstart)/2
  } else if (is.numeric(tstart)) {
    (tstart+tstop)/2
  } else {
    stop("YTError: invalid class!")
  }
}





#' Extract Time
#'
#' Get time from a date-time POSIXct object.
#' @param datetime POSIXct object
#' @param format character parameter for formatting of time. Default format "10:30AM"
#' @return character with time component
#' @export
get.time <- function(datetime,format="%I:%m%p") {
  if (!is.POSIXt(datetime)) {
    stop("YTError: datetime is not a POSIX date-time!")
  }
  format(datetime,format)
}








# dplyr group functions ------------------------------------------------------




#' Group By All Distinct Variables
#'
#' Can be used similar to `group_by`, but will try to add additional variables to the group list, such that the grouping remains the same.
#' In other words, `group_by_all_distinct(data,a,b,c)` will group by a,b,c,x,y,z, where x,y,z do not alter the groups.
#' This is useful for keeping extra variables that go with the grouping, if you perform `summarize` afterwards.
#'
#' This is a convenience function that I made because of sheer laziness....
#' probably better to avoid using this for really rigorous data operations.
#' @param data data frame
#' @param ... variables to group by
#' @return Returns `data`, but grouped by `...` plus other variables that can be grouped along with it.
#' @author Ying Taur
#' @export
group_by_all_distinct <- function(data, ...) {
  id.vars <- enquos(...)
  id.varnames <- sapply(id.vars,as_name)
  data2 <- data %>% group_by(...) %>% summarize_all(function(x) length(unique(x))) %>% ungroup() %>%
    select_if(function(x) all(x==1))
  all.dist.vars <- unique(c(id.varnames,names(data2)))
  not.grouped <- setdiff(names(data),all.dist.vars)
  message("Grouping by [",length(all.dist.vars),"]: ",paste(all.dist.vars,collapse=", "))
  message("[Not grouped by [",length(not.grouped),"]: ",paste(not.grouped,collapse=", "),"]")
  data %>% group_by(!!!syms(all.dist.vars))
}




#' Test data for additional identifiers across groups.
#'
#' @description
#' In a data frame that can be grouped based on one or more column identifier(s), use these functions to test if
#' other columns do not vary within each group. In other words, it tests if additional columns can be added
#' to the grouping definition and would not alter the grouping structure.
#' This can be useful as a way to determine additional identifiers to include,
#' when performing reshaping operations such as `group_by`/`summarize` or `pivot_wider`.
#'
#' * `test_if_nonvarying_by_group()` returns testing results in the form of a named logical vector.
#'
#' * `group_suggest_additional_vars()` prints out the testing results and copies nonvarying vars to clipboard.
#' Use primarily as an aide during coding.
#'
#' * `assert_grouping_vars` performs testing and passes the original data frame if all tested
#' columns pass (are nonvarying across groups). If a column fails, a warning or error will be issued.
#' Use this for error checking within pipelines.
#'
#' @param data data to be tested (data.frame or data.table).
#' @param id_vars [`tidy-select`][`tidyr::tidyr_tidy_select`] ID vars that define the nonvarying
#' groups. Default is to use the grouping variables (from [dplyr::group_by()])
#' @param test_vars [`tidy-select`][`tidyr::tidyr_tidy_select`] variables to be tested. Default is all columns not specified in `id_vars`.
#' @param verbose whether or not to display messages about the testing results
#' @export
#' @rdname test_if_nonvarying_by_group
#' @examples
#' otu <- get.otu.melt(cid.phy)
#'
#' # Returns output of testing results
#' otu %>% test_if_nonvarying_by_group(id_vars=sample,
#'                                     test_vars=c(Sample_ID,Patient_ID,Family,Genus))
#'
#' # Copies variables that passed to clipboard
#' otu %>% group_by(otu) %>% group_suggest_additional_vars()
#'
#' # Issues warning that `test_vars` varies across `sample`
#' otu %>%
#' assert_grouping_vars(id_vars=sample,test_vars=c(Sample_ID,Consistency,Family,Phylum)) %>%
#'   group_by(sample,Sample_ID,Consistency,Family,Phylum) %>%
#'   summarize(totalseqs=sum(numseqs))
test_if_nonvarying_by_group <- function(data,
                                        id_vars = all_of(group_vars(data)),
                                        test_vars = everything(),
                                        verbose = FALSE) {
  # data=get.otu.melt(cid.phy);id_vars=quo(sample);test_vars=quo(everything())
  requireNamespace("data.table",quietly=TRUE)

  id_vars <- enquo(id_vars)
  test_vars <- enquo(test_vars)
  id_vars_ts <- tidyselect::eval_select(id_vars, data=data)
  test_vars_ts <- tidyselect::eval_select(test_vars, data=data)
  test_vars_ts <- test_vars_ts[!(test_vars_ts %in% id_vars_ts)]
  id_var_names <- names(id_vars_ts)
  test_var_names <- names(test_vars_ts)
  if (length(id_vars)==0) {
    warning("YTWarning: no groups detected")
  }

  dt.test <- data %>% data.table::as.data.table(key=id_var_names) %>%
    .[ , .group:=.GRP,by=id_var_names]
  total.groups <- dt.test$.group[nrow(dt.test)]
  total.testcols <- length(test_var_names)
  # whether or not to analyze first 10 groups, which can
  # speed up the calc by removing easy varying columns first.
  # generally worth doing if there are more cols and groups
  if (total.groups>500 && total.testcols>=10) {
    dt.ngroup.sizes <- c(10,Inf)
  } else {
    dt.ngroup.sizes <- Inf
  }
  var.to.test <- test_var_names
  varying <- c()
  for (ngroups in dt.ngroup.sizes) {
    # ngroups=10
    # message(length(var.to.test))
    sub <- dt.test[.group<ngroups ,]
    for (test_var in var.to.test) {
      # test_var=test_var_names[1]
      xx <- sub %>%
        .[ , c(test_var,id_var_names), with=FALSE] %>%
        unique() %>%
        .[ , id_var_names, with=FALSE] %>%
        anyDuplicated()
      if (xx!=0) {
        varying <- c(varying,test_var)
      }
    }
    var.to.test <- setdiff(var.to.test,varying)
  }

  data.testing <- setNames(!(test_var_names %in% varying),test_var_names)

  #whatever is left is non-varying.
  # data.testing <- setNames(test_var_names %in% to.test,test_var_names)
  data.rootgroup <- setNames(rep_along(id_var_names,TRUE),id_var_names)
  # data.testing <- data %>% ungroup() %>% as.data.table(key=id_var_names) %>%
  #   .[, lapply(.SD, uniqueN), .SDcols=test_var_names, by=id_var_names] %>%
  #   .[, lapply(.SD, function(x) all(x==1)), .SDcols=test_var_names] %>%
  #   as_tibble() %>% unlist()

  # data.testing0 <- data %>% ungroup() %>% group_by(!!!syms(id_var_names)) %>%
  #   summarize(across(.cols=all_of(test_var_names), .fns=n_distinct), .groups="drop") %>%
  #   summarize(across(.cols=all_of(test_var_names), .fns=~all(.x==1))) %>% unlist()

  if (verbose) {
    test_var_names_cangroup <- names(data.testing)[data.testing]
    test_var_names_cannotgroup <- names(data.testing)[!data.testing]
    message(str_glue("* ID grouping var(s): {paste(id_var_names,collapse=',')}"))
    message(str_glue("* Additional nonvarying grouping vars: {paste(test_var_names_cangroup,collapse=',')}"))
    message(str_glue("* Varying non-grouping vars: {paste(test_var_names_cannotgroup,collapse=',')}"))
  }
  test <- c(data.rootgroup,data.testing)
  test
}



#' @rdname test_if_nonvarying_by_group
#' @export
group_suggest_additional_vars <- function(data,
                                          id_vars = all_of(group_vars(data)),
                                          test_vars = everything()) {
  id_vars <- enquo(id_vars)
  test_vars <- enquo(test_vars)
  test <- test_if_nonvarying_by_group(data, id_vars=!!id_vars, test_vars=!!test_vars, verbose=TRUE)
  cangroup.vars <- names(test)[test]
  if (length(cangroup.vars)>1) {
    id.text <- paste(cangroup.vars, collapse=",")
    message("\nCopying vars to clipboard (ID vars plus additional nonvarying vars)...")
    copy.to.clipboard(id.text)
  } else {
    message("No additional ID vars found.")
  }
}

#' @rdname test_if_nonvarying_by_group
#' @param stopIfTRUE Whether to raise error is test fails. Default is `FALSE`: issue warning only.
#' @export
assert_grouping_vars <- function(data,
                                 id_vars = all_of(group_vars(data)),
                                 test_vars = everything(),
                                 stopIfTRUE = FALSE) {
  id_vars <- enquo(id_vars)
  test_vars <- enquo(test_vars)
  test <- test_if_nonvarying_by_group(data,id_vars=!!id_vars,test_vars=!!test_vars)
  if (any(!test)) {
    non.grouping.vars <- names(test)[!test]
    non.grouping.text <- paste(non.grouping.vars,collapse=",")
    id.group.vars <- tidyselect::eval_select(id_vars, data=data) %>% names()
    id.var.text <- id.group.vars %>% paste(collapse="+")
    ngroups.orig <- data %>% group_by(!!!syms(id.group.vars)) %>% n_groups()

    msg1 <- str_glue("Detected vars that vary across {id.var.text}: {non.grouping.text}")
    msg2 <- str_glue("-group = {id.var.text}: {ngroups.orig} groups (defined groups)")
    msg3 <- non.grouping.vars %>%
      map_chr(~{
        new.grouping <- c(id.group.vars,.x)
        newgroup.var.text <- new.grouping %>% paste(collapse="+")
        ngroups.new <- data %>% group_by(!!!syms(new.grouping)) %>% n_groups()
        str_glue("-group = {newgroup.var.text}: {ngroups.new} groups")
      }) %>% paste(collapse="\n")
    msg <- paste(msg1,msg2,msg3,sep="\n")

    if (stopIfTRUE) {
      stop(str_glue("YTError: {msg}"))
    } else {
      warning(str_glue("***YTWarning: {msg}"))
    }
  }
  return(data)
}



#' Group by Time
#'
#' Given data frame with start and stop times, group times by non-overlapping start and stop times.
#'
#' This is like running `group_by`, but creates a new grouping variable called `index_` that is created from times.
#' @param data data frame
#' @param start start times
#' @param stop stop times
#' @param ... other variables to group by. These will be applied prior to grouping by times.
#' @param gap time periods differing by this gap or less will be combined in the grouping variable. Default is 1.
#' @param add Same as the add option in `group_by`. When TRUE, will add to groups, rather than overriding them.
#' @return Returns `data`, but grouped by times and other variables.
#' @author Ying Taur
#' @export
#' @examples
#' library(tidyverse)
#' data <- tribble(
#'   ~subject, ~start, ~stop,
#'   "A",      1,      2,
#'   "A",      3,      4,
#'   "A",      5,      6,
#'   "A",      14,     15,
#'   "A",      16,     19,
#'   "A",      23,     30,
#'   "B",      3,      4,
#'   "B",      5,      6,
#'   "B",      7,      8,
#'   "B",      18,     19,
#'   "B",      21,     22,
#'   "B",      27,     29
#' )
#' grouped.data <- data %>%
#'   group_by_time(start,stop,subject,gap=2) %>%
#'   mutate(timegroup=factor(cur_group_id()))
#' # data is grouped based on start/stop times
#' grouped.data %>%  arrange(subject,start)
#' grouped.data %>%
#'   ggplot(aes(x=start-0.45,xend=stop+0.45,y=subject,yend=subject,color=timegroup))  +
#'   geom_segment(size=4) + xlab("time")
group_by_time <- function(data,start,stop, ... ,gap=1,add=FALSE) {
  group_vars <- enquos(...)
  start <- enquo(start)
  stop <- enquo(stop)
  data %>% group_by(!!!group_vars,.add=add) %>%
    arrange(!!start,!!stop) %>%
    mutate(index_=lag(cumsum(lead(!!start)-cummax(!!stop)>gap),default=0)) %>%
    group_by(index_,.add=TRUE)
}



#' Group by Time Streaks
#'
#' Group time data by consecutive streaks of a certain indicator variable.
#'
#' Similar to `group_by_time`, but for a different purpose. This function groups by consecutive values of the indicator variable.
#' This is to measure how long the indicator remains in the same state. One situation where I use this is calculating
#' when BMT engraftment has occurred. It is defined as the first day on which absolute neutrophil count is >500
#' for at least three consecutive measurements on at least three consecutive days.
#' @param data data frame
#' @param time time variable
#' @param indicator variable to group consecutive streaks
#' @param ... other variables to group by. These will be applied prior to grouping by time streaks.
#' @param gap time periods differing by this gap or less will be combined in the grouping variable. Default is `Inf`, i.e. no gap.
#' @param na.skip whether to ignore `NA` values in the indicator. Default is `FALSE`, which will just break streaks and provide a warning if they are encountered.
#' @param add Same as the add option in `group_by`. When TRUE, will add to groups, rather than overriding them.
#' @return Returns `data`, but grouped by time streaks
#' @author Ying Taur
#' @export
#' @examples
#' library(tidyverse)
#' data <- tribble(
#'   ~time, ~indicator,
#'   1,     TRUE,
#'   2,     FALSE,
#'   3,     TRUE,
#'   4,     FALSE,
#'   5,     TRUE,
#'   6,     TRUE,
#'   7,     TRUE,
#'   8,     FALSE,
#'   9,     FALSE,
#'   10,    FALSE
#' )
#' data %>% group_by_time_streaks(time,indicator)
group_by_time_streaks <- function(data,time,indicator, ... ,gap=Inf,na.skip=FALSE,add=FALSE) {
  time <- enquo(time)
  indicator <- enquo(indicator)
  group_vars <- enquos(...)

  ind <- pull(data,!!indicator)
  if (any(is.na(ind))) {
    warning("YTwarning: Found NA values in the indicator,", quo_name(indicator),". Streaks are broken whenever these are encountered.")
  }

  data %>% group_by_time(!!time,!!time,!!!group_vars,gap=gap,.add=add) %>%
    arrange(!!time) %>%
    mutate(index2_=(!!indicator)!=lag(!!indicator),
           index2_=is.na(index2_)|index2_,
           index2_=cumsum(index2_)) %>%
    group_by(!!indicator,index2_,.add=TRUE)
}







#' Sample n groups from a grouped table
#'
#' @param grouped_df a grouped data frame to be sampled
#' @param size number of groups to sample
#' @param weight sampling weights. Can be any expression that can be used in `summarize()` to calculate a valid weight.
#' @param replace sample with or without replacement? Default is `FALSE`
#' @return a subset of the original grouped data frame
#'
#' @examples
#' mtcars %>% group_by(gear) %>% sample_groups(2)
#' mtcars %>% group_by(gear) %>% sample_groups(5,weight=mean(mpg),replace=TRUE)
#' @export
sample_groups = function(grouped_df,size,weight=NULL,replace=FALSE) {
  weight <- enquo(weight)
  grp_var <- grouped_df %>% group_vars()
  if (length(grp_var)==0) {
    warning("YTWarning: no group detected.")
  }
  if (quo_is_null(weight)) {
    random_grp <- grouped_df %>%
      summarise(.groups="drop") %>%
      slice_sample(n=size, replace=replace)
  } else {
    random_grp <- grouped_df %>%
      summarise(weight_=!!weight,
                .groups="drop") %>%
      slice_sample(n=size, weight_by=weight_, replace=replace) %>% select(-weight_)
  }
  grouped_df %>%
    right_join(random_grp, by=grp_var) %>%
    group_by(!!!syms(grp_var))
}




#' Split data frame into named list
#'
#' Same as [dplyr::group_split()], except that the list of tables is named rather than unnamed.
#' Another minor difference is that it keeps previous grouping ([dplyr::group_split()]} wants all grouping vars to be stated at once).
#' @param .tbl A table
#' @param ... Grouping specification
#' @param .keep Should the grouping columns be kept
#' @param sep If multiple grouping vars are specified, separate the values with this character separator.
#'
#' @return A list of tibbles, broken up by grouping.
#' @export
#'
#' @examples
#' group_split_named(iris,Species)
group_split_named <- function(.tbl, ..., .keep = TRUE, sep = " / ") {
  grouped <- group_by(.tbl, ..., .add = TRUE)
  names <- rlang::inject(paste(!!!group_keys(grouped), sep=sep))
  grouped %>% group_split(.keep=.keep) %>% rlang::set_names(names)
}





# clipboard operations ----------------------------------------------------



#' Copy to Clipboard
#'
#' Copies object to the clipboard, which can be used to paste into other programs such as Word or Excel.
#'
#' This is now done using the [`clipr`] package. Previously I did this manually for each operating system.
#'
#' @param obj object to by copied. Can be data frame, matrix, table, vector.
#' @author Ying Taur
#' @export
copy.to.clipboard <- function(x) {
  if (is(x,"gg")) {
    copy.to.clipboard.gg(x)
  } else {
    requireNamespace("clipr",quietly=TRUE)
    clipr::write_clip(x)
    message("Copied to clipboard")
  }
}


#' @rdname copy.to.clipboard
#' @export
copy.to.clipboard.gg <- function(obj,width=10,height=7,dpi=150,pointsize=12,rescale="R") {
  if (Sys.info()['sysname']=="Windows") {
    windows(width=width,height=height,pointsize=pointsize,xpinch=dpi,ypinch=dpi,rescale=rescale)
    print(obj)
    savePlot("clipboard",type="wmf")
    dev.off()
    message("Copied image to clipboard (windows)")
  } else if (Sys.info()['sysname']=="Linux") {
    temp <- tempfile(fileext="png")
    png(temp,width=10,height=7,pointsize=12,units="in",res=150)
    print(obj)
    dev.off()
    system(str_glue("xclip -selection clipboard -t image/png -i {temp}"))
    if (file.remove(temp)) {
      message("Copied image to clipboard (linux-xclip)")
    }
  } else {
    error("YTError: don't yet know how to copy ggplot objects in this operating system.")
  }
}



#' Read Clipboard
#'
#' Read clipboard into vector or data frame.
#'
#' Attempts to determine if content is vector or data frame. If reading a data frame, it will assume first row
#' as header (specify `header=FALSE` if necessary). If first cell is blank, it will assume row and column names.
#' Note: This is now done using the `clipr` package. Previously I did this manually for each operating system.
#' @param ... Options to pass to [utils::read.table()] (e.g. `header`, `row.names`, `sep`, `as.is`)
#' @return Contents of clipboard
#' @author Ying Taur
#' @export
read.clipboard <- function(...) {
  requireNamespace("clipr",quietly=TRUE)
  obj <- clipr::read_clip()
  if (all(grepl("\t",obj))) {
    obj <- clipr::read_clip_tbl(x=obj, ...)
  }
  if (is.null(obj)) {
    message("Nothing found in clipboard")
  } else {
    message("Read ",class(obj)[1]," from clipboard")
  }
  return(obj)
}


# formatting functions --------------------------------------------------------------






#' Pretty Numeric Format (Non-scientific)
#'
#' Use to format axes in ggplot with non-scientific notation. Good for abundances!
#'
#' Note,
#' @param x number vector to be formatted.
#' @return Expression for x, in non-scientific notation.
#' @examples
#' x <- c(12,23.456789,1111e-7,230000022.11111,0.001234567)
#' pretty_number(x)
#'
#' dtime <- as.difftime(x,units="secs")
#' pretty_number(dtime)
#' @rdname pretty_number
#' @export
pretty_number <- function(x,...) UseMethod("pretty_number")
#' @rdname pretty_number
#' @export
pretty_number.default <- function(x,digits=2) {
  sapply(x,function(y) format(y,scientific=FALSE,trim=TRUE,big.mark=",",digits=digits))
}
#' @rdname pretty_number
#' @export
pretty_number.difftime <- function(x,...) {
  zero <- as.POSIXct(0,origin="1970-01-01")
  sapply(x,function(d) {
    diff <- d+zero-zero
    num <- diff %>% as.numeric() %>% pretty_number.default(...)
    units <- units(diff)
    paste(num,units)
  })
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


#' Short number formatting
#'
#' Use to abbreviate large numbers (e.g. 3450000 is '3.4M')
#'
#' @param x numeric vector to be formatted
#' @param abbrev named vector specifying the log base 10 cutoff values and their assigned label. Default is `c(K=3,M=6,B=9)`.
#' @param sig.digits number of signficant digits to use.
#'
#' @return character vector of formatted numbers
#' @examples
#' short_number(pi*10^(-1:10))
#' @export
short_number <- function(x,abbrev=c("K"=3,"M"=6,"B"=9),sig.digits=3) {
  abbrev <- c(abbrev,Inf) %>% sort()
  cuts <- cut(log10(x),breaks=abbrev,right=FALSE,labels=FALSE)
  map2_chr(x,cuts,~{
    if (!is.na(.y)) {
      abrv <- names(abbrev)[.y]
      pwr <- abbrev[.y]
      div <- .x / 10^pwr
      root <- signif(div,sig.digits) %>% format(scientific=FALSE)
      text <- str_c(root,abrv)
    } else {
      text <- signif(.x,sig.digits) %>% format(scientific=FALSE)
    }
    return(text)
  })
}





# data reshaping functions -----------------------------------------------------




#' Inner/Left/Right/Full Join with Replace
#'
#' Same as `inner_join`, `left_join`, `right_join`, and `full_join` in the `dplyr` package, except that variables with the
#' same column name will not be renamed with the ".x" and ".y" suffix.
#' Instead, the variables will be turned into one column if the variables are equal. If they are not equal, an error (or warning) is thrown.
#'
#' This is a convenience function that just avoids the renaming of columns.
#' @param x first data frame to be joined
#' @param y second data frame to be joined
#' @param by a character vector of variables to be joined by.
#' @param conflict what to do if columns conflict.
#' 1. `y` always keep the y-value.
#' 2. `x` always keep the x-value.
#' 3. `y.coalesce` keep the y-value unless it is `NA`.
#' 4. `x.coalesce` keep the x-value unless it is `NA`.
#' 5. `error` throw error if there is a conflict.
#'
#' @export
inner_join_replace <- function(x,y,by=NULL, conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {

  mutual.vars <- intersect(names(x),names(y))
  by.vars <- by %||% mutual.vars
  overlap.vars <- setdiff(mutual.vars,by.vars)
  suffix <- paste0("__",c(rlang::hash(x),rlang::hash(y)))
  data <- inner_join(x,y,by=by.vars,suffix=suffix)
  if (length(overlap.vars)==0) {
    return(data)
  }
  non.ident.vars <- c()
  ident.vars <- c()
  conflict <- arg_match(conflict)

  for (var in overlap.vars) {
    xvar <- paste0(var,suffix[1])
    yvar <- paste0(var,suffix[2])
    is.identical <- identical(data[[xvar]],data[[yvar]])
    if (!is.identical) {
      non.ident.vars <- c(non.ident.vars, var)
    } else {
      ident.vars <- c(ident.vars, var)
    }
    data[[var]] <- switch(conflict,
                          yonly=data[[yvar]],
                          xonly=data[[xvar]],
                          ycoalesce=coalesce(data[[yvar]],data[[xvar]]),
                          xcoalesce=coalesce(data[[xvar]],data[[yvar]]),
                          error={
                            if (is.identical) {
                              data[[yvar]]
                            } else {
                              stop(str_glue("YTError: conflicting column: {var}"))
                            }
                          })
    data[[xvar]] <- NULL
    data[[yvar]] <- NULL
  }
  msg <- switch(conflict,
                yonly=str_glue("Encountered {length(non.ident.vars)} conflicting columns. Using Y col values: {paste(non.ident.vars,collapse=', ')}"),
                xonly=str_glue("Encountered {length(non.ident.vars)} conflicting columns. Using X col values: {paste(non.ident.vars,collapse=', ')}"),
                ycoalesce=str_glue("Encountered {length(non.ident.vars)} conflicting columns. Choosing values with coalesce(y,x): {paste(non.ident.vars,collapse=', ')}"),
                xcoalesce=str_glue("Encountered {length(non.ident.vars)} conflicting columns. Choosing values with coalesce(x,y): {paste(non.ident.vars,collapse=', ')}"),
                error=str_glue("Encountered {length(non.ident.vars)} conflicting columns."))
  warning(str_glue("YTWarning: {msg}"))
  return(data)
}


#' @rdname inner_join_replace
#' @export
left_join_replace <- function(x,y,by=NULL,conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {
  data1 <- inner_join_replace(x,y,by=by,conflict=arg_match(conflict))
  data2 <- anti_join(x,y,by=by)
  bind_rows(data1,data2)
}

#' @rdname inner_join_replace
#' @export
right_join_replace <- function(x,y,by=NULL,conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {

  flip <- function(by) {
    by.names <- names(by)
    if (is.null(by.names)) {
      return(by)
    }
    by.vals <- unname(by)
    by.names <- coalesce(na_if(by.names,""),by.vals)
    setNames(by.names,by.vals)
  }

  data1 <- inner_join_replace(x,y,by=by,conflict=arg_match(conflict))
  data2 <- anti_join(y,x,by=flip(by))
  bind_rows(data1,data2)
}


#' @rdname inner_join_replace
#' @export
full_join_replace <- function(x,y,by=NULL,conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {
  data1 <- inner_join_replace(x,y,by=by,conflict=arg_match(conflict))
  data2 <- anti_join(x,y,by=by)
  data3 <- anti_join(y,x,by=flip(by))
  bind_rows(data1,data2,data3)
}



#' Pivot data from long to wide, with recoding
#'
#' Somewhat similar to [tidyr::pivot_wider()], where but where the names_from column is recoded using `recode.grep`,
#' prior to pivotting. This is primarily useful for restructuring data that comes in long format (name-value pairs).
#'
#' In addition to recoding, there are other changes:
#' 1. The names_from column is recoded using
#' 2. asdf
#'
#' @param data data to be pivotted.
#' @param id_cols [`tidy-select`][`tidyr::tidyr_tidy_select`] columns that identify each observation. Used in [tidyr::pivot_wider()].
#' @param names_from [`tidy-select`][`tidyr::tidyr_tidy_select`] column names to be pivotted. Used in [tidyr::pivot_wider()].
#' @param values_from [`tidy-select`][`tidyr::tidyr_tidy_select`] column values to be pivotted. Used in [tidyr::pivot_wider()].
#' @param names_recodes recodes to be done on `names_from` prior to pivotting. Used in [recode.grep()].
#' @param names_else.value default value of recoding. Used in [recode.grep()].
#' @param names_sort whether to sort columns by order of recode.grep. Default is `TRUE`
#' @param values_fill value used when value is missing. Used in [tidyr::pivot_wider()].
#' @param values_fn How recoded columns are combined. Default is `~paste(.x,collapse="|")`, which collapses into a single string. Specify `NULL` to leave as named vector.
#' @param other_fn How the `names_else.value` column is summarized. Default is `~paste2(names(.x),.x,sep="::",collapse="|")`, which displays name and value. Specify `NULL` to leave as named vector.
#' @param unused_fn A function performed on unused columns. Default is `NULL`.
#'
#' @return Pivotted data
#' @export
#' @examples
#' library(tidyverse)
#' cid.bsi %>%
#'   pivot_wider_recode(
#'     id_cols=Patient_ID,
#'     names_from=organism,
#'     values_from=organism,
#'     names_recodes=c("klebs|coli|serratia|pseudomonas|bacter|acineto|steno|lepto|neiss"="gram.neg",
#'                     "staphylococcus|CNST|VRE|enterococcus|bacillus|micrococc"="gram.pos",
#'                     "clostridium|fusobact|bacteroides"="anaerobe"),
#'     values_fill="<none>")
pivot_wider_recode <- function(data,
                                id_cols = NULL,
                                names_from = name,
                                values_from = value,
                                names_recodes,
                                names_else.value = "other",
                                names_sort = TRUE,
                                values_fill = NULL,
                                values_fn = ~paste2(.x,collapse="|"),
                                other_fn = ~paste2(names(.x),.x,sep="::",collapse="|"),
                                unused_fn = NULL) {

  id_cols <- enquo(id_cols)
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)

  data2 <- data %>%
    mutate(name_=recode.grep(!!names_from,recodes=names_recodes,else.value=names_else.value,as.factor=TRUE),
           placeholder_=TRUE) %>%
    mutate(value_=setNames(!!values_from,!!names_from)) %>%
    complete(name_)

  data3 <- data2 %>%
    pivot_wider(id_cols=c(!!id_cols,placeholder_),
                names_from=name_,
                values_from=value_,
                names_sort=names_sort,
                values_fill=NULL,
                values_fn = list,
                unused_fn=unused_fn) %>%
    filter(!is.na(placeholder_)) %>% select(-placeholder_)

  colvars <- levels(data2$name_)
  othervar <- names_else.value
  namevars <- setdiff(colvars,names_else.value)

  # this handles empty lists
  simplify2 <- function(obj) {
    if (is.list(obj) && length(obj)==0)  {
      return(character())
    } else {
      simplify(obj)
    }
  }

  if (!is.null(values_fn)) {
    data3 <- data3 %>%
      mutate(across(all_of(namevars),~map(.x,values_fn)),
             across(all_of(namevars),simplify2))
  }
  if (!is.null(other_fn)) {
    data3 <- data3 %>%
      mutate(across(all_of(othervar),~map(.x,other_fn)),
             across(all_of(othervar),simplify2))
  }
  return(data3)
}

# pivot_wider_recode.OLD <- function(data,
#                                id_cols = NULL,
#                                names_from = name,
#                                values_from = value,
#                                names_recodes,
#                                names_else.value = "other",
#                                names_sort = TRUE,
#                                values_sep = "::",
#                                values_fill = NULL,
#                                values_fn = ~paste(.x,collapse="|"),
#                                unused_fn = NULL) {
#   id_cols <- enquo(id_cols)
#   names_from <- enquo(names_from)
#   values_from <- enquo(values_from)
#
#   data2 <- data %>%
#     mutate(name_=recode.grep(!!names_from,recodes=names_recodes,else.value=names_else.value,as.factor=TRUE),
#            value_=ifelse(name_!=names_else.value,!!values_from,paste(!!names_from,!!values_from,sep=values_sep)),
#            placeholder_=TRUE) %>%
#     complete(name_) %>%
#     select(-!!names_from,-!!values_from)
#
#   data3 <- data2 %>%
#     pivot_wider(id_cols=c(!!id_cols,placeholder_),
#                 names_from=name_,
#                 values_from=value_,
#                 names_sort=names_sort,
#                 values_fill=values_fill,
#                 values_fn=values_fn,
#                 unused_fn=unused_fn) %>%
#     filter(!is.na(placeholder_)) %>% select(-placeholder_)
#   return(data3)
# }

pivot_wider_partial <- function(data,
                                id_cols = NULL,
                                names_from = name,
                                criteria,
                                by.group=TRUE,
                                names_prefix = "",
                                names_sep = "_",
                                names_glue = NULL,
                                names_sort = FALSE,
                                names_repair = "check_unique",
                                values_from = value,
                                values_fill = NULL,
                                values_fn = NULL) {
  id_cols <- enquo(id_cols)
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)
  criteria <- enquo(criteria)

  if (quo_is_null(id_cols)) {
    name_cols <- tidyselect::eval_select(names_from,data) %>% names()
    value_cols <- tidyselect::eval_select(values_from,data) %>% names()
    by_vars <- setdiff(names(data),c(name_cols,value_cols))
  } else if (by.group && is.grouped_df(data)) {
    by_vars <- data %>% group_vars()
  } else {
    by_vars <- tidyselect::eval_select(id_cols,data) %>% names()
  }

  data.pivot <- data %>% ungroup() %>% filter(!!criteria) %>%
    pivot_wider(id_cols=!!id_cols,
                names_from = !!names_from,
                names_prefix = names_prefix,
                names_sep = names_sep,
                names_glue = names_glue,
                names_sort = names_sort,
                names_repair = names_repair,
                values_from = !!values_from,
                values_fill = values_fill,
                values_fn =values_fn)
  data.base <- data %>% ungroup() %>% filter(!(!!criteria))
  pdata <- data.base %>% full_join(data.pivot,by=by_vars)
  newvars <- setdiff(names(pdata),names(data))
  new.grouping <- c(by_vars,newvars)
  if (by.group) {
    pdata <- pdata %>% group_by(!!!syms(new.grouping))
  }
  return(pdata)
}







# ggplot helpers ----------------------------------------------------------


#' Color Shades
#'
#' Creates different shades of the specified color.
#'
#' Use this as a convenience function when creating your plots.
#' @param color character, specifying the color you want to build shades around. (e.g. `"red"` or `"#1460fa"`)
#' @param ncolor number specifying the length of the vector, i.e. how many different shades should be returned (default 3 shades).
#' @param variation a number from 0-1, which determines how different the shades will be. Smaller numbers will be more similar.
#' @return Produces a character vector of colors, corresponding to shades of the specified color.
#' @examples
#' sh1 <- shades("red",5)
#' scales::show_col(sh1)
#'
#' sh2 <- shades("red",20)
#' scales::show_col(sh2)
#'
#' sh3 <- shades("red",20,variation=0.5)
#' scales::show_col(sh3)
#' @author Ying Taur
#' @export
shades <- function(color,ncolor=3,variation=1) {
  #shades("red",3) will give 3 shades of red.
  #pct.variation should be 0-1, and determines variance. lower values will be more similar.
  total.colors <- 100
  end.index <- total.colors * variation
  white.end <- colorRampPalette(c(color,"white"),space="rgb")(total.colors)[end.index]
  black.end <- colorRampPalette(c(color,"black"),space="rgb")(total.colors)[end.index]
  pal <- colorRampPalette(c(white.end,color,black.end),space="rgb")(ncolor+2)
  pal <- pal[c(-1,-length(pal))]
  return(pal)
}




#' Display values for ggplot's shape aesthetic
#'
#' Used for quick reference
#' @export
show_shapes <- function() {
  d <- data.frame(p=c(0:25,32:127))
  ggplot() +
    scale_y_continuous(name="") +
    scale_x_continuous(name="") +
    scale_shape_identity() +
    geom_point(data=d, mapping=aes(x=p%%16, y=p%/%16, shape=p), size=5, fill="red") +
    geom_text(data=d, mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), size=3)
}

#' Display values for ggplot's linetype aesthetic
#'
#' Used for quick reference
#' @export
show_linetypes <- function() {
  d <- data.frame(lt=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"))
  ggplot() +
    scale_x_continuous(name="", limits=c(0,1), breaks=NULL) +
    scale_y_discrete(name="linetype") +
    scale_linetype_identity() +
    geom_segment(data=d, mapping=aes(x=0, xend=1, y=lt, yend=lt, linetype=lt))
}

#' Extract legend from a ggplot2 object
#' @export
gg.legend <- function(a.gplot) {
  #extract legend, so it can be used with grid.arrange or whatever
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#' Default color palette of ggplot2
#'
#' The color palette that ggplot2 uses by default.
#'
#' @param n number of colors to display
#' @param h The hue of the color specified as an angle in the range [0,360]. 0 yields red, 120 yields green 240 yields blue, etc.
#' @return A color palette that would have been used by ggplot2 by default
#' @examples
#' colors <- gg.colors(6)
#' scales::show_col(colors)
#' @author Ying Taur
#' @export
gg.colors <- function(n=6, h=c(0,360)+15) {
  #emulates ggplot's default discrete color palette
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}


#' Stack and line up ggplot objects in a column
#'
#' Use this to arrange ggplot objects, where the axes, plot, and legend are lined up correctly.
#'
#' Performs these steps:
#' 1. change margins so that plots are closer together
#' 2. alters widths of each component so that the plots will line up nicely
#' 3. calls `grid.arrange(...,ncol=1)`
#'
#' If a `NULL` value is passed to the plot list, that plot and the corresponding height value will be omitted.
#'
#' @param ... ggplot objects to be stacked. Can also supply a formula where left hand side is ggplot, right hand side is height.
#' @param heights a numeric vector representing the relative height of each plot. Passed directly to [gridExtra::grid.arrange()].
#' @param align.xlim logical, whether or not to alter the x-limits in each plot to match. Default is `FALSE`. (Note this is experimental and can potentially fail in strange situations)
#' @param adjust.themes logical, whether or not to adjust each plot's theme for stacking (change gap/margin, suppress x-axis in upper plots). Default `TRUE`.
#' @param gg.extras a list of ggplot objects that will be applied to all plots. Default is `NULL`.
#' @param gap size of gap between stacked plots. Default is 0
#' @param margin size of the margin around the plots. Default is 5.5.
#' @param units specifies units used for gap and margin. Default is "pt"
#' @param newpage logical, whether or not to erase current grid device. Default is TRUE. (Note, should turn this off if using in a shiny plot)

#' @param as.gtable logical, whether or not to return as a gtable object (i.e. don't execute `grid.draw`). Default is `FALSE`. Do this if you want to do more arranging afterwards.
#'
#' @return plot of stacked ggplots
#' @export
#' @examples
#' g1 <- ggplot(mtcars,aes(x=mpg,y=disp,color=factor(cyl))) + geom_point()
#' g2 <- ggplot(mtcars,aes(x=mpg,y=wt,fill=factor(cyl))) + geom_col() + scale_fill_discrete("Number of Cylinders")
#' g3 <- ggplot(mtcars,aes(x=mpg,y=wt,label=cyl,fill=factor(cyl))) + geom_label()
#'
#' # grid.arrange does not align correctly, basically due to because of legend/axis differences
#' gridExtra::grid.arrange(g1,g2,g3,ncol=1)
#' # gg.stack aligns correctly
#' gg.stack(g1,g2,g3)
#' # vary the heights
#' gg.stack(g1,g2,g3,heights=c(1,2,3))
#' # alternatively, use formulas to specify
#' gg.stack(g1~3,
#'          g2~2,
#'          g3~1,heights=c(1,2,3))
gg.stack <- function (..., heights = NULL, align.xlim = FALSE, adjust.themes = TRUE, gg.extras = NULL, gap = 0, margin = 5.5, units = "pt", newpage = TRUE, as.gtable = FALSE) {
  requireNamespace(c("grid", "gridExtra", "gtable"), quietly = TRUE)
  grobs <- list(...)

  if (all(map_lgl(grobs, rlang::is_formula))) {
    default_env <- caller_env()
    heights <- grobs %>% map(~eval_tidy(f_rhs(.x),env=default_env))
    grobs <- grobs %>% map(~eval_tidy(f_lhs(.x),env=default_env))
  }

  keep <- !sapply(grobs, is.null)
  if (!is.null(heights)) {
    if (length(grobs) != length(heights)) {
      stop("YTError: number of grobs does not match the number of heights.")
    }
    heights <- heights[keep]
  }
  else {
    heights <- rep(1, length.out = length(grobs))
  }
  grobs <- grobs[keep]
  if (align.xlim) {
    grobs <- gg.align.xlim(grobs)
  }
  length.grobs <- length(grobs)
  if (length.grobs > 1) {
    g.top <- grobs[[1]]
    g.middle.list <- lapply(grobs[c(-1, -length.grobs)], function(g) {
      g
    })
    g.bottom <- grobs[[length.grobs]]
    if (adjust.themes) {
      top.theme <- theme(plot.margin = unit(c(margin, margin, gap, margin), units), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      middle.theme <- theme(plot.margin = unit(c(gap, margin, gap, margin), units), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      bottom.theme <- theme(plot.margin = unit(c(gap, margin, margin, margin), units))
      g.top <- g.top + top.theme
      g.middle.list <- lapply(g.middle.list, function(g) {
        g + middle.theme
      })
      g.bottom <- g.bottom + bottom.theme
    }
    grobs1 <- c(list(g.top), g.middle.list, list(g.bottom))
  }
  else {
    grobs1 <- grobs
  }
  grobs2 <- lapply(grobs1, function(g) {
    gr <- ggplotGrob(g)
    return(gr)
  })
  nwidths <- max(sapply(grobs2, function(g) length(g$width)))
  grobs3 <- lapply(grobs2, function(g) {
    columns.needed <- nwidths - length(g$widths)
    if (columns.needed > 0) {
      for (x in 1:columns.needed) {
        g <- gtable::gtable_add_cols(g, unit(1, "null"))
      }
    }
    return(g)
  })
  grobs4 <- mapply(function(gr, ht) {
    ht.char <- as.character(gr$heights)
    null.heights <- grep("null", ht.char)
    relative.heights <- as.numeric(sub("null", "", ht.char[null.heights]))
    total.null.height <- sum(relative.heights)
    gr$heights[null.heights] <- gr$heights[null.heights] * (1/total.null.height) * ht
    return(gr)
  }, grobs3, heights, SIMPLIFY = FALSE)
  args <- c(grobs4, list(size = "max"))
  gtable.final <- do.call(gridExtra::gtable_rbind, args)
  if (as.gtable) {
    return(gtable.final)
  }
  else {
    if (newpage) {
      grid::grid.newpage()
    }
    grid::grid.draw(gtable.final)
  }
}



#' Stack and line up ggplot objects in a column
#'
#' Use this to arrange ggplot objects, where the axes, plot, and legend are lined up correctly.
#'
#' Makes use of [`patchwork`][`patchwork-package`] package to align. The previous version ([gg.stack()])
#' manually adjusted widths to perform the aligment.
#'
#' @param ... ggplot objects to be stacked. Can also supply a formula where left hand side is ggplot, right hand side is height.
#' @param heights a numeric vector representing the relative height of each plot. Passed directly to [gridExtra::grid.arrange()].
#' @param adjust.themes logical, whether or not to adjust each plot's theme for stacking (change gap/margin, suppress x-axis in upper plots). Default `TRUE`.
#' @param gg.extras a list of ggplot objects that will be applied to all plots. Default is `NULL`.
#' @param gap size of gap between stacked plots. Default is `unit(0,"pt")`.
#' @param margin size of the margin around the plots. Default is `unit(5.5,"pt")`, which is the `ggplot2` default.
#' @return plot of stacked ggplots
#' @export
#' @examples
#' g1 <- ggplot(mtcars,aes(x=mpg,y=disp,color=factor(cyl))) + geom_point()
#' g2 <- ggplot(mtcars,aes(x=mpg,y=wt,fill=factor(cyl))) + geom_col() + scale_fill_discrete("Number of Cylinders")
#' g3 <- ggplot(mtcars,aes(x=mpg,y=wt,label=cyl,fill=factor(cyl))) + geom_label()
#'
#' # grid.arrange does not align correctly, basically due to because of legend/axis differences
#' gridExtra::grid.arrange(g1,g2,g3,ncol=1)
#' # gg.stack2 aligns correctly
#' gg.stack2(g1,g2,g3)
#' # vary the heights
#' gg.stack2(g1,g2,g3,heights=c(1,2,3))
#' # alternatively, use formulas to specify
#' gg.stack2(g1~3,
#'          g2~2,
#'          g3~1,heights=c(1,2,3))
gg.stack2 <- function(..., heights = NULL, gap=unit(0,"pt"),
                      margin = theme_get()$plot.margin, adjust.themes = TRUE,
                      return.gg.list=FALSE) {
  requireNamespace("grid", quietly = TRUE)
  grobs <- list(...)

  if (all(map_lgl(grobs, rlang::is_formula))) {
    default_env <- caller_env()
    heights <- grobs %>% map(~eval_tidy(f_rhs(.x),env=default_env))
    grobs <- grobs %>% map(~eval_tidy(f_lhs(.x),env=default_env))
  }
  keep <- !sapply(grobs, is.null)
  grobs <- grobs[keep]
  length.grobs <- length(grobs)
  if (length.grobs > 1) {
    if (adjust.themes) {
      g.top <- grobs[[1]]
      g.middle.list <- lapply(grobs[c(-1, -length.grobs)], function(g) {
        g
      })
      g.bottom <- grobs[[length.grobs]]
      top.theme <- theme(plot.margin = grid::unit.c(margin[1], margin[2], gap, margin[4]),
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank())
      middle.theme <- theme(plot.margin = grid::unit.c(gap, margin[2], gap, margin[4]),
                            axis.title.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank())
      bottom.theme <- theme(plot.margin = grid::unit.c(gap, margin[2], margin[3], margin[4]))
      g.top <- g.top + top.theme
      g.middle.list <- lapply(g.middle.list, function(g) {
        g + middle.theme
      })
      g.bottom <- g.bottom + bottom.theme
    }
    grobs1 <- c(list(g.top), g.middle.list, list(g.bottom))
  } else {
    grobs1 <- grobs
  }
  if (return.gg.list){
    return(grobs1)
  }

  Reduce(`+`, grobs1) + plot_layout(ncol=1,heights=heights)
}







#' Align X-Limits
#'
#' For a given list of ggplot objects, make the x-limits the same across all plots.
#'
#' This is useful when stacking plots like in [gg.stack()].
#' @param glist a list of ggplot objects
#'
#' @return a modified list of ggplot objects, with modified x-limits
#' @export
gg.align.xlim <- function(glist) {
  requireNamespace(c("ggfun","aplot"),quietly=TRUE)
  new.xlim <- glist %>% map(ggfun::xrange) %>% transpose() %>%
    simplify_all() %>% map2_dbl(list(min,max),~.y(.x))
  new.glist <- glist %>% map(~.x + aplot::xlim2(limits=new.xlim))
  return(new.glist)
}





#' Time bars
#'
#' This geom and scale are used to transform the x-scale such that the scale moves slowly at certain points (`day`).
#' The x-scale can be numeric or date.
#'
#' @param ... arguments passed to [ggplot2::scale_x_continuous()]
#' @param xlim 2-value vector specifying the x-axis limits.
#' @param days vector of values where
#' @param div size of the bars, specified as reciprocal fraction of plot width. Default is 30 (1 bar is 1/30th of plot width).
#' @export
#' @examples
#' data <- tibble(days=c(1,2,50,100))
#' xlim <- c(-10,200)
#' ggplot(data) +
#'   geom_col(aes(x=days,y=1,fill=factor(days)),width=1) +
#'   scale_x_timebars(xlim=xlim,days=data$days,div=10)
scale_x_timebars <- function( days=NULL, xlim=NULL, div=30, breaks = NULL, ... ) {
  days <- unique(days) %>% sort()
  if (length(days)==0) {
    message("YTNote: No days specified. X-axis will not be transformed.")
    return(scale_x_continuous(...))
  }
  if (is.null(xlim)) {
    xwidth <- max(days)-min(days)+1
    # x width should be minimum div.
    min.pad <- (div-xwidth) / 2
    ndays <-  length(days)
    average.dist <- xwidth/ndays
    dist.pad <- average.dist
    pad <- ceiling(max(min.pad,dist.pad))
    xlim <- range(days) + c(-pad,pad)
  }
  is.betw <-is.between(days,xlim[1],xlim[2])
  if (!all(is.betw)) {
    warning("YTWarning: not all days fall within xlim!")
    days <- days[is.betw]
  }
  if (is.null(breaks))  {
    breaks <- days
  }
  xlim.real <- xlim + c(-0.5,0.5)

  trans <- barwidth_spacing_trans(days=days,xlim=xlim.real,div=div)
  list(scale_x_continuous(... , expand=c(0,0),
                          breaks=breaks,
                          trans=trans),
       coord_cartesian(xlim=xlim.real)
  )
}




#' @export
#' @rdname scale_x_timebars
barwidth_spacing_trans <- function(days,xlim,div) {
  is_date <- lubridate::is.Date(days)

  xlim <- as.numeric(xlim)
  xlim.width <- as.numeric(xlim[2]) - as.numeric(xlim[1])
  n.days <- length(days)
  t.width <- div

  if (t.width<=n.days){
    warning("YTWarning: div must be more than length(days). Increasing div.")
    t.width <- n.days * 1.01
  }
  nonbar.rate <- (xlim.width-n.days) / (t.width-n.days)
  trans <- function(y) {
    sapply(y,function(yy) {
      bar.lengths <- sum(pmax(pmin(as.numeric(yy)-(as.numeric(days)-0.5),1),0))
      nonbar.lengths <- as.numeric(yy)-xlim[1]-bar.lengths
      ans <- bar.lengths + (nonbar.lengths / nonbar.rate)
      ans
      return(ans)
    })
  }
  days.trans <- trans(days)
  inv <- function(x) {
    sapply(x,function(xx) {
      bar.lengths <- sum(pmax(pmin(xx-(days.trans-0.5),1),0))
      nonbar.lengths <- xx-bar.lengths
      ans <- bar.lengths + (nonbar.lengths * nonbar.rate) + xlim[1]
      return(ans)
    })
  }
  if (is_date) {
    format <- function(x) {as.Date(x,origin="1970-01-01")}
  } else {
    format <- round
  }
  breaks <- days
  major <- function() {
    function(x) {
      return(breaks)
    }
  }
  minor <- function () {
    function(b, limits, n) {
      dlimits <- limits %>% inv()
      br <- seq(dlimits[1],dlimits[2],by=1)
      br2 <- br %>% trans()
      return(br2)
    }
  }
  scales::trans_new("barwidth_spacing",
                    transform=trans,
                    inverse=inv,
                    # breaks=major(),
                    format=format,
                    minor=minor())
}




#' Plot timeline bars
#'
#' Plot timeline items in the form of bars, such as medication administration over time. The X-axis represents time.
#'
#' This custom geom uses [get.row()] to arrange the timeline events into rows, where aesthetics are mapped
#' to the function parameters:
#' * `aes(xmin=)` is mapped to `get.row(start=)`
#' * `aes(xmax=)` is mapped to `get.row(xstop=)`
#' * `aes(label=)` is mapped to `get.row(row=)`
#' * `aes(by=)` is mapped to `get.row(by=)`
#'
#' Some data prepping is done prior to plotting:
#' 1. Merge any timeline events that overlap using [group_by_time()] (optional step if `merge=TRUE`)
#' 2. Pad each event by +/- 0.45 days so that they span the length of the days they occur on, and so single day events do not have zero width.
#' 3. Determine Y = row position for all events, using [get.row()], using above aesthetic mappings.
#' 4. Calculate X midpoint of each timeline event, for plotting of the label.
#' If X-axis transformations are used (e.g. [scale_x_timebar()]), note that merging (#1) and padding (#2) are done
#' on un-transformed data, whereas `get.row()` (#3) and midpoint (#4) are done after transformation. This is more
#' seamless compared with the hassle of doing manually.
#' @eval ggplot2:::rd_aesthetics("geom", "timeline")
#' @inheritParams get.row
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_text
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param merge whether or not to merge adjacent/overlapping bars into 1 row before plotting (using [group_by_time()]). Default is `TRUE`.
#' Note that merging only occurs if the bars are overlapping, and have the same label and fill.
#' @param merge.gap if `merge=TRUE`, the maximum distance between two bars that are merged. Default is `1`.
#' @param min.gap The allowable gap before two distinct rows are fitted on the same row, expressed as a proportion of the X-axis length. Note that this different than direct use of [get.row()]
#' @param row.overlap
#' @param check_overlap
#' @param linejoin
#' @param na.rm
#' @param show.legend
#' @param parse
#' @param inherit.aes
#' @return
#' @export
#' @examples
#' library(tidyverse)
#' data <- cid.meds %>% filter(Patient_ID=="166")
#' # Standard timeline plot without much cleanup
#' ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
#'                                  label=med.clean,by=med.class,fill=med.class),alpha=0.7)
#'
#' # Make it look nice and clean with merge and check_overlap, use min.gap to share rows where possible.
#' ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
#'                                  label=med.clean,by=med.class,fill=med.class),alpha=0.7,
#'                              merge=TRUE,check_overlap=TRUE,min.gap=0.15)
#'
#' # To see all events separately (even same labelled events never overlap), use row.overlap=FALSE
#' ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
#'                                  label=med.clean,by=med.class,fill=med.class),alpha=0.7,
#'                              row.overlap = FALSE)
#'
#' # To fit in as few rows as possible, try removing by, and set min.gap=0 and merge=TRUE.
#' ggplot(data) + geom_timeline(aes(xmin=startday,xmax=endday,
#'                                  label=med.clean,fill=med.class),alpha=0.7,
#'                              merge=TRUE,min.gap=0,check_overlap=TRUE)
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "timeline", position = "identity",
                          ...,
                          merge = FALSE,
                          min.gap = 1,
                          row.overlap=  TRUE,
                          check_overlap = FALSE,
                          merge.gap = 1,
                          linejoin = "mitre",
                          na.rm = FALSE,
                          show.legend = NA,
                          parse = FALSE,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      linejoin = linejoin,
      na.rm = na.rm,
      parse = parse,
      check_overlap = check_overlap,
      min.gap = min.gap,
      row.overlap = row.overlap,
      merge = merge,
      merge.gap = merge.gap,
      ...
    )
  )
}





GeomTimeline <- ggproto("GeomTimeline", GeomRect,
                        default_aes = aes(
                          label = "",
                          by = NA,
                          fill = "grey35",
                          colour = NA,
                          linewidth = 0.5,
                          linetype = 1,
                          alpha = NA,
                          fontcolour="black",
                          size=3.88,
                          angle=0,
                          hjust=0.5,
                          vjust=0.5,
                          fontalpha=NA,
                          family="",
                          fontface=1,
                          lineheight=0.75),
                        required_aes = c("xmin", "xmax"),
                        draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre",
                                              parse = FALSE, check_overlap = FALSE, inherit.aes = TRUE) {
                          grob1 <- GeomRect$draw_panel(data=data, panel_params = panel_params, coord = coord,
                                                       lineend = lineend, linejoin = linejoin)
                          data2 <- data %>% mutate(colour = fontcolour, alpha = fontalpha)
                          grob2 <- GeomText$draw_panel(data=data2, panel_params=panel_params, coord=coord,
                                                       parse = parse, na.rm = na.rm,
                                                       check_overlap = check_overlap)
                          grid::gTree("timeline_grob", children = gList(grob1, grob2))
                        }
)



# Use this to create rows using get.row. This can't be done in GeomTimeline because
# the scale transformations are easily available.
StatTimeline <- ggproto("StatTimeline",Stat,
                        default_aes = aes(
                          label = "",
                          by = NA,
                          fill = "grey35",
                          colour = NA,
                          linewidth = 0.5,
                          linetype = 1,
                          alpha = NA,
                          fontcolour="black",
                          size=3.88,
                          angle=0,
                          hjust=0.5,
                          vjust=0.5,
                          fontalpha=NA,
                          family="",
                          fontface=1,
                          lineheight=0.75),
                        required_aes = c("xmin", "xmax"),
                        compute_panel = function(self, data, scales,
                                                 merge=FALSE, merge.gap=1,
                                                 min.gap = 1, row.overlap=TRUE) {
                          #transform back to pre-transformation values,
                          # perform merging and row creation,
                          # then re-transform.
                          inv <- scales$x$trans$inverse
                          trans <- scales$x$trans$transform
                          grouping_vars <- setdiff(names(data),c("xmin", "xmax"))
                          data2 <- data %>%
                            mutate(xmin0=inv(xmin),
                                   xmax0=inv(xmax))
                          if (merge)  {
                            data2 <- data2 %>%
                              group_by_time(xmin0,xmax0,!!!syms(grouping_vars),gap=merge.gap) %>%
                              summarize(xmin0=min(xmin0),
                                        xmax0=max(xmax0),
                                        .groups="drop")
                          }
                          if (is.null(data$by)) {
                            qby <- expr(NULL)
                          } else {
                            qby <- expr(by)
                          }
                          if (is.null(data$label)) {
                            qlabel <- expr(NULL)
                          } else {
                            qlabel <- expr(label)
                          }
                          #slightly off because of expansion.
                          xlim1 <- scales$x$dimension()
                          # xlim2 <- scales$x$range$range
                          xwidth <- xlim1[2] - xlim1[1]
                          min.gap.true  <-  min.gap * xwidth

                          newdata <- data2 %>%
                            mutate(xmin0=xmin0-0.45,
                                   xmax0=xmax0+0.45,
                                   xmin=trans(xmin0),
                                   xmax=trans(xmax0)) %>%
                            group_by(PANEL) %>%
                            mutate(y=get.row(xmin,xmax,row=!!qlabel,by=!!qby,
                                             min.gap = min.gap.true,
                                             row.overlap=row.overlap)) %>%
                            ungroup() %>%
                            mutate(ymin=y-0.45,
                                   ymax=y+0.45,
                                   x=midpoint(xmin,xmax)) %>%
                            select(-xmin0,-xmax0)

                          ggproto_parent(StatIdentity, self)$compute_layer(newdata, params, layout)
                        })






#' Draw Brackets
#'
#' Draw brackets in a ggplot.
#'
#' This draws brackets by using grid-drawking methods from [geom_text()], [geom_segment()], and [geom_curve()].
#' @eval ggplot2:::rd_aesthetics("geom", "bracket")
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::geom_text
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param na.rm
#' @param bracket.width size of bracket edges. Default is `unit(0.75,'char')`.
#' @param tip The style of bracket to use. Can be one of: `"round", "bare", "square", "ibeam", "arrow", "iarrow", "curly"`. Default is `"round"`.
#' @param arrow The style of arrow to use, if `tip="arrow"` or `tip="iarrow"`. Default selection is based on `bracket.width`.
#' @param arrow.fill Fill color to use for closed arrowheads, if `tip="arrow"` or `tip="iarrow"`.
#' @param lineend
#' @param linejoin
#' @param parse
#' @param check_overlap
#' @param inherit.aes
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' mt <- mtcars %>%
#'   mutate(mpg.group=cut2(mpg, n.splits=3, lvls=c("Low MPG", "Med MPG", "High MPG")))
#' g <- ggplot(mt) +
#'   geom_point(aes(x=mpg, y=hp, color=mpg.group), size=3)
#'
#' # whole-data brackets
#' g + geom_bracket(aes(x=mpg, y=350), label="MPG values") +
#'   geom_bracket(aes(x=35, y=hp), label="HP values", flip=TRUE)
#'
#' # brackets by group
#' g + geom_bracket(aes(x=mpg, y=350, label=mpg.group))
#'
#' # adjust the brackets with padding
#' g + geom_bracket(aes(x=mpg - 0.25, xend=mpg + 0.25, y=350, label=mpg.group))
#'
#' # brackets by group, vary height
#' g + geom_bracket(aes(x=mpg, y=ave(hp,mpg.group,FUN=max)+20, yend=ave(hp,mpg.group,FUN=max)+20, label=mpg.group))
#'
#' # manually specified brackets
#' g + geom_bracket(aes(x=18, y=225, yend=350, label="Tier 1"),flip=TRUE) +
#'   geom_bracket(aes(x=18, y=220, xend=25,yend=150, label="Tier 2")) +
#'   geom_bracket(aes(x=26,xend=35, y=150, label="Tier 3"))
#'
#' # customization
#' g + geom_bracket(aes(x=27, y=200, yend=350, label="200+ HP"), flip=TRUE,
#'   linewidth=2, color="purple", fontsize=7, angle=0, hjust=0, fontface="italic", bracket.width=unit(0.5,"in"))
#'
#' # various tip styles
#' ggplot() +
#'   geom_bracket(aes(x=1, xend=5, y=7, label="round"), tip="round") +
#'   geom_bracket(aes(x=1, xend=5, y=6, label="bare"), tip="bare") +
#'   geom_bracket(aes(x=1, xend=5, y=5, label="square"), tip="square") +
#'   geom_bracket(aes(x=1, xend=5, y=4, label="ibeam"), tip="ibeam") +
#'   geom_bracket(aes(x=1, xend=5, y=3, label="arrow"), tip="arrow") +
#'   geom_bracket(aes(x=1, xend=5, y=2, label="iarrow"), tip="iarrow") +
#'   geom_bracket(aes(x=1, xend=5, y=1, label="curly"), tip="curly") +
#'   xlim(0,6)
geom_bracket <- function(mapping = NULL,
                         data = NULL,
                         stat = StatBracket,
                         position = "identity",
                         ### fixed params ###
                         ... ,
                         na.rm = FALSE,
                         bracket.width = unit(0.75,"char"),
                         tip="round",
                         arrow = grid::arrow(angle = 20, length = bracket.width, ends = "both", type = "closed"),
                         arrow.fill=NULL,
                         lineend = "butt",
                         linejoin = "round",
                         parse = FALSE,
                         check_overlap = FALSE,
                         ####################
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomBracket,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(na.rm = na.rm,
                       bracket.width = bracket.width,
                       tip = tip,
                       arrow = arrow,
                       arrow.fill = arrow.fill,
                       lineend = lineend,
                       linejoin = linejoin,
                       parse = parse,
                       check_overlap = check_overlap,
                       ...))
}


#' @export
GeomBracket <- ggproto("GeomBracket",Geom,
                       required_aes = c("x", "y", "xend", "yend"),
                       default_aes = aes(
                         "flip"=FALSE,     # text
                         "label"=NA,       # text
                         "colour"="black", # segment+text
                         "linewidth"=0.5,  # segment
                         "linetype"=1,     # segment
                         "angle"=NA,       # text
                         "vjust"=NA,       # text
                         "hjust"=NA,       # text
                         "alpha"=NA,       # segment+text
                         "fontsize"=3.88,  # text
                         "family"="",      # text
                         "fontface"=1,     # text
                         "lineheight"=0.8  # text
                       ),
                       # draw_layer = function(self, data, params, layout, coord) {
                       #   ggproto_parent(GeomSegment, self)$draw_layer(data, params, layout, coord)
                       # },
                       draw_panel = function (self, data, panel_params, coord,
                                              na.rm = FALSE,
                                              bracket.width = unit(1,"cm"),
                                              tip="round",
                                              arrow = NULL,
                                              arrow.fill = NULL,
                                              lineend = "butt",
                                              linejoin = "round",
                                              parse = FALSE,
                                              check_overlap = FALSE) {
                         # checks copied from GeomSegment$draw_panel
                         data <- ggplot2:::check_linewidth(data, snake_class(self))
                         data <- remove_missing(data, na.rm = na.rm, c("x", "y", "xend", "yend", "linetype", "linewidth", "shape"),
                                                name = "geom_backet")
                         if (ggplot2:::empty(data))
                           return(zeroGrob())
                         data <- data %>% mutate(x0=x,xend0=xend,y0=y,yend0=yend,
                                                 x=ifelse(flip,xend0,x0),
                                                 xend=ifelse(flip,x0,xend0),
                                                 y=ifelse(flip,yend0,y0),
                                                 yend=ifelse(flip,y0,yend0)) %>%
                           select(-x0,-xend0,-y0,-yend0,-flip)
                         if (coord$is_linear()) {
                           coord <- coord$transform(data, panel_params)
                           arrow.fill <- arrow.fill %||% coord$colour

                           grob_bracket <- bracketGrob(
                             x0 = coord$x,
                             y0 = coord$y,
                             x1 = coord$xend,
                             y1 = coord$yend,
                             default.units = "native",
                             bracket.width = bracket.width,
                             tip = tip,
                             arrow = arrow,
                             gp.segment = gpar(
                               col = alpha(coord$colour,coord$alpha),
                               fill = alpha(arrow.fill, coord$alpha),
                               lwd = coord$linewidth * .pt,
                               lty = coord$linetype,
                               lineend = lineend,
                               linejoin = linejoin
                             ),
                             label=coord$label,
                             angle=coord$angle,
                             hjust=coord$hjust,
                             vjust=coord$vjust,
                             check.overlap = check_overlap,
                             gp.text = gpar(
                               col = alpha(coord$colour,data$alpha),
                               fontsize = coord$fontsize * .pt,
                               fontfamily = coord$family,
                               fontface = coord$fontface,
                               lineheight = coord$lineheight
                             ))

                         } else { #non linear coord....
                           data_segment$group <- 1:nrow(data_segment)
                           starts <- subset(data_segment, select = c(-xend, -yend))
                           ends <- rename(subset(data_segment, select = c(-x, -y)), c(xend = "x", yend = "y"))
                           pieces <- vec_rbind0(starts, ends)
                           pieces <- pieces[order(pieces$group), ]
                           grob_bracket <- GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow, lineend = lineend)
                         }

                         #### assembled ####
                         ggplot2:::ggname("bracket_grob", grid::gTree(children = grid::gList(grob_bracket)))
                       }
)



#' @export
StatBracket <- ggproto("StatBracket",Stat,
                       required_aes = c("x","y"),
                       compute_layer = function(self, data, params, layout) {
                         newdata <- data %>%
                           mutate(xend={if ("xend" %in% names(.)) xend else x},
                                  yend={if ("yend" %in% names(.)) yend else y}) %>%
                           group_by(across(-all_of(c("x","y","xend","yend")))) %>%
                           summarize(x=min(x),
                                     xend=max(xend),
                                     y=min(y),
                                     yend=max(yend),
                                     .groups="drop")
                         return(newdata)
                       })




#' bracketGrob
#'
#' This creates the brackets in [geom_bracket()].
#' @examples
#' library(tidyverse)
#' x0=0.15; x1=0.85; y0=0.5; y1=0.5; bw=0.1
#' ggplot(mtcars) +
#'   geom_bracket(aes(x=x0,xend=x1,y=y0,yend=y1,label="[Bracket label]"),linewidth=1.25,fontsize=6,bracket.width = unit(bw,"npc")) +
#'   annotate("segment",x=x0,y=y0,xend=x1,yend=y1,linetype="longdash",color="blue") +
#'   annotate("point",x=x0,y=y0,color="blue") +
#'   annotate("text",x=x0,y=y0+0.01,label="(x,y)",color="blue") +
#'   annotate("point",x=x1,y=y1,color="blue") +
#'   annotate("text",x=x1,y=y1+0.01,label="(xend,yend)",color="blue") +
#'   geom_bracket(aes(x=x0-0.35*bw,y=y0,yend=y0-bw,label="bracket.width"),curly=FALSE,color="blue",flip=TRUE) +
#'   geom_bracket(aes(x=0.6,y=y0,yend=y0+bw,label="bracket.width"),curly=FALSE,color="blue",flip=TRUE) +
#'   geom_bracket(aes(x=x0,xend=x0+2.5*bw,y=y0-1.25*bw,label="curve starts at\n2.5 * bracket.width"),curly=FALSE,color="blue",flip=TRUE) +
#'   coord_fixed(xlim=c(0,1),ylim=c(0,1),expand=FALSE) +
#'   theme_void()
#' @export
bracketGrob <- function (x0 = unit(0, "npc"),
                         y0 = unit(0, "npc"),
                         x1 = unit(1, "npc"),
                         y1 = unit(1, "npc"),
                         default.units = "npc",
                         bracket.width = unit(1,"cm"),
                         tip = "round",
                         arrow = NULL,
                         gp.segment = gpar(),
                         label = NA,
                         angle = NA,
                         hjust = NULL,
                         vjust = NULL,
                         check.overlap = FALSE,
                         gp.text = gpar(),
                         name = NULL,
                         vp = NULL) {
  # tip <- match.arg(arg = "tip", choices = c("round", "square"))

  if (!is.unit(x0))
    x0 <- unit(x0, default.units)
  if (!is.unit(x1))
    x1 <- unit(x1, default.units)
  if (!is.unit(y0))
    y0 <- unit(y0, default.units)
  if (!is.unit(y1))
    y1 <- unit(y1, default.units)

  gTree(
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    label = label,
    bracket.width = bracket.width,
    tip = tip,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    check.overlap = check.overlap,
    arrow = arrow, name = name,
    gp.segment = gp.segment,
    gp.text = gp.text,
    vp = vp, cl = "bracket")
}



#' @export
makeContent.bracket <- function(x) {

  x0 <- convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- convertY(x$y1, "mm", valueOnly = TRUE)
  bracket.width <- convertUnit(x$bracket.width, unitTo = "mm",valueOnly = TRUE)
  tip <- x$tip
  arrow <- x$arrow
  label <- x$label
  check.overlap = x$check.overlap
  gp.segment <- x$gp.segment
  gp.text <- x$gp.text
  vp <- x$vp
  #calculate vectors
  xdiff <- x1 - x0
  ydiff <- y1 - y0
  length.segment <- sqrt(xdiff^2 + ydiff^2)
  x.vector <- xdiff/length.segment
  y.vector <- ydiff/length.segment
  x.right.vector <- y.vector
  y.right.vector <- -x.vector

  text.angle.calc <- atan((y1 - y0)/(x1 - x0)) * 180/pi
  text.angle <- coalesce(x$angle, text.angle.calc)
  vjust <- coalesce(x$vjust, 0.5)
  hjust <- coalesce(x$hjust, 0.5)
  skew <- 1.75

  if (tip=="round") {
    curve.too.big <- 2*bracket.width*skew > length.segment

    if (any(curve.too.big)) {
      skew = min(length.segment)/2/bracket.width
      warning(str_glue("YTWarning: segment length (length.segment/2) is smaller than bracket width*skew. Changing skew to {skew}"))
    }
  } else if (tip=="curly") {
    curve.too.big <- 4*bracket.width*skew > length.segment
    if (any(curve.too.big)) {
      skew = min(length.segment)/4/bracket.width
      warning(str_glue("YTWarning: segment length (length.segment/2) is smaller than bracket width*skew. Changing skew to {skew}"))
    }
  }

  xmiddle <- midpoint(x0, x1)
  ymiddle <- midpoint(y0, y1)
  # round, curly
  x0.inset <- x0 + skew * x.vector * bracket.width
  y0.inset <- y0 + skew * y.vector * bracket.width
  x1.inset <- x1 - skew * x.vector * bracket.width
  y1.inset <- y1 - skew * y.vector * bracket.width
  # round, curly, square
  x1.tick1 <- x0 + x.right.vector * bracket.width
  y1.tick1 <- y0 + y.right.vector * bracket.width
  x1.tick2 <- x1 + x.right.vector * bracket.width
  y1.tick2 <- y1 + y.right.vector * bracket.width
  # ibeam, iarrow
  x1.ibeam1 <- x0 + x.right.vector * bracket.width/2
  y1.ibeam1 <- y0 + y.right.vector * bracket.width/2
  x1.ibeam2 <- x1 + x.right.vector * bracket.width/2
  y1.ibeam2 <- y1 + y.right.vector * bracket.width/2
  x1.ibeam1.opp <- x0 - x.right.vector * bracket.width/2
  y1.ibeam1.opp <- y0 - y.right.vector * bracket.width/2
  x1.ibeam2.opp <- x1 - x.right.vector * bracket.width/2
  y1.ibeam2.opp <- y1 - y.right.vector * bracket.width/2
  # curly
  x.mid.curly <- xmiddle - x.right.vector * bracket.width
  y.mid.curly <- ymiddle - y.right.vector * bracket.width
  x0.mid.segment <- xmiddle - skew * x.vector * bracket.width
  y0.mid.segment <- ymiddle - skew * y.vector * bracket.width
  x1.mid.segment <- xmiddle + skew * x.vector * bracket.width
  y1.mid.segment <- ymiddle + skew * y.vector * bracket.width

  if (tip=="curly") {
    x.text <- xmiddle - 2*x.right.vector * bracket.width
    y.text <- ymiddle - 2*y.right.vector * bracket.width
  } else {
    x.text <- xmiddle - x.right.vector * bracket.width
    y.text <- ymiddle - y.right.vector * bracket.width
  }


  if (tip=="bare") {
    grob_segment <- segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, default.units = "mm", gp = gp.segment, vp = vp)
  } else if (tip == "round") {
    theta <- 360 * atan(1/skew)/pi
    curvature <- arcCurvature(theta)
    angle <- 90
    shape <- 0.5
    ncp <- 20
    grob_segment <- gList(
      segmentsGrob(x0 = x0.inset, y0 = y0.inset, x1 = x1.inset, y1 = y1.inset, default.units = "mm", gp = gp.segment, vp = vp),
      curveGrob(x1 = x0.inset, y1 = y0.inset, x2 = x1.tick1, y2 = y1.tick1, default.units = "mm", curvature = curvature, angle = angle, shape = shape, ncp=ncp, square=FALSE, gp = gp.segment, vp = vp),
      curveGrob(x1 = x1.inset, y1 = y1.inset, x2 = x1.tick2, y2 = y1.tick2, default.units = "mm", curvature = -curvature, angle = 180 - angle, shape = shape, ncp = ncp, square=FALSE, gp = gp.segment, vp = vp)
    )
  } else if (tip == "curly") {
    theta <- 360 * atan(1/skew)/pi
    curvature <- arcCurvature(theta)
    angle <- 90
    shape <- 0.5
    ncp <- 20

    grob_segment <- gList(
      segmentsGrob(x0 = x0.inset, y0 = y0.inset, x1 = x0.mid.segment, y1 = y0.mid.segment, default.units = "mm", gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x1.inset, y0 = y1.inset, x1 = x1.mid.segment, y1 = y1.mid.segment, default.units = "mm", gp = gp.segment, vp = vp),
      curveGrob(x1 = x0.mid.segment, y1 = y0.mid.segment, x2 = x.mid.curly, y2 = y.mid.curly, default.units = "mm", curvature=curvature,angle=angle,shape=shape,ncp=ncp, square=FALSE, gp = gp.segment, vp = vp),
      curveGrob(x1 = x1.mid.segment, y1 = y1.mid.segment, x2 = x.mid.curly, y2 = y.mid.curly, default.units = "mm", curvature=-curvature,angle=180 - angle,shape=shape,ncp=ncp, gp = gp.segment, vp = vp, square=FALSE),
      curveGrob(x1 = x0.inset, y1 = y0.inset, x2 = x1.tick1, y2 = y1.tick1,  default.units = "mm", curvature = curvature, angle = angle, shape = shape, ncp=ncp, square=FALSE,gp = gp.segment, vp = vp),
      curveGrob(x1 = x1.inset, y1 = y1.inset, x2 = x1.tick2, y2 = y1.tick2, default.units = "mm", curvature = -curvature, angle = 180 - angle, shape = shape, ncp = ncp, square=FALSE,gp = gp.segment, vp = vp)
    )
  } else if (tip == "square") {
    grob_segment <- gList(
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, default.units = "mm", gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1.tick1, y1 = y1.tick1, default.units = "mm", gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x1, y0 = y1, x1 = x1.tick2, y1 = y1.tick2,  default.units = "mm", gp = gp.segment, vp = vp)
    )
  } else if (tip == "ibeam") {
    grob_segment <- gList(
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, default.units = "mm", gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x1.ibeam1, y0 = y1.ibeam1, x1 = x1.ibeam1.opp, y1 = y1.ibeam1.opp, default.units = "mm", gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x1.ibeam2, y0 = y1.ibeam2, x1 = x1.ibeam2.opp, y1 = y1.ibeam2.opp, default.units = "mm", gp = gp.segment, vp = vp)
    )
  } else if (tip == "iarrow") {
    # double.arrow <- arrow(angle = 20, length = unit(bracket.width.mm,"mm"), ends = "both", type = "closed")
    grob_segment <- gList(
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, default.units = "mm", arrow=arrow, gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x1.ibeam1, y0 = y1.ibeam1, x1 = x1.ibeam1.opp, y1 = y1.ibeam1.opp, default.units = "mm", gp = gp.segment, vp = vp),
      segmentsGrob(x0 = x1.ibeam2, y0 = y1.ibeam2, x1 = x1.ibeam2.opp, y1 = y1.ibeam2.opp, default.units = "mm", gp = gp.segment, vp = vp)
    )
  } else if (tip == "arrow") {
    # double.arrow <- arrow(angle = 20, length = unit(bracket.width.mm,"mm"), ends = "both", type = "closed")
    grob_segment <- gList(
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, default.units = "mm", arrow=arrow, gp = gp.segment, vp = vp)
    )
  } else {
    stop("YTError: tip not recognized")
  }

  ## handle text label
  grob_segment_text <- textGrob(label=label,x = x.text, y = y.text,
                                default.units = "mm",
                                hjust=hjust,vjust=vjust,
                                rot=text.angle,check.overlap=check.overlap,gp = gp.text, vp = vp)

  # put together
  setChildren(x, gList(grob_segment, grob_segment_text))
}



#' Annotation Ruler
#'
#' Add a ruler to the plot, for sizing, eyeballing purposes.
#' @param units Units to use. Default is `"cm"`. Can be values from [unit::grid()]
#' @param colour Color to use. Default is `"blue"`.
#' @param size Line size to use. Default is `0.5`.
#' @param linetype Line type to use. Default is `1`.
#' @param alpha Alpha to use. Default is `1`.
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' g <- ggplot(iris,aes(x=Petal.Length,y=Petal.Width,color=Species,label=Species)) +
#'   geom_point(size=5,alpha=0.5) +
#'   geom_text(color="black",check_overlap = TRUE)
#' g + annotation_ruler("cm")
#' g + annotation_ruler("inches")
#' g + annotation_ruler("mm")
annotation_ruler <- function(units="cm",
                             colour = "black",
                             size = 0.5,
                             linetype = 1,
                             alpha = 1) {
  layer(data = ggplot2:::dummy_data(),
        mapping = NULL,
        stat = StatIdentity,
        geom = GeomRuler,
        position = PositionIdentity,
        show.legend = FALSE,
        inherit.aes = FALSE,
        params = list(units = units,
                      colour = colour,
                      size = size,
                      linetype = linetype,
                      alpha = alpha))
}

#' @export
GeomRuler <- ggproto("GeomRuler", Geom,
                     default_aes = aes(colour = "black",
                                       size = 0.5,
                                       linetype = 1,
                                       alpha = 1),
                     handle_na = function(data, params) {
                       data
                     },
                     draw_panel = function(data, panel_params, coord, units) {
                       gTree(units=units,cl="ruler")
                     }
)

#' @export
makeContent.ruler <- function(x) {
  units <- x$units
  gp.line <- gpar(col = "blue")
  gp.text <- gpar(col = "blue", fontsize = 9)
  major.tick.size <- unit(2,"mm")
  minor.tick.size <- unit(1,"mm")
  major.interval.size <- unit(1,"cm")
  minor.interval.size <- unit(0.5,"cm")
  digits <- 15
  Q.major <- c(1,1)
  Q.minor <- c(1,5)

  # x line
  y.intercept <- 0.5
  xlim <- c(0,1)
  xlim.units <- convertX(unit(xlim,"npc"),unitTo=units,valueOnly = TRUE)
  x.ruler <- segmentsGrob(x0=0, y0=y.intercept, x1=1, y1=y.intercept, gp=gp.line)
  # x ticks
  m.major.tick.x <- floor(convertX(unit(1,"npc"),unitTo="mm",valueOnly = TRUE) /
                            convertX(major.interval.size,unitTo="mm",valueOnly = TRUE))
  x.major.breaks.units <- labeling::extended(dmin=xlim.units[1],dmax=xlim.units[2],
                                             m=m.major.tick.x,Q=Q.major) %>%
    signif(digits=digits) %>% {.[between(.,xlim.units[1],xlim.units[2])]}
  x.major.breaks <- convertX(unit(x.major.breaks.units,units=units),unitTo = "npc",valueOnly=TRUE)
  x.major.ticks <- segmentsGrob(x0=x.major.breaks, y0=y.intercept,
                                x1=x.major.breaks, y1=y.intercept+convertX(major.tick.size,"npc",valueOnly = TRUE),
                                gp=gp.line)
  m.minor.tick.x <- floor(convertX(unit(1,"npc"),unitTo="mm",valueOnly = TRUE) /
                            convertX(minor.interval.size,unitTo="mm",valueOnly = TRUE))
  x.minor.breaks.units <- labeling::extended(dmin=xlim.units[1],dmax=xlim.units[2],
                                             m=m.minor.tick.x,Q=Q.minor) %>%
    signif(digits=digits) %>% {.[between(.,xlim.units[1],xlim.units[2])]} %>%
    setdiff(x.major.breaks.units)
  x.minor.breaks <- convertX(unit(x.minor.breaks.units,units=units),unitTo = "npc",valueOnly=TRUE)
  x.minor.ticks <- segmentsGrob(x0=x.minor.breaks, y0=y.intercept,
                                x1=x.minor.breaks, y1=y.intercept+convertX(minor.tick.size,"npc",valueOnly = TRUE), gp=gp.line)
  # x numbers
  x.labels <- textGrob(label=x.major.breaks.units,
                       x=x.major.breaks,
                       y=y.intercept+convertX(major.tick.size,"npc",valueOnly = TRUE),gp=gp.text,vjust=0)
  x.unitlabel <- textGrob(label=units,x=0,y=y.intercept,gp=gp.text,hjust=-0.05,vjust=1)
  # y line
  x.intercept <- 0.5
  ylim <- c(0,1)
  ylim.units <- convertY(unit(ylim,"npc"),unitTo=units,valueOnly = TRUE)
  y.ruler <- segmentsGrob(x0=x.intercept, y0=0, x1=x.intercept, y1=1, gp=gp.line)
  # y ticks
  m.major.tick.y <- floor(convertY(unit(1,"npc"),unitTo="mm",valueOnly = TRUE) /
                            convertY(major.interval.size,unitTo="mm",valueOnly = TRUE))
  y.major.breaks.units <- labeling::extended(dmin=ylim.units[1],dmax=ylim.units[2],
                                             m=m.major.tick.y,Q=Q.major) %>%
    signif(digits=digits) %>% {.[between(.,ylim.units[1],ylim.units[2])]}
  y.major.breaks <- convertY(unit(y.major.breaks.units,units=units),unitTo = "npc",valueOnly=TRUE)
  y.major.ticks <- segmentsGrob(x0=x.intercept, y0=y.major.breaks,
                                x1=x.intercept+convertY(major.tick.size,"npc",valueOnly = TRUE),
                                y1=y.major.breaks, gp=gp.line)
  m.minor.tick.y <- floor(convertY(unit(1,"npc"),unitTo="mm",valueOnly = TRUE) /
                            convertY(minor.interval.size,unitTo="mm",valueOnly = TRUE))
  y.minor.breaks.units <- labeling::extended(dmin=ylim.units[1],dmax=ylim.units[2],
                                             m=m.minor.tick.y,Q=Q.minor) %>%
    signif(digits=digits) %>% {.[between(.,ylim.units[1],ylim.units[2])]} %>%
    setdiff(y.major.breaks.units)
  y.minor.breaks <- convertY(unit(y.minor.breaks.units,units=units),unitTo = "npc",valueOnly=TRUE)
  y.minor.ticks <- segmentsGrob(x0=x.intercept, y0=y.minor.breaks,
                                x1=x.intercept+convertY(minor.tick.size,"npc",valueOnly = TRUE),
                                y1=y.minor.breaks,
                                gp=gp.line)
  # y numbers
  y.labels <- textGrob(label=y.major.breaks.units,
                       x=x.intercept+convertY(major.tick.size,"npc",valueOnly = TRUE),
                       y=y.major.breaks,gp=gp.text,hjust=0)
  y.unitlabel <- textGrob(label=units,x=x.intercept,y=0,gp=gp.text,hjust=1.05,vjust=0)

  gTree(children = gList(x.ruler, x.major.ticks,
                         x.minor.ticks,
                         x.labels,x.unitlabel,
                         y.ruler, y.major.ticks,
                         y.minor.ticks,
                         y.labels,y.unitlabel))
}






#' List items within ggplot object
#'
#' A recursive function that lists all objects in a `ggplot` object.
#' @param ggobj ggplot object
#' @param prefix for internal use, to track super.
#' @param depth for internal use, to keep track of depth.
#'
#' @return a data frame listing of all objects.
#' @export
#'
#' @examples
list.gg.items <- function(ggobj,prefix=NULL,depth=1) {
  # ggobj=gg
  get.inheritance <- function(ggproto,name) {
    # ggproto=GeomCol;name="draw_panel"
    if (!is.ggproto(ggproto)) {
      return(NA_character_)
    }
    obj <- ggproto
    for (i in 1:length(class(ggproto))) {
      ggclass <- class(obj)[1]
      if (name %in% ls(envir=obj)) {
        fname <- paste0(ggclass,"$",name)

        # if (fname=="GeomBar2$setup_data"){
        #   browser()
        # }

        is.fn <- tryCatch({
          is.function(eval_tidy(parse_expr(fname)))
        }, error=function(e)  {
          FALSE
        })
        if (is.fn) {
          fmls <- tryCatch({
            eval_tidy(ggformals(!!parse_expr(fname))) %>% names() %>% paste(collapse=", ")
          },error=function(e){
            "..."
          })
          text <- str_glue("{fname}({fmls})")
        } else {
          text <- fname
        }
        return(text)
      } else {
        obj <- obj$super()
      }
    }
    warning("YTWarning: inheritance not found")
    return(NA_character_)
  }

  if (is.null(prefix)) {
    prefix <-  deparse1(substitute(ggobj))
  }
  if (is.ggplot(ggobj)) {
    ggobj <- as.list(ggobj)
  }
  if (is.ggproto(ggobj)) {
    ggiterable <- as.list(ggobj)
  } else {
    ggiterable <- ggobj
  }

  tbl <- imap_dfr(ggiterable,function(obj,name) {
    if (grepl("^[0-9]+$",name)) {
      label <- paste0("[[",name,"]]")
    } else {
      label <- paste0("$",name)
    }
    class <- paste(class(obj),collapse="|")
    inheritance <- get.inheritance(ggobj,name)
    t <- tibble(depth=depth,
                name=paste0(prefix,label),
                class=class,
                inheritance=inheritance)
    if (is.ggproto(obj) | (is.list(obj) & !is.data.frame(obj))) {
      t2 <- list.gg.items(obj,prefix=t$name,depth=depth+1)
      t <- bind_rows(t,t2)
    }
    return(t)
  })
  return(tbl)
}


# data cleanup functions -------------------------------------------------------------------



#' Reorder factor levels based on other variables.
#'
#' A `forcats`-like function which allows factor re-ordering based on sorting of multiple variables.
#'
#' This is similar to `data %>% arrange(...) %>% mutate(.f=fct_inorder(.f))`. This was a feature
#' request in `forcats` but for some reason it was never added. If it is added I'll remmove this.
#' @param .f the factor to be re-ordered.
#' @param ... variables to sort by
#'
#' @return the re-ordered factor
#' @export
#'
#' @examples
fct_reordern <- function(.f, ...) {
  f <- forcats:::check_factor(.f)
  fct_reorder(f,order(order(...)),.fun=first)
}



#' @export
trim <- function(x,...) UseMethod("trim")
#' @export
trim.default <- function(string) {
  gsub("(^ +)|( +$)", "",string)
}
#' @export
trim.data.frame <- function(data,verbose=TRUE) {
  strvars <- sapply(data,is.character)
  data[,strvars] <- sapply(data[,strvars],str_trim)
  if (verbose) {
    msg <- paste0("trim: looked through ",sum(strvars)," character variables to trim.")
    message(msg)
  }
  return(data)
}



#' Search and Convert Date variables
#'
#' In a given data frame, look for variables resembling dates and convert them to Dates.
#'
#' Basically applies [as.Date2()] to all variables.
#' @param data The data frame to be converted.
#' @param verbose logical indicating whether or not to display info on date conversions. Default is `FALSE`.
#' @export
convert.dates <- function(data,verbose=FALSE) {
  #data=xx
  newdata <- data
  for (var in names(newdata)) {
    newdata[[var]] <- as.Date2(newdata[[var]])
  }
  oldclass <- sapply(data,function(x) class(x)[1])
  newclass <- sapply(newdata,function(x) class(x)[1])
  changes <- data_frame(var=names(data),oldclass,newclass) %>% dplyr::filter(oldclass!=newclass)
  # cat("Looking for date variables to convert...  ")
  if (nrow(changes)==0) {
    msg <- "convert.dates: no date vars"
  } else {
    msg <- with(changes,paste0("convert.dates: dates converted: ",paste0(var,collapse=",")))
  }
  if (verbose) {
    message(msg)
  }
  return(newdata)
}




#' Determine if variable is a properly formatted MSKCC MRN
#'
#' MRN's must characters with 8 digits, where the first is either "0" or "3".
#'
#' Note that many numeric vectors _could_ be an MRN. Will issue a warning if the variable
#' passes the tests but is not necessarily an MRN variable.
#' @param mrn vector to be examined.
#' @param like logical, whether or not to check if the variable _could_ be an MRN. If `FALSE`, it checks strictly. If `TRUE`, will allow for classes other than character, and for whitespaces.
#' @return Returns logical stating whether or not this variable is an MRN.
#' @examples
#' @author Ying Taur
#' @export
is.mrn <- function(mrn,like=FALSE) {
  if (like) { #if numeric or factor, and if leading zeroes gone, can still hit.
    mrn <- as.character(str_trim(mrn))
    mrn <- mrn[!is.na(mrn) & mrn!=""]
    answer <- all.grepl("^[03][0-9]{7}$|^[0-9]{1,7}$",mrn)
    if (answer & is.numeric(mrn)) {
      if (mean(grepl("^3[0-9]{7}$",mrn))<0.80) {
        warning("YT: numeric variable may not really be an MRN.")
      }
    }
    return(answer)
  } else {
    mrn.pattern <- "^[03][0-9]{7}$"
    is.character(mrn) & all(grepl(mrn.pattern,mrn),na.rm=TRUE)
  }
}

#' Convert to MRN format
#'
#' Formats data containing MRNs
#'
#' Formats MRN data. When data is pulled in from other sources (e.g. Excel, Access),
#' the MRNs may be formatted differently depending on source. This unifies the formatting
#' of MRNs by converting to a 8-character vector. Adds 0's to the beginning if necessary,
#' to make every value 8 characters long. If a data frame is entered, the same data frame
#' is returned, but any column resembling an MRN will be formatted (if column name is not
#' already "MRN", an extra column will be added to the data frame).
#'
#' @param data Character or numeric vector or data frame to be analyzed.
#' @return An 8-character vector is returned. If data frame is entered, a data frame with MRNs converted will be returned.
#' @examples
#' as.mrn(1234)
#' as.mrn("1234")
#' @author Ying Taur
#' @export
as.mrn <- function(x,...) UseMethod("as.mrn")

#' @rdname as.mrn
#' @export
as.mrn.default <- function(mrn) {
  if (!is.mrn(mrn,like=TRUE)) {
    stop("YT: Error, data does not match MRN format!")
  }
  if (length(mrn)==0) {
    return(mrn)
  }
  mrn <- str_trim(as.character(mrn))
  mrn[mrn==""] <- NA
  mrn <- sapply(mrn,function(x) {
    if (is.na(x)) {
      NA_character_
    } else {
      str_pad(x, width=8, side="left", pad="0")
      # paste(c(rep("0",8-nchar(x)),x),collapse="")
    }
  })
  return(unname(mrn))
}



#' @rdname as.mrn
#' @export
as.mrn.data.frame <- function(data,verbose=FALSE) {
  #looks like mrn
  mrn.form <- grep("mrn",names(data),ignore.case=TRUE,value=TRUE)
  if (length(mrn.form)==0) {
    mrn.vars <- NULL
  } else {
    mrn.vars <- mrn.form[sapply(mrn.form,function(x) is.mrn(data[[x]],like=TRUE))]
  }
  if (length(mrn.vars)>0) {
    for (mv in mrn.vars) {
      data[[mv]] <- as.mrn(data[[mv]])
    }
    mrn.vars.summary <- mrn.vars
    if ("MRN" %!in% names(data)) {
      data <- dplyr::rename_(data,MRN=mrn.vars[1])
      mrn.vars.summary[1] <- paste0("MRN=",mrn.vars.summary[1])
    }
    msg <- paste0("as.mrn: ",paste(mrn.vars.summary,collapse=", "))
  } else {
    msg <- "as.mrn: none"
  }
  if (verbose) {
    message(msg)
  }
  return(data)
}



#' Remove NA columns
#'
#' @param data data frame to be filtered.
#' @param verbose logical indicating whether or not to display info on columns removed. Default is `FALSE`.
#' @return The original data frame, with blank columns removed.
#' @export
remove.na.cols <- function(data,verbose=FALSE) {
  keepcols <- sapply(data,function(col) !all(is.na(col)))
  if (verbose) {
    n.col.discard <- sum(!keepcols)
    if (n.col.discard==0) {
      msg <- "remove.na.cols: no blank columns found."
    } else {
      msg <- paste0("remove.na.cols: removing ",n.col.discard," blank columns.")
    }
    message(msg)
  }
  return(data[,keepcols])
}


#' Remove NA rows
#'
#' @param data data frame to be filtered.
#' @param verbose logical indicating whether or not to display info on rows removed. Default is `FALSE`.
#' @return The original data frame, with blank rows removed.
#' @export
remove.na.rows <- function(data,verbose=FALSE) {
  keeprows <- apply(data,1,function(row) !all(is.na(row)))
  if (verbose) {
    n.row.discard <- sum(!keeprows)
    if (n.row.discard==0) {
      msg <- "remove.na.rows: no blank rows found."
    } else {
      msg <- paste0("remove.na.rows: removing ",n.row.discard," blank rows")
    }
    message(msg)
  }
  return(data[keeprows,])
}


#' Make Syntactically Valid Names (Ying's version)
#'
#' Make syntactically valid names out of character vectors.
#'
#' Like original [base::make.names()], but gets rid of any repeating periods('.'), as well as periods at the end. This is just an aesthetic modification.
#' @param names character vector to be coerced to syntactically valid names. This is coerced to character if necessary.
#' @param verbose logical indicating whether or not to display info name cleanup. Default is `FALSE`. Data frame only
#' @return Data frame with corrected names
#' @export
make.names <- function(x,...) UseMethod("make.names")

#' @export
make.names.default <- base::make.names

#' @export
make.names.data.frame <- function(data,verbose=FALSE) {
  oldnames <- names(data)
  names(data) <- make.names(names(data),unique=TRUE)
  newnames <- names(data)
  if (verbose) {
    changes <- oldnames!=newnames

    if (any(changes)) {
      renamed.vars <- paste0(oldnames[changes],"->",newnames[changes],collapse=";")
      msg <- paste0("make.names: renamed variables: ",renamed.vars)
    } else {
      msg <- "make.names: none to correct"
    }
    message(msg)
  }
  return(data)
}


#' Fill in Blanks
#'
#' For a given vector,  in blanks with previous value.
#' @param vec the vector to be ed in.
#' @param blank vector of values that denote a blank. By default, `""` is used.
#' @param include.na vector of values that denote a blank. By default, `""` is used.
#' @return Returns `vec`, with blanks filled in.
#' @examples
#' fill_in_blanks(c("1",NA,"2","","3","","","4",NA,NA))
#' @author Ying Taur
#' @export
fill_in_blanks <- function(vec,blank="",include.na=TRUE) {
  if (include.na) {
    non.blanks <- !is.na(vec) & !(vec %in% blank)
  } else {
    non.blanks <- !(vec %in% blank)
  }
  c(vec[non.blanks][1], vec[non.blanks])[cumsum(non.blanks)+1]
}



#' Cleanup data
#'
#' Cleans up a data frame by performing 5 tasks:
#' 1. Remove any column or row that is all `NA` values ([remove.na.rows()],[remove.na.cols()])
#' 2. Make column names well-formatted ([make.names()])
#' 3. Remove any leading or trailing whitespace from character variables ([stringr::str_trim()])
#' 4. Look for variables that look like date/time variables, and convert them to Date or POSIXct format ([convert.dates()])
#' 5. Look for variables that look like MRNs and format them properly ([as.mrn()])
#'
#' @param remove.na.cols If `TRUE`, will remove any column consisting entirely of `NA`'s. Default=`FALSE`
#' @param remove.na.rows If `TRUE`, will remove any row consisting entirely of `NA`'s. Default=`TRUE`
#' @param make.names If `TRUE`, will fix variable names. Default=`TRUE`
#' @param trim If `TRUE`, will remove whitespace from all character variables. Default=`TRUE`
#' @param convert.dates If `TRUE`, will convert variables that look like dates to Date format. Default=`TRUE`
#' @param as.mrn If `TRUE`, will looking for variables that look like MRN and convert to 8-digit character. Default=`TRUE`
#' @param verbose logical indicating whether or not to display info on data cleaning. Default is `FALSE`.
#' @return Returns a clean version of `data`.
#' @examples
#' #####
#' @author Ying Taur
#' @export
cleanup.data <- function(data,remove.na.cols=FALSE,remove.na.rows=TRUE,make.names=TRUE,trim=TRUE,convert.dates=TRUE,as.mrn=TRUE,verbose=FALSE) {
  #data=d;make.names=TRUE;trim=TRUE;convert.dates=TRUE;as.mrn=TRUE;remove.na.cols.rows=FALSE
  #data=diet;make.names=TRUE;trim=TRUE;convert.dates=TRUE;as.mrn=TRUE;remove.na.cols.rows=FALSE
  if (remove.na.cols) {
    data <- remove.na.cols(data,verbose=verbose)
  }
  if (remove.na.rows) {
    data <- remove.na.rows(data,verbose=verbose)
  }
  if (make.names) {
    data <- make.names(data,verbose=verbose)
    #names(data) <- make.names(names(data),unique=TRUE)
    # setnames(data,names(data),make.names(names(data),unique=TRUE))
  }
  if (trim) {
    data <- trim(data,verbose=verbose) #gets rid of whitespace
  }
  if (convert.dates) {
    data <- convert.dates(data,verbose=verbose)
  }
  if (as.mrn) {
    data <- as.mrn(data,verbose=verbose)
  }
  return(data)
}



# ggplot transformations ---------------------------------------------------------



#' Log Epsilon Tranformation
#'
#' Use this transformation for plotting log data including 0. You can't use regular log transformation because it can't take zero.
#'
#' The transformation used is \eqn{\log{(|x|+\frac{epsilon}{8})} - \log(\frac{epsilon}{8})}, where epsilon is the parameter controlling the scale. The 1/8 portion is to make distances between ticks equal, so it's visually pleasing.
#' @param epsilon This parameter controls scaling. Think of this as the value of the first axis tick after zero. Default is 0.001.
#' @return Tranformation function to be plugged into ggplot.
#' @examples
#' values <- c(0,10^(-10:0))
#' d <- data.frame(x=1:length(values),y=values)
#' g <- ggplot(d,aes(x=x,y=y,label=y)) + geom_point() + geom_line() + geom_text()
#' g1 <- g + scale_y_continuous(breaks=values) + ggtitle("untransformed")
#' g2 <- g + scale_y_continuous(trans=log_epsilon_trans(0.0001)) + ggtitle("scale_trans, epsilon=0.0001")
#' g3 <- g + scale_y_continuous(trans=log_epsilon_trans(10^-6.)) + ggtitle("scale_trans, epsilon=0.0000001")
#' g4 <- g + scale_y_continuous(trans=log_epsilon_trans(10^-10)) + ggtitle("scale_trans, epsilon=0.0000000001")
#' gridExtra::grid.arrange(g1,g2,g3,g4,nrow=2)
#' @author Ying Taur
#' @export
log_epsilon_trans <- function(epsilon=0.001) {
  requireNamespace("scales",quietly=TRUE)
  trans <- function(x) {
    # if (is.null(epsilon)) {return(x)}
    sign(x)*(log(abs(x)+epsilon/8)-log(epsilon/8))
  }
  inv <- function(y) {
    # if (is.null(epsilon)) {return(y)}
    sign(y)*epsilon/8*(exp(abs(y))-1)
  }
  scales::trans_new(paste0("log_epsilon-",format(epsilon)),
                    transform = trans,
                    inverse = inv,
                    breaks=log_epsilon_trans_breaks(epsilon),
                    format=pretty_number,
                    domain=c(-Inf,Inf))
}

#' Breaks for Log Epsilon Tranformation
#'
#' This is used by scant_trans as default method for breaks. Will fill in logs of 10.
#' @param epsilon scaling parameter used in [log_epsilon_trans()]
#' @return break function returning break values.
#' @export
log_epsilon_trans_breaks <- function(epsilon) {
  # if (is.null(epsilon)) {return(scales::extended_breaks())}
  function(x) {
    firsttick <- round(log(epsilon,10))
    lasttick <- floor(log(x[2],10))
    x <- c(0,10^(firsttick:lasttick))
    by <- ceiling(length(x) / 5)
    x[seq(1,length(x),by=by)]
  }
}



#' Logistic Transformation
#'
#' Performs logistic transformation of data. This is useful for graphing.
#'
#' When graphing a continuous measure, this transformation is useful if you need to fit all values
#' into a particular space. You can play with parameters to get the transformation just how you want it.
#' @param value1 First value whose percent height you'd like to specify.
#' @param value2 Second value whose percent height you'd like to specify.
#' @param pct.value1 Percent height of the first value. Default is 0.1.
#' @param pct.value2 Percent height of the second value. Default is 0.9.
#' @param invert whether or not to flip the logistic curve. Default is `FALSE`.
#' @return Returns the logistic transformation of `var`, where values will fall within `scale`, and where `inner.range` will be transformed to `percentiles`.
#' @examples
#' #Example: WBC. Values between 0.2 and 10 take up 80% of the space. Values outside of that de-emphasized.
#' wbc <- seq(0,20,by=0.1)
#' wbc.logist <- trans.logistic(wbc,inner.range=c(0.2,10))
#' ggplot(data.frame(wbc,wbc.logist)) + geom_point(aes(x=wbc,y=wbc.logist))
#' data <- tibble(wbc=seq(0,20,by=0.01))
#' ggplot(data,aes(x=wbc,y=wbc)) + geom_point() + expand_limits(y=0) +
#'   scale_y_continuous("log-transformed wbc",trans=logistic_trans(.1,5),
#'                      breaks=c(0,0.5,1,5,20))
#' @author Ying Taur
#' @export
logistic_trans <- function(value1,value2,pct.value1=0.1,pct.value2=0.9) {
  inner.range <- c(value1,value2)
  percentiles <- c(pct.value1,pct.value2)
  a <- (inner.range[1]*log(1/percentiles[2]-1)-inner.range[2]*log(1/percentiles[1]-1))/(inner.range[1]-inner.range[2])
  b <- (log(1/percentiles[1]-1)-log(1/percentiles[2]-1))/(inner.range[1]-inner.range[2])
  trans <- function(x) {
    1/(1+exp(a+b*x))
  }
  inv <- function(y) {
    y[y<=0] <- .Machine$double.eps
    y[y>=1] <- 1-.Machine$double.eps
    (log(1/y-1)-a) / b
  }
  trans_new("logistic",trans,inv,breaks=logistic_trans_breaks(inner.range))
}

#' Breaks for Logistic Tranformation
#'
#' Simple breaks for logistic, just use the inner.range used to define the curve.
#' @param inner.range the 2-value numeric used in logistic_trans
#' @return break function returning break values.
#' @export
logistic_trans_breaks <- function(inner.range) {
  function(x) {
    inner.range
  }
}





# regression functions ----------------------------------------------------






#' Chop survival endpoint
#'
#' For a given survival endpoint, censor at earlier timepoints, if they occur.
#' @param data the data frame with survival data
#' @param newvar the name (unquoted) of the new survival endpoint to be created (creates `newvar`, plus `paste0(newvar,"_day")`
#' @param oldvar the original survival endpoint, to be censored.
#' @param ... columns representing censoring times.
#' @param censor.as.tdvar whether to censor endpoints occurring exactly at the censoring time. Use `TRUE` for time-dependent predictors, `FALSE` for endpoints.
#' @return Returns `data`, with a newly defined survival endpoint (`newvar`), which has been censored wherever the censoring times occur before the original end of survival time.
#' @examples
#' # create a endpoint(dead30d), which represents death within 30 days or discharge.
#' new.pt <- cid.patients %>% chop.endpoint(dead30d,dead,30,discharge.day)
#' new.pt %>% select(dead30d, dead30d_day,dead, dead_day, discharge.day) %>% head(10)
#' @author Ying Taur
#' @export
chop.endpoint <- function(data,newvar,oldvar,...,censor.as.tdvar=FALSE) {
  newvar <- ensym(newvar)
  oldvar <- enquo(oldvar)
  oldvar_day <- paste0(as_name(oldvar),"_day")
  oldvar_day <- sym(oldvar_day)
  newvar <- as_name(newvar)
  newvar_day <- paste0(as_name(newvar),"_day")
  vars <- enquos(...)
  ov <- pull(data,!!oldvar)
  if (!is.logical(ov) & !all(ov %in% 0:1,na.rm=TRUE)) {stop("YTError: oldvar should be a logical or 0-1!")}
  if (!has_name(data,as_name(oldvar_day))) {stop("YTError: ",as_name(oldvar_day)," does not exist!")}
  ovd <- pull(data,!!oldvar_day)
  if (!is.numeric(ovd)) {stop("YTError: oldvar should be a logical or 0-1!")}

  if (censor.as.tdvar) {
    # for tdvars
    data2 <- data %>%
      mutate(chop_=pmin(!!!vars),
             !!newvar:=ifelse(chop_<=!!oldvar_day,FALSE,!!oldvar),
             !!newvar_day:=ifelse(chop_<=!!oldvar_day,chop_,!!oldvar_day)) %>%
      select(-chop_)
  } else {
    # for endpoints.
    data2 <- data %>%
      mutate(chop_=pmin(!!!vars),
             !!newvar:=ifelse(chop_<!!oldvar_day,FALSE,!!oldvar),
             !!newvar_day:=ifelse(chop_<!!oldvar_day,chop_,!!oldvar_day)) %>%
      select(-chop_)
  }
  data2
}


#' Make a survival endpoint
#'
#' Construct a survival endpoint, by specifying times or survival other survival endpoints.
#' These can be regular survival or competing endpoints (which you would analyze with something like Fine-Grey in `cmprsk` package).
#'
#' Note, endpoints (primary and competing) can be specified either as a "varname" and "varname_day" pair, representing survival indicator and survival time,
#' or a single column representing positive endpoints (`NA` or `Inf`) otherwise.
#'
#' If survival endpoints are specified, note that censored times may be ignored.
#'
#' @param data the data to be modified, containing the endpoints to be combined
#' @param newvar the name (unquoted) of the new competing survival endpoint to be created (creates `newvar`, plus `paste0(newvar,"_day")`)
#' @param primary the original survival endpoint, to be converted to a competing endpoint
#' @param ... columns representing competing endpoints.
#' @param censor variable representing censoring times. Default is to use censoring times from the primary... or time=`Inf`, if it doesn't exist.
#' @param competing whether to code as competing. If FALSE, competing endpoints will be censored.
#'
#' @return Returns `data`, with a newly defined survival endpoint (`newvar`), which represents the combined competing endpoint.
#' `newvar` is the numeric indicator of the endpoint,
#' `newvar_day` is the survival time,
#' `newvar_code` is a character showing the value definition,
#' `newvar_info` shows the status of all endpoints, in order.
#' You would primarily use `newvar` and `newvar_day` with packages such as `cmprsk` for competing risk analysis.
#' @examples
#' # create a combined endpoint
#' cid.patients %>% make.surv.endpt(competing.enterodom,enterodom30,dead,strepdom30,proteodom30,30)
#' @author Ying Taur
#' @export
make.surv.endpt <- function(data, newvar, primary, ... , censor=NULL,competing=FALSE) {
  newvar <- ensym(newvar)
  newvar_day <- paste0(as_name(newvar),"_day")
  newvar_code <- paste0(as_name(newvar),"_code")
  newvar_info <- paste0(as_name(newvar),"_info")
  primary <- enquo(primary)
  censor <- enquo(censor)
  competing.vars <- enquos(...)

  vartype <- function(data,var) {
    var <- enquo(var)
    varday <- paste0(as_label(var),"_day")
    x <- data %>% mutate(.x=!!var) %>% pull(.x)
    has.day <- has_name(data,varday) && is.numeric(pull(data,!!sym(varday)))
    looks.logical <- is.logical(x) || all(x %in% c(0,1,NA))
    looks.competing <- is.wholenumber(x) && all(x>=0,na.rm=TRUE)
    if (looks.logical & has.day) {
      return("survival")
    } else if (looks.competing & has.day) {
      return("competing")
    } else if (is.numeric(x) & !has.day) {
      return("numeric")
    } else {
      stop("YTError")
    }
  }

  get.surv <- function(var) {
    var <- enquo(var)
    varname <- as_label(var)
    print(varname)
    varday <- paste0(as_label(var),"_day")
    if (quo_is_null(var)) { #censor
      if (vartype(data,!!primary) %in% c("survival","competing")) {
        primary_day <- paste0(as_name(primary),"_day")
        data <- data %>% mutate(.v=1,.vd=!!sym(primary_day))
      } else {
        data <- data %>% mutate(.v=1,.vd=Inf)
      }
    } else if (vartype(data,!!var)=="survival") {
      # var+var_day: return the endpoint
      data <- data %>% mutate(.v=as.numeric(!!var),.vd=!!sym(varday))
    } else if (vartype(data,!!var)=="numeric") {
      # var only: assume these are times, create endpoint (NAs are censored at Inf)
      data <- data %>% mutate(.v=as.numeric(!is.na(!!var)),.vd=ifelse(.v==1,!!var,Inf))
    } else if (vartype(data,!!var)=="competing") {
      stop("YTError: competing endpoint: ",varname)
    } else {
      stop("YTError: unknown type: ",varname)
    }
    data2 <- data %>%
      mutate(.var=varname,
             .row=seq_along(.v)) %>%
      select(.row,.v,.vd,.var)
    return(data2)
  }

  varlist <- c(primary,competing.vars,censor)
  varnames <- sapply(varlist,as_label)
  varnumbers <- c(seq_along(varnames)[-length(varnames)],0)
  if (!competing) {
    varnumbers <-ifelse(varnumbers>1,0,varnumbers)
  }
  varrecodes <- setNames(varnumbers,varnames)
  survlist <- varlist %>%
    lapply(function(var) {
      get.surv(!!var)
    })
  endpts <- survlist %>% bind_rows() %>%
    mutate(.var=factor(.var,levels=varnames),
           .varnum=varrecodes[as.character(.var)],
           .var_label=as.character(.var),
           .var_label=ifelse(.var_label=="NULL","<censor>",.var_label),
           .info=paste0(.var_label,"[t=",.vd,"]"),
           .is.na=is.na(.v)|is.na(.vd),
           .vd=ifelse(.is.na,NA_real_,.vd), #NAs in any column will be carried forward
           .varnum=ifelse(.is.na,NA_integer_,.varnum))

  final <- endpts %>%
    group_by(.row) %>%
    arrange(!.is.na,desc(.v),.vd,.var) %>% #sort by NA, then =1 values, then time, then var order.
    summarize(.final_v=first(.varnum),
              .final_vd=first(.vd),
              .final_code=first(.var_label),
              .final_info=paste(.info[.v==1],collapse=", "),
              .groups='drop') %>%
    arrange(.row)

  newdata <- data %>%
    mutate(!!newvar:=final$.final_v,
           !!newvar_day:=final$.final_vd,
           !!newvar_code:=final$.final_code,
           !!newvar_info:=final$.final_info)
  message(vartype(newdata,!!newvar)," endpoint variable created: ",as_name(newvar))
  na.count <- newdata %>% filter(is.na(!!newvar)|is.na(!!newvar_day)) %>% nrow()
  if (na.count>0) {
    message("note: ",na.count," NA values")
  }
  return(newdata)
}




#' Cox Proportional Hazard model
#'
#' Run a Cox (or Fine-Gray) regression.
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the time-to-event outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted). If a predictor time-dependent, the split the corresonding rows of the data frame.
#' @param starttime optional parameter specifying analysis start time. Use this for left censoring.... no need to use it for setting time zero.
#' @param return.split.data if `TRUE`, returns the data frame after splitting rows that are time-dependent.
#' @param return.model.obj if `TRUE`, returns the model object of the [survival::coxph()] command
#' @param formatted returns a formatted regression table (default `TRUE`). Otherwise, return the raw, unformatted regression table (essentially, the output of `broom::tidy`, plus a few additional columns)
#'
#' @return by default, returns a formatted regression table
#' @examples
#' cid.patients %>% cox(vre.bsi,enterodom30,starttime=firstsampday)
#' @export
cox <- function(data, yvar, ... , starttime=NULL,return.split.data=FALSE,return.model.obj=FALSE,firth=FALSE,
                do.competing=NULL,
                firth.opts=list(),formatted=TRUE) {
  requireNamespace(c("coxphf","scales","cmprsk"),quietly=TRUE)
  yvar <- enquo(yvar)
  starttime <- enquo(starttime)
  xvars <- enquos(...)
  yvarday <- as_name(yvar) %>% paste0("_day") %>% sym()
  is.td <- function(var) {
    var <- enquo(var)
    vardayname <- as_name(var) %>% paste0("_day")
    has_name(data,vardayname)
  }
  xvars.td <- xvars[sapply(xvars,is.td)]
  xvarnames <- xvars %>% sapply(as_name)
  if (length(xvars.td)>0) {
    xvarsdays.td <- xvars.td %>% sapply(as_name) %>% paste0("_day") %>% syms()
  } else {
    xvarsdays.td <- syms(NULL)
  }
  timevars <- c(yvarday,xvarsdays.td)
  data <- data %>% mutate(across(c(!!yvar,!!!xvars.td),as.numeric))
  ## shift time0 if needed, and deal with starttime.
  ## shifting time0 is because some methods can't deal with negative times.
  ## starttime is for left censoring.
  ## note that td vars can be negative times that occur well before time0.
  min.time <- data %>% select(!!yvarday,!!starttime) %>% min(na.rm=TRUE)
  if (min.time>0) {
    time0 <- 0
  } else {
    time0 <- min.time-1
    message(str_glue("Negative times detected. Setting time zero as: {time0}"))
  }

  if (quo_is_null(starttime)) {
    data <- data %>% mutate(.y=!!yvar,.tstart=time0,.tstop=!!yvarday)
  } else {
    message(str_glue("starttime specified as {as_label(starttime)}."))
    data <- data %>% mutate(.y=!!yvar,.tstart=!!starttime,.tstop=!!yvarday)
  }
  #if time0 is not zero, this will shift everything.
  data <- data %>% mutate(across(c(.tstart,.tstop,!!!timevars),~.x-time0))

  n.left.censored <- sum(data$.tstart!=min(data$.tstart,na.rm=TRUE),na.rm=TRUE)
  if (n.left.censored>0) {
    message(str_glue("{n.left.censored} observations are left censored."))
  }

  splitline <- function(data,xvar) {
    xvar <- enquo(xvar)
    xvarday <- as_name(xvar) %>% paste0("_day") %>% sym()
    data.nochange <- data %>% filter(!!xvar==0|is.na(!!xvar))
    data.split <- data %>% filter(!!xvar==1,.tstart<!!xvarday,!!xvarday<.tstop)
    data.xafter <- data %>% filter(!!xvar==1,.tstop<=!!xvarday)
    data.xbefore <- data %>% filter(!!xvar==1,!!xvarday<=.tstart)
    data.nochange.new <- data.nochange
    data.xbefore.new <- data.xbefore
    data.xafter.new <- data.xafter %>% mutate(!!xvar:=0)
    data.split.new1 <- data.split %>% mutate(.tstop=!!xvarday,!!xvar:=0,.y=0)
    data.split.new2 <- data.split %>% mutate(.tstart=!!xvarday,!!xvar:=1)
    newdata <- bind_rows(data.nochange.new,data.xbefore.new,data.xafter.new,data.split.new1,data.split.new2) %>%
      select(-!!xvarday)
    return(newdata)
  }
  data2 <- data
  for (xvar in xvars.td) {
    data2 <- data2 %>% splitline(!!xvar)
  }
  has.timevarying <- length(xvars.td)>0 & nrow(data2)>nrow(data)
  if (has.timevarying) {
    message(str_glue("Time-varying X's detected: {paste(xvarnames,collapse=\",\")}. Transforming data from {nrow(data)} to {nrow(data2)} rows."))
  }
  if (return.split.data) {
    fn <- ifelse(firth,"coxphf","coxph")
    form <- deparse(formula)
    message(str_glue("Returning split data. Can run as follows:\n{fn}({deparse(form)},data={{data}})"))
    return(data2)
  }
  is.competing <- !all(pull(data,!!yvar) %in% c(0,1,NA))
  if (is.null(do.competing)) {
    if (is.competing) {
      message("Competing endpoint detected. Performing competing risk regression.")
    }
    do.competing <- is.competing
  }
  if (!do.competing) {
    ###### do cox regression (with or without firth)
    leftside <- "survival::Surv(.tstart,.tstop,.y)"
    rightside <- xvarnames %>% paste(collapse=" + ")
    model <- paste(leftside,rightside,sep=" ~ ")
    formula <- as.formula(model)
    if (!firth) {
      # message(str_glue("Running coxph: {model}"))
      message(str_glue("Running coxph: {as_label(yvar)} ~ {rightside}"))
      result <- survival::coxph(formula,data=data2)
    } else {
      # message(str_glue("Running coxphf: {model}"))
      message(str_glue("Running coxphf: {as_label(yvar)} ~ {rightside}"))
      result <- do.call(coxphf::coxphf,c(list(formula,data=data2),firth.opts))
    }
  } else {
    ###### do fine gray
    if (!all(data2$.tstart==0)) {
      #this may never happen because of the time0 shifting code above.
      stop("YTError: .tstart needs to be zero for competing risk regression!")
    }
    if (firth) {
      stop("YTError: can't perform Firth correction for competing risk regression!")
    }
    if (has.timevarying) {
      stop("YTError: I don't think we can do time-varying predictors for competing risk regression!")
    }
    rightside <- xvarnames %>% paste(collapse=" + ")
    mm <- model.matrix(as.formula(paste0("~",rightside)),data=data2)
    cols <- colnames(mm) %>% setdiff("(Intercept)")
    cov1 <- mm[,cols,drop=FALSE]
    ftime <- data2$.tstop
    fstatus <- data2$.y
    message(str_glue("Running crr: {as_label(yvar)} ~ {rightside}"))
    result <- cmprsk::crr(ftime=ftime,fstatus=fstatus,cov1=cov1)
  }

  if (return.model.obj) {
    return(result)
  }

  terms.to.varnames <- function(terms,vars,data) {
    dict <- lapply(xvarnames,function(var) {
      mm <- model.matrix(as.formula(paste0("~",var)),data=data)
      term <- colnames(mm) %>% setdiff("(Intercept)")
      rep(var,length(term)) %>% setNames(term)
    }) %>% do.call(c,.)
    if (anyDuplicated(names(dict))) {
      stop("YTError: duplicate terms found during terms.to.varnames function!")
    }
    if (!all(terms %in% names(dict))) {
      stop("YTError: varnames and terms don't match in the terms.to.varnames function!")
    }
    dict[match(terms,names(dict))]
  }

  tbl <- yt.tidy(result) %>%
    mutate(xvar=terms.to.varnames(term,xvarnames,data2),
           yvar=as_label(yvar),
           time.dependent=xvar %in% sapply(xvars.td,as_label))

  tbl.extra <- lapply(xvars,function(x) {
    #time dependent
    if (as_name(x) %in% sapply(xvars.td,as_name)) {
      xday <- x %>% as_name() %>% paste0("_day") %>% sym()
      count <- data %>% summarize(count=sum((!!x==1) & (!!xday < !!yvarday),na.rm=TRUE)) %>% pull(count)
      extra <- tibble(xvar=as_label(x),n=count) %>% mutate(term=xvar)
      return(extra)
    }
    vec <- data %>% pull(!!x)
    is.01 <- function(v) {is.numeric(v) & all(v %in% c(0,1),na.rm=TRUE)}
    if (is.01(vec)) {
      extra <- tibble(xvar=as_name(x),n=sum(vec,na.rm=TRUE)) %>% mutate(term=xvar)
      return(extra)
    } else if (is.numeric(vec)) {
      extra <- tibble(xvar=as_name(x),n=NA_real_) %>% mutate(term=xvar)
      return(extra)
    } else {
      tbl <- table(vec)
      extra <- tibble(xvar=as_name(x),n=as.vector(tbl)) %>% mutate(term=paste0(xvar,names(tbl)))
      return(extra)
    }
  }) %>% bind_rows()
  tbl <- tbl %>% left_join(tbl.extra,by=c("xvar","term"))

  if (formatted) {
    tbl <- tbl %>%
      mutate(xvar=ifelse(time.dependent,paste0(xvar,"(td)"),xvar),
             p.value=scales::pvalue(p.value)) %>%
      mutate_at(vars(estimate,conf.low,conf.high),~formatC(.,format="f",digits=2)) %>%
      transmute(yvar,xvar,term,n,haz.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value)
  }
  return(tbl)
}


#' @export
yt.tidy <- function(x,...) UseMethod("yt.tidy")
#' @export
yt.tidy.coxph <- function(obj) {
  requireNamespace("broom",quietly=TRUE)
  obj %>% broom::tidy(exponentiate=TRUE,conf.int=TRUE,conf.level=0.95)
}
#' @export
yt.tidy.coxphf <- function(obj) {
  tibble(term=names(obj$coefficients),
         estimate=exp(obj$coefficients),
         statistic=NA,
         std.error=NA,
         p.value=obj$prob,
         conf.low=obj$ci.lower,
         conf.high=obj$ci.upper)
}
#' @export
yt.tidy.crr <- function(obj) {
  requireNamespace("broom",quietly=TRUE)
  obj %>% broom::tidy(exponentiate=TRUE,conf.int=TRUE,conf.level=0.95)
}
#' @export
yt.tidy.glm <- function(obj) {
  requireNamespace("broom",quietly=TRUE)
  obj %>% broom::tidy(exponentiate=TRUE,conf.int=TRUE,conf.level=0.95) %>%
    filter(term!="(Intercept)")
}
#' @export
yt.tidy.logistf <- function(obj) {
  tibble(term=obj$terms,
         estimate=exp(obj$coefficients),
         statistic=NA,
         std.error=NA,
         p.value=obj$prob,
         conf.low=exp(obj$ci.lower),
         conf.high=exp(obj$ci.upper)) %>%
    filter(term!="(Intercept)")
}


#' Univariate and Multivariate Cox Regression
#'
#' Uses the [cox()] function to perform a univariate and multivariate model analysis.
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the time-to-event outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted). If a predictor time-dependent, the split the corresonding rows of the data frame.
#' @param starttime optional parameter specifying analysis start time.
#' @param multi if `TRUE`, perform multivariate analysis.
#' @param multi.cutoff P-value threshold for inclusion into the multivariate model (default is `0.25`)
#' @param formatted returns a formatted regression table (default `TRUE`). Otherwise, return the raw, unformatted regression table (essentially, the output of [broom::tidy()], plus a few additional columns)
#' @return by default, returns a formatted regression table
#' @examples
#' @export
univariate.cox <- function(data, yvar, ..., starttime=NULL,multi=TRUE,multi.cutoff=0.25,firth=FALSE,formatted=TRUE) {
  yvar <- enquo(yvar)
  starttime <- enquo(starttime)
  xvars <- quos(...)
  univariate.reglist <- lapply(xvars,function(x) {
    message(as_label(x))
    cox(!!yvar,!!x,starttime=!!starttime,data=data,firth=firth,formatted=FALSE)
  })
  univariate.tbl <- univariate.reglist %>% bind_rows()
  if (multi) {
    multi.xvars <- univariate.tbl %>% filter(p.value<=multi.cutoff) %>% pull(xvar) %>% unique()
    if (length(multi.xvars)==0) {
      message("No predictors entered multivariate model")
      multivariate.tbl <- univariate.tbl %>% mutate_at(vars(-yvar,-xvar,-term,-time.dependent),~na_if(.,.)) %>% rename_at(vars(-yvar,-xvar,-term,-time.dependent),~paste0("multi.",.))
    } else {
      multivariate.tbl <- cox(!!yvar,!!!syms(multi.xvars),data=data,firth=firth,formatted=FALSE) %>% rename_at(vars(-yvar,-xvar,-term,-time.dependent),~paste0("multi.",.))
    }
    tbl <- univariate.tbl %>% left_join(multivariate.tbl,by=c("yvar","xvar","term","time.dependent"))
    if (formatted) {
      tbl <- tbl %>%
        mutate(xvar=ifelse(time.dependent,paste0(xvar,"(td)"),xvar),
               p.value=pvalue(p.value),
               multi.p.value=pvalue(multi.p.value)) %>%
        mutate_at(vars(estimate,conf.low,conf.high,multi.estimate,multi.conf.low,multi.conf.high),~formatC(.,format="f",digits=2)) %>%
        transmute(yvar,xvar,term,n,
                  haz.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value,
                  multi.haz.ratio=paste0(multi.estimate," (",multi.conf.low," - ",multi.conf.high,")"),multi.p.value)
    }
  } else {
    tbl <- univariate.tbl
    if (formatted) {
      tbl <- tbl %>%
        mutate(xvar=ifelse(time.dependent,paste0(xvar,"(td)"),xvar),
               p.value=pvalue(p.value)) %>%
        mutate_at(vars(estimate,conf.low,conf.high),~formatC(.,format="f",digits=2)) %>%
                    transmute(yvar,xvar,term,n,
                              haz.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value)
    }
  }
  tbl
}







#' Logistic regression
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted).
#' @param starttime optional parameter specifying analysis start time.
#' @param return.model.obj if `TRUE`, returns the model object of the [survival::coxph()] command
#' @param formatted returns a formatted regression table (default `TRUE`). Otherwise, return the raw, unformatted regression table (essentially, the output of [broom::tidy()], plus a few additional columns)
#' @return by default, returns a formatted regression table
#' @export
logit <- function(data, yvar, ... , return.model.obj=FALSE,firth=FALSE,formatted=TRUE) {
  requireNamespace(c("broom","logistf","scales"),quietly=TRUE)
  yvar <- enquo(yvar)
  xvars <- enquos(...)
  xvarnames <- sapply(xvars,as_name)
  formula <- as.formula(paste0(as_name(yvar),"~",paste(xvarnames,collapse="+")))
  if (!firth) {
    result <- glm(formula,data=data,family="binomial")
  } else {
    result <- logistf::logistf(formula,data=data,firth=TRUE)
  }
  if (return.model.obj) {
    return(result)
  }
  terms.to.varnames <- function(terms,vars,data) {
    dict <- lapply(xvarnames,function(var) {
      mm <- model.matrix(as.formula(paste0("~",var)),data=data)
      term <- colnames(mm) %>% setdiff("(Intercept)")
      rep(var,length(term)) %>% setNames(term)
    }) %>% do.call(c,.)
    if (anyDuplicated(names(dict))) {
      stop("YTError: duplicate terms found during terms.to.varnames function!")
    }
    if (!all(terms %in% names(dict))) {
      stop("YTError: not all terms found in varnames!")
    }
    dict[match(terms,names(dict))]
  }
  tbl <- yt.tidy(result)
  tbl <- tbl %>%
    mutate(xvar=terms.to.varnames(term,xvarnames,data),
           yvar=as_label(yvar)) %>%
      select(yvar,xvar,term,everything())

  #create 'n' column for categorical variables (factor, character, logical, 0-1)
  tbl.extra <- lapply(xvars,function(x) {
    vec <- data %>% pull(!!x)
    is.01 <- function(v) {is.numeric(v) & all(v %in% c(0,1),na.rm=TRUE)}
    if (is.01(vec)) {
      extra <- tibble(xvar=as_label(x),n=sum(vec)) %>% mutate(term=xvar)
      return(extra)
    } else if (is.numeric(vec)) {
      extra <- tibble(xvar=as_label(x),n=NA_real_,term=xvar)
      return(extra)
    } else {
      tbl <- table(vec)
      extra <- tibble(xvar=as_label(x),n=as.vector(tbl)) %>% mutate(term=paste0(xvar,names(tbl)))
      return(extra)
    }
  }) %>% bind_rows()

  tbl <- tbl %>% left_join(tbl.extra,by=c("xvar","term"))
  if (formatted) {
    tbl <- tbl %>%
      mutate(p.value=scales::pvalue(p.value)) %>%
      mutate_at(vars(estimate,conf.low,conf.high),~formatC(.,format="f",digits=2)) %>%
      transmute(yvar,xvar,term,n,odds.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value)
  }
  tbl
}




#' Univariate and Multivariate Cox Regression
#'
#' Uses the `cox` function to perform a univariate and multivariate model analysis.
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the time-to-event outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted). If a predictor time-dependent, the split the corresonding rows of the data frame.
#' @param starttime optional parameter specifying analysis start time.
#' @param multi if `TRUE`, perform multivariate analysis.
#' @param multi.cutoff P-value threshold for inclusion into the multivariate model (default is `0.25`)
#' @param formatted returns a formatted regression table (default `TRUE`). Otherwise, return the raw, unformatted regression table (essentially, the output of [broom::tidy()], plus a few additional columns)
#' @return by default, returns a formatted regression table
#' @examples
#' @export
univariate.logit <- function(data, yvar, ..., multi=TRUE,multi.cutoff=0.25,firth=FALSE,formatted=TRUE) {
  yvar <- enquo(yvar)
  xvars <- enquos(...)
  univariate.reglist <- lapply(xvars,function(x) {
    message(as_name(x))
    logit(!!yvar,!!x,data=data,formatted=FALSE,firth=firth)
  })
  univariate.tbl <- univariate.reglist %>% bind_rows()
  if (multi) {
    multi.xvars <- univariate.tbl %>% filter(p.value<=multi.cutoff) %>% pull(xvar) %>% unique()
    if (length(multi.xvars)==0) {
      message("No predictors entered multivariate model")
      multivariate.tbl <- univariate.tbl %>% mutate_at(vars(-yvar,-xvar,-term),~na_if(.,.)) %>% rename_at(vars(-yvar,-xvar,-term),~paste0("multi.",.))
    } else {
      multivariate.tbl <- logit(!!yvar,!!!syms(multi.xvars),data=data,firth=firth,formatted=FALSE) %>%
        rename_at(vars(-yvar,-xvar,-term),~paste0("multi.",.))
    }
    tbl <- univariate.tbl %>% left_join(multivariate.tbl,by=c("yvar","xvar","term"))
    if (formatted) {
      tbl <- tbl %>%
        mutate(p.value=pvalue(p.value),
               multi.p.value=pvalue(multi.p.value)) %>%
        mutate_at(vars(estimate,conf.low,conf.high,multi.estimate,multi.conf.low,multi.conf.high),~formatC(.,format="f",digits=2)) %>%
        transmute(yvar,xvar,term,n,
                  odds.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value,
                  multi.odds.ratio=paste0(multi.estimate," (",multi.conf.low," - ",multi.conf.high,")"),multi.p.value)
    }
  } else {
    tbl <- univariate.tbl
    if (formatted) {
      tbl <- tbl %>%
        mutate(p.value=pvalue(p.value)) %>%
        mutate_at(vars(estimate,conf.low,conf.high),~formatC(.,format="f",digits=2)) %>%
        transmute(yvar,xvar,term,n,
                  odds.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value)
    }
  }
  tbl
}



# excel functions ---------------------------------------------------------




#' Read Multiple Excel Sheets Into a List of Data Frames
#'
#' @param ... Either a file(s) or folder(s). If a folder is specified, it will look for all files ending in (.xlsx/.xls).
#' @param col_names `TRUE` to use the first row as column names, `FALSE` to get default names, or a character
#' vector giving a name for each column. This is passed to [readxl::read_excel()] function.
#' @param keep.nested If `TRUE`, returns a nested list of files and then sheets. Otherwise, a list of sheets is normally returned.
#' @param bare.filename Whether to use bare vs. full path as the filename.
#' @return A named list of data frames, where each data frame represents a sheet.
#' @export
read_all_excel <- function( ... ,col_names=TRUE,keep.nested=FALSE,bare.filename=TRUE) {
  pathlist <- list(...)
  requireNamespace("readxl",quietly=TRUE)

  filelist <- sapply(pathlist,function(path) {
    if (dir.exists(path)) {
      list.files(path=path,pattern="(xls|xlsx)$",recursive=TRUE,full.names=TRUE)
    } else if (all(file.exists(path))) {
      path
    } else {
      stop("YTError: Input should be path or folder")
    }
  })
  read_excel_file <- function(file,col_names=TRUE) {
    sheets <- readxl::excel_sheets(file)
    message(file,": ",length(sheets)," sheets")
    allsheets <-lapply(sheets,function(sheet) {
      suppressMessages(readxl::read_excel(path=file,sheet=sheet,col_names=col_names,col_types="text"))
    })
    names(allsheets) <- sheets
    return(allsheets)
  }
  excellist <- lapply(filelist,read_excel_file,col_names=col_names)

  if (bare.filename) {
    names(excellist) <- sub("\\.xlsx$","",basename(filelist))
  } else {
    names(excellist) <- filelist
  }
  if (!keep.nested) {
    excellist <- unlist(excellist,recursive=FALSE)
  }
  return(excellist)
}



#' Write multiple data frames to an Excel file
#'
#' @param ... objects to be written to the Excel file. Can be a data frames or lists of data frames.
#' @param file the Excel file to be written
#' @export
write_all_excel <- function(..., file) {
  requireNamespace("xlsx",quietly=TRUE)
  quolist <- enquos(..., .named=TRUE)
  objlist <- quolist %>% map(eval_tidy)
  wb <- xlsx::createWorkbook()
  for (i in 1:length(objlist)) {
    data <- objlist[[i]] %>% as.data.frame() # doesn't handle tibble well, so convert to plain data.frame
    sheetname <- names(objlist)[i]
    if (!is.data.frame(data)) {
      stop(st_glue("YTError: '{sheetname}' is not a dataframe."))
    }
    sheet  <- xlsx::createSheet(wb,sheetName=sheetname)
    xlsx::addDataFrame(data,sheet,row.names=FALSE)
  }
  xlsx::saveWorkbook(wb,file)
}


#' Read Excel File 2
#'
#' Same as [readxl::read_excel()] function, but col_types can be named vector
#'
#' @param path Path to the xls/xlsx file
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Defaults to the first sheet.
#' @param col_names Either TRUE to use the first row as column names, FALSE to number columns sequentially from X1 to Xn, or a character vector giving a name for each column.
#' @param col_types Either NULL to guess from the spreadsheet or a character vector containing "blank", "numeric", "date" or "text". (YTmod: can be named vector listing only variables you want to change)
#' @param na Missing value. By default readxl converts blank cells to missing data. Set this value if you have used a sentinel value for missing values.
#' @param skip Number of rows to skip before reading any data.
#' @return A data frame consisting of Excel data.
#' @export
read_excel2 <- function(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0) {
  requireNamespace("readxl",quietly=TRUE)
  #path="Allo Patients 2015Apr15.xlsx";col_types=c("ANC 500"="date","POD Date"="date","Last Contact"="date","Relapse Date"="date","Onset"="date");sheet=1;col_names=TRUE;na="";skip=0
  if (!is.null(col_types)) {
    data <- readxl::read_excel(path=path,sheet=sheet,col_names=col_names,na=na,skip=skip)
    dtypes <- structure(recode2(sapply(data,function(x) first(class(x))),c("character"="text","POSIXct"="date")),names=names(data))
    if (any(names(col_types) %!in% names(dtypes))) {
      stop("YTError, variable names in col_types not all found in excel sheet names!")
    }
    dtypes[match(names(col_types),names(dtypes))] <- col_types
    col_types <- dtypes
  }
  xl <- readxl::read_excel(path=path,sheet=sheet,col_names=col_names,col_types=col_types,na=na,skip=skip)
  return(xl)
}






# miscellaneous system operation functions --------------------------------





send.text.message <- function(text,
                              to=Sys.getenv("TWILIO_DEFAULT_TO"),
                              from=Sys.getenv("TWILIO_DEFAULT_FROM")) {
  requireNamespace("twilio")
  if (to=="" | from=="") {
    TWILIO_DEFAULT_TO
    stop("YTError: to use defaults, need TWILIO_DEFAULT_TO and TWILIO_DEFAULT_FROM in the R environment.")
  }

  # twilio::tw_send_message(to="9149802489",from="8445581194",body="testing!",)
  # (to, from, body = NULL, media_url = NULL)

  # need these in r_environ:
  # TWILIO_SID = xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # TWILIO_TOKEN = xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # TWILIO_DEFAULT_TO = 123456789
  # TWILIO_DEFAULT_FROM = 987654321
  twilio::tw_send_message(to=to,from=from,body=text)
}








#' Open a File using File Associations
#'
#' Opens the specified file using the application specified.
#'
#' Note that this does different things depending on operating system. Windows R already has a [base::shell.exec()] function, so it just uses that.
#' Linux R doesn't have this function, so this emulates on Linux using the 'xdg-open' terminal command..
#' @param file file to be opened
#' @return No value.
#' @author Ying Taur
#' @export
shell.exec <- function(file) {
  if (Sys.info()['sysname']=="Linux") {
    file <- gsub(" ","\\\\ ",file)
    system(paste("xdg-open",file),wait=FALSE)
  } else if (Sys.info()['sysname']=="Windows") {
    base::shell.exec(normalizePath(file))
  } else if (Sys.info()['sysname']=="Darwin") {
    system(paste("open",file),wait=FALSE)
  } else {
    stop("YTError: Not sure how to handle this operating system: ",Sys.info()["sysname"],"\nGo tell Ying about this.")
  }
}


#' Traverse a list (or other iterable object)
#'
#' Everyone knows I like recursion. This function recursively iterates through nested lists and runs a specified expression.
#' @param .obj object list to be traversed
#' @param expr expression to run on each object in the list. Reserved words can be used, as follows:
#' * `.obj`: the current object
#' * `.name`: a name for the object
#' * `.class`: the class of the object
#' * `.level`: an integer specifying the number of levels of recursion.
#' * `.parents`: a list of parent objects (first element is most direct parent)
#' * `.circular`: whether the object points to a parent
#' @param .name used for passing information to other instances. Leave these be.
#' @param .level used for passing information to other instances. Leave these be.
#' @param .parents used for passing information to other instances. Leave these be.
#'
#' @return a list of objects generated from `expr`
#' @export
#'
#' @examples
#' g <- ggplot(mtcars,aes(x=mpg,y=hp)) + geom_point()
#' g.info <- traverse(g,expr=tibble(name=.name,class=.class,level=.level)) %>%
#'   bind_rows()
#' g.info
traverse <- function(.obj, expr=NULL, .name=NULL,.level=1,.parents=list(caller_env=rlang:::caller_env())) {
  expr <- enquo(expr)
  if (is.null(.name))  {
    .name <-  deparse1(substitute(.obj))
  }
  .class <- paste(class(.obj),collapse="|")
  circular.check <- map_lgl(.parents,~identical(.x,.obj))
  .circular <- any(circular.check)
  env <- rlang::quo_get_env(expr)
  env$.class <- .class
  env$.name <- .name
  env$.parents <- .parents
  env$.circular <- .circular
  env$.level <- .level
  env$.obj <- .obj
  data <- rlang::eval_tidy(expr,env=env)

  ldata <- list(data)
  if ((is.list(.obj) || is.environment(.obj)) && length(.obj)>0 && !.circular) {
    len <- length(.obj)
    index1 <-  names(.obj)
    index2 <- 1:len

    if (is.null(index1)) {
      index <- index2 %>% as.list()
    } else {
      index <- map2(index1,index2,~{
        if (.x!="") {
          return(.x)
        } else {
          return(.y)
        }
      })
    }
    .objname <- index %>% map(~{
      if (is.numeric(.x)) {
        paste0("[[",.x,"]]")
      } else {
        paste0("$",.x)
      }
    })
    lineage <- c(.parents,setNames(list(.obj),.name))

    children <- map2(index,.objname,~{
      # message(.name,.y)
      child <- .obj[[.x]]
      full.name <- paste0(.name,.y)
      traverse(.obj=child, expr=!!expr, .name=full.name, .level=.level+1,.parents=lineage)
    }) %>% do.call(c,.)
    ldata <- c(ldata,children)
  }
  return(ldata)
}



#' Display sizes of objects in memory
#'
#' Use this to see what is occupying memory
#' @param envir the environment (or object) from which to list objects. Default is `.GlobalEnv`
#' @return a data frame showing objects and the object size, in Mb.
#' @export
ls.object.sizes <- function(envir=.GlobalEnv) {
  # recursive function to deal with environments
  get.size <- function(obj) {
    if (is_environment(obj)) {
      subobj.list <- ls(obj)
      size <- subobj.list %>% map(~get(.,envir=obj)) %>% map_dbl(get.size) %>% sum()
    } else {
      size <- object.size(obj) %>% as.numeric()
    }
    return(size)
  }
  objects <- ls(pos=envir)
  if (length(objects)==0) {
    message("No objects found.")
    return(NULL)
  }
  dsize <- lapply(objects,function(objname) {
    obj <- get(objname,pos=envir)
    bytes <- get.size(obj)
    # mb <- format(size,units="Mb")
    size <- utils:::format.object_size(bytes, "auto")
    class <- class(obj)[1]
    tibble(obj=objname,class,size,bytes)
  }) %>% bind_rows() %>% arrange(desc(bytes)) %>% select(-bytes)
  return(dsize)
}


#' Determine all dependent packages
#'
#' For a given installed package, determine all the downstream dependent packages, nesting through all sub-packages.
#' Removes R itself and its included base packages, such as `stats` or `utils`.
#'
#' This function works by searching through dependencies in a recursive manner. A global data frame is maintained to remove packages that
#' have already been processed. This increases the efficiency and avoids any possibility of circular dependencies (if that is even possible).
#'
#' @param pkg package name
#'
#' @return a character vector of all sub-packages
#' @export
#'
#' @examples
#' get.all.dependencies("purrr")
get.all.dependencies <- function(pkg)  {

  all.pkgs <- installed.packages() %>% as_tibble()
  base.pkgs <- all.pkgs %>% filter(Priority=="base")
  installed.pkgs <- all.pkgs %>% filter(Priority!="base"|is.na(Priority))
  pkgs <- installed.pkgs %>%
    mutate(across(c(Depends,Imports),~str_split(.x,", ?")),
           deps=map2(Depends,Imports,~{
             all <- c(.x,.y) %>% {.[!is.na(.)]} %>% str_replace_all("\\n","") %>% str_replace_all(" ?\\(.+\\)$","")
             setdiff(all,c("R",base.pkgs$Package))
           })) %>%
    select(Package,deps) %>% unique()
  #recursive function, with global variable
  local({
    pp <- pkgs
    f <- function(pkg) {
      i <- which(pp$Package==pkg)[1]
      if (length(i)==0)  { # pkg not there
        return(character())
      }
      subs <- pp$deps[[i]]
      pp <<- pp %>% filter(Package!=pkg)
      if (length(subs)==0) {
        return(pkg)
      } else {
        sub.pkgs <- subs %>% map(f) %>% simplify()
        return(c(pkg,sub.pkgs))
      }
    }
    f(pkg)
  })
}




#' Set Namespace Precedence
#'
#' @param pkg package name to be prioritized
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(plyr)
#'
#' # will not work
#' mtcars %>% rename(mpg1=mpg)
#'
#' set_precedence("dplyr")
#'
#' # will work
#' mtcars %>% rename(mpg1=mpg)
set_precedence <- function(pkg) {
  detach(paste0("package:", pkg), character.only = TRUE)
  attachNamespace(pkg)
}

#' List all functions exported by installed packages
#'
#' Find all exported functions from all installed packages. Use this to find namespace collisions.
#'
#' @param remove.reexported If `TRUE` (default), remove functions that were re-exported from other packages. (e.g. `ggtree::ggplot` is the same as `ggplot2::ggplot`)
#' @export
#' @examples
#' f <- list.all.exported.functions()
#' f %>% filter(Function=="rename")
#' f %>% filter(Function=="slice")
#' f %>% filter(Function=="rescale")
#' f %>% filter(Function=="first")
list.all.exported.functions <- function(remove.reexported=TRUE)  {

  get_exported <- function(pkg) {
    fs <- tryCatch({
      getNamespaceExports(pkg)
    },error=function(e) {
      NULL
    })
    return(fs)
  }
  get_imported <- function(pkg) {
    fs <- tryCatch({
      getNamespaceImports(pkg)
    },error=function(e) {
      NULL
    })
    return(fs)
  }

  pkgs0 <- installed.packages() %>% as_tibble() %>%
    mutate(loaded=Package %in% .packages(),
           Function=map(Package,get_exported))

  pkgs <- pkgs0 %>%
    unnest(Function) %>%
    select(loaded,Function,Package,everything())

  imp <- tibble(Package=pkgs0$Package) %>%
    mutate(Function=map(Package,get_imported)) %>%
    unnest(Function) %>%
    mutate(origin.pkg=names(Function)) %>%
    filter(origin.pkg!="base") %>%
    unnest(Function) %>% mutate(re.exported=TRUE)

  pkgs2 <- pkgs %>% left_join(imp,by=c("Package","Function")) %>%
    select(Function,Package,origin.pkg,loaded,re.exported,everything()) %>%
    replace_na(list(re.exported=FALSE))

  if(remove.reexported){
    pkgs2 <- pkgs2 %>% filter(!re.exported)
  }
  return(pkgs2)
}


#' File activity status
#'
#' Given filepath(s), detect changes in file sizes to see if it is actively changing.
#' @param paths the filepath(s) to be read.
#' @param wait.time amount of time in seconds to wait between reads. Default is 1 second.
#'
#' @return table describing files and their activity
#' @export
#'
#' @examples
file_activity_status <- function(paths,wait.time=1)  {
  get_files <- function(paths,tvar) {
    tvar <- enquo(tvar)
    paths %>% map_dfr(~{
      if (dir.exists(.x)) {
        tibble(dir=.x,file=list.files(.x,recursive=TRUE,full.names=TRUE))
      } else if (file.exists(.x))  {
        .x <- normalizePath(.x)
        tibble(dir=dirname(.x),file=.x)
      } else  {
        stop("YTError: file/dir not found!")
      }
    }) %>% mutate(!!tvar:=file.size(file))
  }

  d1 <- get_files(paths,size1)
  Sys.sleep(wait.time)
  d2 <- get_files(paths,size2)

  d <- full_join(d1,d2,by=c("dir","file")) %>%
    mutate(diff=size2-size1,
           file=basename(file))
  files.inactive <- all(!is.na(d$diff) & d$diff==0)
  if (files.inactive) {
    message("files are inactive.")
  } else {
    message("files are active.")
  }
  return(d)
}




#' Kill port process
#'
#' Find process that is listening to the specified port and kill it.
#'
#' @param port the port (numeric) to search.
#' @examples
#' kill_port_process(4567)
#' @export
kill_port_process <- function(port) {
  requireNamespace("pingr",quietly=TRUE)
  localhost <- "127.0.0.1"
  port_active <- pingr::is_up(localhost,port,check_online=FALSE)
  if (port_active) {
    if (.Platform$OS.type=="unix") {
      kill <- paste0("kill -9 $(lsof -t -i:",port," -sTCP:LISTEN)")
      system(kill)
    } else if (.Platform$OS.type=="windows") {
      kill <- paste0('for /f "tokens=5" %a in (\'netstat -aon ^| find ":',port,'" ^| find "LISTENING"\') do taskkill /f /pid %a')
      shell(kill)
    } else {
      stop("YTError: this function doesn't yet work for this OS.")
    }
  }
}



#' Evaluate expression in another R session
#'
#' Use these to run code in a separate instance of R, separate from your current console.
#' This is essentially doing what the [`callr`] package does, but just adds a few modifications to make it easier to use.
#'
#' In some cases it is useful to run code in a separate R session... this is where functions from the [`callr`] package
#' come in handy, such as [callr::r()] or [callr::r_bg()]. However, in order to use these, you have to be sure to:
#' 1. place the code inside an anonymous function,
#' 2. refer to functions and variables explicitly from other packages using the :: notation,
#' 3. pass any necessary local variables as arguments to the anonymous function.
#'
#' `run_r()` and `run_r_bg()` are running [callr::r()] and [callr::r_bg()], except that you can
#' just insert the code without worrying about about the 3 modifications above.
#'
#' @param expr expression to be run in separate R session.
#' @param envir the environment to execute the code.
#'
#' @return For `run_r`: the value of the evaluated expression.
#' For `run_r_bg`: an [`callr::r_process`] object, which has a [callr::get_result()] method to collect the result.
#' For `run_r_callargs`: a list containing a modified function and a list of arguments to pass to the function;
#' this is designed to be the arguments that can be plugged into [callr::r()] or [callr::r_bg()].
#' @export
run_r <- function(expr,envir=parent.frame()) {
  requireNamespace("callr",quietly=TRUE)
  expr <- enexpr(expr)
  call.arglist <- run_r_callargs(!!expr,envir=envir)
  do.call(r,call.arglist)
}

#' @rdname run_r
#' @export
run_r_bg <- function(expr,envir=parent.frame()) {
  requireNamespace("callr",quietly=TRUE)
  expr <- enexpr(expr)
  call.arglist <- run_r_callargs(!!expr,envir=envir)
  do.call(callr::r_bg,call.arglist)
}

#' @rdname run_r
#' @export
run_r_callargs <- function(expr,envir=parent.frame()) {
  requireNamespace(c("stringr","callr"),quietly=TRUE)
  expr <- enexpr(expr)
  text <- quo_text(expr)
  info <- get.code.info(text=text,envir=envir)
  argline <- paste(info$locals,collapse=",")
  newtext <- info$parsedata %>% group_by(line1) %>%
    summarize(code=paste(explicit.text,collapse=" ")) %>%
    # summarize(code=paste(text,collapse=" ")) %>%
    ungroup() %>% pull(code) %>% paste(collapse="\n")

  if (length(info$library.pkgs)>0) {
    library.line <- paste0("library(",info$library.pkgs,")",collapse=";")
  } else {
    library.line <- ""
  }
  fncode <- stringr::str_glue("function({argline}) {{
    {library.line}
    {newtext}
  }}")
  fn <- eval(parse(text=fncode))
  print(info$locals)
  arglist <- map(info$locals,get,envir=envir) %>% setNames(info$locals)
  call.arglist <- list(func=fn,args=arglist)
  return(call.arglist)
}



#' Run a shiny gadget in background
#'
#' Similar to [shiny::runGadget()], you can use this to run shiny apps in the viewer pane of RStudio.
#' The difference is that the R console is not blocked during execution,
#' so you can continue coding while the shiny app is running.
#'
#' This function works by deploying the Shiny app is run in the background
#' (using [callr::r_bg()]), then having the viewer panel set to display the corresponding port.
#' If a process already exists that is listening to the port, that process is killed (using [kill_port_process()]).
#'
#' @param app A Shiny app object created by [shiny::shinyApp()]
#' @param port The TCP port that the application should listen on.
#' @return A [`callr::r_process`] object, which is running separately in the background.
#' @examples
#' library(shiny)
#' app <- shinyApp(ui = fluidPage(
#'   titlePanel(paste0("Hello Shiny!")),
#'   sidebarLayout(
#'     sidebarPanel(
#'       sliderInput(inputId = "bins",
#'                   label = "Number of bins:",
#'                   min = 1, max = 50, value = 30)
#'     ),
#'     mainPanel(
#'       plotOutput(outputId = "distPlot")
#'     )
#'   )
#' ), server = function(input, output) {
#'   output$distPlot <- renderPlot({
#'     x    <- faithful$waiting
#'     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'     hist(x, breaks = bins, col = "#75AADB", border = "white",
#'          xlab = "Waiting time to next eruption (in mins)",
#'          main = "Histogram of waiting times")
#'   })
#' })
#' ps <- runGadget_bg(app)
#'
#' # to stop the app, run this or quit RStudio
#' ps$kill()
#' @export
runGadget_bg <- function(app,args=list(),port=4567) {
  requireNamespace(c("shiny","callr","pingr"),quietly=TRUE)
  expr <- enquo(expr)
  localhost <- "127.0.0.1"
  url <- paste0("http://",localhost,":",port)
  kill_port_process(port)
  ps <- callr::r_bg(function(...) {
    shiny::runApp(app,port)
  },args=args)
  message("Running Shiny app; setting viewer to ",url)
  for (i in 1:200) {
    port_ready <- pingr::is_up(localhost,port,check_online=FALSE)
    if (port_ready) {
      break
    }
    Sys.sleep(0.2)
  }
  getOption("viewer")(url)
  return(ps)
}

