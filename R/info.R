

#' Info on an object, or compare two objects
#'
#' Provide information about an object, or compare two objects.
#' @details
#' ```{asciicast info-compare}
#' x <- c("A","B","C")
#' cli_alert("letters are {x}")
#' ```
#' @param x object to be described/compared.
#' @param y object to be compared.
#' @export
#' @rdname info-compare-methods
#' @examples
#' library(tidyverse)
#' chr <- sentences[1:10]
#' chr.rev <- rev(chr)
#' chr.dup <- c(chr,chr,chr)
#' chr.dup2 <- c(chr,chr) %>% sort()
#' chr.part <- chr[1:5]
#' num <- 1:10
#' dates <- Sys.Date() + 1:10
#' fct <- as.factor(chr)
#' fct.rev <- fct_rev(fct)
#' lgl <- c(T,T,F,F,T,T,T,F,F,T)
#' lst <- as.list(df)
#' mat <- as.matrix(mtcars)
#' df <- mtcars %>% rownames_to_column("car")
#' df.dup <- bind_rows(df,df)
#' df.sort <- df %>% arrange(mpg)
#' df.partrows <- df %>% slice(1:6)
#' df.partcols <- df %>% select(mpg,cyl,disp) %>% mutate(col=1)
#' df.part1 <- df %>% select(car,2:9) %>% slice(1:22)
#' df.part2 <- df %>% select(car,5:12) %>% slice(15:32)
#' df.diff <- df %>% mutate(mpg=ifelse(row_number()==1,1,mpg))
#' df.diff2 <- df %>% mutate(mpg=mpg+1,disp=disp+1)
#' df.nojoin <- df %>% mutate(car=paste(car,"x"))
#' info(chr)
#' info(chr.dup)
#' info(chr.rev)
#' info(chr.part)
#' info(num)
#' info(dates)
#' info(fct)
#' info(lgl)
#' info(df)
#' info(df,by="car")
#' info(df,by=c("mpg","car"))
#' compare(chr,chr)
#' compare(chr,chr.rev)
#' compare(chr,chr.dup)
#' compare(chr.dup,chr.dup2)
#' compare(fct,fct.rev)
#' compare(df,df)
#' compare(df,df.sort)
#' compare(df,df.partcols)
#' compare(df,df.partrows)
#' compare(df.partcols,df.partrows)
#' compare(df,df.partrows,by="car")
#' compare(df,df.nojoin,by="car")
#' compare(df,df.dup,by="car")
#' compare(df.part1,df.part2,by="car")
#' compare(df,df.diff,by="car")
#' compare(df,df.diff2,by="car")
setGeneric("info", function(x,...) {
  standardGeneric("info")
})
#' @rdname info-compare-methods
#' @aliases info,ANY-method
setMethod("info", signature("ANY"), function(x) {
  # atomic vectors
  l <- .info.vector(x)
  cat_line(l$cli.desc)
  return(invisible(l))
})
#' @rdname info-compare-methods
#' @aliases info,data.frame-method
setMethod("info", signature("data.frame"), function(x,by=NULL) {
  l <- .info.df(x,by=by)
  cat_line(l$cli.desc)
  return(invisible(l))
})


#' @rdname info-compare-methods
#' @export
setGeneric("compare", function(x,y,...) {
  standardGeneric("compare")
})
#' @rdname info-compare-methods
#' @aliases compare,ANY,ANY-method
setMethod("compare", signature("ANY","ANY"), function(x,y) {
  # atomic vectors
  l <- .compare.vector(x,y)
  # cat(l$oneliner,sep="\n")
  cat_line(l$cli.desc)
  return(invisible(l))
})
setMethod("compare", signature("data.frame","data.frame"), function(x,y,by=NULL) {
  l <- .compare.df(x,y,by=by)
  # cat(l$oneliner,sep="\n")
  cat_line(l$cli.desc)
  return(invisible(l))
})


# for values
ansi_values <- function(x,sep=", ",trunc=Inf) {
  ansi_collapse(pillar:::format_glimpse_1(x),sep=sep,last=sep,style="head",trunc=trunc)
}

ansi_cols <- function(x,sep=", ",trunc=Inf) {
  ansi_collapse(col_br_yellow(x),sep=sep,last=sep,style="head",trunc=trunc)
}

cli_short_list <- function(x,n=3) {
  cli_vec(x,style=list("vec-trunc"=n,
                       "vec-sep"=", ",
                       "vec-last" = ", "))
}




.info.vector <- function(x) {
  l <- list()
  l$len <- length(x)
  l$is_named <- !is.null(names(x))
  l$is_empty <- l$len==0
  l$type <- class(x)
  l$type_sum <- paste0(type_sum(x),"[",l$len,"]")
  l$n_na <- sum(is.na(x))
  l$is_sorted <- !is.unsorted(x,na.rm=TRUE)
  l$n_distinct <- n_distinct(x)
  l$is_distinct <- l$len==l$n_distinct
  if (is.character(x) || is.factor(x)) {
    l$maxchar <- max(nchar(x))
  } else {
    l$maxchar <- NA_integer_
  }

  # l$is_distinct_without_na <- !l$is_distinct && all(tbl[!is.na(names(tbl))]==1)
  pct_distinct <- l$n_distinct / l$len
  if (l$is_distinct) {
    cli_uniqueness <- "distinct"
  } else if (pct_distinct>0.9) {
    cli_uniqueness <- str_glue("{scales::label_percent()(pct_distinct)} distinct")
  } else {
    cli_uniqueness <- str_glue("{short_number(l$n_distinct)} vals")
  }
  pct_na <- l$n_na/l$len
  if (pct_na>0.01) {
    pct <- scales::label_percent()(pct_na)
    cli_na_values <- paste(pct,"NA")
  } else {
    cli_na_values <- NULL
  }
  if (l$is_named) {
    cli_named <- "named"
  } else {
    cli_named <- NULL
  }
  if (l$is_sorted) {
    cli_sorted <- "sorted"
  } else {
    cli_sorted <- NULL
  }
  if (!is.na(l$maxchar) && l$maxchar>100) {
    cli_maxchar <- str_glue("nchar={l$maxchar}")
  } else {
    cli_maxchar <- NULL
  }
  descriptors <- c(cli_uniqueness,cli_na_values,cli_maxchar,cli_named,cli_sorted) %>%
    paste(collapse="; ")
  maxvals <- (cli::console_width() %/% 3) + 1
  l$cli.table <- tibble(
    type = col_grey(l$type_sum),
    desc = col_grey(style_italic(str_glue("<{descriptors}>"))),
    values = ansi_values(head(x,maxvals))
  )
  l$cli.desc <- cli_format_method({
    cli_table(l$cli.table)
  })
  return(l)
}


.info.df <- function(x, by=NULL) {
  l <- list()
  l$nrow <- nrow(x)
  l$ncol <- ncol(x)
  l$type_sum <- str_glue("df[{l$nrow},{l$ncol}]")
  l$rownames <- rownames(x)
  l$colnames <- colnames(x)
  l$cols <- map(x,.info.vector)

  l$distinct.cols <- names(l$cols)[map_lgl(l$cols,~{.x$is_distinct})]

  # l$cols$mpg$cli.table
  l$cli.table <- imap(l$cols,~{
    .x$cli.table %>% mutate(name=ansi_cols(.y)) %>% select(name,everything())
  }) %>% list_rbind()

  by <- by %||% names(x)
  if (setequal(by,names(x))) {
    bytext <- "Across all cols"
  } else {
    #bytext <- paste("By",ansi_collapse(col_blue(by)))
    bytext <- paste("By",ansi_cols(by,sep="+"))
  }
  # any(by %in% l$distinct.cols) faster than is.distinct(x,!!!syms(by))
  l$is_distinct_across_by <- !is.null(by) && (any(by %in% l$distinct.cols) || is.distinct(x,!!!syms(by)))

  if (l$is_distinct_across_by) {
    rowtext <- "rows are distinct"
  } else {
    rowtext <- "rows are non-distinct"
  }
  l$cli.desc <- cli_format_method({
    cli_text("{bytext}: {rowtext}")
    cli_table(l$cli.table)
  })
  return(l)
}


.compare.vector <- function(x,y) {
  l <- list()
  X <- col_red("X")
  Y <- col_br_blue("Y")
  l$x <- .info.vector(x)
  l$y <- .info.vector(y)
  l$is_sametype <- l$x$type[1]==l$y$type[1]
  l$is_samelength <- l$x$len==l$y$len
  l$is_identical <-
    l$is_sametype &&
    l$is_samelength &&
    all.same(x,y)
  l$is_identical_but_difforder <-
    l$is_samelength &&
    !l$is_identical &&
    all.same(l$x$sort,l$y$sort)
  l$is_identical_but_difftype <-
    !l$is_identical &&
    l$is_samelength &&
    # !l$is_sametype &&
    all.same(as.character(x),as.character(y))

  # join relationship
  l$join_relationship <- paste0(ifelse(l$x$is_distinct,"one","many"),"-to-",ifelse(l$y$is_distinct,"one","many"))
  # set ops
  l$xy.setdiff <- setdiff(x,y)
  l$xy.setdiff_len <- length(l$xy.setdiff)
  l$yx.setdiff <- setdiff(y,x)
  l$yx.setdiff_len <- length(l$yx.setdiff)
  l$intersect <- intersect(x,y)
  l$intersect_len <- length(l$intersect)
  l$is_setequal <- setequal(x,y)  #length(l$xy.setdiff)==0 && length(l$yx.setdiff)==0
  l$has_nooverlap <- l$intersect_len==0
  l$y.subsetof.x <- l$xy.setdiff_len>0 && l$yx.setdiff_len==0
  l$x.subsetof.y <- l$xy.setdiff_len==0 && l$yx.setdiff_len>0

  tx <- tibble(var=paste0(X,":")) %>% cbind(l$x$cli.table)
  ty <- tibble(var=paste0(Y,":")) %>% cbind(l$y$cli.table)
  tbl <- bind_rows(tx,ty)

  l$cli.desc <- cli_format_method({
    if (l$is_identical) {
      cli_text("{X} and {Y} are identical")
    } else if (l$is_identical_but_difforder) {
      cli_text("{X} and {Y} are identical, but different order")
    } else if (l$is_identical_but_difftype) {
      cli_text("{X} and {Y} are identical, but different type/levels")
    } else if (l$is_setequal) {
      if (!l$x$is_distinct && l$y$is_distinct) {
        cli_text("{X} and {Y} are equal sets, where {X} has duplicates")
      } else if (l$x$is_distinct && !l$y$is_distinct) {
        cli_text("{X} and {Y} are equal sets, where {Y} has duplicates")
      } else if (!l$x$is_distinct && !l$y$is_distinct) {
        cli_text("{X} and {Y} are equal sets, but with different freqs")
      } else {
        cli_text("???????")
      }
    } else if (l$x.subsetof.y) {
      cli_text("{X} is a subset of {Y}")
    } else if (l$y.subsetof.x) {
      cli_text("{Y} is a subset of {X}")
    } else if (l$has_nooverlap) {
      cli_text("{X} and {Y} do not overlap")
    } else {
      cli_text("{X} and {Y} partially overlap")
    }
    cli_table(tbl)
  })
  return(l)
}


.compare.df <- function(x,y, by=NULL) {

  # x=df;y=df;by=NULL
  # x=df;y=df.partcols;by="car"
  # x=df;y=df.partrows;by="car"

  l <- list()
  X <- col_red("X")
  Y <- col_blue("Y")
  l$is_identical <- all.same(x,y)
  l$x <- .info.df(x)
  l$y <- .info.df(y)
  l$colnames <- .compare.vector(l$x$colnames,l$y$colnames)

  if (is.null(by)) {
    by <- l$colnames$intersect
  }
  l$join <- length(by)>0

  if (l$join) {
    by.x <- (names(by) %||% by) %>% if_else(.=="",by,.) %>% unname()  # similar to coalesce; if names(by) is NULL, then =by.
    by.y <- unname(by)
    if (!all(by.x %in% l$x$colnames)) {
      cli_abort("YTError: not all by cols found in X: {setdiff(by.x,l$x$colnames)}")
    }
    if (!all(by.y %in% l$y$colnames)) {
      cli_abort("YTError: not all by cols found in Y: {setdiff(by.y,l$y$colnames)}")
    }
    xx <- x %>% select(!!!syms(by.x)) %>%
      unite(col="..byvar..",!!!syms(by.x),sep="___") %>% mutate(..xrow..=row_number())
    yy <- y %>% select(!!!syms(by.y)) %>%
      unite(col="..byvar..",!!!syms(by.y),sep="___") %>% mutate(..yrow..=row_number())
    l$joinby <- .compare.vector(xx$..byvar..,yy$..byvar..)

    xx.yy <- inner_join(xx,yy,by="..byvar..",relationship="many-to-many")
    xx.orphan <- anti_join(xx,yy,by="..byvar..")
    yy.orphan <- anti_join(yy,xx,by="..byvar..")

    # this will be for when by is separated
    xcols <- setdiff(l$x$colnames,by.x)
    ycols <- setdiff(l$y$colnames,by.y)
    xy.compare.cols <- intersect(xcols,ycols)
    l$xy.value.overlap <- nrow(xx.yy)>0 && length(xy.compare.cols)>0
    if (l$xy.value.overlap) {
      xi <- xx.yy$..xrow..
      yi <- xx.yy$..yrow..
      xi.yi <- xy.compare.cols %>%
        map_int(~{
          xvals <- x[[.x]][xi]
          yvals <- y[[.x]][yi]
          sum(!is.same(xvals,yvals))
        }) %>% setNames(xy.compare.cols)
      l$xy.value.compare <- xi.yi
    }
    row1 <- tibble(col0="",
                   col1=str_glue("{X} only({l$colnames$xy.setdiff_len}): {ansi_cols(l$colnames$xy.setdiff,trunc=4)}"),
                   col2=str_glue("{X} and {Y}({l$colnames$intersect_len}): {ansi_cols(l$colnames$intersect,trunc=4)}"),
                   col3=str_glue("{Y} only({l$colnames$yx.setdiff_len}): {ansi_cols(l$colnames$yx.setdiff,trunc=4)}"))
    row2 <- tibble(col0=str_glue("{X} only: {nrow(xx.orphan)} rows"),
                   col1=str_glue("{nrow(xx.orphan)} x {l$colnames$xy.setdiff_len}"),
                   col2=str_glue("{nrow(xx.orphan)} x {l$colnames$intersect_len}"),
                   col3="(none)")
    row3 <- tibble(col0=str_glue("{X} and {Y}: {nrow(xx.yy)} rows"),
                   col1=str_glue("{nrow(xx.yy)} x {l$colnames$xy.setdiff_len}"),
                   col2=str_glue("{nrow(xx.yy)} x {l$colnames$intersect_len}, overlap"),
                   col3=str_glue("{nrow(xx.yy)} x {l$colnames$yx.setdiff_len}"))
    row4 <- tibble(col0=str_glue("{Y} only: {nrow(yy.orphan)} rows"),
                   col1="(none)",
                   col2=str_glue("{nrow(yy.orphan)} x {l$colnames$intersect_len}"),
                   col3=str_glue("{nrow(yy.orphan)} x {l$colnames$yx.setdiff_len}"))
    tbl <- bind_rows(row1,row2,row3,row4) %>%
      mutate(across(.cols=everything(),.fns=cli:::ansi_string))
    tbl$col1[4] <- col_grey(tbl$col1[4])
    tbl$col3[2] <- col_grey(tbl$col3[2])
    if (nrow(xx.yy)==0) {
      tbl$col1[3] <- col_grey("(none)")
      tbl$col2[3] <- col_grey("(none)")
      tbl$col3[3] <- col_grey("(none)")
    }
    if (nrow(xx.orphan)==0) {
      tbl$col1[2] <- col_grey("(none)")
      tbl$col2[2] <- col_grey("(none)")
      # tbl$col3[2] <- col_grey("(none)")
    }
    if (nrow(yy.orphan)==0) {
      # tbl$col1[4] <- col_grey("(none)")
      tbl$col2[4] <- col_grey("(none)")
      tbl$col3[4] <- col_grey("(none)")
    }
    if (length(l$colnames$xy.setdiff)==0) {
      tbl$col1[2] <- col_grey("(none)")
      tbl$col1[3] <- col_grey("(none)")
    }

    if (length(l$colnames$intersect)==0) {
      tbl$col2[2] <- col_grey("(none)")
      tbl$col2[3] <- col_grey("(none)")
      tbl$col2[4] <- col_grey("(none)")
    }

    if (length(l$colnames$yx.setdiff)==0) {
      tbl$col3[3] <- col_grey("(none)")
      tbl$col3[4] <- col_grey("(none)")
    }



  }


  l$cli.desc <- cli_format_method({
    if (l$is_identical) {
      cli_text("{X} & {Y} are identical")
    }

    if (l$colnames$intersect_len==0) {
      cli_text("Columns: {X} and {Y} do not share columns")
    } else if (l$colnames$xy.setdiff_len==0 && l$colnames$yx.setdiff_len==0) {
      cli_text("Columns: are the same ({ansi_cols(l$colnames$intersect)})")
    } else if (l$colnames$xy.setdiff_len==0 && l$colnames$yx.setdiff_len>0) {
      cli_text("Columns: {X} is a subset of {Y}")
    } else if (l$colnames$xy.setdiff_len>0 && l$colnames$yx.setdiff_len==0) {
      cli_text("Columns: {Y} is a subset of {X}")
    } else {
      cli_text("Columns: {X} and {Y} have some overlap")
    }
    if (l$join) {
      cli_text("Join by: {col_br_magenta(by)} ({l$joinby$join_relationship})")
      cli_table(tbl,sep=" | ")
      if (l$xy.value.overlap) {
        total.diff <- sum(l$xy.value.compare)
        if (total.diff==0) {
          cli_text("{X} and {Y} overlap values: all match")
        } else {
          diff.cols <- names(l$xy.value.compare)[l$xy.value.compare>0]
          cli_text("{X} and {Y} overlap values: mismatch in {total.diff} values in {ansi_cols(diff.cols)}")
        }
      }
    }
  })
  return(l)
}


# if (l$is_identical) {
#   cli_text("X and Y are identical")
# } else if (l$is_identical_but_difforder) {
#   cli_text("X and Y are identical, but different order")
# } else if (l$is_identical_but_difftype) {
#   cli_text("X and Y are identical, but different type")
# } else if (l$is_setequal) {
#   if (!l$x$is_distinct && l$y$is_distinct) {
#     cli_text("X and Y are equal sets, where X has duplicates")
#   } else if (l$x$is_distinct && !l$y$is_distinct) {
#     cli_text("X and Y are equal sets, where Y has duplicates")
#   } else if (!l$x$is_distinct && !l$y$is_distinct) {
#     cli_text("X and Y are equal sets, but with different freqs")
#   } else {
#     cli_text("???????")
#   }
# } else if (l$x.subsetof.y) {
#   cli_text("X is a subset of Y")
# } else if (l$y.subsetof.x) {
#   cli_text("Y is a subset of X")
# } else if (l$has_nooverlap) {
#   cli_text("X and Y do not overlap")
# } else {
#   cli_text("X and Y partially overlap")
# }




