
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
NULL


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




#' Not In
#'
#' Convenience function. \code{a \%!in\% b} is equivalent to \code{!(a \%in\% b)}
#' @export
"%!in%" = function(x,y) {
  !(x %in% y)
}

#' All In
#'
#' Convenience function. \code{a \%allin\% b} is equivalent to \code{all(a \%in\% b, na.rm=FALSE)}
#' @export
"%allin%" = function(x,y) {
  all(x %in% y)
}


#' Subtract Dates
#'
#' Returns number of days.
#' @export
"%-%" = function(x,y) {
  as.numeric(difftime(x,y,units="days"))
}


#' Left Join Replace
#'
#' Like left_join, but replaces column names in x with new columns from y.
#'
#' @param x, y tbls to join
#' @param by a character vector of variables to join by.
#' @return Returns a joined data frame
#' @author Ying Taur
#' @export
left_join_replace <- function(x,y,by=NULL) {
  replace.vars <- setdiff(intersect(names(x),names(y)),by)
  keep.xvars <- setdiff(names(x),setdiff(names(y),by))
  orig.x <- x %>% anti_join(y,by=by)
  change.x <- x %>% select_(.dots=keep.xvars) %>% inner_join(y,by=by)
  message(nrow(orig.x)," rows in x unchanged")
  message(nrow(change.x)," rows in x updated")
  message(length(replace.vars)," columns updated: ",paste(replace.vars,collapse=","))
  bind_rows(orig.x,change.x)
}




#' Tabulate
#'
#' Tabulates frequencies of vectors. By default, sorts by frequency.
#'
#' @param var the vector to be tabulated
#' @param sortby.freq if \code{TRUE}, sorts by order
#' @param useNA character specifying whether to tally \code{NA} values. This is passed to \code{tabulate}
#' @param as.char logical specifying whether to return tabulation as a single character. Useful for summarizing data within grouping commands such as \code{ddply} or \code{group_by}/\code{summarize}
#' @return Returns a data frame with tabulations.
#' @examples
#' ...examples.here....
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


#' Ying's DT view
#'
#' Use to peruse a dataframe within RStudio. Utilizes \code{DT} package.
#'
#' If data frame is grouped (i.e. \code{group_by} in dplyr), the rows will be sorted and shaded by group.
#' @param data dataframe to be viewed.
#' @param fontsize numeric controlling font size in the table, measured in px. Default is 12.
#' @param maxrows numeric controlling max number of rows to display. The purpose is to prevent DT from handling excessively large data frames. Default is 1000.
#' @return A javascript-style datatable, which displays in the Rstudio viewer.
#' @examples
#' mtcars %>% dt()
#' mtcars %>% group_by(cyl) %>% dt()
#' @author Ying Taur
#' @export
dt <- function(data,fontsize=10,maxrows=1000) {
  fontsize <- paste0(fontsize,"px")
  grps <- groups(data)
  #sort data by groups
  data2 <- data %>% ungroup() %>% arrange_(.dots=grps)
  data2$index_ <- data2 %>% group_indices_(.dots=grps)
  n.groups <- n_distinct(data2$index_)
  indices <- 1:n.groups
  n.colors <- 4
  pal <- c("light gray",brewer_pal("qual")(n.colors-1))
  clrs <- rep_len(pal,length.out=n.groups)
  clrs.rgb <- paste0("rgb(",apply(col2rgb(clrs),2,function(x) paste(x,collapse=",")),")")

  data2 %>%
    filter(row_number()<=maxrows) %>%
    DT::datatable(
      options=list(
        initComplete=JS(paste0("function(settings, json) {$(this.api().table().header()).css({'font-size':'",fontsize,"'});}")),
        paging=FALSE
      )
    ) %>%
    formatStyle(0:length(data2),fontSize=fontsize,lineHeight="95%") %>%
    formatStyle("index_",target="row",backgroundColor=styleEqual(indices,clrs.rgb))
}





#' Ying's Paste
#'
#' Similar to \code{paste} command, except that \code{NA}s are not converted to text.
#' If all fields are \code{NA}, then return \code{NA} if collapse if specified.
#'
#' This is useful when dealing with \code{NA} values. \code{paste} produces character \code{"NA"} values,
#' of course I could just convert those to actual \code{NA} values afterwards. However, if I use \code{collapse} option,
#' I can frequently get a lot of character combinations with \code{NA} in it, where it can be hard to remove.
#' This function comes in handy when using the \code{collapse} option and I need to guarantee a single character value.
#' An example is the \code{fun.aggregate} function for \code{dcast}, which requires a function that returns a single value.
#'
#' @param ... one or more \code{R} objects, to be converted to character vectors (same as \code{paste}).
#' @param sep a character string to separate the terms (same as \code{paste}).
#' @param an optional character string to separate the results (same as \code{paste}).
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
  data <- lapply(list(...),as.character)
  data <- do.call(cbind,data)
  p.text <- apply(data,1,function(x) {
    x <- x[!is.na(x)]
    paste(x,collapse=sep)
  })
  p.text[p.text==""] <- NA
  if (is.null(collapse)) {
    #no collapse
    return(p.text)
  } else {
    p.text <- p.text[!is.na(p.text)]
    if (length(p.text)==0) {
      #collapse: all NA, so return NA.
      return(as.character(NA))
    } else {
      return(paste(p.text,collapse=collapse))
    }
  }
}

#' Ying's Min/Max
#'
#' Similar to \code{min}/\code{max} command, except that if all values are \code{NA}s, the function returns
#' \code{NA} when \code{na.rm=TRUE} is specified, instead of \code{+/-Inf}.
#'
#' This is useful when using the function repetitively and it's possible that everything can be \code{NA}.
#' This might come in handy if running min/max functions across an \code{apply} or \code{ddply} command.
#'
#' @param ... numeric or character arguments.
#' @param na.rm a logical indicating whether missing values should be removed.
#' @return A length-one vector representing max or min.
#' @describeIn max2 \code{max2} maximum value.
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
  if (all(is.na(c(...)))) {
    return(as.numeric(NA))
  } else {
    max(...,na.rm=na.rm)
  }
}

#' @describeIn max2 \code{max2} minimum value.
#' @export
min2 <- function(...,na.rm=FALSE) {
  if (all(is.na(c(...)))) {
    return(as.numeric(NA))
  } else {
    min(...,na.rm=na.rm)
  }
}



#' Ying's Cut 2
#'
#' @export
cut2 <- function(x,lower,upper,quantiles,percentiles,date.bin,lvls) {
  if (!missing(lower)) {
    breaks <- unique(c(-Inf,lower,Inf))
    right=FALSE
  } else if (!missing(upper)) {
    breaks <- unique(c(-Inf,upper,Inf))
    right=TRUE
  } else if (!missing(quantiles)) {
    breaks=quantiles
    right=TRUE
  } else if (!missing(percentiles)) {
    breaks <- quantile(x,seq(0,1,1/percentiles))
    right=TRUE
  } else if (!missing(date.binsize) & class(x)=="Date") {
    startdate <- as.Date(paste0(min(year(x),na.rm=TRUE),"-01-01"))
    enddate <- as.Date(paste0(max(year(x),na.rm=TRUE)+1,"-01-01"))
    breaks <- seq(startdate,enddate,date.bin)
    right=FALSE
  } else {
    print("Error, need parameters!")
    return(NULL)
  }
  new.x <- cut(x,breaks,right=right,include.lowest=TRUE)
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
    levels(new.x) <- sapply(levels(new.x),function(y) {
      b1 <- ifelse(grepl("\\[",y),"<=","<")
      b2 <- ifelse(grepl("\\]",y),"<=","<")
      num <- unlist(str_extract_all(y,"-?[0-9.Inf]+"))
      newlvl <- paste0(num[1],b1,"X",b2,num[2])
      newlvl
    })
    levels(new.x) <- gsub("-Inf<=?|<=?Inf","",levels(new.x))
  }
  if (!missing(lvls)) {
    levels(new.x) <- lvls
  }
  return(new.x)
}




#' Color Shades
#'
#' Creates different shades of the specified color.
#'
#' Use this as a convenience function when creating your plots.
#' @param color character, specifying the color you want to build shades around. (e.g. \code{"red"} or \code{"#1460fa"})
#' @param ncolor number specifying the length of the vector, i.e. how many different shades should be returned (default 3 shades).
#' @param variation a number from 0-1, which determines how different the shades will be. Smaller numbers will be more similar.
#' @return Produces a character vector of colors, corresponding to shades of the specified color.
#' @examples
#' sh1 <- shades("red",5)
#' show_col(sh1)
#'
#' sh2 <- shades("red",20)
#' show_col(sh2)
#'
#' sh3 <- shades("red",20,variation=0.5)
#' show_col(sh3)
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

#' Copy to Clipboard
#'
#' Copies object to the clipboard, which can be used to paste into other programs such as Word or Excel.
#'
#' If \code{obj} is a data frame, it will look for carriage returns within the data, and replace with ";"
#' Note that if this is accomplished differently depending on operating system.
#' If Linux, xclip is used, so you may need to install this from terminal first: 'sudo apt-get install xclip'
#'
#' @param obj object to by copied. Can be data frame, matrix, table, vector.
#' @author Ying Taur
#' @export
copy.to.clipboard <- function(obj) {
  if (Sys.info()["sysname"]=="Linux") {
    con <- pipe("xclip -selection clipboard -i", open="w")
    col.names <- !is.vector(obj)
    write.table(obj, con, sep="\t",quote=FALSE, row.names=FALSE, col.names=col.names)
    close(con)
  } else if (Sys.info()["sysname"]=="Windows") { #windows
    if (is.data.frame(obj)) {
      #obj=cod.summary
      if (any((grepl("\n",c(as.character(unlist(obj)),names(obj)),fixed=TRUE)))) {
        print("Warning, carriage returns detected! Removing...")
        names(obj) <- gsub("\n",";",names(obj),fixed=TRUE)
        obj <- data.frame(lapply(obj,function(x) {
          if (is.character(x) | is.factor(x)) {
            gsub("\n",";",x,fixed=TRUE)
          } else {
            x
          }
        }),check.names=FALSE)
      }
      write.table(obj,"clipboard",sep="\t",row.names=FALSE,quote=FALSE)
    } else {
      writeClipboard(obj)
    }
  } else {
    stop("YTError: Not sure how to handle this operating system: ",Sys.info()["sysname"],"\nGo tell Ying about this.")
  }
  print("Copied to clipboard")
}


#' Open a File using File Associations
#'
#' Opens the specified file using the application specified.
#'
#' Note that this does different things depending on operating system. Windows R already has a \code{shell.exec} function, so it just uses that.
#' Linux R doesn't have this function, so this emulates on Linux using the 'xdg-open' terminal command..
#' @param file file to be opened
#' @return No value.
#' @author Ying Taur
#' @export
shell.exec <- function(file) {
  if (Sys.info()['sysname']=="Linux") {
    file <- gsub(" ","\\\\ ",file)
    system(paste("xdg-open",file))
  } else if (Sys.info()['sysname']=="Windows") {
    base::shell.exec(file)
  } else {
    stop("YTError: Not sure how to handle this operating system: ",Sys.info()["sysname"],"\nGo tell Ying about this.")
  }
}

#' Read Clipboard
#'
#' Read clipboard into vector or data frame.
#'
#' @param sep separator between lines
#' @return Contents of clipboard
#' @author Ying Taur
#' @export
read.clipboard <- function(sep="\n") {
  cb <- file("clipboard")
  vec <- scan(cb,what=character(),sep=sep)
  close(cb)
  tabs <- str_count(vec,"\t")
  #look for tabs
  if (all(tabs>0) & length(unique(str_count(vec,"\t")))==1) {
    header <- make.names(unlist(strsplit(vec[1],split="\t")),unique=TRUE)
    line <- vec[-1]
    tbl <- data_frame(line) %>% separate(line,into=header,sep="\t")
    return(tbl)
  } else {
    return(vec)
  }
}


#' Convert object to R-code.
#'
#' Produces R-code that would create the object inputted. I use this if I have some data object that I obtained
#' somehow but just want to declare it in the code.
#'
#' @param x object to be converted to R-code. Can be vector or data frame.
#' @param copy.clipboard logical, if \code{TRUE}, will copy the R-code to the Clipboard.
#' @return Returns the R-code.
#' @examples
#' values <- (1:5)^1.23
#' copy.as.Rcode(values)
#' @author Ying Taur
#' @export
copy.as.Rcode <- function(x,copy.clipboard=TRUE,fit=TRUE,width=getOption("width")-15) {
  #converts x to R-code.
  if (is.data.frame(x)) {
    x.cols <- sapply(x,copy.as.Rcode,copy.clipboard=FALSE)
    x.cols <- mapply(function(varname,var) paste0("\"",varname,"\"=",var),names(x),x.cols)
    rcode <- paste(x.cols,collapse=",\n")
    rcode <- paste0("data.frame(",rcode,")")
  } else if (is.Date(x)) {
    x.char <- copy.as.Rcode(as.character(x),copy.clipboard=FALSE)
    rcode <- paste0("as.Date(",x.char,")")
  } else if (is.POSIXlt(x)) { #these need to come before list, since these are lists.
    x.char <- copy.as.Rcode(as.character(x,usetz=TRUE),copy.clipboard=FALSE)
    rcode <- paste0("as.POSIXlt(",x.char,")")
  } else if (is.POSIXct(x)) {
    x.char <- copy.as.Rcode(as.character(x,usetz=TRUE),copy.clipboard=FALSE)
    rcode <- paste0("as.POSIXct(",x.char,")")
  } else if (is.factor(x)) {
    x.char <- copy.as.Rcode(as.character(x),copy.clipboard=FALSE)
    x.lvls <- copy.as.Rcode(as.character(levels(x)),copy.clipboard=FALSE)
    rcode <- paste0("factor(",x.char,",levels=",x.lvls,")")
  } else if (is.list(x)) {
    x.cols <- sapply(x,copy.as.Rcode,copy.clipboard=FALSE)
    x.cols <- mapply(function(varname,var) paste0("\"",varname,"\"=",var),names(x),x.cols)
    rcode <- paste(x.cols,collapse=",\n")
    rcode <- paste0("list(",rcode,")")
  } else {
    if (is.character(x)) {
      x <- gsub("\\\\","\\\\\\\\",x) #\\
      x <- gsub("\t","\\\\t",x) #\t
      x <- gsub("\n","\\\\n",x) #\n
      x <- gsub("\"","\\\\\"",x) #\"
      rcode <- ifelse(is.na(x),x,paste0("\"",x,"\""))
    } else {
      rcode <- x #e.g. numeric or logical
    }
    if (!is.null(names(x))) {
      x.names <- names(x)
      x.names <- paste0("\"",x.names,"\"")
      #x.names <- ifelse(sapply(x.names,function(n) n==make.names(n)),x.names,paste0("\"",x.names,"\""))
      rcode <- paste0(x.names,"=",rcode)
    }
    if (length(x)>1) {
      rcode <- paste(rcode,collapse=",")
      rcode <- paste0("c(",rcode,")")
    }
  }
  if (fit) {
    rcode <- fit(rcode,width=width,copy.clipboard=FALSE)
  }
  if (copy.clipboard) {
    copy.to.clipboard(rcode)
  }
  return(rcode)
}



#' Extract any text within quotes.
#'
#' Works like \code{str_extract_all}, but is used to extract quoted text within text. This comes for example text a character string contains code itself, like a python list.
#'
#' This is more difficult than you might think. \code{str_extract_all(text,middle.pattern("\"",".*","\""))}
#' doesn't work because (1) it includes stuff on either side of the quote, and (2) it will fail if there are quotes inside the text (which look like \code{\\\"})within the quoted text.
#' So you need to extract based on \code{\"} but ignore \code{\\\"}, and only extract stuff between pairs of quotes.
#' @param text character vector with quotes to be extracted.
#' @param convert.text.quotes logical indicating whether or not to convert \\\" to \" after converting.
#' @examples
#' #Should be a 3 item python list, with middle item being empty.
#' python.list <- "[\"no quotes here, ok?\",\"\",\"I like to put \\\"things\\\" in quotes\"]"
#' #This doesn't work....
#' str_extract_all(python.list,middle.pattern("\"",".*","\""))
#' #This also doesn't work...
#' str_extract_all(python.list,middle.pattern("\"","[^\"]*","\""))
#' #Even this doesn't work
#' str_extract_all(python.list,middle.pattern("(?<!\\\\)\\\"",".*","(?<!\\\\)\\\""))
#' #But: use this function to get it done.
#' str_extract_all_quotes(python.list)
#' @author Ying Taur
#' @export
str_extract_all_quotes <- function(text,convert.text.quotes=TRUE) {
  #text="\"\", \"tRNA acetyltransferase TAN1\""
  quote.pattern <- "(?<!\\\\)\\\""
  quote.list <- lapply(text,function(x) {
    quote.pos <- gregexpr(quote.pattern,x,perl=TRUE)[[1]]
    if (quote.pos[1]==-1) {
      return(NULL)
    }
    if (length(quote.pos) %% 2!=0) stop("YTError: Found an odd number of quotes in this character string:\n",x)
    quote.pairs <- split(quote.pos,cumsum(rep(1:0,length.out=length(quote.pos))))
    within.quotes <- sapply(quote.pairs,function(y) substr(x,y[1]+1,y[2]-1))
    if (convert.text.quotes) {
      within.quotes <- gsub("\\\\\"","\\\"",within.quotes)
    }
    return(within.quotes)
  })
  return(quote.list)
}




fit <- function(x,width=100,copy.clipboard=TRUE) {
  #width=100;copy.clipboard=TRUE
  cr.pattern <- "(?<!\\\\)\\n"
  multi.line <- grepl(cr.pattern,x,perl=TRUE)
  if (multi.line) {
    lines <- str_split(x,cr.pattern)[[1]]
    out <- paste(sapply(lines,function(x) fit(x,width=width,copy.clipboard=FALSE)),collapse="\n")
  } else {
    #find quotes (cannot be preceded by backslash)
    if (nchar(x)<=width) {
      out <- x
    } else {
      quote.pos <- gregexpr("(?<!\\\\)\\\"",x,perl=TRUE)[[1]]
      if (quote.pos[1]==-1) {
        within.quotes <- NULL
      } else {
        if (length(quote.pos) %% 2!=0) stop("YTError: Found an odd number of quotes in this character string:\n",x)
        quote.pairs <- split(quote.pos,cumsum(rep(1:0,length.out=length(quote.pos))))
        #mark characters that are within quotes.
        within.quotes <- lapply(quote.pairs,function(x) x[1]:x[2])
        within.quotes <- stack(within.quotes)$values
      }
      comma.pos <- gregexpr(",",x)[[1]]
      if (comma.pos[1]==-1) {
        out <- x
      } else {
        valid.cr.pos <- setdiff(comma.pos,within.quotes)
        min.valid.cr.pos <- min(valid.cr.pos)
        if (min.valid.cr.pos>=width) {
          new.cr <- min.valid.cr.pos
        } else {
          chars <- strsplit(x,"")[[1]]
          cumsum.chars <- cumsum(chars!="\\")
          valid.crs.length <- cumsum.chars[valid.cr.pos]
          new.cr <- max(valid.cr.pos[valid.crs.length<=width])
        }
        first.half <- substr(x,1,new.cr)
        second.half <- substr(x,new.cr+1,nchar(x))
        out <- paste(first.half,fit(second.half,width=width,copy.clipboard=FALSE),sep="\n")
      }
    }
  }
  if (copy.clipboard) {
    copy.to.clipboard(out)
  }
  return(out)
}


#' Convert vector to SQL code.
#'
#' Produces SQL code for a vector of values.
#'
#' @param x vector to be converted to SQL code.
#' @param copy.clipboard logical, if \code{TRUE}, will copy the SQL code to the Clipboard.
#' @return Returns the SQL code.
#' @examples
#' values <- c("35171234",""35507574)
#' copy.as.sql(values)
#' @author Ying Taur
#' @export
copy.as.sql <- function(x,copy.clipboard=TRUE,fit=TRUE,width=getOption("width")-15) {
  #converts x to R-code.
  if (is.vector(x)) {
    x <- as.character(x)
    sql <- paste0("(",paste0("'",x,"'",collapse=","),")")
    if (fit) {
      sql <- fit(sql,width=width,copy.clipboard=FALSE)
    }
  } else if (is.data.frame(x)) {
    #   select '12345678' as mrn, 12 as number, '2016-01-01' as trans_dte
    #   from idb.oms_ord_catalog where OOC_MSTR_ITEM_GUID = '1000001000074005'
    #   union all
    #   select '12345679' as mrn, 13 as number, '2016-01-01' as trans_dte
    #   from idb.oms_ord_catalog where OOC_MSTR_ITEM_GUID = '1000001000074005'
    #   union all
    #   select '12345668' as mrn, 12 as number, '2016-01-01' as trans_dte
    #   from idb.oms_ord_catalog where OOC_MSTR_ITEM_GUID = '1000001000074005'
    #   union all
    #   select '12345448' as mrn, 14 as number, '2016-01-01' as trans_dte
    #   from idb.oms_ord_catalog where OOC_MSTR_ITEM_GUID = '1000001000074005'

    #add quotations if necessary
    format.value <- function(col) {
      if (is.numeric(col)) {
        newcol <- as.character(col)
      } else {
        newcol <- paste0("'",as.character(col),"'")
      }
      return(newcol)
    }
    data2 <- mutate_each(x,funs(format.value))
    for (var in names(data2)) {
      data2[[var]] <- paste(data2[[var]],"as",var)
    }
    sql.values <- apply(data2,1,function(x) {
      paste(x,collapse=",")
    })
    sql <- paste("select",sql.values,"from idb.oms_ord_catalog where OOC_MSTR_ITEM_GUID = '1000001000074005'",collapse="\nunion all\n")
  }
  if (copy.clipboard) {
    copy.to.clipboard(sql)
  }
  return(sql)
}


makesql <- function(data) {

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

#' Display values for ggplot's shape aesthetic
#'
#' Used for quick reference
#' @export
show_shapes <- function() {
  d=data.frame(p=c(0:25,32:127))
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
  d=data.frame(lt=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"))
  ggplot() +
    scale_x_continuous(name="", limits=c(0,1), breaks=NULL) +
    scale_y_discrete(name="linetype") +
    scale_linetype_identity() +
    geom_segment(data=d, mapping=aes(x=0, xend=1, y=lt, yend=lt, linetype=lt))
}


#' Stack and line up ggplot objects in a column
#'
#' Use this to arrange ggplot objects, where the axes, plot, and legend are lined up correctly.
#'
#' Performs these steps:
#' (1) change margins so that plots are closer together
#' (2) alters widths of each component so that the plots will line up nicely
#' (3) calls \code{grid.arrange(...,ncol=1)}
#' If a \code{NULL} value is passed to the plot list, that plot and the corresponding height value will be omitted.
#' @param ...
#' @param heights a numeric vector representing the relative height of each plot. Passed directly to \code{grid.arrange}.
#' @param gg.extras a list of ggplot objects that will be applied to all plots. Default is \code{NULL}.
#' @param gap size of gap between stacked plots. Default is 0
#' @param margin size of the margin around the plots. Default is 5.5.
#' @param units specifies units used for gap and margin. Default is "pt"
#' @param newpage logical, whether or not to erase current grid device. Default is TRUE. (Note, should turn this off if using in a shiny plot)
#' @return plot of stacked ggplots
#' @export
gg.stack <- function(...,heights = NULL,gg.extras=NULL,gap=0,margin=5.5,units="pt",newpage=TRUE) {
  # g1 <- ggplot(mtcars,aes(x=mpg,y=disp)) + facet_grid(cyl~am) + geom_point()
  # g2 <- ggplot(mtcars,aes(x=mpg,y=disp)) + facet_grid(cyl~am,space="free",scales="free") + geom_point()
  # grobs=list(g1,g2,gt,gm)
  #grobs=list(gm,gt)
  #heights = c(1,2,3,4);gg.extras=NULL;gap=0;margin=5.5;units="pt";newpage=TRUE
  grobs <- list(...)
  if (length(grobs)!=length(heights)) {
    stop("YTError: number of grobs does not match the number of heights.")
  }
  keep <- !sapply(grobs,is.null)
  grobs <- grobs[keep]
  heights <- heights[keep]

  length.grobs <- length(grobs)
  if (length.grobs<=1) {
    stop("YTError: should have at least 2 grobs")
  }
  if (is.null(heights)) {
    heights <- rep(1,length.grobs)
  }
  top.theme <- theme(plot.margin=unit(c(margin, margin, gap, margin),units),
                     axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
  middle.theme <- theme(plot.margin=unit(c(gap, margin, gap, margin),units),
                        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
  bottom.theme <- theme(plot.margin=unit(c(gap, margin, margin, margin),units))
  g.top <- grobs[[1]] + top.theme + gg.extras
  g.middle.list <- lapply(grobs[c(-1,-length.grobs)],function(g) {
    g + middle.theme + gg.extras
  })
  g.bottom <- grobs[[length.grobs]] + bottom.theme + gg.extras

  grobs1 <- c(list(g.top),g.middle.list,list(g.bottom))
  #list of ggplotGrobs
  grobs2 <- lapply(grobs1,function(g) {
    gr <- ggplotGrob(g)
  })
  #get max number of columns for each ggplot
  nwidths <- max(sapply(grobs2,function(g) length(g$width)))
  #if a plot has fewer columns, add null columns to the end.
  grobs3 <- lapply(grobs2,function(g) {
    columns.needed <- nwidths - length(g$widths)
    if (columns.needed>0) {
      for (x in 1:columns.needed) {
        g <- gtable_add_cols(g,unit(1,"null"))
      }
    }
    return(g)
  })

  #normalize null heights to 1 within each plot. should be able to handle facets with varying heights.
  #then alter heights of null portion of each plot.
  grobs4 <- mapply(function(gr,ht) {
    #gr=grobs3[[2]]
    #gr$heights
    ht.char <- as.character(gr$heights)
    null.heights <- grep("null",ht.char)
    relative.heights <- as.numeric(sub("null","",ht.char[null.heights]))
    total.null.height <- sum(relative.heights)
    gr$heights[null.heights] <- gr$heights[null.heights] * (1/total.null.height) * ht
    return(gr)
  },grobs3,heights)

  args <- c(grobs4,list(size="max"))
  gtable.final <- do.call(rbind,args)
  if (newpage) {
    grid.newpage()
  }
  grid.draw(gtable.final)
}

#' Age in years
#'
#' Calculates REAL age.
#'
#' Some people calculate age by taking age in days and dividing by 365.25. That approach can be inaccurate
#' because of leap years. That's weak!
#'
#' @param bdate \code{Date}, vector of birthdays.
#' @param now, \code{Date}, vector representing the time by which to calculate age.
#' @return Returns the age in years.
#' @examples
#' year(as.Date("1975-02-21"))
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


#' Make Table
#'
#' Creates a summary table (data frame) variables from the data.
#'
#' This was written to create a "Table 1" of a manuscript.
#' @param data Data frame containing data to be described.
#' @param vars character vector of variables within \code{data} to be summarized.
#' @param by Optional, variable name (character) by which to summarize the data. Each separate value will be a column of data in the table.
#' @param showdenom logical, whether to show denominator in the cells.
#' @param fisher.test fisher logical, whether or not to calculate Fisher exact tests. Only performed if \code{by} is also specified.
#' @return Returns a data frame formatted to be summary table.
#' @examples
#' make.table(mtcars,c("cyl","gear"))
#' make.table(mtcars,c("cyl","gear"),by="vs",showdenom=TRUE)
#' @author Ying Taur
#' @export
make.table <- function(data,vars,by=NULL,showdenom=FALSE,fisher.test=TRUE) {
  #data=pt80;vars=xvars;by="invsimpson.group";showdenom=F;fisher.test=T
  #data=p;vars=c("cdi.1y","cdi.ever");by=NULL;showdenom=FALSE;fisher.test=TRUE
  #data=cdi.all;vars="multi.cdi";by="first.cdi.pre";showdenom=FALSE;fisher.test=TRUE
  all.vars <- c(vars,by)
  if (any(all.vars %!in% names(data))) {stop("YTError, variable not found in data frame: ",paste(setdiff(c(vars,by),names(data)),collapse=", "))}
  data <- data[,all.vars,drop=FALSE]
  if (!is.null(by)) {
    if (by %in% vars) {stop("YTError, by-variable cannot be in vars list!")}
  }
  factorize <- function(x,ifany=TRUE,as.string=TRUE) {
    if (!is.factor(x)) {x <- factor(x)}
    if (ifany & !any(is.na(x))) {return(x)}
    ll <- levels(x)
    if (!any(is.na(ll))) {ll <- c(ll, NA)}
    x <- factor(x, levels = ll, exclude = NULL)
    if(as.string) {levels(x)[is.na(levels(x))] <- "NA"}
    return(x)
  }
  data <- data %>% mutate_each_(funs(factorize),c(vars,by))
  get.column <- function(subdata) {
    #subdata=data
    denom <- nrow(subdata)
    subtbl <- adply(vars,1,function(var) {
      subdata %>% group_by_(value=var) %>% tally() %>% complete(value,fill=list(n=0)) %>%
        mutate(var=var,value=ifelse(!is.na(value),as.character(value),"NA"),denom=denom,pct=n/denom)
    },.id=NULL)
    if (showdenom) {
      subtbl <- subtbl %>% mutate(lbl=paste0(n,"/",denom," (",percent(pct),")"))
    } else {
      subtbl <- subtbl %>% mutate(lbl=paste0(n," (",percent(pct),")"))
    }
    #combine var and value pairs. the combined variable is saved as factor to preserve the order during spread
    subtbl <- subtbl %>% unite(var_value,var,value,sep="==") %>% mutate(var_value=factor(var_value,levels=var_value))
    return(subtbl)
  }
  tbl <- get.column(data) %>% mutate(column="total")
  if (!is.null(by)) {
    #run get.column function for each subgroup
    sub.tbl <- data %>% group_by_(column=by) %>% do(get.column(.)) %>% ungroup()
    #recode subgroup values to include variable name
    levels(sub.tbl$column) <- paste0(by,"=",levels(sub.tbl$column))
    #combines total and subgroups. use factor levels to preserve subgroup order when spread is performed.
    tbl <- tbl %>% bind_rows(sub.tbl) %>%
      mutate(column=factor(column,levels=c("var","value",levels(sub.tbl$column),"total")))
  }
  #reshape into final columns using spread command. then re-separate the var_value into separate variables
  tbl.all <- tbl %>% dplyr::select(var_value,column,lbl) %>% spread(column,lbl) %>% separate(var_value,c("var","value"),sep="==")

  if (fisher.test & !is.null(by)) {
    fisher.pval <- sapply(vars,function(var) {
      ftest <- fisher.test(data[[var]],data[[by]])
      ftest$p.value
    })
    tbl.all$fisher <- ""
    tbl.all$fisher[match(names(fisher.pval),tbl.all$var)] <- formatC(fisher.pval,format="f",digits=3)
  }
  return(tbl.all)
}


#' @export
trim <- function(x,...) UseMethod("trim")
#' @export
trim.default <- function(string) {
  #removes whitespaces from string.
  gsub("(^ +)|( +$)", "",string)
  #str_trim(string)
}
#' @export
trim.data.frame <- function(data,verbose=TRUE) {
  strvars <- sapply(data,is.character)
  data[,strvars] <- sapply(data[,strvars],trim)
  if (verbose) {
    msg <- paste0("trim: looked through ",sum(strvars)," character variables to trim.")
    message(msg)
  }
  return(data)
}

#' @export
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
  #vec=data$Entered.Date.and.Time
  if (is.POSIXt(vec)) {
    if (all(format(vec,"%H:%M:%S")=="00:00:00")) {
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
#' @param date Date object
#' @param time character with time in it
#' @return Returns POSIXct object with date and time combined.
#' @examples
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
  return(dt)
}



#' Extract Time
#'
#' @param datetime POSIXct object
#' @param format character parameter for formatting of time. Default is "%I:%m%p" (e.g. 10:30AM)
#' @return character with time component
#' @export
get.time <- function(datetime,format="%I:%m%p") {

  if (!is.POSIXt(datetime)) {
    stop("YTError: datetime is not a POSIX date-time!")
  }
  format(datetime,format)
}



#' All Grepl
#'
#' Equivalent to \code{all(grepl(...))}. Performs an initial screen of first 10000 values, to save time.
#'
#' This is used in \code{as.Date2} function to save time on pattern matching.
#' @export
all.grepl <- function(pattern, x, n.screen=10000, ... ) {
  if (length(x) > n.screen) {
    x.screen <- x[1:n.screen]
    if (!all(grepl(pattern, x.screen, ... ))) {
      return(FALSE)
    }
  }
  all(grepl(pattern, x, ... ))
}


#' Search and Convert Date variables
#'
#' In a given data frame, look for variables resembling dates and convert them to Dates.
#'
#' Basically applies \code{as.Date2} to all variables.
#' @param data The data frame to be converted.
#' @param verbose logical indicating whether or not to display info on date conversions. Default is \code{FALSE}.
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
#' Note that many numeric vectors \emph{could} be an MRN. Will issue a warning if the variable
#' passes the tests but is not necessarily an MRN variable.
#' @param mrn vector to be examined.
#' @param like logical, whether or not to check if the variable \emph{could} be an MRN. If \code{FALSE}, it checks strictly. If \code{TRUE}, will allow for classes other than character, and for whitespaces.
#' @return Returns logical stating whether or not this variable is an MRN.
#' @examples
#' asdf
#' @author Ying Taur
#' @export
is.mrn <- function(mrn,like=FALSE) {
  if (like) { #if numeric or factor, and if leading zeroes gone, can still hit.
    mrn <- as.character(trim(mrn))
    mrn <- mrn[!is.na(mrn) & mrn!=""]
    #answer <- all(grepl("^[03][0-9]{7}$|^[0-9]{1,7}$",mrn))
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
  mrn <- trim(as.character(mrn))
  mrn[mrn==""] <- NA
  mrn <- sapply(mrn,function(x) {
    if (is.na(x)) {
      NA_character_
    } else {
      paste(c(rep("0",8-nchar(x)),x),collapse="")
    }
  })
  return(mrn)
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
#' @param verbose logical indicating whether or not to display info on columns removed. Default is \code{FALSE}.
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
#' @param verbose logical indicating whether or not to display info on rows removed. Default is \code{FALSE}.
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
#' Like original \code{make.names}, but gets rid of any repeating periods('.'), as well as periods at the end. This is just an aesthetic modification.
#' @param names character vector to be coerced to syntactically valid names. This is coerced to character if necessary.
#' @param verbose logical indicating whether or not to display info name cleanup. Default is \code{FALSE}. Dataframe only
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



#' Cleanup data
#'
#' Cleans up a data frame by performing 5 tasks:
#' (1) Remove any column or row that is all \code{NA} values (\code{remove.na.rows},\code{remove.na.cols})
#' (2) Make column names well-formatted (\code{make.names})
#' (3) Remove any leading or trailing whitespace from character variables (\code{trim})
#' (4) Look for variables that look like date/time variables, and convert them to Date or POSIXct format (\code{convert.dates})
#' (5) Look for variables that look like MRNs and format them properly (\code{as.mrn})
#'
#' @param remove.na.cols If \code{TRUE}, will remove any column consisting entirely of \code{NA}'s. Default=\code{FALSE}
#' @param remove.na.rows If \code{TRUE}, will remove any row consisting entirely of \code{NA}'s. Default=\code{TRUE}
#' @param make.names If \code{TRUE}, will fix variable names. Default=\code{TRUE}
#' @param trim If \code{TRUE}, will remove whitespace from all character variables. Default=\code{TRUE}
#' @param convert.dates If \code{TRUE}, will convert variables that look like dates to Date format. Default=\code{TRUE}
#' @param as.mrn If \code{TRUE}, will looking for variables that look like MRN and convert to 8-digit character. Default=\code{TRUE}
#' @param verbose logical indicating whether or not to display info on data cleaning. Default is \code{FALSE}.
#' @return Returns a clean version of \code{data}.
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


#' Cleanup data (OLD)
#'
#' Cleans up a data frame by performing 4 tasks:
#' (1) \code{trim}: for all character variables, remove whitespace
#' (2) \code{convert.dates}: for all character variables, remove whitespace
#' (3) \code{as.mrn}: will properly format any MRN variable.
#' (4) Remove any column or row that is all \code{NA} values.
#'
#' @param make.names If \code{TRUE}, will fix variable names. Default=\code{TRUE}
#' @param trim If \code{TRUE}, will remove whitespace from all character variables. Default=\code{TRUE}
#' @param convert.dates If \code{TRUE}, will convert variables that look like dates to Date format. Default=\code{TRUE}
#' @param as.mrn If \code{TRUE}, will looking for variables that look like MRN and convert to 8-digit character. Default=\code{TRUE}
#' @param remove.na.cols.rows If \code{TRUE}, will remove any column or row consisting entirely of \code{NA}'s. Default=\code{FALSE}
#' @return Returns a clean version of \code{data}.
#' @examples
#' #####
#' @author Ying Taur
#' @export
cleanup.data.old <- function(data,make.names=TRUE,trim=TRUE,convert.dates=TRUE,as.mrn=TRUE,remove.na.cols.rows=FALSE) {
  #data=d;make.names=TRUE;trim=TRUE;convert.dates=TRUE;as.mrn=TRUE;remove.na.cols.rows=FALSE
  #data=diet;make.names=TRUE;trim=TRUE;convert.dates=TRUE;as.mrn=TRUE;remove.na.cols.rows=FALSE
  if (remove.na.cols.rows) {
    data <- remove.na.cols.rows(data)
  }
  if (make.names) {
    #names(data) <- make.names(names(data))
    setnames(data,names(data),make.names(names(data),unique=TRUE))
  }
  if (trim) {
    data <- trim(data) #gets rid of whitespace
  }
  if (convert.dates) {
    data <- convert.dates(data)
  }
  if (as.mrn) {
    data <- as.mrn(data)
  }
  return(data)
}



#' Ying's Recode
#'
#' Recode a variable
#'
#' @param var the vector to be recoded.
#' @param recodes typically, a named vector specifying recodes. Note that order matters; first matching recode will apply
#' (assuming \code{multi.hits=FALSE}). As an additional option, a named list of vectors can also be used, where each vector of
#' values will be recoded to its corresponding name.
#' @param else.value the value to be used if a value is not recoded. Default is the old value.
#' @param as.factor whether or not to code as a factor. The levels will ordered based on \code{recodes}. Default is to base it on whether original vector is a factor.
#' @param regexp if \code{TRUE}, use regular expressions. Default is \code{FALSE}, which performs exact matching.
#' @param replace if \code{TRUE}, replace the hit (using \code{gsub})instead of replacing the entire field. Note that \code{regexp} and \code{multi.hits} should be \code{TRUE}, if not it will be changed. Default is \code{FALSE}.
#' @param multi.hits if \code{TRUE}, will evaluate every value for every recode. So values can be recoded more than one time.
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is \code{TRUE}
#' @param perl whether to use perl-style regular expressions. Default is \code{FALSE}
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for \code{grep})
#' @return A vector consisting of the recoded values of \code{var}
#' @examples
#' data(cid)
#'
#' # Recode if field matches exactly.
#' recodes1 <- c("Pseudomonas aeruginosa"="P. aeruginosa","Staphylococcus aureus oxacillin resistant"="MRSA")
#' bsi$org.short.1 <- recode2(bsi$org.short,recodes1)
#' bsi$org.short.1
#'
#' # Recode if there is a regular expression pattern match.
#' recodes2 <- c("Pseud.+aerug"="P. aeruginosa","oxacill.+resist"="MRSA")
#' bsi$org.short.2 <- recode2(bsi$org.short,recodes2,regexp=TRUE)
#' bsi$org.short.2
#'
#' # Instead of recoding, find and replace text.
#' recodes3 <- c("Pseudomonas"="P.","Staphylococcus"="S.")
#' bsi$org.short.3 <- recode2(bsi$org.short,recodes3,replace=TRUE,multi.hits=TRUE)
#' bsi$org.short.3
#'
#' # Recode via regular expressions to merge groups.
#' recodes4 <- c("enterococcus|staph|streptococcus|cnst|vre"="gram positive",
#'               "klebs|coli|serrat|pseudo|steno|citro|acinet|enterobact"="gram negative")
#' bsi$org.short.4 <- recode2(bsi$org.short,recodes4,regexp=TRUE,else.value="Other Bacteria")
#' bsi$org.short.4
#'
#' # if recodes are in list object, vectors of possible hits can be listed. This will do the same thing as #4.
#' recodes5 <- list("gram positive"=c("entero","staph","strep","cnst","vre"),
#'                  "gram negative"=c("klebs","coli","serrat","pseudo","steno","citro","acinet","enterobact"))
#' bsi$org.short.5 <- recode2(bsi$org.short,recodes5,regexp=TRUE,else.value="Other Bacteria")
#' bsi$org.short.5
#' @author Ying Taur
#' @export
recode2 <- function(var,recodes,else.value,as.factor,regexp=FALSE,replace=FALSE,multi.hits=FALSE,ignore.case=TRUE,perl=FALSE,useBytes=TRUE) {
  #var=met.meds$GENERIC_DRUG_NAME;regexp=TRUE;replace=FALSE;multi.hits=F;ignore.case=T;perl=F;useBytes=T;recodes=med.class.recodes;else.value="Other"
  if (is.null(names(recodes))) {
    stop("Variable recodes needs to be a named vector or list")
  }
  if (replace & (!multi.hits | !regexp)) {
    warning("You specified replace=TRUE, setting regexp and multi.hits to TRUE.")
    multi.hits <- TRUE
    regexp <- TRUE
  }
  var.recode <- rep(NA,length(var)) #create NA vector
  if (is.list(recodes)) { #if recodes is list, convert it to vector
    recodes <- unlist(lapply(names(recodes),function(n) {
      re <- recodes[[n]]
      structure(rep(n,length(re)),names=re)
    }))
  }
  for (i in 1:length(recodes)) {
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
      var.recode <- ifelse(is.na(var.recode),var,var.recode) #should only run first time through
      var.recode[evals][hit] <- gsub(pattern,newname,var.recode[evals][hit],ignore.case=ignore.case,perl=perl,useBytes=useBytes)
    } else { #normal replacing
      var.recode[evals][hit] <- newname
    }
  }
  # names(var.recode) <- var #add names to result.
  #handling non-matches
  if (missing(else.value)) {
    #default is to use old value
    var.recode <- ifelse(is.na(var.recode),var,var.recode)
  } else {
    var.recode <- ifelse(is.na(var.recode),else.value,var.recode)
  }
  if (missing(as.factor)) {
    as.factor <- is.factor(var)
  }
  if (as.factor) {
    var.recode.levels <- c(unique(recodes),else.value)
    var.recode <- factor(var.recode,levels=var.recode.levels)
  }
  return(var.recode)
}


#' @rdname recode2
#' @export
recode.grep <- function(...) {
  recode2(regexp=TRUE,...)
  #var,recodes,else.value,as.factor,regexp=FALSE,replace=FALSE,multi.hits=FALSE,ignore.case=TRUE,perl=FALSE,useBytes=TRUE
}




#' Ying's Replace Grep
#'
#' Perform multiple text replacements at once using regular expressions. Similar in form to \code{recode2} and \code{recode.grep}.
#'
#' @param var the character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, the names are the regular expression, and the value is the replacement text.
#' @param result.as.list if \code{TRUE}, returns a 2-vector list containing replaced text and text hits. Default is \code{FALSE}.
#' @param replace.text text to replace hits with. Default is \code{""}
#' @param collapse.hits the separator with which all hits are pasted together. If \code{NULL}, hits will remain as an uncollapsed list. Default is \code{"|"}. Note that this parameter is not relevant unless \code{result.as.list=TRUE}
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is \code{TRUE}
#' @param perl whether to use perl-style regular expressions. Default is \code{TRUE}
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for \code{grep})
#' @return By default, returns \code{var}, but with all regular expression hits replaced. If \code{result.as.list=TRUE} is specified, the hits themselves are also returned, within a 2-vector list.
#' @export
replace.grep <- function(var,recodes,result.as.list=FALSE,replace.text="",collapse.hits="|",ignore.case=TRUE,perl=TRUE,useBytes=TRUE) {
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
  if (!is.null(collapse.hits)) {
    hits <- sapply(hits,function(x) paste(x,collapse=collapse.hits))
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
#' Uses \code{replace.grep}.
#'
#' @param data the data frame to be manipulated.
#' @param var the bare character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, the names are the regular expression, and the value is the replacement text.
#' @param newvar bare name of column to hold the replaced version of \code{var}. If \code{NULL} (default), \code{var} will be overwritten.
#' @param replace.text text to replace hits with. Default is \code{""}
#' @param hits.var bare name of column to hold the text hits. If \code{NULL} (default), hits are not stored.
#' @param collapse.hits the separator with which all hits are pasted together. If \code{NULL}, hits will remain as an uncollapsed list. Default is \code{"|"}. Note that this parameter is not relevant unless \code{hits.var} is specified.
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is \code{TRUE}
#' @param perl whether to use perl-style regular expressions. Default is \code{TRUE}
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for \code{grep})
#' @return By default, returns \code{var}, but with all regular expression hits replaced. If \code{result.as.list=TRUE} is specified, the hits themselves are also returned, within a 2-vector list.
#' @export
replace.grep.data <- function(data,var,recodes,newvar=NULL,replace.text="",hits.var=NULL,collapse.hits="|",ignore.case=TRUE,perl=TRUE,useBytes=TRUE) {
  newvar <- as.character(substitute(newvar))
  hits.var <- as.character(substitute(hits.var))
  if (length(newvar)==0) {
    newvar <- as.character(substitute(var))
  }
  store.hits <- length(hits.var)>0
  oldvar <- lazyeval::lazy_eval(lazyeval::lazy(var),data)
  results <- replace.grep(var=oldvar,recodes=recodes,result.as.list=store.hits,
                          replace.text=replace.text,collapse.hits=collapse.hits,ignore.case=ignore.case,perl=perl,useBytes=useBytes)
  if (store.hits) {
    newdata <- data
    newdata[[newvar]] <- results$replace.text
    newdata[[hits.var]] <- results$hits
  } else {
    newdata <- data
    newdata[[newvar]] <- results$replace.text
    newdata[[hits.var]] <- results$hits
  }
  return(newdata)
}







#' Find All Distinct Variables
#'
#' Find Distinct
#'
#' @param data Dataframe to be analyzed
#' @param ... grouping variables that define data units.
#' @return prints whether variables matching the groups or not.
#' @export
find.all.distinct.vars <- function(data, ...) {
  args <- lazyeval::lazy_dots(...)
  group.vars <- unname(sapply(args, function(x) deparse(x$expr)))
  other.vars <- setdiff(names(data),group.vars)
  data2 <- data %>% group_by(...) %>% summarize_each(funs(n_distinct)) %>% ungroup()
  allone <- function(x) {
    all(x==1)
  }
  data3 <- data2 %>% select_(.dots=other.vars) %>% summarize_each(funs(allone))
  distinct.vars <- names(data3)[t(data3)]
  non.distinct.vars <- names(data3)[!t(data3)]
  distinct.vars.text <- paste0("[",paste(group.vars,collapse=","),"],",paste(distinct.vars,collapse=","))
  non.distinct.vars.text <- paste0(non.distinct.vars,collapse=",")
  message("distinct: ",distinct.vars.text,"\nnot distinct: ",non.distinct.vars.text,"\n")
}


#' Read Excel File 2
#'
#' Same as \code{readxl::read_excel} function, but col_types can be named vector
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
  xl <- read_excel(path=path,sheet=sheet,col_names=col_names,col_types=col_types,na=na,skip=skip)
  return(xl)
}


#' Send a Text Message
#'
#' Send a text message to your phone!
#'
#' @param message A string containing the message you want to send.
#' @param number The phone number you want to send the message to. Default is Ying's cell number.
#' @return No value.
#' @examples
#' send.text.message("Hello, this is a text message")
#' @author Ying Taur
#' @export
send.text.message <- function(message,number="9149802489") {
  if (Sys.info()["sysname"]=="Linux") {
    system(paste0("curl http://textbelt.com/text -d number=",number," -d \"message=",message,"\""))
  } else {
    stop("YTError: Not sure how to handle this operating system: ",Sys.info()["sysname"],"\nGo tell Ying about this.")
  }
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
#' str_extract(text,middle.pattern("start",".+","end"))
#' str_extract(text,middle.pattern("start",".+"))
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


#' Fill in Blanks
#'
#' For a given vector,  in blanks with previous value.
#' @param vec the vector to be ed in.
#' @param blank vector of values that denote a blank. By default, \code{""} is used.
#' @param include.na vector of values that denote a blank. By default, \code{""} is used.
#' @return Returns \code{vec}, with blanks filled in.
#' @examples
#' fill.in.blanks(c("1",NA,"2","","3","","","4",NA,NA))
#' @author Ying Taur
#' @export
fill.in.blanks <- function(vec,blank="",include.na=TRUE) {
  if (include.na) {
    non.blanks <- !is.na(vec) & !(vec %in% blank)
  } else {
    non.blanks <- !(vec %in% blank)
  }
  c(vec[non.blanks][1], vec[non.blanks])[cumsum(non.blanks)+1]
}

#Imports: ape,phytools,dplyr,ggtree,phyloseq,ggplot2,scales,colorspace,stringr,logistf,coxphf,reshape2,lubridate,plyr,tidyr,data.table,Hmisc,readxl

#' Determines if tstart-tstop occurs anywhere within interval.
#' @export
occurs.within <- function(tstart,tstop,start.interval,stop.interval) {
  tstop>=start.interval & stop.interval>=tstart
}


#' @export
chop.endpoint <- function(data,newvar,oldvar, ...) {
  #create a new endpoint where multiple timepoints of censoring can be specified.
  listargs <- lapply(list(...),function(x) {
    if (is.character(x)) {
      x <- data[,x]
    }
    return(x)
  })
  chop.time <- do.call(min,listargs)
  newvar_day <- paste0(newvar,"_day")
  oldvar_day <- paste0(oldvar,"_day")
  data[,newvar] <- ifelse(chop.time<data[,oldvar_day],FALSE,data[,oldvar])
  data[,newvar_day] <- ifelse(chop.time<data[,oldvar_day],chop.time,data[,oldvar_day])
  return(data)
}



#' Cox Proportional Hazards Regression
#'
#' Analyzes survival data by Cox regression.
#'
#' Convenience function for survival analysis. Typically uses the \code{coxphf} function.
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
stcox <- function( ... ,starttime="tstart",data,addto,as.survfit=FALSE,firth=TRUE,formatted=TRUE,logrank=FALSE,coxphf.obj=FALSE) {
  y <- c(...)[1]
  xvars <- c(...)[-1]
  y.day <- paste0(y,"_day")
  #xvars that are time-dependent
  td.xvars <- xvars[paste(xvars,"_day",sep="") %in% names(data)]
  if (length(td.xvars)>0) {
    td.xvars.day <- paste(td.xvars,"_day",sep="")
  } else {
    td.xvars.day <- NULL
  }
  all.vars <- unique(c(y,y.day,xvars,td.xvars.day,starttime))
  #if data is large, this speeds up a lot
  data <- data[,all.vars]
  #define y, s.start and s.stop
  data$y <- data[,y]
  data$s.start <- data[,starttime]
  data$s.stop <- data[,y.day]
  data <- subset(data,s.start<s.stop)
  #### check for missing x values.
  for (x in xvars) {
    missing.x <- is.na(data[,x])
    if (any(missing.x)) {
      print(paste0("  NOTE - missing values in ",x,", removing ",length(sum(missing.x))," values."))
      data <- data[!is.na(data[,x]),]
    }
  }
  splitline <- function(x) {
    #time dep xvars where x=1 (xvar.split are vars from td.xvars where splitting needs to be done)
    xvar.split <- td.xvars[c(x[,td.xvars]==1)]
    xvarday.split <- td.xvars.day[c(x[,td.xvars]==1)]
    if (length(xvar.split)==0) {
      return(x)
    } else {
      #cutpoints=time dep where xvars=1, and are between s.start and s.stop
      cutpoints <- unlist(c(x[,xvarday.split])) #days at which an xvar==1
      cutpoints <- cutpoints[x$s.start<cutpoints & cutpoints<x$s.stop] #eliminate days on s.start or s.stop
      cutpoints <- unique(cutpoints)
      #sort cutpoints and add s.start and s.stop at ends.
      cutpoints <- c(x$s.start,cutpoints[order(cutpoints)],x$s.stop)
      #set up new.x, with s.start and s.stop, y=0 by default
      new.x <- data.frame(s.start=cutpoints[-length(cutpoints)],s.stop=cutpoints[-1],y=0)
      #the last row of new.x takes value of y, the rest are y=0
      new.x[nrow(new.x),"y"] <- x$y
      #determine values of time dep x's (td.xvars) for each time interval. first, x=0 by default
      new.x[,td.xvars] <- 0
      for (i in 1:length(xvar.split)) {
        xvar <- xvar.split[i]
        xvarday <- xvarday.split[i]
        new.x[new.x$s.start>=x[,xvarday.split[i]],xvar.split[i]] <- 1
      }
      remaining.vars <- setdiff(names(x),names(new.x))
      new.x[,remaining.vars] <- x[,remaining.vars]
      return(new.x)
    }
  }
  if (length(td.xvars)>0) {
    data <- adply(data,1,splitline)
  }
  #calculate model
  leftside <- "Surv(s.start,s.stop,y)"
  rightside <- paste(xvars,collapse=" + ")
  model <- paste(leftside,rightside,sep=" ~ ")
  formula <- as.formula(model)
  if (as.survfit) {
    #return a survfit object
    return(survfit(formula,data=data))
  } else if (logrank) {
    #output logrank test
    results <- summary(coxph(formula,data=data))
    return(results$logtest[3])
  } else {
    results <- coxphf(formula,data=data,firth=firth)
    if (coxphf.obj) {
      return(results)
    }
    results.table <- data.frame(
      model=model,
      yvar=y,
      xvar=names(results$coefficients),
      haz.ratio=exp(results$coefficients),
      lower.ci=results$ci.lower,
      upper.ci=results$ci.upper,
      p.value=results$prob,
      row.names=NULL,stringsAsFactors=FALSE)
    #mark time-dependent xvars with "(td)". note that this just looks for var in td.xvars;
    #if variable has level specifications, then it won't work. (could fix this with reg expr instead)
    if (length(td.xvars)>0) {
      results.table$xvar[results.table$xvar %in% td.xvars] <- sapply(results.table$xvar[results.table$xvar %in% td.xvars],function(x) paste0(x,"(td)"))
    }
    if (formatted) {
      results.table$signif <- as.character(cut(results.table$p.value,breaks=c(-Inf,0.05,0.20,Inf),labels=c("****","*","-")))
      results.table$haz.ratio <- format(round(results.table$haz.ratio,2),nsmall=2)
      results.table$lower.ci <- format(round(results.table$lower.ci,2),nsmall=2)
      results.table$upper.ci <- format(round(results.table$upper.ci,2),nsmall=2)
      results.table$p.value <- format(round(results.table$p.value,3),nsmall=3)
      results.table <- adply(results.table,1,function(x) {
        x$haz.ratio <- paste0(x$haz.ratio," (",x$lower.ci," - ",x$upper.ci,")")
        return(x)
      })
      results.table <- subset(results.table,select=c(model,yvar,xvar,haz.ratio,p.value,signif))
    }
    if (missing(addto)) {
      return(results.table)
    } else {
      return(rbind(addto,results.table))
    }
  }
}




#' ...Title...
#'
#' ...Description...
#'
#' @usage ...usage.code...
#'
#' ...details...
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
univariate.stcox <- function(yvar,xvars,starttime="tstart",data,firth=TRUE,multi=FALSE,multi.cutoff=0.2,referrent=FALSE) {
  results.table <- data.frame()
  for (xvar in xvars) {
    print(xvar)
    results.table <- stcox(yvar,xvar,starttime=starttime,data=data,addto=results.table,firth=firth)
  }
  if (multi) {
    multivars <- results.table$xvar[results.table$p.value<=multi.cutoff]
    multivars <- unique(sub("\\(td\\)$","",multivars))
    multivars <- sapply(multivars,function(x) {
      xvars[sapply(xvars,function(y) {
        y==x | grepl(paste0("^",y),x) & sub(y,"",x) %in% as.character(unique(c(data[,y],levels(data[,y]))))
      })]
    })
    print("Multivariate model:")
    if (length(multivars)>0) {
      print(paste0(multivars))
      multi.table <- stcox(yvar,multivars,starttime=starttime,data=data,firth=firth)
      #names(multi.table) <- car::recode(names(multi.table),"'haz.ratio'='multi.haz.ratio';'p.value'='multi.p.value';'signif'='multi.signif'")
      names(multi.table) <- recode2(names(multi.table),c("haz.ratio"="multi.haz.ratio","p.value"="multi.p.value","signif"="multi.signif"))
      multi.table <- subset(multi.table,select=-model)
      results.table <- subset(results.table,select=-model)
      #combined.table <- merge(results.table,multi.table,all.x=TRUE)
      combined.table <- results.table %>% left_join(multi.table,by=c("yvar","xvar"))
      #sort by original order
      results.table <- combined.table[order(factor(combined.table$xvar,levels=results.table$xvar)),]
      for (v in c("multi.haz.ratio","multi.p.value","multi.signif")) {
        results.table[is.na(results.table[,v]),v] <- ""
      }
      n.events <- sum(data[,yvar])
      n.multivars <- length(unique(multivars))
      print(paste0(n.events/n.multivars," events per multivariable (",n.events,"/",n.multivars,", consider overfitting if less than 10)"))
      #results.table <- data.frame(lapply(results.table,function(x) recode(x,"NA=''")))
    } else {
      print("No variables in multivariate!")
      results.table <- subset(results.table,select=-model)
      results.table$multi.haz.ratio <- ""
      results.table$multi.p.value <- ""
      results.table$multi.signif <- ""
    }
  }
  return(results.table)
}


#' Create Survival Data Frame
#'
#' @param f.survfit the survival data
#' @param time0 time0 can be specified. It must be <= first event
#' @return Returns survival data frame
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @author Ying Taur
#' @export
createSurvivalFrame <- function(f.survfit,time0=0) {
  # define custom function to create a survival data.frame
  #YT edit: time0 can be specified. it must be <= first event. if it isn't use first event as time0
  time0 <- min(time0,f.survfit$time[1])
  # initialise frame variable
  f.frame <- NULL
  # check if more then one strata
  if (length(names(f.survfit$strata))==0) {
    f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower)
    # create first two rows (start at 1)
    f.start <- data.frame(time=c(time0, f.frame$time[1]), n.risk=c(f.survfit$n, f.survfit$n), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1))
    # add first row to dataset
    f.frame <- rbind(f.start, f.frame)
    # remove temporary data
    rm(f.start)
    # create data.frame with data from survfit
  } else { #multiple strata
    # create vector for strata identification
    f.strata <- NULL
    for(f.i in 1:length(f.survfit$strata)){
      # add vector for one strata according to number of rows of strata
      f.strata <- c(f.strata, rep(names(f.survfit$strata)[f.i], f.survfit$strata[f.i]))
    }
    # create data.frame with data from survfit (create column for strata)
    f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit
                          $n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower, strata=factor(f.strata))
    # remove temporary data
    rm(f.strata)
    # create first two rows (start at 1) for each strata
    for(f.i in 1:length(f.survfit$strata)){
      # take only subset for this strata from data
      f.subset <- subset(f.frame, strata==names(f.survfit$strata)[f.i])
      # create first two rows (time: time0, time of first event) YT edit, not time 0 but
      f.start <- data.frame(time=c(time0, f.subset$time[1]), n.risk=rep(f.survfit[f.i]$n, 2), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(f.survfit$strata)[f.i],2))
      # add first two rows to dataset
      f.frame <- rbind(f.start, f.frame)
      # remove temporary data
      rm(f.start, f.subset)
    }
    # reorder data
    f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
    # rename row.names
    rownames(f.frame) <- NULL
  }
  # return frame
  return(f.frame)
}




#' At-Risk Table from Survival Data Frame
#'
#' @param t.breaks vector of times to calculate at-risk
#' @param sf survival frame
#' @param minus.epsilon.last.t means we substract a small amt from last timepoint, because otherwise it's all NA.
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
survival.frame.atrisk.table <- function(t.breaks,sf,minus.epsilon.last.t=TRUE,melt=FALSE,row.name.xloc) {
  #t.breaks=0:3*365;minus.epsilon.last.t=TRUE;melt=TRUE;row.name.xloc=-200
  epsilon <- (max(t.breaks) - min(t.breaks)) / 1000000
  t.breaks2 <- ifelse(t.breaks==max(t.breaks),t.breaks-epsilon,t.breaks)
  tbl <- data.frame(lapply(t.breaks2,function(t) {
    survival.frame.info(t,sf,"n.risk")
  }))
  names(tbl) <- paste0("time.",t.breaks)
  #the factor is to keep factor order same as order of table
  tbl <- data.frame(strata=factor(row.names(tbl),levels=row.names(tbl)),tbl,row.names=NULL)
  if (melt) {
    #default for xlocation
    if (missing(row.name.xloc)) {
      row.name.xloc <- -200
    }
    tbl[,paste0("time.",row.name.xloc)] <- tbl$strata
    measure.vars <- grep("time\\.",names(tbl),value=TRUE)
    atrisk.melt <- melt(tbl,measure.vars=measure.vars)
    atrisk.melt$x <- as.numeric(sub("^time\\.","",atrisk.melt$variable))
    atrisk.melt$y <- as.numeric(atrisk.melt$strata)
    atrisk.melt$label <- atrisk.melt$value
    return(atrisk.melt)
  } else {
    return(tbl)
  }
}

#' Survival Frame Info
#'
#' Given survival.frame, and time, provide info in the survivalframe.
#' @param t time
#' @param sf survival frame
#' @param infotype type of info needed. Needs to be a variable in survival frame.
#' @return info needed from survival frame
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
survival.frame.info <- function(t,sf,infotype) {
  #given survival.frame, and time, provide info in the survivalframe.
  #infotype is the name of variable: "n.risk","surv",etc.
  if (!(infotype %in% names(sf))) {
    print("Error, infotype needs to be a variable in survival.frame")
    return(NULL)
  }
  if (!("strata" %in% names(sf))) {
    sf$strata <- "num.at.risk"
  }
  sf <- sf[order(sf$strata),]
  daply(sf,"strata",function(x) {
    before.times <- x$time<=t
    if (all(before.times)) {
      return(NA)
    } else {
      sub.x <- x[before.times,]
      return(sub.x[order(sub.x$time,decreasing=TRUE)[1],infotype])
    }
  })
}






#' Generate Kaplan-Meier curve in ggplot2
#'
#' Creates a Kaplan-Meier curve which can be used like a geom in ggplot2.
#'
#' Use this to make Kaplan-Meier curves in ggplot2. Utilizes the \code{geom_step} function to draw.
#' \code{yingtools::stcox} function is used to generate data from a survival frame.
#'
#' @param yvar character, used to indicate the variable set to be used as survival endpoint of interest.
#' For example, if \code{yvar="var1"} is specified, then \code{data} should have \code{"var1"} will represent
#' whether the endpoint occurred (logical or 0-1), and \code{"var1_day"} will represent the time at which
#' the event occurred (or didn't occur).
#' @param xvar character, indicating the variable within \code{data} that will contain the groups by which curves will be generated.
#' @param data data frame containing the survival data.
#' @param starttime character, specifying the column within \code{data} that indicates the survival start time.
#' If there is no left censoring, then this would refer to a vector of 0's. Default is \code{"tstart"}
#' @param flip.y logical, indicating whether or not to flip the y-axis. \code{flip.y = FALSE} by default (curve is downwards).
#' @param size line thickness for the survival curve.
#' @return A ggplot2 geom object with Kaplan-Meier curve.
#' @examples
#' ggplot() + geom_kaplanmeier("dead","Intensity",data)
#' @seealso \code{\link{xxxxx}}
#' @author Ying Taur
#' @export
geom_kaplanmeier <- function(yvar,xvar=NULL,data,starttime="tstart",flip.y=FALSE,size=NULL,logrank=FALSE,logrank.xpos,logrank.ypos,logrank.fontsize=5) {
  if (is.null(xvar)) {
    data$one <- 1
    sf <- createSurvivalFrame(stcox(yvar,"one",starttime=starttime,data=data,as.survfit=TRUE))
  } else {
    sf <- createSurvivalFrame(stcox(yvar,xvar,starttime=starttime,data=data,as.survfit=TRUE))
    sf[,xvar] <- sub("^.+=","",sf$strata)
    if (is.factor(data[,xvar])) {
      sf[,xvar] <- factor(sf[,xvar],levels=levels(data[,xvar]))
    }
  }
  if (flip.y) {
    sf$surv <- 1 - sf$surv
  }
  if (is.null(xvar)) {
    if (is.null(size)) {
      g <- geom_step(data=sf,aes_string(x="time",y="surv"))
    } else {
      g <- geom_step(data=sf,aes_string(x="time",y="surv"),size=size)
    }
  } else {
    if (is.null(size)) {
      g <- geom_step(data=sf,aes_string(x="time",y="surv",color=xvar,group=xvar))
    } else {
      g <- geom_step(data=sf,aes_string(x="time",y="surv",color=xvar,group=xvar),size=size)
    }
    if (logrank) {
      g <- list(g,geom_logrank(yvar=yvar,xvar=xvar,data=data,starttime=starttime,x.pos=logrank.xpos,y.pos=logrank.ypos,logrank.fontsize=logrank.fontsize))
    }
  }
  return(g)
}


#' Generate label for Log-rank test results in ggplot2
#'
#' Adds the p-value for a log-rank test to a ggplot2 graph.
#'
#' ...details...
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
geom_logrank <- function(yvar,xvar,data,starttime="tstart",x.pos,y.pos,logrank.fontsize=5) {
  logrank <- stcox(yvar=yvar,yvar=xvar,data=data,starttime=starttime,logrank=TRUE)
  logrank <- paste0("Log-rank\nP = ",formatC(logrank,format="f",digits=3))
  annotate("text",x=x.pos,y=y.pos,label=logrank,size=logrank.fontsize)
}


#' ...Title...
#'
#' ...Description...
#'
#' @usage ...usage.code...
#'
#' ...details...
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
logistic <- function(x,...) UseMethod("logistic")



#' ...Title...
#'
#' ...Description...
#'
#' @usage ...usage.code...
#'
#' ...details...
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
logistic.character <- function( ... ,data,firth=FALSE,formatted=TRUE,digits=3,addto) {
  y <- c(...)[1]
  x <- paste(c(...)[-1],collapse="+")
  model <- paste(y,x,sep="~")
  logistic(as.formula(model),data=data,firth=firth,formatted=formatted,digits=digits,addto=addto)
}

#' ...Title...
#'
#' ...Description...
#'
#' @usage ...usage.code...
#'
#' ...details...
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
logistic.formula <- function( ... ,firth=FALSE,formatted=TRUE,digits=3,addto) {
  results <- logistf( ... , firth=firth)
  results.table <- data.frame(
    model=gsub("\"| ","",paste(deparse(results$formula),collapse="")),
    yvar=as.character(results$formula)[2],
    xvar=results$terms,
    odds.ratio=exp(results$coefficients),
    lower.ci=exp(results$ci.lower),
    upper.ci=exp(results$ci.upper),
    p.value=results$prob,
    row.names=NULL,stringsAsFactors=FALSE)
  #get rid of intercept line, keep above vars only
  results.table <- subset(results.table,xvar!="(Intercept)",select=c(model,yvar,xvar,odds.ratio,lower.ci,upper.ci,p.value))
  if (formatted) {
    results.table$signif <- cut(results.table$p.value,breaks=c(-Inf,0.05,0.20,Inf),labels=c("****","*","-"))
    numvars <- sapply(results.table,is.numeric)
    results.table[,numvars] <- sapply(results.table[,numvars],function(x) formatC(x,format="f",digits=digits))
    results.table <- adply(results.table,1,function(x) {
      x$odds.ratio <- paste0(x$odds.ratio," (",x$lower.ci," - ",x$upper.ci,")")
      return(x)
    })
    results.table <- subset(results.table,select=c(model,yvar,xvar,odds.ratio,p.value,signif))
  }
  if (missing(addto)) {
    return(results.table)
  } else {
    return(rbind(addto,results.table))
  }
}




#' ...Title...
#'
#' ...Description...
#'
#' @usage ...usage.code...
#'
#' ...details...
#'
#' @param .param1. ...param1.description...
#' @param .param2. ...param2.description...
#' @return ...description.of.data.returned...
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @seealso \code{\link{cdiff.method}}
#' @author Ying Taur
#' @export
univariate.logistic <- function(yvar,xvars,data,firth=FALSE,multi=FALSE,multi.cutoff=0.2,formatted=TRUE,digits=3) {
  results.table <- data.frame()
  for (xvar in xvars) {
    print(xvar)
    results.table <- logistic(yvar,xvar,data=data,firth=firth,addto=results.table,formatted=formatted,digits=digits)
  }
  if (multi) {
    multivars <- results.table$xvar[results.table$p.value<=multi.cutoff]
    multivars <- unique(multivars)
    multivars <- sapply(multivars,function(x) {
      xvars[sapply(xvars,function(y) {
        y==x | grepl(paste0("^",y),x) & sub(y,"",x) %in% as.character(unique(c(data[,y],levels(data[,y]))))
      })]
    })
    print("multivariate model: ")
    print(paste0(multivars,collapse=", "))
    multi.table <- logistic(yvar,multivars,data=data,firth=firth,digits=digits)
    names(multi.table) <- car::recode(names(multi.table),"'odds.ratio'='multi.odds.ratio';'p.value'='multi.p.value';'signif'='multi.signif'")
    multi.table <- subset(multi.table,select=-model)
    results.table <- subset(results.table,select=-model)
    combined.table <- merge(results.table,multi.table,all.x=TRUE)
    results.table <- combined.table[order(factor(combined.table$xvar,levels=results.table$xvar)),]
    results.table <- data.frame(lapply(results.table,function(x) car::recode(x,"NA=''")))
  }
  return(results.table)
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
#' gg.colors(6)
#' @author Ying Taur
#' @export
gg.colors <- function(n=6, h=c(0,360)+15) {
  #emulates ggplot's default discrete color palette
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
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




#' Group by Time
#'
#' Given data frame with start and stop times, group times by non-overlapping start and stop times.
#'
#' This is like running \code{group_by}, but creates a new grouping variable called \code{index_} that is created from times.
#' @param data data frame
#' @param start start times
#' @param stop stop times
#' @param ... other variables to group by. These will be applied prior to grouping by times.
#' @param gap time periods differing by this gap or less will be combined in the grouping variable. Default is 1.
#' @param add Same as the add option in \code{group_by}. When TRUE, will add to groups, rather than overriding them.
#' @return Returns \code{data}, but grouped by times and other variables.
#' @author Ying Taur
#' @export
group_by_time <- function(data,start,stop, ... ,gap=1,add=FALSE) {
  mutate_call <- lazyeval::interp(~lag(cumsum(lead(x)-cummax(y)>gap),default=0),
                                  x=lazyeval::lazy(start),y=lazyeval::lazy(stop))
  data %>% group_by_(.dots=lazyeval::lazy_dots(...),add=add) %>%
    arrange_(.dots=list(lazyeval::lazy(start),lazyeval::lazy(stop))) %>%
    mutate_(.dots=setNames(list(mutate_call),"index_")) %>%
    group_by(index_,add=TRUE)
}


#' Select 2
#'
#' Basically \code{dplyr::select}, but ignores variables that aren't found in the data frame.
#'
#' @param data Data frame
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables.
#' @return Returns \code{data}, but grouped by times and other variables.
#' @author Ying Taur
#' @export
select2 <- function(data,...) {
  args <- lazyeval::lazy_dots(...)
  arg.values <- unname(sapply(args,function(x) deparse(x$expr)))
  funct <- grepl("(",arg.values,fixed=TRUE) #probably better way to do this...
  found <- arg.values %in% names(data)
  keep <- funct|found
  args2 <- args[keep]
  dplyr::select_(data,.dots=args2)
}



#' Cumulative Max
#'
#' Overrides base::cummax in order to correctly manage dates, if that is the input.
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





# gg.stack.old <- function(...,heights = NULL,gg.extras=NULL,gap=0,margin=1,units="inches",as.list=FALSE) {
#   grobs <- list(...)
#   length.grobs <- length(grobs)
#   if (length.grobs<=1) {
#     stop("YTError: should have at least 2 grobs")
#   }
#   top.theme <- theme(plot.margin=unit(c(margin, margin, gap, margin),units),
#                      axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#   middle.theme <- theme(plot.margin=unit(c(gap, margin, gap, margin),units),
#                         axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#   bottom.theme <- theme(plot.margin=unit(c(gap, margin, margin, margin),units))
#   g.top <- grobs[[1]] + top.theme + gg.extras
#   g.middle.list <- lapply(grobs[c(-1,-length.grobs)],function(g) {
#     g + middle.theme + gg.extras
#   })
#   g.bottom <- grobs[[length.grobs]] + bottom.theme + gg.extras
#   grobs1 <- c(list(g.top),g.middle.list,list(g.bottom))
#   grobs2 <- lapply(grobs1,function(g) {
#     #gr <- ggplotGrob(g1)
#     gr <- ggplotGrob(g)
#   })
#   nwidths <- max(sapply(grobs2,function(g) length(g$width)))
#   grobs3 <- lapply(grobs2,function(g) {
#     if (length(g$widths)<nwidths) {
#       g <- gtable_add_cols(g,unit(1,"null"))
#     }
#     return(g)
#   })
#   max.widths <- do.call(unit.pmax,lapply(grobs3,function(x) x$width))
#   grobs4 <- lapply(grobs3,function(g) {
#     g$widths <- max.widths
#     return(g)
#   })
#   if (as.list) {
#     return(grobs4)
#   }
#   args <- c(grobs4,list(ncol=1,heights=heights))
#   do.call(grid.arrange,args)
# }
# #' Coalesce
# #'
# #' Similar to SQL, use vectors in order until not NA.
# #' @param ... vectors, all the same size, listed in order of priority
# #' @return A single vector of values
# #' @examples
# #' a <- c(1,  2,  NA, 4, NA)
# #' b <- c(NA, NA, NA, 5, 6)
# #' c <- c(7,  8,  NA, 9, 10)
# #' coalesce(a,b,c)
# #' @export
# coalesce <- function(...) {
#   Reduce(function(x, y) {
#     i <- which(is.na(x))
#     x[i] <- y[i]
#     x},
#     list(...))
# }


#
# if (FALSE) { #use this to re-create cid data
#   rm(list=ls())
#   load("C:/Users/taury/Desktop/superrun 300/bmtmicro.part5.RData")
#   library(yingtools)
#   cid.init <- c("AA","AL","ARC","BG","CFR","CLA","CSC","DB","DC","DC2","DNV","DRS","DTC","EJB","FG","FL","FS","GD","GIR","GM","GP","HAS","HH","HS","IG","IR","JAF","JCP","JD","JD3","JF","JG","JHC","JJA","JK3","JLO","JP","JRR","JSC","JT","JT2","KD","KD2","KFM","KS","LC","LCD","LL","LR","LRV","MB","MB2","MC","MEA","MEB","MEL","MGA","MIW","MJC","MP","MP2","NB","NN","PAL","PJ","RAS","RC","REB","RG","RK","RL","RM","RMS","RSA","RT","RTH","SA","SDB","SEC","SFD","SK","SM","SN","SN2","SP","SPF","SS2","TC","TG","TL","TS","VM","WEM","WJW")
#   hosp <- d.admissions %.% filter(init %in% cid.init)
#   pt <- p %.% filter(init %in% cid.init)
#   samp <- s %.% filter(init %in% cid.init,seq.status=="sequenced",platform=="454")
#   tax <- t %.% filter(group %in% samp$group)
#   bsi <- bsi %.% filter(init %in% cid.init)
#   med <- d.meds %.% filter(init %in% cid.init)
#   cdiff <- m.cdiff %.% left_join(p.bmts,by="MRN") %.% filter(init %in% cid.init)
#   fever <- d.tmax %.% filter(init %in% cid.init)
#   save(pt,samp,tax,bsi,med,cdiff,fever,hosp,file="D:/Google Drive/R/yingtools/yingtools/data/cid.rda")
#
# }
#
# if (FALSE) {
#   library(Hmisc) #labelling of variables
#   library(ggplot2) #go-to graphing package
#   library(gridExtra) #grid.arrange
#   library(plyr) #ddply and adply
#   library(scales) #percents on axes in ggplot2
#   library(stringr) #str_extract
#   library(logistf) #Firth's penalized likelihood, logistic regression
#   library(coxphf) #Firth's penalized likelihood, survival analysis
#   library(reshape2)
#   library(ifultools)
#   library(car) #recode
#   library(gdata) #remove.vars, read.xls
#   library(chron)
#   library(dplyr)
#   #library(RODBC) #read/writing Office files (Excel, Access)
# }
#
#

#
#
# #' Run commands while away
# #'
# #' @param expr The commands to be run
# #' @param log.file Name of the file name log.
# #' @param send.text Whether to send text message.
# #' @examples
# #' run.while.away({
# #' Sys.sleep(2.3)
# #' log("a")
# #' },send.text=TRUE)
# #' @export
# run.while.away <- function(expr,log.file="~/Desktop/all.Rout",send.text=FALSE) {
#   t0 <- Sys.time()
#   zz <- file(log.file, open="wt")
#   sink(zz,type=c("message"))
#   try(expr)
#   sink(type="message")
#   t1 <- Sys.time()
#   td <- t1 - t0
#   diff <- paste(round(as.numeric(td),1),units(td))
#   fileout <- readLines(log.file)
#   if (length(fileout)>0) {
#     message <- paste0("Job: had error: ",last(fileout))
#   } else {
#     message <- paste0("Job: complete, no errors.")
#   }
#   message <- paste0(message,"\nExecution time: ",diff)
#   if (send.text) {
#     send.text.message(message)
#   }
#   cat(message)
# }
#
#
# #' Sample Data With Replacement
# #'
# #' For a given data frame, sample rows of data with replacement. This is similar to dplyr::sample_n, except that it accepts frequency and returns the sampled data frame with adjusted decreased frequency.
# #' @export
# sample_with_repl <- function(tbl,size,freq=NULL) {
#   #tbl <- data_frame(x=101:110,n=1101:1110,g=c(1,1,1,1,1,2,2,2,2,2)) %>% group_by(g);size=400;freq="n"
#   tbl.index <- tbl %>% select_(n=freq)
#   for (x in 1:size) {
#     tbl.index <- tbl.index %>% mutate(n=n-(1:n()==sample.int(n(),size=1,prob=n)))
#   }
#   tbl[,freq] <- tbl[,freq]-tbl.index$n
#   tbl <- tbl %>% filter_(paste0(freq,">0"))
#   return(tbl)
# }
#
#
# #' Get Additional Unique Identifiers
# #'
# #' For a given data frame and known unique identifier variables, finds all unique identifiers.
# #' This is useful for grouping commands such as \code{ddply}, \code{group_by}, and \code{dcast}.
# #' @export
# get.additional.unique.identifiers <- function(data,known.unique.vars) {
#   #data=m0
#   #known.unique.vars=c("CollectDateODBC","AccNumber")
#   vars.to.test <- setdiff(names(data),known.unique.vars)
#   vars.tally <- sapply(vars.to.test,function(x) {
#     tally <- data %.% s_group_by(known.unique.vars) %.%
#       s_summarize(paste0("count=length(unique(",x,"))"))
#     return(max(tally$count))
#   })
#   unique.vars <- c(known.unique.vars,vars.to.test[vars.tally==1])
#   nonunique.vars <- vars.to.test[vars.tally>1]
#   print(paste("Unique vars:",paste(unique.vars,collapse=", ")))
#   print(paste("Non-unique vars:",paste(nonunique.vars,collapse=", ")))
#   unique.vars
# }
#
#
# #' Examine 1 or 2 Vectors
# #'
# #' Display information about 1 or 2 vectors. Typically useful for determining uniqueness within or among vectors.
# #' @export
# examine <- function(x,y) {
#   #x=o$oligo.file;y=o$pool
#   #y=o$oligo.file;x=o$pool
#   #x=o$oligo.file;y=o$oligo.file
#   n <- length(x)
#   n.values <- length(unique(x))
#   is.unique <- n==n.values
#   cat("x:",ifelse(is.unique,"unique,","not unique,"),n.values,"unique values within",n,"\n")
#   if (!missing(y)) {
#     n.y <- length(y)
#     if (n.y!=n) {
#       stop("YTError: lengths of x and y are not the same!")
#     }
#     n.values.y <- length(unique(y))
#     is.unique.y <- n.y==n.values.y
#     cat("y:",ifelse(is.unique.y,"unique,","not unique,"),n.values.y,"unique values within",n.y,"\n")
#     tab.x <- data.frame(x,y,stringsAsFactors=FALSE) %>% group_by(x) %>% summarize(n=length(unique(y)),y=paste(unique(y)[1:2],collapse="\n")) %>% filter(n>1) %>% as.data.frame()
#     tab.y <- data.frame(x,y,stringsAsFactors=FALSE) %>% group_by(y) %>% summarize(n=length(unique(x)),x=paste(unique(x)[1:2],collapse="\n")) %>% filter(n>1) %>% as.data.frame()
#     if (nrow(tab.x)==0 & nrow(tab.y)==0) {
#       cat("x and y values are 1:1 matched")
#     } else {
#       if (nrow(tab.x)>0) {
#         cat("For each x, multiple values of y:\n")
#         cat(">x value:\n",tab.x$x[1],"\n>y values:\n",tab.x$y[1],sep="")
#       }
#       if (nrow(tab.y)>0) {
#         cat("For each y, multiple values of x.\n")
#         cat(">y value:\n",tab.y$y[1],"\n>x values:\n",tab.y$x[1],sep="")
#       }
#     }
#   }
# }
#
#

#
#
#
# #' Change proper case
# #'
# #' Changes a character vector to proper case (first letter of every word capitalized).
# #' @param text a charcter vector
# #' @return Returns \code{text}, where first letter of each word is capitalized.xx
# #' @examples
# #' proper("ying taur")
# #' @author Ying Taur
# #' @export
# proper <- function(text) {
#   properCase(text)
# }
#
# #' Change upper case
# #'
# #' Changes a character vector to all caps.
# #'
# #' @param text a charcter vector
# #' @return Returns \code{text}, where all letters are upper case.
# #' @examples
# #' upper("All Caps")
# #' @author Ying Taur
# #' @export
# upper <- function(text) {
#   upperCase(text)
# }
#
# #' Change lower case
# #'
# #' Changes a character vector to all lower case.
# #'
# #' @param text a charcter vector
# #' @return Returns \code{text}, where all letters are lower case.
# #' @examples
# #' lower("LOWER CASE")
# #' @author Ying Taur
# #' @export
# lower <- function(text) {
#   lowerCase(text)
# }
#
#

#
# #
# # x=met.samps
# # 12345678901234567890
# # x=c("448G","448Fx,xxxxx",
# #     "448E","448D",
# #     "448C")
# # # [^\"]*
# # # \"
# # rcode
# # copytoClipboard(rcode)
# # c("448G","448Fx,xxxxx","448E","448D","448C")
# #
# # grepl("\"*","")
# #
# # pat <- "^[^\"]*\"[^\"]*\"[^\"]*,"
# # xx=gregexpr(pat,x)
# # gregexpr(",",x)
# #
# # xx
# # rcode
# # str_extract_all(rcode,pat)
# #
# # rcode=copy.as.Rcode(x,format.width=20)
# # c("448G","448Fx,
# # xxxxx","448E",
# #   "448D","448C")
# #
#
#
#

#
#
# #' Split into lines
# #'
# #' Given a string of lines separated by "xxx", divide into a character vector.
# #'
# #' This is the opposite command of \code{collapse.lines}
# #' @export
# splitlines <- function(str) {
#   if (length(str)>1 | class(str)!="character") {
#     print("Error! str is not a character or has length > 1")
#     return(NULL)
#   }
#   unlist(strsplit(str,split="\n"))
# }
#
# #' Collapse Lines
# #'
# #' Given a character vector, collapse into one atomic vector, separated by "xxx"
# #' @export
# collapselines <- function(lines,trim=FALSE,sep="\n") {
#   #if(trim) {
#   #  lines <- trim(lines)
#   #}
#   paste(lines,collapse=sep)
# }
#
#
# #' @export
# grouplines <- function(lines,newline.pattern,offset=0,invert=FALSE,ignorecase=TRUE,split.first.only=FALSE,collapse=FALSE) {
#   #for a vector of strings, group into an array of separate vectors
#   #based on regexp pattern for the new line. offset alters index of regexp pattern
#   #e.g. if pattern is for last line of the first section, use offset=1.
#   #splitall specifies whether to split at every find.
#   #if collapse is true, instead return vector of strings, where char vector collapsed.
#   d <- data.frame(lines,stringsAsFactors=FALSE)
#   if (invert) {
#     newline.index <- which(!grepl(newline.pattern,d$lines,ignore.case=ignorecase)) + offset
#   } else {
#     newline.index <- which(grepl(newline.pattern,d$lines,ignore.case=ignorecase)) + offset
#   }
#   #make sure indices are within bounds.
#   newline.index <- newline.index[newline.index<=nrow(d)]
#   if (split.first.only) {
#     newline.index <- newline.index[1]
#   }
#   d$newline <- FALSE
#   d$newline[newline.index] <- TRUE
#   d$group.index <- cumsum(d$newline)
#   group.array <- split(d$lines,d$group.index)
#   if (collapse) {
#     group.array <- aaply(group.array,1,collapse.lines)
#   }
#   return(group.array)
# }
#
# #' @export
# grep.between <- function(x,start.pattern,stop.pattern,non.matches) {
#
#   d <- data.frame(x,stringsAsFactors=FALSE)
#   d$start.pos <- regexpr(start.pattern,d$x)
#   d$start.pos <- d$start.pos + attr(d$start.pos,"match.length")
#   d$start.pos <- as.vector(d$start.pos)
#
#   d <- adply(d,1,function(y) {
#     stop.hits <- as.vector(unlist(gregexpr(stop.pattern,y$x)))
#     y$stop.pos <- stop.hits[stop.hits==-1|stop.hits>y$start.pos][1] - 1
#     return(y)
#   })
#   d$result <- NA
#   quals <- !is.na(d$stop.pos) & d$start.pos>0 & d$stop.pos>0
#   d$result[quals] <- substr(d$x[quals],d$start.pos[quals],d$stop.pos[quals])
#   if (!missing(non.matches)) {
#     d$nonmatches <- non.matches
#     d$result[is.na(d$result)] <- d$nonmatches[is.na(d$result)]
#   }
#   d$result
# }
#
#

#
#
#
#
#
# #' @export
# screen.resolution <- function() {
#   #Returns the screen resolution in a vector. Can handle two screens.
#   scr_width <- system("wmic desktopmonitor get screenwidth", intern=TRUE)
#   scr_height <- system("wmic desktopmonitor get screenheight", intern=TRUE)
#   scr_width <- as.numeric(scr_width[-c(1, length(scr_width))])
#   scr_height <- as.numeric(scr_height[-c(1, length(scr_height))])
#   c(rbind(scr_width,scr_height))
# }
#
#
#
# #' @export
# recode.grep <- function(var,recodes,else.value,as.factor,multi.hits=FALSE,ignore.case=TRUE,perl=FALSE) {
#   #recodes a character vector based on recodes, a vector of reg expressions
#   #if multi.hits=FALSE, first hit in recodes takes priority. if multi.hits=TRUE, hits are combined in order they appear within string (takes longer).
#   if (multi.hits) {
#     var.recode <- sapply(var,function(x) {
#       #apply across all recodes
#       if (is.na(x)) {
#         return(NA)
#       }
#       hits <- mapply(function(pat,newname) {
#         regexpr(pat,x,ignore.case=ignore.case,perl=perl)
#       },names(recodes),recodes)
#       names(hits) <- recodes
#       if (all(hits==-1)) {
#         return(NA)
#       } else {
#         hits <- hits[hits!=-1]
#         if (multi.hits) {
#           hits.seq <- unique(names(hits)[order(hits)])
#           return(paste(hits.seq,collapse=";"))
#         } else {
#           return(names(hits)[1])
#         }
#       }
#     })
#   } else { #this code is faster
#     var.recode <- rep(NA,length(var))
#     for (i in 1:length(recodes)) {
#       pattern <- names(recodes)[i]
#       newname <- recodes[i]
#       still.na <- is.na(var.recode)
#       hit <- grepl(pattern,var[still.na],ignore.case=ignore.case,perl=perl)
#       var.recode[still.na][hit] <- newname
#     }
#     names(var.recode) <- var
#   }
#   #handling non-matches
#   if (missing(else.value)) {
#     #default is to use old value
#     var.recode <- ifelse(is.na(var.recode),var,var.recode)
#   } else {
#     var.recode <- ifelse(is.na(var.recode),else.value,var.recode)
#   }
#   if (missing(as.factor)) {
#     as.factor <- is.factor(var)
#   }
#   if (as.factor) {
#     if (missing(else.value)) {
#       var.recode <- factor(var.recode)
#     } else {
#       var.recode <- factor(var.recode,levels=c(recodes,else.value))
#     }
#   }
#   return(var.recode)
# }
#

#
# # recode.grep.old2 <- function(var,recodes,else.value,as.factor,multi.hits=FALSE,ignore.case=TRUE,perl=FALSE) {
# #   #recodes a character vector based on recodes, a vector of reg expressions
# #   #if multi.hits=FALSE, first hit in recodes takes priority. if multi.hits=TRUE, hits are combined in order they appear within string.
# #   #apply to each value of var
# #   var.recode <- sapply(var,function(x) {
# #     #apply across all recodes
# #     if (is.na(x)) {
# #       return(NA)
# #     }
# #     hits <- mapply(function(pat,newname) {
# #       regexpr(pat,x,ignore.case=ignore.case,perl=perl)
# #     },names(recodes),recodes)
# #     names(hits) <- recodes
# #     if (all(hits==-1)) {
# #       return(NA)
# #     } else {
# #       hits <- hits[hits!=-1]
# #       if (multi.hits) {
# #         hits.seq <- unique(names(hits)[order(hits)])
# #         return(paste(hits.seq,collapse=";"))
# #       } else {
# #         return(names(hits)[1])
# #       }
# #     }
# #   })
# #   #handling non-matches
# #   if (missing(else.value)) {
# #     #default is to use old value
# #     var.recode <- ifelse(is.na(var.recode),var,var.recode)
# #   } else {
# #     var.recode <- ifelse(is.na(var.recode),else.value,var.recode)
# #   }
# #   if (missing(as.factor)) {
# #     as.factor <- is.factor(var)
# #   }
# #   if (as.factor) {
# #     if (missing(else.value)) {
# #       var.recode <- factor(var.recode)
# #     } else {
# #       var.recode <- factor(var.recode,levels=c(recodes,else.value))
# #     }
# #   }
# #   return(var.recode)
# # }
#
#
#
#
#
# #' Test regular expression
# #'
# #' Used to test out a regular expression. Provides tabulation of hits or non-hits.
# #' @param pattern character; the regular expression to be tested.
# #' @param char.vector character; the strings on which to test the regular expression.
# #' @param invert if \code{TRUE}, will report non-hits instead of hits
# #' @param ignore.case if \code{FALSE}, pattern matching will be case-sensitive. This is passed to \code{grep} command.
# #' @param agrep if \code{TRUE}, will perform fuzzy matching with the \code{agrep} command.
# #' @param max.distance the maximum distance allowed for a match. This is passed to \code{grep} command.
# #' @return An ordered table of hits/non-hits.
# #' @author Ying Taur
# #' @export
# test.grep <- function(pattern,char.vector,invert=FALSE,ignore.case=TRUE,agrep=FALSE,max.distance=0.1) {
#   #use to test reg expressions. provides table of results in order
#   #use agrep for fuzzy matching, where max.distance sets the fuzziness (invert will be ignored)
#   #pattern="vori";char.vector=d.meds$DRUG_GENERIC_NAME;invert=FALSE;ignore.case=TRUE
#   if (agrep) {
#     results <- agrep(pattern,char.vector,ignore.case=ignore.case,value=TRUE,max.distance=max.distance)
#   } else {
#     results <- grep(pattern,char.vector,invert=invert,ignore.case=ignore.case,value=TRUE)
#   }
#
#   results <- table(results)
#   results <- results[order(results,decreasing=TRUE)]
#   return(results)
# }
#
# #' @export
# compare.grep <- function(pattern1,pattern2,char.vector,ignore.case=TRUE) {
#   #use to compare two reg expressions.
#   #pattern1="vori";pattern2="voriconazole";char.vector=d.meds$DRUG_GENERIC_NAME;ignore.case=TRUE
#   results1 <- grepl(pattern1,char.vector,ignore.case=ignore.case)
#   results2 <- grepl(pattern2,char.vector,ignore.case=ignore.case)
#   matches <- table(char.vector[results1 & results2])
#   matches <- matches[order(matches,decreasing=TRUE)]
#   pattern1.only <- table(char.vector[results1 & !results2])
#   pattern1.only <- pattern1.only[order(pattern1.only,decreasing=TRUE)]
#   pattern2.only <- table(char.vector[!results1 & results2])
#   pattern2.only <- pattern2.only[order(pattern2.only,decreasing=TRUE)]
#   neither <- table(char.vector[!results1 & !results2])
#   neither <- neither[order(neither,decreasing=TRUE)]
#   print(sapply(c("matches","pattern1.only","pattern2.only","neither"),function(x) length(get(x))))
#   list(matches=matches,pattern1.only=pattern1.only,pattern2.only=pattern2.only,neither=neither)
# }
#
# #' Compare agrep
# #' @export
# compare.agrep <- function(pattern.fuzzy,pattern.exact,char.vector,ignore.case=TRUE,max.distance=3) {
#   #compares fuzzy search to exact search.
#   #pattern.fuzzy="voriconazole";pattern.exact="v.+e";char.vector=d.meds$DRUG_GENERIC_NAME;ignore.case=TRUE;max.distance=2
#   fuzzy.results <- agrep(pattern.fuzzy,char.vector,ignore.case=ignore.case,max.distance=max.distance)
#   exact.results <- grep(pattern.exact,char.vector,ignore.case=ignore.case)
#   neither.results <- setdiff(1:length(char.vector),c(fuzzy.results,exact.results))
#   both.match <- table(char.vector[intersect(fuzzy.results,exact.results)])
#   #fuzzy and exact will be sorted by distance rather than frequency
#   fuzzy.only <- table(char.vector[setdiff(fuzzy.results,exact.results)])
#   if (length(fuzzy.only)>0) {
#     fuzzy.dist <- sapply(names(fuzzy.only),function(x) adist(pattern.exact,x,ignore.case=ignore.case,partial=TRUE,fixed=FALSE))
#     names(fuzzy.only) <- paste0(names(fuzzy.only)," [",fuzzy.dist,"]")
#     fuzzy.only <- fuzzy.only[order(fuzzy.dist)]
#   }
#   exact.only <- table(char.vector[setdiff(exact.results,fuzzy.results)])
#   if (length(exact.only)>0) {
#     exact.dist <- sapply(names(exact.only),function(x) adist(pattern.fuzzy,x,ignore.case=ignore.case,partial=TRUE,fixed=FALSE))
#     names(exact.only) <- paste0(names(exact.only)," [",exact.dist,"]")
#     exact.only <- exact.only[order(exact.dist)]
#   }
#
#   neither <- table(char.vector[neither.results])
#   neither <- neither[order(neither,decreasing=TRUE)]
#   print(sapply(c("both.match","fuzzy.only","exact.only","neither"),function(x) length(get(x))))
#   list(both.match=both.match,fuzzy.only=fuzzy.only,exact.only=exact.only,neither=neither)
# }
#
# #' Group time segments
# #'
# #' Given start and stop times, return a grouping variable where adjacent times are grouped.
# #'
# #' Note that start and stop times do not need to be sorted.
# #' @param tstart vector representing start times
# #' @param tstop vector representing stop times
# #' @param gap number designating the minimum allowable time gap between time segments. Time segments with greater gaps are in separate groups.
# #' @return An integer vector, encoding time groups.
# #' @author Ying Taur
# #' @export
# group.times <- function(tstart,tstop,gap=-Inf,mid.gap=-Inf) {
#   #tstart <- c(2,3,12,14,22,25,32,36,42,43,48)
#   #tstop <- c(4,5,14,16,24,27,34,38,49,44,49)
#
#   #place in data.frame
#   d <- data_frame(tstart,tstop,i=1:length(tstart)) %>% mutate(tmid=(tstart+tstop)/2)
#   #sort d by times
#   dd <- d %>% arrange(tstart,tstop) %>%
#     mutate(prior.tstop=lag(cummax(tstop),default=-Inf),
#            diff=tstart-prior.tstop,
#            cummax.tmid=cummax(tmid),
#            diff.mid=tmid-lag(cummax(tmid),default=-Inf),
#            newgroup=diff>gap & diff.mid>mid.gap,
#            cumsum=cumsum(newgroup))
#   #ggplot(dd,aes(x=tstart,xend=tstop,y=i,yend=i,label=factor(cumsum),color=factor(cumsum))) + geom_segment() + geom_text(aes(x=tmid)) + geom_vline(aes(xintercept=tstart),color="gray",linetype="longdash") + geom_vline(aes(xintercept=tstop),color="gray",linetype="longdash") + scale_x_continuous(breaks=min(tstart):max(tstop))
#   #place back in original order
#   d2 <- dd %>% arrange(i)
#   return(d2$cumsum)
# }
#
#
# #' Timeline rows
# #'
# #' Create a data frame to be used for plotting timelines in ggplot::geom_rect.
# #' @param data the data frame containing timeline data
# #' @param tstart character, the variable in data for start time
# #' @param tstop character, the variable in data for stop time
# #' @param rowvar character, the variable in data which will define the rows.
# #' @param label character, the variable in data for text label. Often same was rowvar.
# #' @param fill character, the variable in data representing fill color.
# #' @param nrows number of rows to use.
# #' @param sortby character, the variable in data to sort rows by. NULL by default, which is no sorting.
# #' @param plot.yrange a 2-number vector specifying the range of y-values where rows will be spread across. By default, rows are distributed across c(0,1)
# #' @return A data frame, similar to \code{data}, but with additional vars: x, y, xmin, xmax, ymin, ymax
# #' @author Ying Taur
# #' @export
# timeline.rows <- function (data, tstart, tstop, rowvar, label, fill, nrows, sortby = NULL, collate=FALSE, text.merge=FALSE,
#                            text.gap=1, text.mid.gap=0, share.rows=FALSE, yrange = NULL, ytrans = NULL) {
#   #data=met.meds %>% filter(MRN==mrn,occurs.within(start.day,end.day,xlim[1],xlim[2]));tstart="start.day"
#   #tstop="end.day";rowvar="med";share.rows=TRUE;label="med";fill="med.class";text.merge=T;rowvar="med"
#   #sortby="med.class";collate=T;nrows=20;text.gap=1;text.mid.gap=4;plot.yrange=c(0,1)
#   data$tstart <- data[[tstart]]
#   data$tstop <- data[[tstop]]
#   if (!is.null(sortby)) {
#     data <- data %>% arrange_(sortby)
#   } else {
#     data <- data %>% arrange(tstart)
#   }
#   data[[rowvar]] <- factor(data[[rowvar]],levels=unique(data[[rowvar]]))
#   if (share.rows) {
#     print("doesn't work yet")
#     #nothing yet
#   }
#   if (collate) {
#     data <- data %>% group_by_(rowvar,label,fill) %>%
#       group_by(group=group.times(tstart,tstop,gap=1),add=TRUE) %>%
#       summarize(tstart=min(tstart),tstop=max(tstop)) %>% ungroup()
#   }
#   data$y <- as.numeric(data[[rowvar]])
#   data$x <- (data$tstart + data$tstop)/2
#   data$label <- data[[label]]
#   data$fill <- data[[fill]]
#   if (text.merge) {
#     data <- data %>% group_by(y,label,fill) %>%
#       group_by(group=group.times(tstart,tstop,gap=text.gap,mid.gap=text.mid.gap),add=TRUE) %>%
#       mutate(x=(min(tstart)+max(tstop))/2,
#              label2=c(first(as.character(label)),rep("",n()-1))) %>% ungroup() %>%
#       mutate(label=label2) %>% select(-label2)
#   }
#   data$xmin <- data$tstart - 0.45
#   data$xmax <- data$tstop + 0.45
#   data$ymin <- data$y - 0.45
#   data$ymax <- data$y + 0.45
#   if (missing(nrows)) {
#     from.range <- c(0.5, max(data$y)+0.5)
#   } else {
#     from.range <- c(0.5, max(max(data$y),nrows)+0.5)
#   }
#   if (!is.null(yrange)) {
#     if (!is.null(ytrans)) {
#       yrange <- ytrans$transform(yrange)
#       data$ymin <- ytrans$inverse(rescale(data$ymin, yrange, from = from.range))
#       data$ymax <- ytrans$inverse(rescale(data$ymax, yrange, from = from.range))
#       data$y <- ytrans$inverse(rescale(data$y, yrange, from = from.range))
#     } else {
#       data$ymin <- rescale(data$ymin, yrange, from = from.range)
#       data$ymax <- rescale(data$ymax, yrange, from = from.range)
#       data$y <- rescale(data$y, yrange, from = from.range)
#     }
#   }
#   #ggplot() +  geom_rect(data=data,aes_auto(data)) + geom_text(data=data,aes_auto(data))
#   return(data)
# }
#
# # timeline.rows <- function (data, tstart, tstop, rowvar, nrows, sortby = NULL, plot.yrange = c(0,1)) {
# #   #tstart="start.day";tstop="end.day";rowvar="med";sortby="med.class";plot.yrange=c(1.15,2.3);data=med
# #
# #   if (!is.null(sortby)) {
# #     data <- data %>% arrange_(sortby)
# #   } else {
# #     data <- data %>% arrange_(tstart)
# #   }
# #   data[[rowvar]] <- factor(data[[rowvar]],levels=unique(data[[rowvar]]))
# #   data$y <- as.numeric(data[[rowvar]])
# #   data$x <- (data[[tstart]] + data[[tstop]])/2
# #   data$xmin <- data[[tstart]] - 0.45
# #   data$xmax <- data[[tstop]] + 0.45
# #   data$ymin <- data$y - 0.45
# #   data$ymax <- data$y + 0.45
# #   if (missing(nrows)) {
# #     from.range <- c(0.5, max(data$y)+0.5)
# #   } else {
# #     from.range <- c(0.5, max(max(data$y),nrows)+0.5)
# #   }
# #
# #   data$ymin <- scales::rescale(data$ymin, plot.yrange, from = from.range)
# #   data$ymax <- scales::rescale(data$ymax, plot.yrange, from = from.range)
# #   data$y <- scales::rescale(data$y, plot.yrange, from = from.range)
# #   return(data)
# # }
#
#
#
#
#
#
#
#
# #' @export
# geom_timeline <- function(data,tstart,tstop,label,fill,rowvar,sortby=NULL,plot.yrange=NULL,angle=0,alpha=1) {
#   if (nrow(data)==0) {
#     return(NULL)
#   }
#   tl <- timeline.rows(data=data,tstart=tstart,tstop=tstop,rowvar=rowvar,sortby=sortby,plot.yrange=plot.yrange)
#   list(geom_rect(data=tl,aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax",fill=fill)),
#        geom_text(data=tl,aes_string(x="x",y="y",label=label,angle=angle)))
# }
#
# #' @export
# geom_linestrip <- function(data,yvar,xvar,data.yrange=c(0,1),plot.yrange=c(0,1),background.fill="gray",background.alpha=0.3) {
#   #data=pha.sub;yvar="PHA";xvar="day";data.yrange=c(0,1);plot.yrange=c(11,12)
#   if (nrow(data)==0) {
#     return(NULL)
#   }
#   #if yvar doesn't stay within yrange, scale it.
#   real.yrange <- range(data[,yvar])
#   if (!(occurs.within(data.yrange[1],data.yrange[2],real.yrange[1],real.yrange[2]))) {
#     warning("YT: Expected yvar to be within data.yrange=",copy.as.Rcode(data.yrange,copy.clipboard=FALSE),", but it wasn't. Scaled data range to fit.")
#     data$new.y <- rescale(data[,yvar],from=range(data[,yvar]))
#     yvar <- "new.y"
#   }
#   data$y <- rescale(data[,yvar],from=data.yrange,to=plot.yrange)
#   data$x <- data[,xvar]
#   g.linestrip <- list(geom_point(data=data,aes(x=x,y=y)),geom_line(data=data,aes(x=x,y=y)))
#   if (!is.null(background.fill)) {
#     g.linestrip <- c(annotate("rect",xmin=-Inf,xmax=Inf,ymin=plot.yrange[1],ymax=plot.yrange[2],fill=background.fill,alpha=background.alpha),g.linestrip)
#   }
#   return(g.linestrip)
# }
#
#
#
#

#
#
# # trans.logistic <- function(var,inner.range,percentiles=c(0.1,0.9),invert=FALSE,scale=c(0,1),show.transformation=FALSE) {
# #   #performs logistic transformation. Choose inner.range to be range of important values you'd like to see.
# #   a <- (inner.range[1]*log(1/percentiles[2]-1)-inner.range[2]*log(1/percentiles[1]-1))/(inner.range[1]-inner.range[2])
# #   b <- (log(1/percentiles[1]-1)-log(1/percentiles[2]-1))/(inner.range[1]-inner.range[2])
# #   logist <- function(x) {
# #     y <- 1/(1+exp(a+b*x))
# #     if (any(scale!=c(0,1))) {
# #       y <- rescale(y,to=scale,from=c(0,1))
# #     }
# #     return(y)
# #   }
# #   logist.inverse <- function(y) {
# #     if (any(scale!=c(0,1))) {
# #       y <- rescale(y,to=c(0,1),from=scale)
# #     }
# #     x <- (log(1/y-1)-a) / b
# #     return(x)
# #   }
# #
# #   if (invert) {
# #     logist.var <- logist.inverse(var)
# #   } else {
# #     logist.var <- logist(var)
# #   }
# #   if (show.transformation) {
# #     d <- data.frame(var,logist.var)
# #     yvals <- c(scale,percentiles)
# #     g <- ggplot() + geom_point(data=d,aes(x=var,y=logist.var),color=muted("red")) +
# #       annotate("segment",x=inner.range,y=percentiles,xend=inner.range,yend=-Inf,linetype="dotted") +
# #       annotate("segment",x=inner.range,y=percentiles,xend=-Inf,yend=percentiles,linetype="dotted") +
# #       scale_y_continuous(breaks=yvals) +
# #       ggtitle("Logistic transformation")
# #     return(g)
# #   } else {
# #     return(logist.var)
# #   }
# # }
#
#


#
#
#

#

#
#
# #' Year
# #'
# #' Obtains year portion of date.
# #'
# #' @param date a \code{Date} vector.
# #' @param as.numeric logical, whether to format as numeric.
# #' @return Returns the year.
# #' @examples
# #' year(as.Date("1975-02-21"))
# #' @author Ying Taur
# #' @export
# year <- function(date,as.numeric=TRUE) {
#   #extracts year
#   new.date <- format(date,format="%Y")
#   if (as.numeric) {
#     new.date <- as.numeric(new.date)
#   }
#   return(new.date)
# }
#
# #' @export
# month <- function(date,as.numeric=FALSE) {
#   #as.numeric(format(date,format="%Y"))
#   new.date <- format(date,format="%Y-%m")
#   if (as.numeric) {
#     new.date <- as.numeric(new.date)
#   }
#   return(new.date)
# }
#
# #' @export
# half.year <- function(date,as.numeric=FALSE) {
#   mo <- as.numeric(format(date,format="%m"))
#   yr <- as.numeric(format(date,format="%Y"))
#   new.mo <- ifelse(mo %in% 1:6,1,7)
#   new.date <- as.Date(mapply(function(m,y) paste0(c(y,m,1),collapse="-"),new.mo,yr))
#   new.date <- format(new.date,"%Y-%m")
#   if (as.numeric) {
#     new.date <- as.numeric(new.date)
#   }
#   return(new.date)
# }
#
# #' @export
# third.year <- function(date,as.numeric=FALSE) {
#   mo <- as.numeric(format(date,format="%m"))
#   yr <- as.numeric(format(date,format="%Y"))
#   new.mo <- ifelse(mo %in% 1:4,1,ifelse(mo %in% 5:8,5,9))
#   new.date <- as.Date(mapply(function(m,y) paste0(c(y,m,1),collapse="-"),new.mo,yr))
#   new.date <- format(new.date,"%Y-%m")
#   if (as.numeric) {
#     new.date <- as.numeric(new.date)
#   }
#   return(new.date)
# }
#
#
#
# #' Determine C.diff testing method
# #'
# #' For a C.difficile assay performed at MSKCC, determines what method was used,
# #' based on date of specimen collection.
# #'
# #' Uses collection date (\code{cdt}) to determine C.diff testing method. Cytotoxicity assay was used early on,
# #' then changed to GDH/cytotoxicity after 8/29/2008, then changed to PCR testing
# #' on 9/10/2010.
# #'
# #' @param cdt Date vector corresponding to dates of collection
# #' @return A factor vector is returned, which may be "Cytotoxicity", "GDH/Cytotoxicity", or "PCR"
# #' @examples
# #' dates <- as.Date(c("2006-01-01","2009-01-01","2012-01-01"))
# #' cdiff.method(dates)
# #' @export
# cdiff.method <- function(cdt) {
#   #given collect date, return the method of cdiff based on dates.
#   if (class(cdt)!="Date") {
#     stop("Error, input is not a date!")
#   }
#   method <- rep(NA,length(cdt))
#   method[!is.na(cdt)] <- "GDH/Cytotoxicity"
#   method[cdt>=as.Date("2010-09-10")] <- "PCR"
#   method[cdt<as.Date("2008-08-29")] <- "Cytotoxicity"
#   method <- factor(method,levels=c("Cytotoxicity","GDH/Cytotoxicity","PCR"))
#   return(method)
# }
#
#
# #####################################
# #### logistic regression methods ####
# #####################################


#
# #' @export
# forest.plot <- function(data.table,varname="xvar",estimate=NULL,lower="lower.ci",upper="upper.ci",p.value="p.value",
#                         include.ci=FALSE,limits=NULL,dashline.at=NULL) {
#   if (is.null(estimate)) {
#     estimate <- grep("ratio",names(data.table),value=TRUE)[1]
#     if (is.null(dashline.at)) {
#       dashline.at <- 1
#     }
#   }
#   data.table$signif <- data.table[,p.value] <= 0.05
#   if (include.ci) {
#     est <- round(data.table[,estimate],2)
#     low <- round(data.table[,lower],2)
#     up <- round(data.table[,upper],2)
#     data.table$text <- paste0(est," (",low,"-",up,")")
#   } else {
#     data.table$text <- round(data.table[,estimate],2)
#   }
#   #reverse levels
#   data.table[,varname] <- factor(data.table[,varname],rev(data.table[,varname]))
#
#   g <- ggplot(data.table,aes_string(x=varname,y=estimate,ymin=lower,ymax=upper,label="text",color="signif")) +
#     geom_pointrange() + geom_text(vjust=-1,size=3.5) +
#     scale_color_manual(values=c("FALSE"="black","TRUE"="red"),guide=FALSE)
#   if (!is.null(dashline.at)) {
#     g <- g + geom_hline(yintercept=dashline.at,color="blue",linetype="dashed")
#   }
#   if (!is.null(limits)) {
#     g <- g + coord_flip(ylim=limits)
#   } else {
#     g <- g + coord_flip()
#   }
#   return(g)
# }
#
#
#
#

#
# #' ...Title...
# #'
# #' ...Description...
# #'
# #' @usage ...usage.code...
# #'
# #' ...details...
# #'
# #' @param .param1. ...param1.description...
# #' @param .param2. ...param2.description...
# #' @return ...description.of.data.returned...
# #' @examples
# #' ...examples.here....
# #' @keywords keyword1 keyword2 ...
# #' @seealso \code{\link{cdiff.method}}
# #' @author Ying Taur
# #' @export
# get.vars <- function(var.values,data) {
#   #given variable vector, return variable name from data.
#   #can use with regression table data
#   #var.values=table.cox.bmtdead$xvar;data=pp
#   get.var <- function(vv) { #get var for one var.value
#     #vv="StemSource2Unrelated Identical"
#     var <- names(data)[sapply(names(data),function(v) startsWith(vv,v))]
#     if (length(var)==0) {
#       print(paste("Error! Could not find variable:",vv))
#       return(NA)
#     } else if (length(var)==1) {
#       return(var)
#     } else {
#       level.fits <- sapply(var,function(v) {
#         remainder <- substr(vv,nchar(v)+1,nchar(vv))
#         remainder %in% as.character(data[,v])
#       })
#       var <- var[level.fits]
#       if (length(var)!=1) {
#         print(paste("Error! Could not find variable:",vv))
#         return(NA)
#       } else {
#         return(var)
#       }
#     }
#   }
#   sapply(var.values,get.var)
# }
#
# #' ...Title...
# #'
# #' ...Description...
# #'
# #' @usage ...usage.code...
# #'
# #' ...details...
# #'
# #' @param .param1. ...param1.description...
# #' @param .param2. ...param2.description...
# #' @return ...description.of.data.returned...
# #' @examples
# #' ...examples.here....
# #' @keywords keyword1 keyword2 ...
# #' @seealso \code{\link{cdiff.method}}
# #' @author Ying Taur
# #' @export
# iflabel <- function(vars,data) {
#   #var.values
#   #vars <- get.vars(var.values,data)
#   sapply(vars,function(v) {
#     if (v %in% names(data)) {
#       ifelse(label(data[,v])=="",v,label(data[,v]))
#     } else {
#       v
#     }
#   })
# }
#
# #' ...Title...
# #'
# #' ...Description...
# #'
# #' @usage ...usage.code...
# #'
# #' ...details...
# #'
# #' @param .param1. ...param1.description...
# #' @param .param2. ...param2.description...
# #' @return ...description.of.data.returned...
# #' @examples
# #' ...examples.here....
# #' @keywords keyword1 keyword2 ...
# #' @seealso \code{\link{cdiff.method}}
# #' @author Ying Taur
# #' @export
# format.regression.table <- function(tbl,data,xvar="xvar",yvar="yvar") {
#   #tbl=table.cox.bmtdead;data=pp;xvar="xvar";yvar="yvar"
#   yvar.label <- iflabel(tbl[,yvar],data=data)
#   xvar.values <- tbl[,xvar]
#   xvars <- get.vars(xvar.values,data=data)
#   xvars.label <- iflabel(xvars,data=data)
#   xvalues <- substr(xvar.values,nchar(xvars)+1,nchar(xvar.values))
#   tbl.new <- data.frame(yvar.label,xvars.label,xvalues,xvars,tbl,stringsAsFactors=FALSE,row.names=NULL)
#   tbl.left <- unique(subset(tbl.new,select=c(yvar.label,yvar,xvars.label,xvars)))
#   tbl.left <- adply(tbl.left,1,function(x) {
#     if (is.logical(data[,x$xvars]) | is.factor(data[,x$xvars]) | is.character(data[,x$xvars])) {
#       lvls <- levels(factor(data[,x$xvars]))
#       if (length(lvls)>2) {
#         xvalues <- lvls
#       } else {
#         xvalues <- lvls[-1]
#       }
#     } else {
#       xvalues <- ""
#     }
#     data.frame(xvars=x,xvalues,stringsAsFactors=FALSE,row.names=NULL)
#   })
#   tbl.left$order <- 1:nrow(tbl.left) #merging and ddplying change the order
#   tbl <- merge(tbl.left,tbl.new,by=c("yvar.label","yvar","xvars","xvars.label","xvalues"),all=TRUE)
#   tbl$haz.ratio[is.na(tbl$haz.ratio)] <- "1.0"
#   tbl$multi.haz.ratio[is.na(tbl$multi.haz.ratio)] <- "1.0"
#   tbl$p.value[is.na(tbl$p.value)] <- "-"
#   tbl$multi.p.value[is.na(tbl$multi.p.value)] <- "-"
#   tbl <- tbl[,c("order","yvar.label","xvars.label","xvalues","xvar","haz.ratio","p.value","multi.haz.ratio","multi.p.value")]
#   tbl <- ddply(tbl,"xvars.label",function(x) {
#     if (nrow(x)>=3) {
#       header <- data.frame(t(rep("",length(x))),stringsAsFactors=FALSE)
#       names(header) <- names(x)
#       header$xvars.label <- x$xvars.label[1]
#       header$order <- min(x$order)-0.5
#       x$xvars.label <- paste0("--",x$xvalues)
#       x <- rbind(header,x)
#     }
#     return(x)
#   })
#   tbl <- tbl[order(tbl$order),]
#   if (anyDuplicated(tbl$order)) {
#     print("Error!")
#     return(NULL)
#   }
#   tbl <- subset(tbl,select=-c(xvalues,order))
#   return(tbl)
# }
#
#

#


#
#
#' #' @rdname recode2
#' #' @export
#' replace.grep.old <- function(var,recodes,ignore.case=TRUE,perl=FALSE,useBytes=TRUE) {
#'   newvar <- var
#'   for (i in 1:length(recodes)) {
#'     pattern <- names(recodes)[i]
#'     replacetext <- recodes[i]
#'     newvar <- gsub(pattern,replacetext,newvar,ignore.case=ignore.case,perl=perl,useBytes=useBytes)
#'   }
#'   return(newvar)
#' }
#
#
#
#
#
# #' Make Table 1
# #'
# #' Creates a summary table (data frame) variables from the data
# #'
# #' @param vars character vector of variables in to describe within \code{data}.
# #' @param data data frame of the data to be described.
# #' @param by character denoting variable by which \code{data} will be summarized. If not specified, only total column will be used.
# #' @param maxlevels for each variable in \code{vars}, determines whether to describe as factor or numeric. This is the cutoff number of levels for determining that.
# #' @param removefalses logical, whether or not to remove rows pertaining to \code{FALSE} values.
# #' @param fullcols logical, whether or not to show the calculation of each cell in the table.
# #' @param showdenom logical, whether to show denominator in the cells.
# #' @param fisher logical, whether or not to calculate Fisher exact tests.
# #' @return Returns a data frame formatted to be Table 1 in a manuscript.
# #' @examples
# #' ****examples.here....
# #' @author Ying Taur
# #' @export
# make.table1 <- function(vars,data,by,maxlevels=10,removefalses=FALSE,fullcols=FALSE,showdenom=TRUE,fisher=TRUE) {
#   #remove falses: for 0-1 data, only show 1's
#   #nicecols: only display summary line.
#   data$total <- ""
#   vars <- c(vars,"total")
#   #convert any 0-1 data to logical
#   binaryvars <- vars[sapply(data[,vars],function(x) is.numeric(x) & setequal(x,c(0,1)))]
#   data[,binaryvars] <- sapply(data[,binaryvars],as.logical)
#   #store list of logicalvars. used later if removefalses=TRUE
#   logicalvars <- vars[sapply(data[,vars],is.logical)]
#   #makes table1. deals with factors, logical, and numbers with few levels (up to maxlevels)
#   varlevels <- sapply(vars,function(x) length(unique(data[,x])))
#   #determine which vars should be analyzed
#   vars.skip <- names(varlevels[varlevels>maxlevels])
#   vars.keep <- setdiff(vars,vars.skip)
#   #convert keepvars to factors
#   data[,vars.keep] <- data.frame(lapply(data[,vars.keep],factor))
#   #given a dataframe of factors, return column of summary data.
#   make.column <- function(d) {
#     d.keep <- d[,vars.keep]
#     d.skip <- d[,vars.skip]
#     #for a variable vector, make descriptive data.
#     desc.var <- function(v) {
#       tab <- table(v)
#       data.frame(
#         values=names(tab),
#         count=as.vector(tab),
#         total=sum(tab),
#         percent=as.vector(prop.table(tab))
#       )
#     }
#     d.col <- adply(d.keep,2,desc.var)
#     names(d.col)[1] <- "variable"
#     if (showdenom) {
#       #d.col$summary <- paste0(d.col$count,"/",d.col$total," (",round(100*d.col$percent,1),"%)")
#       d.col$summary <- paste0(d.col$count,"/",d.col$total," (",percent.format(d.col$percent,digits=1),")")
#     } else {
#       #d.col$summary <- paste0(d.col$count," (",round(100*d.col$percent,1),"%)")
#       d.col$summary <- paste0(d.col$count," (",percent.format(d.col$percent,digits=1),")")
#     }
#     if (!fullcols) { #only use summary line
#       d.col <- subset(d.col,select=c(variable,values,summary))
#     }
#     return(d.col)
#   }
#   total <- make.column(data)
#   if (removefalses) {
#     #currently not working.
#     #     falses <- all$variable %in% logicalvars & all$values=="FALSE"
#     #     all <- all[!falses,]
#     #     trues <- all$variable %in% logicalvars & all$values=="TRUE"
#     #     all$values[trues] <- ""
#   }
#   if (missing(by)) {
#     all <- total
#   } else { #make total column and calc fisher
#     subpop.list <- dlply(data,by,function(x) subset(make.column(x),select=-c(variable,values)))
#     all.list <- subpop.list
#     all.list[[length(all.list)+1]] <- subset(total,select=-c(variable,values))
#     names(all.list)[length(all.list)] <- "total"
#     all <- do.call(cbind,all.list)
#     #this conditioning is probably redundant
#     if (!fullcols & length(all)==length(all.list)) {
#       names(all) <- names(all.list)
#     }
#     all <- cbind(subset(total,select=c(variable,values)),all)
#     if (fisher) {
#       #fisher test.
#       f.vars <- vars[vars!="total"]
#       names(f.vars) <- NULL
#       ftest.p <- sapply(f.vars,function(v) {
#         ftest <- fisher.test(data[,v],data[,by])
#         ftest$p.value
#       })
#       all$fisher.pval <- ftest.p[match(all$variable,names(ftest.p))]
#       all$fisher.pval <- as.character(round(all$fisher.pval,3))
#       first.row <- c(TRUE,all$variable[-1]!=all$variable[-length(all$variable)]) & !is.na(all$fisher.pval)
#       all$fisher.pval <- ifelse(first.row,all$fisher.pval,"")
#     }
#   }
#   return(all)
# }
#
#

#
# #' @export
# align_plots <- function(..., xlim,all.x=FALSE,width=unit(3, "cm")){
#   left_width <- function(g) {
#     axis_l_index <- g$layout$r[g$layout$name=="axis-l"]
#     ylab_index <- g$layout$r[g$layout$name=="ylab"]
#     g$widths[[axis_l_index]] + g$widths[[ylab_index]]
#   }
#   full_width <- function(g) {
#     sum(g$widths)
#   }
#   set_panel_width <- function(g, width=unit(3, "cm")) {
#     panel_index <- g$layout$r[g$layout$name=="panel"]
#     g$widths[[panel_index]] <- width
#     g
#   }
#   as.gtable <- function(p) {
#     ggplot_gtable(ggplot_build(p))
#   }
#   grid.newpage()
#   pl <- list(...)
#   #top: get rid of x-axis
#   pl[[1]] <- pl[[1]] + opts(plot.margin=unit(c(0.075, 0.075, 0, 0.075),"npc"),
#                             axis.title.x=theme_blank()) + xlim(xlim)
#   #bottom
#   pl[[length(pl)]] <- pl[[length(pl)]] + opts(plot.margin=unit(c(0, 0.075, 0.075, 0.075),"npc")) + xlim(xlim)
#   #middle
#   if (length(pl)>2) {
#     mid.items <- c(FALSE,rep(TRUE,length(pl)-2),FALSE)
#     pl[mid.items] <- lapply(pl[mid.items], function(x) x + opts(plot.margin=unit(c(0, 0.075, 0, 0.075),"npc"),
#                                                                 axis.title.x=theme_blank()) + xlim(xlim))
#   }
#   if (!all.x) {
#     mid.axes <- c(rep(TRUE,length(pl)-1),FALSE)
#     pl[mid.axes] <- lapply(pl[mid.axes],function(x) x + opts(axis.ticks=theme_blank(),axis.text.x=theme_blank()))
#   }
#   gl <- lapply(pl, as.gtable)
#   gl <- lapply(gl, set_panel_width, width=width)
#   left <- lapply(gl, left_width)
#   max_left <- max(do.call(unit.c, left))
#   widths <- lapply(gl, full_width)
#   max_width <- max(do.call(unit.c, widths))
#
#   lay <- grid.layout(nrow=length(gl), ncol=1)
#   vp <- viewport(layout=lay)
#   pushViewport(vp)
#
#   for(ii in seq_along(gl)){
#     pushViewport(viewport(layout.pos.row=ii))
#     pushViewport(viewport(x=unit(0.5, "npc") - 0.5*max_width + max_left - left[[ii]],
#                           just="left", width=widths[[ii]]))
#     grid.draw(gl[[ii]])
#     upViewport(2)
#   }
#   upViewport()
# }
#
#

#
# #' Body Mass Index
# #'
# #' Calculates BMI, given height and weight.
# #'
# #' Uses the formula: \code{weight.kg / height.meters / height.meters}
# #'
# #' @param height.cm numeric, height in cm.
# #' @param weight.kg numeric, weight in kg.
# #' @return Returns a numeric corresponding to body mass index
# #' @examples
# #' bmi(170,65)
# #' @author Ying Taur
# #' @export
# bmi <- function(height.cm,weight.kg) {
#   height.meters <- height.cm/100
#   weight.kg / height.meters / height.meters
# }
#
# #' Calculate Ideal Body Weight
# #'
# #' ...Description...
# #'
# #' @usage ...usage.code...
# #'
# #' ...details...
# #'
# #' @param .param1. ...param1.description...
# #' @param .param2. ...param2.description...
# #' @return ...description.of.data.returned...
# #' @examples
# #' ...examples.here....
# #' @keywords keyword1 keyword2 ...
# #' @seealso \code{\link{cdiff.method}}
# #' @author Ying Taur
# ibw <- function(female,ht.cm) {
#   #devine
#   ht.inches <- ht.cm * 0.3937
#   ifelse(female,45.5+(2.3*(ht.inches-60)),50+(2.3*(ht.inches-60)))
# }
#
# #' Calculate Creatinine Clearance
# #' @param age.yrs Age in years
# #' @param wt.kg Weight in kilos
# #' @param female TRUE=female, FALSE=male
# #' @param creat Serum creatinine
# #' @param ht.cm.adjust.bmi If TRUE, adjust weight based on extreme BMI
# #' @author Ying Taur
# #' @export
# creatinine.clearance <- function(age.yrs,wt.kg,female,creat,ht.cm.adjust.bmi=NULL) {
#   #by cockroft-gault. is
#   if (is.null(ht.cm.adjust.bmi)) {
#     weight <- wt.kg
#   } else {
#     bmi.calc <- bmi(ht.cm.adjust.bmi,wt.kg)
#     ideal <- ibw(female,ht.cm.adjust.bmi)
#     adjusted <- ideal + 0.4*(wt.kg-ideal)
#     weight <- ifelse(bmi.calc<18.5,wt.kg,ifelse(bmi.calc>=23,adjusted,ideal))
#   }
#   (140-age.yrs) * (weight) * ifelse(female,0.85,1) / (72 * creat)
# }
#
#

#
# #' Translate antibiotic code
# #'
# #' Given a vector of codes, returns the antibiotics. \code{NA} if no match.
# #' @export
# antibiotic.code <- function(code) {
#   dict <- c(AMK="amikacin",AMP="ampicillin",AMS="ampicillin-sulbactam",AUG="amoxacillin-clavulanate",AZT="aztreonam",
#             BLAC="beta lactamase",CAX="ceftriaxone",CAZ="ceftazidime",CD="clindamycin",CF="cephalothin",CFT="cefotaxime"
#             ,CFX="cefoxitin",CFZ="cefazolin",CH="chloramphenicol",CIP="ciprofloxacin",CLA="clarithromycin",CPD="cefpodoxime",
#             CPM="cefepime",CRM="cefuroxime",CTN="cefotetan",DAP="daptomycin",DOX="doxycycline",E="erythromycin",
#             ERT="ertapenem",FUR="nitrofurantoin",GAT="gatifloxacin",GN="gentamicin",GNS="gentamicin synergy",
#             GREP="grepafloxacin",IM="imipenem",LEV="levofloxacin",LNZ="linezolid",LZD="linezolid",MEN="S pneumoniae meningitis",
#             MER="meropenem",MET="metronidazole",MXF="moxifloxacin",MZ="mezlocillin",NXN="norfloxacin",OFL="ofloxacin",
#             OX="oxacillin",PEN="penicillin",PI="piperacillin",PX="polymyxin B",RIF="rifampin",SCOM="special comment",
#             STS="streptomycin synergy",SX="sulfazoxole",SYD="quinupristin-dalfopristin",TAZ="piperacillin-tazobactam",
#             TE="tetracycline",TIC="ticarcillin",TIG="tigecycline",TIM="ticarcillin-clavulanate",TO="tobramycin",
#             TRV="trovafloxacin",TS="trimethoprim-sulfamethoxazole",TV="trovafloxacin",TZP="piperacillin-tazobactam",
#             V="vancomycin",ZITH="azithromycin")
#   dict[match(code,names(dict))]
# }
#
#
# #' ...Title...
# #'
# #' ...Description...
# #'
# #' @usage ...usage.code...
# #'
# #' ...details...
# #' ***********************************
# #' Some quick latex:
# #' \emph{italics}, \strong{bold}, \code{<r-code>}, \pkg{package_name}
# #' \code{\link{function}} will be a link to another function
# #' \enumerate{ numbered list
# #'   \item First item
# #'   \item Second item
# #' }
# #' \itemize{ bulleted list
# #'   \item First item
# #'   \item Second item
# #' }
# #' \describe{ definition list
# #'   \item{One}{First item}
# #'   \item{Two}{Second item}
# #' }
# #' note that you can use refer to another rd file:
# #' <at>describeIn <title> <blurb>
# #' <at>rdname <title>
# #' ***********************************
# #'
# #' @param .param1. ...param1.description...
# #' @param .param2. ...param2.description...
# #' @return ...description.of.data.returned...
# #' @examples
# #' ...examples.here....
# #' @keywords keyword1 keyword2 ...
# #' @seealso \code{\link{cdiff.method}}
# #' @author Ying Taur
# #' @export
# function.template <- function() {
#   1+1
# }
#
#
#
#
#
#
# #
# # #' @export
# # preview <- function(df,pdffile="temp.pdf",width=256,height=180) {
# #   #given dataframe df, show graphical overview of each variable. Saves to a pdf file.
# #   plot.var <- function(df,var) {
# #     n.vals <- length(unique(df[,var]))
# #     na.vals <- length(which(is.na(df[,var])))
# #     xlabel <- paste0(var, " (",class(df[,var]),")\n",
# #                      n.vals," values, ",
# #                      na.vals," missing")
# #     if (nrow(df)==na.vals) { #big box that says it's all NA
# #       ggplot(bb) + geom_text(aes(x=1,y=1,label="All NA")) + xlab(xlabel) +
# #         theme(axis.title.x = element_text(colour="#990000"))
# #     } else {
# #       ggplot(df,aes_string(x=var)) + geom_bar() + xlab(xlabel)+
# #         theme(axis.title.x = element_text(colour="#990000"))
# #     }
# #   }
# #   #get screen resolution and divide by plotsize
# #   screen.width <- screen.resolution()[1]
# #   screen.height <- screen.resolution()[2]
# #   nplots.width <- floor(screen.width/width)
# #   nplots.height <- floor(screen.height/height)
# #   nplots.total <- nplots.width *nplots.height
# #   vars <- names(df)
# #
# #   pdf(pdffile,width=40,height=20)
# #   i <- 1
# #   while (i<length(vars)) {
# #     vars.subset <- names(df[,i:min(i+nplots.total-1,length(vars))])
# #     plots <- lapply(vars.subset,function(x) plot.var(df,x))
# #     do.call(grid.arrange,c(plots,nrow=nplots.height,ncol=nplots.width))
# #     i <- i + nplots.total
# #   }
# #   dev.off()
# #   shell.exec(pdffile)
# # }
# #
# # #' Modified version of dplyr's filter that uses string arguments
# # #' @export
# # s_filter = function(.data, ...) {
# #   eval.string.dplyr(.data,"filter", ...)
# # }
# #
# # #' Modified version of dplyr's select that uses string arguments
# # #' @export
# # s_select = function(.data, ...) {
# #   eval.string.dplyr(.data,"select", ...)
# # }
# #
# # #' Modified version of dplyr's arrange that uses string arguments
# # #' @export
# # s_arrange = function(.data, ...) {
# #   eval.string.dplyr(.data,"arrange", ...)
# # }
# #
# # #' Modified version of dplyr's arrange that uses string arguments
# # #' @export
# # s_mutate = function(.data, ...) {
# #   eval.string.dplyr(.data,"mutate", ...)
# # }
# #
# # #' Modified version of dplyr's summarise that uses string arguments
# # #' @export
# # s_summarize = function(.data, ...) {
# #   eval.string.dplyr(.data,"summarise", ...)
# # }
# #
# # #' Modified version of dplyr's group_by that uses string arguments
# # #' @export
# # s_group_by = function(.data, ...) {
# #   eval.string.dplyr(.data,"group_by", ...)
# # }
# #
# # #' Internal function used by s_filter, s_select etc.
# # eval.string.dplyr = function(.data, .fun.name, ...) {
# #   args = list(...)
# #   args = unlist(args)
# #   code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
# #   df = eval(parse(text=code,srcfile=NULL))
# #   df
# # }
# #
#
# #
# # #' Given plot list, arrange into plots for pages.
# # #' @export
# # plot.pages <- function(plot.list,nrow=3,ncol=5,pdf.file="plots.pdf",pdf.width=40,pdf.height=20) {
# #   #plot.list <- km.plots;nrow=5;ncol=5;pdf.width=40;pdf.height=20;pdf.file="plots.pdf";pdf.width=40;pdf.height=20
# #   n.plots <- length(plot.list)
# #   plots.per.page <- nrow * ncol
# #   n.pages <- ceiling(n.plots/plots.per.page)
# #   first.index <- (plots.per.page * 0:(n.pages-1)) + 1
# #   print(paste0("Pages: ",n.pages))
# #   print(paste0("Total plots: ",n.plots))
# #   print(paste0("Plots per page: ",plots.per.page))
# #   pdf(pdf.file,width=pdf.width,height=pdf.height)
# #   for (x in first.index) {
# #     last.index <- min(x+plots.per.page-1,n.plots)
# #     page <- plot.list[x:last.index]
# #     do.call(grid.arrange,c(page,nrow=nrow,ncol=ncol))
# #   }
# #   dev.off()
# #   pdf.file
# # }
# #
# # tab <- function(var,sortby.freq=TRUE,useNA="ifany",as.char=FALSE) {
# #   tbl <- data.frame(table(var,useNA=useNA)) %>% dplyr::rename(freq=Freq) %>% mutate(pct=percent(prop.table(freq)))
# #   if (sortby.freq) {
# #     tbl <- tbl %>% arrange(desc(freq))
# #   }
# #   if (as.char) {
# #     char <- paste(tbl$var,": n=",tbl$freq," (",tbl$pct,")",sep="",collapse="\n")
# #     return(char)
# #   } else {
# #     return(tbl)
# #   }
# # }
# #
# #
# # # @export
# # merge.left <- function(x,y,by) {
# #   #performs a left join, using merge with all.x.
# #   #all this does is make sure x doesn't change size.
# #   #if y[,by] has any NAs, remove them.
# #   anyNA <- apply(as.matrix(y[,by]),1,function(x) any(is.na(x)))
# #   y <- y[!anyNA,]
# #   if (ncol(x)>ncol(y)) {
# #     merged.data <- merge(x,y,by=by,all.x=TRUE,sort=FALSE)
# #   } else {
# #     merged.data <- merge(y,x,by=by,all.y=TRUE,sort=FALSE)
# #   }
# #   if (!setequal(x[,by],merged.data[,by])) {
# #     stop("YTError, x[,by] altered by the merge!")
# #   }
# #   return(merged.data)
#
# # }
#
# # #' @export
# # if.adply <- function(test, .data, ... ) {
# #   #same as adply, but only apply to rows meeting test criteria.
# #   data.true <- .data[test,]
# #   data.false <- .data[!test,]
# #   data.true <- adply(data.true, ... )
# #   rbind(data.true,data.false)
# # }
#
# # #' OLD!
# # #' @export
# # trans.scantabundance <- function(trans_name,epsilon=0.001) {
# #   #transformation that's useful for relative abundances.
# #   #epsilon controls where on the log scale to transform to (closer to 0 magnifies low numbers)
# #   #yttrans_trans <- trans.scantabundance("yttrans")
# #   #ggplot(sm,aes(x=genus.species,y=value)) + geom_bar(stat="identity") + coord_trans(y="yttrans")
# #   f1 <- function(x) sign(x)*(log(abs(x)+epsilon)-log(epsilon))
# #   f2 <- function(y) sign(y)(epsilon*(exp(abs(y))-1))
# #   return(function() trans_new(trans_name,f1,f2))
# # }
#
# # #these are old.....
# # #' @export
# # is.Date <- function(x,...) UseMethod("is.Date")
# # #' @export
# # is.Date.default <- function(var) {
# #   date.pattern <- c("^(19|20)[0-9]{2}-[0-9]{1,2}-[0-9]{1,2} [0-9]{2}:[0-9]{2}:[0-9]{2}")
# #   all(grepl(paste(date.pattern,collapse="|"),var) | (is.na(var)) & !all(is.na(var))) | class(var)[1]=="POSIXct"
# # }
# #
# # #' @export
# # is.Date.data.frame <- function(data) {
# #   #logical vector of names in data, true if date.
# #   sapply(data,is.Date)
# # }
#
# #
# # #' @export
# # as.Date2 <- function(vec) {
# #
# #   if (all(is.na(vec)) | class(vec)[1]=="Date") {
# #     return(vec)
# #   } else if (class(vec)[1]=="POSIXct") {
# #     return(as.Date(vec))
# #   }
# #   vec2 <- vec
# #   vec2[vec2==""] <- NA
# #   #9/27/1945 or 9-27-1945
# #   if (all(grepl("^[01]?[0-9][-/][0-3]?[0-9][-/](19|20)[0-9]{2}$",vec2[!is.na(vec2)]))) {
# #     return(as.Date(vec2,"%m/%d/%Y"))
# #   }
# #   #"2002-09-23" or "2002/09/23"
# #   if (all(grepl("^(19|20)[0-9]{2}[-/][01]?[0-9][-/][0-3]?[0-9]$",vec2[!is.na(vec2)]))) {
# #     return(as.Date(vec2))
# #   }
# #   #"4-Aug-11" or "27-Oct-11"
# #   if (all(grepl("^[0-3]?[0-9]-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-[0-9]{2}",vec2[!is.na(vec2)],ignore.case=TRUE))) {
# #     return(as.Date(vec2,"%d-%b-%y"))
# #   }
# #   #"1925-08-05 00:00:00"
# #   if (all(grepl("^(19|20)[0-9]{2}[-/][01]?[0-9][-/][0-3]?[0-9] 00:00:00$",vec2[!is.na(vec2)]))) {
# #     return(as.Date(vec2))
# #   }
# #   #1925-08-05 23:03:19 -> chron object
# #   if (all(grepl("^(19|20)[0-9]{2}[-/][01]?[0-9][-/][0-3]?[0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]$",vec2[!is.na(vec2)]))) {
# #     dt <- as.data.frame(do.call(rbind,strsplit(vec2," ")),stringsAsFactors=FALSE)
# #     vec2.chron <- rep(NA,nrow(dt)) #this is the only way to avoid the NA warning
# #     vec2.chron[!is.na(dt[,1])] <- chron(dt[!is.na(dt[,1]),1],dt[!is.na(dt[,1]),2],format=c('y-m-d','h:m:s'))
# #     return(as.chron(vec2.chron))
# #   }
# #   #2012-02-27-08.34.00.000000 -> chron object
# #   if (all(grepl("^(19|20)[0-9]{2}-[01][0-9]-[0-3]?[0-9]-[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{6}",vec2[!is.na(vec2)]))) {
# #     dt <- substr(vec,1,10)
# #     tm <- substr(vec,12,26)
# #     tm <- sub("\\.",":",tm)
# #     tm <- sub("\\.",":",tm)
# #     vec2.chron <- chron(dt,tm,format=c("y-m-d","h:m:s"))
# #     return(vec2.chron)
# #   }
# #
# #   #otherwise no change
# #   return(vec)
# # }
# # #' ...Title...
# # #'
# # #' ...Description...
# # #'
# # #' @usage ...usage.code...
# # #'
# # #' ...details...
# # #' ***********************************
# # #' Some quick latex:
# # #' \emph{italics}, \strong{bold}, \code{<r-code>}, \pkg{package_name}
# # #' \code{\link{function}} will be a link to another function
# # #' \enumerate{ numbered list
# # #'   \item First item
# # #'   \item Second item
# # #' }
# # #' \itemize{ bulleted list
# # #'   \item First item
# # #'   \item Second item
# # #' }
# # #' \describe{ definition list
# # #'   \item{One}{First item}
# # #'   \item{Two}{Second item}
# # #' }
# # #' note that you can use refer to another rd file:
# # #' <at>describeIn <title> <blurb>
# # #' <at>rdname <title>
# # #' ***********************************
# # #'
# # #' @param .param1. ...param1.description...
# # #' @param .param2. ...param2.description...
# # #' @return ...description.of.data.returned...
# # #' @examples
# # #' ...examples.here....
# # #' @keywords keyword1 keyword2 ...
# # #' @seealso \code{\link{cdiff.method}}
# # #' @author Ying Taur
# # #' @export
# # mothur.taxsummary <- function(taxfile,groupfile,namefile) {
# #   command <- paste0("summary.tax(taxonomy=",taxfile,",name=",namefile,",group=",groupfile,")")
# #   mothur(command)
# # }
# # #' Mothur command
# # #'
# # #' Executes commands in mothur
# # #'
# # #' @usage ...usage.code...
# # #'
# # #' ...details...
# # #' ***********************************
# # #' Some quick latex:
# # #' \emph{italics}, \strong{bold}, \code{<r-code>}, \pkg{package_name}
# # #' \code{\link{function}} will be a link to another function
# # #' \enumerate{ numbered list
# # #'   \item First item
# # #'   \item Second item
# # #' }
# # #' \itemize{ bulleted list
# # #'   \item First item
# # #'   \item Second item
# # #' }
# # #' \describe{ definition list
# # #'   \item{One}{First item}
# # #'   \item{Two}{Second item}
# # #' }
# # #' note that you can use refer to another rd file:
# # #' <at>describeIn <title> <blurb>
# # #' <at>rdname <title>
# # #' ***********************************
# # #'
# # #' @param .param1. ...param1.description...
# # #' @param .param2. ...param2.description...
# # #' @return ...description.of.data.returned...
# # #' @examples
# # #' ...examples.here....
# # #' @keywords keyword1 keyword2 ...
# # #' @seealso \code{\link{cdiff.method}}
# # #' @author Ying Taur
# # #' @export
# # # mothur <- function(command) {
# # #   shell(paste0("mothur \"#",command,"\""))
# # # }
# # #' ...Title...
# # #'
# # #' ...Description...
# # #'
# # #' @usage ...usage.code...
# # #'
# # #' ...details...
# # #'
# # #' @param .param1. ...param1.description...
# # #' @param .param2. ...param2.description...
# # #' @return ...description.of.data.returned...
# # #' @examples
# # #' ...examples.here....
# # #' @keywords keyword1 keyword2 ...
# # #' @seealso \code{\link{cdiff.method}}
# # #' @author Ying Taur
# # #' @export
# # get.winpath <- function() {
# #   #obtain path from clipboard and produce the path formatted into R with forward slashes.
# #   path <- readClipboard()
# #   formatted.path <- gsub("\\\\","/",path)
# #   copytoClipboard(formatted.path)
# #   print("Changed clipboard to forward slash path.")
# #   return(formatted.path)
# # }
#
# # #' Percent format
# # #'
# # #' Displays number as a nicely formatted percent.
# # #'
# # #' Convenience function. Can typically use this for graphs. Uses function \code{formatC} to accomplish this.
# # #' @param x the number to be formatted.
# # #' @param digits number of decimal places to display.
# # #' @param format character denoting the format to use. This is the parameter used by \code{formatC}
# # #' @param ... remaining arguments are passed to \code{formatC}
# # #' @return Returns a data frame formatted to be Table 1 in a manuscript.
# # #' @examples
# # #' percent.format(0.65)
# # #' percent.format(0.65,digits=1)
# # #' @author Ying Taur
# # #' @export
# # percent.format <- function(x,digits=2,format="f", ...) {
# #   #formats numeric as percent
# #   paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
# # }
#
# # recode.grep.old <- function(var,recodes,else.value,as.factor,ignore.case=TRUE) {
# #   #recodes a character vector based on recodes, a vector of reg expressions
# #   # elsevalue: default is to use old value.
# #   # as.factor: default is to base it on whether original var is a factor
# #   if (missing(as.factor)) {
# #     as.factor <- is.factor(var)
# #   }
# #   d <- data.frame(var=as.character(var),newvar=as.character(var),stringsAsFactors=FALSE)
# #   if (!missing(else.value)) {
# #     d$newvar <- else.value
# #   }
# #   for (i in 1:length(recodes)) {
# #     d$newvar[grepl(names(recodes[i]),d$var,ignore.case=ignore.case)] <- recodes[i]
# #   }
# #   if (as.factor) {
# #     if (missing(else.value)) {
# #       d$newvar <- factor(d$newvar)
# #     } else {
# #       d$newvar <- factor(d$newvar,levels=c(recodes,else.value))
# #     }
# #   }
# #   return(d$newvar)
# # }
# #
# # replace.grep <- function(var,renames,ignore.case=TRUE) {
# #   new.var <- var
# #   for (i in 1:length(renames)) {
# #     new.var <- gsub(names(renames)[i],renames[i],new.var,ignore.case=ignore.case)
# #   }
# #   return(new.var)
# # }


