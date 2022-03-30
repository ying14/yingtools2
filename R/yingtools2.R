


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


#' Regular Expression Operator
#'
#' Shorthand operator for regular expression.
#' @export
"%like%" = function(x,y) {
  grepl(y,x,ignore.case=TRUE)
}



#' Find Regular Expression Operator
#'
#' Shorthand operator for finding a pattern.
#' @export
"%find%" = function(x,y) {
  grep(y,x,ignore.case=TRUE,value=TRUE)
}


#' Inner/Left/Right/Full Join with Replace
#'
#' Same as \code{inner_join}, \code{left_join}, \code{right_join}, and \code{full_join} in the \code{dplyr} package, except that variables with the
#' same column name will not be renamed with the ".x" and ".y" suffix.
#' Instead, the variables will be turned into one column if the variables are equal. If they are not equal, an error (or warning) is thrown.
#'
#' This is a convenience function that just avoids the renaming of columns.
#' @param errorIfDifferent whether to throw an error if a difference is detected (default is \code{TRUE})
#'
#' @export
inner_join_replace <- function(x,y,by=NULL,errorIfDifferent=TRUE) {
  requireNamespace("stringi",quietly=TRUE)

  data <- inner_join(x,y,by=by)
  x.vars0 <- str_extract_all(names(data),"(?<=^).+(?=\\.x$)") %>% unlist()
  y.vars0 <- str_extract_all(names(data),"(?<=^).+(?=\\.y$)") %>% unlist()
  overlap.vars <- intersect(x.vars0,y.vars0)
  is.identical <- overlap.vars %>% map_lgl(~{
    xvar <- stringi::stri_join(.x,".x")
    yvar <- stringi::stri_join(.x,".y")
    identical(data[[xvar]],data[[yvar]])
  })
  ident.vars <- overlap.vars[is.identical]
  non.ident.vars <- overlap.vars[!is.identical]
  ident.vars.x <- stringi::stri_join(ident.vars,".x")
  ident.vars.y <- stringi::stri_join(ident.vars,".y")
  non.ident.vars.x <- stringi::stri_join(non.ident.vars,".x")
  non.ident.vars.y <- stringi::stri_join(non.ident.vars,".y")
  if (length(non.ident.vars)>0) {
    if (errorIfDifferent) {
      stop(str_glue("YTError: overlapping variables do not match in value: {paste(non.ident.vars,collapse=\",\")}"))
    } else {
      warning(str_glue("YTWarning: overlapping variables have different values. These will be kept separate: {paste(sort(c(non.ident.vars.x,non.ident.vars.y)),collapse=\", \")}"))
    }

  }
  data2 <- data %>% select(-all_of(ident.vars.x)) %>%
    rename(!!!rlang::syms(setNames(ident.vars.y,ident.vars)))
  #double check names
  old.names <- c(names(x),names(y))
  new.names <- names(data2)
  if (!setequal(old.names,new.names)) {
    warning(str_glue("YTWarning: weird name change: {paste(setdiff(old.names,new.names),setdiff(new.names,old.names),sep=\", \")}"))
  }
  return(data2)
}



#' @rdname inner_join_replace
#' @export
left_join_replace <- function(x,y,by=NULL,errorIfDifferent=FALSE) {
  data1 <- inner_join_replace(x,y,by=by,errorIfDifferent=errorIfDifferent)
  data2 <- anti_join(x,y,by=by)
  bind_rows(data1,data2)
}




#' @rdname inner_join_replace
#' @export
right_join_replace <- function(x,y,by=NULL,errorIfDifferent=FALSE) {
  left_join_replace(y,x,by=by,errorIfDifferent=errorIfDifferent)
}




#' @rdname inner_join_replace
#' @export
full_join_replace <- function(x,y,by=NULL,errorIfDifferent=FALSE) {
  data1 <- inner_join_replace(x,y,by=by,errorIfDifferent=errorIfDifferent)
  data2 <- anti_join(x,y,by=by)
  data3 <- anti_join(y,x,by=by)
  bind_rows(data1,data2,data3)
}




#' Compare data frames
#'
#' @param x table to be compared
#' @param y table to be compared
#' @param by varaible(s) to join by and compare
#' @return a table from full_join(x,y,by). Contains columns .status and .diffs.
#' @export
compare <- function(x,y,by=NULL) {
  if (is.null(by)) {
    by <- intersect(names(x),names(y))
  }
  by.x <- names(by) %||% by
  by.y <- unname(by)
  x.is.distinct <- x %>% is.distinct(!!!syms(by.x))
  y.is.distinct <- y %>% is.distinct(!!!syms(by.y))

  if (by.x==by.y) {
    by.x <- paste0(by.x,".x")
    by.y <- paste0(by.y,".y")
  }

  # both <- inner_join_replace(x,y,by=by,errorIfDifferent = FALSE)
  all <- full_join(x,y,by=by,keep=TRUE) %>%
    mutate(.status=case_when(
      !is.na(!!sym(by.x)) & !is.na(!!sym(by.y)) ~ "both x and y",
      !is.na(!!sym(by.x)) & is.na(!!sym(by.y)) ~ "x only",
      is.na(!!sym(by.x)) & !is.na(!!sym(by.y)) ~ "y only"
    ))
  x.vars0 <- str_extract_all(names(all),"(?<=^).+(?=\\.x$)") %>% unlist()
  y.vars0 <- str_extract_all(names(all),"(?<=^).+(?=\\.y$)") %>% unlist()
  overlap.vars <- intersect(x.vars0,y.vars0)

  for (var in overlap.vars) {
    var.x <- paste0(var,".x")
    var.y <- paste0(var,".y")
    diff <- all[[var.x]]!=all[[var.y]]
    diff <- !is.na(diff) & diff
  }

  diffs <- map(overlap.vars,~{
    var.x <- paste0(.x,".x")
    var.y <- paste0(.x,".y")
    diff <- all[[var.x]]!=all[[var.y]]
    diff <- !is.na(diff) & diff
    ifelse(diff,.x,NA_character_)
  }) %>% set_names(overlap.vars) %>%
    do.call(paste2,.)
  all$.diffs <- diffs
  message(str_glue("x: {pretty_number(nrow(x))} rows ({ifelse(x.is.distinct,\"distinct\",\"not distinct\")})"))
  message(str_glue("y: {pretty_number(nrow(y))} rows ({ifelse(y.is.distinct,\"distinct\",\"not distinct\")})"))
  all %>% count(.status,.diffs) %>% print()
  all
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


#' Sample N Groups
#'
#' Sample groups from a grouped data frame
#' @param grouped_df the grouped data frame to be sampled
#' @param size number of groups to sample
#' @return a subset of the grouped data frame
#' @export
#' @examples
#' gdf <- mtcars %>% group_by(gear,carb)
#' sample_n_groups(gdf,3)
sample_n_groups <- function(grouped_df, size) {
  dplyr::group_data(grouped_df) %>%
    dplyr::sample_n(size) %>%
    dplyr::select(-.rows) %>%
    dplyr::inner_join(grouped_df,by=dplyr::group_vars(grouped_df)) %>%
    dplyr::group_by(!!!groups(grouped_df))
}


#' Ying's DT view
#'
#' Use to peruse a dataframe within RStudio. Utilizes \code{DT} package.
#'
#' If data frame is grouped (i.e. \code{group_by} in dplyr), the rows will be sorted and shaded by group.
#'
#' @param data dataframe to be viewed.
#' @param fontsize numeric controlling font size in the table, measured in px. Default is 11.
#' @param maxchars max number of characters before adding an ellipsis \code{...}. Default is 250.
#' @param whiteSpace CSS property sets how white space inside an element is handled. Default is "pre-wrap".
#' @param pageLength number of rows to display per page (Default \code{Inf}, show all rows)
#' @param maxrows numeric controlling max number of rows to display. The purpose is to prevent \code{DT} from handling excessively large data frames. Default is 1000.
#' @param rownames whether or not to show row names (passed directly to \code{\link[DT:datatable]{DT::datatable}}).
#' @param class the CSS class(es) of the table (passed directly to \code{\link[DT:datatable]{DT::datatable}}).
#' @param escape whether to escape HTML entities in the table (passed directly to \code{\link[DT:datatable]{DT::datatable}}).
#'
#' @return A javascript-style datatable, which displays in the Rstudio viewer.
#' @examples
#' library(dplyr)
#' mtcars %>% dt()
#' mtcars %>% group_by(cyl) %>% dt()
#' @author Ying Taur
#' @export
dt <- function(data,fontsize=11,pageLength=Inf,maxchars=250,maxrows=500,rownames=FALSE,escape=FALSE,class="compact cell-border stripe",whiteSpace="pre-wrap") {
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
    text <- rlang::quo_text(expr)
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

#' Evaluate expression in another R session
#'
#' Use these to run code in a separate instance of R, separate from your current console.
#' This is essentially doing what the \code{callr} package does, but just adds a few modifications to make it easier to use.
#'
#' In some cases it is useful to run code in a separate R session... this is where functions from the \code{callr} package
#' come in handy, such as \code{r()} or \code{r_bg()}. However, in order to use these, you have to be sure to:
#' (1) place the code inside an anonymous function,
#' (2) refer to functions and variables explicitly from other packages using the :: notation,
#' (3) pass any necessary local variables as arguments to the anonymous function.
#' \code{run_r()} and \code{run_r_bg()} are running \code{callr::r()} and \code{callr::r_bg()}, except that you can
#' just insert the code without worrying about about the 3 modifications above.
#'
#' @param expr expression to be run in separate R session.
#' @param envir the environment to execute the code.
#'
#' @return For \code{run_r}: the value of the evaluated expression.
#' For \code{run_r_bg}: an \code{r_process} object, which has a \code{get_result()} method to collect the result.
#' For \code{run_r_callargs}: a list containing a modified function and a list of arguments to pass to the function;
#' this is designed to be the arguments that can be plugged into \code{callr::r} or \code{callr::r_bg}.
#' @export
#' @examples
run_r <- function(expr,envir=parent.frame()) {
  requireNamespace("callr",quietly=TRUE)
  expr <- rlang::enexpr(expr)
  call.arglist <- run_r_callargs(!!expr,envir=envir)
  do.call(r,call.arglist)
}

#' @rdname run_r
#' @export
run_r_bg <- function(expr,envir=parent.frame()) {
  requireNamespace("callr",quietly=TRUE)
  expr <- rlang::enexpr(expr)
  call.arglist <- run_r_callargs(!!expr,envir=envir)
  do.call(callr::r_bg,call.arglist)
}

#' @rdname run_r
#' @export
run_r_callargs <- function(expr,envir=parent.frame()) {
  requireNamespace(c("stringr","callr"),quietly=TRUE)
  expr <- rlang::enexpr(expr)
  text <- rlang::quo_text(expr)
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
#' Similar to \code{shiny::runGadget}, you can use this to run shiny apps in the viewer pane of RStudio.
#' The difference is that the R console is not blocked during execution,
#' so you can continue coding while the shiny app is running.
#'
#' This function works by deploying the Shiny app is run in the background
#' (using \code{callr::r_bg}), then having the viewer panel set to display the corresponding port.
#' If a process already exists that is listening to the port, that process is killed (using \code{kill_port_process}.
#'
#' @param app A Shiny app object created by shinyApp()
#' @param port The TCP port that the application should listen on.
#' @return An \code{r_process} object, which is running separately in the background.
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
  expr <- enquo(expr)
  requireNamespace(c("shiny","callr","pingr"),quietly=TRUE)
  localhost <- "127.0.0.1"
  url <- paste0("http://",localhost,":",port)
  kill_port_process(port)
  ps <- r_bg(function(...) {
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


#' Paste 2
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
#' Similar to \code{min}/\code{max} command, except that if the data is empty, the function returns
#' \code{NA} instead of \code{+/-Inf}.
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
  suppressWarnings({
    val <-   max(...,na.rm=na.rm)
  })
  val[is.infinite(val)] <- NA
  return(val)
}

#' @describeIn max2 \code{max2} minimum value.
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
#' @param quantiles an integer specifying number of quantiles.
#' @param percentiles a vector of percentile breakpoints
#' @param lvls optional vector for renaming the levels
#' @return a factor derived from grouping of x
#' @export
cut2 <- function(x,lower,upper,quantiles,percentiles,lvls) {
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

  if (rlang::quo_is_null(id_cols)) {
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
    system(paste("xdg-open",file),wait=FALSE)
  } else if (Sys.info()['sysname']=="Windows") {
    base::shell.exec(file)
  } else {
    stop("YTError: Not sure how to handle this operating system: ",Sys.info()["sysname"],"\nGo tell Ying about this.")
  }
}

#' Copy to Clipboard
#'
#' Copies object to the clipboard, which can be used to paste into other programs such as Word or Excel.
#'
#' This is now done using the \code{clipr} package. Previously I did this manually for each operating system.
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
#' as header (specify \code{header=FALSE} if necessary). If first cell is blank, it will assume row and column names.
#' Note: This is now done using the \code{clipr} package. Previously I did this manually for each operating system.
#' @param ... Options to pass to \link[utils]{read.table} (e.g. header, row.names, sep, as.is)
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



#' Convert object to R-code.
#'
#' Produces R-code that would create the object inputted. I use this if I have some data object that I obtained
#' somehow but just want to declare it in the code.
#'
#' @param x object to be converted to R-code. Can be vector or data frame.
#' @param fit whether to insert carriage returns, in order to fit code on the screen (default \code{TRUE})
#' @param width max character width of each line, if \code{fit=TRUE}. Default is \code{getOption("width")-15}
#' @param copy.clipboard logical, if \code{TRUE}, will copy the R-code to the Clipboard.
#'
#' @return Returns the R-code.
#' @examples
#' x <- c("a","b","c")
#' copy.as.Rcode(x)
#' x <- tibble("a"=1:4,"_b"=c(T,F,T,F),"c c"=Sys.Date()+4:1,"d"=factor(LETTERS[1:4]))
#' copy.as.Rcode(x)
#' @author Ying Taur
#' @export
copy.as.Rcode <- function(x,copy.clipboard=TRUE,fit=TRUE,width=getOption("width")-15) {
  #converts x to R-code.
  if (is.atomic(x)) {
    if (is.Date(x)) {
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
    } else if (is.logical(x) | is.numeric(x) | is.character(x)){
      rcode <- deparse(x) %>% paste(collapse="")
    }
  } else { ##### not atomic
    if (is.data.frame(x)) {
      df_names <- names(x)
      df_names <- ifelse(df_names==make.names(df_names),df_names,paste0("\"",df_names,"\""))
      x.cols <- sapply(x,copy.as.Rcode,copy.clipboard=FALSE)
      x.cols <- mapply(function(varname,var) paste0(varname,"=",var),df_names,x.cols)
      rcode <- paste(x.cols,collapse=",\n")
      rcode <- paste0("tibble(",rcode,")")
    } else if (is.list(x)) {
      x.cols <- sapply(x,copy.as.Rcode,copy.clipboard=FALSE)
      x.cols <- mapply(function(varname,var) paste0("\"",varname,"\"=",var),names(x),x.cols)
      rcode <- paste(x.cols,collapse=",\n")
      rcode <- paste0("list(",rcode,")")
    } else {
      rcode <- deparse(x)
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
#' # Should be a 3 item python list, with middle item being empty.
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
#' values <- c("35171234","35507574")
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
    # data2 <- mutate_all(x,funs(format.value))
    data2 <- mutate_all(x,format.value)
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



#' Display sizes of objects in memory
#'
#' Use this to see what is occupying memory
#' @param envir the environment to list objects. Default is \code{.GlobalEnv}
#' @return a data frame showing objects and the object size, in Mb.
#' @export
ls.object.sizes <- function(envir=.GlobalEnv) {
  objects <- ls(envir=envir)
  if (length(objects)==0) {
    message("No objects found.")
    return(NULL)
  }
  dsize <- lapply(objects,function(objname) {
    obj <- get(objname)
    size <- object.size(obj)
    bytes <- as.numeric(size)
    mb <- format(size,units="Mb")
    class <- class(obj)[1]
    tibble(obj=objname,class,bytes,mb)
  }) %>% bind_rows() %>% arrange(desc(bytes)) %>% select(-bytes)
  return(dsize)
}

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
#' @param abbrev named vector specifying the log base 10 cutoff values and their assigned label. Default is \code{c(K=3,M=6,B=9)}.
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
  requireNamespace("scales",quietly=TRUE)
  ep8 <- epsilon/8
  trans <- function(x) sign(x)*(log(abs(x)+epsilon/8)-log(epsilon/8))
  inv <- function(y) sign(y)*epsilon/8*(exp(abs(y))-1)
  scales::trans_new(paste0("log_epsilon-",format(epsilon)),trans,inv,
            breaks=log_epsilon_trans_breaks(epsilon),
            format=pretty_number,
            domain=c(-Inf,Inf))
}


#' Breaks for Log Epsilon Tranformation
#'
#' This is used by scant_trans as default method for breaks. Will fill in logs of 10.
#' @param epsilon scaling parameter used in \code{log_epsilon_trans}
#' @return break function returning break values.
#' @export
log_epsilon_trans_breaks <- function(epsilon) {
  function(x) {
    firsttick <- round(log(epsilon,10))
    lasttick <- floor(log(x[2],10))
    c(0,10^(firsttick:lasttick))
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
#' @param invert whether or not to flip the logistic curve. Default is \code{FALSE}.
#' @return Returns the logistic transformation of \code{var}, where values will fall within \code{scale}, and where \code{inner.range} will be transformed to \code{percentiles}.
#' @examples
#' #Example: WBC. Values between 0.2 and 10 take up 80% of the space. Values outside of that de-emphasized.
#' wbc <- seq(0,20,by=0.1)
#' wbc.logist <- trans.logistic(wbc,inner.range=c(0.2,10))
#' ggplot(data.frame(wbc,wbc.logist)) + geom_point(aes(x=wbc,y=wbc.logist))
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

#' Stack and line up ggplot objects in a column
#'
#' Use this to arrange ggplot objects, where the axes, plot, and legend are lined up correctly.
#'
#' Performs these steps:
#' (1) change margins so that plots are closer together
#' (2) alters widths of each component so that the plots will line up nicely
#' (3) calls \code{grid.arrange(...,ncol=1)}
#' If a \code{NULL} value is passed to the plot list, that plot and the corresponding height value will be omitted.
#' @param ... ggplot objects to be stacked
#' @param heights a numeric vector representing the relative height of each plot. Passed directly to \code{grid.arrange}.
#' @param adjust.themes logical, whether or not to adjust each plot's theme for stacking (change gap/margin, suppress x-axis in upper plots). Default \code{TRUE}.
#' @param gg.extras a list of ggplot objects that will be applied to all plots. Default is \code{NULL}.
#' @param gap size of gap between stacked plots. Default is 0
#' @param margin size of the margin around the plots. Default is 5.5.
#' @param units specifies units used for gap and margin. Default is "pt"
#' @param newpage logical, whether or not to erase current grid device. Default is TRUE. (Note, should turn this off if using in a shiny plot)
#' @param as.gtable logical, whether or not to return as a gtable object (i.e. don't execute \code{grid.draw}). Default is \code{FALSE}. Do this if you want to do more arranging afterwards.
#' @return plot of stacked ggplots
#' @export
gg.stack <- function(...,heights=NULL,adjust.themes=TRUE,gg.extras=NULL,gap=0,margin=5.5,units="pt",newpage=TRUE,as.gtable=FALSE) {
  requireNamespace(c("grid","gridExtra","gtable"),quietly=TRUE)

  grobs <- list(...)
  keep <- !sapply(grobs,is.null)

  if (!is.null(heights)) {
    if (length(grobs)!=length(heights)) {
      stop("YTError: number of grobs does not match the number of heights.")
    }
    heights <- heights[keep]
  }
  grobs <- grobs[keep]
  length.grobs <- length(grobs)
  # if (length.grobs<=1) {
  #   stop("YTError: should have at least 2 grobs")
  # }
  if (is.null(heights)) {
    heights <- rep(1,length.grobs)
  }

  g.top <- grobs[[1]] + gg.extras
  g.middle.list <- lapply(grobs[c(-1,-length.grobs)],function(g) {
    g + gg.extras
  })
  g.bottom <- grobs[[length.grobs]] + gg.extras

  if (adjust.themes) {
    top.theme <- theme(plot.margin=unit(c(margin, margin, gap, margin),units),
                       axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
    middle.theme <- theme(plot.margin=unit(c(gap, margin, gap, margin),units),
                          axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
    bottom.theme <- theme(plot.margin=unit(c(gap, margin, margin, margin),units))
    g.top <- g.top + top.theme
    g.middle.list <- lapply(g.middle.list,function(g) {
      g + middle.theme
    })
    g.bottom <- g.bottom + bottom.theme
  }
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
        g <- gtable::gtable_add_cols(g,unit(1,"null"))
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
  gtable.final <- do.call(gridExtra::gtable_rbind,args)

  if (as.gtable) {
    return(gtable.final)
  } else {
    if (newpage) {
      grid::grid.newpage()
    }
    grid::grid.draw(gtable.final)
  }
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




#' Make Table
#'
#' Creates a summary table (data frame) variables from the data.
#'
#' This was written to create a "Table 1" of a manuscript.
#'
#' @param data Data frame containing data to be described.
#' @param ... column names (bare) within \code{data} to be summarized.
#' @param denom whether to show the denominator in the summary
#' @param maxgroups max number of groups before collapsing into an "Other" category.
#' @param by optional variable name (bare) by which to summarize the data. Each separate value will be a column of data in the table.
#' @param fisher whether or not to perform Fisher test. Performed if by=... is specified.
#'
#' @return Returns a data frame formatted to be summary table.
#' @examples
#' make_table(mtcars,cyl,gear)
#' @author Ying Taur
#' @export
make_table <- function(data,...,by=NULL,denom=FALSE,maxgroups=10,fisher=TRUE) {
  requireNamespace(c("rlang","purrr"),quietly=TRUE)
  vars <- quos(...)
  by <- enquo(by)
  totalvar <- quo(total_)
  allvars <- append(vars,totalvar)

  vars.by <- vars %>% append(by) %>% map(quo_name)
  is.td <- vars.by %>% paste0("_day") %>% has_name(data,.)
  if (any(is.td)) {
    warning("YTWarning: possible time-dependent variables detected: ",paste(vars.by[is.td],collapse=","),". Consider carefully before incorporating these.")
  }
  d <- data %>% ungroup() %>%
    mutate(!!totalvar:="") %>%
    select(!!!allvars,!!by) %>%
    mutate(across(where(is.character),fct_infreq),
           across(where(~!is.character(.) & !is.factor(.)),as.character),
           across(where(~n_distinct(.)>5),~fct_lump_n(.,n=5,ties.method="first")))
  tbl <- lapply(allvars,function(var) {
    d %>% count(value=!!var) %>%
      complete(value,fill=list(n=0)) %>%
      mutate(sum=sum(n),
             pct=n/sum,
             percent=percent(pct,accuracy=0.1),
             var=quo_name(var),
             text=purrr::when(denom ~ str_glue("{n} ({percent})"),
                              ~ str_glue("{n}/{sum} ({percent})"))) %>%
      select(var,value,all=text)
  }) %>% bind_rows()

  if (!rlang::quo_is_null(by)) {
    by.table <- lapply(allvars,function(var) {

      d %>% count(value=!!var,col=paste0(quo_name(by),"=",!!by)) %>%
        complete(value,col,fill=list(n=0)) %>%
        group_by(col) %>%
        mutate(sum=sum(n),
               pct=n/sum,
               percent=scales::percent(pct,accuracy=0.1),
               var=quo_name(var),
               text=purrr::when(denom ~ str_glue("{n} ({percent})"),
                                ~ str_glue("{n}/{sum} ({percent})"))) %>%
        ungroup() %>%
        pivot_wider(id_cols=c(var,value),names_from=col,values_from=text)
    }) %>% bind_rows()
    tbl <- full_join(by.table,tbl,by=c("var","value"))
  }
  if (fisher & !rlang::quo_is_null(by)) {
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
      tibble(var=quo_name(var),fisher.pvalue=pval)
    }) %>% bind_rows()
    tbl <- tbl %>% left_join(f.tbl,by="var") %>%
      mutate(fisher.pvalue=if_else(duplicated(var),NA_character_,fisher.pvalue))
  }
  return(tbl)
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
  message("Note, make.table is deprecated, consider using make_table")
  all.vars <- unique(c(vars,by))
  if (any(all.vars %!in% names(data))) {stop("YTError, variable not found in data frame: ",paste(setdiff(c(vars,by),names(data)),collapse=", "))}
  data <- data[,all.vars,drop=FALSE]
  if (!is.null(by)) {
    if (by %in% vars) {warning("YTWarning, ",paste(intersect(by,vars),collapse=",")," is listed in both 'vars' and 'by'!")}
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
  data <- data %>% mutate_all(factorize)
  get.column <- function(subdata) {
    #subdata=data
    denom <- nrow(subdata)
    subtbl <- plyr::adply(vars,1,function(var) {
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
      if (n_distinct(data[[var]])==1) {
        warning("YTWarning: ",var," does not vary. Skipping Fisher test.")
        return(NA_real_)
      }
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
  gsub("(^ +)|( +$)", "",string)
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
#' Note: The particular use is in ytdata::visits() function.
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
#' This creates a fractional date. If you do as.Date(datetime), it removes the time part.
#' @param datetime a POSIXct vector to be convered
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
#' @return a vector of values represent the midpoint between \code{tstart} and \code{tstop}.
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
#' @author Ying Taur
#' @export
is.mrn <- function(mrn,like=FALSE) {
  if (like) { #if numeric or factor, and if leading zeroes gone, can still hit.
    mrn <- as.character(trim(mrn))
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



#' Coalesce indicator variables into one summary variable.
#'
#' After providing multiple indicator variables, summarize them by creating a character vector.
#' @param ... indicator variables to coalesce together. Should be all logical.
#' @param else.value The character value if there are no hits. Default is \code{NA}
#' @param first.hit.only If \code{TRUE}, will only show first hit (which is a true coalesce). Default is \code{FALSE}, which concatenates all hits.
#' @return A vector of same length as the indicators, displaying variable names that were \code{TRUE}
#' @examples
#' #####
#' @author Ying Taur
#' @export
coalesce_indicators <- function(...,else.value=NA_character_,first.hit.only=FALSE) {
  vars <- quos(...)
  # varnames <- sapply(vars,quo_name)
  varnames <- map_chr(vars, quo_name)
  labels <- names(vars)
  labels <- unname(if_else(labels=="",varnames,labels))

  arglist <- list(...)
  len <- unique(sapply(arglist,length))
  if (length(len)>1) {
    stop("YTError: arguments are different lengths!")
  }
  #empty data, return empty character
  if (length(len)==1 & len==0) {
    return(character())
  }
  mat <- do.call(cbind,arglist)
  output <- sapply(1:len,function(i) {
    row <- mat[i,]
    keep <- !is.na(row) & row
    vec <- labels[keep]
    if (length(vec)==0) {
      return(else.value)
    } else {
      if (!first.hit.only) { # collapse all
        return(paste(vec,collapse="|"))
      } else { # coalesce
        return(vec[1])
      }
    }
  })
  return(output)
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
#'
#' @examples
coalesce_values <- function(...,sep="=",collapse="|",omit.na=FALSE) {
  vars <- enquos(...)
  varnames <- map_chr(vars, quo_name)
  labels <- names(vars)
  labels <- unname(if_else(labels=="",varnames,labels))
  arglist <- list(...) %>% map(as.character)
  arglist2 <- map2(labels,arglist,function(v,x) {
    if (omit.na) {
      ifelse(!is.na(x),paste0(v,sep,x),NA)
    } else {
      paste0(v,sep,x)
    }
  })
  final <- arglist2 %>% transpose() %>% simplify_all() %>%
    map_chr(~paste2(.,collapse=collapse))
  final
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
recode2 <- function(var,recodes,else.value=NULL,as.factor=NULL,regexp=FALSE,replace=FALSE,multi.hits=FALSE,ignore.case=TRUE,perl=FALSE,useBytes=TRUE) {
  if (is.null(names(recodes))) {
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
#' Perform multiple text replacements at once using regular expressions. Similar in form to \code{recode2} and \code{recode.grep}.
#'
#' @param var the character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, the names are the regular expression, and the value is the replacement text.
#' @param result.as.list if \code{TRUE}, returns a 2-vector list containing replaced text and text hits. Default is \code{FALSE}.
#' @param replace.text text to replace hits with. Default is \code{""}
#' @param collapse.hits the separator with which all hits are pasted together. If \code{NULL}, hits will remain as an uncollapsed list. Default is \code{"|"}. Note that this parameter is not relevant unless \code{result.as.list=TRUE}
#' @param recode.hits whether to recode the hits into the with the replacement. Default if \code{FALSE}. This is relevant if \code{result.as.list=TRUE}.
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is \code{TRUE}
#' @param perl whether to use perl-style regular expressions. Default is \code{TRUE}
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for \code{grep})
#'
#' @return By default, returns \code{var}, but with all regular expression hits replaced. If \code{result.as.list=TRUE} is specified, the hits themselves are also returned, within a 2-vector list.
#' @export
replace.grep <- function(var,recodes,result.as.list=FALSE,replace.text="",collapse.hits="|",recode.hits=FALSE,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) {
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
#' @param recode.hits whether to recode the hits into the with the replacement. Default if \code{FALSE}. This is relevant if \code{result.as.list=TRUE}.
#' @param useBytes logical. If TRUE the regex matching is done byte-by-byte rather than character-by-character. Avoids weird locale warnings. (see help for \code{grep})
#'
#' @return By default, returns \code{var}, but with all regular expression hits replaced. If \code{result.as.list=TRUE} is specified, the hits themselves are also returned, within a 2-vector list.
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
#' This is roughly equivalent to repeatedly running '\code{stringr::str_replace_all()} and/or \code{stringr::str_extract_all()} on the same
#' column of text.
#'
#' This function attempts to perform multiple text manipulations (replacements and/or extractions) in an easy and efficient way.
#' It can be faster than manually running '\code{stringr::str_replace_all()} and/or \code{stringr::str_extract_all()} for a few
#' reasons: (1) it performs one search for both replacement and extraction, (2) it performs an initial search and ignores any rows
#' that didn't match, which saves time especially if most rows are not hits.
#'
#' @param data the data frame to be manipulated.
#' @param var the bare character vector to be searched.
#' @param recodes a vector of regular expressions. Can be named or unnamed; if named, the names are the regular expression, and the value is the replacement text.
#' @param newvar bare name of column to hold the replaced version of \code{var}. If \code{NULL} (default), \code{var} will be overwritten.
#' @param hits bare name of column to hold the text hits. If \code{NULL} (default), hits are not stored. This will store a list of extracted text, similar to the output of \code{str_extract_all()}
#' @param ignore.case whether or not to ignore case, passed to regular expression. Default is \code{TRUE}
#' @param collapse.fn optional function to apply to each element of \code{hits}, to create an atomic vector Non-hits are ignored.
#' @return returns the data with the above replacement text and stored hits.
#' @examples
#' library(stringr)
#' recodes <- c("<s-word>"="\\bs[a-z]+","<r-word>"="\\br[a-z]+")
#' data <- tibble(text=stringr::sentences)
#' data %>% replace_grep_data(recodes,text,new.sentence,hits)
#' data %>% replace_grep_data(recodes,text,new.text,hits,collapse.fn=~paste(names(.),"=",.,collapse="; "))
#' @export
replace_grep_data <- function(data,recodes,var,newvar=NULL,hits=NULL,ignore.case=TRUE,collapse.fn=NULL) {
  requireNamespace(c("rlang","stringi","purrr"),quietly=TRUE)
  var <- enquo(var)
  newvar <- enquo(newvar)
  hits <- enquo(hits)
  get.hits <- !rlang::quo_is_null(hits)
  get.replace <- !rlang::quo_is_null(newvar)
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
    data[[quo_name(hits)]] <- hitlist
  }
  if (get.replace) {
    data[[quo_name(newvar)]] <- var
  }
  return(data)

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
  id.vars <- quos(...)
  id.varnames <- sapply(id.vars,quo_name)
  other.varnames <- setdiff(names(data),id.varnames)
  other.vars <- syms(other.varnames)
  data2 <- data %>% group_by(...) %>% summarize_all(function(x) length(unique(x))) %>% ungroup()
  data3 <- data2 %>% select(!!!other.vars) %>% summarize_all(function(x) all(x==1))

  distinct.vars <- names(data3)[t(data3)]
  non.distinct.vars <- names(data3)[!t(data3)]
  distinct.vars.text <- paste0("[",paste(id.varnames,collapse=","),"],",paste(distinct.vars,collapse=","))
  non.distinct.vars.text <- paste0(non.distinct.vars,collapse=",")
  message("distinct: ",distinct.vars.text,"\nnot distinct: ",non.distinct.vars.text,"\n")

}

#' Is Distinct
#'
#' Determine if specified columns within data are distinct for individual rows.
#' @param data Dataframe to be analyzed
#' @param ... grouping variables that define data units.
#' @param add.group.vars if \code{TRUE}, add any grouping variables.
#' @return Logical indicating whether or not columns are distinct.
#' @export
is.distinct <- function(data, ..., add.group.vars=TRUE) {
  vars <- quos(...)
  row.tally <- data %>%
    group_by(!!!vars,.add=add.group.vars) %>%
    summarize(n=n()) %>%
    ungroup()
  is.dist <- max(row.tally$n)==1
  return(is.dist)
}



#' Read Multiple Excel Sheets Into a List of Data Frames
#'
#' @param ... Either a file(s) or folder(s). If a folder is specified, it will look for all files ending in (.xlsx/.xls).
#' @param col_names \code{TRUE} to use the first row as column names, \code{FALSE} to get default names, or a character
#' vector giving a name for each column. This is passed to \code{readxl::read_excel} function.
#' @param keep.nested If \code{TRUE}, returns a nested list of files and then sheets. Otherwise, a list of sheets is normally returned.
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
  quolist <- quos(...)
  sheetnames <- ifelse(names(quolist)!="",names(quolist),unname(sapply(quolist,quo_name)))
  objlist <- list(...)
  names(objlist) <- sheetnames
  is.list.of.dataframes <- function(x) {
    is.list(x) & all(sapply(x,is.data.frame))
  }
  sheetlist <- sapply(objlist,function(obj) {
    if (is.list.of.dataframes(obj)) {
      obj
    } else if (is.data.frame(obj)) {
      list(obj)
    } else {
      stop("YTError: arguments should be either a data frame or list of dataframes.")
    }
  }) %>% unlist(recursive=FALSE)
  message("Writing ",length(sheetlist)," sheets.")

  wb <- xlsx::createWorkbook()
  for (i in 1:length(sheetlist)) {
    sheet  <- xlsx::createSheet(wb,sheetName=names(sheetlist)[i])
    xlsx::addDataFrame(sheetlist[[i]],sheet,row.names=FALSE)
  }
  saveWorkbook(wb,file)
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




#' Determines if tstart-tstop occurs anywhere within interval.
#' @export
occurs.within <- function(tstart,tstop,start.interval,stop.interval) {
  message("YTNote: occurs.within() was renamed to overlaps()")
  tstop>=start.interval & stop.interval>=tstart
}


#' Determines if 2 sets of time intervals overlap.
#'
#' @param start1 start times for interval 1
#' @param stop1 stop times for interval 1
#' @param start2 start times for interval 2
#' @param stop2 stop times for interval 2
#'
#' @export
overlaps <- function(start1,stop1,start2,stop2) {
  if (any(start1>stop1,na.rm=TRUE)) {stop("YTError: start1 is greater than stop1")}
  if (any(start2>stop2,na.rm=TRUE)) {stop("YTError: start2 is greater than stop2")}
  stop1>=start2 & stop2>=start1
}





#' Any overlap
#'
#' For a given set of intervals, determine whether any interval is overlapping.
#' @param start vector specifying the start of the intervals
#' @param stop vector specifying the end of the intervals
#' @param na.rm whether to remove NA values (default is TRUE)
#' @return whether or not at least one interval is overlapping.
#' @export
any.overlap <- function(start,stop,na.rm=TRUE) {
  t <- tibble(start=start,stop=stop) %>%
    arrange(start) %>%
    mutate(diff = start - lag(stop))
  return(any(t$diff[-1]<0,na.rm=na.rm))
}



#' Determines if x is between start and stop/
#'
#' Similar to \code{dplyr::between}, except that the vectors are recycled, so x can be a fixed value.
#' @export
is.between <- function(x,start,stop) {
  if (any(start>stop,na.rm=TRUE)) {stop("YTError: start is greater than stop")}
  overlaps(x,x,start,stop)
}



#' Chop survival endpoint
#'
#' For a given survival endpoint, censor at earlier timepoints, if they occur.
#' @param data the data frame with survival data
#' @param newvar the name (unquoted) of the new survival endpoint to be created (creates \code{newvar}, plus \code{paste0(newvar,"_day")}
#' @param oldvar the original survival endpoint, to be censored.
#' @param ... columns representing censoring times.
#' @param censor.as.tdvar whether to censor endpoints occurring exactly at the censoring time. Use \code{TRUE} for time-dependent predictors, \code{FALSE} for endpoints.
#' @return Returns \code{data}, with a newly defined survival endpoint (\code{newvar}), which has been censored wherever the censoring times occur before the original end of survival time.
#' @examples
#' # create a endpoint(dead30d), which represents death within 30 days or discharge.
#' new.pt.cid94 <- cid.patients %>% chop.endpoint(dead30d,dead,30,discharge.day)
#' @author Ying Taur
#' @export
chop.endpoint <- function(data,newvar,oldvar,...,censor.as.tdvar=FALSE) {
  newvar <- enquo(newvar)
  oldvar <- enquo(oldvar)
  oldvar_day <- paste0(quo_name(oldvar),"_day")
  oldvar_day <- sym(oldvar_day)
  newvar <- quo_name(newvar)
  newvar_day <- paste0(quo_name(newvar),"_day")
  vars <- quos(...)
  ov <- pull(data,!!oldvar)
  if (!is.logical(ov) & !all(ov %in% 0:1,na.rm=TRUE)) {stop("YTError: oldvar should be a logical or 0-1!")}
  if (!has_name(data,quo_name(oldvar_day))) {stop("YTError: ",quo_name(oldvar_day)," does not exist!")}
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
#' These can be regular survival or competing endpoints (which you would analyze with something like Fine-Grey in \code{cmprsk} package).
#'
#' Note, endpoints (primary and competing) can be specified either as a "varname" and "varname_day" pair, representing survival indicator and survival time,
#' or a single column representing positive endpoints (\code{NA} or \code{Inf}) otherwise.
#'
#' If survival endpoints are specified, note that censored times may be ignored.
#'
#' @param data the data to be modified, containing the endpoints to be combined
#' @param newvar the name (unquoted) of the new competing survival endpoint to be created (creates \code{newvar}, plus \code{paste0(newvar,"_day")})
#' @param primary the original survival endpoint, to be converted to a competing endpoint
#' @param ... columns representing competing endpoints.
#' @param censor variable representing censoring times. Default is to use censoring times from the primary... or time=\code{Inf}, if it doesn't exist.
#' @param competing whether to code as competing. If FALSE, competing endpoints will be censored.
#'
#' @return Returns \code{data}, with a newly defined survival endpoint (\code{newvar}), which represents the combined competing endpoint.
#' \code{newvar} is the numeric indicator of the endpoint,
#' \code{newvar_day} is the survival time,
#' \code{newvar_code} is a character showing the value definition,
#' \code{newvar_info} shows the status of all endpoints, in order.
#' You would primarily use \code{newvar} and \code{newvar_day} with packages such as \code{cmprsk} for competing risk analysis.
#' @examples
#' # create a combined endpoint
#' cid.patients %>% make.endpt(competing.enterodom,enterodom30,dead,strepdom30,proteodom30,30)
#' @author Ying Taur
#' @export
make.surv.endpt <- function(data, newvar, primary, ... , censor=NULL,competing=FALSE) {
  newvar <- enquo(newvar)
  newvar_day <- paste0(quo_name(newvar),"_day")
  newvar_code <- paste0(quo_name(newvar),"_code")
  newvar_info <- paste0(quo_name(newvar),"_info")
  primary <- enquo(primary)
  censor <- enquo(censor)
  competing.vars <- quos(...)

  vartype <- function(data,var) {
    var <- enquo(var)
    varday <- paste0(quo_name(var),"_day")
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
    varname <- quo_name(var)
    varday <- paste0(quo_name(var),"_day")
    if (rlang::quo_is_null(var)) { #censor
      if (vartype(data,!!primary) %in% c("survival","competing")) {
        primary_day <- paste0(quo_name(primary),"_day")
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
  varnames <- sapply(varlist,quo_name)
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
              .final_info=paste(.info[.v==1],collapse=", ")) %>%
    ungroup() %>%
    arrange(.row)

  newdata <- data %>%
    mutate(!!newvar:=final$.final_v,
           !!newvar_day:=final$.final_vd,
           !!newvar_code:=final$.final_code,
           !!newvar_info:=final$.final_info)
  message(vartype(newdata,!!newvar)," endpoint variable created: ",quo_name(newvar))
  na.count <- newdata %>% filter(is.na(!!newvar)|is.na(!!newvar_day)) %>% nrow()
  if (na.count>0) {
    message("note: ",na.count," NA values")
  }
  return(newdata)
}



#' Cox Proportional Hazard model
#'
#' Run a Cox model
#'
#' If any of the \code{...}
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the time-to-event outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted). If a predictor time-dependent, the split the corresonding rows of the data frame.
#' @param starttime optional parameter specifying analysis start time.
#' @param return.split.data if \code{TRUE}, returns the data frame after splitting rows that are time-dependent.
#' @param return.model.obj if \code{TRUE}, returns the model object of the \code{coxph} command
#' @param formatted returns a formatted regression table (default \code{TRUE}). Otherwise, return the raw, unformatted regression table (essentially, the output of \code{broom::tidy}, plus a few additional columns)
#'
#' @return by default, returns a formatted regression table
#' @examples
#' library(yingtools2)
#' cid.patients %>% cox(vre.bsi,enterodom30,starttime=firstsampday)
#' @export
cox <- function(data, yvar, ... , starttime=NULL, return.split.data=FALSE,return.model.obj=FALSE,firth=FALSE,
                firth.opts=list(),formatted=TRUE) {
  requireNamespace(c("broom","coxphf","scales"),quietly=TRUE)
  yvar <- enquo(yvar)
  starttime <- enquo(starttime)
  xvars <- quos(...)
  yvarday <- quo_name(yvar) %>% paste0("_day") %>% sym()
  is.td <- function(var) {
    var <- enquo(var)
    vardayname <- quo_name(var) %>% paste0("_day")
    has_name(data,vardayname)
  }
  xvars.td <- xvars[sapply(xvars,is.td)]
  if (length(xvars.td)>0) {
    xvarsdays.td <- xvars.td %>% sapply(quo_name) %>% paste0("_day") %>% syms()
  } else {
    xvarsdays.td <- syms(NULL)
  }
  timevars <- c(yvarday,xvarsdays.td)
  data <- data %>% mutate_at(vars(!!yvar,!!!xvars.td),as.numeric)

  if (rlang::quo_is_null(starttime)) {
    # data <- data %>% mutate(.y=!!yvar,.tstart=-10000,.tstop=!!yvarday)
    # .tstart is pmin of all time vars, because coxphf can't handle -Inf as tstart.
    mintime <- data %>% select(!!yvarday,!!!timevars) %>% min(na.rm=TRUE)
    start <- min(mintime-1,0)
    message("Setting start time as: ",start)
    data <- data %>%
      mutate(.y=!!yvar,.tstart=start,.tstop=!!yvarday) %>%
      mutate_at(vars(.tstart,.tstop,!!!timevars),function(x) x-start)
  } else {
    data <- data %>% mutate(.y=!!yvar,.tstart=!!starttime,.tstop=!!yvarday)
  }
  splitline <- function(data,xvar) {
    xvar <- enquo(xvar)
    xvarday <- quo_name(xvar) %>% paste0("_day") %>% sym()
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
  is.competing <- !all(pull(data,!!yvar) %in% c(0,1,NA))
  has.timevarying <- length(xvars.td)>0 & nrow(data2)>nrow(data)
  leftside <- "Surv(.tstart,.tstop,.y)"
  xvarnames <- xvars %>% sapply(quo_name)
  rightside <- xvarnames %>% paste(collapse=" + ")
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
  model <- paste(leftside,rightside,sep=" ~ ")
  formula <- as.formula(model)
  if (is.competing) {
    stop("YTError: can't handle competing endpoints in Cox.")
  }
  if (return.split.data) {
    fn <- ifelse(firth,"coxphf","coxph")
    form <- deparse(formula)
    message(str_glue("Returning split data. Can run as follows:\n{fn}({deparse(form)},data={{data}})"))
    return(data2)
  }
  if (!firth) {
    result <- survival::coxph(formula,data=data2)
  } else {
    result <- do.call(coxphf::coxphf,c(list(formula,data=data2),firth.opts))
  }
  if (return.model.obj) {
    return(result)
  }
  tbl <- yt.tidy(result) %>%
    mutate(xvar=terms.to.varnames(term,xvarnames,data2),
           yvar=quo_name(yvar),
           time.dependent=xvar %in% sapply(xvars.td,quo_name))

  tbl.extra <- lapply(xvars,function(x) {
    #time dependent
    if (quo_name(x) %in% sapply(xvars.td,quo_name)) {
      xday <- x %>% quo_name() %>% paste0("_day") %>% sym()
      count <- data %>% summarize(count=sum((!!x==1) & (!!xday < !!yvarday))) %>% pull(count)
      extra <- tibble(xvar=quo_name(x),n=count) %>% mutate(term=xvar)
      return(extra)
    }
    vec <- data %>% pull(!!x)
    is.01 <- function(v) {is.numeric(v) & all(v %in% c(0,1),na.rm=TRUE)}
    if (is.01(vec)) {
      extra <- tibble(xvar=quo_name(x),n=sum(vec,na.rm=TRUE)) %>% mutate(term=xvar)
      return(extra)
    } else if (is.numeric(vec)) {
      extra <- tibble(xvar=quo_name(x),n=NA_real_) %>% mutate(term=xvar)
      return(extra)
    } else {
      tbl <- table(vec)
      extra <- tibble(xvar=quo_name(x),n=as.vector(tbl)) %>% mutate(term=paste0(xvar,names(tbl)))
      return(extra)
    }
  }) %>% bind_rows()
  tbl <- tbl %>% left_join(tbl.extra,by=c("xvar","term"))

  if (formatted) {
    tbl <- tbl %>%
      mutate(xvar=ifelse(time.dependent,paste0(xvar,"(td)"),xvar),
             p.value=pvalue(p.value)) %>%
      mutate_at(vars(estimate,conf.low,conf.high),~formatC(.,format="f",digits=2)) %>%
      transmute(yvar,xvar,term,n,haz.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value)
  }
  return(tbl)
}




yt.tidy <- function(x,...) UseMethod("yt.tidy")
yt.tidy.coxph <- function(obj) {
  obj %>% broom::tidy(exponentiate=TRUE,conf.int=TRUE,conf.level=0.95)
}
yt.tidy.coxphf <- function(obj) {
  tibble(term=names(obj$coefficients),
         estimate=exp(obj$coefficients),
         statistic=NA,
         std.error=NA,
         p.value=obj$prob,
         conf.low=obj$ci.lower,
         conf.high=obj$ci.upper)
}

yt.tidy.glm <- function(obj) {
  obj %>% broom::tidy(exponentiate=TRUE,conf.int=TRUE,conf.level=0.95) %>%
    filter(term!="(Intercept)")
}

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
#' Uses the \code{cox} function to perform a univariate and multivariate model analysis.
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the time-to-event outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted). If a predictor time-dependent, the split the corresonding rows of the data frame.
#' @param starttime optional parameter specifying analysis start time.
#' @param multi if \code{TRUE}, perform multivariate analysis.
#' @param multi.cutoff P-value threshold for inclusion into the multivariate model (default is \code{0.25})
#' @param formatted returns a formatted regression table (default \code{TRUE}). Otherwise, return the raw, unformatted regression table (essentially, the output of \code{broom::tidy}, plus a few additional columns)
#' @return by default, returns a formatted regression table
#' @examples
#' @export
univariate.cox <- function(data, yvar, ..., starttime=NULL,multi=TRUE,multi.cutoff=0.25,firth=FALSE,formatted=TRUE) {
  yvar <- enquo(yvar)
  starttime <- enquo(starttime)
  xvars <- quos(...)
  univariate.reglist <- lapply(xvars,function(x) {
    message(quo_name(x))
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



#' Cox Proportional Hazards Regression (TAKE 2)
#'
#' STILL WRITING THIS
#' @export
cox.old <- function(data, yvar, ... , starttime=NULL, return.split.data=FALSE,args5=list(cens.model="cox",model="fg")) {

  requireNamespace(c("coxphf","cmprsk","timereg","riskRegression"),quietly=TRUE)
  yvar <- enquo(yvar)
  starttime <- enquo(starttime)
  xvars <- quos(...)
  # yvar=sym("vre.bsi");xvars=syms("agebmt");starttime=sym(NULL)

  yvarday <- quo_name(yvar) %>% paste0("_day") %>% sym()
  is.td <- function(var) {
    var <- enquo(var)
    vardayname <- quo_name(var) %>% paste0("_day")
    has_name(data,vardayname)
  }
  xvars.td <- xvars[sapply(xvars,is.td)]
  if (length(xvars.td)>0) {
    xvarsdays.td <- xvars.td %>% sapply(quo_name) %>% paste0("_day") %>% syms()
  } else {
    xvarsdays.td <- syms(NULL)
  }
  timevars <- c(yvarday,xvarsdays.td)
  data <- data %>% mutate_at(vars(!!yvar,!!!xvars.td),as.numeric)

  if (rlang::quo_is_null(starttime)) {
    # data <- data %>% mutate(.y=!!yvar,.tstart=-10000,.tstop=!!yvarday)
    # .tstart is pmin of all time vars, because coxphf can't handle -Inf as tstart.
    mintime <- data %>% select(!!yvarday,!!!timevars) %>% min(na.rm=TRUE)
    start <- min(mintime-1,0)
    message("Setting start time as: ",start)
    data <- data %>% mutate(.y=!!yvar,.tstart=start,.tstop=!!yvarday) %>%
      mutate_at(vars(.tstart,.tstop,!!!timevars),function(x) x-start)
  } else {
    data <- data %>% mutate(.y=!!yvar,.tstart=!!starttime,.tstop=!!yvarday)
  }
  splitline <- function(data,xvar) {
    xvar <- enquo(xvar)
    xvarday <- quo_name(xvar) %>% paste0("_day") %>% sym()
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
  if (return.split.data) {
    return(data2)
  }
  is.competing <- !all(pull(data,!!yvar) %in% c(0,1,NA))
  has.timevarying <- length(xvars.td)>0 & nrow(data2)>nrow(data)
  leftside <- "Surv(.tstart,.tstop,.y)"
  rightside <- xvars %>% sapply(quo_name) %>% paste(collapse=" + ")
  model <- paste(leftside,rightside,sep=" ~ ")
  formula <- as.formula(model)

  #result 1, regular cox
  src <- tibble(xvar="<error>",method="coxph")
  if (is.competing) {
    src <- tibble(xvar="<competing>",method="coxph")
  } else {
    tryCatch({
      results <- coxph(formula,data=data2)
      sr <- summary(results)
      src <- sr$conf.int %>% as.data.frame() %>% rownames_to_column("var") %>%
        as_tibble() %>%
        select(xvar=var,haz.ratio=`exp(coef)`,lower.ci=`lower .95`,upper.ci=`upper .95`) %>%
        mutate(p.value=sr$coefficients[,"Pr(>|z|)"],method="coxph")
    },error=function(e) {
    })
  }

  #result 2
  src2 <- tibble(xvar="<error>",method="coxphf.F")
  if (is.competing) {
    src2 <- tibble(xvar="<competing>",method="coxphf.F")
  } else {
    tryCatch({
      results2 <- coxphf(formula,data=data2,firth=F)
      sr2 <- summary(results2)
      src2 <- tibble(xvar=names(sr2$coefficients),
                     haz.ratio=exp(sr2$coefficients),
                     lower.ci=sr2$ci.lower,
                     upper.ci=sr2$ci.upper,
                     p.value=sr2$prob,
                     method="coxphf.F")

    },error=function(e) {
    })

  }
  #result 3
  src3 <- tibble(xvar="<error>",method="xxx")
  if (is.competing) {
    src3 <- tibble(xvar="<competing>",method="coxphf.T")
  } else {
    tryCatch({
      results3 <- coxphf(formula,data=data2,firth=T)
      sr3 <- summary(results3)
      src3 <- tibble(xvar=names(sr3$coefficients),
                     haz.ratio=exp(sr3$coefficients),
                     lower.ci=sr3$ci.lower,
                     upper.ci=sr3$ci.upper,
                     p.value=sr3$prob,
                     method="coxphf.T")
    },error=function(e) {
    })
  }

  #result 4
  src4 <- tibble(xvar="<error>",method="xxx")
  if (has.timevarying) {
    src4 <- tibble(xvar="<timevarying>",method="crr")
  } else {
    tryCatch({
      cov <- paste0("~",rightside) %>% as.formula() %>% model.matrix(data=data2)
      cov <- cov[,-1,drop=FALSE]
      results4 <- data2 %>% with(crr(.tstop,.y,cov1=cov))
      sr4 <- summary(results4)
      src4 <- sr4$conf.int %>% as.data.frame() %>% rownames_to_column("var") %>%
        as_tibble() %>%
        select(xvar=var,haz.ratio=`exp(coef)`,lower.ci=`2.5%`,upper.ci=`97.5%`) %>%
        mutate(p.value=sr4$coef[,"p-value"],method="crr")
    },error=function(e) {
    })

  }

  #results 5, Fine gray riskRegression
  src5 <- tibble(xvar="<error>",method="xxx")
  if (has.timevarying) {
    src5 <- tibble(xvar="<timevarying>",method="riskRegression")
  } else {
    tryCatch({
      leftside.b <- "Hist(.tstop, .y)"
      rightside <- xvars %>% sapply(quo_name) %>% paste(collapse=" + ")
      model <- paste(leftside.b,rightside,sep=" ~ ")
      formula <- as.formula(model)
      print(formula)
      r5 <- FGR(formula,data=data2,cause=1)
      sr5 <- summary(r5)
      src5 <- cbind(sr5$coef,sr5$conf.int) %>% as.data.frame() %>% rownames_to_column("var") %>%
        select(xvar=var,haz.ratio=`exp(coef)`,lower.ci=`2.5%`,upper.ci=`97.5%`,p.value=`p-value`) %>%
        mutate(method="riskRegression")
    },error=function(e) {
    })


  }

  # #results6 timereg
  # leftside.b <- "Event(.tstart, .tstop, .y)"
  # rightside.b <- xvars %>% sapply(quo_name) %>% paste0("const(",.,")",collapse=" + ")
  # model.b <- paste0(leftside.b," ~ ",rightside.b)
  # args <- c(list(formula=as.formula(model.b),data=data2,cause=1),args5)
  # results6 <- do.call(comp.risk,args)
  # sr6 <- coef(results6) %>% as.data.frame() %>% rownames_to_column("var")
  # src6 <- tibble(xvar=sr6$var,haz.ratio=exp(sr6$Coef.),lower.ci=exp(sr6$`lower2.5%`),upper.ci=exp(sr6$`upper97.5%`),p.value=sr6$`P-val`,method="timereg")

  d <- bind_rows(src,src2,src3,src4,src5)
  d
  # list(coxph=results,coxphf.F=results2,coxphf.T=results3,
  #      crr=results4,data=d)
}



#' Cox Proportional Hazards Regression
#'
#' Analyzes survival data by Cox regression.
#'
#' Convenience function for survival analysis. Typically uses the \code{coxphf} function.
#'
#' @param  ... variable names in the regression
#' @param starttime character column name for start times (either point to zero or indicate left censor times). Default is "tstart".
#' @param data survival data.
#' @param addto if specified, add results to this data.frame of results. Default is NULL
#' @param as.survfit if TRUE, return the survival fit object (use for kaplan-meier stuff).
#' @param firth whether or not to perform Firth's penalized likelihood. Default is TRUE.
#' @param formatted whether to format the data.frame of results. Default is TRUE
#' @param logrank whether to calculate log rank p-value. Default is FALSE
#' @param coxphf.obj whether to return cox results object (rather than regression table). Default is FALSE.
#' @param return.split.data whether to return data after split (do this to split time-dependent variables and run Cox manually). Default is FALSE
#' @return a regression table with the survival results
#' @author Ying Taur
#' @export
stcox <- function( ... ,starttime="tstart",data,addto,as.survfit=FALSE,firth=TRUE,formatted=TRUE,logrank=FALSE,coxphf.obj=FALSE,return.split.data=FALSE) {
  requireNamespace("coxphf",quietly=TRUE)
  data <- data.frame(data)
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
    data <- plyr::adply(data,1,splitline)
  }
  if (return.split.data) {
    return(data)
  }

  #calculate model
  leftside <- "survival::Surv(s.start,s.stop,y)"
  rightside <- paste(xvars,collapse=" + ")
  model <- paste(leftside,rightside,sep=" ~ ")
  formula <- as.formula(model)

  for (x in xvars) { #check for nonvarying predictors
    xvalues <- unique(data[,x])
    if (length(xvalues)==1) {
      stop("YTError: This predictor does not vary across observations: ",x," is always equal to ",xvalues)
    }
  }
  if (as.survfit) {
    #return a survfit object
    return(survival::survfit(formula,data=data))
  } else if (logrank) {
    #output logrank test
    results <- summary(coxph(formula,data=data))
    return(results$logtest[3])
  } else {
    results <- coxphf::coxphf(formula,data=data,firth=firth)
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
      results.table <- plyr::adply(results.table,1,function(x) {
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




#' Univariate Cox Proportional Hazards
#'
#' Perform univariate survival, then multivariate on significant variables.
#' @param yvars column name of survival endpoint.
#' @param xvars column names of predictors.
#' @param tstart column name of start variable.
#' @param data the data frame to be analyzed
#' @param firth whether to perform Firth's penalized likelihood correction.
#' @param multi whether to perform multivariate modelling of signficant univariate predictors
#' @param multi.cutoff if multivariate is done, the P-value cutoff for inclusion into the multivariate model.
#' @return A regression table containing results
#' @export
univariate.stcox <- function(yvar,xvars,starttime="tstart",data,firth=TRUE,multi=FALSE,multi.cutoff=0.2) {
  # yvar="dead.180";xvars=c("age","race.group","detect.trop","imm.med.group2");starttime="tstart";data=pt;firth=F;multi=TRUE;multi.cutoff=0.2;referrent=FALSE
  results.list <- lapply(xvars,function(xvar) {
    print(xvar)
    tryCatch({
      stcox(yvar,xvar,starttime=starttime,data=data,firth=firth)
    },error=function(e) {
      warning(e$message)
      data.frame(model=paste0("Surv(s.start,s.stop,y) ~ ",xvar),yvar=yvar,xvar=paste0(xvar,"[ERROR non-varying]"),haz.ratio=NA,p.value=NA,signif=NA)
    })
  })

  results.table <- results.list %>% bind_rows()
  if (multi) {
    multi.signif <- sapply(results.list,function(tbl) {
      any(tbl$p.value<=multi.cutoff)
    })
    multivars <- xvars[multi.signif]
    message("Multivariate model:")
    if (length(multivars)>0) {
      message(paste0(multivars,collapse=","))
      multi.table <- stcox(yvar,multivars,starttime=starttime,data=data,firth=firth)
      names(multi.table) <- recode2(names(multi.table),c("haz.ratio"="multi.haz.ratio","p.value"="multi.p.value","signif"="multi.signif"))
      multi.table <- multi.table %>% select(-model)
      results.table <- results.table %>% select(-model)

      combined.table <- results.table %>% left_join(multi.table,by=c("yvar","xvar")) %>%
        mutate_at(vars(multi.haz.ratio,multi.p.value,multi.signif),function(x) ifelse(is.na(x),"",x))
      # return(list(uni=results.table,multi=multi.table))

      n.events <- sum(data[[yvar]])
      n.multivars <- length(multivars)
      print(paste0(round(n.events/n.multivars,3)," events per multivariable (",n.events,"/",n.multivars,", consider overfitting if less than 10)"))
      return(combined.table)
    } else {
      print("No variables in multivariate!")
      results.table <- subset(results.table,select=-model)
      results.table$multi.haz.ratio <- ""
      results.table$multi.p.value <- ""
      results.table$multi.signif <- ""
      return(results.table)
    }
  }
  return(results.table)
}




#' Create Survival Data Frame
#'
#' @param f.survfit the survival data
#' @param time0 time0 can be specified. It must be <= first event
#' @return Returns survival data frame
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
#' library(ggplot2)
#' ggplot() + geom_kaplanmeier("dead","intensity",data=cid.patients)
#' @author Ying Taur
#' @export
geom_kaplanmeier <- function(yvar,xvar=NULL,data,starttime="tstart",flip.y=FALSE,size=NULL,logrank=FALSE,logrank.pos=NULL,logrank.fontsize=5) {
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
    g <- list(g,ylim(0,1))
  } else {
    if (is.null(size)) {
      g <- geom_step(data=sf,aes_string(x="time",y="surv",color=xvar,group=xvar))
    } else {
      g <- geom_step(data=sf,aes_string(x="time",y="surv",color=xvar,group=xvar),size=size)
    }
    g <- list(g,ylim(0,1))
    if (logrank) {
      #function to find best x,y for text
      find_best_spot <- function(plot) {
        gb <- ggplot_build(plot)
        xlim <- gb$layout$panel_params[[1]]$x.range
        ylim <- gb$layout$panel_params[[1]]$y.range
        xrange <- xlim[2]-xlim[1]
        yrange <- ylim[2]-ylim[1]
        xs <- seq(xlim[1],xlim[2],length.out=50)
        ys <- seq(ylim[1],ylim[2],length.out=50)
        d.data <- lapply(gb$data,function(data) {
          d.pts <- tibble()
          if (c("x","y") %allin% names(data)) {
            newdata <- data %>% select(x=x,y=y)
            d.pts <- d.pts %>% bind_rows(newdata)
          }
          if (c("xend","yend") %allin% names(data)) {
            newdata <- data %>% select(x=x,y=y)
            d.pts <- d.pts %>% bind_rows(newdata)
          }
          if (c("xmin","xmax","ymin","ymax") %allin% names(data)) {
            newdata1 <- data %>% select(x=xmin,y=ymin)
            newdata2 <- data %>% select(x=xmax,y=ymin)
            newdata3 <- data %>% select(x=xmin,y=ymax)
            newdata4 <- data %>% select(x=xmax,y=ymax)
            d.pts <- d.pts %>% bind_rows(newdata1,newdata2,newdata3,newdata4)
          }
          return(d.pts)
        }) %>% bind_rows() %>%
          filter(between(x,xlim[1],xlim[2]),between(y,ylim[1],ylim[2]))
        d.box1 <- tibble(x=xs) %>% crossing(y=ylim)
        d.box2 <- tibble(y=ys) %>% crossing(x=xlim)
        d <- bind_rows(d.box1,d.box2,d.data)
        pts <- tibble(xx=xs) %>% crossing(yy=ys) %>%
          crossing(d) %>%
          mutate(dist=sqrt(abs((xx-x)/xrange)^2+abs((yy-y)/yrange)^2)) %>%
          group_by(xx,yy) %>%
          summarize(min.dist=min(dist)) %>%
          ungroup() %>%
          slice(which.max(min.dist))
        return(tibble(x=pts$xx,y=pts$yy))
      }
      if (is.null(logrank.pos)) {
        gg <- ggplot() + g
        bestpos <- find_best_spot(gg)
        logrank.pos <- c(bestpos$x,bestpos$y)
      }
      g <- list(g,geom_logrank(yvar=yvar,xvar=xvar,data=data,starttime=starttime,pos=logrank.pos,logrank.fontsize=logrank.fontsize))
    }
  }
  return(g)
}


#' Generate label for Log-rank test results in ggplot2
#'
#' Adds the p-value for a log-rank test to a ggplot2 graph.
#' @author Ying Taur
#' @export
geom_logrank <- function(yvar,xvar,data,starttime="tstart",pos,logrank.fontsize=5) {
  if (length(pos)!=2) {
    stop("YTError: Logrank position should be a vector of size 2: c(x,y)")
  }
  logrank <- stcox(yvar=yvar,yvar=xvar,data=data,starttime=starttime,logrank=TRUE)
  logrank <- paste0("Log-rank\nP = ",formatC(logrank,format="f",digits=3))
  annotate("text",x=pos[1],y=pos[2],label=logrank,size=logrank.fontsize)
}


#' Logistic Regression
#'
#' Performs univariate or multivariate logistic regression
#'
#' Logistic regression is for prediction of yes/no outcomes.
#'
#' @return A logistic regression table containing predictors, odds ratios, confidence limits, and p-values.
#' @examples
#' # logistic regression predicting vs with mpg, cyl, and disp:
#' # specify yvar and xvar in model:
#' logistic("vs",c("mpg","cyl","disp"),data=mtcars)
#' # specify model:
#' logistic(vs~mpg+cyl+disp,data=mtcars)
#' @author Ying Taur
#' @export
logistic <- function(x,...) UseMethod("logistic")


#' @rdname logistic
#' @param yvar Y-variable of interest (column name within data). Should be either logical or 0-1.
#' @param xvar X-variable(s) of interest (vector of column names within data). A vector of length=1 will perform a univariate analysis, length>1 will perform a multivariate analysis.
#' @param data data frame containing the data.
#' @param firth Whether to apply Firth's penalized likelihood correction. Default is \code{FALSE}
#' @param formatted logical specifying whether to format the data in a table. Default is \code{TRUE}.
#' @param digits number of significant digits in results. Default is 3.
#' @export
logistic.character <- function(yvar, xvars ,data,firth=FALSE,formatted=TRUE,digits=3) {
  # y <- c(...)[1]
  # x <- paste(c(...)[-1],collapse="+")
  # model <- paste(y,x,sep="~")
  model <- paste0(yvar,"~",paste(xvars,collapse="+"))
  logistic(as.formula(model),data=data,firth=firth,formatted=formatted,digits=digits)
}

#' @rdname logistic
#' @param formula formula on which to perform logistic regression.
#' @export
logistic.formula <- function(formula, data=sys.parent(), firth=FALSE,formatted=TRUE,digits=3) {
  requireNamespace("logistf",quietly=TRUE)
  results <- logistf::logistf(formula, data=data, firth=firth)
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
    results.table <- plyr::adply(results.table,1,function(x) {
      x$odds.ratio <- paste0(x$odds.ratio," (",x$lower.ci," - ",x$upper.ci,")")
      return(x)
    })
    results.table <- subset(results.table,select=c(model,yvar,xvar,odds.ratio,p.value,signif))
  }
  return(results.table)
}



#' Univariate Logistic Regression
#'
#' Perform logistic regression analysis on a group of predictors, and optionally perform multivariate analysis on significant univariate predictors.
#'
#' @param yvar ...param1.description...xxx
#' @param xvars ...param2.description...
#' @param data data frame containing the data.
#' @param firth Whether to apply Firth's penalized likelihood correction. Default is \code{FALSE}
#' @param multi whether to contruct a multivariate model using univariate predictors. Default is \code{FALSE}
#' @param multi.cutoff P-value cutoff at which a univariate predictor is included in the multivariate. Default is \code{0.2}.
#' @param digits number of significant digits in results. Default is 3.
#' @return A logistic regression table containing predictors, odds ratios, confidence limits, and p-values.
#' @examples
#' univariate.logistic("vs",c("mpg","cyl","disp","am","gear"),data=mtcars,multi=TRUE)
#' @author Ying Taur
#' @export
univariate.logistic <- function(yvar,xvars,data,firth=FALSE,multi=FALSE,multi.cutoff=0.2,digits=3) {
  # yvar="vs";xvars=c("mpg","cyl","disp","hp","drat","wt","qsec","am","gear","carb");data=mtcars;firth=F;multi=T;multi.cutoff=0.2;digits=3
  # results.table <- data.frame()
  # for (xvar in xvars) {
  #   print(xvar)
  #   results.table <- logistic(yvar,xvar,data=data,firth=firth,addto=results.table,digits=digits)
  # }
  results.table <- lapply(xvars,function(xvar) {
    print(xvar)
    logistic(yvar,xvar,data=data,firth=firth,digits=digits)
  }) %>% bind_rows()

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
    names(multi.table) <- c("model","yvar","xvar","multi.odds.ratio","multi.p.value","multi.signif")
    multi.table <- subset(multi.table,select=-model)
    results.table <- subset(results.table,select=-model)
    combined.table <- merge(results.table,multi.table,all.x=TRUE)
    results.table <- combined.table[order(factor(combined.table$xvar,levels=results.table$xvar)),]
    results.table <- data.frame(lapply(results.table,function(x) ifelse(is.na(x),"",as.character(x))))
  }
  return(results.table)
}





#' Logistic regression
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted).
#' @param starttime optional parameter specifying analysis start time.
#' @param return.model.obj if \code{TRUE}, returns the model object of the \code{coxph} command
#' @param formatted returns a formatted regression table (default \code{TRUE}). Otherwise, return the raw, unformatted regression table (essentially, the output of \code{broom::tidy}, plus a few additional columns)
#' @return by default, returns a formatted regression table
#' @export
logit <- function(data, yvar, ... , return.model.obj=FALSE,firth=FALSE,formatted=TRUE) {
  requireNamespace(c("broom","logistf","scales"),quietly=TRUE)
  yvar <- enquo(yvar)
  xvars <- quos(...)
  xvarnames <- sapply(xvars,quo_name)
  formula <- as.formula(paste0(quo_name(yvar),"~",paste(xvarnames,collapse="+")))
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
           yvar=quo_name(yvar)) %>%
      select(yvar,xvar,term,everything())

  #create 'n' column for categorical variables (factor, character, logical, 0-1)
  tbl.extra <- lapply(xvars,function(x) {
    vec <- data %>% pull(!!x)
    is.01 <- function(v) {is.numeric(v) & all(v %in% c(0,1),na.rm=TRUE)}
    if (is.01(vec)) {
      extra <- tibble(xvar=quo_name(x),n=sum(vec)) %>% mutate(term=xvar)
      return(extra)
    } else if (is.numeric(vec)) {
      extra <- tibble(xvar=quo_name(x),n=NA_real_,term=xvar)
      return(extra)
    } else {
      tbl <- table(vec)
      extra <- tibble(xvar=quo_name(x),n=as.vector(tbl)) %>% mutate(term=paste0(xvar,names(tbl)))
      return(extra)
    }
  }) %>% bind_rows()

  tbl <- tbl %>% left_join(tbl.extra,by=c("xvar","term"))
  if (formatted) {
    tbl <- tbl %>%
      mutate(p.value=pvalue(p.value)) %>%
      mutate_at(vars(estimate,conf.low,conf.high),~formatC(.,format="f",digits=2)) %>%
      transmute(yvar,xvar,term,n,odds.ratio=paste0(estimate," (",conf.low," - ",conf.high,")"),p.value)
  }
  tbl
}


#' Univariate and Multivariate Cox Regression
#'
#' Uses the \code{cox} function to perform a univariate and multivariate model analysis.
#' @param data the data frame containing the variables to be analyzed.
#' @param yvar the time-to-event outcome (bare unquoted).
#' @param ... predictors in the model (bare unquoted). If a predictor time-dependent, the split the corresonding rows of the data frame.
#' @param starttime optional parameter specifying analysis start time.
#' @param multi if \code{TRUE}, perform multivariate analysis.
#' @param multi.cutoff P-value threshold for inclusion into the multivariate model (default is \code{0.25})
#' @param formatted returns a formatted regression table (default \code{TRUE}). Otherwise, return the raw, unformatted regression table (essentially, the output of \code{broom::tidy}, plus a few additional columns)
#' @return by default, returns a formatted regression table
#' @examples
#' @export
univariate.logit <- function(data, yvar, ..., multi=TRUE,multi.cutoff=0.25,firth=FALSE,formatted=TRUE) {
  yvar <- enquo(yvar)
  xvars <- quos(...)
  univariate.reglist <- lapply(xvars,function(x) {
    message(quo_name(x))
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





#' Group By All Distinct Variables
#'
#' Can be used similar to \code{group_by}, but will try to add additional variables to the group list, such that the grouping remains the same.
#' In other words, \code{group_by_all_distinct(data,a,b,c)} will group by a,b,c,x,y,z, where x,y,z do not alter the groups.
#' This is useful for keeping extra variables that go with the grouping, if you perform \code{summarize} afterwards.
#'
#' This is a convenience function that I made because of sheer laziness....
#' probably better to avoid using this for really rigorous data operations.
#' @param data data frame
#' @param ... variables to group by
#' @return Returns \code{data}, but grouped by \code{...} plus other variables that can be grouped along with it.
#' @author Ying Taur
#' @export
group_by_all_distinct <- function(data, ...) {
  id.vars <- quos(...)
  id.varnames <- sapply(id.vars,quo_name)
  data2 <- data %>% group_by(...) %>% summarize_all(function(x) length(unique(x))) %>% ungroup() %>%
    select_if(function(x) all(x==1))
  all.dist.vars <- unique(c(id.varnames,names(data2)))
  not.grouped <- setdiff(names(data),all.dist.vars)
  message("Grouping by [",length(all.dist.vars),"]: ",paste(all.dist.vars,collapse=", "))
  message("[Not grouped by [",length(not.grouped),"]: ",paste(not.grouped,collapse=", "),"]")
  data %>% group_by(!!!syms(all.dist.vars))
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
  group_vars <- quos(...)
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
#' Similar to \code{group_by_time}, but for a different purpose. This function groups by consecutive values of the indicator variable.
#' This is to measure how long the indicator remains in the same state.
#' @param data data frame
#' @param time time variable
#' @param indicator variable to group consecutive streaks
#' @param ... other variables to group by. These will be applied prior to grouping by time streaks.
#' @param gap time periods differing by this gap or less will be combined in the grouping variable. Default is \code{Inf}, i.e. no gap.
#' @param na.skip whether to ignore \code{NA} values in the indicator. Default is \code{FALSE}, which will just break streaks and provide a warning if they are encountered.
#' @param add Same as the add option in \code{group_by}. When TRUE, will add to groups, rather than overriding them.
#' @return Returns \code{data}, but grouped by time streaks
#' @author Ying Taur
#' @export
group_by_time_streaks <- function(data,time,indicator, ... ,gap=Inf,na.skip=FALSE,add=FALSE) {
  time <- enquo(time)
  indicator <- enquo(indicator)
  group_vars <- quos(...)


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
#' @param replace sample with or without replacement?
#' @param weight sampling weights.
#'
#' @return a subset of the original grouped data frame
#'
#' @examples
#' mtcars %>% group_by(gear) %>% sample_groups(2)
#' @export
sample_groups = function(grouped_df,size,replace=FALSE,weight=NULL) {
  grp_var <- grouped_df %>%
    groups %>%
    unlist %>%
    as.character
  if (length(grp_var)==0) {
    warning("YTWarning: no group detected.")
  }
  random_grp <- grouped_df %>%
    summarise() %>%
    sample_n(size, replace, weight) %>%
    mutate(unique_id = 1:NROW(.))
  grouped_df %>%
    right_join(random_grp, by=grp_var) %>%
    group_by_(grp_var)
}






#' Get Rows (optimized for timeline plots)
#'
#' Given timeline event data with event type labels and start/stop times, calculate rows.
#' If requested, this will attempt to save vertical plot space by placing two event types on the same row, where possible.
#' @param start vector of event start times (numeric or Date).
#' @param stop vector of event stop times (numeric or Date).
#' @param row vector of event types. Can be a list of more than one vector.
#' @param by optional grouping variable (vector or list of vectors), where events of the same group will be kept to together. Default is \code{NULL}
#' @param row.overlap whether or not the same row value can overlap. TRUE: each value is always one row FALSE: each row can occupy several rows if necessary
#' @param min.gap the minimum gap allowed before 2 different row values can be combined. Inf: different row values can never share the same row position. 0: fit different rows as much as possible.
#' @return Returns a vector of row number assignments for each time event.
#'
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
#' g1 <- pt.meds %>%
#'   mutate(row=get.row(startday,endday,row=med.clean,by=med.class)) %>%
#'   plot.meds("strictly one row per med\n(no.row.overlap=FALSE and min.gap=Inf), arranged by class")
#'
#' g2 <- pt.meds %>%
#'   mutate(row=get.row(startday,endday,row=med.clean,by=med.class,no.row.overlap=TRUE)) %>%
#'   plot.meds("same meds are in different rows, if they overlap\nthe same time (no.row.overlap=TRUE)")
#'
#' g3 <- pt.meds %>%
#'   mutate(row=get.row(startday,endday,row=med.clean,min.gap=1,by=med.class)) %>%
#'   plot.meds("To save space, different meds can be in the same row,\nas long as they are sufficiently separated (min.gap=1)")
#'
#' g4 <- pt.meds %>%
#'   mutate(row=get.row(startday,endday,no.row.overlap=TRUE)) %>%
#'   plot.meds("Arrange everything in as few rows as possible\n(row=NULL, by=NULL, no.row.overlap=TRUE)")
#'
#' gridExtra::grid.arrange(g1,g2,g3,g4)
#' @export
get.row <- function(start,stop,row=NULL,by=NULL,no.row.overlap=FALSE,min.gap=Inf) {
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
  if (no.row.overlap) {
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
    arrange(by,row2,row1) %>%
    mutate(finalrow=paste(by,row2,row1,sep=";;"),
           finalrow=fct_inorder(finalrow),
           finalrow=as.numeric(finalrow)) %>%
    arrange(i)
  return(t3$finalrow)
}



#' Get Rows (optimized for timeline plots) OLD
#'
#' Given timeline event data with event type labels and start/stop times, calculate rows.
#' If requested, this will attempt to save vertical plot space by placing two event types on the same row, where possible.
#' @param start vector of event start times (numeric or Date).
#' @param stop vector of event stop times (numeric or Date).
#' @param row vector of event types. Can be original row assignments or event labels.
#' @param by optional grouping variable (vector or list of vectors), where events of the same group will be kept to together. Default is \code{NULL}
#' @param min.gap minimum allowable gap between two different event types, if they are to be placed on the same row. Default is \code{Inf}: no row merging, \code{0} tries to perform as much merging as possible.
#' @return Returns a vector of row number assignments for each time event.
#' @author Ying Taur
#' @export
get.row.OLD <- function(start,stop,row,by=NULL,min.gap=Inf) {
  # start=medssub$start_day;stop=medssub$stop_day;row=medssub$y.row;by=list(medssub$abx_class,medssub$med_class3);min.gap=0
  if (min.gap<0) {
    stop("YTError: min.gap must be greater than 0")
  }
  if (length(start)==0|length(stop)==0) {return(NA_integer_)}
  if (!is.null(by)) {
    d <- data.frame(start,stop,row,by)
    by.list <- setdiff(names(d),c("start","stop","row"))
    dd <- d %>%
      mutate(orig.order=1:n()) %>%
      group_by_(.dots=by.list) %>%
      mutate(newrow=get.row(start,stop,row,min.gap=min.gap)) %>%
      ungroup() %>%
      arrange_(.dots=c(by.list,"newrow"))
    dd$newrow2 <- do.call(paste,dd[,c(by.list,"newrow")])
    dd <- dd %>%
      mutate(#newrow2=paste(by,newrow),
        newrow2=factor(newrow2,levels=unique(newrow2)),
        newrow2=as.numeric(newrow2)) %>%
      arrange(orig.order)
    return(dd$newrow2)
  }

  d <- data.frame(start,stop,row)
  d.collapse <- d %>% group_by(row) %>%
    summarize(start=min(start),stop=max(stop)) %>% ungroup() %>%
    mutate(y.row1=row_number(stop),
           y.row2=row_number(start)-n())

  d.row.test <- plyr::adply(0:(nrow(d.collapse)-1),1,function(overlap) {
    d.test <- d.collapse %>%
      mutate(y.row2=y.row2+overlap,
             y.row3=ifelse(y.row2>=1,y.row2,y.row1))
    overlap.check <- d.test %>% filter(y.row3<=overlap) %>%
      group_by(y.row3) %>% filter(n()==2) %>%
      arrange(start) %>%
      summarize(start1=start[1],stop1=stop[1],start2=start[2],stop2=stop[2]) %>%
      mutate(gap=start2-stop1,
             overlaps=start2-stop1<=min.gap)
    data.frame(overlap,n.rows=n_distinct(d.test$y.row3),
               gap=suppressWarnings(min(overlap.check$gap)))
  },.id=NULL)
  d.use.row <- d.row.test %>% filter(gap>=min.gap) %>% arrange(n.rows,desc(gap)) %>% slice(1)
  d.final <- d.collapse %>%
    mutate(y.row2=y.row2+d.use.row$overlap,
           y.row3=ifelse(y.row2>=1,y.row2,y.row1),
           y.row3=dense_rank(y.row3))
  newrow <- d.final$y.row3[match(d$row,d.final$row)]
  return(newrow)
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
  select_vars <- quos(...)
  select_var_names <- sapply(select_vars,quo_name)
  select_vars_keep <- select_vars[select_var_names %in% names(data)]
  data %>% select(!!!select_vars_keep)
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









