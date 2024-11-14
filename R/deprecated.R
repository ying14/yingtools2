
# from microbiota.R -------------------------------------------------------



#' Calculate the `taxhorn` distance
#'
#' `r lifecycle::badge("deprecated")`
#' Ephraim Slamka helped to develop this metric, in which the Horn distance is calculated over after
#' collapsing at each taxonomic level and then taking the weighted average of distance values.
#' @param phy phyloseq object
#'
#' @return distance metric of `taxhorn` distances
#' @export
#'
#' @examples
calc.taxhorn.distance <- function(phy) {
  message("YTNote: calc.taxhorn.distance is deprecated. Consider using distance2(phy, 'mean.horn')")

  fn <- function(x){
    set <- x[-1]
    weights <- length(set):1
    sum(set*weights) / sum(weights)
  }
  method <- "horn"

  phy <- phyloseq(otu_table(phy),tax_table(phy))
  ranks <- rank_names(phy)
  samples <- sample_names(phy)
  # create multiple phyloseq objects, collapsed at the
  # Superkingdom, Phylum, .... , Species level.
  phy.levels <- ranks %>% seq_along() %>%
    map(~ranks[1:.x]) %>% map(~phy.collapse(phy,taxranks=.x)) %>%
    setNames(ranks)
  phy.levels <- c(phy.levels,list("asv"=phy))
  all.levels <- names(phy.levels)
  # calculate the distance matrix (metric=method) for each level.
  # this is a list of distance matrices.
  dist.levels <- phy.levels %>% map(~distance(.x,method=method))
  # run get.pairwise() to get a list of pairwise distances.
  pairwise.levels <- dist.levels %>% imap(~{
    newname <- str_glue("dist.{.y}")
    get.pairwise(.x) %>% rename(!!sym(newname):=dist)
  })
  pairwise.all <- pairwise.levels[[1]]

  for (i in seq_along(all.levels)[-1]) {
    pairwise.all <- pairwise.all %>% full_join(pairwise.levels[[i]],by=c("sample1","sample2"))
  }
  pairwise.melt <- pairwise.all %>% pivot_longer(cols=-c(sample1,sample2),
                                                 names_to="dist.type",values_to="dist")
  pairwise.calcdist <- pairwise.melt %>% group_by(sample1,sample2) %>%
    summarize(dist.list=list(setNames(dist,dist.type)),
              dist=map_dbl(dist.list,fn),
              .groups = "drop")
  # if (show.work) {
  #   return(pairwise.calcdist)
  # }
  taxdist <- get.dist(pairwise.calcdist)
  taxdist
}

# Calculate distances for specified samples from phyloseq
#
# Calculate distances from a phyloseq object. This is the similar to [calc.distance()],
# except that this does not return a distance matrix. Instead, it returns a vector of distances,
# only calculating the comparisons you specified.
# @param sample1 a character vector specifying the first sample(s) for comparison. Should be a sample in `phy`.
# @param sample2 a character vector specifying the second sample(s) for comparison. Should be a sample in `phy`.
# @param phy a phyloseq object containing the samples to be compared.
# @param method the distance metric method to be used. Can be a method from
# [`phyloseq`][`phyloseq::distanceMethodList`], or `"taxhorn'`.
#
# @return a vector of distances, corresponding to `sample1` and `sample2`.
# @export
#
# @examples
# library(tidyverse)
# tbl <- tibble(sample1=c("191A", "228A", "132A", "1045", "179B"),
#               sample2=c("198A", "205B", "202C", "175B", "192D"))
# tbl.dists <- tbl %>%
#   mutate(taxhorn.dist=calc.pairwise(sample1,sample2,cid.phy,method="taxhorn"))
# calc.pairwise <- function(sample1,sample2,phy,method="bray") {
#   message("YTNote: calc.pairwise is deprecated. Consider using calc.pairwise.dist")
#   stopifnot(length(sample1)==length(sample2))
#   stopifnot(all(c(sample1,sample2) %in% sample_names(phy)))
#   dist <- map2_dbl(as.character(sample1),as.character(sample2),~{
#     if (.x==.y) return(0)
#     physub <- prune_samples(c(.x,.y),phy)
#     calc.distance(physub,method=method)
#   })
#   return(dist)
# }



# Calculate distance matrix from phyloseq data
#
# Basically same as [phyloseq::distance()], but adds `taxhorn` metric
# @param phy phyloseq object
# @param method character string indicating distance metric to be calculated. Can be a method from
# [`phyloseq`][`phyloseq::distanceMethodList`], or `"taxhorn'`, or a function.
# @param ... passed to [phyloseq::distance()]
#
# @return a distance metric
# @export
# #'
# #' @examples
# calc.distance <- function(phy, method, ...) {
#   message("YTNote: calc.distance is deprecated. Consider using distance2")
#   if (rlang::is_function(method)) {
#     dist <- method(phy)
#   } else if (is.character(method)) {
#     if (method=="taxhorn") {
#       dist <- calc.taxhorn.distance(phy)
#     } else {
#       dist <- distance(physeq=phy, method=method, ...)
#     }
#   }
#   return(dist)
# }



#' Calculate axis limits
#'
#' `r lifecycle::badge("deprecated")`
#' Determines the actual limits of X and Y, for a given ggplot object. This is used by [gg.align.xlim()].
#' @param gg the ggplot object
#' @return a list containing inforation about limits for X and Y.
#' @example
#' g1 <- ggplot(mtcars,aes(x=mpg)) + geom_histogram()
#' g2 <- ggplot(mtcars,aes(x=mpg,y=disp,color=factor(cyl))) + geom_point()
#' g3 <- ggplot(mtcars,aes(x=mpg)) + geom_histogram(bins=3) + coord_cartesian(expand=FALSE)
#' g4 <- ggplot(mtcars,aes(x=mpg,y=disp)) + geom_point() + coord_cartesian(xlim=c(2,55),expand=TRUE)
#' gg.axis.limits(g1)
#' gg.axis.limits(g2)
#' gg.axis.limits(g3)
#' gg.axis.limits(g4)
#'
#' g1 <- ggplot(mtcars,aes(x=mpg)) + geom_histogram() + scale_x_log10()
#' g2 <- ggplot(mtcars,aes(x=mpg,y=disp,color=factor(cyl))) + geom_point() + scale_x_log10()
#' g3 <- ggplot(mtcars,aes(x=mpg)) + geom_histogram(bins=3) + coord_cartesian(expand=FALSE) + scale_x_log10()
#' g4 <- ggplot(mtcars,aes(x=mpg,y=disp)) + geom_point() + coord_cartesian(expand=FALSE,xlim=c(2,55)) + scale_x_log10()
#' gg.axis.limits(g1)
#' gg.axis.limits(g2)
#' gg.axis.limits(g3)
#' gg.axis.limits(g4)
#'
#' g1 <- ggplot(starwars,aes(x=eye_color)) + geom_bar()
#' g2 <- ggplot(starwars,aes(x=eye_color,y=height)) + geom_boxplot()
#' g3 <- ggplot(starwars,aes(x=eye_color,fill=species)) + geom_bar(width=3)
#' g4 <- ggplot(starwars,aes(x=eye_color,y=height)) + geom_boxplot()
#' gg.axis.limits(g1)
#' gg.axis.limits(g2)
#' gg.axis.limits(g3)
#' gg.axis.limits(g4)
#'
#' g1 <- ggplot(presidential,aes(x=start)) + geom_histogram()
#' g2 <- ggplot(presidential,aes(x=end)) + geom_histogram()
#' g3 <- ggplot(presidential,aes(y=name,yend=name,x=start,xend=end)) + geom_segment(size=5)
#' g4 <- ggplot(presidential,aes(y=name,yend=name,x=start,xend=end,fill=party)) + geom_segment(size=5)
#' gg.axis.limits(g1)
#' gg.axis.limits(g2)
#' gg.axis.limits(g3)
#' gg.axis.limits(g4)
#' @export
gg.axis.limits <- function(gg) {
  message("YTNote: gg.axis.limits is deprecated. ggfun::xrange() and ggfun::yrange() perform this function.")

  gb <- suppressMessages(ggplot_build(gg))
  coord_flip <- is(gb$layout$coord,"CoordFlip")
  # expand <- gb$layout$coord$expand
  x <- list(
    lim = gb$layout$panel_params[[1]]$x.range, #****the ultimate plot limits, post transform, post expansion, post coord lim
    # lim.fct = gb$layout$panel_scales_x[[1]]$range_c$range, #exists if categorical, and is numeric representation of lim
    # lim2 = gb$layout$panel_scales_x[[1]]$range$range, #lim is the data limits, can be numeric or factor, pre-expansion, post-transform, if not overruled by coord.
    # lim3 = gb$layout$panel_params[[1]]$x$limits, #basically same as lim
    # lim4 = gb$layout$panel_params[[1]]$x$continuous_range,
    # lim5 = gb$layout$panel_params[[1]]$x$get_limits(),
    # lim.coord=gb$layout$coord$limits$x,
    # expansion = gb$layout$panel_scales_x[[1]]$expand,
    transform = gb$layout$panel_scales_x[[1]]$trans$transform,
    inverse = gb$layout$panel_scales_x[[1]]$trans$inverse
  )
  y <- list(
    lim = gb$layout$panel_params[[1]]$y.range,
    transform = gb$layout$panel_scales_y[[1]]$trans$transform,
    inverse = gb$layout$panel_scales_y[[1]]$trans$inverse
  )
  if (is.null(x$inverse)) {
    x$coord_lim <- x$lim
  } else {
    x$coord_lim <- x$inverse(x$lim)
  }

  if (is.null(y$inverse)) {
    y$coord_lim <- y$lim
  } else {
    y$coord_lim <- y$inverse(y$lim)
  }
  return(list(x=x,y=y,coord_flip=coord_flip))
}



#' Create a color palette for taxonomy
#'
#' `r lifecycle::badge("deprecated")`
#' Given microbiota data, generate a color palette that can be used in ggplot2 plots.
#'
#' Note that the `tax.palette` formula list is evaluated in order, and should probably end in TRUE  (similar to [dplyr::case_when()]).
#' @param data taxonomic data, can be [`phyloseq`][`phyloseq::phyloseq-class`], [get.otu.melt()] data frame, or [get.tax()] data frame.
#' @param unitvar the granular column (bare unquoted) by which colors will be assigned. Default is `Species`.
#' Sometimes you might want to switch to another granular identifer, such as `otu`. Depending on the situation.
#' @param tax.palette a list of formulas used to assign colors. Each element should take the form: `"<label>" = <true/false expression> ~ <color vector>`. See examples and details.
#' @return named vector of colors, which can be used in: `ggplot( ... ) + scale_fill_manual(values = <pal> )`
#' @export
#' @examples
#' # generate a stacked plot for one subject
#' otusub <- cid.phy %>% get.otu.melt() %>% filter(Patient_ID=="221") %>%
#'   arrange(!!!syms(rank_names(cid.phy))) %>%
#'   mutate(otu=fct_inorder(otu))
#' g <- ggplot(otusub,aes(x=day,y=pctseqs,fill=otu)) +
#'   geom_col(show.legend=FALSE,width=1) +
#'   expand_limits(x=50)
#' g
#'
#' # use default bacterial palette (yt.palette3)
#' pal1 <- get.tax.palette(cid.phy,unitvar="otu")
#' legend1 <- get.tax.legend() %>%
#'   annotation_custom(xmin=30, xmax=50, ymin=0.7, ymax=1)
#' g + scale_fill_manual(values=pal1) + legend1
#'
#' # generate a custom palette
#' custom_pal <- exprs(
#'   "Gram positives" = Phylum=="Firmicutes" ~ shades("purple",variation=0.25),
#'   "Gram negatives" = Phylum=="Bacteroidetes" | Phylum=="Proteobacteria" ~ shades("red",variation=0.25),
#'   "Others" = TRUE ~ shades("gray", variation=0.25)
#' )
#' pal2 <- get.tax.palette(cid.phy,unitvar="otu",tax.palette = custom_pal)
#' legend2 <- get.tax.legend(tax.palette = custom_pal) %>%
#'   annotation_custom(xmin=30, xmax=50, ymin=0.7, ymax=1)
#' g + scale_fill_manual(values=pal2) + legend2
get.tax.palette <- function(data,unitvar=Species,tax.palette=yt.palette3) {
  # data=phy1;unitvar="Species";tax.palette=yt.palette2
  warning("YTWarning: Please note that this function is deprecated, consider using geom_taxonomy / scale_fill_taxonomy")
  requireNamespace("phyloseq",quietly=TRUE)
  unitvar <- ensym(unitvar)
  if (is(data,"phyloseq") | is(data,"taxonomyTable")) {
    data <- get.tax(data)
  }
  if (!(is.list(tax.palette) && all(map_lgl(tax.palette,is_formula)))) {
    stop("YTError: tax.palette needs to be a list of formulas!")
  }
  vars.needed <- tax.palette %>% map(~{
    rlang::f_lhs(.x) %>% all.vars()
  }) %>% simplify() %>% c(as_label(unitvar)) %>% unique() %>% as.character()
  if (!all(vars.needed %in% names(data))) {
    missing.vars <- setdiff(vars.needed,names(data))
    stop("YTError: the tax.palette has vars that were not found in data: ",paste(missing.vars,collapse=","))
  }
  tax <- data %>% select(!!!syms(vars.needed)) %>% unique() %>%
    assert_grouping_vars(id_vars=!!unitvar,stopIfTRUE = FALSE)
  is_color <- function(x) {
    iscolor <- grepl('^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$', x) |
      (x %in% c(colors(),as.character(1:8),"transparent")) |
      is.na(x)
    return(all(iscolor))
  }
  color.list <- map(tax.palette,function(exp) {
    colors <- rlang::f_rhs(exp) %>% rlang::eval_tidy()
    if (!is_color(colors)) {
      stop("YTError: not a valid color set: {paste(colors,collapse=', ')}")
    }
    criteria <- rlang::f_lhs(exp)
    # message(criteria)
    color.yes.no <- tax %>%
      mutate(criteria=!!criteria,
             criteria=criteria & !is.na(criteria)) %>%
      pull(criteria)
    # x.color <- character()
    x.color <- rep(NA_character_,length.out=nrow(tax))
    x.color[color.yes.no] <- rep(colors,length.out=sum(color.yes.no))
    return(x.color)
  }) %>% do.call(coalesce,.)
  tax$color <- color.list
  pal <- setNames(tax$color,tax[[as_label(unitvar)]])
  return(pal)
}


#' Generate tax legend
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param tax.palette a list of formulas specifying the palette. Default is [`yt.palette3`].
#' @param fontsize Font size. Default is 5.
#' @return a ggplot object showing the legend.
#' @describeIn get.tax.palette
#' @export
#' @examples
get.tax.legend <- function(tax.palette=yt.palette3,fontsize=5) {
  warning("YTWarning: Please note that this function is deprecated, consider using geom_taxonomy / scale_fill_taxonomy")
  glist <- imap(tax.palette,function(exp,label) {
    colors <- rlang::f_rhs(exp) %>% rlang::eval_tidy()
    criteria <- rlang::f_lhs(exp)
    d <- tibble(color=colors)
    divs <- seq(0,1,length.out=nrow(d)+1)
    d$xmin <- divs[-length(divs)]
    d$xmax <- divs[-1]
    ggplot() + expand_limits(x=-2) +
      geom_rect(data=d,aes(xmin=xmin,xmax=xmax,ymin=-0.5,ymax=0.5,fill=color)) +
      annotate("text",x=0,y=0,label=label,hjust=1,size=fontsize) +
      scale_fill_identity() + theme_void()
  })
  rlang::inject(gridExtra::arrangeGrob(!!!glist,ncol=1))
}

#' Generate tax legend 2
#'
#' `r lifecycle::badge("deprecated")`
get.tax.legend2 <- function(tax.palette=yt.palette3,fontsize=5,ncol=NULL,nrow=NULL) {
  warning("YTWarning: Please note that this function is deprecated, consider using geom_taxonomy / scale_fill_taxonomy")
  if (is.null(ncol) && is.null(nrow))  {
    ncol <- 1
  }
  glist <- imap(tax.palette,function(exp,label) {
    colors <- rlang::f_rhs(exp) %>% rlang::eval_tidy()
    criteria <- rlang::f_lhs(exp)
    d <- tibble(color=colors)
    divs <- seq(0,1,length.out=nrow(d)+1)
    d$xmin <- divs[-length(divs)]
    d$xmax <- divs[-1]
    ggplot() + expand_limits(x=-2) +
      geom_rect(data=d,aes(xmin=xmin,xmax=xmax,ymin=-0.5,ymax=0.5,fill=color)) +
      annotate("text",x=0,y=0,label=label,hjust=1,size=fontsize) +
      scale_fill_identity() + theme_void()
  })
  Reduce(`+`,glist) + plot_layout(ncol=ncol,nrow=nrow)
}



#' Plot tax
#'
#' `r lifecycle::badge("deprecated")`
#' @param t data frame containing melted tax data. Needs to have vars sample, pctseqs, Kingdom, ... , Species
#' @param xvar xvar by which to plot data. This should be a distinct identifier for samples.
#' @param data whether to return data frame only (`TRUE`), or  proceed with plotting (`FALSE`).
#' @param pctseqs the relative abundance column name  (default `pctseqs`)
#' @param unitvar unit variable (bare unquoted). Default `Species`
#' @param label column name (bare unquoted) for labels var. Default is `Species`.
#' @param tax.levels tax ranks.
#' @param label.pct.cutoff cutoff by which to label abundances, stored in tax.label
#'
#' @return either ggplot2 object, or data frame.
#' @examples
#' @author Ying Taur
#' @export
tax.plot <- function(t,xvar=sample,pctseqs=pctseqs,unitvar=Species,
                     label=Species,
                     tax.levels = c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                     data=TRUE,label.pct.cutoff=0.3) {
  warning("YTWarning: Please note that this function is deprecated, consider using geom_taxonomy / scale_fill_taxonomy")

  #t=get.otu.melt(phy.species)
  # tax.levels <- c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")
  xvar <- ensym(xvar)
  pctseqs <- ensym(pctseqs)
  unitvar <- ensym(unitvar)
  label <- ensym(label)

  vars <- c(as_label(xvar),as_label(pctseqs),tax.levels,as_label(unitvar),as_label(label)) %>% unique()

  if (!all(vars %in% names(t))) {
    missing.vars <- setdiff(vars,names(t))
    stop("YTError: missing var:",paste(missing.vars,collapse=","))
  }
  t <- t %>% arrange(!!!syms(tax.levels)) %>%
    mutate(!!unitvar:=fct_inorder(!!unitvar)) %>%
    group_by(!!xvar) %>% arrange(!!unitvar) %>%
    mutate(cum.pct=cumsum(!!pctseqs),
           y.text=(cum.pct + c(0,cum.pct[-length(cum.pct)])) / 2,
           y.text=1-y.text) %>%
    ungroup() %>%
    select(-cum.pct) %>%
    mutate(tax.label=ifelse(!!pctseqs>=label.pct.cutoff,as.character(!!label),""))
  # pal <- get.yt.palette(t,use.cid.colors=use.cid.colors)
  # attr(t,"pal") <- pal
  if (data) {
    return(t)
  } else {
    g <- ggplot() +
      geom_bar(data=t,aes(x=!!xvar,y=!!pctseqs,fill=!!unitvar),stat="identity",position="fill") +
      geom_text(data=t,aes(x=!!xvar,y=y.text,label=tax.label),angle=-90,lineheight=0.9) +
      scale_fill_manual(values=attr(t,"pal")) +
      theme(legend.position="none")
    return(g)
  }
}



old3.phy.collapse.base <- function(otudt,taxdt,taxranks,level,criteria,fillin.levels) {
  # declare.args(    otudt=get.otu.melt(cid.phy,sample_data=FALSE,tax_data=FALSE) %>% as.data.table(),    taxdt=get.tax(cid.phy) %>% as.data.table(),    taxranks=rank_names(cid.phy),    criteria=quo(max.pctseqs<=0.001 | pct.detectable<=0.005),    level=7,    fillin.levels=FALSE,    yingtools2:::phy.collapse.base)
  requireNamespace("data.table",quietly=TRUE)
  criteria <- enquo(criteria)
  otudt <- data.table::as.data.table(otudt)  #make sure it's data.table
  taxdt <- data.table::as.data.table(taxdt)
  nsamps <- data.table::uniqueN(otudt$sample)
  allranks <- c(taxranks,"strain")
  subscript <- function(x,i) {
    paste(x,i,sep="_")
  }
  # taxdt[,strain:=otu] %>% setnames(allranks,subscript(allranks,1))
  taxdt <- taxdt[,strain:=otu] %>% data.table::setnames(allranks,subscript(allranks,1))
  # taxdt <- taxdt %>% mutate(strain=otu) %>% rename_with(.fn=~paste(.x,"1",sep="_"),.cols=all_of(allranks))

  # look at criteria, determine necessary calculations in make.tax
  allcalcs <- rlang::exprs(n.detectable = sum(numseqs > 0),
                           pct.detectable = sum(numseqs > 0)/..nsamps,   # pct.detectable=mean(pctseqs>0),
                           mean.pctseqs = sum(pctseqs)/..nsamps,
                           median.pctseqs = median(c(pctseqs, rep(0, length.out = ..nsamps - .N))), # median.pctseqs=median(pctseqs),
                           max.pctseqs = max(pctseqs),
                           min.pctseqs = fifelse(..nsamps == .N, min(pctseqs), 0),   # min.pctseqs=min(pctseqs),
                           total.numseqs = sum(numseqs),
                           max.numseqs = max(numseqs),
                           min.numseqs = fifelse(..nsamps == .N, min(numseqs), 0),   # min.numseqs=min(numseqs),
                           n.samps = ..nsamps)
  # allcalcs <- rlang::exprs(n.detectable=sum(numseqs>0),pct.detectable=sum(numseqs>0) / nsamps,mean.pctseqs=sum(pctseqs) / nsamps,median.pctseqs=median(c(pctseqs,rep(0,length.out=nsamps-n()))),max.pctseqs=max(pctseqs),min.pctseqs=ifelse(nsamps==n(),min(pctseqs),0),total.numseqs=sum(numseqs),max.numseqs=max(numseqs),min.numseqs=ifelse(nsamps==n(),min(numseqs),0),n.samps=nsamps,n.rows=nsamps)

  # some calcs depend on other lines, determine the dependencies
  depends <- allcalcs %>% imap(~all.vars(.x) %>% intersect(names(allcalcs)) %>% c(.y))
  calcvars <- depends[all.vars(criteria)] %>% unname() %>% simplify()
  # subset of allcalc that is needed
  calcs <- allcalcs[names(allcalcs) %in% calcvars]
  make.otu <- function(ss,tt,i) {
    by1 <- subscript(allranks,i)
    by2 <- subscript(allranks,i+1)
    # ss %>% inner_join(tt,by=by1) %>% group_by(!!!syms(by2),sample) %>% summarize(pctseqs=sum(pctseqs),numseqs=sum(numseqs),.groups="drop")
    ss %>%
      merge(tt,by=by1) %>%
      .[, .(pctseqs=sum(pctseqs),numseqs=sum(numseqs)), by=c("sample",by2)]
  }
  make.tax <- function(ss,i) {
    by1 <- subscript(allranks,i)
    by2 <- subscript(allranks,i+1)
    collapse_var <- subscript("collapse",i)
    rank <- length(allranks)+1-i
    parent.groups <- by1[1:(rank-1)]
    new.tax.var.exprs <- seq_along(by1) %>% setNames(by2) %>%
      map(~{
        if (.x<rank) {
          cur.rank <- sym(by1[.x])
          expr(!!cur.rank)
        } else if (.x==rank) {
          parent <- sym(by1[rank-1])
          expr(ifelse(!!sym(collapse_var),
                      paste("<miscellaneous>",!!parent),
                      !!sym(by1[.x])))
        } else {
          expr(ifelse(!!sym(collapse_var),
                      NA_character_,
                      !!sym(by1[.x])))
        }
      })
    # tt <- ss %>% group_by(!!!syms(by1)) %>% summarize(!!!calcs0, .groups="drop") %>% mutate(!!sym(collapse_var):=!!criteria, !!!new.tax.var.exprs) %>% group_by(!!!syms(parent.groups)) %>% mutate(n.collapse=sum(!!sym(collapse_var)), nrows=n()) %>% ungroup() %>% mutate(!!sym(collapse_var):=!!sym(collapse_var) & n.collapse>1) %>% select(!!sym(collapse_var),!!!syms(by1),!!!syms(by2))
    tt <- inject(
      ss                                                              # ss %>%
      [, .(!!!calcs), by = by1]                                       # group_by(!!!syms(by1)) %>% summarize(!!!calcs, .groups = "drop") %>%
      [, (collapse_var) := !!quo_squash(criteria)]                    # mutate(!!sym(collapse_var) := !!criteria) %>%
      [, `:=`(!!!new.tax.var.exprs)]                                  # mutate(!!!new.tax.var.exprs) %>%
      [, `:=`(n.collapse=sum(!!sym(collapse_var)),                    # group_by(!!!syms(parent.groups)) %>%
              nrows = .N), by=parent.groups]                          #   mutate(n.collapse = sum(!!sym(collapse_var)), nrows = n()) %>%
      [, (collapse_var) := !!sym(collapse_var) & n.collapse>1]        # mutate(!!sym(collapse_var) := !!sym(collapse_var) & n.collapse > 1) %>%
      [, c(collapse_var,by1,by2), with=FALSE]                         # select(!!sym(collapse_var), !!!syms(by1), !!!syms(by2))
    )
    return(tt)
  }
  # each iteration checks criteria and collapses one level

  # ss <- taxdt %>% inner_join(otudt,by="otu")
  ss <- taxdt %>% merge(otudt,by="otu")
  taxmap.raw <- taxdt
  trace <- c()
  for (i in 1:level) {
    # i=1
    # message(i)
    tt <- make.tax(ss,i)
    ss <- make.otu(ss,tt,i)
    byvar <- subscript(allranks,i)
    taxmap.raw <- taxmap.raw %>% data.table::merge.data.table(tt,all.x=TRUE, by=byvar)
    # taxmap.raw <- taxmap.raw %>% left_join(tt,by=byvar)
    trace <- c(trace,nrow(tt))
  }
  by.tax <- subscript(allranks,i+1) %>% setNames(allranks) %>% map(~expr(!!sym(.x)))
  # taxmap <- taxmap.raw %>% transmute(otu,!!!by.tax)
  taxmap <- inject(taxmap.raw[, .(otu,!!!by.tax)])

  # new.tax.otu <- otudt %>% left_join(taxmap,by="otu") %>% group_by(sample,!!!syms(allranks)) %>% summarize(numseqs=sum(numseqs), pctseqs=sum(pctseqs), .groups="drop") %>% mutate(otu=paste2(!!!syms(allranks),sep="|")) %>% arrange(otu)
  new.tax.otu <- inject(
    data.table::merge.data.table(otudt,taxmap, all.x=TRUE, by="otu")                     # left_join(taxmap,by="otu") %>%
    [, .(numseqs=sum(numseqs),                                  # group_by(sample,!!!syms(allranks)) %>%
         pctseqs=sum(pctseqs)), by=c("sample",allranks)]        #       summarize(numseqs=sum(numseqs),pctseqs=sum(pctseqs),.groups="drop") %>%
    [, otu:=paste2(!!!syms(allranks),sep="|")]                  # mutate(otu=paste2(!!!syms(allranks),sep="|"))
  )
  # new.tax.otu <- as_tibble(new.tax.otu)
  if (fillin.levels) {
    for (i in seq_along(taxranks)[-1]) {
      var <- taxranks[i]
      allvars <- taxranks[i:1]
      inject(new.tax.otu[, (var):=coalesce(!!!syms(allvars))])
      # new.tax.otu <- new.tax.otu %>% mutate(!!sym(var):=coalesce(!!!syms(allvars)))
    }
  }
  # new.tax <- new.tax.otu %>% select(otu,!!!syms(taxranks)) %>% unique()
  # tcols <- c("otu",taxranks)
  # new.tax <- new.tax.otu[, ..tcols] %>% unique()
  new.tax <- new.tax.otu[, c("otu",taxranks), with=FALSE] %>% unique()
  # new.otu <- new.tax.otu %>% select(otu,sample,numseqs,pctseqs)
  # ocols <- c("otu","sample","numseqs","pctseqs")
  # new.otu <- new.tax.otu[, ..ocols]
  new.otu <- new.tax.otu[, c("otu","sample","numseqs","pctseqs"), with=FALSE]
  trace <- c(trace,nrow(new.tax)) %>% setNames(rev(allranks)[1:(level+1)])
  message(str_glue("Evaluated across levels: {paste(names(trace),collapse=', ')} ({length(trace)-1} rounds)"))
  message(str_glue("Number of taxa: {paste(trace,collapse=' -> ')} (final number of taxa)"))
  # ntaxa.final <- nrow(new.tax)
  # message(str_glue("Collapsed taxa, {ntaxa.orig} to {ntaxa.final}"))
  list(tax=new.tax,otu=new.otu)
}


#' @rdname old3.phy.collapse.bins
#' @export
old3.phy.collapse.bins <- function(x,...) UseMethod("old3.phy.collapse.bins")


#' Collapse phyloseq or otu.melt data into smaller tax bins
#'
#' Collapse [`phyloseq`][`phyloseq::phyloseq-class`] object into smaller bins, using specified criteria based on the data.
#' Examines the data at each level (e.g. otu, Species, Genus, Family, and so on), and at each level, collapses 2 or more taxa if they meet the specified criteria.
#'
#' The binning criteria can be defined multiple ways. These variables are available for criteria, for each taxon:
#'
#'   * `n.detectable` number of samples where taxa abundance is above 0.
#'
#'   * `pct.detectable` percent of samples where taxa abundance is above 0.
#'
#'   * `mean.pctseqs` mean relative abundance for a taxon.
#'
#'   * `median.pctseqs` median relative abundance for a taxon.
#'
#'   * `max.pctseqs` highest relative abundance of sequences for a taxon (across all samples)
#'
#'   * `min.pctseqs` lowest relative abundance of sequences for a taxon (across all samples)
#'
#'   * `total.numseqs` total number of sequences for a taxon (across all samples)
#'
#'   * `max.numseqs` highest number of sequences for a taxon (across all samples)
#'
#'   * `min.numseqs` lowest number of sequences for a taxon (across all samples)
#'
#'   * `n.samps` total number of samples (regardless of abundance)
#'
#'   * `n.rows`  total number of rows for a taxon
#'
#' @param phy [`phyloseq`][`phyloseq::phyloseq-class`] object to be collapsed
#' @param level number of tax levels to evaluate and collapse by, starting with `otu` and moving up. For instance, `level=4` means collapse at `otu`, `Species`, `Genus`, `Family`.
#' @param fillin.levels Whether or not to fill in `NA` values with the collapsed taxa name. Default is `FALSE`.
#' @param criteria an expression that evaluates to TRUE/FALSE, whether to collapse at a particular level. Create this using calculated stats for each taxa (see Details)
#' @return [`phyloseq`][`phyloseq::phyloseq-class`] object or data frame (depending on what was supplied), representing the data, after criteria-based taxonomic binning.
#'
#' @examples
#' # original phyloseq
#' cid.phy
#'
#' # after default binning
#' phy.collapse.bins(cid.phy)
#'
#' # customized binning
#' phy.collapse.bins(cid.phy, level = 5,
#'                   criteria = mean.pctseqs < 0.001 & n.detectable <= 2)
#' @rdname old3.phy.collapse.bins
#' @export
old3.phy.collapse.bins.phyloseq <- function(phy,
                                       level=length(rank_names(phy)),
                                       fillin.levels=FALSE,
                                       criteria=max.pctseqs<=0.001 | pct.detectable<=0.005) {
  # phy=cid.phy;criteria <- quo(max.pctseqs<=0.001 | pct.detectable<=0.005);level=7;fillin.levels=FALSE
  # declare.args(phy=cid.phy,yingtools2:::phy.collapse.bins.phyloseq)
  requireNamespace(c("phyloseq","data.table"),quietly=TRUE)
  criteria <- enquo(criteria)
  phy <- suppressMessages(prune_unused_taxa(phy))
  taxranks <- rank_names(phy)
  otudt <- get.otu.melt(phy,sample_data=FALSE,tax_data=FALSE) %>% data.table::as.data.table()
  taxdt <- get.tax(phy) %>% data.table::as.data.table()
  objset <- old3.phy.collapse.base(otudt=otudt,taxdt=taxdt,taxranks=taxranks,level=level,criteria=!!criteria,fillin.levels=fillin.levels)
  new.tax <- objset$tax %>% as_tibble()
  new.otu <- objset$otu %>%
    data.table::dcast.data.table(formula=otu ~ sample,value.var="numseqs",fill=0) %>%
    as_tibble()
  # new.otu <- new.otu %>% as_tibble() %>% pivot_wider(id_cols=otu,names_from=sample,values_from=numseqs,values_fn = sum,values_fill=0)
  new.phy <- phyloseq(set.otu(new.otu),set.tax(new.tax),sample_data(phy))
  return(new.phy)
}







#' @param data data frame, formatted as [get.otu.melt()] data. Note, it must have columns `otu`, `sample`, `numseqs`, `pctseqs`, and all values in `taxranks`.
#' @param taxranks character vector of taxonomic ranks in `data`.
#' @param sample_vars whether to include sample variables in the data.
#' Can be `TRUE` (include sample vars, and try to determine which ones),
#' `FALSE` no sample vars, or a character vector specifying col names in `data` to be included.
#' @param sample_id name of sample column identifier. Default is `"sample"`.
#' @param taxa_id name of taxon column identifier. Default is `"otu"`.
#' @rdname old3.phy.collapse.bins
#' @examples
#' # You can also enter the data in long form (rows=sample*taxa, such as that produced by [get.otu.melt()]).
#' otu <- get.otu.melt(cid.phy)
#' otu.bin <- phy.collapse.bins(otu,
#'                              taxranks = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "taxon"),
#'                              sample_vars = c("Sample_ID", "Patient_ID", "day", "day.old", "Consistency"),
#'                              level = 5,
#'                              criteria = mean.pctseqs < 0.001 & n.detectable <= 2)
#' n_distinct(otu$otu)
#' n_distinct(otu.bin$otu)
#' @export
old3.phy.collapse.bins.data.frame <- function(data,
                                         taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                                         level=length(taxranks),
                                         fillin.levels=FALSE,
                                         sample_id="sample",
                                         taxa_id="otu",
                                         abundance_var="numseqs",
                                         criteria=max.pctseqs<=0.001 | pct.detectable<=0.005,
                                         sample_vars=TRUE) {
  requireNamespace("data.table",quietly=TRUE)
  # declare.args(data=get.otu.melt(cid.phy), taxranks <- rank_names(cid.phy), criteria=quo(max.pctseqs<=0.001 | pct.detectable<=0.005), yingtools2:::phy.collapse.bins.data.frame)
  criteria <- enquo(criteria)
  needvars <- c(taxa_id, sample_id, abundance_var, taxranks)
  if (!all(needvars %in% names(data))) {
    stop(str_glue("YTError: vars not found in data: {paste(setdiff(needvars,names(data)),collapse=',')}"))
  }
  data <- data %>% rename(sample=!!sym(sample_id),otu=!!sym(taxa_id),numseqs=!!sym(abundance_var))
  rows.are.distinct <- is.distinct(data,otu,sample)
  if (!rows.are.distinct) {
    stop(str_glue("YTError: rows are not distinct across (sample_id x taxa_id)!"))
  }
  taxdt <- data %>% select(otu,!!!syms(taxranks)) %>% data.table::as.data.table() %>% unique()

  if (isTRUE(sample_vars)) { #logical and true
    message("Attempting to determine sample vars...\n")
    vars.to.check <- setdiff(names(data),c("otu","numseqs",taxranks))
    distinct_sample_vars <- data %>%
      test_if_nonvarying_by_group(id_vars=sample, test_vars=all_of(vars.to.check)) %>%
      {names(.)[.]} %>% setdiff("sample")
    sample_vars <- distinct_sample_vars
  } else if (isFALSE(sample_vars))  {
    #no sample variables
    sample_vars <- c()
  } else if (is.character(sample_vars)) {
    # sample_vars are specified do nothing
    sample_vars <- setdiff(sample_vars,"sample")
  } else {
    stop("YTError: sample_vars should be a character or logical!")
  }

  otudt <- data %>% select(otu,sample,numseqs,pctseqs) %>% data.table::as.data.table()
  # sampdt <- samp %>% data.table::as.data.table()
  objset <- old3.phy.collapse.base(otudt=otudt,taxdt=taxdt,taxranks=taxranks,level=level,criteria=!!criteria,fillin.levels=fillin.levels)
  new.otudt <- objset$otu
  new.taxdt <- objset$tax
  new.dt <- new.otudt %>%
    data.table::merge.data.table(new.taxdt, by="otu",all.x = TRUE)
  if (length(sample_vars)>0) {
    sampdt <- data %>% select(sample,!!!syms(sample_vars)) %>% distinct() %>%
      data.table::as.data.table()
    if (anyDuplicated(sampdt$sample)>0) {
      stop("YTError: sample data with selected sample vars is not unique!")
    }
    new.dt <- new.dt %>%
      data.table::merge.data.table(sampdt,by="sample",all.x = TRUE)
  }
  new.data <- new.dt %>%
    as_tibble() %>%
    rename(!!sym(sample_id):=sample,!!sym(taxa_id):=otu,!!sym(abundance_var):=numseqs)
  # new.data <- new.otu %>% left_join(new.tax,by="otu") %>% left_join(samp,by="sample") %>% rename(!!sym(taxa_id):=otu,!!sym(sample_id):=sample)
  return(new.data)
}





compareOLD.data.frame.OLD <- function(x,y,by=NULL) {

  # declare.args(x=d1,y=d1[,-1],yingtools2:::compare.data.frame)

  # x.name <- as_label(enquo(x))
  # y.name <- as_label(enquo(y))
  x.name <- deparse1(substitute(x))
  y.name <- deparse1(substitute(y))

  if (is.null(by)) {
    by <- intersect(names(x),names(y))
  }

  by.x <- names(by) %||% by
  by.y <- unname(by)
  x.is.distinct <- x %>% is.distinct(!!!syms(by.x))
  y.is.distinct <- y %>% is.distinct(!!!syms(by.y))
  if (all(by.x==by.y)) {
    by.x <- paste0(by.x,".x")
    by.y <- paste0(by.y,".y")
  }

  # both <- inner_join_replace(x,y,by=by,errorIfDifferent = FALSE)
  all <- full_join(x,y,by=by,keep=TRUE) %>%
    mutate(.status=case_when(
      !is.na(!!sym(by.x[1])) & !is.na(!!sym(by.y[1])) ~ str_glue("both {x.name}(x) and {y.name}(y)"),
      !is.na(!!sym(by.x[1])) & is.na(!!sym(by.y[1])) ~ str_glue("{x.name}(x) only"),
      is.na(!!sym(by.x[1])) & !is.na(!!sym(by.y[1])) ~ str_glue("{y.name}(y) only")
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

  message(str_glue("X <{x.name}> vs. Y <{y.name}>"))
  message(str_glue("X: {pretty_number(nrow(x))} rows ({ifelse(x.is.distinct,\"distinct\",\"not distinct\")})"))
  message(str_glue("Y: {pretty_number(nrow(y))} rows ({ifelse(y.is.distinct,\"distinct\",\"not distinct\")})"))
  all %>% count(.status,.diffs) %>% print()
  invisible(all)
}


test_if_nonvarying_by_group_OLD <- function(data,
                                            id_vars = all_of(group_vars(data)),
                                            test_vars = everything(),
                                            verbose = FALSE) {
  # data=get.otu.melt(cid.phy);id_vars=quo(sample);test_vars=quo(everything())


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
  data.rootgroup <- setNames(rep_along(id_var_names,TRUE),id_var_names)

  # dt.testing <- data %>% ungroup() %>%
  #   as.data.table(key=id_var_names) %>%
  #   .[,group:=.GRP,by=id_var_names]
  # groups <- dt.testing[["group"]] %>% unique()
  # to.test <- test_var_names
  # test <- function(x) {
  #   uniqueN(x)==1
  # }
  #
  # for (i in groups) {
  #   # message(i)
  #   results <- dt.testing[group==i, lapply(.SD, test), .SDcols=to.test] %>% unlist()
  #   to.test <- names(results)[results]
  #   if (length(to.test)==0) {
  #     break
  #   }
  # }
  #whatever is left is non-varying.
  data.testing <- setNames(test_var_names %in% to.test,test_var_names)

  data.testing <- data %>% ungroup() %>% as.data.table(key=id_var_names) %>%
    .[, lapply(.SD, uniqueN), .SDcols=test_var_names, by=id_var_names] %>%
    .[, lapply(.SD, function(x) all(x==1)), .SDcols=test_var_names] %>%
    as_tibble() %>% unlist()

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


phy.collapse.base.old <- function(otu,tax,taxranks,level,criteria,fillin.levels) {

  # phy=cid.phy;criteria <- quo(max.pctseqs<=0.001 | pct.detectable<=0.005);level=7;fillin.levels=FALSE
  criteria <- enquo(criteria)
  # ntaxa.orig <- nrow(tax)
  nsamps <- n_distinct(otu$sample)
  allranks <- c(taxranks,"strain")
  tax <- tax %>% mutate(strain=otu) %>%
    rename_with(.fn=~paste(.x,"1",sep="_"),.cols=all_of(allranks))
  # look at criteria, determine necessary calculations in make.tax
  allcalcs <- rlang::exprs(n.detectable=sum(pctseqs>0),
                           pct.detectable=n.detectable / nsamps,  # pct.detectable=mean(pctseqs>0),
                           mean.pctseqs=sum(pctseqs) / nsamps,
                           median.pctseqs=median(c(pctseqs,rep(0,length.out=nsamps-n()))),  # median.pctseqs=median(pctseqs),
                           max.pctseqs=max(pctseqs),
                           min.pctseqs=ifelse(nsamps==n(),min(pctseqs),0),   # min.pctseqs=min(pctseqs),
                           total.numseqs=sum(numseqs),
                           max.numseqs=max(numseqs),
                           min.numseqs=ifelse(nsamps==n(),min(numseqs),0),  # min.numseqs=min(numseqs),
                           n.samps=nsamps,
                           n.rows=nsamps)
  # some calcs depend on other lines, determine the dependencies
  depends <- allcalcs %>% imap(~all.vars(.x) %>% intersect(names(allcalcs)) %>% c(.y))
  calcvars <- depends[all.vars(criteria)] %>% unname() %>% simplify()
  # subset of allcalc that is needed
  calcs <- allcalcs[names(allcalcs) %in% calcvars]

  make.otu <- function(ss,tt,level) {
    by1 <- paste(allranks,level,sep="_")
    by2 <- paste(allranks,level+1,sep="_")
    ss %>%
      inner_join(tt,by=by1) %>%
      group_by(!!!syms(by2),
               sample) %>%
      summarize(pctseqs=sum(pctseqs),
                numseqs=sum(numseqs),
                .groups="drop")
  }
  make.tax <- function(ss,level) {
    by1 <- paste(allranks,level,sep="_")
    by2 <- paste(allranks,level+1,sep="_")
    collapse_var <- str_glue("collapse{level}")
    rank <- length(allranks)+1-level
    parent.groups <- by1[1:(rank-1)]
    new.tax.var.exprs <- seq_along(by1) %>% setNames(by2) %>%
      map(~{
        if (.x<rank) {
          cur.rank <- sym(by1[.x])
          expr(!!cur.rank)
        } else if (.x==rank) {
          parent <- sym(by1[rank-1])
          expr(ifelse(!!sym(collapse_var),
                      paste("<miscellaneous>",!!parent),
                      !!sym(by1[.x])))
        } else {
          expr(ifelse(!!sym(collapse_var),
                      NA_character_,
                      !!sym(by1[.x])))
        }
      })
    tt <- ss %>%
      group_by(!!!syms(by1)) %>%
      summarize(!!!calcs,
                .groups="drop") %>%
      mutate(!!sym(collapse_var):=!!criteria,
             !!!new.tax.var.exprs) %>%
      group_by(!!!syms(parent.groups)) %>%
      mutate(n.collapse=sum(!!sym(collapse_var)),
             nrows=n()) %>%
      ungroup() %>%
      mutate(!!sym(collapse_var):=!!sym(collapse_var) & n.collapse>1,
             !!!new.tax.var.exprs) %>%
      select(!!sym(collapse_var),
             !!!syms(by1),!!!syms(by2))
    return(tt)
  }
  # each iteration checks criteria and collapses one level
  ss <- tax %>% inner_join(otu,by="otu")
  taxmap.raw <- tax
  trace <- c()
  for (i in 1:level) {
    # message(i)
    tt <- make.tax(ss,i)
    ss <- make.otu(ss,tt,i)
    byvar <- paste(allranks,i,sep="_")
    taxmap.raw <- taxmap.raw %>% left_join(tt,by=byvar)
    trace <- c(trace,nrow(tt))
  }

  by.tax <- paste(allranks,i+1,sep="_") %>% setNames(allranks) %>% map(~expr(!!sym(.x)))
  taxmap <- taxmap.raw %>% transmute(otu,!!!by.tax)

  # coll_vars <- paste0("collapse",1:level) %>% rev()
  # taxmap.raw %>% count(!!!syms(coll_vars))
  new.tax.otu <- otu %>%
    left_join(taxmap,by="otu") %>%
    group_by(sample,!!!syms(allranks)) %>%
    summarize(numseqs=sum(numseqs),
              pctseqs=sum(pctseqs),
              .groups="drop") %>%
    mutate(otu=paste2(!!!syms(allranks),sep="|")) %>% arrange(otu)
  if (fillin.levels) {
    for (i in seq_along(taxranks)[-1]) {
      var <- taxranks[i]
      allvars <- taxranks[i:1]
      new.tax.otu <- new.tax.otu %>%
        mutate(!!sym(var):=coalesce(!!!syms(allvars)))
    }
  }
  new.tax <- new.tax.otu %>% select(otu,!!!syms(taxranks)) %>% unique()
  new.otu <- new.tax.otu %>% select(otu,sample,numseqs,pctseqs)

  trace <- c(trace,nrow(new.tax)) %>% setNames(rev(allranks[1:level]))

  message(str_glue("Evaluated across levels: {paste(names(trace),collapse=', ')} ({length(trace)-1} rounds)"))
  message(str_glue("Number of taxa: {paste(trace,collapse=' -> ')} (final number of taxa)"))
  # ntaxa.final <- nrow(new.tax)
  # message(str_glue("Collapsed taxa, {ntaxa.orig} to {ntaxa.final}"))
  list(tax=new.tax,otu=new.otu)
}


#' @rdname phy.collapse.bins.old
#' @export
phy.collapse.bins.old <- function(x,...) UseMethod("phy.collapse.bins.old")


#' Collapse phyloseq or otu.melt data into smaller tax bins
#'
#' Collapse phyloseq object into smaller bins, using specified criteria based on the data.
#' Examines the data at each level (e.g. otu, Species, Genus, Family, and so on), and at each level, collapses 2 or more taxa if they meet the specified criteria.
#'
#' The binning criteria can be defined multiple ways. These variables are available for criteria, for each taxon:
#'
#'   * `n.detectable` number of samples where taxa abundance is above 0.
#'
#'   * `pct.detectable` percent of samples where taxa abundance is above 0.
#'
#'   * `mean.pctseqs` mean relative abundance for a taxon.
#'
#'   * `median.pctseqs` median relative abundance for a taxon.
#'
#'   * `max.pctseqs` highest relative abundance of sequences for a taxon (across all samples)
#'
#'   * `min.pctseqs` lowest relative abundance of sequences for a taxon (across all samples)
#'
#'   * `total.numseqs` total number of sequences for a taxon (across all samples)
#'
#'   * `max.numseqs` highest number of sequences for a taxon (across all samples)
#'
#'   * `min.numseqs` lowest number of sequences for a taxon (across all samples)
#'
#'   * `n.samps` total number of samples (regardless of abundance)
#'
#'   * `n.rows`  total number of rows for a taxon
#'
#' @param phy phyloseq object to be collapsed
#' @param level number of tax levels to evaluate and collapse by, starting with `otu` and moving up. For instance, level=4 means collapse at otu, Species, Genus, Family.
#' @param fillin.levels Whether or not to fill in `NA` values with the collapsed taxa name. Default is `FALSE`.
#' @param criteria an expression that evaluates to TRUE/FALSE, whether to collapse at a particular level. Create this using calculated stats for each taxa (see Details)
#' @return phyloseq object or data frame (depending on what was supplied), representing the data, after criteria-based taxonomic binning.
#'
#' @examples
#' # original phyloseq
#' cid.phy
#'
#' # after default binning
#' phy.collapse.bins(cid.phy)
#'
#' # customized binning
#' phy.collapse.bins(cid.phy, level = 5,
#'                   criteria = mean.pctseqs < 0.001 & n.detectable <= 2)
#' @rdname phy.collapse.bins.old
#' @export
phy.collapse.bins.old.phyloseq <- function(phy,
                                           level=length(rank_names(phy)),
                                           fillin.levels=FALSE,
                                           criteria=max.pctseqs<=0.001 | pct.detectable<=0.005) {
  # phy=cid.phy;criteria <- quo(max.pctseqs<=0.001 | pct.detectable<=0.005);level=7;fillin.levels=FALSE
  criteria <- enquo(criteria)
  phy <- suppressMessages(prune_unused_taxa(phy))
  taxranks <- rank_names(phy)
  otu <- get.otu.melt(phy,sample_data=FALSE,tax_data=FALSE)
  tax <- get.tax(phy)
  objset <- phy.collapse.base.old(otu=otu,tax=tax,taxranks=taxranks,level=level,criteria=!!criteria,fillin.levels=fillin.levels)
  new.tax <- objset$tax
  new.otu <- objset$otu %>%
    pivot_wider(id_cols=otu,
                names_from=sample,
                values_from=numseqs,
                values_fn = sum,
                values_fill=0)
  new.phy <- phyloseq(set.otu(new.otu),set.tax(new.tax),sample_data(phy))
  return(new.phy)
}


#' @param data data frame, formatted as `get.otu.melt`. Note, it must have columns `otu`, `sample`, `numseqs`, `pctseqs`, and all values in `taxranks`.
#' @param taxranks character vector of taxonomic ranks in `data`.
#' @param sample_vars character vector of column names in `data` that convey sample-level information. These will be retained as such after binning.
#' @param sample_id name of sample column identifier. Default is "sample".
#' @param taxa_id name of taxon column identifier. Default is "otu".
#' @rdname phy.collapse.bins.old
#' @examples
#' #  You can also enter the data in long form (rows=sample*taxa, such as that produced by get.otu.melt).
#' otu <- get.otu.melt(cid.phy)
#' otu.bin <- phy.collapse.bins(otu,
#'                              taxranks = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "taxon"),
#'                              sample_vars = c("Sample_ID", "Patient_ID", "day", "day.old", "Consistency"),
#'                              level = 5,
#'                              criteria = mean.pctseqs < 0.001 & n.detectable <= 2)
#' n_distinct(otu$otu)
#' n_distinct(otu.bin$otu)
#' @export
phy.collapse.bins.old.data.frame <- function(data,
                                             taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                                             level=length(taxranks),
                                             fillin.levels=FALSE,
                                             sample_id="sample",
                                             taxa_id="otu",
                                             criteria=max.pctseqs<=0.001 | pct.detectable<=0.005,
                                             sample_vars=NULL) {


  # data <- get.otu.melt(cid.phy);taxranks <- rank_names(cid.phy);level=4;fillin.levels=FALSE;criteria=quo(max.pctseqs<=0.001 | pct.detectable<=0.005);sample_vars=NULL
  criteria <- enquo(criteria)
  needvars <- c(taxa_id,sample_id,"numseqs","pctseqs",taxranks)
  if (!all(needvars %in% names(data))) {
    stop(str_glue("YTError: vars not found in data: {paste(setdiff(needvars,names(data)),collapse=',')}"))
  }
  data <- data %>% rename(sample=!!sym(sample_id),otu=!!sym(taxa_id))
  rows.are.distinct <- is.distinct(data,otu,sample)
  if (!rows.are.distinct) {
    stop(str_glue("YTError: rows are not distinct across (sample_id x taxa_id)!"))
  }

  tax <- data %>% select(otu,!!!syms(taxranks)) %>% unique()
  sample_vars <- setdiff(sample_vars,"sample")
  if (is.null(sample_vars)) {
    message("Attempting to determine sample vars...\n")
  }
  samp <- data %>% select(sample,!!!syms(sample_vars)) %>% unique()
  otu <- data %>% select(otu,sample,numseqs,pctseqs)
  # phy.collapse()
  objset <- phy.collapse.base.old(otu=otu,tax=tax,taxranks=taxranks,level=level,criteria=!!criteria,fillin.levels=fillin.levels)
  new.data <- objset$otu %>%
    left_join(objset$tax,by="otu") %>%
    left_join(samp,by="sample") %>%
    rename(!!sym(taxa_id):=otu,!!sym(sample_id):=sample)
  return(new.data)

}



#' Get YT Palette
#' @param tax either a data.frame, phyloseq, or tax_table
#' @param use.cid.colors whether to use classic CID colors
#' @return a color palette that can be used in \code{ggplot2}
#' @examples
#' @author Ying Taur
#' @export
get.yt.palette <- function(tax,use.cid.colors=TRUE) {
  requireNamespace("phyloseq",quietly=TRUE)
  message("YTNote: get.yt.palette() and get.yt.palette2 are deprecated. Try using get.tax.palette().")

  if (class(tax)[1] %in% c("phyloseq","taxonomyTable")) {
    tax <- get.tax(tax.obj)
  }
  ranks <- c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")
  if (!all(ranks %in% names(tax))) {
    stop("YTError: need to have taxon levels: Superkingdom, Phylum, Class, Order, Family, Genus, Species")
  }
  tax.dict <- tax[,ranks] %>% distinct()
  #bacteria are shades of gray by default
  tax.dict$color <- rep(shades("gray"),length.out=nrow(tax.dict))
  #proteobacteria: red
  proteo <- tax.dict$Phylum=="Proteobacteria"
  tax.dict$color[proteo] <- rep(shades("red",variation=0.4),length.out=sum(proteo))
  #bacteroidetes: cyan
  bacteroidetes <- tax.dict$Phylum=="Bacteroidetes"
  tax.dict$color[bacteroidetes] <- rep(shades("#2dbfc2",variation=0.4),length.out=sum(bacteroidetes))
  #actinobacteria: purple
  actino <- tax.dict$Phylum=="Actinobacteria"
  tax.dict$color[actino] <- rep(shades("purple",variation=0.4),length.out=sum(actino))
  #firmicutes:
  firm <- tax.dict$Phylum=="Firmicutes"
  tax.dict$color[firm] <- rep(shades("#8f7536",variation=0.3),length.out=sum(firm))
  #within firmicutes, color bacilli
  bacilli <- tax.dict$Class=="Bacilli"
  tax.dict$color[bacilli] <- rep(shades("#3b51a3",variation=0.2),length.out=sum(bacilli))
  #within firmicutes, color blautia
  blautia <- tax.dict$Genus=="Blautia"
  tax.dict$color[blautia] <- rep(shades("#f69ea0",variation=0.2),length.out=sum(blautia))
  if (use.cid.colors) {
    cid.colors.new <- c("Enterococcus"="#129246","Streptococcus"="#a89e6a","Staphylococcus"="#f1eb25")
    cid <- cid.colors.new[match(tax.dict$Genus,names(cid.colors.new))]
    tax.dict$color <- ifelse(is.na(cid),tax.dict$color,cid)
  }
  tax.palette <- structure(tax.dict$color,names=as.character(tax.dict$Species))
  tax.palette
}




#' Get YT Palette 2
#' @param tax either a data.frame, phyloseq, or tax_table
#' @param use.cid.colors whether to use classic CID colors
#' @return a color palette that can be used in \code{ggplot2}
#' @examples
#' @author Ying Taur
#' @export
get.yt.palette2 <- function (tax) {
  requireNamespace("phyloseq",quietly=TRUE)
  message("YTNote: get.yt.palette() and get.yt.palette2 are deprecated. Try using get.tax.palette().")

  if (is(tax,"phyloseq") | is(tax,"taxonomyTable")) {
    tax <- get.tax(tax)
  }
  ranks <- c("Superkingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  if (!all(ranks %in% names(tax))) {
    missing.vars <- setdiff(ranks,names(tax))
    stop("YTError: need to have taxon levels: Superkingdom, Phylum, Class, Order, Family, Genus, Species; missing: ",paste(missing.vars,collapse=","))
  }
  tax.dict <- tax[, ranks] %>% distinct()
  tax.dict$color <- rep(shades("gray", variation=0.25),length.out = nrow(tax.dict))
  proteo <- tax.dict$Phylum == "Proteobacteria"
  tax.dict$color[proteo] <- rep(shades("red", variation = 0.4), length.out = sum(proteo))
  actino <- tax.dict$Phylum == "Actinobacteria"
  tax.dict$color[actino] <- rep(shades("#A77097", variation = 0.25), length.out = sum(actino))
  bacteroidetes <- tax.dict$Phylum == "Bacteroidetes"
  tax.dict$color[bacteroidetes] <- rep(shades("#51AB9B", variation = 0.25), length.out = sum(bacteroidetes))
  clost <- tax.dict$Order == "Clostridiales"
  tax.dict$color[clost] <- rep(shades("#9C854E", variation = 0.25), length.out = sum(clost))
  lachno <- tax.dict$Family == "Lachnospiraceae"
  tax.dict$color[lachno] <- rep(shades("#EC9B96", variation = 0.25), length.out = sum(lachno))
  rumino <- tax.dict$Family == "Ruminococcaceae"
  tax.dict$color[rumino] <- rep(shades("#9AAE73", variation = 0.25), length.out = sum(rumino))
  cid.colors.new <- c(Enterococcus = "#129246", Streptococcus = "#9FB846", Staphylococcus = "#f1eb25" , Lactobacillus="#3b51a3")
  cid <- cid.colors.new[match(tax.dict$Genus, names(cid.colors.new))]
  tax.dict$color <- ifelse(is.na(cid), tax.dict$color, cid)
  tax.palette <- structure(tax.dict$color, names = as.character(tax.dict$Species))
  tax.palette
}




#' Collapse phyloseq into smaller bins
#'
#' Collapse phyloseq object into smaller bins, using specified criteria based on the data.
#'
#' These variables are available for criteria, for each taxon:
#'
#'   * `n.detectable` number of samples where taxa abundance is above 0.
#'
#'   * `pct.detectable` percent of samples where taxa abundance is above 0.
#'
#'   * `mean.pctseqs` mean relative abundance for a taxon.
#'
#'   * `median.pctseqs` median relative abundance for a taxon.
#'
#'   * `max.pctseqs` highest relative abundance of sequences for a taxon (across all samples)
#'
#'   * `min.pctseqs` lowest relative abundance of sequences for a taxon (across all samples)
#'
#'   * `total.numseqs` total number of sequences for a taxon (across all samples)
#'
#'   * `max.numseqs` highest number of sequences for a taxon (across all samples)
#'
#'   * `min.numseqs` lowest number of sequences for a taxon (across all samples)
#'
#'   * `n.samps` total number of samples (regardless of abundance)
#'
#'   * `n.rows`  total number of rows for a taxon
#'
#' @param phy phyloseq object to be collapsed
#' @param level number of tax levels to evaluate and collapse by, starting with `otu` and moving up. For instance, level=4 means collapse at otu, Species, Genus, Family.
#' @param fillin.levels Whether or not to fill in `NA` values with the collapsed taxa name. Default is `FALSE`.
#' @param criteria an expression that evaluates to TRUE/FALSE, whether to collapse at a particular level. Create this using calculated stats for each taxa (see Details)
#'
#' @return collapsed phyloseq object
#' @export
#'
#' @examples
#' phy.binned <- phy.collapse.bins(cid.phy)
#' phy.binned2 <- phy.collapse.bins(cid.phy, level=5,
#'                                  criteria=mean.pctseqs<0.001 & n.detectable<=2)
phy.collapse.bins.old <- function(phy,
                                  level=length(rank_names(phy)),
                                  fillin.levels=FALSE,
                                  criteria=max.pctseqs<=0.001 | pct.detectable<=0.005) {
  # criteria <- quo(max.pctseqs<=0.001 | pct.detectable<=0.005);level=7;fillin.levels=TRUE
  criteria <- enquo(criteria)
  ntaxa.orig <- ntaxa(phy)
  phy <- suppressMessages(prune_unused_taxa(phy))
  nsamps <- nsamples(phy)
  taxranks <- rank_names(phy)
  allranks <- c(taxranks,"strain")
  # look at criteria, determine necessary calculations in make.tax
  allcalcs <- rlang::exprs(n.detectable=sum(pctseqs>0),
                           pct.detectable=n.detectable / nsamps,  # pct.detectable=mean(pctseqs>0),
                           mean.pctseqs=sum(pctseqs) / nsamps,
                           median.pctseqs=median(c(pctseqs,rep(0,length.out=nsamps-n()))),  # median.pctseqs=median(pctseqs),
                           max.pctseqs=max(pctseqs),
                           min.pctseqs=ifelse(nsamps==n(),min(pctseqs),0),   # min.pctseqs=min(pctseqs),
                           total.numseqs=sum(numseqs),
                           max.numseqs=max(numseqs),
                           min.numseqs=ifelse(nsamps==n(),min(numseqs),0),  # min.numseqs=min(numseqs),
                           n.samps=nsamps,
                           n.rows=nsamps)
  # some calcs depend on other lines, determine the dependencies
  depends <- allcalcs %>% imap(~all.vars(.x) %>% intersect(names(allcalcs)) %>% c(.y))
  calcvars <- depends[all.vars(criteria)] %>% unname() %>% simplify()
  # subset of allcalc that is needed
  calcs <- allcalcs[names(allcalcs) %in% calcvars]

  make.otu <- function(ss,tt,level) {
    by1 <- paste(allranks,level,sep="_")
    by2 <- paste(allranks,level+1,sep="_")
    ss %>%
      inner_join(tt,by=by1) %>%
      group_by(!!!syms(by2),
               sample) %>%
      summarize(pctseqs=sum(pctseqs),
                numseqs=sum(numseqs),
                .groups="drop")
  }
  make.tax <- function(ss,level) {
    # level=1
    by1 <- paste(allranks,level,sep="_")
    by2 <- paste(allranks,level+1,sep="_")
    collapse_var <- str_glue("collapse{level}")
    rank <- length(allranks)+1-level
    parent.groups <- by1[1:(rank-1)]
    new.tax.var.exprs <- seq_along(by1) %>% setNames(by2) %>%
      map(~{
        if (.x<rank) {
          expr(!!sym(by1[.x]))
        } else if (.x==rank) {
          parent <- rank-1
          expr(ifelse(!!sym(collapse_var),
                      paste("<miscellaneous>",!!sym(by1[parent])),
                      !!sym(by1[.x])))
        } else {
          expr(ifelse(!!sym(collapse_var),
                      NA_character_,
                      !!sym(by1[.x])))
        }
      })
    tt <- ss %>%
      group_by(!!!syms(by1)) %>%
      summarize(!!!calcs,
                .groups="drop") %>%
      mutate(!!sym(collapse_var):=!!criteria,
             !!!new.tax.var.exprs) %>%
      group_by(!!!syms(parent.groups)) %>%
      mutate(n.collapse=sum(!!sym(collapse_var)),
             nrows=n()) %>%
      ungroup() %>%
      mutate(!!sym(collapse_var):=!!sym(collapse_var) & n.collapse>1,
             !!!new.tax.var.exprs) %>%
      # filter(!!sym(collapse_var)) %>%
      select(!!sym(collapse_var),
             !!!syms(by1),!!!syms(by2))
    return(tt)
  }
  otu <- get.otu.melt(phy,sample_data=FALSE,tax_data = FALSE)
  tax <- get.tax(phy) %>% mutate(strain=otu) %>%
    rename_with(.fn=~paste(.x,"1",sep="_"),.cols=all_of(allranks))
  # each iteration checks criteria and collapses one level
  ss <- tax %>% inner_join(otu,by="otu")
  taxmap.raw <- tax
  for (i in 1:level) {
    tt <- make.tax(ss,i)
    ss <- make.otu(ss,tt,i)
    byvar <- paste(allranks,i,sep="_")
    taxmap.raw <- taxmap.raw %>% left_join(tt,by=byvar)
  }
  by.tax <- paste(allranks,i+1,sep="_") %>% setNames(allranks) %>% map(~expr(!!sym(.x)))
  taxmap <- taxmap.raw %>% mutate(otu,!!!by.tax)
  # coll_vars <- paste0("collapse",1:level) %>% rev()
  # xx=taxmap %>% mutate(asdf=coalesce_indicators(!!!syms(coll_vars),else.value="other",first.hit.only=TRUE))
  new.tax.otu <- otu %>%
    left_join(taxmap,by="otu") %>%
    pivot_wider(id_cols=c(!!!syms(allranks)),
                names_from=sample,
                values_from=numseqs,
                values_fn = sum,
                values_fill=0) %>%
    mutate(otu=paste2(!!!syms(allranks),sep="|")) %>% arrange(otu)

  if (fillin.levels) {
    for (i in seq_along(taxranks)[-1]) {
      var <- taxranks[i]
      allvars <- taxranks[i:1]
      new.tax.otu <- new.tax.otu %>%
        mutate(!!sym(var):=coalesce(!!!syms(allvars)))
    }
  }
  new.tax <- new.tax.otu %>% select(otu,!!!syms(taxranks))
  new.otu <- new.tax.otu %>% select(otu,!!!syms(sample_names(phy)))
  new.phy <- phyloseq(set.otu(new.otu),set.tax(new.tax),sample_data(phy))
  ntaxa.final <- ntaxa(new.phy)
  message(str_glue("Collapsed taxa, {ntaxa.orig} to {ntaxa.final}"))
  return(new.phy)
}



#' Read OTU table from text file
#'
#' Used for uparse pipeline to read in otu table.
#'
#' Assumes tab-delimited file, with 'OTUID' to specify otu column.
#' @param otu.file text file to be read, containing otu table data
#' @param otu.row.names specify column listing OTU names. This is passed to \code{read.delim}; i.e. can be vector of accual row names, single number of column, or character string name of the column.
#' @return Dataframe containing otu table
#' @export
read.otu.table <- function(otu.file,row.names="OTUId") {
  warning("YTWarning: Please note that this function is deprecated, uparse/mothur functions have been taken offsite.")
  otu <- read.delim(otu.file,header=TRUE,check.names=FALSE,row.names=row.names) %>%
    rownames_to_column("otu")
  return(otu)
}

#' Read Tree File (Label-Fix)
#'
#' Read in tree file created by uparse pipeline.
#'
#' Functions like \code{read.tree} have trouble reading uparse-generated trees, because the OTU names contain semicolons(;), e.g. "\code{'OTU_2080;size=5;'}". This function temporarily replaces \code{;} with \code{__}, reads in successfully, and reverts back to names with semicolons. Also, removes the single quotes (\code{\'}) from the name. If read in this way, the tree can be quickly merged into a phyloseq object.
#'
#' @param tree.file Newick-formatted text file
#' @return Returns a \code{phylo} object, with original labels.
#' @examples
#' b <- import_biom("uparse/total.8.otu-tax.biom")
#' tr <- read.tree.uparse("uparse/total.10.tree")
#' phy <- merge_phyloseq(b,tr)
#' @author Ying Taur
#' @export
read.tree.uparse <- function(tree.file) {
  warning("YTWarning: Please note that this function is deprecated, uparse/mothur functions have been taken offsite.")

  tree.text <- scan(tree.file,what=character(),quiet=TRUE)
  #replace ; with __, and place single quotes around, if not already there. (qiime places quotes, mothur does not)
  new.tree.text <- gsub("'?(OTU_[0-9]+);(size=[0-9]+);'?","'\\1__\\2__'",tree.text)
  #new.tree.text <- gsub("(OTU_[0-9]+);(size=[0-9]+);","\\1__\\2__",tree.text)
  tr <- ape::read.tree(text=new.tree.text)
  tr$tip.label <- gsub("(OTU_[0-9]+)__(size=[0-9]+)__","\\1;\\2;",tr$tip.label)
  tr$node.label <- gsub("(OTU_[0-9]+)__(size=[0-9]+)__","\\1;\\2;",tr$node.label)
  #tr$tip.label <- gsub("'?(OTU_[0-9]+)__(size=[0-9]+)__'?","\\1;\\2;",tr$tip.label)
  #tr$node.label <- gsub("'?(OTU_[0-9]+)__(size=[0-9]+)__'?","\\1;\\2;",tr$node.label)
  return(tr)
}


#' Read in mothur taxonomy file.
#'
#' @param tax.file mothur taxonomy file to be read.
#' @return Returns a data frame containing taxonomy
#' @author Ying Taur
#' @export
read.mothur.taxfile <- function(tax.file) {
  warning("YTWarning: Please note that this function is deprecated, uparse/mothur functions have been taken offsite.")
  taxlevels <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
  tbl <- read.delim(tax.file,header=FALSE,row.names=NULL) %>% rename(otu=V1,taxonomy=V2) %>%
    mutate(taxonomy=sub(";$","",taxonomy)) %>%
    separate(taxonomy,taxlevels,sep=";",remove=FALSE)
  for (lvl in taxlevels) {
    pctvar <- paste0(lvl,".pid")
    tbl[[pctvar]] <- as.numeric(str_extract(tbl[[lvl]],middle.pattern("\\(","[0-9.]+","\\)")))
    tbl[[lvl]] <- sub("\\([0-9.]+\\)","",tbl[[lvl]])
    tbl[[lvl]] <- sub("[kpcofgs]_+","",tbl[[lvl]])
  }
  return(tbl)
}

#' Import UPARSE pipeline data and create phyloseq
#'
#' Reads folder and looks for key files used to create phyloseq object.
#'
#' @param dirpath directory path (character) specifying folder where uparse data is.
#' @param otu.file otu table file. Default is 'total.6.otu-table.txt'
#' @param tax.file tax file (blastn). Default is 'total.5.repset.fasta.blastn.refseq_rna.txt'. Specify \code{NULL} to skip.
#' @param repseq.file rep seq file (fasta format). Defaul is 'total.5.repset.fasta' Specify \code{NULL} to skip.
#' @param tree.file phylo tree file (newick file). Default is 'total.10.tree' Specify \code{NULL} to skip.
#' @return phyloseq object containing the specified UPARSE data.
#' @author Ying Taur
#' @export
read.uparse.data <- function(dirpath,
                             otu.file="total.6.otu-table.txt",
                             tax.file="total.5.repset.fasta.blastn.refseq_rna.txt",
                             repseq.file="total.5.repset.fasta",
                             tree.file="total.10.tree") {
  requireNamespace("phyloseq",quietly=TRUE)
  warning("YTWarning: Please note that this function is deprecated, uparse/mothur functions have been taken offsite.")
  # dirpath="uparse";otu.file="total.6.otu-table.txt";tax.file="total.5.repset.fasta.blastn.refseq_rna.txt";repseq.file="total.5.repset.fasta";tree.file="total.10.tree"
  if (!dir.exists(dirpath)) stop("YTError: This directory doesn't exist: ",dirpath)
  # dirpath="uparse"
  otu.file <- file.path(dirpath,otu.file)
  if (!file.exists(otu.file)) stop("YTError not found: ",otu.file)
  phy <- read.otu.table(otu.file) %>% set.otu()
  if (!is.null(tax.file)) {
    tax.file <- file.path(dirpath,tax.file)
    if (!file.exists(tax.file)) stop("YTError not found: ",tax.file)
    tax <- read.blastn.file(tax.file) %>% set.tax()
    phy <- merge_phyloseq(phy,tax)
  }
  if (!is.null(repseq.file)) {
    repseq.file <- file.path(dirpath,repseq.file)
    if (!file.exists(repseq.file)) stop("YTError not found: ",repseq.file)
    repseq <- phyloseq::import_qiime(refseqfilename=repseq.file)
    phy <- merge_phyloseq(phy,repseq)
  }
  if (!is.null(tree.file)) {
    tree.file <- file.path(dirpath,tree.file)
    if (!file.exists(tree.file)) stop("YTError not found: ",tree.file)
    tree <- read.tree.uparse(tree.file)
    phy <- merge_phyloseq(phy,tree)
  }
  return(phy)
}




#' Plot Principal Components Analysis
#'
#' Plots PCA from distance matrix data.
#'
#' @param dist distance matrix to be plotted.
#' @param data logical, if \code{TRUE}, returns a data frame of PCA axes instead of the plot. Default is \code{FALSE}.
#' @param prefix character, an optional prefix text for PCA variable names. E.g. if \code{"unifrac"} is used, \code{"PCA1"} becomes \code{"unifrac.PCA1"}.
#' @return Returns a \code{ggplot2} graph of PCA1 and PCA2.
#' @examples
#' @author Ying Taur
#' @export
pca.plot <- function(dist,data=FALSE,prefix=NA) {
  pca <- prcomp(dist)
  pca.axes <- data.frame(pca$x,stringsAsFactors=FALSE)
  pca.loadings <- summary(pca)$importance["Proportion of Variance",]
  pca.labels <- paste0(sub("PC","PCA",names(pca.loadings))," (",percent(pca.loadings)," variation explained)")
  for (i in 1:length(pca.labels)) {
    label(pca.axes[,i]) <- pca.labels[i]
  }
  pca.axes$sample <- row.names(pca.axes)
  if (data) {
    names(pca.axes) <- sub("^PC",paste2(prefix,"PCA",sep="."),names(pca.axes))
    return(pca.axes)
  } else {
    g <- ggplot(pca.axes) +
      geom_point(aes(x=PC1,y=PC2,color=sample,size=3)) +
      geom_text(aes(x=PC1,y=PC2,label=sample),size=3,vjust=1.4) +
      theme(aspect.ratio=1)
    return(g)
  }
}




#' LEfSe prep
#'
#' Create the initial file needed for LEfSe (LDA Effect Size) analysis.
#'
#' This function performs the analysis using the following steps.
#' (1) Creates lefse.txt from phyloseq data, a tab-delimited file in the format input required by LEfSe.
#' (2) Provides the command line for running the analysis, assuming you have the scripts locally.
#' @param phy the phyloseq object containing data
#' @param class variable to be tested by LEfSe. This must be a variable in sample_data(phy)
#' @param subclass variable to perform subclass testing. This step is skipped if it is not specified.
#' @param subject variable referring to the subject level designation. This is only necessary if multiple samples per subject.
#' @param anova.alpha alpha level of the kruskal-wallis testing. Default is 0.05
#' @param wilcoxon.alpha alpha level at which to perform wilcoxon testing of subclass testing. Default is 0.05.
#' @param lda.cutoff Cutoff LDA to be reported. Default is 2.0.
#' @param wilcoxon.within.subclass Set whether to perform Wilcox test only among subclasses with the same name (Default FALSE)
#' @param mult.test.correction Can be {0,1,2}. Set the multiple testing correction options. 0 no correction (more strict, default), 1 correction for independent comparisons, 2 correction for independent comparison
#' @param one.against.one for multiclass tasks, sets whether testing is performed one-against-one (TRUE - more strict) or one-against-all (FALSE - less strict)
#' @param levels Taxonomic levels to be tested. Default is to test all levels: rank_names(phy)
#' @return Returns data
#' @examples
#' lefse.tbl <- lefse(phy1,class="CDI",subclass="Sex")
#' @export
lefse.prep <- function(phy,class,subclass=NA,
                       subject=NA,
                       anova.alpha = 0.05,
                       wilcoxon.alpha = 0.05,
                       lda.cutoff = 2,
                       wilcoxon.within.subclass = FALSE,
                       one.against.one = FALSE,
                       n_boots = 30,
                       min_c = 10,
                       f_boots = 0.67,
                       mult.test.correction = 0,
                       by_otus=FALSE,
                       levels=phyloseq::rank_names(phy)) {
  requireNamespace(c("phyloseq","data.table"),quietly=TRUE)
  # pkgs <- c("splines","stats4","survival","mvtnorm","modeltools","coin","MASS")
  # missing.pkgs <- setdiff(pkgs,installed.packages()[,"Package"])
  # if (length(missing.pkgs)>0) {
  #   warning("YTWarning: R packages are needed for the LEFSE scripts to work: ",paste(missing.pkgs,collapse=", "))
  # }
  warning("YTWarning: Please note that this function is deprecated, and only does the formatting step.
Use this with Docker or Conda image, or consider using lda.effect.")

  keepvars <- c(class,subclass,subject,"sample")
  keepvars <- unique(keepvars[!is.na(keepvars)])
  samp <- get.samp(phy)[,keepvars]
  if (by_otus) { #perform by otu only
    otu <- get.otu.melt(phy,sample_data=FALSE)
    otu.levels <- otu %>% mutate(taxon=otu) %>%
      group_by(sample,taxon) %>% summarize(pctseqs=sum(pctseqs)) %>%
      mutate(taxon=gsub(" ","_",taxon))
  } else { #divide by taxonomy
    otu <- get.otu.melt(phy,sample_data=FALSE)
    otu.list <- lapply(1:length(levels),function(i) {
      lvls <- levels[1:i]
      lvl <- levels[i]
      otu.level <- otu
      otu.level$taxon <- do.call(paste,c(lapply(lvls,function(l) otu[[l]]),sep="|"))
      otu.level$rank <- lvl
      otu.level2 <- otu.level %>% group_by(sample,taxon,rank) %>% summarize(pctseqs=sum(pctseqs)) %>% ungroup()
      return(otu.level2)
    })
    otu.levels <- bind_rows(otu.list) %>%
      mutate(taxon=gsub(" ","_",taxon))
  }
  otu.tbl <- otu.levels %>%
    dcast(sample~taxon,value.var="pctseqs",fill=0) %>%
    left_join(samp,by="sample") %>%
    select_(.dots=c(keepvars,lazyeval::interp(~everything())))
  if (is.na(subject) | subject!="sample") {
    otu.tbl <- otu.tbl %>% select(-sample)
  }
  tbl <- otu.tbl %>% t()
  write.table(tbl,"lefse.txt",quote=FALSE,sep="\t",col.names=FALSE)

  opt.class <- paste("-c",which(keepvars %in% class))
  opt.subclass <- ifelse(is.na(subclass),"",paste("-s",which(keepvars %in% subclass)))
  opt.subject <-ifelse(is.na(subject),"",paste("-u",which(keepvars %in% subject)))
  format.command <- paste("format_input.py lefse.txt lefse.in",opt.class,opt.subclass,opt.subject,"-o 1000000")
  # system(format.command)
  #   -m {f,s}              set the policy to adopt with missin values: f removes
  #   the features with missing values, s removes samples
  #   with missing values (default f)
  #   -n int                set the minimum cardinality of each subclass
  #   (subclasses with low cardinalities will be grouped
  #   together, if the cardinality is still low, no pairwise
  #   comparison will be performed with them)

  lefse.command <- paste("run_lefse.py lefse.in lefse.res",
                         "-a",anova.alpha,
                         "-w",wilcoxon.alpha,
                         "-l",lda.cutoff,
                         "-e",as.numeric(wilcoxon.within.subclass),
                         "-y",as.numeric(one.against.one),
                         "-b",n_boots,
                         "--min_c",min_c,
                         "-f",f_boots,
                         "-s",mult.test.correction)
  message("Commands: ")
  message(format.command)
  message(lefse.command)
  message("plot_res.py lefse.res lefse_lda.png")
  message("plot_cladogram.py lefse.res lefse_clado.pdf --format pdf")
  return(list(format=format.command,
              lefse=lefse.command,
              plot1="plot_res.py lefse.res lefse_lda.png",
              plot2="plot_cladogram.py lefse.res lefse_clado.pdf --format pdf"))
}


# from yingtools.R --------------------------------------------------------



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
#' 1. `y` always keep the y-value (default).
#' 2. `x` always keep the x-value.
#' 3. `y.coalesce` keep the y-value unless it is `NA`.
#' 4. `x.coalesce` keep the x-value unless it is `NA`.
#' 5. `error` throw error if there is a conflict.
#'
#' @export
#' @examples
#' tbl1 <- tibble(id=1:10) %>% mutate(source="table1")
#' tbl2 <- tibble(id=5:15) %>% mutate(source="table2")
#' full_join(tbl1,tbl2,by="id")
#' full_join_replace(tbl1,tbl2,by="id") %>% arrange(id)
inner_join_replace_old <- function(x,y,by=NULL, conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {

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


#' @rdname inner_join_replace_old
#' @export
left_join_replace_old <- function(x,y,by=NULL,conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {
  data1 <- inner_join_replace(x,y,by=by,conflict=arg_match(conflict))
  data2 <- anti_join(x,y,by=by)
  bind_rows(data1,data2)
}

#' @rdname inner_join_replace_old
#' @export
right_join_replace_old <- function(x,y,by=NULL,conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {

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


#' @rdname inner_join_replace_old
#' @export
full_join_replace_old <- function(x,y,by=NULL,conflict=c("yonly","xonly","ycoalesce","xcoalesce","error")) {
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
  data2 <- anti_join(x,y,by=by)
  data3 <- anti_join(y,x,by=flip(by))
  bind_rows(data1,data2,data3)
}






copy.as.sql.old <- function(x,copy.clipboard=TRUE,fit=TRUE,width=getOption("width")-15) {
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

#' Sample N Groups
#'
#' Sample groups from a grouped data frame
#' @param grouped_df the grouped data frame to be sampled
#' @param size number of groups to sample
#' @return a subset of the grouped data frame
#' @examples
#' gdf <- mtcars %>% group_by(gear,carb)
#' sample_n_groups(gdf,3)
sample_n_groups_OLD <- function(grouped_df, size) {
  dplyr::group_data(grouped_df) %>%
    dplyr::sample_n(size) %>%
    dplyr::select(-.rows) %>%
    dplyr::inner_join(grouped_df,by=dplyr::group_vars(grouped_df)) %>%
    dplyr::group_by(!!!groups(grouped_df))
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


#not sure I need this
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
  select_var_names <- sapply(select_vars,as_name)
  select_vars_keep <- select_vars[select_var_names %in% names(data)]
  data %>% select(!!!select_vars_keep)
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

  if (quo_is_null(starttime)) {
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


#' Determines if tstart-tstop occurs anywhere within interval.
#' @export
occurs.within <- function(tstart,tstop,start.interval,stop.interval) {
  message("YTNote: occurs.within() was renamed to overlaps()")
  tstop>=start.interval & stop.interval>=tstart
}






