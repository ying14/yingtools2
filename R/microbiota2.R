# need this for get.otu.melt to work in package.
# This tells data.table that you as a package developer have designed your code to intentionally
# rely on data.table functionality even though it may not be obvious from inspecting your NAMESPACE file.
.datatable.aware = TRUE

# phyloseq manipulation ---------------------------------------------------




#' Extract Phyloseq sample_data
#'
#' Returns [sample_data][phyloseq::sample_data-class] component from phyloseq object, as a data frame.
#'
#' This basically is similar to the function [phyloseq::sample_data()], but does a few extra things.
#' 1. Converts to a data frame
#' 2. The sample name is stored in a column called `sample`. ([`phyloseq`][`phyloseq::phyloseq-class`] normally stores as a row name)
#' 3. Calculates number of sequences and alpha diversity metrics, if desired.
#'
#' This function is the opposite of [set.samp()], which converts the data frame back into a [sample_data][phyloseq::sample_data-class].
#' Note that if the [`phyloseq`][`phyloseq::phyloseq-class`] object does not contain [sample_data][phyloseq::sample_data-class], a data frame containing a single column, `sample`, is returned.
#' @param phy phyloseq object containing [sample_data][phyloseq::sample_data-class]
#' @param stats logical, whether or not to include summary statistics of samples. Stores `nseqs`, and diversity metrics.
#' @param measures diversity measures to calculate, if stats is `TRUE.` Default: `c("Observed","InvSimpson","Shannon")`
#' @return Data frame containing [sample_data][phyloseq::sample_data-class] data.
#' @examples
#' get.samp(cid.phy)
#' @export
get.samp <- function(phy,stats=FALSE,measures=c("Observed","InvSimpson","Shannon")) {
  requireNamespace("phyloseq",quietly=TRUE)
  if (is.null(sample_data(phy,FALSE))) {
    #if no sample_data, return single data frame with sample column
    sdata <- tibble(sample=phyloseq::sample_names(phy))
  } else {
    if ("sample" %in% phyloseq::sample_variables(phy)) {stop("YTError: phyloseq sample_data already contains the reserved variable name \"sample\"")}
    sdata <- phyloseq::sample_data(phy) %>% data.frame(stringsAsFactors=FALSE) %>% rownames_to_column("sample") %>% as_tibble()
  }

  sdata.newcols <- tibble(nseqs=unname(phyloseq::sample_sums(phy)))
  if (stats) {
    stat.data <- phyloseq::estimate_richness(phy,measures=measures) %>% as_tibble()
    sdata.newcols <- cbind(sdata.newcols,stat.data)
  }
  names.exist <- intersect(names(sdata.newcols),names(sdata))
  values.all.equal <- map_lgl(names.exist,~{
    all(sdata.newcols[[.x]]==sdata[[.x]],na.rm=TRUE)
  })
  names.exist.and.different <- names.exist[!values.all.equal]
  if (length(names.exist.and.different)>0) {
    warning("YTWarning: sample data contains columns which will be overwritten with values that look different: ",paste(names.exist.and.different,collapse=", "))
  }
  sdata <- sdata %>% select(-all_of(names.exist)) %>% cbind(sdata.newcols)
  return(sdata)
}


#' Convert data frame to phyloseq sample_data
#'
#' Use this on data with sample info. The opposite of function get.samp. Make sure it contains variable "sample"
#' @param sdata Data frame to be converted back to [sample_data][phyloseq::sample_data-class]
#' @return formatted [sample_data][phyloseq::sample_data-class].
#' @export
set.samp <- function(sdata) {
  requireNamespace(c("phyloseq"),quietly=TRUE)
  ss <- sdata %>% column_to_rownames("sample") %>%
    data.frame(stringsAsFactors=FALSE) %>% phyloseq::sample_data()
  return(ss)
}

#' Extract Phyloseq tax_table
#'
#' Creates data.frame from [tax_table][phyloseq::taxonomyTable-class], storing the rownames as variable "otu". The opposite of set.tax function.
#'
#' @param phy phyloseq object containing tax_data
#' @return Dataframe containing tax data
#' @export
get.tax <- function(phy) {
  requireNamespace(c("phyloseq"),quietly=TRUE)
  phyloseq::tax_table(phy) %>% data.frame(stringsAsFactors=FALSE) %>% rownames_to_column("otu") %>% as_tibble()
}


#' Convert data frame to phyloseq tax_table
#'
#' Use this on data.frames with tax data. The opposite of get.tax function. Make sure it contains the variable "otu".
#' @param tdata dataframe to be converted back to [tax_table][phyloseq::taxonomyTable-class].
#' @return formatted [tax_table][phyloseq::taxonomyTable-class].
#' @export
set.tax <- function(tdata) {
  requireNamespace(c("phyloseq"),quietly=TRUE)
  tt <- tdata %>% column_to_rownames("otu") %>%
    as.matrix() %>% phyloseq::tax_table()
  return(tt)
}



#' Extract Phyloseq otu_table
#'
#' Creates data.frame from otu_table, storing the rownames as variable "otu". The opposite of set.tax function.
#'
#' @param phy phyloseq object containing otu_data
#' @param as.matrix if `TRUE`, return matrix (instead of data frame with otu as column)
#' @return Dataframe containing otu table
#' @export
get.otu <- function(phy,as.matrix=TRUE) {
  requireNamespace("phyloseq",quietly=TRUE)

  if (phyloseq::taxa_are_rows(phy)) {
    otu <- phyloseq::otu_table(phy) %>% as("matrix")
  } else {
    otu <- phyloseq::otu_table(phy) %>% t() %>% as("matrix")
  }
  if (as.matrix) {
    return(otu)
  } else {
    # otu.df <- otu %>% data.frame(stringsAsFactors=FALSE,check.names = FALSE) %>% rownames_to_column("otu") %>% as_tibble()
    otu.df <- otu %>% as_tibble(rownames="otu")
    return(otu.df)
  }
}


#' Convert OTU table to phyloseq otu_table
#'
#' Use this on data.frames with tax data. The opposite of get.tax function. Make sure it contains the variable "otu".
#' @param odata otu table (matrix or dataframe with 'otu' column) to be converted back to otu_table.
#' @return formatted [tax_table][phyloseq::taxonomyTable-class].
#' @export
set.otu <- function(odata,taxa_are_rows=TRUE) {
  requireNamespace("phyloseq",quietly=TRUE)
  if (is.data.frame(odata)) {
    if (!("otu" %in% colnames(odata))) {
      stop("YTError: got a data frame without 'otu' as a column!")
    }
    odata <- odata %>% column_to_rownames("otu") %>% as.matrix()
  }
  odata %>% phyloseq::otu_table(taxa_are_rows=taxa_are_rows)
}



#' Convert Phyloseq to Melted OTU x Sample Data
#'
#' Creates OTU+Sample-level data, using phyloseq object (ID=otu+sample)
#'
#' Essentially gives back the OTU table, in melted form, such that each row represents a certain OTU for a certain sample.
#' Adds sample and taxonomy table data as columns. Uses the following reserved varnames: otu, sample, numseqs, pctseqs.
#' Note that phyloseq has a similar function, [phyloseq::psmelt()], but that takes longer.
#' The `get.otu.melt()` now works by performing operations via data table, making it about 30x faster than before.
#'
#' @param phy phyloseq object containing sample data
#' @param filter.zero Logical, whether or not to remove zero abundances. Default `TRUE`.
#' @param tax_data Logical, whether or not to join with `tax_data`. Default `TRUE`.
#' @param sample_data Logical, whether or not to join with [sample_data][phyloseq::sample_data-class]. Default `TRUE`.
#'
#' @return Data frame melted OTU data
#' @export
#' @examples
#' library(phyloseq)
#' get.otu.melt(cid.phy)
get.otu.melt <- function(phy,filter.zero=TRUE,sample_data=TRUE,tax_data=TRUE) {
  requireNamespace(c("phyloseq","data.table"),quietly=TRUE)
  # supports "naked" otu_table as `phy` input.
  otutab = as(phyloseq::otu_table(phy), "matrix")
  if (!phyloseq::taxa_are_rows(phy)) {
    otutab <- t(otutab)
  }
  if (filter.zero) {
    otutab[otutab==0] <- NA_real_
  }
  otudt = data.table::data.table(otutab, keep.rownames = TRUE)
  data.table::setnames(otudt, "rn", "otu")
  # Enforce character otu key
  # note that .datatable.aware = TRUE needs to be set for this to work well.
  otudt[, otuchar:=as.character(otu)]
  otudt[, otu := NULL]
  data.table::setnames(otudt, "otuchar", "otu")
  # Melt count table
  mdt = data.table::melt.data.table(otudt, id.vars = "otu", variable.name = "sample", variable.factor=FALSE, value.name = "numseqs", na.rm = filter.zero)
  # if (filter.zero) {
  #   # Remove zeroes, NAs
  #   mdt <- mdt[numseqs > 0][!is.na(numseqs)]
  # } else {
  #   mdt <- mdt[!is.na(numseqs)]
  # }
  # Calculate relative abundance
  mdt[, pctseqs := numseqs / sum(numseqs), by = sample]
  if(tax_data & !is.null(phyloseq::tax_table(phy, errorIfNULL=FALSE))) {
    # If there is a tax_table, join with it. Otherwise, skip this join.
    taxdt = data.table::data.table(as(phyloseq::tax_table(phy, errorIfNULL = TRUE), "matrix"), keep.rownames = TRUE)
    data.table::setnames(taxdt, "rn", "otu")
    # Enforce character otu key
    taxdt[, otuchar := as.character(otu)]
    taxdt[, otu := NULL]
    data.table::setnames(taxdt, "otuchar", "otu")
    # Join with tax table
    data.table::setkey(taxdt, "otu")
    data.table::setkey(mdt, "otu")
    mdt <- taxdt[mdt]
  }
  if (sample_data & !is.null(phyloseq::sample_data(phy, errorIfNULL = FALSE))) {
    # If there is a sample_data, join with it.
    sampledt = data.table::data.table(as(phyloseq::sample_data(phy, errorIfNULL = TRUE), "data.frame"),keep.rownames=TRUE)
    data.table::setnames(sampledt, "rn", "sample")
    # Enforce character sample key
    sampledt[, samplechar := as.character(sample)]
    sampledt[, sample := NULL]
    data.table::setnames(sampledt, "samplechar", "sample")
    # Join with tax table
    data.table::setkey(sampledt, "sample")
    data.table::setkey(mdt, "sample")
    mdt <- sampledt[mdt]
  }
  mdt <- mdt %>% as_tibble() %>% select(sample,otu,everything())
  return(mdt)
}



#' Convert melted OTU table to phyloseq object
#'
#' @param otu.melt table of taxa x sample abundances, similar to output of `get.otu.melt`
#' @param sample_id sample ID variable. Default `"sample"`.
#' @param taxa_id taxa/OTU ID variable. Default `"otu"`.
#' @param abundance_var abundance variable, used to fill [phyloseq::otu_table()]. Default `"numseqs"`.
#' @param taxranks vector of taxonomic ranks to be included in [tax_table][phyloseq::taxonomyTable-class].
#' @param sample_vars whether to include sample variables in the data. Can be `TRUE` (include sample vars, and try to determine which ones), `FALSE` no sample vars, or a character vector specifying col names to be included.
#' @return phyloseq object, generated from the `otu.melt` data.
#' @export
#' @examples
#' library(phyloseq)
#' phy <- cid.phy
#' ranks <- rank_names(phy)
#' otu <- get.otu.melt(cid.phy)
#' phy2 <- get.phyloseq.from.melt(otu,taxranks=ranks)
#' phy
#' phy2
get.phyloseq.from.melt <- function(otu.melt,
                                   taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                                   sample_vars=TRUE,
                                   sample_id="sample",abundance_var="numseqs",taxa_id="otu") {
  # declare.args(otu.melt=get.otu.melt(cid.phy), taxranks=rank_names(cid.phy), get.phyloseq.from.melt)
  requireNamespace("phyloseq",quietly=TRUE)
  rows.are.distinct <- is.distinct(otu.melt, !!sym(taxa_id), !!sym(sample_id))
  if (!rows.are.distinct) {
    stop(str_glue("YTError: rows are not distinct across (sample_id x taxa_id)!"))
  }
  otu <- otu.melt %>%
    transmute(otu=as.character(!!sym(taxa_id)),
              sample=as.character(!!sym(sample_id)),
              numseqs=!!sym(abundance_var)) %>%
    pivot_wider(id_cols=otu,names_from=sample,values_from=numseqs,values_fill=0)
  tax <- otu.melt %>% select(otu=!!sym(taxa_id),!!!syms(taxranks)) %>% distinct()

  if (isTRUE(sample_vars)) { #logical and true
    # determine sample  vars
    message("Attempting to determine sample vars...\n")
    vars.to.check <-setdiff(names(otu.melt),c(taxa_id,taxranks,abundance_var))
    distinct_sample_vars <- otu.melt %>%
      test_if_nonvarying_by_group(id_vars=all_of(sample_id),test_vars=all_of(vars.to.check)) %>%
      {names(.)[.]} %>% setdiff(sample_id)
    sample_vars <- distinct_sample_vars
  } else if (isFALSE(sample_vars))  {
    #no sample variables
    sample_vars <- c()
  } else if (is.character(sample_vars)) {
    # sample_vars are specified do nothing
    sample_vars <- setdiff(sample_vars,sample_id)
  } else {
    stop("YTError: sample_vars should be a character or logical!")
  }

  if (anyDuplicated(tax$otu)!=0) {stop("YTError: taxranks are not distinct over taxa_id!")}
  phy <- phyloseq::phyloseq(set.otu(otu),set.tax(tax))
  if (length(sample_vars)>0) {
    samp <- otu.melt %>% select(sample=!!sym(sample_id),!!!syms(sample_vars)) %>% distinct()
    if (anyDuplicated(samp$sample)!=0) {stop("YTError: sample vars are not distinct over sample!")}
    phyloseq::sample_data(phy) <- samp %>% set.samp()
  }

  leftover.vars <- setdiff(names(otu.melt),c(abundance_var,sample_id,taxa_id,taxranks,sample_vars))
  message(str_glue("sample vars: [{sample_id}]; {paste(sample_vars,collapse=\", \")}"))
  message(str_glue("tax vars: [{taxa_id}]; {paste(taxranks,collapse=\", \")}"))
  message(str_glue("abundance var: [{abundance_var}]"))
  if (length(leftover.vars)>0) {
    message(str_glue("(vars not used: {paste(leftover.vars,collapse=\", \")})"))
  }
  return(phy)
}



#' Calculate Abundance from Phyloseq
#'
#' Given a [`phyloseq`][`phyloseq::phyloseq-class`], calculate relative abundance for each sample.
#' @param phy [`phyloseq`][`phyloseq::phyloseq-class`] object to be analyzed
#' @param ... one or more taxonomic expressions defining the abundance to be calculated. Can be named, in order to specify the column name.
#' @param counts if `TRUE`, will return count rather than relative abundance
#' @return a data frame containing sample identifier and abundance columns
#'
#' @examples
#' library(phyloseq)
#' get.abundance(cid.phy,pct.entero=Genus %in% "Enterococcus",pct.proteo=Phylum %in% "Proteobacteria")
#' @export
get.abundance <- function(phy,..., counts=FALSE) {
  requireNamespace("phyloseq",quietly=TRUE)

  vars <- quos(...)
  varnames <- names(vars)
  novarname <- varnames==""
  if (any(novarname)) {
    default.name <- make.names(rep("pctseqs",sum(novarname)),unique=TRUE)
    varnames[novarname] <- default.name
  }
  t <- get.tax(phy)
  otu <- otu_table(phy,taxa_are_rows=FALSE)
  seq.total <- sample_sums(phy)
  n.zero.samps <- sum(seq.total==0)
  if (n.zero.samps>0) {
    warning("YTWarning: ",n.zero.samps," samples have zero sequences")
  }
  s <- tibble(sample=sample_names(phy))
  for (i in 1:length(vars)) {
    var <- vars[[i]]
    varname <- varnames[i]
    select.otus <- t %>% mutate(.criteria=!!var) %>% pull(.criteria)
    if (all(!select.otus)) {
      warning("YTWarning: no taxa meeting this criteria: ",as_label(var))
      tax.pctseqs <- 0
    } else {
      tax.sums <- otu[select.otus,,drop=FALSE] %>% apply(2,sum)
      if (counts) {
        tax.pctseqs <- tax.sums
      } else {
        tax.pctseqs <- tax.sums/seq.total
      }
    }
    s <- s %>% mutate(!!varname:=unname(tax.pctseqs))
  }
  message("Created column(s): ",paste(varnames,collapse=", "))
  return(s)
}


#' Add abundance to sample data
#'
#'
#' @param sdata sample data to be modified
#' @param ... one or more taxonomic expressions defining the abundance to be calculated. Can be named, in order to specify the column name.
#' @param phy [`phyloseq`][`phyloseq::phyloseq-class`] object to be used for caluclation
#' @param counts if `TRUE`, will return count rather than relative abundance
#'
#' @return returns `sdata`, with additional columns for abundance.
#'
#' @examples
#' library(phyloseq)
#' get.samp(cid.phy) %>%
#'   add.abundance(pct.entero=Genus=="Enterococcus",
#'                 pct.proteo=Phylum=="Proteobacteria",
#'                 phy=cid.phy)
#' @export
add.abundance <- function(sdata, ... ,phy,counts=FALSE) {
  requireNamespace("phyloseq",quietly=TRUE)
  s <- sdata %>% pull(sample) %>% prune_samples(phy) %>%
    prune_taxa(taxa_sums(.)>0,.) %>%
    get.abundance(...,counts=counts)
  missing.samps <- setdiff(sdata$sample,s$sample)
  if (length(missing.samps)>0) {
    stop("YTError: ",length(missing.samps)," samples are missing from phyloseq object")
  }
  sdata %>% left_join(s,by="sample")
}


#' Check if taxonomy levels are distinct.
#'
#' @param data tax data to be tested. Can be a [`phyloseq`][`phyloseq::phyloseq-class`], tax table (from [get.tax()]), or otu-melt table (from [get.otu.melt()]).
#' @param taxranks character vector of tax levels to be tested.
#'
#' @return logical indicating whether or not taxonomy levels are distinct.
#' @export
#'
#' @examples
taxonomy_is_distinct <- function(data,taxranks=c("Superkingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
  if (length(taxranks)<2) {
    stop("YTError: taxranks should be at least length 2")
  }
  if (is(data,"phyloseq")) {
    data <- get.tax(data)
  }
  data <- data %>% select(!!!syms(taxranks)) %>% distinct()

  full.taxonomy <- map_dfr(1:length(taxranks),~{
    lvl <- taxranks[.x]
    all.lvl <- taxranks[1:.x]
    data %>% select(!!!syms(all.lvl)) %>%
      distinct() %>%
      transmute(taxonomy=paste(!!!syms(all.lvl),sep="|"),
                taxon=!!sym(lvl))
  },.id="rank")
  taxa.are.distinct <- anyDuplicated(full.taxonomy$taxon)==0
  return(taxa.are.distinct)
}



#' @rdname make_taxonomy_distinct
#' @export
make_taxonomy_distinct <- function(x,...) UseMethod("make_taxonomy_distinct")

#' Make taxonomy distinct
#'
#' Modifies (if necessary) the data frame or [tax_table][phyloseq::taxonomyTable-class] of [`phyloseq`][`phyloseq::phyloseq-class`] object such that each rank level is a distinct identifier.
#'
#' Sometimes there are two taxonomy naming issues that can cause issues or confusion:
#'
#' 1. Two or more distinct taxonomies can have the same duplicate names at the lowest level, e.g. Genus=Ileibacterium can
#' either have Family=`"Erysipelotrichaceae" `or Family=`"Clostridiales Family XIII. Incertae Sedis"`.
#'
#' 2. The same name is used for 2 different levels, e.g. `"Actinobacteria"` is both a Phylum and a Class
#'
#' This will handle by adding `"#1", "#2", ...` to the name in the event of #1, and adds rank for #2.
#' @param phy [`phyloseq`][`phyloseq::phyloseq-class`] object
#'
#' @return modified data frame or [`phyloseq`][`phyloseq::phyloseq-class`] object with corrected names.
#' @examples
#' @rdname make_taxonomy_distinct
#' @export
#' d.phy <- make_taxonomy_distinct(cid.phy)
#' get.tax(d.phy)
make_taxonomy_distinct.phyloseq <- function(phy,add.rank=FALSE) {
  tax <- get.tax(phy)
  ranks <- rank_names(phy)
  tax <- make_taxonomy_distinct.data.frame(tax,taxranks=ranks,add.rank=add.rank)
  tax_table(phy) <- tax %>% set.tax()
  return(phy)
}


#' @param data data to be modified
#' @param taxranks vector of column names to be checked and modified.
#' @param add.rank logical, whether to add rank to taxon names: e.g. `Enterococcus` would be renamed to `Enterococcus (Genus)`. Default is `FALSE`
#'
#' @rdname make_taxonomy_distinct
#' @export
make_taxonomy_distinct.data.frame <- function(data,taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                                              add.rank=FALSE) {
  for (level in seq_along(taxranks)[-1]) {
    taxlevel <- taxranks[level]
    parentlevels <- taxranks[1:level-1]
    data <- data %>%
      group_by(!!sym(taxlevel)) %>%
      mutate(
        parentlevels=paste(!!!syms(parentlevels),sep="|"),
        parent.rank=dense_rank(parentlevels),
        parents.ndistinct=max(parent.rank),
        !!taxlevel:=ifelse(parents.ndistinct>1,
                           paste0(!!sym(taxlevel)," #",parent.rank),
                           !!sym(taxlevel))) %>%
      select(-parentlevels,-parent.rank,-parents.ndistinct) %>%
      ungroup()
    if (add.rank) {
      data <- data %>%
        mutate(!!taxlevel:=paste0(!!sym(taxlevel)," (",taxlevel,")"))
    }
  }
  return(data)
}






#' Collapse Phyloseq Into Taxonomy
#'
#' In a [`phyloseq`][`phyloseq::phyloseq-class`] object, combine OTUs of the same taxonomic classification.
#'
#' Similar to [phyloseq::tax_glom()], but with the following differences:
#' 1. it performs much faster,
#' 2. requires all tax levels to be specified (instead of assuming all ranks to the left of the tax-level)
#' 3. the new OTU names will specify old OTU names separated by `'|'`
#'
#' @param phy A phylsoeq object.
#' @param taxranks tax levels to collapse by. Default is `c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")`.
#' @param short_taxa_names How to name the collapsed OTUs. If `TRUE`, use name of first OTU plus number of OTUs being collapsed. If `FALSE`, paste the OTU names together.
#' @return A [`phyloseq`][`phyloseq::phyloseq-class`] object with OTUs collapsed.
#' @export
#' @examples
#' library(phyloseq)
#' cid.phy.family <- phy.collapse(cid.phy,taxranks=c("Kingdom", "Phylum", "Class", "Order", "Family"))
#' cid.phy
#' cid.phy.family
phy.collapse <- function(phy,taxranks=rank_names(phy),short_taxa_names=TRUE) {
  # declare.args(phy=cid.phy,phy.collapse)

  # phy=phy1;taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")
  requireNamespace(c("phyloseq","data.table"),quietly=TRUE)
  otudt <- phyloseq::otu_table(phy) %>% as("matrix") %>% data.table::data.table()
  taxdt = as(phyloseq::tax_table(phy,errorIfNULL=TRUE),"matrix") %>% data.table::data.table() %>% .[,taxranks,with=FALSE]
  # indices_ <- taxdt %>% group_by(!!!taxranks) %>% group_indices()
  indices_ <- taxdt[, .group:=.GRP, by=taxranks]$.group

  new.otudt <- otudt[,lapply(.SD,sum),by=indices_]
  new.taxdt <- taxdt[,lapply(.SD,first),by=indices_]
  otu.names <- data.table::data.table(otu=taxa_names(phy))
  otu.names <- otu.names[,lapply(.SD,function(x) {
    # x <- x[order(as.numeric(str_extract(x,"[0-9]+")))]
    if (short_taxa_names) {
      paste0(x[1],"_ntaxa=",length(x))
    } else {
      paste(x,collapse="|")
    }
  }),by=indices_] %>% pull(otu)
  otu.rep <- data.table::data.table(otu=taxa_names(phy))
  otu.rep <- otu.rep[,lapply(.SD,function(x) {
    rep <- x[order(as.numeric(str_extract(x,"[0-9]+")))[1]]
    rep
  }),by=indices_] %>% .[["otu"]]
  new.otudt <- new.otudt[,"indices_":=NULL] %>% as.matrix()
  new.taxdt <- new.taxdt[,"indices_":=NULL] %>% as.matrix()
  row.names(new.otudt) <- otu.names
  row.names(new.taxdt) <- otu.names

  new.otu <- new.otudt %>% phyloseq::otu_table(taxa_are_rows=TRUE)
  new.tax <- new.taxdt %>% phyloseq::tax_table()
  samp <- phyloseq::sample_data(phy,errorIfNULL=FALSE)
  tree <- phyloseq::phy_tree(phy,errorIfNULL=FALSE)
  if (!is.null(tree)) {
    tree <- phyloseq::prune_taxa(otu.rep,tree)
    taxa_names(tree) <- unname(setNames(otu.names,otu.rep)[taxa_names(tree)])
  }
  seqs <- phyloseq::refseq(phy,errorIfNULL=FALSE)
  if (!is.null(seqs)) {
    seqs <- phyloseq::prune_taxa(otu.rep,seqs)
    taxa_names(seqs) <- unname(setNames(otu.names,otu.rep)[taxa_names(seqs)])
  }
  new.phy <- phyloseq::merge_phyloseq(new.otu,new.tax,samp,tree,seqs)
  return(new.phy)
}






phy.collapse.base <- function(otudt,taxdt,taxranks,level,criteria,fillin.levels) {
  # declare.args(    otudt=get.otu.melt(cid.phy,sample_data=FALSE,tax_data=FALSE) %>% as.data.table(),    taxdt=get.tax(cid.phy) %>% as.data.table(),    taxranks=rank_names(cid.phy),    criteria=quo(max.pctseqs<=0.001 | pct.detectable<=0.005),    level=7,    fillin.levels=FALSE,    yingtools2:::phy.collapse.base)
  # declare.args(    otudt=get.otu.melt(cid.phy,sample_data=FALSE,tax_data=FALSE) %>% as.data.table(),    taxdt=get.tax(cid.phy) %>% as.data.table(),    taxranks=rank_names(cid.phy),    criteria=quo(Genus=="Enterococcus"),    level=7,    fillin.levels=FALSE,    yingtools2:::phy.collapse.base)

  requireNamespace("data.table",quietly=TRUE)
  criteria <- enquo(criteria)
  otudt <- data.table::as.data.table(otudt)  #make sure it's data.table
  taxdt <- data.table::as.data.table(taxdt)
  nsamps <- data.table::uniqueN(otudt$sample)
  allranks <- c(taxranks,"strain")
  subscript <- function(x,i) {
    paste(x,i,sep="_")
  }
  taxdt <- taxdt[,strain:=otu]
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
  # some calcs depend on other lines, determine the dependencies
  depends <- allcalcs %>% imap(~all.vars(.x) %>% intersect(names(allcalcs)) %>% c(.y))
  calcvars <- depends[all.vars(criteria)] %>% unname() %>% simplify()
  # subset of allcalc that is needed
  calcs <- allcalcs[names(allcalcs) %in% calcvars]


  make.tax <- function(ss,i) {
    by1 <- allranks
    by2 <- subscript(allranks,"new")
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
    # add this to make current.level available in calculations,
    # but also to add a placeholder if calcs is empty list up until now.
    crank <- length(allranks)+1-i
    calcs <- c(calcs,exprs(current.level=crank))
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
  make.otu <- function(ss,tt,i) {
    by1 <- allranks
    by2 <- subscript(allranks,"new")
    ss %>%
      merge(tt,by=by1) %>%
      .[, .(pctseqs=sum(pctseqs),numseqs=sum(numseqs)), by=c("sample",by2)]
  }
  # each iteration checks criteria and collapses one level
  ss <- taxdt %>% merge(otudt,by="otu")
  taxmap.raw <- taxdt %>% data.table::setnames(old=allranks,new=subscript(allranks,1))
  trace <- c()
  for (i in 1:level) {
    # i=1
    tt <- make.tax(ss,i)
    ss <- make.otu(ss,tt,i)
    by1 <- subscript(allranks,i)
    by2 <- subscript(allranks,i+1)
    by.new <- subscript(allranks,"new")
    taxmap.raw <- tt %>% data.table::setnames(old=allranks,new=by1) %>% data.table::setnames(old=by.new,new=by2) %>%
      data.table::merge.data.table(taxmap.raw,all.y=TRUE, by=by1)
    trace <- c(trace,nrow(tt))
    ss <- ss %>% data.table::setnames(old=by.new,new=allranks)
  }
  #rename back to normal
  by.tax <- subscript(allranks,i+1) %>% setNames(allranks) %>% map(~expr(!!sym(.x)))
  taxmap <- inject(taxmap.raw[, .(otu,!!!by.tax)])

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
  new.tax <- new.tax.otu[, c("otu",taxranks), with=FALSE] %>% unique()
  new.otu <- new.tax.otu[, c("otu","sample","numseqs","pctseqs"), with=FALSE]
  trace <- c(trace,nrow(new.tax)) %>% setNames(rev(allranks)[1:(level+1)])
  message(str_glue("Evaluated across levels: {paste(names(trace),collapse=', ')} ({length(trace)-1} rounds)"))
  message(str_glue("Number of taxa: {paste(trace,collapse=' -> ')} (final number of taxa)"))
  list(tax=new.tax,otu=new.otu)
}


#' @rdname phy.collapse.bins
#' @export
phy.collapse.bins <- function(x,...) UseMethod("phy.collapse.bins")


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
#'   * `n.rows` total number of rows for a taxon
#'
#'   * `current.level` current level being evaluated
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
#' @rdname phy.collapse.bins
#' @export
phy.collapse.bins.phyloseq <- function(phy,
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
  objset <- phy.collapse.base(otudt=otudt,taxdt=taxdt,taxranks=taxranks,level=level,criteria=!!criteria,fillin.levels=fillin.levels)
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
#' @rdname phy.collapse.bins
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
phy.collapse.bins.data.frame <- function(data,
                                         taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                                         level=length(taxranks),
                                         fillin.levels=FALSE,
                                         sample_id=sample,
                                         taxa_id=otu,
                                         abundance_var=numseqs,
                                         criteria=max.pctseqs<=0.001 | pct.detectable<=0.005,
                                         sample_vars=TRUE) {
  requireNamespace("data.table",quietly=TRUE)
  # declare.args(data=get.otu.melt(cid.phy), taxranks <- rank_names(cid.phy), criteria=quo(max.pctseqs<=0.001 | pct.detectable<=0.005), yingtools2:::phy.collapse.bins.data.frame)
  criteria <- enquo(criteria)
  sample_id <- ensym(sample_id)
  taxa_id <- ensym(taxa_id)
  abundance_var <- ensym(abundance_var)

  needvars <- c(taxa_id, sample_id, abundance_var, taxranks)
  if (!all(needvars %in% names(data))) {
    stop(str_glue("YTError: vars not found in data: {paste(setdiff(needvars,names(data)),collapse=',')}"))
  }
  data <- data %>% rename(sample=!!sample_id,otu=!!taxa_id,numseqs=!!abundance_var)
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
  # otudt <- data %>% select(otu,sample,numseqs,pctseqs) %>% data.table::as.data.table()
  otudt <- data %>% select(otu, sample, numseqs) %>%
    group_by(sample) %>%
    mutate(pctseqs=numseqs/sum(numseqs)) %>%
    ungroup() %>%
    data.table::as.data.table()
  # sampdt <- samp %>% data.table::as.data.table()
  objset <- phy.collapse.base(otudt=otudt,taxdt=taxdt,taxranks=taxranks,level=level,criteria=!!criteria,fillin.levels=fillin.levels)
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
    rename(!!sample_id:=sample,!!taxa_id:=otu,!!abundance_var:=numseqs)
  # new.data <- new.otu %>% left_join(new.tax,by="otu") %>% left_join(samp,by="sample") %>% rename(!!sym(taxa_id):=otu,!!sym(sample_id):=sample)
  return(new.data)
}





#' Prune Unused Taxa from a phyloseq object
#'
#' In a [`phyloseq`][`phyloseq::phyloseq-class`] object, remove any taxa that are not used in the samples.
#' Consider using this after subsetting the samples.
#' @param phy [`phyloseq`][`phyloseq::phyloseq-class`] object
#' @return a [`phyloseq`][`phyloseq::phyloseq-class`] object, with empty taxa removed.
#' @export
#' @examples
#' library(phyloseq)
#' physub <- cid.phy %>%
#'   subset_samples(Patient_ID=="301")
#' physub.clean <- prune_unused_taxa(physub)
#' physub
#' physub.clean
prune_unused_taxa <- function(phy,verbose=TRUE) {
  requireNamespace("phyloseq",quietly=TRUE)
  keep <- taxa_sums(phy)>0
  if (verbose) {
    message(str_glue("Prune unused taxa: {length(keep)} to {sum(keep)} taxa"))
  }
  prune_taxa(keep,phy)
}








# stacked taxplot functions -----------------------------------------------



#' YT Palette 2
#'
#' The old customary palette for Bacteria.
#'
#'
#' @export
yt.palette2 <- exprs(
  "Bacteroidetes (phylum)"=Phylum == "Bacteroidetes" ~ shades("#51AB9B", variation = 0.25),
  "Lachnospiraceae (family)"=Family == "Lachnospiraceae" ~ shades("#EC9B96", variation = 0.25),
  "Ruminococcaceae (family)"=Family == "Ruminococcaceae" ~ shades("#9AAE73", variation = 0.25),
  "Clostridiales (order)"=Order == "Clostridiales" ~ shades("#9C854E", variation = 0.25),
  "Actinobacteria (phylum)"=Phylum == "Actinobacteria" ~ shades("#A77097", variation = 0.25),
  "Enterococcus (genus)"=Genus == "Enterococcus" ~ "#129246",
  "Streptococcus (genus)"=Genus == "Streptococcus" ~ "#9FB846",
  "Staphylococcus (genus)"=Genus == "Staphylococcus" ~ "#f1eb25",
  "Lactobacillus (genus)"=Genus == "Lactobacillus" ~ "#3b51a3",
  "Proteobacteria (phylum)"=Phylum == "Proteobacteria" ~ shades("red", variation = 0.4),
  "Other Bacteria"=TRUE ~ shades("gray", variation = 0.25)
)



#' YT Palette 3
#'
#' The customary palette for Bacteria.
#'
#' Slightly different than [yt.palette2]; now everything cycles through shades.
#' Also accomodates the name changes of Bacteroidetes to Bacteroidota,  Clostridiales to Eubacteriales, Ruminococcaceae to Oscillospiraceae
#'
#'
#' ```{r}
#' #| echo: false
#' taxlegend3 <- get.tax.legend(tax.palette = yt.palette3, fontsize = 5)
#' grid::grid.newpage()
#' grid::grid.draw(taxlegend3)
#' ```
#' @export
yt.palette3 <- exprs(
  "Bacteroidota/Bacteroidetes (phylum)" = Phylum %in% c("Bacteroidetes","Bacteroidota") ~ shades("#51AB9B", variation = 0.25),
  "Lachnospiraceae (family)" = Family=="Lachnospiraceae" ~ shades("#EC9B96", variation = 0.25),
  "Oscillospiraceae/Ruminococcaceae (family)"  = Family %in% c("Ruminococcaceae","Oscillospiraceae") ~ shades("#9AAE73", variation = 0.25),
  "Eubacteriales/Clostridiales (order)" = Order %in% c("Clostridiales","Eubacteriales") ~ shades("#9C854E", variation = 0.25),
  "Actinomycetota/Actinobacteria (phylum)" = Phylum %in% c("Actinobacteria","Actinomycetota") ~ shades("#A77097", variation = 0.25),
  "Enterococcus (genus)" = Genus=="Enterococcus" ~ shades("#129246", variation = 0.15),
  "Streptococcus (genus)" = Genus=="Streptococcus" ~ shades("#9FB846", variation = 0.15),
  "Staphylococcus (genus)"  = Genus=="Staphylococcus" ~ shades("#f1eb25", variation = 0.15),
  "Lactobacillus (genus)" = Genus=="Lactobacillus" ~ shades("#3b51a3", variation = 0.15),
  "Proteobacteria (phylum)" = Phylum=="Proteobacteria" ~ shades("red", variation = 0.4),
  "Other Bacteria" = TRUE ~ shades("gray", variation=0.25)
)




#' Fungal Palette
#'
#' The customary palette for Fungi.
#'
#' @export
fungal.palette <- exprs(
  "Saccharomyces cerevisiae (species)" = Species == "Saccharomyces cerevisiae" ~  "#DA8686",
  "Saccharomycetales (order)" = Order == "Saccharomycetales" ~ shades("#F0C3C3", variation = 0.4),
  "Candida (genus)" = Genus == "Candida" & Family == "Debaryomycetaceae" ~ shades("#DE0000", ncolor=6,variation = 0.6),
  "Aspergillus (genus)" = Genus == "Aspergillus" ~ shades("#3F8D3D", variation = 0.4),
  "Miscellaneous molds" = Class %in% c("Arthoniomycetes","Coniocybomycetes","Dothideomycetes",
                                       "Eurotiomycetes","Geoglossomycetes","Laboulbeniomycetes",
                                       "Lecanoromycetes","Leotiomycetes","Lichinomycetes","Orbiliomycetes",
                                       "Pezizomycetes","Sordariomycetes","Xylonomycetes") ~ shades("#ADDADA",variation=0.4),
  "Malassezia (genus)" = Genus == "Malassezia" ~ shades("#8A3030", variation = 0.6),
  "Basidiomycota (phylum)" = Phylum == "Basidiomycota" ~ shades("#C48C66", variation = 0.4),
  "Other" = TRUE ~ shades("gray", variation=0.25)
)

#' Create a color palette for taxonomy
#'
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
  requireNamespace(c("phyloseq","formula.tools"),quietly=TRUE)
  unitvar <- ensym(unitvar)
  if (is(data,"phyloseq") | is(data,"taxonomyTable")) {
    data <- get.tax(data)
  }
  if (!(is.list(tax.palette) && all(map_lgl(tax.palette,is_formula)))) {
    stop("YTError: tax.palette needs to be a list of formulas!")
  }
  vars.needed <- tax.palette %>% map(~{
    formula.tools::lhs(.x) %>% all.vars()
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
    colors <- formula.tools::rhs(exp) %>% eval_tidy()
    if (!is_color(colors)) {
      stop("YTError: not a valid color set: {paste(colors,collapse=', ')}")
    }
    criteria <- formula.tools::lhs(exp)
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
#' @param tax.palette a list of formulas specifying the palette. Default is [`yt.palette3`].
#' @param fontsize Font size. Default is 5.
#' @return a ggplot object showing the legend.
#' @describeIn get.tax.palette
#' @export
#' @examples
get.tax.legend <- function(tax.palette=yt.palette3,fontsize=5) {
  requireNamespace("formula.tools",quietly=TRUE)
  glist <- imap(tax.palette,function(exp,label) {
    colors <- formula.tools::rhs(exp) %>% rlang::eval_tidy()
    criteria <- formula.tools::lhs(exp)
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


#' Plot tax
#'
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


#' The color scheme used in CID manuscript.
#' @author Ying Taur
#' @export
cid.colors <- c("Enterococcus"="#129246","Streptococcus"="#a89e6a","Blautia"="#f69ea0",
                "Bacteroides"="#2dbfc2","Lactobacillus"="#3b51a3","Dorea"="#a9853e",
                "Staphylococcus"="#f1eb25","Coprobacillus"="#b53572",
                "unclassified_Firmicutes"="#79449a","unclassified_Lachnospiraceae"="#afd7db",
                "Roseburia"="#9ba744","Parabacteroides"="#329982","Coprococcus"="#663939",
                "Spracetigenium"="#72b443","Veillonella"="#653f99","Lactococcus"="#51a546",
                "Granulicatella"="#a5a7aa","Proteobacteria"="#ed2024","Other Bacteroidetes"="#963695",
                "Other Firmicutes"="#929497","Other Bacteria"="#6d6e70")


# tree drawing/manipulation functions --------------------------------------------------




#' Highlight a clade in ggtree
#'
#' @param gdata a data frame from a ggtree object.
#' @param var the variable to be matched (unquoted).
#' @param value the value that `var` needed for inclusion in the clade to be highlighted.
#' @param criteria an expression that evaluates to logical, can use as an alternative (or in addition to) var/value,
#' @param ymin the min value of y in the clade.
#' @param ymax the max value of y in the clade.
#' @param label A `glue` expression for the clade label. Default is `"{value`\n({var})"}
#' @param fill.color the clade fill color (default is `NA`, no fill)
#' @param line.color the color of the outline around the clade (default `"dark gray"`)
#' @param alpha the alpha value of the fill color (default 1)
#' @param xmax the x depth at which the edge of the clade is drawn (optional)
#' @param xscalar if `xmax` is not specified, the x depth can be specified as a scaled factor of maximum x of all clade tips. Default is `1.5`
#' @param fill.in whether to fill all gaps in the clade (default is `FALSE`)
#' @param font.size font size of the text (default is 4)
#'
#' @return
#' @export
#'
#' @examples
hilight.clade <- function(gdata, var=NULL, value=NULL,
                          criteria=NULL,
                          ymin=-Inf,ymax=Inf,
                          label="{value}\n({var})",
                          fill.color=NA,line.color="dark gray",
                          alpha=1,
                          xmax=NULL,xscalar=1.5,fill.in=FALSE,font.size=4) {
  requireNamespace(c("ggtree","glue"),quietly=TRUE)
  if (is(gdata,"ggtree")) {
    gdata <- gdata$data
  }
  if (!is.data.frame(gdata)) {
    stop("YTError: gdata is not a data frame from ggtree!")
  }
  qvar <- enquo(var)
  var <- as_label(qvar)
  qvalue <- enquo(value)
  value <- eval_tidy(qvalue)
  criteria <- enquo(criteria)
  .ymin <- ymin
  .ymax <- ymax
  # if (!quo_is_null(qvar) && !quo_is_null(qvalue)) {
  if (!quo_is_null(qvar) && !quo_is_null(qvalue)) {
    var.value.criteria <- expr(!!qvar==!!qvalue)
  } else {
    var.value.criteria <- TRUE
  }
  if (quo_is_null(criteria)) {
    criteria <- TRUE
  }
  gdata <- gdata %>%
    mutate(.include=!!var.value.criteria & !!criteria & is.between(y,.ymin,.ymax) & isTip) %>%
    replace_na(list(.include=FALSE)) %>%
    arrange(y)
  if (sum(gdata$.include)==0) {
    warning(str_glue("YTWarning: no tips found for {var}={value}"))
    return(NULL)
  }
  ylow <- min(gdata$y[gdata$.include]) - 0.5
  yhigh <- max(gdata$y[gdata$.include]) + 0.5
  if (fill.in) {
    gdata <- gdata %>% dplyr::mutate(.include=isTip & is.between(y,ylow,yhigh))
  }
  gdata.sub <- gdata %>% filter(.include)
  ymid <- (yhigh+ylow)/2
  xmin1 <- dplyr::first(gdata.sub$x)
  xmin2 <- dplyr::last(gdata.sub$x)
  if (is.null(xmax)) {
    xmax <- max(gdata.sub$x) * xscalar
  }
  glabel <- glue::glue(label)
  rect.list <- list(geom_rect(data=gdata.sub,aes(xmin=x,xmax=xmax,ymin=y-0.5,ymax=y+0.5),alpha=alpha,fill=fill.color))
  segment.list <- list(
    annotate("segment",x=xmin1,xend=xmax,y=ylow,yend=ylow,color=line.color),
    annotate("segment",x=xmin2,xend=xmax,y=yhigh,yend=yhigh,color=line.color),
    annotate("segment",x=xmax,xend=xmax,y=ylow,yend=yhigh,color=line.color),
    annotate("text",x=xmax,y=ymid,label=glabel,lineheight=0.9,size=font.size))
  return(c(rect.list,segment.list))
}





#' Hilight a set of ggtree tips.
#'
#' geom_hilight(aes(isTip=isTip,var=Genus,value="Blautia"))
#' geom_hilight(aes(isTip=isTip,var=Genus,value="Blautia"),xend=2)
#' geom_hilight(aes(isTip=isTip,var=Genus,value="Blautia"),xadd=1.5)
#' @param oligo.file oligo file to be read. If oligo.file is a directory, all oligo files (*.oligos) will be read in.
#' @return Returns a data frame containing oligo information
#' @examples
#' @author Ying Taur
#' @export
geom_hilight <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity", ...,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat, geom = GeomHilight,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list( na.rm = na.rm, ...)
  )
}

#' @export
GeomHilight <- ggplot2::ggproto("GeomHilight", ggplot2::Geom,
                                default_aes = ggplot2::aes(label = NA,
                                                           colour = "black",
                                                           fill = "light blue",
                                                           xend = NA,
                                                           xadd = NA,
                                                           linesize = 0.5,
                                                           linetype = 1, lineheight = 1.2,
                                                           alpha = NA,
                                                           size = 3.88,
                                                           angle = 0, hjust = 0.5, vjust = 0.5, family = "", fontface = 1),
                                required_aes = c("x","y"),
                                setup_data = function(data, params) {
                                  data %>% filter(isTip,var==value)
                                },
                                draw_group = function(data, panel_scales, coord) {
                                  linesize <- data$linesize[1]
                                  topdata <- data[which.max(data$y),,drop=FALSE]
                                  bottomdata <- data[which.min(data$y),,drop=FALSE]
                                  xend <- data$xend[1]
                                  xadd <- data$xadd[1]
                                  label <- data$label[1]
                                  if (is.na(label)) {
                                    label <- data$value[1]
                                  }
                                  if (is.na(xend) & is.na(xadd)) {
                                    xend <- max(data$x,na.rm=TRUE)
                                    xadd <- 0.5
                                  } else if (is.na(xend) & !is.na(xadd)) {
                                    xend <- max(data$x)
                                  } else if (!is.na(xend) & is.na(xadd)) {
                                    xadd <- 0
                                  } else if (!is.na(xend) & !is.na(xadd)) {
                                    warning("YTWarning: both xend and xadd were specified, only one should be specified")
                                  } else stop("YTError: should not happen, look for code issues")
                                  xend <- xend + xadd
                                  xtop <- topdata$x
                                  ytop <- topdata$y+0.5
                                  xbottom <- bottomdata$x
                                  ybottom <- bottomdata$y-0.5
                                  rect_data <- data.frame(xmin=data$x,
                                                          xmax=xend,
                                                          ymin=data$y-0.5,
                                                          ymax=data$y+0.5,
                                                          colour=NA,data[1,,drop=FALSE],row.names=NULL)
                                  text_data <- data.frame(x=xend,y=(ytop+ybottom)/2,label=label,
                                                          data[1,,drop=FALSE],row.names=NULL)
                                  line_data <- data.frame(x=c(xend,xtop,xbottom),y=c(ytop,ytop,ybottom),
                                                          xend=c(xend,xend,xend),yend=c(ybottom,ytop,ybottom),
                                                          size=linesize,
                                                          data[1,,drop=FALSE],row.names=NULL)
                                  gList(GeomRect$draw_panel(rect_data, panel_scales, coord),
                                        GeomText$draw_panel(text_data, panel_scales, coord),
                                        GeomSegment$draw_panel(line_data, panel_scales, coord))}
                                #draw_key = GeomRect$draw_key
)



#' Modify Angle for Text on Circular ggtree
#'
#' @param angle numeric vector of angles taken from ggtree data
#' @param hjust logical indicating whether to output the corresponding hjust parameter.
#' @param right logical indicating whether label should be at right angle to the tip being labelled.
#' @return new calculated angle or hjust aesthetic
#' @export
fan.angle <- function(angle,hjust=FALSE,right=FALSE) {
  angle <- angle %% 360
  top.side <- angle>=0 & angle<180
  right.side <- angle<=90 | angle>270
  if (right) {
    if (hjust) {
      rep(0.5,length(angle))
    } else {
      ifelse(top.side,angle-90,angle+90)
    }
  } else {
    if (hjust) {
      ifelse(right.side,0,1)
    } else {
      ifelse(right.side,angle,angle+180)
    }
  }
}




#' Conversion from Taxonomy Variables to Phylogenetic Trees (YT converted)
#'
#' Used to convert taxonomy table into a phylo object for plotting. A revised version of ape::as.phylo.formula.
#' Modified from a version from Liam J. Revell, University of Massachusetts, (https://stat.ethz.ch/pipermail/r-sig-phylo/2013-August/003017.html)
#'
#' Here are the changes:
#' (1) Corrected branch lengths. The original ape::as.phylo.formula does not work well because it uses ape::read.tree to read in data,
#' which can only read Newick trees without singleton branches. This uses read.newick instead, which can handle the singleton branches.
#' (2) Single branches are not automatically collapsed. This is so you can plot all levels of taxonomy.
#' (3) All nodes are labeled.
#' @param x a right-side formula describing the taxonomic relationship: ~C1/C2/.../Cn.
#' @param data the data.frame where to look for the variables (default to environment).
#' @param collapse.singles whether or not to collapse singleton nodes. Default is FALSE.
#' @param ... further arguments to be passed from other methods.
#' @return An object of class phylo.
#' @examples
#' library(ggtree)
#' library(ggplot)
#' t <- tribble (
#'   ~Superkingdom,  ~Phylum,          ~Class,                ~Order,               ~Family,               ~Genus,                   ~Species,
#'   "Bacteria",     "Firmicutes",     "Bacilli",             "Lactobacillales",    "Enterococcaceae",     "Enterococcus",           "Enterococcus faecium",
#'   "Bacteria",     "Firmicutes",     "Bacilli",             "Lactobacillales",    "Streptococcaceae",    "Streptococcus",          "Streptococcus salivarius",
#'   "Bacteria",     "Firmicutes",     "Erysipelotrichia",    "Erysipelotrichales", "Erysipelotrichaceae", "Erysipelatoclostridium", "Erysipelatoclostridium ramosum",
#'   "Bacteria",     "Firmicutes",     "Erysipelotrichia",    "Erysipelotrichales", "Erysipelotrichaceae", "Erysipelatoclostridium", "Erysipelatoclostridium ramosum",
#'   "Bacteria",     "Firmicutes",     "Erysipelotrichia",    "Erysipelotrichales", NA_character_,         NA_character_,            NA_character_,
#'   "Bacteria",     "Proteobacteria", "Gammaproteobacteria", "Enterobacterales",   "Enterobacteriaceae",  "Escherichia",            "Escherichia coli",
#'   "Bacteria",     "Actinobacteria", NA_character_,         NA_character_,        NA_character_,         NA_character_,            NA_character_)
#' phy <- as.phylo.formula2(~Superkingdom/Phylum/Class/Order/Family/Genus/Species,data=t)
#' gt <- ggtree(phy,layout="rectangular")
#' gt + geom_text(aes(label=label))
#'
#' #levels are not same level.
#' library(ape)
#' data(carnivora)
#' t1 <- as.phylo.formula(~SuperFamily/Family/Genus/Species, data=carnivora)
#' par(lend=2)
#' plot(t1,edge.width=2,cex=0.6,no.margin=TRUE)
#' #this is correct.
#' t2 <- as.phylo.formula2(~SuperFamily/Family/Genus/Species, data=carnivora)
#' par(lend=2)
#' plot(t2,edge.width=2,cex=0.6,no.margin=TRUE)
#' @author Ying Taur
#' @export
as.phylo.formula2 <- function (x, data = parent.frame(), collapse.singles=FALSE, distinct.tree=TRUE, full.taxonomy.only=FALSE, ...){
  requireNamespace("phytools",quietly=TRUE)
  err <- "Formula must be of the kind \"~A1/A2/.../An\"."
  if (length(x) != 2)
    stop(err)
  if (x[[1]] != "~")
    stop(err)
  f <- x[[2]]
  taxo <- list() #list of vectors, from species->phylum
  while (length(f) == 3) {
    if (f[[1]] != "/")
      stop(err)
    taxo[[deparse(f[[3]])]] <- data[[deparse(f[[3]])]]
    if (length(f) > 1)
      f <- f[[2]]
  }
  taxo[[deparse(f)]] <- data[[deparse(f)]]
  taxnames <- taxo %>% map(~unique(.[!is.na(.)])) %>% unlist(use.names=FALSE)
  nodenames <- seq_along(taxnames) %>% paste0("v",.) %>% as.character()
  taxo <- taxo %>% map(~nodenames[match(.,taxnames)])
  taxo.data <- as.data.frame(taxo) #tax data from species>kingdom
  if (distinct.tree) {
    taxo.data <- taxo.data %>% distinct()
  }
  if (full.taxonomy.only) {
    taxo.data <- taxo.data[!is.na(taxo.data[,1]),]
  }
  # leaves.names <- as.character(taxo.data[, 1]) #species
  # taxo.data[, 1] <- 1:nrow(taxo.data) #replace species with node numbers
  f.rec <- function(subtaxo) {
    u <- ncol(subtaxo) #number of ranks
    all.na <- apply(subtaxo,1,function(x) all(is.na(x)))
    subtaxo <- subtaxo[!all.na,,drop=FALSE]
    if (nrow(subtaxo)==0) {
      return(NULL)
    }
    levels <- unique(subtaxo[, u]) #last column (bacteria,...,genus)
    if (u == 1) {
      if (length(levels) != nrow(subtaxo))
        warning("Error, leaves names are not unique.")
      return(as.character(subtaxo[, 1]))
    }
    t <- character(length(levels))
    for (l in 1:length(levels)) { #for each taxon of the level
      #l=1
      x <- f.rec(subtaxo[subtaxo[, u] == levels[l], ][1:(u - 1)]) #subset of taxo.data for that level.
      if (is.null(x)) {
        t[l] <- levels[l]
      } else {
        t[l] <- paste("(", paste(x,collapse=","), ")",levels[l], sep = "")
      }
    }
    return(t)
  }
  string <- paste("(", paste(f.rec(taxo.data), collapse = ","),");", sep = "")
  phy <- phytools::read.newick(text = string) ## so that singles will be read without error
  phy$edge.length <- rep(1,nrow(phy$edge))
  if (collapse.singles) {
    phy <- collapse.singles(phy)
  }
  phy$tip.label <- phy$tip.label %>% map_chr(~taxnames[match(.,nodenames)])
  phy$node.label <- phy$node.label %>% map_chr(~coalesce(taxnames[match(.,nodenames)],.))
  return(phy)
}






# lefse functions ---------------------------------------------------------



#' LDA Effect Size
#'
#' Given a [`phyloseq`][`phyloseq::phyloseq-class`] object and class variable, perform LDA Effect Size analysis.
#' @param phy the [`phyloseq`][`phyloseq::phyloseq-class`] object containing the data
#' @param class set which variable use as class
#' @param subclass set which variable use as subclass (default is no subclass)
#' @param anova.alpha set the alpha value for the Kruskal-Wallis test (default 0.05)
#' @param wilcoxon.alpha set the alpha value for the Wilcoxon test (default 0.05)
#' @param lda.cutoff set the threshold on the absolute value of the logarithmic LDA score (default 2.0)
#' @param wilcoxon.within.subclass set whether perform the wilcoxon test only among the subclasses with the same name (default FALSE)
#' @param one.against.one (for multiclass tasks) set whether the test is performed in a
#' one-against-one (TRUE - more strict) or in a one-against-all setting (FALSE - less strict) (default FALSE)
#' @param n_boots set the number of bootstrap iteration for LDA (default 30). Set to NULL to skip bootstrapping altogether.
#' @param min_c minimum number of samples per subclass for performing wilcoxon test (default 10)
#' @param f_boots set the subsampling fraction value for each bootstrap iteration (default 0.67)
#' @param mult.test.correction set the multiple testing correction options. 0 no correction (more strict, default),
#' 1 correction for independent comparisons, 2 correction for dependent comparison
#' @return a table containing features tested and results.
#' @examples
#' lda <- lda.effect(cid.phy,class="Consistency")
#' @export
lda.effect <- function(phy,class,subclass=NULL,
                       subject=NULL,
                       anova.alpha = 0.05,
                       wilcoxon.alpha = 0.05,
                       lda.cutoff = 2,
                       wilcoxon.within.subclass = FALSE,
                       one.against.one = FALSE,
                       n_boots = 30,
                       min_c = 10,
                       f_boots = 0.67,
                       mult.test.correction = 0) {

  requireNamespace(c("phyloseq","progress","MASS","purrr"),quietly=TRUE)
  if (!(mult.test.correction %in% c(0,1,2))) {stop("YTError: mult.test.correction should 0, 1, or 2.")}
  ranks <- phyloseq::rank_names(phy)
  if (is.null(subclass)) {
    # add a meaningless non-varying subclass as placeholder, if it is not specified
    subclass <- paste0(class,"_subclass")
    phyloseq::sample_data(phy)[[subclass]] <- phyloseq::sample_data(phy)[[class]]
  }
  s <- suppressWarnings(get.samp(phy)) %>%
    mutate(across(any_of(c(class,subclass)),as.character))
  phyloseq::sample_data(phy) <- s %>% set.samp()

  # calculate totalseqs... need to recalculate pctseqs to avoid floating point errors.
  otu <- get.otu.melt(phy,filter.zero=FALSE) %>%
    add_count(sample,wt=numseqs,name="totalseqs")
  # create data for all tax levels for analysis
  otul <- lapply(1:length(ranks),function(x) {
    lvls <- ranks[1:x]
    vars <- c("sample","totalseqs",class,subclass,lvls)
    otu %>%
      group_by(!!!syms(vars)) %>%
      summarize(numseqs=sum(numseqs),.groups="drop") %>%
      mutate(taxonomy_=paste(!!!syms(lvls),sep="|"),rank=x)
  }) %>% bind_rows() %>%
    mutate(pctseqs=1000000*numseqs/totalseqs)

  class.levels <- unique(otu[[class]]) %>% as.character()
  subclass.levels <- unique(otu[[subclass]]) %>% as.character()

  n.features <- n_distinct(otul$taxonomy_)
  #create class and subclass comparisons. these can be inner joined to otul to get the correct comparison.
  class.comparisons <- combn(class.levels,2,simplify=FALSE) %>%
    purrr::imap_dfr(~tibble(!!class:=.x,class=.x,class.list=list(.x),class.id=.y,group.number=1:2,class.desc=paste(.x,collapse=" vs. ")))
  subclass.comparisons <- cross2(subclass.levels,subclass.levels) %>% simplify_all() %>%
    purrr::imap_dfr(~tibble(!!subclass:=.x,subclass=.x,subclass.list=list(.x),subclass.id=.y,group.number=1:2,subclass.desc=paste(.x,collapse=" vs. ")))
  if (wilcoxon.within.subclass) {
    subclass.comparisons <- subclass.comparisons %>%
      group_by(subclass.id) %>%
      filter(n_distinct(subclass)==1) %>%
      ungroup()
  }
  #determine all possible class/subclass combinations
  s.levels <- s %>% count(!!sym(class),!!sym(subclass)) %>%
    group_by(!!sym(class)) %>%
    mutate(n.sublevels=n()) %>%
    ungroup()
  all.comparisons <- inner_join(class.comparisons,subclass.comparisons,by="group.number") %>%
    inner_join(s.levels,by=c(class,subclass)) %>%
    group_by(class.id,subclass.id) %>%
    filter(n()==2) %>%
    mutate(all.id=cur_group_id(),
           wilcoxon.alpha.corrected=case_when(
             mult.test.correction == 0 ~ wilcoxon.alpha,
             mult.test.correction == 1 ~ 1-(1-wilcoxon.alpha)^(n.sublevels[1]*n.sublevels[2]),
             mult.test.correction == 2 ~ wilcoxon.alpha * n.sublevels[1] * n.sublevels[2]
           )) %>%
    ungroup()

  message(str_glue("class: {class} ({length(class.levels)} values)\nsubclass: {subclass} ({length(subclass.levels)} values)\nclass/subclass comparisons per tax level: {n_distinct(all.comparisons$all.id)}\nlevels: {n.features} taxonomic features (across {length(ranks)} levels: {paste(ranks,collapse=\"|\")})"))

  pb <- progress::progress_bar$new(total = n.features)
  pb$message(str_glue("Performing KW and W subclass testing ({n.features} tax features)"))
  results <- otul %>%
    group_by(taxonomy_,rank,!!!syms(ranks)) %>%
    group_modify(function(x,...) {
      # x <- otul %>% filter(taxonomy_=="Bacteria|Acidobacteria|Acidobacteria_Gp11")
      pb$tick()
      # kruskal test (class)
      kw <- kruskal.test(x[["pctseqs"]],as.factor(x[[class]]))
      kw.summary <- tibble(kw.pvalue=kw$p.value) %>%
        mutate(kw.pass=!is.na(kw.pvalue) & (kw.pvalue<anova.alpha),
               kw.info=if_else(kw.pass,NA_character_,"KW: non-signif"))
      # wilcoxon test overall (class/subclass)
      if (!kw.summary$kw.pass) {
        w.summary <- tibble(w.pass=NA,w.info=NA_character_)
      } else { # do the wilcox subclass testing
        w <- x %>% inner_join(all.comparisons,by=c(class,subclass)) %>%
          group_by(all.id,class.list,class.id,class.desc,subclass.list,subclass.id,subclass.desc,wilcoxon.alpha.corrected) %>%
          group_modify(function(y,...) {
            # y <- x %>% inner_join(all.comparisons,by=c(class,subclass)) %>% filter(all.id==first(all.id)) %>% mutate(pctseqs=0)
            if (n_distinct(y$pctseqs)==1) {
              return(tibble(w.pvalue=NA_real_,
                            direction=NA_character_,
                            small.size=NA,
                            all.same.value=TRUE))
            }
            wilcox <- suppressWarnings(wilcox.test(pctseqs ~ class, data=y))
            highest.median <- y %>% group_by(class) %>%
              summarize(median.pct=median(pctseqs),n=n(),.groups="drop") %>%
              arrange(desc(median.pct)) %>%
              summarize(highest=if_else(n_distinct(median.pct)==1,"[equal]",first(class)),
                        min.size=min(n))
            tbl <- tibble(w.pvalue=wilcox$p.value,
                          direction=highest.median$highest,
                          small.size=highest.median$min.size<min_c,
                          all.same.value=FALSE)
            return(tbl)
          }) %>%
          group_by(class.id,class.list,class.desc) %>%
          mutate(w.signif=!is.na(w.pvalue) & (w.pvalue<2*wilcoxon.alpha.corrected),
                 one.direction=(n_distinct(direction,na.rm=TRUE)==1),
                 equal.median=(n()>1) & direction=="[equal]",
                 subclass.pass=!all.same.value & (w.signif|small.size) & one.direction & !equal.median,
                 subclass.info=coalesce_indicators("all.same.value"=all.same.value,
                                                   "equal.medians"=equal.median,
                                                   "not.signif"=!w.signif & !small.size,
                                                   "different.direction"=!one.direction,
                                                   first.hit.only=TRUE)) %>%
          ungroup()

        w.class <- w %>%
          group_by(class.id,class.list,class.desc) %>%
          summarize(w.pass=all(subclass.pass,na.rm=TRUE),
                    w.info=list(subclass.info),
                    .groups="drop")

        if (one.against.one) { # one.against.one: pass if all class comparisons pass
          w.summary <- w.class %>%
            summarize(w.pass=all(w.pass,na.rm=TRUE),
                      w.info=paste(unique(sort(fct_infreq(unlist(w.info)))),collapse="|"))
        } else { # one.against.all: pass if all class comparisons within a certain class value pass.
          w.summary <- w.class %>%
            unnest(cols=class.list) %>%
            group_by(class.list) %>%
            summarize(w.num.passed=sum(w.pass,na.rm=TRUE),.groups="drop",
                      w.info=list(w.info)) %>%
            mutate(w.pass=w.num.passed>=length(class.levels)-1) %>%
            summarize(w.pass=any(w.pass,na.rm=TRUE),
                      w.info=paste2(unique(sort(fct_infreq(unlist(w.info)))),collapse="|"))
        }
        w.summary <- w.summary %>% mutate(w.info=str_c("W: ",w.info))
      }
      summary <- bind_cols(kw.summary,w.summary) %>%
        mutate(disc.feature=kw.pass & (w.pass | is.na(w.pass)))
      return(summary)
    }) %>% ungroup()

  tax.features <- results %>% filter(disc.feature) %>% pull(taxonomy_) %>% unique()
  direction <- otul %>%
    group_by(!!sym(class),taxonomy_) %>%
    summarize(pctseqs=mean(pctseqs),.groups="drop") %>%
    group_by(taxonomy_) %>%
    arrange(desc(pctseqs)) %>%
    summarize(direction=first(!!sym(class)),log.max=log10(first(pctseqs)),.groups="drop")

  calc.lda <- function(otudata,formula) {
    z <- suppressWarnings(MASS::lda(formula,data=otudata,tol=0.0000000001))
    w <- z$scaling[,1]
    names(w) <- gsub("`","",names(w))
    w.unit <- w/sqrt(sum(w^2))
    xy.matrix <- otudata %>% select(-!!(sym(class)),-sample) %>% as.matrix()
    LD <- xy.matrix %*% w.unit
    p <- class.levels
    effect.size <- abs(mean(LD[otudata[,class]==p[1]]) - mean(LD[otudata[,class]==p[2]]))
    scal <- w.unit * effect.size
    rres <- z$means
    colnames(rres) <- gsub("`","",colnames(rres))
    lenc <- length(colnames(rres))
    coeff <- if_else(!is.na(scal),scal,0)
    gm  <- apply(rres,2,function(x) abs(x[1]-x[2]))
    means <- (gm + coeff)*0.5
    means
  }

  if (length(tax.features)==0) {
    # message("No significant features found.")
    log.bootmeans <- tibble(taxonomy_=NA_character_,lda=NA_real_)
  } else {
    otu.d <- otul %>% filter(taxonomy_ %in% tax.features) %>%
      pivot_wider(id_cols=c(sample,!!sym(class)),names_from=taxonomy_,values_from=pctseqs)
    otu.d.rnorm <- otu.d %>% group_by(!!sym(class)) %>% mutate(across(all_of(tax.features),~{
      if (n_distinct(.)>length(.)*0.5) {
        new.pcts <- .
      } else {
        new.pcts <- purrr::map_dbl(.,~abs(.+rnorm(1,0,sd=pmax(.*0.05,0.01))))
      }
      new.pcts
    })) %>% ungroup()
    formula <- as.formula(paste0(class," ~ ",paste0("`",tax.features,"`",collapse=" + ")))
    if (!is.null(n_boots)) {
      pb <- progress::progress_bar$new(total = n_boots)
      pb$message(str_glue("Bootstrapping LDA effect sizes ({n_boots} samples)..."))
      lda.bootstrap <- purrr::map_dfr(1:n_boots,~{
        pb$tick()
        for (i in 1:1000) {
          sub_d <- otu.d.rnorm %>% sample_frac(size=f_boots,replace=TRUE)
          class.counts <- sub_d %>% count(!!sym(class))
          all.classes.present <- nrow(class.counts)==length(class.levels)
          all.counts.above.min <- all(class.counts$n>=min_c)
          if (all.classes.present & all.counts.above.min) {
            break
          }
        }
        calc.lda(sub_d,formula)
      })
      bootmeans <- lda.bootstrap %>% purrr::map(mean)
    } else {
      message("Skipping bootstrap step.")
      bootmeans <- calc.lda(otu.d.rnorm,formula)
    }
    log.bootmeans <- bootmeans %>%
      purrr::map(~sign(.)*log10(1+abs(.))) %>%
      # take max across pairs map()
      purrr::map_dfr(~tibble(lda=.x),.id="taxonomy_")
    if (nrow(log.bootmeans)==0) {
      log.bootmeans <- tibble(taxonomy_=NA_character_,lda=NA_real_)
      message("No features significant!")
    }
  }

  final <- results %>%
    left_join(log.bootmeans,by="taxonomy_") %>%
    left_join(direction,by="taxonomy_") %>%
    rowwise() %>%
    mutate(taxrank=purrr::map_chr(rank,~ranks[.]),
           taxrank=factor(taxrank,levels=rank_names(phy)),
           taxon_=c(!!!syms(ranks))[rank]) %>%
    ungroup() %>%
    mutate(taxon_=str_glue("{taxon_} ({taxrank})"),
           taxon_=make.unique(taxon_),lda=abs(lda),
           lda.pass=lda>lda.cutoff,
           lda.info=ifelse(lda.pass,NA_character_,"LDA: below cutoff"),
           pass=kw.pass & (is.na(w.pass) | w.pass) & (lda.pass & !is.na(lda.pass)),
           info=coalesce(kw.info,w.info,lda.info),
           info=paste2(ifelse(pass,"PASS","FAIL:"),info)) %>%
    select(-disc.feature,-kw.info,-lda.info,-w.info,-all_of(ranks)) %>%
    select(taxonomy=taxonomy_,taxon=taxon_,direction,lda,pass,kw.pass,w.pass,lda.pass,info,everything())
  message(str_glue("Number of significantly discriminative features: {length(tax.features)} ( {sum(results$kw.pass,na.rm=TRUE)} ) before internal wilcoxon
                   Number of discriminative features with abs LDA score > {lda.cutoff} : {sum(final$pass)}"))
  return(final)
}




#' Plot LDA Results
#' @param lda lda table from `lda.effect()`
#' @param tax.label either "taxon" or "taxonomy"
#'
#' @return a ggplot of lda results
#' @export
#' @examples
lda.plot <- function(lda,tax.label="taxon") {
  if (n_distinct(lda$direction)==2) {
    ldaplot <- lda %>% filter(pass) %>%
      mutate(lda=if_else(as.numeric(factor(direction))==2,-lda,lda),
             hjust=if_else(as.numeric(factor(direction))==2,0,1)) %>%
      arrange(lda) %>%
      mutate(tax.label=fct_inorder(!!sym(tax.label)))
    limits <- max(lda$lda,na.rm=TRUE) * c(-1,1)
  } else {
    ldaplot <- lda %>% filter(pass) %>%
      arrange(direction,lda) %>%
      mutate(tax.label=fct_inorder(!!sym(tax.label)),
             hjust=1)
    limits <- c(0,max(lda$lda,na.rm=TRUE))
  }
  ggplot(ldaplot,aes(x=tax.label,y=lda,fill=direction)) +
    geom_col() + geom_text(aes(y=0,label=tax.label,hjust=hjust)) + coord_flip() +
    scale_fill_discrete("Group") +
    scale_y_continuous("LDA score (log10)",limits=limits) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="top")
}


#' Plot LDA Results in Cladogram
#'
#' @param lda lda table from [lda.effect()]
#' @param layout Either "cirular" or "rectangular"
#' @param check_overlap Only write clade labels if they do not overlap each other. Default is `TRUE`. This is passed to `geom_text`.
#' @param font.size Font size of clade labels. Default is 3.88.
#' @param clade.label.height Height of clade labels, relative to the size of the concentric tax levels. Default is 1 ()
#' @param pad Determines the spacing of surrounding clade labels.
#' @return ggplot of lda cladogram
#' @export
#'
#' @examples
lda.clado <- function(lda,layout="circular",pad=2,check_overlap=TRUE,
                      font.size=3.88,
                      clade.label.height=1) {
  # For each taxonomy, determine the individual level values. Make sure the values remain unique.
  #   E.g. for taxonomy = Bacteria|Bacteroidetes|Bacteroidia :
  #   Superkingdom = Bacteria
  #   Phylum = Bacteria|Bacteroidetes
  #   Class = Bacteria|Bacteroidetes|Bacteroidia
  #   Order/Family/Genus/Species = NA
  lda.tbl <- lda
  split <- str_split(lda.tbl$taxonomy,"\\|")
  max.n.levels <- split %>% map_int(length) %>% max()

  if (is.factor(lda$taxrank)) {
    lvls <- levels(lda$taxrank)
  } else {
    warning("YTWarning: taxrank is not a factor, rank levels may be missing")
    lvls <- lda %>% arrange(rank) %>% pull(taxrank) %>% unique()
  }
  if (max.n.levels!=length(lvls)) {
    stop(str_glue("YTError: taxonomy splits into {max.n.levels} levels, but {length(lvls)} were in taxrank. Consider converting taxrank to a factor containing all levels."))
  }
  for (i in 1:length(lvls)) {
    lvl <- lvls[i]
    lda.tbl[[lvl]] <- split %>% map_chr(~str_c(.[1:i],collapse="|"))
  }
  form <- as.formula(paste0("~",paste(lvls,collapse="/")))
  lefse.phy <- as.phylo.formula2(form,data=lda.tbl,full.taxonomy.only=FALSE)
  gt <- ggtree(lefse.phy,layout=layout)
  get.children.yrange <- function(node,gd) {
    hits <- gd$node[gd$parent==node]
    if (length(hits)==0 | node %in% hits) {
      return(gd$y[gd$node==node])
    } else {
      return(unlist(lapply(hits,get.children.yrange,gd)))
    }
  }
  if (!all(lda$taxonomy %in% gt$data$label)) {
    n.orphans <- setdiff(lda$taxonomy,gt$data$label) %>% length()
    n.total <- length(lda$taxonomy)
    stop(str_glue("YTError: not all taxonomy elements translated to phylo object! {n.orphans} out of {n.total} taxa not found."))
  }
  gd <- gt$data %>% left_join(lda,by=c("label"="taxonomy")) %>%
    mutate(y.range=lapply(node,get.children.yrange,cur_data()),
           ymin=map_dbl(y.range,min)-0.5,ymax=map_dbl(y.range,max)+0.5,ymid=(ymin+ymax)/2,
           xmin=x,
           # xmax=pad+2*length(lvls)-x,
           xmax=(1 + length(lvls)) + clade.label.height*(1 + length(lvls)-x),
           xtext=xmax-clade.label.height*0.5,
           short.label=map_chr(str_split(label,"\\|"),last)) %>%
    arrange(pass,desc(ymax-ymin))
  if (layout!="circular") {
    gd <- gd %>% mutate(angle.label=0)
  } else {
    gd <- gd %>%
      mutate(angle.label=scales::rescale(ymid,from=c(0,max(y)),to=c(0,360)),
             angle.label=if_else(is.between(angle.label,0,180),angle.label-90,angle.label+90))
  }
  gt +
    geom_point(data=gd,aes(size=log.max),color="dark gray",fill="gray",shape=21,alpha=0.75) +
    geom_rect(data=filter(gd,pass),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=direction),color="dark gray",alpha=0.2) +
    # geom_fit_text(data=filter(gd,pass),aes(xmin=xmax-1,xmax=xmax,ymin=ymin,ymax=ymax,label=short.label,fill=direction),color="dark gray",alpha=0.2,min.size = 0) +
    geom_text(data=filter(gd,pass),aes(x=xtext,y=ymid,label=short.label,angle=angle.label),lineheight=0.75,check_overlap=check_overlap,size=font.size) +
    geom_point(data=filter(gd,pass),aes(fill=direction,size=log.max),shape=21,alpha=0.75) +
    scale_size_continuous("Max Abund (Log10)") +
    scale_fill_discrete("Group") +
    theme(legend.position="right")
}





# seq pipeline functions --------------------------------------------------




#' Read in tax file from blastn output
#'
#' This reads in the file created by the YT python script, blastn.py
#'
#' Chooses one taxonomy from the hits, also listing runner-up taxonomy.
#' * qseqid: Query Seq-id
#' * qgi: Query GI
#' * qacc: Query accesion
#' * qaccver: Query accesion.version
#' * qlen: Query sequence length
#' * sseqid: Subject Seq-id
#' * sallseqid: All subject Seq-id(s), separated by a ';'
#' * sgi: Subject GI
#' * sallgi: All subject GIs
#' * sacc: Subject accession
#' * saccver: Subject accession.version
#' * sallacc: All subject accessions
#' * slen: Subject sequence length
#' * qstart: Start of alignment in query
#' * qend: End of alignment in query
#' * sstart: Start of alignment in subject
#' * send: End of alignment in subject
#' * qseq: Aligned part of query sequence
#' * sseq: Aligned part of subject sequence
#' * evalue: Expect value. Describes the number of hits one can "expect" to see by chance when searching a database of a particular size.
#'   It decreases exponentially as the Score (S) of the match increases. Essentially, the E value describes the random background noise.
#'   For example, an E value of 1 assigned to a hit can be interpreted as meaning that in a database of the current size one might
#'   expect to see 1 match with a similar score simply by chance.
#' * bitscore: Bit score
#' * score: Raw score
#' * length: Alignment length
#' * pident: Percentage of identical matches
#' * nident: Number of identical matches
#' * mismatch: Number of mismatches
#' * positive: Number of positive-scoring matches
#' * gapopen: Number of gap openings
#' * gaps: Total number of gaps
#' * ppos: Percentage of positive-scoring matches
#' * frames: Query and subject frames separated by a '/'
#' * qframe: Query frame
#' * sframe: Subject frame
#' * btop: Blast traceback operations (BTOP)
#' * staxid: Subject Taxonomy ID
#' * ssciname: Subject Scientific Name
#' * scomname: Subject Common Name
#' * sblastname: Subject Blast Name
#' * sskingdom: Subject Super Kingdom
#' * staxids: unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)
#' * sscinames: unique Subject Scientific Name(s), separated by a ';'
#' * scomnames: unique Subject Common Name(s), separated by a ';'
#' * sblastnames: unique Subject Blast Name(s), separated by a ';' (in alphabetical order)
#' * sskingdoms: unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)
#' * stitle: Subject Title
#' * salltitles: All Subject Title(s), separated by a '<>'
#' * sstrand: Subject Strand
#' * qcovs: Query Coverage Per Subject
#' * qcovhsp: Query Coverage Per HSP
#' * qcovus: Query Coverage Per Unique Subject (blastn only)
#'
#' @param tax Taxonomy data from blastn, either as the file or a data frame.
#' @param tax_table logical, if TRUE (default), will return a data frame of taxonomy, which can be directly converted to a phyloseq [tax_table][phyloseq::taxonomyTable-class] object. If FALSE, returns data frame with all hits and associated data.
#' @return Data from the blastn data file.
#' @author Ying Taur
#' @export
read.blastn.file <- function(tax.file,tax_table=TRUE) {
  requireNamespace(c("data.table","readr"),quietly=TRUE)
  # t <- data.table::fread(tax.file,colClasses=c("sallgi"="character","staxids"="character"),quote="") %>% tbl_df()
  t <- readr::read_tsv(tax.file,col_types=readr::cols(sallgi=readr::col_character(),staxids=readr::col_character()))
  ranklevels <- unlist(str_extract_all(t$taxonomy[1],middle.pattern("\\[","[a-z ]+","\\]")))
  ranklevels <- stringr::str_to_title(make.names(ranklevels))
  t <- t %>%
    mutate(taxonomy=gsub("\\[[a-z ]+\\]","",taxonomy),
           staxid=as.numeric(sapply(strsplit(staxids,split=";"),first)),
           otu=qseqid,    # otu=sub(";?$",";",qseqid),
           otu.number=as.numeric(str_extract(otu,"(?<=(OTU|ASV)_)[0-9]+"))) %>%
    separate(taxonomy,into=ranklevels,sep="\\|",remove=FALSE) %>%
    arrange(otu.number,otu,evalue,staxid) %>%
    group_by(otu) %>%
    # filter(!duplicated(taxonomy)) %>%
    mutate(evalue.rank=dense_rank(evalue)) %>%
    select(otu,Phylum,Family,Species,evalue,staxid,evalue.rank,pident,length,everything()) %>%
    ungroup()
  if (!tax_table) {
    return(t)
  } else {
    t <- t %>%
      group_by(otu) %>%
      # mutate(n.ties=sum(dense_rank(evalue)==1),blast.data=paste0(Species," (eval=",evalue,",pid=",pident,")",collapse=";")) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      select(otu,evalue,pident,!!!ranklevels)
    return(t)
  }
}





#' Read in oligos file.
#'
#' Reads in oligos file, listing pertinent information
#'
#' Oligos files are formatted for use in mothur. We are still using this format, despite the fact that
#' we no longer use mothur in our pipeline.
#' @param oligo.file oligo file to be read. If oligo.file is a directory, all oligo files (*.oligos) will be read in.
#' @param remove.commented logical indicating whether or not to remove commented lines (default TRUE)
#' @return Returns a data frame containing oligo information
#' @author Ying Taur
#' @export
read.oligos <- function(oligo.file,remove.commented=TRUE) {
  if (!file.exists(oligo.file)) {
    stop("YTError: file/directory not found: ",oligo.file)
  }
  if (dir.exists(oligo.file)) {
    oligo.files <- list.files(oligo.file,pattern="\\.oligos$",recursive=TRUE,full.names=TRUE)
    if (length(oligo.files)==0) {
      stop("YTError: no oligo files found in dir: ",oligo.file)
    }
    out <- bind_rows(lapply(oligo.files,read.oligos,remove.commented=remove.commented))
    return(out)
  }

  d <- tibble(line=scan(oligo.file,what=character(),sep="\n",quiet=TRUE)) %>%
    mutate(line=sub("[\t ]+$","",line),
           row=1:n(),
           info=recode2(line,c("^(primer|forward)\t"="primer",
                               "^#?barcode\t"="barcode",
                               "^#"="comment"),regexp=TRUE,else.value="error"))
  if (any(d$info=="error")) {
    stop("YTError: Did not understand one of the lines in the oligos file!\nFile: ",oligo.file,"\nLine: ",d$line[d$info=="error"][1])
  }

  p <- d %>% filter(info=="primer") %>%
    mutate(primer=sub("^(primer|forward)\t","",line))
  primer <- p$primer[1]
  b <- d %>% filter(info=="barcode") %>%
    mutate(line0=line,
           fields=str_split(line,"\t"),
           n.fields=sapply(fields,length),
           sample=sapply(fields,last),
           barcode.commented=substr(line,1,1)=="#",
           barcode=sapply(fields,function(x) paste(x[c(-1,-length(x))],collapse="\t")))
  if (!(all(b$n.fields==3)|all(b$n.fields==4))) {
    stop("YTError: barcode line did not contain 3 or 4 fields!")
  }
  # b <- b %>% select(-fields,-n.fields)
  cmt <- d %>% filter(info=="comment")
  pool <- sub("\\.oligos$","",basename(oligo.file),ignore.case=TRUE)
  if (is.na(pool)) {
    stop("YTError: Could not extract pool name from file!")
  }
  n.primers <- nrow(p)
  two.golay.barcodes <- all(grepl("[ACGT]{12}\t[ACGT]{12}",b$barcode))
  one.454.barcode <- all(grepl("[ACGT]{5,7}",b$barcode))
  if (n.primers==2 & two.golay.barcodes) {
    platform <- "miseq"
  } else if (n.primers==1 & one.454.barcode) {
    platform <- "454"
  } else {
    stop("YTError: Not sure what platform!")
  }

  out <- data.frame(b,primer,oligo.file,pool,platform,comment=paste(cmt$line,collapse="\n"),stringsAsFactors=FALSE) %>%
    select(pool,sample,primer,barcode,oligo.file,platform,barcode.commented,comment,row,line=line0)
  if (remove.commented) {
    out <- out %>% filter(!barcode.commented)
  }
  return(out)
}



