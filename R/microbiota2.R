
#need this for get.otu.melt to work in package.
.datatable.aware = TRUE


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
  otu <- read.delim(otu.file,header=TRUE,check.names=FALSE,row.names=row.names) %>%
    rownames_to_column("otu")
  return(otu)
}


#' Extract Phyloseq sample_data
#'
#' Returns \code{sample_data} component from phyloseq object, as a data frame.
#'
#' This basically is similar to the function \code{phyloseq::sample_data}, but does a few extra things.
#' (1) Converts to a data frame
#' (2) The sample name is stored in a column called \code{sample}. (\code{phyloseq} normally stores as a row name)
#' (3) Calculates number of sequences and alpha diversity metrics, if desired.
#' This function is the opposite of \code{set.samp}, which converts the data frame back into a \code{sample_data}.
#' Note that if the \code{phyloseq} object does not contain \code{sample_data}, a data frame containing a single column, \code{sample}, is returned.
#' @param phy phyloseq object containing sample_data
#' @param stats logical, whether or not to include summary statistics of samples. Stores \code{nseqs}, and diversity metrics.
#' @param measures diversity measures to calculate, if stats is TRUE. Default: c("Observed","InvSimpson","Shannon")
#' @return Data frame containing \code{sample_data} data.
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
#' @param sdata Data frame to be converted back to sample_data
#' @return formatted sample_data.
#' @export
set.samp <- function(sdata) {
  requireNamespace(c("phyloseq"),quietly=TRUE)
  ss <- sdata %>% column_to_rownames("sample") %>%
    data.frame(stringsAsFactors=FALSE) %>% phyloseq::sample_data()
  return(ss)
}

#' Extract Phyloseq tax_table
#'
#' Creates data.frame from tax_table, storing the rownames as variable "otu". The opposite of set.tax function.
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
#' @param tdata dataframe to be converted back to tax_table.
#' @return formatted tax_table.
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
#' @param as.matrix if \code{TRUE}, return matrix (instead of data frame with otu as column)
#' @return Dataframe containing otu table
#' @export
get.otu <- function(phy,as.matrix=TRUE) {
  requireNamespace("phyloseq",quietly=TRUE)

  if (phyloseq::taxa_are_rows(phy)) {
    otu <- phyloseq::otu_table(phy) %>% t() %>% as.matrix()
  } else {
    otu <- phyloseq::otu_table(phy) %>% as.matrix()
  }
  if (as.matrix) {
    return(otu)
  } else {
    otu.df <- otu %>% data.frame(stringsAsFactors=FALSE) %>% rownames_to_column("otu") %>% as_tibble()
    return(otu.df)
  }
}


#' Convert OTU table to phyloseq otu_table
#'
#' Use this on data.frames with tax data. The opposite of get.tax function. Make sure it contains the variable "otu".
#' @param odata otu table (matrix or dataframe with 'otu' column) to be converted back to otu_table.
#' @return formatted tax_table.
#' @export
set.otu <- function(odata,taxa_are_rows=TRUE) {
  requireNamespace("phyloseq",quietly=TRUE)
  if (is.data.frame(odata)) {
    if (("otu" %in% colnames(odata))) {
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
#' Note that phyloseq has a similar function, \code{psmelt}, but that takes longer.
#' The \code{get.otu.melt} now works by performing operations via data table, making it about 30x faster than before.
#' @param phy phyloseq object containing sample data
#' @param filter.zero Logical, whether or not to remove zero abundances. Default \code{TRUE}.
#' @param sample_data Logical, whether or not to join with \code{sample_data}. Default \code{TRUE}.
#' @return Data frame melted OTU data
#' @export
get.otu.melt <- function(phy,filter.zero=TRUE,sample_data=TRUE) {
  requireNamespace(c("phyloseq","data.table"),quietly=TRUE)
  # supports "naked" otu_table as `phy` input.
  otutab = as(phyloseq::otu_table(phy), "matrix")
  if (!phyloseq::taxa_are_rows(phy)) {
    otutab <- t(otutab)
  }
  otudt = data.table::data.table(otutab, keep.rownames = TRUE)
  data.table::setnames(otudt, "rn", "otu")
  # Enforce character otu key
  # note that .datatable.aware = TRUE needs to be set for this to work well.
  otudt[, otuchar:=as.character(otu)]
  otudt[, otu := NULL]
  data.table::setnames(otudt, "otuchar", "otu")
  # Melt count table
  mdt = data.table::melt.data.table(otudt, id.vars = "otu", variable.name = "sample", variable.factor=FALSE, value.name = "numseqs")
  if (filter.zero) {
    # Remove zeroes, NAs
    mdt <- mdt[numseqs > 0][!is.na(numseqs)]
  } else {
    mdt <- mdt[!is.na(numseqs)]
  }
  # Calculate relative abundance
  mdt[, pctseqs := numseqs / sum(numseqs), by = sample]
  if(!is.null(phyloseq::tax_table(phy, errorIfNULL=FALSE))) {
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
#' @param otu.melt table of taxa x sample abundances, similar to output of \code{get.otu.melt}
#' @param sample_id sample ID variable. Default \code{"sample"}.
#' @param taxa_id taxa/OTU ID variable. Default \code{"otu"}.
#' @param abundance_var abundance variable, used to fill \code{otu_table()}. Default \code{"sample"}.
#' @param taxranks vector of taxonomic ranks to be included in \code{tax_table()}.
#' @param sample_vars vector of sample variables to be included in \code{sample_data()}. If \code{NULL}, will attempt to determine sample vars automatically (vars that are distinct along with \code{sample_id}).
#'
#' @return phyloseq object, generated from the \code{otu.melt} data.
#' @export
#'
#' @examples
#' phy <- cid.phy
#' ranks <- rank_names(phy)
#' otu <- get.otu.melt(cid.phy)
#' phy2 <- get.phyloseq.from.melt(otu,taxranks=ranks)
get.phyloseq.from.melt <- function(otu.melt,sample_id="sample",abundance_var="numseqs",taxa_id="otu",
                                   taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),
                                   sample_vars=NULL) {

  sample_vars <- setdiff(sample_vars,sample_id)
  rows.are.distinct <- is.distinct(otu.melt, !!sym(taxa_id), !!sym(sample_id))
  if (!rows.are.distinct) {
    stop(str_glue("YTError: rows are not distinct across (sample_id x taxa_id)!"))
  }
  otu <- otu.melt %>%
    transmute(otu=as.character(!!sym(taxa_id)),sample=as.character(!!sym(sample_id)),numseqs=!!sym(abundance_var)) %>%
    pivot_wider(id_cols=otu,names_from=sample,values_from=numseqs,values_fill=0)
  tax <- otu.melt %>% select(otu=!!sym(taxa_id),!!!syms(taxranks)) %>% distinct()

  if (is.null(sample_vars)) {
    message("Attempting to determine sample vars...\n")
    distinct_sample_vars <- otu.melt %>% select(-all_of(c(taxa_id,taxranks,abundance_var))) %>%
      group_by(!!sym(sample_id)) %>%
      summarize(across(.fns=n_distinct,.groups="drop")) %>%
      summarize(across(-all_of(sample_id),.fns=~all(.==1))) %>%unlist() %>% {names(.)[.]}
    sample_vars <- distinct_sample_vars
  }
  if (length(sample_vars)==0) {
    samp <- NULL
  } else {
    samp <- otu.melt %>% select(sample=!!sym(sample_id),!!!syms(sample_vars)) %>% distinct()
  }
  if (anyDuplicated(tax$otu)!=0) {stop("YTError: taxranks are not distinct over taxa_id!")}
  if (anyDuplicated(samp$sample)!=0) {stop("YTError: sample vars are not distinct over sample!")}
  leftover.vars <- setdiff(names(otu.melt),c(abundance_var,sample_id,taxa_id,taxranks,sample_vars))
  message(str_glue("sample vars: [{sample_id}]; {paste(sample_vars,collapse=\", \")}"))
  message(str_glue("tax vars: [{taxa_id}]; {paste(taxranks,collapse=\", \")}"))
  message(str_glue("abundance var: [{abundance_var}]"))
  if (length(leftover.vars)>0) {
    message(str_glue("(vars not used: {paste(leftover.vars,collapse=\", \")})"))
  }
  phy <- phyloseq(set.otu(otu),set.tax(tax))
  if (ncol(samp)>1) {
    sample_data(phy) <- samp %>% set.samp()
  }
  return(phy)
}









#' Calculate Abundance from Phyloseq
#'
#' Given a phyloseq, calculate relative abundance for each sample.
#' @param phy phyloseq object to be analyzed
#' @param ... one or more taxonomic expressions defining the abundance to be calculated. Can be named, in order to specify the column name.
#' @param counts if \code{TRUE}, will return count rather than relative abundance
#' @return a data frame containing sample identifier and abundance columns
#'
#' @examples
#' get.abundance(cid.phy,pct.entero=Genus=="Enterococcus",pct.proteo=Phylum=="Proteobacteria")
#'
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
#' @param sdata sample data to be modified
#' @param ... one or more taxonomic expressions defining the abundance to be calculated. Can be named, in order to specify the column name.
#' @param phy phyloseq object to be used for caluclation
#' @param counts if \code{TRUE}, will return count rather than relative abundance
#'
#' @return returns \code{sdata}, with additional columns for abundance.
#'
#' @examples
#' get.samp(cid.phy) %>% add.abundance(pct.entero=Genus=="Enterococcus",pct.proteo=Phylum=="Proteobacteria",phy=cid.phy)
#'
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





#' Collapse Phyloseq Into Taxonomy
#'
#' In a phyloseq object, combine OTUs of the same taxonomic classification.
#'
#' Similar to \code{phyloseq::tax_glom}, but with the following differences:
#' (a) it performs much faster,
#' (b) requires all tax levels to be specified (instead of assuming all ranks to the left of the tax-level)
#' (c) the new OTU names will specify old OTU names separated by '|'
#' @param phy A phylsoeq object.
#' @param taxranks tax levels to collapse by. Default is \code{c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")}.
#' @param short_taxa_names How to name the collapsed OTUs. If \code{TRUE}, use name of first OTU plus number of OTUs being collapsed. If \code{FALSE}, paste the OTU names together.
#' @return A phyloseq object with OTUs collapsed.
#' @export
phy.collapse <- function(phy,taxranks=c("Superkingdom","Phylum","Class","Order","Family","Genus","Species"),short_taxa_names=TRUE) {
  requireNamespace(c("phyloseq","data.table"),quietly=TRUE)
  taxranks <- syms(taxranks)
  otudt <- as(phyloseq::otu_table(phy),"matrix") %>% data.table::data.table()
  taxdt = as(phyloseq::tax_table(phy,errorIfNULL=TRUE),"matrix") %>% data.table::data.table() %>% select(!!!taxranks)
  indices_ <- taxdt %>% group_indices(!!!taxranks)
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
  }),by=indices_] %>% pull(otu)
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





#' Prune Unused Taxa from a phyloseq object
#'
#' In a phyloseq object, remove any taxa that are not used in the samples.
#' @param phy phyloseq object
#'
#' @return a phyloseq object, with empty taxa removed.
#' @export
#' @examples
prune_unused_taxa <- function(phy) {
  requireNamespace("phyloseq",quietly=TRUE)
  keep <- taxa_sums(phy)>0
  message(str_glue("Prune unused taxa: {length(keep)} to {sum(keep)} taxa"))
  prune_taxa(keep,phy)
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

#' Read in tax file from blastn output
#'
#' This reads in the file created by the YT python script, blastn.py
#'
#' Chooses one taxonomy from the hits, also listing runner-up taxonomy.
#' qseqid: Query Seq-id
#' qgi: Query GI
#' qacc: Query accesion
#' qaccver: Query accesion.version
#' qlen: Query sequence length
#' sseqid: Subject Seq-id
#' sallseqid: All subject Seq-id(s), separated by a ';'
#' sgi: Subject GI
#' sallgi: All subject GIs
#' sacc: Subject accession
#' saccver: Subject accession.version
#' sallacc: All subject accessions
#' slen: Subject sequence length
#' qstart: Start of alignment in query
#' qend: End of alignment in query
#' sstart: Start of alignment in subject
#' send: End of alignment in subject
#' qseq: Aligned part of query sequence
#' sseq: Aligned part of subject sequence
#' evalue: Expect value. Describes the number of hits one can "expect" to see by chance when searching a database of a particular size.
#'   It decreases exponentially as the Score (S) of the match increases. Essentially, the E value describes the random background noise.
#'   For example, an E value of 1 assigned to a hit can be interpreted as meaning that in a database of the current size one might
#'   expect to see 1 match with a similar score simply by chance.
#' bitscore: Bit score
#' score: Raw score
#' length: Alignment length
#' pident: Percentage of identical matches
#' nident: Number of identical matches
#' mismatch: Number of mismatches
#' positive: Number of positive-scoring matches
#' gapopen: Number of gap openings
#' gaps: Total number of gaps
#' ppos: Percentage of positive-scoring matches
#' frames: Query and subject frames separated by a '/'
#' qframe: Query frame
#' sframe: Subject frame
#' btop: Blast traceback operations (BTOP)
#' staxid: Subject Taxonomy ID
#' ssciname: Subject Scientific Name
#' scomname: Subject Common Name
#' sblastname: Subject Blast Name
#' sskingdom: Subject Super Kingdom
#' staxids: unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)
#' sscinames: unique Subject Scientific Name(s), separated by a ';'
#' scomnames: unique Subject Common Name(s), separated by a ';'
#' sblastnames: unique Subject Blast Name(s), separated by a ';' (in alphabetical order)
#' sskingdoms: unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)
#' stitle: Subject Title
#' salltitles: All Subject Title(s), separated by a '<>'
#' sstrand: Subject Strand
#' qcovs: Query Coverage Per Subject
#' qcovhsp: Query Coverage Per HSP
#' qcovus: Query Coverage Per Unique Subject (blastn only)
#' @param tax Taxonomy data from blastn, either as the file or a data frame.
#' @param tax_table logical, if TRUE (default), will return a data frame of taxonomy, which can be directly converted to a phyloseq tax_table object. If FALSE, returns data frame with all hits and associated data.
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

#' Read in mothur taxonomy file.
#'
#' @param tax.file mothur taxonomy file to be read.
#' @return Returns a data frame containing taxonomy
#' @author Ying Taur
#' @export
read.mothur.taxfile <- function(tax.file) {
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


#' Read in oligos file.
#'
#' Reads in oligos file, listing pertinent information
#'
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





#' Highlight a clade in ggtree
#'
#' @param gdata a data frame from a ggtree object.
#' @param var the variable to be matched (unquoted).
#' @param value the value that \code{var} needed for inclusion in the clade to be highlighted.
#' @param criteria an expression that evaluates to logical, can use as an alternative (or in addition to) var/value,
#' @param ymin the min value of y in the clade.
#' @param ymax the max value of y in the clade.
#' @param label A \code{glue} expression for the clade label. Default is \code{"{value}\n({var})"}
#' @param fill.color the clade fill color (default is \code{NA}, no fill)
#' @param line.color the color of the outline around the clade (default \code{"dark gray"})
#' @param alpha the alpha value of the fill color (default 1)
#' @param xmax the x depth at which the edge of the clade is drawn (optional)
#' @param xscalar if \code{xmax} is not specified, the x depth can be specified as a scaled factor of maximum x of all clade tips. Default is \code{1.5}
#' @param fill.in whether to fill all gaps in the clade (default is \code{FALSE})
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


#' Get YT Palette
#' @param tax either a data.frame, phyloseq, or tax_table
#' @param use.cid.colors whether to use classic CID colors
#' @return a color palette that can be used in \code{ggplot2}
#' @examples
#' @author Ying Taur
#' @export
get.yt.palette <- function(tax,use.cid.colors=TRUE) {
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
  if (class(tax)[1] %in% c("phyloseq", "taxonomyTable")) {
    tax <- get.tax(tax.obj)
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

#' Plot tax
#'
#' @param t data frame containing melted tax data. Needs to have vars sample, pctseqs, Kingdom, ... , Species
#' @param xvar xvar by which to plot data
#' @param data whether to return data frame
#' @param label.pct.cutoff cutoff by which to label abundances, stored in tax.label
#' @param cid.colors whether to use conventional cid colors.
#' @return either ggplot2 object, or data frame.
#' @examples
#' @author Ying Taur
#' @export
tax.plot <- function(t,xvar="sample",data=FALSE,label.pct.cutoff=0.3,use.cid.colors=TRUE) {
  #t=get.otu.melt(phy.species)
  tax.levels <- c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")
  vars <- c("sample","pctseqs",tax.levels)
  if (!all(vars %in% names(t))) {
    missing.vars <- setdiff(vars,names(t))
    stop("YTError: missing var:",paste(missing.vars,collapse=","))
  }
  t <- t %>% arrange(Superkingdom,Phylum,Class,Order,Family,Genus,Species) %>%
    mutate(Species=fct_inorder(Species)) %>%
    group_by(sample) %>% arrange(Species) %>%
    mutate(cum.pct=cumsum(pctseqs),
           y.text=(cum.pct + c(0,cum.pct[-length(cum.pct)])) / 2,
           y.text=1-y.text) %>%
    ungroup() %>%
    select(-cum.pct) %>%
    mutate(tax.label=ifelse(pctseqs>=label.pct.cutoff,as.character(Species),""))
  pal <- get.yt.palette(t,use.cid.colors=use.cid.colors)
  attr(t,"pal") <- pal
  if (data) {
    return(t)
  } else {
    g <- ggplot() +
      geom_bar(data=t,aes_string(x=xvar,y="pctseqs",fill="Species"),stat="identity",position="fill") +
      geom_text(data=t,aes_string(x=xvar,y="y.text",label="tax.label"),angle=-90,lineheight=0.9) +
      scale_fill_manual(values=attr(t,"pal")) +
      theme(legend.position="none")
    return(g)
  }
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


#' LDA Effect Size
#'
#' Given a phyloseq object and class variable, perform LDA Effect Size analysis.
#' @param phy the phyloseq object containing the data
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
#' @param lda lda table from \code{lda.effect()}
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
#' @param lda lda table from \code{lda.effect()}
#' @param layout Either "cirular" or "rectangular"
#' @param check_overlap Only write clade labels if they do not overlap each other. Default is \code{TRUE}. This is passed to \code{geom_text}.
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

#' Conversion from Taxonomy Variables to Phylogenetic Trees (YT converted)
#'
#' Used to convert taxonomy table into a phylo object for plotting. A revised version of ape::as.phylo.formula.
#' Modified from a version from Liam J. Revell, University of Massachusetts, \link{https://stat.ethz.ch/pipermail/r-sig-phylo/2013-August/003017.html}
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




