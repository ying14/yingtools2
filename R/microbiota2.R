

#' The color scheme used in CID manuscript.
#' @author Ying Taur
#' @export
cid.colors <- c("Enterococcus"="#129246","Streptococcus"="#a89e6a","Blautia"="#f69ea0",
                "Bacteroides"="#2dbfc2","Lactobacillus"="#3b51a3","Dorea"="#a9853e",
                "Staphylococcus"="#f1eb25","Coprobacillus"="#b53572",
                "unclassified_Firmicutes"="#79449a","Lachnospiraceae"="#afd7db",
                "Roseburia"="#9ba744","Parabacteroides"="#329982","Coprococcus"="#663939",
                "Spracetigenium"="#72b443","Veillonella"="#653f99","Lactococcus"="#51a546",
                "Granulicatella"="#a5a7aa","Proteobacteria"="#ed2024","Other Bacteroides"="#963695",
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
  #replace ; with __
  new.tree.text <- gsub("(OTU_[0-9]+);(size=[0-9]+);","\\1__\\2__",tree.text)
  tr <- ape::read.tree(text=new.tree.text)
  tr$tip.label <- gsub("'(OTU_[0-9]+)__(size=[0-9]+)__'","\\1;\\2;",tr$tip.label)
  tr$node.label <- gsub("'(OTU_[0-9]+)__(size=[0-9]+)__'","\\1;\\2;",tr$node.label)
  return(tr)
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
#' @export
get.samp <- function(phy,stats=FALSE,measures=c("Observed","InvSimpson","Shannon")) {

  if (is.null(sample_data(phy,FALSE))) {
    #if no sample_data, return single data frame with sample column
    sdata <- data.frame(sample=sample_names(phy))
  } else {
    if ("sample" %in% sample_variables(phy)) {stop("YTError: phyloseq sample_data already contains the reserved variable name \"sample\"")}
    sdata <- sample_data(phy) %>% data.frame() %>% add_rownames("sample")
  }
  if (stats) {
    dup.names <- intersect(c("nseqs",measures),names(sdata))
    if (length(dup.names)>0) {
      sdata <- sdata[,setdiff(names(sdata),dup.names)]
      warning("YTWarning: Following variables are duplicated. Deleting old values from phyloseq: ",paste(dup.names,collapse=", "))
    }
    sdata$nseqs <- sample_sums(phy)
    sdata <- cbind(sdata,estimate_richness(phy,measures=measures))
  }
  return(sdata)
}


#' Convert data frame to phyloseq sample_data
#'
#' Use this on data with sample info. The opposite of function get.samp. Make sure it contains variable "sample"
#' @param sdata Data frame to be converted back to sample_data
#' @return formatted sample_data.
#' @export
set.samp <- function(sdata) {
  ss <- sdata %>% dplyr::select(-sample)
  row.names(ss) <- sdata[["sample"]]
  ss <- ss %>% sample_data()
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
  tax_table(phy) %>% data.frame() %>% add_rownames("otu")
}

#' Convert data frame to phyloseq tax_table
#'
#' Use this on data.frames with tax data. The opposite of get.tax function. Make sure it contains the variable "otu".
#' @param tdata dataframe to be converted back to tax_table.
#' @return formatted tax_table.
#' @export
set.tax <- function(tdata) {
  tt <- tdata %>% dplyr::select(-otu)
  row.names(tt) <- tdata[["otu"]]
  tt <- tt %>% as.matrix() %>% tax_table()
  return(tt)
}





#' Convert Phyloseq to Melted OTU x Sample Data
#'
#' Creates OTU+Sample-level data, using phyloseq object (ID=otu+sample)
#'
#' Essentially gives back the OTU table, in melted form, such that each row represents a certain OTU for a certain sample.
#' Adds sample and taxonomy table data as columns. Uses the following reserved varnames: otu, sample, numseqs, pctseqs.
#' Note that phyloseq has a similar function, \code{psmelt}, but that takes longer.
#'
#' @param phy phyloseq object containing sample data
#' @param filter.zero Logical, whether or not to remove zero abundances. Default \code{TRUE}.
#' @param sample_data Logical, whether or not to join with \code{sample_data}. Default \code{TRUE}.
#' @return Data frame melted OTU data
#' @export
get.otu.melt <- function(phy,filter.zero=TRUE,sample_data=TRUE) {
  #phy0=phy;phy=subset_taxa(phy0,taxa_names(phy0) %in% head(taxa_names(phy0),10))
  otu0 <- otu_table(phy) %>% as.matrix() %>% reshape2::melt(varnames=c("otu","sample"),value.name="numseqs") %>%
    dplyr::as_data_frame() %>% mutate(otu=as.character(otu),sample=as.character(sample))
  tax0 <- get.tax(phy)
  tax0.match <- dplyr::select(tax0,-otu)[match(otu0$otu,tax0$otu),]
  otu <- cbind(otu0,tax0.match) %>%
    group_by(sample) %>% mutate(pctseqs=prop.table(numseqs)) %>% ungroup() %>% dplyr::tbl_df()
  if (filter.zero) {
    otu <- otu %>% filter(numseqs>0)
  }
  #add sample data
  if (sample_data & !is.null(phyloseq::sample_data(phy,FALSE))) {
    samp0 <- get.samp(phy,stats=FALSE)
    otu <- otu %>% dplyr::left_join(samp0,by="sample")
  }
  return(otu)
}


#' Read in tax file from blastn output
#'
#' This reads in the file created by the YT python script, blastn.py
#'
#' Chooses one taxonomy from the hits, also listing runner-up taxonomy.
#'
#' @param tax Taxonomy data from blastn, either as the file or a data frame.
#' @param tax_table logical, if TRUE (default), will return a data frame of taxonomy, which can be directly converted to a phyloseq tax_table object. If FALSE, returns data frame with Default is fault, which provides more information.
#' @param blastn.data logical, if TRUE will include a blastn.data column containing extra info, such as runner-up hits.
#' @return Data from the blastn data file.
#' @author Ying Taur
#' @export
read.blastn.file <- function(tax.file,tax_table=TRUE,blastn.data=FALSE) {
  #tax.file="uparse/total.5.repset.fasta.blastn.refseq_rna.txt";tax_table=TRUE;blastn.data=FALSE
  t <- data.table::fread(tax.file,colClasses=c("sallgi"="character","staxids"="character")) %>% dplyr::tbl_df() %>%
    dplyr::mutate(taxonomy=gsub("\\[(superkingdom|phylum|class|order|family|genus|species)\\]","",taxonomy),
           staxid=as.numeric(sapply(strsplit(staxids,split=";"),first)),
           otu=paste0(qseqid,";"),
           otu.number=as.numeric(stringr::str_extract(otu,"(?<=OTU_)[0-9]+"))) %>%
    tidyr::separate(taxonomy,into=c("Kingdom","Phylum","Class","Order","Family","Genus","Species"),sep="\\|",remove=FALSE) %>%
    dplyr::group_by(otu) %>%
    dplyr::arrange(evalue,staxid) %>%
    dplyr::filter(!duplicated(taxonomy)) %>%
    dplyr::mutate(n.ties=sum(dense_rank(evalue)==1),
                  blast.data=paste0(Species," (eval=",evalue,",pid=",pident,")",collapse=";")) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(otu.number) %>%
    dplyr::select(otu,evalue,pident,Kingdom,Phylum,Class,Order,Phylum,Class,Order,Family,Genus,Species,n.ties,blast.data,everything())
  if (tax_table) {
    if (blastn.data) {
      tt <- t %>% dplyr::select(otu,Kingdom,Phylum,Class,Order,Phylum,Class,Order,Family,Genus,Species,blast.data)
    } else {
      tt <- t %>% dplyr::select(otu,Kingdom,Phylum,Class,Order,Phylum,Class,Order,Family,Genus,Species)
    }
    return(tt)
  } else {
    return(t)
  }
}



#' Read in oligos file.
#'
#' Reads in oligos file, listing pertinent information
#'
#' @param oligo.file oligo file to be read. If oligo.file is a directory, all oligo files will be read in.
#' @return Returns a data frame containing oligo information
#' @examples
#' ...examples.here....
#' @keywords keyword1 keyword2 ...
#' @author Ying Taur
#' @export
read.oligos <- function(oligo.file) {
  if (!file.exists(oligo.file)) {
    stop("YTError: file/directory not found: ",oligo.file)
  }
  if (dir.exists(oligo.file)) {
    oligo.files <- list.files(oligo.file,pattern="\\.oligos$",recursive=TRUE,full.names=TRUE)
    if (length(oligo.files)==0) {
      stop("YTError: no oligo files found in dir: ",oligo.file)
    }
    out <- dplyr::rbind_all(lapply(oligo.files,read.oligos))
    return(out)
  }

  d <- data.frame(line=scan(oligo.file,what=character(),sep="\n",quiet=TRUE),stringsAsFactors=FALSE)
  d$line0 <- d$line
  d$line <- sub("[\t ]+$","",d$line)
  d$n <- 1:nrow(d)
  d$info <- recode2(d$line,c("^(primer|forward)\t"="primer",
                             "^#?barcode\t"="barcode",
                             "^#"="comment"),regexp=TRUE,else.value="error")
  if (any(d$info=="error")) {
    stop("YTError: Did not understand one of the lines in the oligos file!\nFile: ",oligo.file,"\nLine: ",d$line[d$info=="error"][1])
  }
  p <- d %>% filter(info=="primer") %>%
    mutate(primer=sub("^(primer|forward)\t","",line))
  primer <- p$primer[1]
  b <- d %>% filter(info=="barcode") %>%
    mutate(group=sapply(strsplit(line,split="\t"),last),
           barcode.commented=substr(line,1,1)=="#",
           barcode=sapply(strsplit(line,split="\t"),function(x) paste(x[c(-1,-length(x))],collapse="\t")))
  cmt <- d %>% filter(info=="comment")
  pool <- str_extract(oligo.file,"pool[^/]+(?=\\.oligos)")
  if (is.na(pool)) {
    stop("YTError: Could not extract pool number from file!")
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
    dplyr::select(pool,group,primer,barcode,oligo.file,platform,barcode.commented,comment,line=line0)
  return(out)
}


#' Right Angle for Text on circular ggtree
#'
#' @param angle
#' @param hjust logical indicating whether to output the corresponding hjsut parameter.
#' @return angle, turned 90 degrees.
#' @export
right.angle <- function(angle,hjust=FALSE) {
  #ranges 0 to 360 no matter what
  angle <- angle %% 360
  right.side <- angle>=0 & angle<180
  if (hjust) {
    ifelse(right.side,1,0)
  } else {
    ifelse(right.side,angle-90,angle+90)
  }
}



#' Get YT Palette
#' @param tax either a data.frame, phyloseq, or tax_table
#' @param use.cid.colors whether to use classic CID colors
#' @return a color palette that can be used in \code{ggplot2}
#' @examples
#' ...examples.here....
#' @author Ying Taur
#' @export
get.yt.palette <- function(tax,use.cid.colors=TRUE) {
  if (class(tax)[1] %in% c("phyloseq","taxonomyTable")) {
    tax <- get.tax(tax.obj)
  }
  ranks <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
  if (!all(ranks %in% names(tax))) {
    stop("YTError: need to have taxon levels: Kingdom, Phylum, Class, Order, Family, Genus, Species")
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
  #cid
  if (use.cid.colors) {
    cid <- cid.colors[match(tax.dict$Genus,names(cid.colors))]
    tax.dict$color <- ifelse(is.na(cid),tax.dict$color,cid)
  }
  tax.palette <- structure(tax.dict$color,names=as.character(tax.dict$Species))
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
#' ...examples.here....
#' @author Ying Taur
#' @export
tax.plot <- function(t,xvar="sample",data=FALSE,label.pct.cutoff=0.3,use.cid.colors=TRUE) {
  #t=get.otu.melt(phy.species)
  vars <- c("sample","pctseqs","Kingdom","Phylum","Class","Order","Family","Genus","Species")
  if (!all(vars %in% names(t))) {
    stop("YTError: need to have these vars: sample, pctseqs, Kingdom, Phylum, Class, Order, Family, Genus, Species")
  }
  t <- t %>% arrange(Kingdom,Phylum,Class,Order,Family,Genus,Species) %>%
    mutate(Species=factor(Species,levels=unique(Species))) %>%
    group_by(sample) %>% arrange(Species) %>%
    mutate(cum.pct=cumsum(pctseqs),
           y.text=(cum.pct + c(0,cum.pct[-length(cum.pct)])) / 2) %>% ungroup() %>% dplyr::select(-cum.pct) %>%
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
#' xxx
#' @keywords keyword1 keyword2 ...
#' @author Ying Taur
#' @export
pca.plot <- function(dist,data=FALSE,prefix=NA) {
  pca <- prcomp(dist)
  pca.axes <- data.frame(pca$x)
  pca.loadings <- summary(pca)$importance["Proportion of Variance",]
  pca.labels <- paste0(sub("PC","PCA",names(pca.loadings))," (",percent(pca.loadings)," variation explained)")
  for (i in 1:length(pca.labels)) {
    label(pca.axes[,i]) <- pca.labels[i]
  }
  pca.axes$group <- row.names(pca.axes)

  if (data) {
    names(pca.axes) <- sub("^PC",paste2(prefix,"PCA",sep="."),names(pca.axes))
    return(pca.axes)
  } else {
    g <- ggplot(pca.axes) +
      geom_point(aes(x=PC1,y=PC2,color=group,size=3)) +
      geom_text(aes(x=PC1,y=PC2,label=group),size=3,vjust=1.4) +
      theme(aspect.ratio=1)
    return(g)
  }
}

# #' Simpson's diversity
# #' @export
# simpson.diversity <- function(pcts) {
#   if (sum(pcts)-1>.Machine$double.eps) stop("Error, pcts don't add to 1... sum(pcts)=",sum(pcts))
#   sum(pcts^2)
# }
#
#
# #' Inverse Simpson diversity
# #' @export
# invsimpson.diversity <- function(pcts) {
#   1/simpson.diversity(pcts)
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
# read.pca <- function(pcafile,name,addto,keep=3) {
#   #reads in pca file (with loadings) and returns sample dataframe with group identifier.
#   #keep is number of axes to keep.
#   #name for pca axis is as: pca__bray__1__10.214159
#   headers <- as.vector(as.matrix(read.delim(pcafile, header=FALSE, nrows=1)))
#   headers <- c(headers,NA) #for some reason there is one less header than data columns.
#   p <- read.delim(pcafile, header=FALSE, skip=1, col.names=headers)
#   loadingsfile <- sub(".axes",".loadings",pcafile)
#   l <- read.delim(loadingsfile)
#   #rename axes to pca__bray__1__10.214159
#   new.names <- paste("pca",name,l$axis,l$loading, sep="__")
#   names(p)[grepl("axis",names(p))] <- new.names
#   p <- subset(p,select=1:(keep+1))
#   if (!missing(addto)) {
#     p <- merge(p,addto,by="group",all=TRUE)
#   }
#   return(p)
# }
#
#
#

#

#
#
# #' Filter Distance Matrix
# #'
# #' Creates subset of distance matrix.
# #'
# #' @param dist distance matrix to be subsetted.
# #' @param sdata data frame of sample data. Rows should be samples, and should contain data by which subsetting is to be done, and sample identifier (\code{sdata$group}) which correspondonds to row and column names of the distance matrix.
# #' @param ... subsetting arguments. These are passed on to the \code{filter} function in \code{dplyr}
# #' @param sample.id.name character corresponding to the name of the variable containing sample ID. This variable should contain the row and column names of the distance matrix.
# #' @return Returns a subset of \code{dist}.
# #' @examples
# #' ...examples.here....
# #' @keywords keyword1 keyword2 ...
# #' @author Ying Taur and Eric Littmann
# #' @export
# filter.dist <- function(dist,sdata, ..., sample.id.name="group") {
#   #dist=unifrac.dist;sdata=s.s0
#   #s.sub <- filter(sdata, SCType=="Autologous",SampleType=="Buccal Swab")
#   s.sub <- filter(sdata, ...)
#   mat <- as.matrix(dist)
#   samp.subset.list <- s.sub[,sample.id.name]
#   if (!all(samp.subset.list %in% colnames(mat))) {
#     stop("YTError: names in sdata and distance don't match!")
#   }
#   as.dist(mat[samp.subset.list,samp.subset.list])
# }
#
#

#
# #' The color scheme used in CID manuscript (basics only).
# #' @author Ying Taur
# #' @export
# cid.colors.basic <- c("Enterococcus"="#129246","Streptococcus"="#a89e6a","Blautia"="#f69ea0",
#                       "Lactobacillus"="#3b51a3","Staphylococcus"="#f1eb25")
#
#
# #' LEfSe
# #'
# #' Run LEfSe (LDA Effect Size) analysis using a phyloseq object.
# #'
# #' This function performs the analysis using the following steps.
# #' (1) Creates lefse.txt from phyloseq data, a tab-delimited file in the format input required by LEfSe.
# #' (2) Executes format_input.py to further format lefse.txt into lefse.in
# #' (3) Executes run_lefse.py, which does the actual analysis and produces lefse.res.
# #' (4) Executes plot_res.py and plot_cladogram.py, which create the graphics for LEfSe.
# #' Note that
# #' @param phy the phyloseq object containing data
# #' @param class variable to be tested by LEfSe. This must be a variable in sample_data(phy)
# #' @param subclass variable to perform subclass testing. This step is skipped if it is not specified.
# #' @param subject variable referring to the subject level designation. This is only necessary if multiple samples per subject.
# #' @param anova.alpha alpha level of the kruskal-wallis testing. Default is 0.05
# #' @param wilcoxon.alpha alpha level at which to perform wilcoxon testing of subclass testing. Default is 0.05.
# #' @param lda.cutoff Cutoff LDA to be reported. Default is 2.0.
# #' @param wilcoxon.within.subclass Set whether to perform Wilcox test only among subclasses with the same name (Default FALSE)
# #' @param one.against.one for multiclass tasks, sets whether testing is performed one-against-one (TRUE - more strict) or one-against-all (FALSE - less strict)
# #' @param levels Taxonomic levels to be tested. Default is to test all levels: rank_names(phy)
# #' @return Returns data
# #' @examples
# #' lefse.tbl <- lefse(ph,class="CDI",subclass="Sex")
# #' @author Ying Taur
# #' @export
# lefse <- function(phy,class,subclass=NA,subject=NA,
#                   anova.alpha=0.05,wilcoxon.alpha=0.05,lda.cutoff=2.0,
#                   wilcoxon.within.subclass=FALSE,one.against.one=FALSE,
#                   make.lefse.plots=FALSE,
#                   levels=rank_names(phy)) {
#   #phy=ph.engraft;class="CDI";subclass=NA;subject=NA;anova.alpha=0.05;wilcoxon.alpha=0.05;lda.cutoff=2.0;wilcoxon.within.subclass=FALSE;one.against.one=FALSE;levels=rank_names(phy)
#   keepvars <- unique(c(class,subclass,subject,"sample"))
#   keepvars <- keepvars[!is.na(keepvars)]
#   samp <- get.samp(phy)[,keepvars]
#
#   tt <- get.tax(phy)
#   tt.prelefse <- tt
#   tt.dict <- data.frame()
#   for (lvl in 1:length(levels)) {
#     #lvl=3
#     ranks <- levels[1:lvl]
#     rank <- levels[lvl]
#     tt.prelefse[,rank] <- apply(data.frame(tt[,ranks]),1,function(row) {
#       row <- gsub("[|.]","",row) #remove separators . or |
#       row <- gsub(" ","_",row) #spaces are deleted, so instead change to _ (this is optional)
#       paste(row,collapse="|")
#     })
#     tt.dict <- bind_rows(tt.dict,data.frame(rank,taxon=tt[,rank],prelefse.fulltaxon=tt.prelefse[,rank]))
#   }
#
#   tax_table(phy) <- tt.prelefse %>% set.tax()
#   tt.dict <- tt.dict %>% distinct() %>%
#     mutate(postlefse.fulltaxon=gsub("\\|",".",prelefse.fulltaxon),
#            postlefse.fulltaxon=gsub("(\\[|\\]|-)","_",postlefse.fulltaxon), #these are changes lefse makes to the names
#            postlefse.fulltaxon=gsub(" ","",postlefse.fulltaxon)) %>%
#     separate(prelefse.fulltaxon,into=levels,sep="\\|",remove=FALSE,extra="drop")
#   for (i in 1:length(levels)) {
#     lvls <- levels[1:i]
#     lvl <- levels[i]
#     lvl0 <- paste0(levels[i],"0")
#     tt.dict[,lvl0] <- apply(tt.dict[,lvls],1,function(row) {
#       paste2(row,collapse="|")
#     })
#     tt.dict[is.na(tt.dict[,lvl]),lvl0] <- NA
#   }
#   tax <- get.otu.melt(phy,sample_data=FALSE)
#   tax.tbl <- tax %>% melt(measure.vars=levels,value.name="taxon",variable.name="level") %>%
#     dcast(sample~taxon,value.var="pctseqs",fill=0,fun.aggregate=sum)
#   tbl <- samp %>% left_join(tax.tbl,by="sample")
#   if (is.na(subject) | subject!="sample") {
#     tbl <- tbl %>% select(-sample)
#   }
#   tbl <- tbl %>% t()
#   write.table(tbl,"lefse.txt",quote=FALSE,sep="\t",col.names=FALSE)
#   opt.class <- paste("-c",which(keepvars %in% class))
#   opt.subclass <- ifelse(is.na(subclass),"",paste("-s",which(keepvars %in% subclass)))
#   opt.subject <-ifelse(is.na(subject),"",paste("-u",which(keepvars %in% subject)))
#   format.command <- paste("format_input.py lefse.txt lefse.in",opt.class,opt.subclass,opt.subject,"-o 1000000")
#   system(format.command)
#   #   -m {f,s}              set the policy to adopt with missin values: f removes
#   #   the features with missing values, s removes samples
#   #   with missing values (default f)
#   #   -n int                set the minimum cardinality of each subclass
#   #   (subclasses with low cardinalities will be grouped
#   #   together, if the cardinality is still low, no pairwise
#   #   comparison will be performed with them)
#   lefse.command <- paste("run_lefse.py lefse.in lefse.res","-a",anova.alpha,"-w",wilcoxon.alpha,"-l",lda.cutoff,"-e",as.numeric(wilcoxon.within.subclass),"-y",as.numeric(one.against.one))
#   system(lefse.command)
#   print("Wrote lefse.res")
#   lefse.out <- read.table("lefse.res",header=FALSE,sep="\t") %>% rename(postlefse.fulltaxon=V1,log.max.pct=V2,direction=V3,lda=V4,p.value=V5)
#   #   -a float        set the alpha value for the Anova test (default 0.05)
#   #   -w float        set the alpha value for the Wilcoxon test (default 0.05)
#   #   -l float        set the threshold on the absolute value of the logarithmic
#   #   LDA score (default 2.0)
#   #   --nlogs int     max log ingluence of LDA coeff
#   #   --verbose int   verbose execution (default 0)
#   #   --wilc int      wheter to perform the Wicoxon step (default 1)
#   #   -r str          select LDA or SVM for effect size (default LDA)
#   #   --svm_norm int  whether to normalize the data in [0,1] for SVM feature
#   #   waiting (default 1 strongly suggested)
#   #   -b int          set the number of bootstrap iteration for LDA (default 30)
#   #   -e int          set whether perform the wilcoxon test only among the
#   #   subclasses with the same name (default 0)
#   #   -c int          set whether perform the wilcoxon test ing the Curtis's
#   #                   approach [BETA VERSION] (default 0)
#   #   -f float        set the subsampling fraction value for each bootstrap
#   #                   iteration (default 0.66666)
#   #   -s {0,1,2}      set the multiple testing correction options. 0 no correction
#   #                   (more strict, default), 1 correction for independent
#   #                   comparisons, 2 correction for independent comparison
#   #   --min_c int     minimum number of samples per subclass for performing
#   #                   wilcoxon test (default 10)
#   #   -t str          set the title of the analysis (default input file without
#   #                   extension)
#   #   -y {0,1}        (for multiclass tasks) set whether the test is performed in
#   #                   a one-against-one ( 1 - more strict!) or in a one-against-
#   #                   all setting ( 0 - less strict) (default 0)
#
#   if (make.lefse.plots) {
#     system("plot_res.py lefse.res lefse_lda.png")
#     print("Wrote lefse_lda.png")
#     system("plot_cladogram.py lefse.res lefse_clado.pdf --format pdf")
#     print("Wrote lefse_clado.pdf")
#   }
#
#   lefse.mismatches <- setdiff(lefse.out$full.taxon,tt.dict$postlefse)
#   if (length(lefse.mismatches)>0) {
#     stop("YTError: items from LEfSE output don't match! Check the function.")
#   }
#   lefse.fulltable <- tt.dict %>% left_join(lefse.out,by="postlefse.fulltaxon") %>% mutate(lefse=!is.na(log.max.pct)) %>% rename(label=prelefse.fulltaxon)
#   #return(lefse.fulltable)
#   #phy.formula <- as.formula(paste0("~",paste(levels,collapse="/")))
#   #tt.phylo.df <- tt.prelefse %>% select(-otu) %>% distinct()
#   #tt.phylo <- as.phylo.formula2(phy.formula,data=tt.phylo.df)
#   #if (anyDuplicated(c(tt.phylo$tip.label,tt.phylo$node.label))) {
#   #  stop("YTError: Duplicate labels in phylo tree!")
#   #}
#   #return(list(tbl=lefse.fulltable,phylo=tt.phylo))
#   return(lefse.fulltable)
# }
#
# #' Conversion from Taxonomy Variables to Phylogenetic Trees (YT converted)
# #'
# #' Used to convert taxonomy table into a phylo object for plotting. A revised version of ape::as.phylo.formula.
# #' Modified from a version from Liam J. Revell, University of Massachusetts, \link{https://stat.ethz.ch/pipermail/r-sig-phylo/2013-August/003017.html}
# #'
# #' Here are the changes:
# #' (1) Corrected branch lengths. The original ape::as.phylo.formula does not work well because it uses ape::read.tree to read in data,
# #' which can only read Newick trees without singleton branches. This uses read.newick instead, which can handle the singleton branches.
# #' (2) Single branches are not automatically collapsed. This is so you can plot all levels of taxonomy.
# #' (3) All nodes are labeled.
# #' @param x a right-side formula describing the taxonomic relationship: ~C1/C2/.../Cn.
# #' @param data the data.frame where to look for the variables (default to environment).
# #' @param collapse.singles whether or not to collapse singleton nodes. Default is FALSE.
# #' @param ... further arguments to be passed from other methods.
# #' @return An object of class phylo.
# #' @examples
# #' #levels are not same level.
# #' data(carnivora)
# #' t1 <- ape::as.phylo.formula(~SuperFamily/Family/Genus/Species, data=carnivora)
# #' par(lend=2)
# #' plot(t1,edge.width=2,cex=0.6,no.margin=TRUE)
# #' #this is correct.
# #' t2 <- as.phylo.formula2(~SuperFamily/Family/Genus/Species, data=carnivora)
# #' par(lend=2)
# #' plot(t2,edge.width=2,cex=0.6,no.margin=TRUE)
# #' @author Ying Taur
# #' @export
# as.phylo.formula2 <- function (x, data = parent.frame(), collapse.singles=FALSE, distinct.tree=TRUE, full.taxonomy.only=TRUE, ...){
#   #data=lefse.results;x=~Kingdom/Phylum/Class/Order/Family/Genus/Species  ;collapse.singles=FALSE; distinct.tree=TRUE; full.taxonomy.only=TRUE
#   err <- "Formula must be of the kind \"~A1/A2/.../An\"."
#   if (length(x) != 2)
#     stop(err)
#   if (x[[1]] != "~")
#     stop(err)
#   f <- x[[2]]
#   taxo <- list() #list of vectors, from species->phylum
#   while (length(f) == 3) {
#     if (f[[1]] != "/")
#       stop(err)
#     #if (!is.factor(data[[deparse(f[[3]])]]))
#     #  stop(paste("Variable", deparse(f[[3]]), "must be a factor."))
#     taxo[[deparse(f[[3]])]] <- data[[deparse(f[[3]])]]
#     if (length(f) > 1)
#       f <- f[[2]]
#   }
#   #if (!is.factor(data[[deparse(f)]]))
#   #  stop(paste("Variable", deparse(f), "must be a factor."))
#   #f=Kingdom
#   taxo[[deparse(f)]] <- data[[deparse(f)]]
#   taxo.data <- as.data.frame(taxo) #tax data from species>kingdom
#   if (distinct.tree) {
#     taxo.data <- taxo.data %>% distinct()
#   }
#   if (full.taxonomy.only) {
#     taxo.data <- taxo.data[!is.na(taxo.data[,1]),]
#   }
#   leaves.names <- as.character(taxo.data[, 1]) #species
#   taxo.data[, 1] <- 1:nrow(taxo.data) #replace species with node numbers
#   f.rec <- function(subtaxo) {
#     #subtaxo=taxo.data
#     u <- ncol(subtaxo) #number of ranks
#     levels <- unique(subtaxo[, u]) #last column (bacteria,...,genus)
#     if (u == 1) {
#       if (length(levels) != nrow(subtaxo))
#         warning("Error, leaves names are not unique.")
#       return(as.character(subtaxo[, 1]))
#     }
#     t <- character(length(levels))
#     for (l in 1:length(levels)) { #for each taxon of the level
#       #l=1
#       x <- f.rec(subtaxo[subtaxo[, u] == levels[l], ][1:(u - 1)]) #subset of taxo.data for that level.
#       #modified this so that node labels are written.
#       #t[l] <- paste("(", paste(x, collapse = ","), ")", sep = "")
#       t[l] <- paste("(", paste(x, collapse = ","), ")",levels[l], sep = "")
#     }
#     return(t)
#   }
#   string <- paste("(", paste(f.rec(taxo.data), collapse = ","),");", sep = "")
#   phy <- read.newick(text = string) ## so that singles will be read without error
#   phy$edge.length <- rep(1,nrow(phy$edge))
#   if (collapse.singles) {
#     phy <- collapse.singles(phy)
#   }
#   phy$tip.label <- leaves.names[as.numeric(phy$tip.label)]
#   return(phy)
# }
#
#
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
# convert.oligo.to.mappingfile <- function(oligo) {
#   d <- read.delim(oligo,sep="\t",header=FALSE)
#   if (length(d)==4) {
#     d <- d[,-3] #implies miseq, remove 3rd column which is useless
#   }
#   names(d) <- c("row","code","group")
#   primer <- d$code[grepl("forward|primer",d$row,ignore.case=TRUE)]
#   d <- subset(d,grepl("^barcode$",row,ignore.case=TRUE))
#   for (i in 1:length(primer)) {
#     p <- primer[i]
#     map <- data.frame(d$group,d$code,p,d$group)
#     names(map) <- c("#SampleID","BarcodeSequence","LinkerPrimerSequence","Description")
#     if (length(primer)>1) {
#       map.filename <- sub("\\.oligos$",paste0(".",i,".map.txt"),oligo)
#     } else {
#       map.filename <- sub("\\.oligos$",".map.txt",oligo)
#     }
#     write.table(map,map.filename,quote=FALSE,row.names=FALSE,sep="\t")
#     cat("Wrote file: ",map.filename,"\n")
#   }
#
# }
#
#
# # #' Read Alpha Diversity File
# # #'
# # #' @param alpha.file name of diversity file. May be named "final.alpha.summary"
# # #' @param label character, specifies what "label" to obtain. The summary file may list different OTU bin sizes, e.g. 0.03, 0.01, unique, etc. Default is 0.03.
# # #' @param addto you can optionally specify a dataframe, which will be merged with alpha diversity data. This is typically "s" data, where each row represents a sample, and where s$group is the unique identifier.
# # #' @return Dataframe containing alpha diversity data.
# # #' @examples
# # #' ...examples.here....
# # #' @keywords keyword1 keyword2 ...
# # #' @seealso \code{\link{cdiff.method}}
# # #' @author Ying Taur
# # #' @export
# # read.alpha <- function(alpha.file,label="0.03",addto) {
# #
# #   #alpha.file = "/home/ying14/Desktop/test1/mothur_onepool_allseqs_gg99_p8/final/final.alpha.summary"
# #   #alpha.file = "/home/ying14/Desktop/test1/qiime_onepool_allseqs_gg99_p8/otus/alpha_diversity.txt"
# #   a <- read.table(alpha.file,header=TRUE,as.is=TRUE)
# #   if ("group" %in% names(a)) { #mothur
# #     if (!"label" %in% names(a) | !label %in% a$label) {
# #       stop(paste0("YTError: ",label," not found within label column"))
# #     }
# #     a$label <- as.character(a$label)
# #     a <- select(a,-label)
# #   } else { #qiime
# #     a$group <- row.names(a)
# #     row.names(a) <- NULL
# #     alpha.recodes <- c("chao1"="chao","simpson_reciprocal"="invsimpson","simpson_e"="shannoneven",
# #                        "observed_species"="sobs","goods_coverage"="coverage")
# #     names(a) <- recode2(names(a),recodes=alpha.recodes)
# #   }
# #   if (!missing(addto)) {
# #     a <- merge(a,addto,by="group",all=TRUE)
# #   }
# #   return(a)
# # }
# #
# #
# # read.alpha.old <- function(alphafile,addto) {
# #   #read in alpha diversity summary file.
# #   a <- read.delim(alphafile,as.is=TRUE)
# #   a <- a[,!sapply(a,function(x) all(is.na(x)))] #get rid of NA column at end
# #   a <- subset(a,select=-label) #get rid of label=0.03 column at beginning
# #   if (!missing(addto)) {
# #     a <- merge(a,addto,by="group",all=TRUE)
# #   }
# #   return(a)
# # }
# #
# #
# # read.tax <- function(taxfile,addto) {
# #   t <- read.delim(taxfile,check.names=FALSE)
# #   t$taxon <- sub("[kpcofgs]__","",t$taxon) #get rid of leading letter, if there
# #   maxlvl <- max(t$taxlevel)
# #   row.names(t) <- paste("tax",t$taxon,t$taxlevel,t$rankID,sep="__")
# #   #get rid of NA column at end, and first row.
# #   t <- t[,sapply(t,function(x) all(!is.na(x)))]
# #   #get rid of text columns
# #   t <- subset(t,select=c(-daughterlevels,-total,-taxlevel,-taxon,-rankID))
# #   #convert to matrix, transpose, convert back
# #   t <- as.data.frame(t(as.matrix(t)))
# #   #create groups column
# #   t$group <- row.names(t)
# #   for (lvl in 2:maxlvl) {
# #     dom.varname <- paste("dom","taxon",lvl,sep=".")
# #     t[,dom.varname] <- get.dominating.taxon(t,lvl)
# #     dom.amt.varname <- paste("dom","amount",lvl,sep=".")
# #     t[,dom.amt.varname] <- get.dominating.amount(t,lvl)
# #   }
# #   #percents
# #   t <- calculate.percent.taxa(t)
# #   #taxvars <- tax.subset(t)
# #   #pctvars <- as.data.frame(sapply(taxvars,function(x) x / taxvars$tax__Bacteria__1__0.1))
# #   #names(pctvars) <- gsub("tax__","pct__",names(pctvars))
# #   #t <- data.frame(t,pctvars)
# #   if (!missing(addto)) {
# #     t <- merge(t,addto,by="group",all=TRUE)
# #   }
# #   return(t)
# # }
# #
# # read.tax.simple <- function(taxfile) {
# #   t <- read.delim(taxfile,stringsAsFactors=FALSE)
# #   t <- t[,sapply(t,function(x) all(!is.na(x)))]
# #   max.level <- max(t$taxlevel)
# #   t.new <- subset(t,taxlevel==max.level)
# #   rank <- t.new$rankID
# #   lvlnames <- c()
# #   for (lvl in max.level:1) {
# #     newname <- paste0("level",lvl)
# #     lvlnames <- c(lvlnames,newname)
# #     t.new[,newname] <- t[match(rank,t$rankID),"taxon"]
# #     rank <- gsub("\\.[0-9]{1,3}$","",rank)
# #   }
# #   t.new <- subset(t.new,select=c(-taxlevel,-taxon,-daughterlevels,-rankID))
# #   pcts <- t.new[,sapply(t.new,is.numeric)]
# #   pcts <- data.frame(lapply(pcts,prop.table),stringsAsFactors=FALSE)
# #   names(pcts) <- paste("pct.",names(pcts),sep="")
# #   t.new <- cbind(t.new,pcts)
# #   t.new <- t.new[,c(rev(lvlnames),setdiff(names(t.new),lvlnames))]
# #   return(t.new)
# # }
# #
# # read.taxonomy <- function(taxfile) {
# #   #reads in taxonomy file (not the summary)
# #   #taxfile="bot95.rdp6.wang.taxonomy";name="rdp6"
# #   tax <- read.delim(taxfile,header=FALSE,sep="\t",as.is=TRUE,col.names=c("header","taxline"))
# #   taxons <- data.frame(do.call(rbind,strsplit(tax$taxline,split=";")),stringsAsFactors=FALSE)
# #   tax.levels <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
# #   names(taxons) <- tax.levels[1:length(taxons)]
# #   tax <- cbind(tax,taxons)
# #   tax <- subset(tax,select=-taxline)
# #   return(tax)
# # }
# #
# # write.taxonomy <- function(taxdata,file) {
# #   #format is header, then phylogeny
# #   tax.levels <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
# #   phylogeny <- taxdata[,tax.levels]
# #   phylogeny <- adply(phylogeny,1,function(x) data.frame(taxline=paste0(paste(x,collapse=";"),";"),stringsAsFactors=FALSE))
# #   finaldata <- data.frame(taxdata$header,phylogeny$taxline)
# #   write.table(finaldata,file,sep="\t",quote=FALSE,col.names=FALSE,row.names=FALSE)
# # }
# #
# #
# #
# #
# # calculate.percent.taxa <- function(sdata) {
# #   #given sdata file, calculate percent data.
# #   #erase pct data, if any exist.
# #   sdata <- sdata[,!(names(sdata) %in% pct.subset(names(sdata)))]
# #   #identify taxvar columns
# #   taxvars <- tax.subset(sdata)
# #   #for each row, divide by tax_Bacteria column
# #   pctvars <- as.data.frame(lapply(taxvars,function(x) x / taxvars$tax__Root__0__0))
# #   #pctvars <- as.data.frame(sapply(taxvars,function(x) x / taxvars$tax__Bacteria__1__0.1))
# #   names(pctvars) <- gsub("tax__","pct__",names(pctvars))
# #
# #   sdata <- data.frame(sdata,pctvars)
# #   return(sdata)
# # }
# #
# #
# # #############
# # #### PCA ####
# # #############
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
# # pca.column <- function(sdata,name,axis) {
# #   #returns name of pca column with name and axis.
# #   #e.g. pca(s,"bray",1)
# #   pattern <- paste("^pca",name,axis,sep="__")
# #   #takes first match of "pca__bray__1" or whatever
# #   #erase if working::#names(sdata)[which(grepl(pattern,names(sdata)))[1]]
# #   grep(pattern,names(sdata),value=TRUE)[1]
# # }
# #
# #
# # pca.var <- function(pca.varname) {
# #   #given pca varname, returns variance explained
# #   variance <- as.numeric(unlist(strsplit(pca.varname,split="__"))[4])
# #   return(variance)
# # }
# #
# #
# # pca.plot <- function(sdata,name,pca.y,pca.x,text,color) {
# #   #e.g. pca.plot(s,"bray",1,2)
# #   title <- paste("PCA plot:",name)
# #   xcolumn <- pca.column(sdata,name,pca.x)
# #   ycolumn <- pca.column(sdata,name,pca.y)
# #   xvariance <- round(pca.var(xcolumn),1)
# #   yvariance <- round(pca.var(ycolumn),1)
# #   xlabel <- paste("PCA",pca.x," (",xvariance,"%)",sep="")
# #   ylabel <- paste("PCA",pca.y,"(",yvariance,"%)",sep="")
# #   g <- ggplot(sdata,aes_string(x=xcolumn,y=ycolumn)) +
# #     scale_y_continuous(name=ylabel) +
# #     scale_x_continuous(name=xlabel) +
# #     opts(title=title,aspect.ratio=1)
# #   if (!missing(text)) {
# #     g <- g + geom_text(aes_string(label=text,vjust=-0.75),size=3)
# #   }
# #   if (missing(color)) {
# #     g <- g + geom_point(size=3)
# #   } else {
# #     g <- g + geom_point(size=3, aes_string(color=color))
# #   }
# #   return(g)
# # }
# #
# #
# # #################
# # ### tax tools ###
# # #################
# #
# # tax.name <- function(taxname) {
# #   #given varname (scalar or vector), return taxname.
# #   #e.g. tax__Enterococcus__6__0.1.15.1.2.3.2 -> Enterococcus
# #   taxname <- as.character(taxname)
# #   tax.list=strsplit(taxname,split="__")
# #   unlist(lapply(tax.list,function(x) x[[2]]))
# # }
# #
# # tax.level <- function(taxname) {
# #   #given varname (scalar or vector), return taxname.
# #   #e.g. tax__Enterococcus__6__0.1.15.1.2.3.2 -> 6
# #   taxname <- as.character(taxname)
# #   tax.list=strsplit(taxname,split="__")
# #   as.integer(unlist(lapply(tax.list,function(x) x[[3]])))
# # }
# #
# # as.taxlevel.factor <- function(lvl) {
# #   #convert number to factor
# #   factor(lvl,levels=1:7,labels=c("Kingdom(1)","Phylum(2)","Class(3)","Order(4)","Family(5)","Genus(6)","Species(7)"),ordered=TRUE)
# # }
# #
# # tax.rank <- function(taxname) {
# #   #given varname (scalar or vector), return rankID.
# #   #e.g. tax__Enterococcus__6__0.1.15.1.2.3.2 -> 0.1.15.1.2.3.2
# #   taxname <- as.character(taxname)
# #   tax.list=strsplit(taxname,split="__")
# #   unlist(lapply(tax.list,function(x) x[[4]]))
# # }
# #
# #
# # tax.var <- function(taxon,level,sdata) {
# #   #given name/level, return taxname.
# #   #e.g. Enterococcus, 6 -> tax__Enterococcus__6__0.1.15.1.2.3.2
# #   #returns first match, in case there are duplicates
# #   pattern <- paste("^tax",taxon,level,"[0-9.]+$",sep="__")
# #   ##erase if not needed: names(sdata)[grepl(pattern,names(sdata))][1]
# #   grep(pattern,names(sdata),value=TRUE)[1]
# # }
# #
# # pct.var <- function(taxon,level,sdata) {
# #   #given name/level, return pct_taxname.
# #   #e.g. Enterococcus, 6 -> pct__Enterococcus__6__0.1.15.1.2.3.2
# #   #returns first match, in case there are duplicates
# #   pattern <- paste("^pct",taxon,level,"[0-9.]+$",sep="__")
# #   ##erase if not needed: names(sdata)[grepl(pattern,names(sdata))][1]
# #   grep(pattern,names(sdata),value=TRUE)[1]
# # }
# # #' ...Title...
# #
# #
# # tax.subset <- function(x,...) UseMethod("tax.subset")
# #
# # tax.subset.character <- function(snames,level) {
# #   #given vector of names, return those matching taxons of specified level.
# #   if (missing(level)) {
# #     pattern <- paste("^tax",".+","[0-9]","[0-9.]+$",sep="__")
# #   } else {
# #     pattern <- paste("^tax",".+",level,"[0-9.]+$",sep="__")
# #   }
# #   ##erase if not needed: snames[grepl(pattern,snames)]
# #   grep(pattern,snames,value=TRUE)
# # }
# #
# # tax.subset.data.frame <- function(sdata,level,keep) {
# #   #given sample data, return subset of taxons
# #   #keep is extra varnames to hold onto.
# #   taxnames <- tax.subset(names(sdata),level)
# #   if (!missing(keep)) {
# #     taxnames <- c(keep,taxnames)
# #   }
# #   sdata[,taxnames]
# # }
# #
# # pct.subset <- function(x,...) UseMethod("pct.subset")
# #
# # pct.subset.character <- function(snames,level) {
# #   #given vector of names, return those matching taxons of specified level.
# #   if (missing(level)) {
# #     pattern <- paste("^pct",".+","[0-9]","[0-9.]+$",sep="__")
# #   } else {
# #     pattern <- paste("^pct",".+",level,"[0-9.]+$",sep="__")
# #   }
# #   ##erase if not needed: snames[grepl(pattern,snames)]
# #   grep(pattern,snames,value=TRUE)
# # }
# #
# # pct.subset.data.frame <- function(sdata,level,keep) {
# #   #given sample data, return subset of pct_taxons
# #   #keep is extra varnames to hold onto.
# #   pctnames <- pct.subset(names(sdata),level)
# #   if (!missing(keep)) {
# #     pctnames <- c(keep,pctnames)
# #   }
# #   sdata[,pctnames]
# # }
# #
# #
# # notax.subset <- function(x,...) UseMethod("notax.subset")
# #
# # notax.subset.character <- function(snames) {
# #   #removes all tax/pct variables
# #   setdiff(snames,c(tax.subset(snames),pct.subset(snames)))
# # }
# #
# #
# # notax.subset.data.frame <- function(sdata) {
# #   sdata[,notax.subset(names(sdata))]
# # }
# #
# # ###still needed?
# # #' @export
# # tax.percent <- function(sdata,taxon,level) {
# #   #given taxon/level, calculates rel abundance and returns vector of percents
# #   taxvarname <- tax.var(taxon,level,sdata)
# #   sdata[,taxvarname] / sdata$tax__Bacteria__1__0.1
# # }
# #
# # group.minorities <- function(sdata,level,
# #                              max.percent.cutoff=0.03,
# #                              total.percent.cutoff=0,
# #                              max.number.taxons=30,
# #                              new.minority.name="Other Bacteria") {
# #   #find minorities and renames them, then returns altered sdata
# #   #just renames columns, basically.
# #   sub <- tax.subset(sdata,level) #select tax vars with level
# #   ####row.names(sub) <- sub$group #copy group var to names
# #   ####sub <- subset(sub,select=-group) #remove group var
# #   sub <- as.matrix(sub)
# #   sub <- prop.table(sub,1) #proportions across rows
# #   total.percent <- apply(sub,2,mean) #overall abundance of taxons
# #   max.percent <- apply(sub,2,max) #max percent for each taxon
# #   minority.bytotal <- names(total.percent)[total.percent<total.percent.cutoff]
# #   minority.bymax <-names(max.percent)[max.percent<max.percent.cutoff]
# #   #minorities to be renamed.
# #   minorities <- unique(c(minority.bymax,minority.bytotal))
# #   remaining.taxons <- colnames(sub)[!(colnames(sub) %in% minorities)]
# #   #if remaining taxons is more than max,
# #   if (length(remaining.taxons)>max.number.taxons) {
# #     max.percent.ordered <- max.percent[order(max.percent,decreasing=TRUE)]
# #     names.ordered <- names(max.percent.ordered)
# #     remaining.taxons.ordered <- names.ordered[!is.na(match(names.ordered,remaining.taxons))]
# #     #shave off bottom and add to minorities
# #     more.minorities <- remaining.taxons.ordered[(max.number.taxons+1):length(remaining.taxons.ordered)]
# #     minorities <- unique(c(minorities,more.minorities))
# #   }
# #   minority.list=strsplit(minorities,split="__")
# #   #rename 2nd element
# #   minority.list.new <- lapply(minority.list,function(x){x[2]<-new.minority.name;return(x)})
# #   #new minority names
# #   minorities.new <- sapply(minority.list.new,function(x) paste(x,collapse="__"))
# #   names(sdata)[match(minorities,names(sdata))] <- minorities.new
# #   ###sdata <- calculate.percent.taxa(sdata)
# #   return(sdata)
# # }
# #
# # get.level.rank <- function(rank) {
# #   #get level given rankID
# #   nchar(rank) - nchar(gsub("\\.","",rank))
# # }
# #
# # rank.pattern <- function(rank,target.level) {
# #   #given rankID, change to a reg.exp pattern for specified level. is recursive.
# #   level <- get.level.rank(rank)
# #   if (level>target.level) {
# #     #shorten rankID
# #     rank <- gsub("\\.[0-9]{1,5}$","",rank)
# #     return(rank.pattern(rank,target.level))
# #   } else if (level<target.level) {
# #     #add to rankID
# #     rank <- paste(rank,".[0-9]{1,5}",sep="")
# #     return(rank.pattern(rank,target.level))
# #   } else {
# #     #rank=target.level
# #     pattern <- paste("^",gsub(".","\\.",rank,fixed=TRUE),"$",sep="")
# #     return(pattern)
# #   }
# # }
# #
# #
# # relative.taxnames <- function(sdata,orig.taxname,target.level) {
# #   #given orig.taxname, return names of taxons within sdata that are at target level.... moves up or down in phylo level.
# #   #going up means listing 1 taxon with more seqs, going down means listing multiple taxons, same number of seqs
# #   orig.taxname <- grep(orig.taxname,names(sdata),value=T)[1]
# #   orig.rank <- tax.rank(orig.taxname)
# #   pattern <- rank.pattern(orig.rank,target.level)
# #   target.taxlist <- tax.subset(names(sdata),target.level)
# #   target.taxlist[grepl(pattern,tax.rank(target.taxlist))]
# # }
# #
# #
# #
# #
# #
# #
# #
# # get.relative <- function(name,sdata,parent.level) {
# #   #get relative name (not variable names)
# #   varname <- grep(name,c(tax.subset(names(sdata)),pct.subset(names(sdata))),value=TRUE)[1]
# #   relatives <- relative.taxnames(sdata,varname,parent.level)
# #   tax.name(relatives)
# # }
# #
# #
# #
# # full.name <- function(name,sdata,top.level=1) {
# #   #obtain full name of taxon: e.g. tax__Enterococcus__6__0.1.15.1.2.3.2 becomes Bacteria|Firmicutes|Bacilli|Lactobacillales|Enterococcaceae|Enterococcus
# #   #sdata=s.s0;name=pct.subset(names(sdata));top.level=1
# #   sapply(name,function(x) {
# #     levels <- top.level:tax.level(x)
# #     all.names <- sapply(levels,function(y) get.relative(x,sdata,y))
# #     print(paste(all.names,collapse="|"))
# #     paste(all.names,collapse="|")
# #   })
# # }
# #
# #
# # rename.taxon.to.parent <- function(parent.taxname,orig.level,sdata,exceptions=NULL,parent.name=NULL) {
# #   if (is.null(parent.name)) {
# #     parent.name <- tax.name(grep(parent.taxname,names(sdata),value=TRUE)[1])
# #   }
# #   #returns sdata with children of parent.taxname at orig.level renamed to the parent taxon.
# #   child.taxnames <- relative.taxnames(sdata,parent.taxname,orig.level)
# #   #remove anything in exceptions
# #   #child.taxnames <- child.taxnames[!(tax.name(child.taxnames) %in% exceptions)]
# #   if (!is.null(exceptions)) {
# #     child.taxnames <- child.taxnames[!grepl(paste(exceptions,collapse="|"),child.taxnames)]
# #   }
# #   child.taxnames.list <- strsplit(child.taxnames,split="__")
# #   #rename 2nd element to parent name
# #   child.taxnames.list.new <- lapply(child.taxnames.list,function(x){x[2]<-parent.name;return(x)})
# #   child.renamed <- sapply(child.taxnames.list.new,function(x) paste(x,collapse="__"))
# #   names(sdata)[names(sdata) %in% child.taxnames] <- child.renamed
# #   #sdata <- calculate.percent.taxa(sdata)
# #   return(sdata)
# # }
# #
# #
# # get.dominating.taxon <- function(sdata,level) {
# #   #returns vector of taxon names representing most dominant taxon.
# #   sdata.subset <- as.matrix(tax.subset(sdata,level))
# #   varname <- apply(sdata.subset,1,function(x) names(x)[order(x,decreasing=TRUE)][1])
# #   tax.name(varname)
# # }
# #
# #
# #
# # get.dominating.amount <- function(sdata,level) {
# #   #returns vector of highest achieved relative abundances.
# #   sdata.subset <- as.matrix(tax.subset(sdata,level))
# #   sdata.subset <- prop.table(sdata.subset,1)
# #   apply(sdata.subset,1,max)
# # }
# #
# #
# #
# #
# # tax.plot.old <- function(sdata,level,xvar="group",facet=NULL,cid=FALSE,return.df=FALSE) {
# #   #plot taxons. x-axis is specified by xvar, which is group by default
# #   #list of taxon variables to melt
# #   if (cid) {
# #     sdata.tax <- cid.reclassify(sdata)
# #   } else {
# #     sdata.tax <- group.minorities(sdata,level,
# #                   max.percent.cutoff=0.03,total.percent.cutoff=0.03,max.number.taxons=15)
# #   }
# #   taxvars <- tax.subset(names(sdata.tax),level)
# #   sdata.melt <- melt(sdata.tax,measure.vars=taxvars,variable_name="taxon")
# #   #after melt, no need to keep track of zeros. not necessarily needed
# #   sdata.melt <- subset(sdata.melt,value!=0)
# #   #now remove tax/pct vars, no longer correct.
# #   sdata.melt <- notax.subset(sdata.melt)
# #   #convert to taxon name (tax__Enterococcus__6__0.1.15.1.2.3.2 -> Enterococcus)
# #   sdata.melt$taxon <- tax.name(sdata.melt$taxon)
# #   if (cid & level==6) {
# #     sdata.melt$taxon <- factor(sdata.melt$taxon,levels=rev(names(cid.colors)))
# #   }
# #   #need to get rid of duplicate names. this will group by group+taxon and add up amts
# #   #occurs mainly because of multiple "unclassified" categories.
# #   sdata.melt.grouped <- ddply(sdata.melt,c("group","taxon"),function(x) data.frame(x[1,setdiff(names(x),"value")],value=sum(x$value)))
# #   #grouped.list <- by(sdata.melt, list(sdata.melt$group,sdata.melt$taxon),
# #                      #function(x) data.frame(x[1,c("group","taxon",xvar)],value=sum(x$value)))
# #   #sdata.melt.grouped <- do.call(rbind,grouped.list)
# #
# #   if (return.df) {
# #     return(sdata.melt.grouped)
# #   } else {
# #     g <- ggplot() +
# #       geom_bar(data=sdata.melt.grouped,aes_string(x=xvar,y="value",fill="taxon",width=0.90),stat="identity",position="fill") +
# #       scale_y_continuous(labels=percent) + ylab("Relative Abundance")
# #     if (cid & level==6) {
# #       g <- g + scale_fill_manual(values=cid.colors) + guides(fill=guide_legend(reverse=TRUE))
# #     }
# #     if (!is.null(facet)) {
# #       g <- g + facet_grid(paste0(". ~ ",facet),scales="free_x",space="free_x")
# #     }
# #     return(g)
# #   }
# # }
# #
# #
# #
# #
# # cid.reclassify <- function(sdata) {
# #   sdata <- rename.taxon.to.parent("tax__Proteobacteria__2__",6,sdata)
# #   sdata <- rename.taxon.to.parent("tax__Bacteroidetes__2__",6,sdata,
# #                                   exceptions=names(cid.colors),parent.name="Other Bacteroidetes")
# #   sdata <- rename.taxon.to.parent("tax__Firmicutes__2__",6,sdata,
# #                                   exceptions=names(cid.colors),parent.name="Other Firmicutes")
# #   sdata <- rename.taxon.to.parent("tax__Bacteria__1__",6,sdata,
# #                                   exceptions=names(cid.colors),parent.name="Other Bacteria")
# #   return(sdata)
# # }
# #
# # tax.colors <- function(taxcolorfile="taxcolors.csv") {
# #   c <- read.csv(taxcolorfile)
# #   code <- rgb(c$red,c$green,c$blue,names=c$taxon,maxColorValue=255)
# #   names(code) <- as.character(c$taxon)
# #   return(code)
# # }
# #
# # create.taxpalette <- function(sdata.tax,level) {
# #   #returns manual color palette, given sdata's taxons
# #   #list of things being graphed
# #   taxvars <- tax.subset(names(sdata.tax),level)
# #   taxnames <- unique(tax.name(taxvars))
# #   #exceptions to coloring:
# #   other.taxon <- c("Other_Bacteria"="#7b7b7b")
# #   #now divide taxnames into color and exceptions to color.
# #   taxnames.other <- grep(names(other.taxon),taxnames,value=TRUE)
# #   taxnames <-grep(names(other.taxon),taxnames,value=TRUE,invert=TRUE)
# #
# #   #assign colors to regular taxons
# #   #palette <- data.frame(taxon=taxnames,pal=brewer.pal(length(taxnames),"Spectral"))
# #   palette <- data.frame(taxon=taxnames,pal=rainbow(length(taxnames)))
# #
# #   ##getparents
# #   #parents <- sapply(taxvars,function(x) relative.taxnames(sdata,x,2))
# #   #now reformat to vector with label
# #   formatted.palette <- as.character(palette$pal)
# #   names(formatted.palette) <- palette$taxon
# #   #combine back with exception color.
# #   palette.combined <- c(formatted.palette,other.taxon)
# #   palette.combined
# #   return(palette.combined)
# # }
# #
# #
# # read.tax.melt <- function(taxfile) {
# #   #taxfile="total.tax.summary"
# #   t0 <- read.delim(taxfile,check.names=FALSE,as.is=TRUE)
# #   t0$taxon <- sub("[kpcofgs]__","",t0$taxon) #get rid of leading letter, if there
# #   #get rid of NA column
# #   t0 <- t0[,!sapply(t0,function(x) all(is.na(x)))]
# #   measure.vars <- names(t0)[(which(names(t0)=="total")+1):length(names(t0))]
# #   max.lvl <- max(t0$taxlevel)
# #   t <- subset(t0,taxlevel==max.lvl)
# #   get.rank <- function(rank,newlevel) {
# #     #rank=t$rankID;newlevel=6
# #     oldlevel <- get.level.rank(rank[1])
# #     diff <- oldlevel - newlevel
# #     if (oldlevel<=newlevel) {
# #       return(rank)
# #     } else {
# #       pattern <- paste(c(rep("\\.[0-9]+",times=oldlevel-newlevel),"$"),collapse="")
# #       return(sub(pattern,"",rank))
# #     }
# #   }
# #   for (l in max.lvl:1) {
# #     varname <- paste0("taxon.",l)
# #     t[,varname] <- t0$taxon[match(get.rank(t$rankID,l),t0$rankID)]
# #   }
# #   t <- subset(t,select=c(-daughterlevels,-total))
# #   t.melt <- melt(t,measure.vars=measure.vars)
# #   t.melt$variable <- as.character(t.melt$variable)
# #   names(t.melt)[names(t.melt)=="variable"] <- "group"
# #   names(t.melt)[names(t.melt)=="value"] <- "numseqs"
# #   t.melt <- ddply(t.melt,"group",function(x) {
# #     x$pctseqs <- prop.table(x$numseqs)
# #     return(x)
# #   },.progress="text")
# #   return(t.melt)
# # }
#
#
#
#
#
# #
# # read.otu.melt.phyloseq.old <- function(phy) {
# #   #phy0=phy;phy=subset_taxa(phy0,taxa_names(phy0) %in% head(taxa_names(phy0),10))
# #   otu0 <- data.frame(otu=taxa_names(phy),otu_table(phy),check.names=FALSE,stringsAsFactors=FALSE)
# #   tax0 <- data.frame(tax_table(phy),check.names=FALSE,stringsAsFactors=FALSE)
# #   maxlvl <- length(tax0)
# #   names(tax0) <- paste0("taxon.",1:maxlvl)
# #   tax0 <- data.frame(lapply(tax0,function(x) {
# #     sub("^[kpcofgs]__","",x)
# #   }),stringsAsFactors=FALSE)
# #   for (lvl in 2:maxlvl) {
# #     taxa <- tax0[,lvl]
# #     parent <- tax0[,lvl-1]
# #     parent <- ifelse(grepl("^unclassified.+\\([0-9]\\)$",parent),parent,paste0("unclassified ",parent,"(",lvl-1,")"))
# #     tax0[,lvl] <- ifelse(is.na(taxa),parent,taxa)
# #   }
# #   tax0$taxon <- tax0[,maxlvl]
# #   tax0$taxlevel <- maxlvl
# #   id.vars <- c("otu",names(tax0))
# #   otu <- cbind(otu0,tax0) %>%
# #     melt(id.vars=id.vars,variable.name="group",value.name="numseqs") %>%
# #     mutate(group=as.character(group)) %>%
# #     filter(numseqs>0) %>%
# #     ddply("group",function(x) {
# #       x$pctseqs <- prop.table(x$numseqs)
# #       return(x)
# #     })
# #   return(otu)
# # }
# # read.otu.melt.phyloseq.old <- function(phy,filter.zero=TRUE,sample_data=TRUE) {
# #   #phy0=phy;phy=subset_taxa(phy0,taxa_names(phy0) %in% head(taxa_names(phy0),10))
# #   otu0 <- data.frame(otu=taxa_names(phy),otu_table(phy),check.names=FALSE,stringsAsFactors=FALSE)
# #   tax0 <- get.tax(phy)
# #   id.vars <- c("otu",names(tax0))
# #   otu <- cbind(otu0,tax0) %>%
# #     melt(id.vars=id.vars,variable.name="sample",value.name="numseqs") %>%
# #     mutate(group=as.character(group)) %>%
# #     group_by(group) %>% mutate(pctseqs=prop.table(numseqs)) %>% ungroup() %>% tbl_df()
# #   if (filter.zero) {
# #     otu <- otu %>% filter(numseqs>0)
# #   }
# #   #add sample data
# #   if (sample_data) {
# #     samp0 <- get.samp(phy)
# #     otu <- otu %>% left_join(samp0,by="group")
# #   }
# #   return(otu)
# # }
# # read.oligos.OLD <- function(dir) {
# #   oligos <- list.files(dir,full.names=TRUE)
# #   ldply(oligos,function(o) {
# #     t <- read.table(o,sep="\t",colClasses="character")
# #     columns <- length(t)
# #     platform <- recode(columns,"3='454';4='miseq';else=NA",as.factor.result=TRUE)
# #     bar <- t[grepl("barcode",t[,1],ignore.case=TRUE),] #obtain barcodes
# #     group <- bar[,columns]
# #     pool <- gsub("\\.?oligos/?","",o)
# #     oligo.data <- data.frame(group,stringsAsFactors=FALSE)
# #     data.frame(pool,platform,oligo.data,stringsAsFactors=FALSE)
# #   })
# # }
# # lefse.format.old <- function(sdata,class,subclass=NULL,subject=NULL,filename="lefse.txt") {
# #   #sdata=pp;class="dead";subclass=NULL;subject=NULL;filename="surv_lefse_nosubclass.txt"
# #   vars <- c(class,subclass,subject)
# #   for (v in vars) {
# #     sdata[,v] <- gsub(" ","_",sdata[,v])
# #   }
# #   taxvars <- pct.subset(names(sdata))
# #   vars <- c(vars,taxvars)
# #   lefse <- t(sdata[,vars])
# #   varlist.rename <- row.names(lefse) %in% taxvars
# #   row.names(lefse)[varlist.rename] <- full.name(row.names(lefse)[varlist.rename],sdata)
# #   write.table(lefse,filename,sep="\t",col.names=FALSE,quote=FALSE)
# # }
# # #' Convert Phyloseq object to Sample Data.
# # #'
# # #' Creates sample-level data from phyloseq object
# # #'
# # #' Extracts sample data from phyloseq, including number of sequences and diversity data.
# # #'
# # #' @param phy phyloseq object (which can be created from biom using \code{import_biom})
# # #' @param measures Diversity indices to calculate. This parameter is passed to \code{estimate_richness}. Supported values include: \code{c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")}
# # #' @return Output is a data frame, listing samples by ID=\code{group}, with corresponding number of sequences (\code{nseqs}), diversity metrics (e.g. \code{Observed}, \code{InvSimpson}), and sample data (if it exists in phyloseq)
# # #' @examples
# # #' phy <- import_biom("uparse/total.8.otu-tax.biom")
# # #' s <- read.samp.biom(phy)
# # #' @author Ying Taur
# # #' @export
# # read.samp.phyloseq <- function(phy,measures=c("Observed","InvSimpson","Shannon")) {
# #   #phy=met;measures=c("Observed","InvSimpson","Shannon")
# #
# #   s <- data.frame(group=sample_names(phy),
# #                   nseqs=sample_sums(phy),
# #                   estimate_richness(phy,measures=measures),stringsAsFactors=FALSE)
# #
# #   #add sample data if present
# #   if (!is.null(sample_data(phy,FALSE))) {
# #     sdata <- sample_data(phy) %>% data.frame(stringsAsFactors=FALSE) %>% mutate(group=row.names(.))
# #     s <- s %>% left_join(sdata)
# #   }
# #   return(s)
# # }
# # get.yt.palette.old <- function(tax.melt,use.cid.colors=TRUE) {
# #
# #   #tax.dict <- unique(subset(tax.melt,select=c(rankID,taxlevel,taxon,taxon.1,taxon.2,taxon.3,taxon.4,taxon.5,taxon.6)))
# #   tax.dict <- unique(subset(tax.melt,select=c(taxlevel,taxon,taxon.1,taxon.2,taxon.3,taxon.4,taxon.5,taxon.6)))
# #   #bacteria are shades of gray by default
# #   tax.dict$color <- rep(shades("gray"),length.out=nrow(tax.dict))
# #   #proteobacteria: red
# #   proteo <- tax.dict$taxon.2=="Proteobacteria"
# #   tax.dict$color[proteo] <- rep(shades("red",variation=0.4),length.out=sum(proteo))
# #   #bacteroidetes: cyan
# #   bacteroidetes <- tax.dict$taxon.2=="Bacteroidetes"
# #   tax.dict$color[bacteroidetes] <- rep(shades("#2dbfc2",variation=0.4),length.out=sum(bacteroidetes))
# #   #actinobacteria: purple
# #   actino <- tax.dict$taxon.2=="Actinobacteria"
# #   tax.dict$color[actino] <- rep(shades("purple",variation=0.4),length.out=sum(actino))
# #   #firmicutes:
# #   firm <- tax.dict$taxon.2=="Firmicutes"
# #   tax.dict$color[firm] <- rep(shades("#8f7536",variation=0.3),length.out=sum(firm))
# #   #cid
# #   if (use.cid.colors) {
# #     cid <- cid.colors[match(tax.dict$taxon.6,names(cid.colors))]
# #     tax.dict$color <- ifelse(is.na(cid),tax.dict$color,cid)
# #   }
# #   tax.palette <- structure(tax.dict$color,names=as.character(tax.dict$taxon))
# #   tax.palette
# # }
# #
# # #' Plot tax
# # #'
# # #' @export
# # plot.tax.old <- function(t,xvar="group",data=FALSE,label.pct.cutoff=0.3,cid.colors=TRUE) {
# #   #t <- t %>% arrange(taxon.1,taxon.2,taxon.3,taxon.4,taxon.5,taxon.6,taxon.7)
# #
# #   taxlvls <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
# #   newlvls <- c("taxon.1","taxon.2","taxon.3","taxon.4","taxon.5","taxon.6","taxon.7","taxon")
# #   if (all(taxlvls %in% names(t)) & any(newlvls %!in% names(t))) {
# #     t <- t %>% mutate(taxon.1=Kingdom,taxon.2=Phylum,taxon.3=Class,taxon.4=Order,taxon.5=Family,taxon.6=Genus,taxon.7=Species,taxon=Species)
# #   }
# #   t <- t[order(t$taxon.1,t$taxon.2,t$taxon.3,t$taxon.4,t$taxon.5,t$taxon.6,t$taxon.7),]
# #
# #   t$taxon <- factor(t$taxon,levels=unique(t$taxon))
# #   t$taxlevel <- 7
# #   t <- ddply(t,"group",function(x) {
# #     x <- x[order(x$taxon),]
# #     cum.pct <- cumsum(x$pctseqs)
# #     x$y.text <- (cum.pct + c(0,cum.pct[-length(cum.pct)])) / 2
# #     return(x)
# #   })
# #   pal <- get.yt.palette(t,use.cid.colors=cid.colors)
# #   attr(t,"pal") <- pal
# #   t$tax.label <- ifelse(t$pctseqs>=label.pct.cutoff,as.character(t$taxon),"")
# #   if (data) {
# #     return(t)
# #   } else {
# #     g <- ggplot() +
# #       geom_bar(data=t,aes_string(x=xvar,y="pctseqs",fill="taxon"),stat="identity",position="fill") +
# #       geom_text(data=t,aes_string(x=xvar,y="y.text",label="tax.label"),angle=-90,lineheight=0.9) +
# #       scale_fill_manual(values=attr(t,"pal")) +
# #       theme(legend.position="none")
# #     return(g)
# #   }
# # }
