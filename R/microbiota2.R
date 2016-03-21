




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
