

# #' CID Data (94 patients)
# #'
# #' A dataset containing clinical and microbiota data on the 94 patients from CID 2012.
# #'
# #' \itemize{
# #'   \item cid.patients Patient data (N=94)
# #'   \item cid.meds Medications dispensed
# #'   \item cid.bsi Bloodstream infection data
# #'   \item cid.cdiff C.difficile testing
# #'   \item cid.hosp Hospitalizations
# #'   \item cid.phy 16S data (phyloseq format)
# #' }
# #'
# #' @docType data
# #' @keywords datasets
# #' @name cid94
# #' @usage data(cid94)
# #' @format Several data frames.
# NULL



#' Bloodstream Infections
#' @format data.frame
"cid.bsi"

#' C. diff results
#' @format data.frame
"cid.cdiff"

#' Hospitalizations
#' @format data.frame
"cid.hosp"

#' Medications
#' @format data.frame
"cid.meds"

#' Patients
#' @format data.frame
"cid.patients"

#' 16S sequence data
#' @format phyloseq
"cid.phy"


#####save all to data directory
# eval(parse(text=paste0("usethis::use_data(",paste(ls(),collapse=", "),",overwrite=TRUE)")))

#####create data documentation roxygen skeleton
# sapply(ls(),function(x) {
#   obj <- get(x)
#   fields <- c(x,
#               paste0("@format ",class(obj)[1]),
#               # "\\describe{",
#               # paste0("\t\\item{",names(obj),"}{xxxdescxx}"),
#               # "}",
#               NULL)
#   text <- paste0("#' ",fields,collapse="\n") %>% paste0("\n\"",x,"\"")
#   text
# }) %>% paste(collapse="\n\n") %>% copy.to.clipboard()


