% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microbiota2.R
\name{read.blastn.file}
\alias{read.blastn.file}
\title{Read in tax file from blastn output}
\usage{
read.blastn.file(tax.file, tax_table = TRUE)
}
\arguments{
\item{tax_table}{logical, if TRUE (default), will return a data frame of taxonomy, which can be directly converted to a phyloseq \link[phyloseq:taxonomyTable-class]{tax_table} object. If FALSE, returns data frame with all hits and associated data.}

\item{tax}{Taxonomy data from blastn, either as the file or a data frame.}
}
\value{
Data from the blastn data file.
}
\description{
This reads in the file created by the YT python script, blastn.py
}
\details{
Chooses one taxonomy from the hits, also listing runner-up taxonomy.
\itemize{
\item qseqid: Query Seq-id
\item qgi: Query GI
\item qacc: Query accesion
\item qaccver: Query accesion.version
\item qlen: Query sequence length
\item sseqid: Subject Seq-id
\item sallseqid: All subject Seq-id(s), separated by a ';'
\item sgi: Subject GI
\item sallgi: All subject GIs
\item sacc: Subject accession
\item saccver: Subject accession.version
\item sallacc: All subject accessions
\item slen: Subject sequence length
\item qstart: Start of alignment in query
\item qend: End of alignment in query
\item sstart: Start of alignment in subject
\item send: End of alignment in subject
\item qseq: Aligned part of query sequence
\item sseq: Aligned part of subject sequence
\item evalue: Expect value. Describes the number of hits one can "expect" to see by chance when searching a database of a particular size.
It decreases exponentially as the Score (S) of the match increases. Essentially, the E value describes the random background noise.
For example, an E value of 1 assigned to a hit can be interpreted as meaning that in a database of the current size one might
expect to see 1 match with a similar score simply by chance.
\item bitscore: Bit score
\item score: Raw score
\item length: Alignment length
\item pident: Percentage of identical matches
\item nident: Number of identical matches
\item mismatch: Number of mismatches
\item positive: Number of positive-scoring matches
\item gapopen: Number of gap openings
\item gaps: Total number of gaps
\item ppos: Percentage of positive-scoring matches
\item frames: Query and subject frames separated by a '/'
\item qframe: Query frame
\item sframe: Subject frame
\item btop: Blast traceback operations (BTOP)
\item staxid: Subject Taxonomy ID
\item ssciname: Subject Scientific Name
\item scomname: Subject Common Name
\item sblastname: Subject Blast Name
\item sskingdom: Subject Super Kingdom
\item staxids: unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)
\item sscinames: unique Subject Scientific Name(s), separated by a ';'
\item scomnames: unique Subject Common Name(s), separated by a ';'
\item sblastnames: unique Subject Blast Name(s), separated by a ';' (in alphabetical order)
\item sskingdoms: unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)
\item stitle: Subject Title
\item salltitles: All Subject Title(s), separated by a '<>'
\item sstrand: Subject Strand
\item qcovs: Query Coverage Per Subject
\item qcovhsp: Query Coverage Per HSP
\item qcovus: Query Coverage Per Unique Subject (blastn only)
}
}
\author{
Ying Taur
}
