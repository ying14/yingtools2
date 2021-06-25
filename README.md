yingtools2 package
================

Ying Taur’s tools for analysis, with particular focus on microbiome data

yingtools2 is an R package containing many tools and functions for
working with clinical and microbiome data.

## Installation

Install this package from Github:

``` r
remotes::install_github("ying14/yingtools2")
```

## Sample Microbiome Dataset

Included is a de-identified microbiome dataset of stool samples
collected from a cohort of bone marrow transplant recipients at Memorial
Sloan Kettering Cancer Center. This was previously published in
[Clinical Infectious Diseases
(2012)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3657523/). The
dataset includes sequence data for 439 samples (from 94 patients), as
well as a variety of accompanying clinical metadata.

-   `cid.phy`: 16S sequence data (phyloseq object, 1,838,204 seqs, 439
    samples, 5,327 OTUs)
-   `cid.patients`: Patients (94 rows)
-   `cid.hosp`: Hospitalizations (314 rows)
-   `cid.meds`: Medications (6,473 rows)
-   `cid.bsi`: Bloodstream Infections (138 rows)
-   `cid.cdiff`: C. diff results (37 rows)

## Code Examples

Below are various use coding examples using yingtools2.

### Basics

-   [Coding Curriculum](examples/curriculum.md).
-   [Exercises](examples/exercises.md).

### Microbiome plots

-   [Figure 1 Diversity plot](examples/cid.diversity.md)
-   [Table 1, Characteristics of
    Patients](examples/cid.patient.characteristics.md)
-   [Figure 3A, Hierarchical clustering](examples/cid.hclust.md)
