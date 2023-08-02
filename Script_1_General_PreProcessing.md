Script_1\_General_PreProcessing
================
Linda Dieckmann
November 2022

# load packages

``` r
library(here)
```

    ## here() starts at /Users/linda_dieckmann/Documents/Processing/P2.5_Helsinki

``` r
library(biomaRt)
library(dplyr)
```

    ## 
    ## Attache Paket: 'dplyr'

    ## Das folgende Objekt ist maskiert 'package:biomaRt':
    ## 
    ##     select

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     filter, lag

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(DESeq2)
```

    ## Warning: Paket 'DESeq2' wurde unter R Version 4.2.2 erstellt

    ## Lade nötiges Paket: S4Vectors

    ## Warning: Paket 'S4Vectors' wurde unter R Version 4.2.2 erstellt

    ## Lade nötiges Paket: stats4

    ## Lade nötiges Paket: BiocGenerics

    ## 
    ## Attache Paket: 'BiocGenerics'

    ## Die folgenden Objekte sind maskiert von 'package:dplyr':
    ## 
    ##     combine, intersect, setdiff, union

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     IQR, mad, sd, var, xtabs

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
    ##     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
    ##     get, grep, grepl, intersect, is.unsorted, lapply, Map, mapply,
    ##     match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
    ##     Position, rank, rbind, Reduce, rownames, sapply, setdiff, sort,
    ##     table, tapply, union, unique, unsplit, which.max, which.min

    ## 
    ## Attache Paket: 'S4Vectors'

    ## Die folgenden Objekte sind maskiert von 'package:dplyr':
    ## 
    ##     first, rename

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     expand.grid, I, unname

    ## Lade nötiges Paket: IRanges

    ## 
    ## Attache Paket: 'IRanges'

    ## Die folgenden Objekte sind maskiert von 'package:dplyr':
    ## 
    ##     collapse, desc, slice

    ## Lade nötiges Paket: GenomicRanges

    ## Warning: Paket 'GenomicRanges' wurde unter R Version 4.2.2 erstellt

    ## Lade nötiges Paket: GenomeInfoDb

    ## Warning: Paket 'GenomeInfoDb' wurde unter R Version 4.2.2 erstellt

    ## Lade nötiges Paket: SummarizedExperiment

    ## Lade nötiges Paket: MatrixGenerics

    ## Lade nötiges Paket: matrixStats

    ## 
    ## Attache Paket: 'matrixStats'

    ## Das folgende Objekt ist maskiert 'package:dplyr':
    ## 
    ##     count

    ## 
    ## Attache Paket: 'MatrixGenerics'

    ## Die folgenden Objekte sind maskiert von 'package:matrixStats':
    ## 
    ##     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
    ##     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
    ##     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
    ##     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
    ##     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
    ##     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
    ##     colWeightedMeans, colWeightedMedians, colWeightedSds,
    ##     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
    ##     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
    ##     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
    ##     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
    ##     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
    ##     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
    ##     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
    ##     rowWeightedSds, rowWeightedVars

    ## Lade nötiges Paket: Biobase

    ## Welcome to Bioconductor
    ## 
    ##     Vignettes contain introductory material; view with
    ##     'browseVignettes()'. To cite Bioconductor, see
    ##     'citation("Biobase")', and for packages 'citation("pkgname")'.

    ## 
    ## Attache Paket: 'Biobase'

    ## Das folgende Objekt ist maskiert 'package:MatrixGenerics':
    ## 
    ##     rowMedians

    ## Die folgenden Objekte sind maskiert von 'package:matrixStats':
    ## 
    ##     anyMissing, rowMedians

``` r
library(rstatix)
```

    ## 
    ## Attache Paket: 'rstatix'

    ## Das folgende Objekt ist maskiert 'package:IRanges':
    ## 
    ##     desc

    ## Das folgende Objekt ist maskiert 'package:biomaRt':
    ## 
    ##     select

    ## Das folgende Objekt ist maskiert 'package:stats':
    ## 
    ##     filter

``` r
library(psych)
```

    ## 
    ## Attache Paket: 'psych'

    ## Das folgende Objekt ist maskiert 'package:SummarizedExperiment':
    ## 
    ##     distance

    ## Das folgende Objekt ist maskiert 'package:GenomicRanges':
    ## 
    ##     distance

    ## Die folgenden Objekte sind maskiert von 'package:IRanges':
    ## 
    ##     distance, reflect

``` r
writeLines(capture.output(sessionInfo()), here("Analyses/Scripts/", "sessionInfo_Script_1.txt"))
```

# load functions

``` r
source(here("Analyses/Scripts/", "functions.R"))
```

    ## 
    ## Attache Paket: 'ggplot2'

    ## Die folgenden Objekte sind maskiert von 'package:psych':
    ## 
    ##     %+%, alpha

    ## Lade nötiges Paket: ggpp

    ## 
    ## Attache Paket: 'ggpp'

    ## Das folgende Objekt ist maskiert 'package:ggplot2':
    ## 
    ##     annotate

    ## 
    ## Attache Paket: 'gtsummary'

    ## Das folgende Objekt ist maskiert 'package:biomaRt':
    ## 
    ##     select

# load & prepare data

## Phenotypes

### register and confounder data

original register and ethnicity variables

``` r
load(here("02_Data/pheno/", "ethnicity_ITU.Rdata"))
load(here("02_Data/pheno/", "ITU_register_vars.Rdata"))
```

data (register, ethnicity, cell types etc.) already matched for RNA data

``` r
load(here("02_Data/pheno/", "confounders_RNA_placenta.Rdata"))
load(here("02_Data/pheno/", "confounders_RNA_cvs.Rdata"))
```

``` r
dim(confounders_RNA_placenta)
```

    ## [1] 494  64

``` r
dim(confounders_RNA_cvs)
```

    ## [1] 267  64

These phenotype data sets have the same sample sizes as our
pre-processed RNASeq data set

ITU birth year data:

``` r
load(here("02_Data/pheno/", "birth_year_itu.Rdata"))
```

### glycyrrhizin data

``` r
load(here("02_Data/pheno/", "gly_mean_mg_overpregnancy_perwomen.Rdata")) 
# mean consumption of glycyrrhizin over pregnancy per woman with available data (n = 612), in mg
load(here("02_Data/pheno/", "gly_max_mg_overpregnancy_perwomen.Rdata")) 
# max of mean consumption of glycyrrhizin over pregnancy per woman with available data (n = 612), in mg

load(here("02_Data/pheno/", "Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared.Rdata")) 
dim(Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared)
```

    ## [1] 1512   30

``` r
# data in other format, with single pregnancy stages information
```

we merge the ITU birth year info:

``` r
Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared <- merge(Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared, birth_year_itu, by="IDfamily")
gly_mean_mg_overpregnancy_perwomen <- merge(gly_mean_mg_overpregnancy_perwomen, birth_year_itu, by="IDfamily")
gly_max_mg_overpregnancy_perwomen <- merge(gly_max_mg_overpregnancy_perwomen, birth_year_itu, by="IDfamily")
```

We have one data set containing the average mg glycyrrhicin consumption
over pregnancy for 612 women, this shall be used for the term placenta
data set.

We have another data set containing the information about glycyrrhizin
consumption including the indication of pregnancy stages. We want to
filter this data set for the first pregnancy stage:

``` r
Glycyrrhizin_trim_I <- Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared[Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared$pregstage == "I", ] 
cat("We have",  length(unique(Glycyrrhizin_trim_I$IDfamily)), "samples with glycyrrhizin consumption info for trimester I")
```

    ## We have 415 samples with glycyrrhizin consumption info for trimester I

we calculate the winsorized value for this data set

``` r
Glycyrrhizin_trim_I$gly_trim_mg_q_pregstageMean_winsz_bypregstage <- DescTools::Winsorize(Glycyrrhizin_trim_I$gly_trim_mg_q_pregstageMean, na.rm =T)
```

``` r
Glycyrrhizin_trim_II <- Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared[Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared$pregstage == "II", ] 
cat("We have",  length(unique(Glycyrrhizin_trim_II$IDfamily)), "samples for trimester II")
```

    ## We have 538 samples for trimester II

we calculate the winsorized value for this data set

``` r
Glycyrrhizin_trim_II$gly_trim_mg_q_pregstageMean_winsz_bypregstage <- DescTools::Winsorize(Glycyrrhizin_trim_II$gly_trim_mg_q_pregstageMean, na.rm =T)
```

``` r
Glycyrrhizin_trim_III <- Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared[Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared$pregstage == "III", ] 
cat("We have",  length(unique(Glycyrrhizin_trim_III$IDfamily)), "samples for trimester III")
```

    ## We have 559 samples for trimester III

we calculate the winsorized value for this data set

``` r
Glycyrrhizin_trim_III$gly_trim_mg_q_pregstageMean_winsz_bypregstage <- DescTools::Winsorize(Glycyrrhizin_trim_III$gly_trim_mg_q_pregstageMean, na.rm =T)
```

make a data frame for this stages data

``` r
Glycyrrhizin_trim_data <- rbind(Glycyrrhizin_trim_I, Glycyrrhizin_trim_II, Glycyrrhizin_trim_III)
```

``` r
save(Glycyrrhizin_trim_data, gly_mean_mg_overpregnancy_perwomen, gly_max_mg_overpregnancy_perwomen, file = here("02_Data/pheno/", "glycyrrhizin_data_all.Rdata"))
```

# merge phenotypes, prepare RNA data sets

## for RNA Placenta

*mean mg glycyrrhizin over pregnancy - main analysis* note that we use
the mean glycyrrhizin consumption over all pregnancy stages

``` r
pheno_rna_placenta_overpreg <- merge(confounders_RNA_placenta, gly_mean_mg_overpregnancy_perwomen, by = "IDfamily")
dim(pheno_rna_placenta_overpreg)
```

    ## [1] 425  69

``` r
placenta_ids_in <- pheno_rna_placenta_overpreg$IDfamily
```

``` r
pheno_rna_placenta_overpreg %>% select(c(gly_trim_mg_mean_over_pregnancy)) %>% describe()
```

    ##                                 vars   n   mean      sd median trimmed    mad
    ## gly_trim_mg_mean_over_pregnancy    1 425 932.92 1470.55 425.63  619.44 564.32
    ##                                 min      max    range skew kurtosis    se
    ## gly_trim_mg_mean_over_pregnancy   0 13637.34 13637.34 3.83    21.03 71.33

we also check outliers / general distribution

``` r
pheno_rna_placenta_outliers_glycyrrhizin_mean_overpreg <- pheno_rna_placenta_overpreg %>%
  rstatix::identify_outliers(gly_trim_mg_mean_over_pregnancy)

cat("We have", sum(pheno_rna_placenta_outliers_glycyrrhizin_mean_overpreg$is.outlier), "outlier samples using the mean glycyrrhizin consumption over pregnancy. ")
```

    ## We have 40 outlier samples using the mean glycyrrhizin consumption over pregnancy.

``` r
cat("We have", sum(pheno_rna_placenta_outliers_glycyrrhizin_mean_overpreg$is.extreme), "'extreme' samples using the mean glycyrrhizin consumption over pregnancy")
```

    ## We have 16 'extreme' samples using the mean glycyrrhizin consumption over pregnancy

``` r
plot_box(pheno_rna_placenta_overpreg, outcome = gly_trim_mg_mean_over_pregnancy, xtitle = "", ytitle = "mean mg glycyrrhizin over pregnancy")+
   geom_text(position=position_dodge(width = 0.8), size = 3, vjust = -0.5, aes(x = 0,y= median(gly_trim_mg_mean_over_pregnancy), label = median(gly_trim_mg_mean_over_pregnancy)))
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
hist(pheno_rna_placenta_overpreg$gly_trim_mg_mean_over_pregnancy)
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->
\* reported is the median value \* dashed line refers to a mean mg
consumption of 6000 (corresponding to previous cutoff value of 500 \* 12
weeks)

we calculate the winsorized value for this data set

``` r
pheno_rna_placenta_overpreg$gly_trim_mg_mean_over_pregnancy_winsz <- DescTools::Winsorize(pheno_rna_placenta_overpreg$gly_trim_mg_mean_over_pregnancy, na.rm =T)
```

``` r
pheno_rna_placenta_outliers_glycyrrhizin_mean_overpreg_winsz <- pheno_rna_placenta_overpreg %>%
  rstatix::identify_outliers(gly_trim_mg_mean_over_pregnancy_winsz)
  
cat("\nWe have", sum(pheno_rna_placenta_outliers_glycyrrhizin_mean_overpreg_winsz$is.outlier), "outlier samples using the winsorized mean glycyrrhizin consumption over pregnancy. ")
```

    ## 
    ## We have 40 outlier samples using the winsorized mean glycyrrhizin consumption over pregnancy.

``` r
cat("We have", sum(pheno_rna_placenta_outliers_glycyrrhizin_mean_overpreg_winsz$is.extreme), "'extreme' samples using the winsorized mean glycyrrhizin consumption over pregnancy")
```

    ## We have 0 'extreme' samples using the winsorized mean glycyrrhizin consumption over pregnancy

``` r
plot_box(pheno_rna_placenta_overpreg, outcome = gly_trim_mg_mean_over_pregnancy_winsz, xtitle = "", ytitle = "mean mg glycyrrhizin over pregnancy, winsorized")+
   geom_text(position=position_dodge(width = 0.8), size = 3, vjust = -0.5, aes(x = 0,y= median(gly_trim_mg_mean_over_pregnancy_winsz), label = median(gly_trim_mg_mean_over_pregnancy_winsz)))
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
hist(pheno_rna_placenta_overpreg$gly_trim_mg_mean_over_pregnancy_winsz)
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->
We see that winsorizing helps to reduce outliers (extreme values)

``` r
cat("We have available data for n =", nrow(pheno_rna_placenta_overpreg), "samples for RNA Placenta Analyses")
```

    ## We have available data for n = 425 samples for RNA Placenta Analyses

*max mg glycyrrhizin over pregnancy (to check)* then we use the max
glycyrrhizin consumption over all pregnancy stages

``` r
pheno_rna_placenta_maxpreg <- merge(confounders_RNA_placenta, gly_max_mg_overpregnancy_perwomen, by = "IDfamily")
dim(pheno_rna_placenta_maxpreg)
```

    ## [1] 425  69

we calculate the winsorized value for this data set

``` r
pheno_rna_placenta_maxpreg$gly_trim_mg_max_over_pregnancy_winsz <- DescTools::Winsorize(pheno_rna_placenta_maxpreg$gly_trim_mg_max_over_pregnancy, na.rm =T)
```

``` r
cat("We have available data for n =", nrow(pheno_rna_placenta_maxpreg), "samples for RNA Placenta Analyses")
```

    ## We have available data for n = 425 samples for RNA Placenta Analyses

*pregnancy stages data* Now we also merge the data per pregnancy stage

``` r
pheno_rna_placenta_stages <- merge(confounders_RNA_placenta, Glycyrrhizin_variables_for_use_imputed_gestageages_data_prepared, by = "IDfamily")
dim(pheno_rna_placenta_stages)
```

    ## [1] 1090   94

``` r
length(unique(pheno_rna_placenta_stages$IDfamily))
```

    ## [1] 425

we want the data per pregnancy stage

``` r
pheno_rna_placenta_stages_I <- pheno_rna_placenta_stages[pheno_rna_placenta_stages$pregstage == "I", ] 
cat("We have",  length(unique(pheno_rna_placenta_stages_I$IDfamily)), "samples in stage I")
```

    ## We have 306 samples in stage I

``` r
placenta_ids_in_trimI <- pheno_rna_placenta_stages_I$IDfamily
```

we calculate the winsorized value for this data set

``` r
pheno_rna_placenta_stages_I$gly_trim_mg_q_pregstageMean_winsz <- DescTools::Winsorize(pheno_rna_placenta_stages_I$gly_trim_mg_q_pregstageMean, na.rm =T)
```

``` r
pheno_rna_placenta_stages_II <- pheno_rna_placenta_stages[pheno_rna_placenta_stages$pregstage == "II", ] 
cat("We have",  length(unique(pheno_rna_placenta_stages_II$IDfamily)), "samples in stage II")
```

    ## We have 387 samples in stage II

``` r
placenta_ids_in_trimII <- pheno_rna_placenta_stages_II$IDfamily
```

we calculate the winsorized value for this data set

``` r
pheno_rna_placenta_stages_II$gly_trim_mg_q_pregstageMean_winsz <- DescTools::Winsorize(pheno_rna_placenta_stages_II$gly_trim_mg_q_pregstageMean, na.rm =T)
```

``` r
pheno_rna_placenta_stages_III <- pheno_rna_placenta_stages[pheno_rna_placenta_stages$pregstage == "III", ] 
cat("We have",  length(unique(pheno_rna_placenta_stages_III$IDfamily)), "samples in stage III")
```

    ## We have 397 samples in stage III

``` r
placenta_ids_in_trimIII <- pheno_rna_placenta_stages_III$IDfamily
```

we calculate the winsorized value for this data set

``` r
pheno_rna_placenta_stages_III$gly_trim_mg_q_pregstageMean_winsz <- DescTools::Winsorize(pheno_rna_placenta_stages_III$gly_trim_mg_q_pregstageMean, na.rm =T)
```

## for RNA CVS

*mg for stage I* Note that we use pregnancy stage I for CVS, as this is
what makes sense

``` r
pheno_rna_cvs_stageI <- merge(confounders_RNA_cvs, Glycyrrhizin_trim_I, by = "IDfamily")
dim(pheno_rna_cvs_stageI)
```

    ## [1] 88 95

``` r
cvs_ids_in <- pheno_rna_cvs_stageI$IDfamily
```

``` r
pheno_rna_cvs_stageI %>% select(c(gly_trim_mg_q_pregstageMean)) %>% describe()
```

    ##                             vars  n    mean      sd median trimmed     mad min
    ## gly_trim_mg_q_pregstageMean    1 88 2038.78 4872.43 999.44 1197.47 1119.67   0
    ##                                  max    range skew kurtosis    se
    ## gly_trim_mg_q_pregstageMean 42122.88 42122.88 6.67     50.1 519.4

``` r
hist(pheno_rna_cvs_stageI$gly_trim_g_q_pregstageMean)
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->
skew is 6.67

we look also again at outliers

``` r
pheno_rna_cvs_outliers_glycyrrhizin_mean_stage1 <- pheno_rna_cvs_stageI %>%
  rstatix::identify_outliers(gly_trim_mg_q_pregstageMean)

cat("We have", sum(pheno_rna_cvs_outliers_glycyrrhizin_mean_stage1$is.outlier), "outlier samples using the mean glycyrrhizin consumption in pregnancy stage I. ")
```

    ## We have 8 outlier samples using the mean glycyrrhizin consumption in pregnancy stage I.

``` r
cat("We have", sum(pheno_rna_cvs_outliers_glycyrrhizin_mean_stage1$is.extreme), "'extreme' samples using the mean glycyrrhizin consumption in pregnancy stage I.")
```

    ## We have 2 'extreme' samples using the mean glycyrrhizin consumption in pregnancy stage I.

``` r
plot_box(pheno_rna_cvs_stageI, outcome = gly_trim_mg_q_pregstageMean, xtitle = "", ytitle = "mean mg glycyrrhizin over pregnancy") +
   geom_text(position=position_dodge(width = 0.8), size = 3, vjust = -0.5, aes(x = 0,y= median(gly_trim_mg_q_pregstageMean), label = median(gly_trim_mg_q_pregstageMean)))
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

we calculate the winsorized value for this data set

``` r
pheno_rna_cvs_stageI$gly_trim_mg_q_pregstageMean_winsz <- DescTools::Winsorize(pheno_rna_cvs_stageI$gly_trim_mg_q_pregstageMean, na.rm = T)
```

``` r
pheno_rna_cvs_outliers_glycyrrhizin_mean_stage1_winsz <- pheno_rna_cvs_stageI %>%
  rstatix::identify_outliers(gly_trim_mg_q_pregstageMean_winsz)

cat("\nWe have", sum(pheno_rna_cvs_outliers_glycyrrhizin_mean_stage1_winsz$is.outlier), "outlier samples using the winsorized mean glycyrrhizin consumption in pregnancy stage I. ")
```

    ## 
    ## We have 8 outlier samples using the winsorized mean glycyrrhizin consumption in pregnancy stage I.

``` r
cat("We have", sum(pheno_rna_cvs_outliers_glycyrrhizin_mean_stage1_winsz$is.extreme), "'extreme' samples using the winsorized mean glycyrrhizin consumption in pregnancy stage I. ")
```

    ## We have 0 'extreme' samples using the winsorized mean glycyrrhizin consumption in pregnancy stage I.

We see that winsorizing helps to reduce outliers (extreme values)

``` r
pheno_rna_cvs_stageI %>% select(c(gly_trim_mg_q_pregstageMean_winsz)) %>% describe()
```

    ##                                   vars  n    mean      sd median trimmed
    ## gly_trim_mg_q_pregstageMean_winsz    1 88 1484.78 1609.29 999.44 1197.47
    ##                                       mad min     max   range skew kurtosis
    ## gly_trim_mg_q_pregstageMean_winsz 1119.67   0 5879.43 5879.43 1.48      1.4
    ##                                       se
    ## gly_trim_mg_q_pregstageMean_winsz 171.55

``` r
hist(pheno_rna_cvs_stageI$gly_trim_g_q_pregstageMean_winsz)
```

![](Script_1_General_PreProcessing_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
cat("We have available data for n =", nrow(pheno_rna_cvs_stageI), "samples for RNA CVS Analyses")
```

    ## We have available data for n = 88 samples for RNA CVS Analyses

## save phenotype data for use

``` r
save(pheno_rna_placenta_overpreg, pheno_rna_placenta_maxpreg, pheno_rna_placenta_stages_I, pheno_rna_placenta_stages_II, pheno_rna_placenta_stages_III, pheno_rna_cvs_stageI, file = here("02_Data/pheno/", "all_phenos_rna.Rdata"))

save(pheno_rna_placenta_overpreg, file = here("02_Data/pheno/", "pheno_rna_placenta_overpreg.Rdata"))
save(pheno_rna_placenta_maxpreg, file = here("02_Data/pheno/", "pheno_rna_placenta_maxpreg.Rdata"))
save(pheno_rna_cvs_stageI, file = here("02_Data/pheno/", "pheno_rna_cvs_stageI.Rdata"))
```

## RNASeq data

### load Placenta ITU

``` r
# placenta final deseq object and raw counts
load(here("02_Data/RNA/", "counts_placenta.Rdata")) # raw counts
load(here("02_Data/RNA/", "dds_final_samples_placenta.Rdata")) # deseq object with basic pre-processing done

dim(counts_placenta)
```

    ## [1] 8245  494

We have 8245 transcripts from 494 samples in in ITU term placenta.

### load CVS ITU

``` r
# cvs final deseq object and raw counts
load(here("02_Data/RNA/", "counts_cvs.Rdata")) 
load(here("02_Data/RNA/", "dds_final_samples_cvs.Rdata"))
dim(counts_cvs)
```

    ## [1] 9089  267

We have 9089 transcripts from 267 samples in ITU CVS.

## annotations

use biomaRt

``` r
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl", GRCh=37)
# Ensembl GRCh37 Feb 2014          https://grch37.ensembl.org  GRCh37     
```

note that we used GRCH37 here to have it concordant with the reference
genome for methylation data (hg19 from Illumina; GRCh 37)

``` r
save(ensembl, file = here("02_Data/RNA/", "ensembl.Rdata"))
```

### placenta

information about location and gene symbols for those genes on
chromosomes 1-22

``` r
annotation_genes_placenta_rna <- Get_Annotation_RNA(rownames(counts_placenta), ensembl)
```

    ## [1] "start length of genes: 8245"
    ##  [1] "missing: ENSG00000275052" "missing: ENSG00000285258"
    ##  [3] "missing: ENSG00000284968" "missing: ENSG00000273841"
    ##  [5] "missing: ENSG00000277443" "missing: ENSG00000274523"
    ##  [7] "missing: ENSG00000279078" "missing: ENSG00000273820"
    ##  [9] "missing: ENSG00000275342" "missing: ENSG00000281649"
    ## [11] "missing: ENSG00000280832" "missing: ENSG00000284024"
    ## [13] "missing: ENSG00000276045" "missing: ENSG00000275202"
    ## [15] "missing: ENSG00000273749" "missing: ENSG00000274383"
    ## [17] "missing: ENSG00000274180" "missing: ENSG00000273611"
    ## [19] "missing: ENSG00000278259" "missing: ENSG00000278311"
    ## [21] "missing: ENSG00000275700" "missing: ENSG00000278540"
    ## [23] "missing: ENSG00000276234" "missing: ENSG00000276023"
    ## [25] "missing: ENSG00000275066" "missing: ENSG00000278053"
    ## [27] "missing: ENSG00000278845" "missing: ENSG00000274211"
    ## [29] "missing: ENSG00000275832" "missing: ENSG00000275023"
    ## [31] "missing: ENSG00000277258" "missing: ENSG00000277791"
    ## [33] "missing: ENSG00000276293" "missing: ENSG00000273559"
    ## [35] "missing: ENSG00000274213" "missing: ENSG00000274173"
    ## [37] "missing: ENSG00000283633" "missing: ENSG00000280109"
    ## [1] "number of genes remaining: 8207"
    ## [1] "number of genes remaining with chromosomes 1-22: 7902"

``` r
# check there are no duplicated ensmebls
length(which(duplicated(annotation_genes_placenta_rna$ensembl_gene_id) == TRUE))
```

    ## [1] 0

``` r
save(annotation_genes_placenta_rna, file = here("02_Data/RNA/", "annotation_genes_placenta_rna.Rdata"))
```

filter RNA-Seq data set to include only annotated genes on chromosome
1-22

``` r
counts_placenta_filtered <- counts_placenta[rownames(counts_placenta) %in% annotation_genes_placenta_rna$ensembl_gene_id, ]
dim(counts_placenta_filtered)
```

    ## [1] 7902  494

``` r
counts_placenta_filtered <- counts_placenta_filtered[match(annotation_genes_placenta_rna$ensembl_gene_id, rownames(counts_placenta_filtered)), ]

identical(rownames(counts_placenta_filtered), annotation_genes_placenta_rna$ensembl_gene_id)
```

    ## [1] TRUE

``` r
save(counts_placenta_filtered, file = here("02_Data/RNA/", "counts_placenta_filtered.Rdata"))
```

### cvs

``` r
annotation_genes_cvs_rna <- Get_Annotation_RNA(rownames(counts_cvs), ensembl)
```

    ## [1] "start length of genes: 9089"
    ##  [1] "missing: ENSG00000277462" "missing: ENSG00000275052"
    ##  [3] "missing: ENSG00000285258" "missing: ENSG00000284968"
    ##  [5] "missing: ENSG00000273841" "missing: ENSG00000277443"
    ##  [7] "missing: ENSG00000274523" "missing: ENSG00000279078"
    ##  [9] "missing: ENSG00000273820" "missing: ENSG00000275342"
    ## [11] "missing: ENSG00000281649" "missing: ENSG00000284024"
    ## [13] "missing: ENSG00000275764" "missing: ENSG00000276045"
    ## [15] "missing: ENSG00000274605" "missing: ENSG00000273749"
    ## [17] "missing: ENSG00000275835" "missing: ENSG00000274383"
    ## [19] "missing: ENSG00000280206" "missing: ENSG00000273611"
    ## [21] "missing: ENSG00000278259" "missing: ENSG00000277161"
    ## [23] "missing: ENSG00000278311" "missing: ENSG00000275700"
    ## [25] "missing: ENSG00000278540" "missing: ENSG00000276234"
    ## [27] "missing: ENSG00000276023" "missing: ENSG00000275066"
    ## [29] "missing: ENSG00000278053" "missing: ENSG00000278845"
    ## [31] "missing: ENSG00000274211" "missing: ENSG00000275832"
    ## [33] "missing: ENSG00000275023" "missing: ENSG00000277972"
    ## [35] "missing: ENSG00000277258" "missing: ENSG00000277791"
    ## [37] "missing: ENSG00000276293" "missing: ENSG00000273559"
    ## [39] "missing: ENSG00000275632" "missing: ENSG00000275234"
    ## [41] "missing: ENSG00000274422" "missing: ENSG00000273899"
    ## [43] "missing: ENSG00000280109" "missing: ENSG00000276529"
    ## [1] "number of genes remaining: 9045"
    ## [1] "number of genes remaining with chromosomes 1-22: 8691"

``` r
# check there are no duplicated ensmebls
length(which(duplicated(annotation_genes_cvs_rna$ensembl_gene_id) == TRUE))
```

    ## [1] 0

``` r
save(annotation_genes_cvs_rna, file = here("02_Data/RNA/", "annotation_genes_cvs_rna.Rdata"))
```

filter RNA-Seq data set to include only annotated genes on chromosome
1-22

``` r
counts_cvs_filtered <- counts_cvs[rownames(counts_cvs) %in% annotation_genes_cvs_rna$ensembl_gene_id, ]
dim(counts_cvs_filtered)
```

    ## [1] 8691  267

``` r
counts_cvs_filtered <- counts_cvs_filtered[match(annotation_genes_cvs_rna$ensembl_gene_id, rownames(counts_cvs_filtered)), ]
identical(rownames(counts_cvs_filtered), annotation_genes_cvs_rna$ensembl_gene_id)
```

    ## [1] TRUE

``` r
save(counts_cvs_filtered, file = here("02_Data/RNA/", "counts_cvs_filtered.Rdata"))
```

### filter expression for samples with available phenotype

We need to subset the counts to the samples we have glycyrrhizin
information for

``` r
counts_placenta_filtered_final <- counts_placenta_filtered[ ,colnames(counts_placenta_filtered) %in% placenta_ids_in]
  # note, samples are the same for the mean/max data frames
counts_cvs_filtered_final <- counts_cvs_filtered[ ,colnames(counts_cvs_filtered) %in% cvs_ids_in]

counts_placenta_filtered_final_s1 <- counts_placenta_filtered[ ,colnames(counts_placenta_filtered) %in% placenta_ids_in_trimI]
counts_placenta_filtered_final_s2 <- counts_placenta_filtered[ ,colnames(counts_placenta_filtered) %in% placenta_ids_in_trimII]
counts_placenta_filtered_final_s3 <- counts_placenta_filtered[ ,colnames(counts_placenta_filtered) %in% placenta_ids_in_trimIII]
```

``` r
save(counts_placenta_filtered_final, counts_placenta_filtered_final_s1, counts_placenta_filtered_final_s2, counts_placenta_filtered_final_s3, counts_cvs_filtered_final, file = here("02_Data/RNA/", "all_final_counts_for_analyses.Rdata"))
```

# Note

The winsorized values were calculated for the subsets where necessary,
but only for the used variable ‘mg’ and ‘q’ for the stages data. If
other variables should be used (e.g. mg/g pwer week etc), this would
need to be checked.
