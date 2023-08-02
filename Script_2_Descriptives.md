Script_2\_Descriptives
================
Linda Dieckmann
November 2022

# load packages

``` r
library(here)
```

    ## here() starts at /Users/linda_dieckmann/Documents/Processing/P2.5_Helsinki

``` r
library(dplyr)
```

    ## 
    ## Attache Paket: 'dplyr'

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     filter, lag

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(gtsummary)
library(webshot2)
library(flextable)
```

    ## 
    ## Attache Paket: 'flextable'

    ## Die folgenden Objekte sind maskiert von 'package:gtsummary':
    ## 
    ##     as_flextable, continuous_summary

``` r
library(ggpubr)
```

    ## Lade nötiges Paket: ggplot2

    ## 
    ## Attache Paket: 'ggpubr'

    ## Die folgenden Objekte sind maskiert von 'package:flextable':
    ## 
    ##     border, font, rotate

``` r
writeLines(capture.output(sessionInfo()), here("Analyses/Scripts/", "sessionInfo_Script_2.txt"))
```

# load functions

``` r
source(here("Analyses/Scripts/", "functions.R"))
```

    ## Lade nötiges Paket: ggpp

    ## 
    ## Attache Paket: 'ggpp'

    ## Das folgende Objekt ist maskiert 'package:ggplot2':
    ## 
    ##     annotate

set summary table option for large number print style

``` r
list("style_number-arg:big.mark" = "") %>%
  set_gtsummary_theme()
```

# load data

full glycyrrhizin data

``` r
load(here("02_Data/pheno/", "glycyrrhizin_data_all.Rdata")) 
```

phenotype data matched with RNA-Seq samples data

``` r
load(here("02_Data/pheno/", "all_phenos_rna.Rdata"))
```

# overall glycyrrhizin consumption in the ITU cohort

## general, distribution of data

``` r
psych::describe(gly_mean_mg_overpregnancy_perwomen$gly_trim_mg_mean_over_pregnancy)
```

    ##    vars   n    mean      sd median trimmed    mad min      max    range skew
    ## X1    1 612 1069.11 1850.29  455.4  690.61 619.78   0 25178.28 25178.28 5.57
    ##    kurtosis    se
    ## X1    53.06 74.79

``` r
psych::describe(gly_max_mg_overpregnancy_perwomen$gly_trim_mg_max_over_pregnancy)
```

    ##    vars   n   mean      sd median trimmed     mad min      max    range skew
    ## X1    1 612 1638.1 2867.72 790.66 1064.07 1032.59   0 42122.88 42122.88 6.37
    ##    kurtosis     se
    ## X1    69.92 115.92

``` r
descriptive_table_gly_pregstages <- 
Glycyrrhizin_trim_data %>% 
  dplyr::select(c(pregstage, gly_trim_mg_q_pregstageMean)) %>% 
  describe_continuous_by_group_table(groupby = "pregstage", variable = "gly_trim_mg_q_pregstageMean", variablelabel = "Glycyrrhizin consumption in mg", headerlabel = "pregnancy stage")

descriptive_table_gly_pregstages
```

<div id="asspeoitgj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#asspeoitgj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#asspeoitgj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#asspeoitgj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#asspeoitgj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#asspeoitgj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#asspeoitgj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#asspeoitgj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#asspeoitgj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#asspeoitgj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#asspeoitgj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#asspeoitgj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#asspeoitgj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#asspeoitgj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#asspeoitgj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#asspeoitgj .gt_from_md > :first-child {
  margin-top: 0;
}

#asspeoitgj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#asspeoitgj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#asspeoitgj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#asspeoitgj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#asspeoitgj .gt_row_group_first td {
  border-top-width: 2px;
}

#asspeoitgj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#asspeoitgj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#asspeoitgj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#asspeoitgj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#asspeoitgj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#asspeoitgj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#asspeoitgj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#asspeoitgj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#asspeoitgj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#asspeoitgj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#asspeoitgj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#asspeoitgj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#asspeoitgj .gt_left {
  text-align: left;
}

#asspeoitgj .gt_center {
  text-align: center;
}

#asspeoitgj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#asspeoitgj .gt_font_normal {
  font-weight: normal;
}

#asspeoitgj .gt_font_bold {
  font-weight: bold;
}

#asspeoitgj .gt_font_italic {
  font-style: italic;
}

#asspeoitgj .gt_super {
  font-size: 65%;
}

#asspeoitgj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#asspeoitgj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#asspeoitgj .gt_indent_1 {
  text-indent: 5px;
}

#asspeoitgj .gt_indent_2 {
  text-indent: 10px;
}

#asspeoitgj .gt_indent_3 {
  text-indent: 15px;
}

#asspeoitgj .gt_indent_4 {
  text-indent: 20px;
}

#asspeoitgj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="pregnancy stage">pregnancy stage</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;&lt;br&gt;N = 415"><strong>I</strong><br>N = 415</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;&lt;br&gt;N = 538"><strong>II</strong><br>N = 538</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;&lt;br&gt;N = 559"><strong>III</strong><br>N = 559</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Glycyrrhizin consumption in mg</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean (SD)</td>
<td headers="stat_1" class="gt_row gt_center">1341 (2808)</td>
<td headers="stat_2" class="gt_row gt_center">991 (1934)</td>
<td headers="stat_3" class="gt_row gt_center">879 (1614)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_1" class="gt_row gt_center">570</td>
<td headers="stat_2" class="gt_row gt_center">338</td>
<td headers="stat_3" class="gt_row gt_center">260</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    IQR</td>
<td headers="stat_1" class="gt_row gt_center">124, 1485</td>
<td headers="stat_2" class="gt_row gt_center">40, 1037</td>
<td headers="stat_3" class="gt_row gt_center">12, 943</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Range</td>
<td headers="stat_1" class="gt_row gt_center">0, 42123</td>
<td headers="stat_2" class="gt_row gt_center">0, 20316</td>
<td headers="stat_3" class="gt_row gt_center">0, 13637</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
descriptive_table_gly_pregstages %>%
  gtsummary::as_flex_table() %>%
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::fontsize(size = 11, part = "all") %>% 
  save_as_docx(., path = here("Analyses/Results/", "descriptive_table_glycyrrhizin_stages.docx"))
```

## consumption over pregnancy stages

- note that we winsorized separately by pregnancy stage

*plot*

``` r
boxplot_gly_pregstages <- plotbox_grouped_pregstage(data = Glycyrrhizin_trim_data, outcome = gly_trim_mg_q_pregstageMean_winsz_bypregstage, ytitle = "maternal glycyrrhizin consumption in mg\n (winsorized by pregnancy stage) \n")
boxplot_gly_pregstages
```

![](Script_2_Descriptives_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
Glycyrrhizin_trim_data %>% 
  dplyr::select(c(pregstage, gly_trim_mg_q_pregstageMean_winsz_bypregstage)) %>% 
  describe_continuous_by_group_table(groupby = "pregstage", variable = "gly_trim_mg_q_pregstageMean_winsz_bypregstage", variablelabel = "glycyrrhizin consumption in mg (winsorized)", headerlabel = "pregnancy stage")
```

<div id="rkjeysowbc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rkjeysowbc .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rkjeysowbc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rkjeysowbc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rkjeysowbc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rkjeysowbc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rkjeysowbc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rkjeysowbc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rkjeysowbc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rkjeysowbc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rkjeysowbc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rkjeysowbc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rkjeysowbc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rkjeysowbc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rkjeysowbc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rkjeysowbc .gt_from_md > :first-child {
  margin-top: 0;
}

#rkjeysowbc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rkjeysowbc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rkjeysowbc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rkjeysowbc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rkjeysowbc .gt_row_group_first td {
  border-top-width: 2px;
}

#rkjeysowbc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rkjeysowbc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rkjeysowbc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rkjeysowbc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rkjeysowbc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rkjeysowbc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rkjeysowbc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rkjeysowbc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rkjeysowbc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rkjeysowbc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rkjeysowbc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rkjeysowbc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rkjeysowbc .gt_left {
  text-align: left;
}

#rkjeysowbc .gt_center {
  text-align: center;
}

#rkjeysowbc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rkjeysowbc .gt_font_normal {
  font-weight: normal;
}

#rkjeysowbc .gt_font_bold {
  font-weight: bold;
}

#rkjeysowbc .gt_font_italic {
  font-style: italic;
}

#rkjeysowbc .gt_super {
  font-size: 65%;
}

#rkjeysowbc .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#rkjeysowbc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rkjeysowbc .gt_indent_1 {
  text-indent: 5px;
}

#rkjeysowbc .gt_indent_2 {
  text-indent: 10px;
}

#rkjeysowbc .gt_indent_3 {
  text-indent: 15px;
}

#rkjeysowbc .gt_indent_4 {
  text-indent: 20px;
}

#rkjeysowbc .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="pregnancy stage">pregnancy stage</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;I&lt;/strong&gt;&lt;br&gt;N = 415"><strong>I</strong><br>N = 415</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;II&lt;/strong&gt;&lt;br&gt;N = 538"><strong>II</strong><br>N = 538</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;III&lt;/strong&gt;&lt;br&gt;N = 559"><strong>III</strong><br>N = 559</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">glycyrrhizin consumption in mg (winsorized)</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean (SD)</td>
<td headers="stat_1" class="gt_row gt_center">1122 (1426)</td>
<td headers="stat_2" class="gt_row gt_center">825 (1151)</td>
<td headers="stat_3" class="gt_row gt_center">748 (1084)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_1" class="gt_row gt_center">570</td>
<td headers="stat_2" class="gt_row gt_center">338</td>
<td headers="stat_3" class="gt_row gt_center">260</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    IQR</td>
<td headers="stat_1" class="gt_row gt_center">124, 1485</td>
<td headers="stat_2" class="gt_row gt_center">40, 1037</td>
<td headers="stat_3" class="gt_row gt_center">12, 943</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Range</td>
<td headers="stat_1" class="gt_row gt_center">0, 5339</td>
<td headers="stat_2" class="gt_row gt_center">0, 4148</td>
<td headers="stat_3" class="gt_row gt_center">0, 3946</td></tr>
  </tbody>
  
  
</table>
</div>

## consumption over years

``` r
boxplot_gly_birthyears <- plot_box(gly_mean_mg_overpregnancy_perwomen, as.factor(Child_Birth_Year), gly_trim_mg_mean_over_pregnancy_winsz, xtitle = "child birth year", ytitle = "average maternal glycyrrhizin consumption in mg \n (winsorized) during pregnancy\n") 
boxplot_gly_birthyears
```

![](Script_2_Descriptives_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
cor.test(gly_mean_mg_overpregnancy_perwomen$gly_trim_mg_mean_over_pregnancy_winsz, gly_mean_mg_overpregnancy_perwomen$Child_Birth_Year, method = "spearman")
```

    ## Warning in
    ## cor.test.default(gly_mean_mg_overpregnancy_perwomen$gly_trim_mg_mean_over_pregnancy_winsz,
    ## : Kann exakten p-Wert bei Bindungen nicht berechnen

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  gly_mean_mg_overpregnancy_perwomen$gly_trim_mg_mean_over_pregnancy_winsz and gly_mean_mg_overpregnancy_perwomen$Child_Birth_Year
    ## S = 49481293, p-value = 8.996e-14
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## -0.295207

``` r
gly_mean_mg_overpregnancy_perwomen %>% 
  dplyr::select(c(Child_Birth_Year, gly_trim_mg_mean_over_pregnancy_winsz)) %>% 
  mutate(Child_Birth_Year = factor(Child_Birth_Year)) %>% 
describe_continuous_by_group_table(groupby = "Child_Birth_Year", variable = "gly_trim_mg_mean_over_pregnancy_winsz", variablelabel = "mean mg glycyrrhizin consumption over pregnancy (winsorized)", headerlabel = "birth year")
```

<div id="lwdevkgtmm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lwdevkgtmm .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lwdevkgtmm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lwdevkgtmm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lwdevkgtmm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lwdevkgtmm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lwdevkgtmm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lwdevkgtmm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lwdevkgtmm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lwdevkgtmm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lwdevkgtmm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lwdevkgtmm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lwdevkgtmm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lwdevkgtmm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lwdevkgtmm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lwdevkgtmm .gt_from_md > :first-child {
  margin-top: 0;
}

#lwdevkgtmm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lwdevkgtmm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lwdevkgtmm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lwdevkgtmm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lwdevkgtmm .gt_row_group_first td {
  border-top-width: 2px;
}

#lwdevkgtmm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lwdevkgtmm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lwdevkgtmm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lwdevkgtmm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lwdevkgtmm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lwdevkgtmm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lwdevkgtmm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lwdevkgtmm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lwdevkgtmm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lwdevkgtmm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lwdevkgtmm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lwdevkgtmm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lwdevkgtmm .gt_left {
  text-align: left;
}

#lwdevkgtmm .gt_center {
  text-align: center;
}

#lwdevkgtmm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lwdevkgtmm .gt_font_normal {
  font-weight: normal;
}

#lwdevkgtmm .gt_font_bold {
  font-weight: bold;
}

#lwdevkgtmm .gt_font_italic {
  font-style: italic;
}

#lwdevkgtmm .gt_super {
  font-size: 65%;
}

#lwdevkgtmm .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#lwdevkgtmm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lwdevkgtmm .gt_indent_1 {
  text-indent: 5px;
}

#lwdevkgtmm .gt_indent_2 {
  text-indent: 10px;
}

#lwdevkgtmm .gt_indent_3 {
  text-indent: 15px;
}

#lwdevkgtmm .gt_indent_4 {
  text-indent: 20px;
}

#lwdevkgtmm .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="birth year">birth year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2012&lt;/strong&gt;&lt;br&gt;N = 14"><strong>2012</strong><br>N = 14</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2013&lt;/strong&gt;&lt;br&gt;N = 39"><strong>2013</strong><br>N = 39</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2014&lt;/strong&gt;&lt;br&gt;N = 54"><strong>2014</strong><br>N = 54</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2015&lt;/strong&gt;&lt;br&gt;N = 163"><strong>2015</strong><br>N = 163</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2016&lt;/strong&gt;&lt;br&gt;N = 160"><strong>2016</strong><br>N = 160</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2017&lt;/strong&gt;&lt;br&gt;N = 182"><strong>2017</strong><br>N = 182</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">mean mg glycyrrhizin consumption over pregnancy (winsorized)</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td>
<td headers="stat_4" class="gt_row gt_center"></td>
<td headers="stat_5" class="gt_row gt_center"></td>
<td headers="stat_6" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mean (SD)</td>
<td headers="stat_1" class="gt_row gt_center">2000 (1654)</td>
<td headers="stat_2" class="gt_row gt_center">1268 (1190)</td>
<td headers="stat_3" class="gt_row gt_center">1169 (1134)</td>
<td headers="stat_4" class="gt_row gt_center">1130 (1195)</td>
<td headers="stat_5" class="gt_row gt_center">889 (1142)</td>
<td headers="stat_6" class="gt_row gt_center">543 (864)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Median</td>
<td headers="stat_1" class="gt_row gt_center">1407</td>
<td headers="stat_2" class="gt_row gt_center">926</td>
<td headers="stat_3" class="gt_row gt_center">938</td>
<td headers="stat_4" class="gt_row gt_center">700</td>
<td headers="stat_5" class="gt_row gt_center">361</td>
<td headers="stat_6" class="gt_row gt_center">247</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    IQR</td>
<td headers="stat_1" class="gt_row gt_center">562, 3657</td>
<td headers="stat_2" class="gt_row gt_center">333, 1985</td>
<td headers="stat_3" class="gt_row gt_center">259, 1528</td>
<td headers="stat_4" class="gt_row gt_center">201, 1678</td>
<td headers="stat_5" class="gt_row gt_center">94, 1344</td>
<td headers="stat_6" class="gt_row gt_center">65, 619</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Range</td>
<td headers="stat_1" class="gt_row gt_center">0, 4086</td>
<td headers="stat_2" class="gt_row gt_center">0, 4086</td>
<td headers="stat_3" class="gt_row gt_center">0, 4086</td>
<td headers="stat_4" class="gt_row gt_center">0, 4086</td>
<td headers="stat_5" class="gt_row gt_center">0, 4086</td>
<td headers="stat_6" class="gt_row gt_center">0, 4086</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
ggsave(here("Analyses/Results/", "glycyrrhizin_pregnancy_stages_years.pdf"),
       ggarrange(boxplot_gly_pregstages, boxplot_gly_birthyears, ncol=2, labels = c("A", "B")),
       width=standard_width_mm_double, height=standard_height_mm/5, units="mm", dpi=600, scale=2, device = cairo_pdf)
```

# descriptives for samples used for differential expression analysis

We want to report descriptives for CVS vs. RNA data set We check for
relationship between glycyrrhizin and other variables

*prepare relevant data*

``` r
pheno_rna_placenta_overpreg_prep <- pheno_rna_placenta_overpreg %>%
  dplyr::select(IDfamily, Maternal_Age_Years, Maternal_Body_Mass_Index_in_Early_Pregnancy, Maternal_Diabetes_dichotom, Maternal_Hypertension_dichotom, smoking_dichotom, Parity_dichotom, Delivery_mode_3l, Child_Sex, Gestational_Age_Weeks, Child_Birth_Weight, SV1, gly_trim_mg_mean_over_pregnancy_winsz)
pheno_rna_placenta_overpreg_prep$Child_Birth_Weight <- pheno_rna_placenta_overpreg_prep$Child_Birth_Weight / 1000
```

``` r
attr(pheno_rna_placenta_overpreg_prep$IDfamily, "label") <- "ID" 
attr(pheno_rna_placenta_overpreg_prep$Maternal_Age_Years, "label") <- "Maternal Age (years)" 
attr(pheno_rna_placenta_overpreg_prep$Maternal_Body_Mass_Index_in_Early_Pregnancy, "label") <- "Early pregnancy BMI" 
attr(pheno_rna_placenta_overpreg_prep$Maternal_Diabetes_dichotom, "label") <- "Diabetes Disorders in pregnancy" 
attr(pheno_rna_placenta_overpreg_prep$Maternal_Hypertension_dichotom, "label") <- "Hypertensive Disorders in pregnancy" 
attr(pheno_rna_placenta_overpreg_prep$smoking_dichotom, "label") <- "Maternal Smoking" 
attr(pheno_rna_placenta_overpreg_prep$Parity_dichotom, "label") <- "Parity" 
attr(pheno_rna_placenta_overpreg_prep$Delivery_mode_3l, "label") <- "Delivery mode" 
attr(pheno_rna_placenta_overpreg_prep$Child_Sex, "label") <- "Child Sex" 
attr(pheno_rna_placenta_overpreg_prep$Gestational_Age_Weeks, "label") <- "Gestational Age (weeks)" 
attr(pheno_rna_placenta_overpreg_prep$Child_Birth_Weight, "label") <- "Birth Weight (kg)" 
attr(pheno_rna_placenta_overpreg_prep$SV1, "label") <- "SV" 
attr(pheno_rna_placenta_overpreg_prep$gly_trim_mg_mean_over_pregnancy_winsz, "label") <- "Glycyrrhizin consumption in mg (winsorized)" 
names(pheno_rna_placenta_overpreg_prep)[names(pheno_rna_placenta_overpreg_prep) == 'gly_trim_mg_mean_over_pregnancy_winsz'] <- 'glycyrrhizin_mg'
```

``` r
pheno_rna_cvs_stageI_prep <- pheno_rna_cvs_stageI %>%
  dplyr::select(IDfamily, Maternal_Age_Years, Maternal_Body_Mass_Index_in_Early_Pregnancy, Maternal_Diabetes_dichotom, Maternal_Hypertension_dichotom, smoking_dichotom, Parity_dichotom, Delivery_mode_3l, Child_Sex, gestage_at_CVS_weeks, Child_Birth_Weight, SV1, gly_trim_mg_q_pregstageMean_winsz)   
pheno_rna_cvs_stageI_prep$Child_Birth_Weight <- pheno_rna_cvs_stageI_prep$Child_Birth_Weight / 1000
```

``` r
attr(pheno_rna_cvs_stageI_prep$IDfamily, "label") <- "ID" 
attr(pheno_rna_cvs_stageI_prep$Maternal_Age_Years, "label") <- "Maternal Age (years)" 
attr(pheno_rna_cvs_stageI_prep$Maternal_Body_Mass_Index_in_Early_Pregnancy, "label") <- "Early pregnancy BMI" 
attr(pheno_rna_cvs_stageI_prep$Maternal_Diabetes_dichotom, "label") <- "Diabetes Disorders in pregnancy" 
attr(pheno_rna_cvs_stageI_prep$Maternal_Hypertension_dichotom, "label") <- "Hypertensive Disorders in pregnancy" 
attr(pheno_rna_cvs_stageI_prep$smoking_dichotom, "label") <- "Maternal Smoking" 
attr(pheno_rna_cvs_stageI_prep$Parity_dichotom, "label") <- "Parity" 
attr(pheno_rna_cvs_stageI_prep$Delivery_mode_3l, "label") <- "Delivery mode" 
attr(pheno_rna_cvs_stageI_prep$Child_Sex, "label") <- "Child Sex" 
attr(pheno_rna_cvs_stageI_prep$gestage_at_CVS_weeks, "label") <- "Gestational Age at CVS sampling (weeks)" 
names(pheno_rna_cvs_stageI_prep)[names(pheno_rna_cvs_stageI_prep) == 'gestage_at_CVS_weeks'] <- 'Gestational_Age_Weeks'
attr(pheno_rna_cvs_stageI_prep$Child_Birth_Weight, "label") <- "Birth Weight (kg)" 
attr(pheno_rna_cvs_stageI_prep$SV1, "label") <- "SV" 
attr(pheno_rna_cvs_stageI_prep$gly_trim_mg_q_pregstageMean_winsz, "label") <- "Glycyrrhizin consumption in mg (winsorized)" 
names(pheno_rna_cvs_stageI_prep)[names(pheno_rna_cvs_stageI_prep) == 'gly_trim_mg_q_pregstageMean_winsz'] <- 'glycyrrhizin_mg'
```

``` r
psych::describe(pheno_rna_cvs_stageI_prep$Gestational_Age_Weeks)
```

    ##    vars  n  mean   sd median trimmed  mad min   max range skew kurtosis   se
    ## X1    1 88 12.71 0.85  12.86   12.75 1.06  11 13.86  2.86 -0.4    -1.13 0.09

We combine Placenta and CVS phenotypes in one data set for visualization

``` r
pheno_rna_placenta_overpreg_prep$Tissue <- "Placenta"
pheno_rna_cvs_stageI_prep$Tissue <- "CVS"
```

``` r
pheno_rna_cvs_placenta <- rbind(pheno_rna_placenta_overpreg_prep, pheno_rna_cvs_stageI_prep)

attr(pheno_rna_placenta_overpreg_prep$Gestational_Age_Weeks, "label") <- "Gestational Age (weeks)" 
attr(pheno_rna_cvs_placenta$Maternal_Diabetes_dichotom, "label") <- "Diabetes Disorders in current pregnancy" 
attr(pheno_rna_cvs_placenta$Maternal_Hypertension_dichotom, "label") <- "Hypertensive Disorders in current pregnancy" 
attr(pheno_rna_cvs_placenta$smoking_dichotom, "label") <- "Maternal Smoking during pregnancy" 
attr(pheno_rna_cvs_placenta$Parity_dichotom, "label") <- "Parity" 
attr(pheno_rna_cvs_placenta$Delivery_mode_3l, "label") <- "Delivery mode" 
attr(pheno_rna_cvs_placenta$Child_Sex, "label") <- "Child Sex" 

levels(pheno_rna_cvs_placenta$Maternal_Diabetes_dichotom) <- c("No", "Yes")
levels(pheno_rna_cvs_placenta$Maternal_Hypertension_dichotom) <- c("No", "Yes")
levels(pheno_rna_cvs_placenta$Parity_dichotom) <- c("Primiparous", "Multiparous")
levels(pheno_rna_cvs_placenta$Delivery_mode_3l) <- c("Vaginal", "Planned Caesarian Section", "Urgent Caesarian Section")
levels(pheno_rna_cvs_placenta$Child_Sex) <- c("Male", "Female")
```

## Placenta & CVS data set descriptive table

(those samples with glycyrrhizin and RNA-Seq data available,
respectively)

``` r
descriptive_table_cvs_placenta_samples <- pheno_rna_cvs_placenta %>% 
  dplyr::select(-c(IDfamily, SV1)) %>% 
  tbl_summary(by = Tissue,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 0))

descriptive_table_cvs_placenta_samples
```

<div id="gfeqvmguvj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gfeqvmguvj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gfeqvmguvj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gfeqvmguvj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gfeqvmguvj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gfeqvmguvj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gfeqvmguvj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gfeqvmguvj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gfeqvmguvj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gfeqvmguvj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gfeqvmguvj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gfeqvmguvj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gfeqvmguvj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gfeqvmguvj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gfeqvmguvj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gfeqvmguvj .gt_from_md > :first-child {
  margin-top: 0;
}

#gfeqvmguvj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gfeqvmguvj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gfeqvmguvj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gfeqvmguvj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gfeqvmguvj .gt_row_group_first td {
  border-top-width: 2px;
}

#gfeqvmguvj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gfeqvmguvj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gfeqvmguvj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gfeqvmguvj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gfeqvmguvj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gfeqvmguvj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gfeqvmguvj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gfeqvmguvj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gfeqvmguvj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gfeqvmguvj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gfeqvmguvj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gfeqvmguvj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gfeqvmguvj .gt_left {
  text-align: left;
}

#gfeqvmguvj .gt_center {
  text-align: center;
}

#gfeqvmguvj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gfeqvmguvj .gt_font_normal {
  font-weight: normal;
}

#gfeqvmguvj .gt_font_bold {
  font-weight: bold;
}

#gfeqvmguvj .gt_font_italic {
  font-style: italic;
}

#gfeqvmguvj .gt_super {
  font-size: 65%;
}

#gfeqvmguvj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#gfeqvmguvj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gfeqvmguvj .gt_indent_1 {
  text-indent: 5px;
}

#gfeqvmguvj .gt_indent_2 {
  text-indent: 10px;
}

#gfeqvmguvj .gt_indent_3 {
  text-indent: 15px;
}

#gfeqvmguvj .gt_indent_4 {
  text-indent: 20px;
}

#gfeqvmguvj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;CVS&lt;/strong&gt;, N = 88&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>CVS</strong>, N = 88<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Placenta&lt;/strong&gt;, N = 425&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Placenta</strong>, N = 425<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Maternal Age (years)</td>
<td headers="stat_1" class="gt_row gt_center">35.85 (5.31)</td>
<td headers="stat_2" class="gt_row gt_center">34.51 (4.60)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Early pregnancy BMI</td>
<td headers="stat_1" class="gt_row gt_center">23.71 (3.85)</td>
<td headers="stat_2" class="gt_row gt_center">23.73 (4.11)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Diabetes Disorders in current pregnancy</td>
<td headers="stat_1" class="gt_row gt_center">20 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">89 (21%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Hypertensive Disorders in current pregnancy</td>
<td headers="stat_1" class="gt_row gt_center">7 (8%)</td>
<td headers="stat_2" class="gt_row gt_center">21 (5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Maternal Smoking during pregnancy</td>
<td headers="stat_1" class="gt_row gt_center">6 (7%)</td>
<td headers="stat_2" class="gt_row gt_center">14 (3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Parity</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Primiparous</td>
<td headers="stat_1" class="gt_row gt_center">37 (42%)</td>
<td headers="stat_2" class="gt_row gt_center">231 (54%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Multiparous</td>
<td headers="stat_1" class="gt_row gt_center">51 (58%)</td>
<td headers="stat_2" class="gt_row gt_center">194 (46%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Delivery mode</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Vaginal</td>
<td headers="stat_1" class="gt_row gt_center">72 (82%)</td>
<td headers="stat_2" class="gt_row gt_center">352 (83%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Planned Caesarian Section</td>
<td headers="stat_1" class="gt_row gt_center">7 (8%)</td>
<td headers="stat_2" class="gt_row gt_center">27 (6%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Urgent Caesarian Section</td>
<td headers="stat_1" class="gt_row gt_center">9 (10%)</td>
<td headers="stat_2" class="gt_row gt_center">46 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Child Sex</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_1" class="gt_row gt_center">47 (53%)</td>
<td headers="stat_2" class="gt_row gt_center">222 (52%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_1" class="gt_row gt_center">41 (47%)</td>
<td headers="stat_2" class="gt_row gt_center">203 (48%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Gestational Age (weeks)</td>
<td headers="stat_1" class="gt_row gt_center">12.71 (0.85)</td>
<td headers="stat_2" class="gt_row gt_center">40.09 (1.38)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Birth Weight (kg)</td>
<td headers="stat_1" class="gt_row gt_center">3.50 (0.51)</td>
<td headers="stat_2" class="gt_row gt_center">3.56 (0.47)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Glycyrrhizin consumption in mg (winsorized)</td>
<td headers="stat_1" class="gt_row gt_center">1484.78 (1609.29)</td>
<td headers="stat_2" class="gt_row gt_center">819.95 (985.37)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Mean (SD); n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
descriptive_table_cvs_placenta_samples %>%
  as_flex_table() %>%
  flextable::font(fontname = "Arial", part = "all") %>%
  fontsize(size = 11, part = "all") %>% 
  save_as_docx(., path = here("Analyses/Results/", "descriptive_table_cvs_placenta_phenos.docx"))

# to save as png:
 # %>%
 #  as_gt() %>%
 #  gt::tab_options(table.font.names = "Arial", table.font.size = 11) %>% 
 #  gt::gtsave(., here("Analyses/Results/", "example.png"))
```

## Spearman correlations between Glycyrrhizin and continous variables

### numeric, correlation:

``` r
# extract variables
i_num_phenos <- sapply(pheno_rna_cvs_placenta, is.numeric)
y_gly_mean <- "glycyrrhizin_mg"
x_num_phenos <- setdiff(names(pheno_rna_cvs_placenta)[i_num_phenos], y_gly_mean)
vars_gly_mean <- data.frame(v1 = y_gly_mean, v2 = x_num_phenos)
```

``` r
corrs_gly_mean_numeric_CVS = do.call(rbind, mapply(corrFunc_Spearman, vars_gly_mean[,1], vars_gly_mean[,2], MoreArgs=list(data=pheno_rna_cvs_placenta[pheno_rna_cvs_placenta$Tissue == "CVS",]), SIMPLIFY=FALSE))
```

``` r
corrs_gly_mean_numeric_Placenta = do.call(rbind, mapply(corrFunc_Spearman, vars_gly_mean[,1], vars_gly_mean[,2], MoreArgs=list(data=pheno_rna_cvs_placenta[pheno_rna_cvs_placenta$Tissue == "Placenta",]), SIMPLIFY=FALSE))
```

``` r
colnames(corrs_gly_mean_numeric_Placenta) <- paste(colnames(corrs_gly_mean_numeric_Placenta), "placenta", sep = "_")
colnames(corrs_gly_mean_numeric_CVS) <- paste(colnames(corrs_gly_mean_numeric_CVS), "cvs", sep = "_")
```

``` r
corrs_gly_mean_numeric <- cbind(corrs_gly_mean_numeric_CVS[,c("estimate_cvs", "p.value_cvs", "statistic_cvs")], corrs_gly_mean_numeric_Placenta[,c("var2_placenta", "estimate_placenta", "p.value_placenta", "statistic_placenta")])
```

``` r
corrs_gly_mean_numeric[corrs_gly_mean_numeric$p.value_placenta < 0.05 | corrs_gly_mean_numeric$p.value_cvs < 0.05, ]
```

    ##                  estimate_cvs p.value_cvs statistic_cvs
    ## glycyrrhizin_mg1  -0.06968107   0.5188592      121477.3
    ##                                                var2_placenta estimate_placenta
    ## glycyrrhizin_mg1 Maternal_Body_Mass_Index_in_Early_Pregnancy         0.1452875
    ##                  p.value_placenta statistic_placenta
    ## glycyrrhizin_mg1      0.002679466           10935363

### categorical, differences in glycyrrhizin between categories:

``` r
Gly_cat_CVS_Table <-
  pheno_rna_cvs_placenta %>%
  filter(Tissue == "CVS") %>%
  tbl_continuous(
    variable = glycyrrhizin_mg,
    include = c(Maternal_Diabetes_dichotom, Maternal_Hypertension_dichotom, smoking_dichotom, Parity_dichotom, Delivery_mode_3l, Child_Sex),
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
```

    ## Warning for variable 'Child_Sex':
    ## simpleWarning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): kann bei Bindungen keinen exakten p-Wert Berechnen

``` r
Gly_cat_Placenta_Table <-
  pheno_rna_cvs_placenta %>%
  filter(Tissue == "Placenta") %>%
  tbl_continuous(
    variable = glycyrrhizin_mg,
    include = c(Maternal_Diabetes_dichotom, Maternal_Hypertension_dichotom, smoking_dichotom, Parity_dichotom, Delivery_mode_3l, Child_Sex),
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
```

``` r
tbl_merge(list(Gly_cat_CVS_Table, Gly_cat_Placenta_Table), tab_spanner = c("Placenta", "CVS"))
```

<div id="xjckhxazko" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xjckhxazko .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xjckhxazko .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xjckhxazko .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xjckhxazko .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xjckhxazko .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xjckhxazko .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xjckhxazko .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xjckhxazko .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xjckhxazko .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xjckhxazko .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xjckhxazko .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xjckhxazko .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xjckhxazko .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#xjckhxazko .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xjckhxazko .gt_from_md > :first-child {
  margin-top: 0;
}

#xjckhxazko .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xjckhxazko .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xjckhxazko .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#xjckhxazko .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#xjckhxazko .gt_row_group_first td {
  border-top-width: 2px;
}

#xjckhxazko .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xjckhxazko .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xjckhxazko .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xjckhxazko .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xjckhxazko .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xjckhxazko .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xjckhxazko .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xjckhxazko .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xjckhxazko .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xjckhxazko .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xjckhxazko .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xjckhxazko .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xjckhxazko .gt_left {
  text-align: left;
}

#xjckhxazko .gt_center {
  text-align: center;
}

#xjckhxazko .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xjckhxazko .gt_font_normal {
  font-weight: normal;
}

#xjckhxazko .gt_font_bold {
  font-weight: bold;
}

#xjckhxazko .gt_font_italic {
  font-style: italic;
}

#xjckhxazko .gt_super {
  font-size: 65%;
}

#xjckhxazko .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#xjckhxazko .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xjckhxazko .gt_indent_1 {
  text-indent: 5px;
}

#xjckhxazko .gt_indent_2 {
  text-indent: 10px;
}

#xjckhxazko .gt_indent_3 {
  text-indent: 15px;
}

#xjckhxazko .gt_indent_4 {
  text-indent: 20px;
}

#xjckhxazko .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Placenta">
        <span class="gt_column_spanner">Placenta</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="CVS">
        <span class="gt_column_spanner">CVS</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 88&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>N = 88</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 425&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>N = 425</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Diabetes Disorders in current pregnancy</td>
<td headers="stat_0_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center">0.71</td>
<td headers="stat_0_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center">0.58</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0_1" class="gt_row gt_center">1103 (364, 2006)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">433 (130, 1111)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0_1" class="gt_row gt_center">714 (322, 1864)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">424 (95, 1061)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Hypertensive Disorders in current pregnancy</td>
<td headers="stat_0_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center">0.98</td>
<td headers="stat_0_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center">0.74</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0_1" class="gt_row gt_center">1013 (362, 1970)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">433 (125, 1083)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0_1" class="gt_row gt_center">589 (345, 2345)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">186 (49, 1529)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Maternal Smoking during pregnancy</td>
<td headers="stat_0_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center">0.28</td>
<td headers="stat_0_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center">0.019</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    no</td>
<td headers="stat_0_1" class="gt_row gt_center">959 (347, 1924)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">415 (112, 1063)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    yes</td>
<td headers="stat_0_1" class="gt_row gt_center">1805 (777, 4976)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">1011 (651, 2378)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Parity</td>
<td headers="stat_0_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center">0.83</td>
<td headers="stat_0_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center">0.053</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Primiparous</td>
<td headers="stat_0_1" class="gt_row gt_center">986 (429, 2267)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">394 (107, 886)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Multiparous</td>
<td headers="stat_0_1" class="gt_row gt_center">1060 (340, 1845)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">499 (143, 1525)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Delivery mode</td>
<td headers="stat_0_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center">0.55</td>
<td headers="stat_0_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center">0.91</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Vaginal</td>
<td headers="stat_0_1" class="gt_row gt_center">959 (357, 1831)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">450 (112, 1108)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Planned Caesarian Section</td>
<td headers="stat_0_1" class="gt_row gt_center">1490 (409, 4232)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">450 (139, 1208)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Urgent Caesarian Section</td>
<td headers="stat_0_1" class="gt_row gt_center">1269 (490, 2397)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">362 (118, 876)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Child Sex</td>
<td headers="stat_0_1" class="gt_row gt_center"></td>
<td headers="p.value_1" class="gt_row gt_center">0.55</td>
<td headers="stat_0_2" class="gt_row gt_center"></td>
<td headers="p.value_2" class="gt_row gt_center">0.81</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_0_1" class="gt_row gt_center">1146 (439, 2161)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">400 (113, 1160)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_0_1" class="gt_row gt_center">933 (318, 1721)</td>
<td headers="p.value_1" class="gt_row gt_center"></td>
<td headers="stat_0_2" class="gt_row gt_center">457 (130, 944)</td>
<td headers="p.value_2" class="gt_row gt_center"></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">1</sup> Glycyrrhizin consumption in mg (winsorized): Median (IQR)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="5"><sup class="gt_footnote_marks">2</sup> Wilcoxon rank sum test; Kruskal-Wallis rank sum test</td>
    </tr>
  </tfoot>
</table>
</div>
