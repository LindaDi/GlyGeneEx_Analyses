########################################################################
## Title: This Script defines some additional functions 
## Date: 2022
## Author: Linda Dieckmann
########################################################################
library(ggplot2)
library(ggpmisc)
library(gtsummary)
library(biomaRt)


# define some figure parameters
standard_textsize <- 8
standard_dpi_fig <- 600
standard_width_mm_single <- 83
standard_width_mm_double <- 147
standard_height_mm <- 230



## Annotations with biomaRt
Get_Annotation_RNA <- function(gene_names_in_rna_vec, ensembl){
  # takes as input a vector of ensembl gene names and a biomaRt ensembl object and gives out genes with available location information
  # print number of genes in the beginning
  print(paste0("start length of genes: ", length(gene_names_in_rna_vec)))
  # get position of genes using biomaRt
  annotation_genes_rna <- getBM(
    attributes = c("ensembl_gene_id", "chromosome_name", "start_position", "end_position", "hgnc_symbol"), 
    filters = "ensembl_gene_id",
    values = gene_names_in_rna_vec,
    mart = ensembl) %>% unique()
  
  # We use the getBM() function, which has four main arguments: 
  # * attributes: is a vector of attributes that one wants to retrieve (= the output of the query). 
  # * filters: is a vector of filters that one wil use as input to the query. 
  # * values: a vector of values for the filters. In case multple filters are in use, the values argument requires a list of values where each position in the list corresponds to the position of the filters in the filters argument. 
  # * mart: is an object of class Mart, which is created by the useEnsembl() function.
  
  # print genes not found and number of rows
  na_genes <- setdiff(gene_names_in_rna_vec,annotation_genes_rna$ensembl_gene_id)
  print(paste0("missing: ",na_genes))
  print(paste0("number of genes remaining: ",nrow(annotation_genes_rna)))
  
  # filter for chromosomes = include only 1-22
  chromosome_list <- as.character(1:22)
  annotation_genes_rna <- annotation_genes_rna[annotation_genes_rna$chromosome_name %in% chromosome_list, ]
  print(paste0("number of genes remaining with chromosomes 1-22: ",nrow(annotation_genes_rna)))
  
  return(annotation_genes_rna)
}


#' Detect outliers using IQR method
#' 
#' @param x A numeric vector
#' @param na.rm Whether to exclude NAs when computing quantiles
#' 
is_outlier <- function(x, na.rm = FALSE) {
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  # Return logical vector
  x > extreme.threshold.upper | x < extreme.threshold.lower
}

## to extract correlation coefficient and p-values for Spearman
corrFunc_Spearman <- function(var1, var2, data) {
  result = cor.test(data[,var1], data[,var2], method = "spearman", exact = FALSE)
  data.frame(var1, var2, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=FALSE)
}

#' Remove rows with outliers in given columns
#' 
#' Any row with at least 1 outlier will be removed
#' 
#' @param df A data.frame
#' @param cols Names of the columns of interest. Defaults to all columns.
#' 
#' 
remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    cat("Removing outliers in column: ", col, " \n")
    df <- df[!is_outlier(df[[col]]),]
  }
  df
}

# order a data frame's rows according to another vector
order_rows_same <- function(df_to_order, column_to_order = "IDfamily", df_template_vector) {
  df_to_order[match(df_template_vector, df_to_order[[column_to_order]]), ]
}

###############################################################################
## basic plotting

### ggplot points
basic_gg_point_plot <- function(data, x, y, title) {
  ggplot(data, aes_string(x, y)) +
    geom_point()+
    theme_bw() +
    theme(
      axis.line = element_line(colour = "grey"),
      panel.background = element_blank(),
      text = element_text(size = standard_textsize),
      axis.title = element_text(size = standard_textsize),
      axis.text = element_text(size = standard_textsize)
    ) +
    labs(x = x, y = y, title = title)
}

### basic scatterplot
plot_scatter <- function(data, x, y, xtitle, ytitle) {
  ggscatter(data, x = {{x}}, y = {{y}},
            add = "reg.line",
            xlab = xtitle, ylab = ytitle,
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE) + 
    stat_cor(method = "spearman", label.x = 3, label.y = 15000) +
    geom_hline(yintercept=6000, linetype="dashed", color = "grey") #with line at 6000 corresponds to 500mg * 12 weeks = 6000 mg consumption
}

### ggplot boxplot
plot_box <- function(data, category, outcome, xtitle, ytitle) {
  if(missing(category)){
    ggplot(data, aes(y = {{outcome}})) +
      geom_boxplot() +
      labs(x=xtitle, y = ytitle) +
      scale_x_discrete() +
      theme_minimal() +
      theme(
        #panel.background = element_blank(),
        text = element_text(size = standard_textsize),
        axis.title = element_text(size = standard_textsize),
        axis.text = element_text(size = standard_textsize)
      ) +
      geom_hline(yintercept=6000, linetype="dashed", color = "grey")
  } else{
  ggplot(data, aes(x = {{category}}, y = {{outcome}}, fill = {{category}})) +
    geom_boxplot() +
    labs(x=xtitle, y = ytitle) +
    theme_minimal()+
    theme(
      panel.background = element_blank(),
      legend.position="none",
      text = element_text(size = standard_textsize),
      axis.title = element_text(size = standard_textsize),
      axis.text = element_text(size = standard_textsize)) +
      scale_color_grey(start = 0.8, end = 0.8) +
      scale_fill_grey(start = 0.8, end = 0.8) +
    geom_hline(yintercept=6000, linetype="dashed", color = "grey")
  }
}


### ggplot boxplot grouped by pregnancy stage
plotbox_grouped_pregstage <- function(data, outcome, ytitle) {
  ggplot(data, aes(x = as.factor(pregstage), y = {{outcome}}, fill = as.factor(pregstage))) + 
  #  stat_boxplot(geom = "errorbar", width = 0.25) + 
    geom_boxplot() + 
    scale_color_grey(labels = c("I", "II", "III"), name = "gestational stage", start = 0.8, end = 0.8) +
    scale_fill_grey(labels = c("I", "II", "III"), name = "gestational stage", start = 0.8, end = 0.8) +
    xlab("pregnancy stage") + 
    ylab(ytitle) +
    theme_minimal() +
    theme(legend.position="none",
          panel.background = element_blank(),
          text = element_text(size = standard_textsize),
          axis.title = element_text(size = standard_textsize),
          axis.text = element_text(size = standard_textsize))+
    geom_hline(yintercept=6000, linetype="dashed", color = "grey")
}


### outlier identification for boxplots
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

### plot ggplot histogram optional grouped
plothist <- function(data, outcome, by, xtitle, title = "histogram") {
  if(missing(by)){
    ggplot(data, aes(x = {{outcome}})) +
      geom_histogram(bins = 200) + 
      labs(title = title, x = xtitle, y = "count (number of observations)") +
      theme_minimal()
  } else{
    ggplot(data, aes(x = {{outcome}}, fill = as.factor({{by}}), color = as.factor({{by}}))) +
      geom_histogram(alpha = 0.1, position = "identity", bins = 200) + 
      scale_color_discrete(labels = c("I", "II", "III"), name = "gestational stage") +
      scale_fill_discrete(labels = c("I", "II", "III"), name = "gestational stage") +
      labs(title = title, x = xtitle, y = "count (number of observations)") +
      theme_minimal()
  }
}


###############################################################################
## descriptive tables
### plot ggplot histogram optional grouped
describe_continuous_by_group_table <- function(data, groupby, variable, variablelabel, headerlabel) {
  data %>%
  tbl_summary(by = {{groupby}},
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})","{median}", "{p25}, {p75}","{min}, {max}")),
              label = {{variable}} ~ variablelabel,
              digits = list(all_continuous() ~ 0, all_categorical() ~ 0)) %>%
    modify_header(
      update = list(all_stat_cols(FALSE) ~ "**{level}**<br>N = {n}"))  %>%
    modify_header(label ~ headerlabel)
}


##############################################################################
## gene expression - consumption plots; using ggpmisc package
#' plot gene expression for genes of interest with winsorized value
plot_genes_winsz <- function(data, predictor = glycyrrhizin_mg_winsz, expression = HSD11B2_expression, xtitle = "glycyrrhizin consumption (mg), winsorized", ytitle = "expression, (transformed) counts", main = "HSD11B2 expression") {
  ggplot(data, aes(x = {{predictor}}, y = {{expression}})) +
    geom_point() +
    stat_poly_line() +
   # stat_poly_eq(aes(label = after_stat(eq.label))) +
    stat_poly_eq(label.x = 0.05, label.y = max(data[deparse(substitute(expression))], na.rm=TRUE)) +
    labs(x = xtitle, y = ytitle, title = main) +
    ylim(min(data[deparse(substitute(expression))], na.rm=TRUE),max(data[deparse(substitute(expression))], na.rm=TRUE)+1) +
    theme(panel.background = element_blank(),
          text = element_text(size = standard_textsize),
          axis.title = element_text(size = standard_textsize),
          axis.text = element_text(size = standard_textsize),
          plot.title = element_text(size = standard_textsize))+
    theme_minimal()
}

plot_gene_scatter <- function(data, predictor = "glycyrrhizin_mg_winsz", expression = "HSD11B2_expression", xtitle = "glycyrrhizin consumption (mg), winsorized", ytitle = "expression, (transformed) counts", maintitle = "HSD11B2 expression") {
  ggscatter(data, x = predictor, y = expression,
    add = "reg.line", conf.int = T, cor.coef = F, #cor.method = "spearman"
   # cor.coeff.args = list(label.y = 11.5),
    #cor.coef.size = 3,
    xlab = xtitle, ylab = ytitle , main = maintitle,
    ) +
    font("title", size = standard_textsize)+
    font("subtitle", size = standard_textsize)+
    font("caption", size = standard_textsize)+
    font("xlab", size = standard_textsize)+
    font("ylab", size = standard_textsize)+
    font("xy.text", standard_textsize) +
    stat_cor(label.y = 11.5, method = "spearman", size = 3, p.accuracy = 0.01, r.accuracy = 0.01)
}
    

plot_gene_pheno_ggside <- function(data, x, y, group) {
  ggplot(data, aes({{x}}, {{y}}, fill = {{group}})) +
    geom_point(aes(color = {{group}})) +
    geom_xsidedensity(alpha = .3, position = "stack") +
    geom_ysideboxplot(aes(x = {{group}}), orientation = "x") +
    scale_ysidex_discrete(guide = guide_axis(angle = 45)) +
    theme_classic() +
    theme(ggside.panel.scale = .3,
          ggside.panel.border = element_rect(NA, "red", size = 2),
          ggside.panel.grid = element_line("black", size = .1, linetype = "dotted"),
          ggside.panel.background = element_blank()) +
    guides(color = "none", fill = "none")
}
