###### general info --------
## name: univariant.R
## purpose: unsupervised learning and Univariate analysis
## version: 0.2.0

## test from Rscript
# args <- commandArgs()
# print(args)

### load libraries --------
require(foreach)
require(limma)
require(edgeR)
require(RBioArray)
require(RBioFS)

univariate_2D <- function(args) {

###### sys variables --------
# ------ file name variables ------
DAT_FILE <- args[6]  # 2D file
MAT_FILE_NO_EXT <- args[7]  # from the raw mat file, for naming export data


# ------ directory variables ------
RES_OUT_DIR <- args[9]

# ------ processing varaibles ------
# NOTE: convert string to expression using eval(parse(text = "string"))
# -- (from config file) --
LOG2_TRANS <- eval(parse(text = args[10]))
HTMAP_TEXTSIZE_COL <- as.numeric(args[11])
HTMAP_TEXTANGLE_COL <- as.numeric(args[12])
HTMAP_LAB_ROW <-eval(parse(text = args[13]))
HTMAP_TEXTSIZE_ROW <- as.numeric(args[14])
HTMAP_KEYSIZE <- as.numeric(args[15])
HTMAP_KEY_XLAB <- args[16]
HTMAP_KEY_YLAB <- args[17]
HTMAP_MARGIN <- eval(parse(text = args[18]))
HTMAP_WIDTH <- as.numeric(args[19])
HTMAP_HEIGHT <- as.numeric(args[20])

PCA_SCALE_DATA <- eval(parse(text = args[21]))
PCA_CENTRE_DATA <- eval(parse(text = args[22]))
PCA_PC <- eval(parse(text = args[23]))
PCA_BIPLOT_SAMPLELABEL_TYPE <- args[24]
PCA_BIPLOT_SAMPLELABEL_SIZE <- as.numeric(args[25])
PCA_BIPLOT_SYMBOL_SIZE <- as.numeric(args[26])
PCA_BIPLOT_ELLIPSE <- eval(parse(text = args[27]))
PCA_BIPLOT_ELLIPSE_CONF <- as.numeric(args[28])
PCA_BIPLOT_LOADING <- eval(parse(text = args[29]))
PCA_BIPLOT_LOADING_TEXTSIZE <- as.numeric(args[30])
PCA_BIPLOT_MULTI_DESITY <- eval(parse(text = args[31]))
PCA_BIPLOT_MULTI_STRIPLABEL_SIZE <- as.numeric(args[32])
PCA_RIGHTSIDE_Y <- eval(parse(text = args[33]))
PCA_X_TICK_LABEL_SIZE <- as.numeric(args[34])
PCA_Y_TICK_LABEL_SIZE <- as.numeric(args[35])
PCA_WIDTH <- as.numeric(args[36])
PCA_HEIGHT <- as.numeric(args[37])

UNI_FDR <- eval(parse(text = args[39]))
UNI_ALPHA <- as.numeric(args[40])
UNI_FOLD_CHANGE <- as.numeric(args[41])
VOLCANO_N_TOP_CONNECTION <- as.numeric(args[42])
VOLCANO_SYMBOL_SIZE <- as.numeric(args[43])
VOLCANO_SIG_COLOUR <- args[44]
VOLCANO_NONSIG_COLOUR <- args[45]
VOLCANO_X_TEXT_SIZE <- as.numeric(args[46])
VOLCANO_Y_TEXT_SIZE <- as.numeric(args[47])
VOLCANO_WIDTH <- as.numeric(args[48])
VOLCANO_HEIGHT <- as.numeric(args[49])

SIG_HTMAP_TEXTSIZE_COL <- as.numeric(args[50])
SIG_HTMAP_TEXTANGLE_COL <- as.numeric(args[51])
SIG_HTMAP_TEXTSIZE_ROW <- as.numeric(args[52])
SIG_HTMAP_KEYSIZE <- as.numeric(args[53])
SIG_HTMAP_KEY_XLAB <- args[54]
SIG_HTMAP_KEY_YLAB <- args[55]
SIG_HTMAP_MARGIN <- eval(parse(text = args[56]))
SIG_HTMAP_WIDTH <- as.numeric(args[57])
SIG_HTMAP_HEIGHT <- as.numeric(args[58])
SIG_PCA_PC <- eval(parse(text = args[59]))
SIG_PCA_BIPLOT_ELLIPSE_CONF <- as.numeric(args[60])

# -- from flags --
CONTRAST <- args[38]

# ------ warning flags ------
if (UNI_FDR) FDR_FAIL_WARNING <- FALSE
NO_SIG_WARNING <- FALSE
ONE_SIG_WARNING <- FALSE
NO_SIG_WARNING_FIT <- FALSE

###### R script --------
# ------ set the output directory as the working directory ------
setwd(RES_OUT_DIR)  # the folder that all the results will be exports to

# ------ load and processed (from raw mat file) data files ------
raw_sample_dfm <- read.csv(file = DAT_FILE, stringsAsFactors = FALSE, check.names = FALSE)


# ------ Data processing for univariate analysis ------
## data formating
x <- raw_sample_dfm[, -c(1:2)]
y <- factor(raw_sample_dfm$group, levels = unique(raw_sample_dfm$group))

# if to log2 transform the data
if (LOG2_TRANS) {
  E <- apply(t(x), c(1, 2), FUN = function(x)log2(x + 2))
} else {
  E <- t(x)
}

colnames(E) <- raw_sample_dfm$sampleid
pair <- data.frame(ProbeName = seq(ncol(raw_sample_dfm) - 2), pair = colnames(raw_sample_dfm)[-c(1:2)])
sample <- paste0(raw_sample_dfm$sampleid, "_", raw_sample_dfm$group)
idx <- data.frame(raw_sample_dfm[, c(1:2)], sample = sample)
rawlist <- list(E = E, genes = pair, targets = idx)

## Normalization
normdata <- rbioarray_PreProc(rawlist = rawlist, offset = 2, normMethod = "quantile", bgMethod = "none")

# ------ all connections clustering analysis -------
# -- hclust --
if (HTMAP_LAB_ROW) {
  rbioarray_hcluster(plotName = paste0(MAT_FILE_NO_EXT, "_hclust_all"),
                     fltlist = normdata, n = "all", fct = y, sampleName = idx$sample,
                     genesymbolOnly = FALSE,
                     trace = "none", ctrlProbe = FALSE, rmControl = FALSE,
                     srtCol = HTMAP_TEXTANGLE_COL, offsetCol = 0,
                     key.title = "", dataProbeVar = "pair",
                     cexCol = HTMAP_TEXTSIZE_COL, cexRow = HTMAP_TEXTSIZE_ROW,
                     keysize = HTMAP_KEYSIZE,
                     key.xlab = HTMAP_KEY_XLAB,
                     key.ylab = HTMAP_KEY_YLAB,
                     plotWidth = HTMAP_WIDTH, plotHeight = HTMAP_HEIGHT,
                     margin = HTMAP_MARGIN)
} else {
  rbioarray_hcluster(plotName = paste0(MAT_FILE_NO_EXT, "_hclust_all"),
                     fltlist = normdata, n = "all", fct = y, sampleName = idx$sample,
                     genesymbolOnly = FALSE,
                     trace = "none", ctrlProbe = FALSE, rmControl = FALSE,
                     srtCol = HTMAP_TEXTANGLE_COL, offsetCol = 0,
                     key.title = "", dataProbeVar = "pair", labRow = FALSE,
                     cexCol = HTMAP_TEXTSIZE_COL, cexRow= HTMAP_TEXTSIZE_ROW,
                     keysize = HTMAP_KEYSIZE,
                     key.xlab = HTMAP_KEY_XLAB,
                     key.ylab = HTMAP_KEY_YLAB,
                     plotWidth = HTMAP_WIDTH, plotHeight = HTMAP_HEIGHT,
                     margin = HTMAP_MARGIN)
}

# -- PCA --
pca_all <- data.frame(normdata$targets[, c("group", "sample")], t(normdata$E), stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
rbioFS_PCA(input = pca_all,
           sampleIDVar = "sample", groupIDVar = "group",
           scaleData = PCA_SCALE_DATA, centerData = PCA_CENTRE_DATA, boxplot = TRUE,
           boxplot.Title = NULL, boxplot.Width = PCA_WIDTH, boxplot.Height = PCA_HEIGHT,
           biplot = TRUE, biplot.comps = PCA_PC, biplot.Title = NULL,
           biplot.sampleLabel.type = PCA_BIPLOT_SAMPLELABEL_TYPE,
           biplot.sampleLabelSize = PCA_BIPLOT_SAMPLELABEL_SIZE,
           biplot.sampleLabel.padding = 0.5, biplot.SymbolSize = PCA_BIPLOT_SYMBOL_SIZE,
           biplot.ellipse = PCA_BIPLOT_ELLIPSE, biplot.ellipse_conf = PCA_BIPLOT_ELLIPSE_CONF,
           biplot.xAngle = 0, biplot.xhAlign = 0.5, biplot.xvAlign = 0.5,
           biplot.loadingplot = FALSE, biplot.loadingplot.textsize = PCA_BIPLOT_LOADING_TEXTSIZE,
           biplot.mtx.densityplot = PCA_BIPLOT_MULTI_DESITY,
           biplot.mtx.stripLblSize = PCA_BIPLOT_MULTI_STRIPLABEL_SIZE,
           biplot.Width = PCA_WIDTH, biplot.Height = PCA_HEIGHT, rightsideY = PCA_RIGHTSIDE_Y,
           fontType = "sans", xTickLblSize = PCA_X_TICK_LABEL_SIZE, yTickLblSize = PCA_Y_TICK_LABEL_SIZE,
           verbose = FALSE)


# ------ univariate analysis ------
# -- set up contrast --
design <- model.matrix(~ 0 + y)
colnames(design) <- levels(y)
contra_string <- unlist(strsplit(CONTRAST, split = ","))
contra_string <- gsub(" ", "", contra_string, fixed = TRUE)  # remove all the white space
# NOTE: below: use do.call to unpack arguments for makeContrasts()
contra <- do.call(makeContrasts, c(as.list(contra_string), levels = as.list(parse(text = "design"))))

# -- Stats --
if (UNI_FDR){
  sig.method <- "fdr"
} else {
  sig.method <- "none"
}

tryCatch(rbioarray_DE(objTitle = MAT_FILE_NO_EXT, output.mode = "probe.all",
                      fltlist = normdata, annot = normdata$genes, design = design, contra = contra,
                      weights = normdata$ArrayWeight,
                      plot = TRUE, geneName = TRUE, genesymbolVar = "pair",
                      topgeneLabel = TRUE, nGeneSymbol = VOLCANO_N_TOP_CONNECTION,
                      padding = 0.5, FC = UNI_FOLD_CHANGE, ctrlProbe = FALSE,
                      ctrlTypeVar = "ControlType", sig.method = sig.method, sig.p = UNI_ALPHA,
                      plotTitle = NULL, xLabel = "log2(fold change)",
                      yLabel = "-log10(p value)", symbolSize = VOLCANO_SYMBOL_SIZE,
                      sigColour = VOLCANO_SIG_COLOUR, nonsigColour = VOLCANO_NONSIG_COLOUR,
                      xTxtSize = VOLCANO_X_TEXT_SIZE, yTxtSize = VOLCANO_Y_TEXT_SIZE,
                      plotWidth = VOLCANO_WIDTH, plotHeight = VOLCANO_HEIGHT,
                      parallelComputing = FALSE, clusterType = "PSOCK", verbose = FALSE),
         warning = function(w) {
           if (length(contra_string) == 1) assign("FDR_FAIL_WARNING", TRUE, envir = .GlobalEnv)
           rbioarray_DE(objTitle = MAT_FILE_NO_EXT, output.mode = "probe.all",
                        fltlist = normdata, annot = normdata$genes, design = design, contra = contra,
                        weights = normdata$ArrayWeight,
                        plot = TRUE, geneName = TRUE, genesymbolVar = "pair",
                        topgeneLabel = TRUE, nGeneSymbol = VOLCANO_N_TOP_CONNECTION,
                        padding = 0.5, FC = UNI_FOLD_CHANGE, ctrlProbe = FALSE,
                        ctrlTypeVar = "ControlType", sig.method = sig.method, sig.p = UNI_ALPHA,
                        plotTitle = NULL, xLabel = "log2(fold change)",
                        yLabel = "-log10(p value)", symbolSize = VOLCANO_SYMBOL_SIZE,
                        sigColour = VOLCANO_SIG_COLOUR, nonsigColour = VOLCANO_NONSIG_COLOUR,
                        xTxtSize = VOLCANO_X_TEXT_SIZE, yTxtSize = VOLCANO_Y_TEXT_SIZE,
                        plotWidth = VOLCANO_WIDTH, plotHeight = VOLCANO_HEIGHT,
                        parallelComputing = FALSE, clusterType = "PSOCK", verbose = FALSE)
         })


# -- Univariate analysis results export --
DE_summary  <- as.data.frame(get(paste0(MAT_FILE_NO_EXT, "_DE_summary")))
names(DE_summary) <- c("comparison", "p-value.threshold", "fold.change.threshold", "sig", "non-sig")

# -- sig clustering --
# hcluster
de_names <- names(get(paste0(MAT_FILE_NO_EXT, "_DE")))
for (i in 1:length(get(paste0(MAT_FILE_NO_EXT, "_DE")))) {
  if (DE_summary[i, "sig"] == 0) {
    NO_SIG_WARNING <- TRUE
  } else if (DE_summary[i, "sig"] == 1) {
    ONE_SIG_WARNING <- TRUE
  } else {
    normdata$E
    normdata$targets
    de_groups <- unlist(strsplit(de_names[i], "-"))
    de_sample_idx <- which(normdata$targets$group %in% de_groups)
    super_cluster_data <- list(E = normdata$E[, de_sample_idx], genes = normdata$genes,
                               targets = normdata$targets[de_sample_idx, ])
    rbioarray_hcluster_super(plotName = paste0(MAT_FILE_NO_EXT, "_DE_",de_names[i]),
                             fltDOI = super_cluster_data, dfmDE = get(paste0(MAT_FILE_NO_EXT, "_DE"))[[i]],
                             DE.sig.method = sig.method, FC = UNI_FOLD_CHANGE, DE.sig.p = UNI_ALPHA,
                             clust = "complete",
                             ctrlProbe = FALSE,
                             fct = y, dataProbeVar = "pair",
                             rowLabel = TRUE,
                             annot = super_cluster_data$genes, annotProbeVar = "pair", genesymbolVar = "pair",
                             sampleName = super_cluster_data$targets$sample,
                             trace = "none", offsetCol = 0.2, adjCol = c(1, 0),
                             key.title = "", keysize = SIG_HTMAP_KEYSIZE, scale = c("row"),
                             cexCol = SIG_HTMAP_TEXTSIZE_COL, cexRow = SIG_HTMAP_TEXTSIZE_ROW,
                             srtCol = SIG_HTMAP_TEXTANGLE_COL,
                             key.xlab = SIG_HTMAP_KEY_XLAB, key.ylab = SIG_HTMAP_KEY_YLAB,
                             margin = SIG_HTMAP_MARGIN,
                             plotWidth = SIG_HTMAP_WIDTH, plotHeight = SIG_HTMAP_HEIGHT,
                             verbose = FALSE)
  }
}

# PCA
fit_dfm <- get(paste0(MAT_FILE_NO_EXT, "_fit"))
names(fit_dfm)[2] <- "pair"
if (length(contra_string) == 1) {
  if (UNI_FDR){
    if (FDR_FAIL_WARNING){
      pcutoff <- UNI_ALPHA
    } else {
      pcutoff <- max(fit_dfm$P.Value[which(fit_dfm$adj.P.Val <= UNI_ALPHA)])
    }
  } else {
    pcutoff <- UNI_ALPHA
  }
  sig_pairs_fit <- as.character(fit_dfm[fit_dfm$P.Value < pcutoff, "pair"])
} else {
  if (UNI_FDR){
    pcutoff <- tryCatch(max(fit_dfm$P.Value[which(fit_dfm$adj.P.Val < UNI_ALPHA)]),
                        warning = function(w){
                          NULL
                        })
    if (is.null(pcutoff)) {
      sig_pairs_fit <- character(0)
    } else {
      sig_pairs_fit <- as.character(fit_dfm[fit_dfm$P.Value <= pcutoff, "pair"])
    }

  } else {
    pcutoff <- UNI_ALPHA
    sig_pairs_fit <- as.character(fit_dfm[fit_dfm$P.Value < pcutoff, "pair"])
  }
}

if (length(sig_pairs_fit) <= 1) {
  NO_SIG_WARNING_FIT <- TRUE
} else {
  pca_sig <- pca_all[, names(pca_all) %in% c("group", "sample", sig_pairs_fit)]
  rbioFS_PCA(input = pca_sig,
             sampleIDVar = "sample", groupIDVar = "group",
             scaleData = PCA_SCALE_DATA, centerData = PCA_CENTRE_DATA, boxplot = TRUE,
             boxplot.Title = NULL, boxplot.Width = PCA_WIDTH, boxplot.Height = PCA_HEIGHT,
             biplot = TRUE, biplot.comps = SIG_PCA_PC, biplot.Title = NULL,
             biplot.sampleLabel.type = PCA_BIPLOT_SAMPLELABEL_TYPE,
             biplot.sampleLabelSize = PCA_BIPLOT_SAMPLELABEL_SIZE,
             biplot.sampleLabel.padding = 0.5, biplot.SymbolSize = PCA_BIPLOT_SYMBOL_SIZE,
             biplot.ellipse = PCA_BIPLOT_ELLIPSE, biplot.ellipse_conf = SIG_PCA_BIPLOT_ELLIPSE_CONF,
             biplot.xAngle = 0, biplot.xhAlign = 0.5, biplot.xvAlign = 0.5,
             biplot.loadingplot = PCA_BIPLOT_LOADING, biplot.loadingplot.textsize = PCA_BIPLOT_LOADING_TEXTSIZE,
             biplot.mtx.densityplot = PCA_BIPLOT_MULTI_DESITY,
             biplot.mtx.stripLblSize = PCA_BIPLOT_MULTI_STRIPLABEL_SIZE,
             biplot.Width = PCA_WIDTH, biplot.Height = PCA_HEIGHT, rightsideY = PCA_RIGHTSIDE_Y,
             fontType = "sans", xTickLblSize = PCA_X_TICK_LABEL_SIZE, yTickLblSize = PCA_Y_TICK_LABEL_SIZE,
             verbose = FALSE)
}


####### clean up the mess and export --------
## clean up the mess from Pathview
suppressWarnings(rm(cpd.simtypes, gene.idtype.bods, gene.idtype.list, korg, i))

## export to results files if needed
x_ml <- t(normdata$E)[, sig_pairs_fit, drop = FALSE]
ml_dfm <- data.frame(y, x_ml, check.names = FALSE, stringsAsFactors = FALSE)
write.csv(file = paste0(RES_OUT_DIR, "/", MAT_FILE_NO_EXT, "_ml.csv"), ml_dfm, row.names = FALSE)

## cat the vairables to export to shell scipt
# cat("\t", dim(raw_sample_dfm), "\n") # line 1: file dimension
# cat("First five variable names: ", names(raw_sample_dfm)[1:5])
cat("2D data file summary\n")
cat("-------------------------------------\n")
cat("2D file dimensions: ", dim(raw_sample_dfm), "\n\n")
cat("Data sample:\n")
print(raw_sample_dfm[1:5, 1:5])
# cat("-------------------------------------\n")
cat("\n\n")
cat("Clustering analysis: all connections\n")
cat("-------------------------------------\n")
cat("Hierarchical clustering heatmap saved to: ", paste0(MAT_FILE_NO_EXT, "_hclust_all.pdf\n"))
cat("\n")
cat("PCA results saved to: \n")
cat("\tbiplot: pca_all.pca.biplot.pdf\n")
cat("\tboxplot: pca_all.pca.boxplot.pdf\n")
# cat("-------------------------------------\n")
cat("\n\n")
cat("Univariate analysis\n")
cat("-------------------------------------\n")
cat("Comparison(s): ", contra_string, "\n")
cat("\n")
cat("Univariate analysis results summary\n")
if (UNI_FDR && FDR_FAIL_WARNING) cat("No significant results found with FDR, using raw p value instead.\n")
print(DE_summary)
cat("\n")
cat("Univariate analysis results saved to files: \n")
cat("\t", paste0(MAT_FILE_NO_EXT, "_DE_Fstats.csv\n"))
cat("\t", paste0(MAT_FILE_NO_EXT, "_threshold_summary.csv\n"))
cat("\t", paste0(MAT_FILE_NO_EXT, "_", contra_string, "_DE.csv"), "\n")
cat("\n")
cat("Volcano plot saved to file(s): \n", paste0("\t", MAT_FILE_NO_EXT, "_", contra_string, ".volcano.pdf\n"))
cat("\n\n")
cat("Clustering analysis: significant connections\n")
cat("-------------------------------------\n")
cat("Hierarchical clustering heatmap: \n")
if (NO_SIG_WARNING) {
  cat("One or more comparisons failed to identify significant results. \n")
  cat("Check output folder for the results. \n")
} else if (ONE_SIG_WARNING) {
  cat("Only one significant result found, no need for supervised culstering. \n")
} else {
  cat(paste0("\t", MAT_FILE_NO_EXT, "_", de_names, "_hclust_sig.pdf\n"))
}
cat("\n")
if (length(contra_string) == 1){
  if (NO_SIG_WARNING_FIT) {
    cat("NO significant reuslts found in F-stats results, no PCA needed. \n")
  } else {
    cat("PCA results saved to: \n")
    cat("\tbiplot: pca_sig.pca.biplot.pdf\n")
    cat("\tboxplot: pca_sig.pca.boxplot.pdf\n")
  }
} else {
  if (NO_SIG_WARNING_FIT) {
    cat("NO significant reuslts found in F-stats results, no PCA needed. Program terminated. \n")
  }
}

}