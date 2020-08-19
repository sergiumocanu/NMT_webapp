###### general info --------
## name: univariant.R
## purpose: unsupervised learning and Univariate analysis
## version: 0.2.0

## test from Rscript
args <- commandArgs()
# print(args)

### load libraries --------
require(foreach)
require(limma)
require(edgeR)
require(splines)
require(RBioArray)
require(RBioFS)

###### sys variables --------
# ------ file name variables ------
DAT_FILE <- args[6]  # 2D file
MAT_FILE_NO_EXT <- args[7]  # from the raw mat file, for naming export data

# ------ directory variables ------
RES_OUT_DIR <- args[8]

# ------ processing varaibles ------
# NOTE: convert string to expression using eval(parse(text = "string"))
# -- (from config file) --
LOG2_TRANS <- eval(parse(text = args[9]))
HTMAP_TEXTSIZE_COL <- as.numeric(args[10])
HTMAP_TEXTANGLE_COL <- as.numeric(args[11])
HTMAP_LAB_ROW <-eval(parse(text = args[12]))
HTMAP_TEXTSIZE_ROW <- as.numeric(args[13])
HTMAP_KEYSIZE <- as.numeric(args[14])
HTMAP_KEY_XLAB <- args[15]
HTMAP_KEY_YLAB <- args[16]
HTMAP_MARGIN <- eval(parse(text = args[17]))
HTMAP_WIDTH <- as.numeric(args[18])
HTMAP_HEIGHT <- as.numeric(args[19])

UNI_FDR <- eval(parse(text = args[20]))
UNI_ALPHA <- as.numeric(args[21])
UNI_FOLD_CHANGE <- as.numeric(args[22])

SIG_HTMAP_TEXTSIZE_COL <- as.numeric(args[23])
SIG_HTMAP_TEXTANGLE_COL <- as.numeric(args[24])
SIG_HTMAP_TEXTSIZE_ROW <- as.numeric(args[25])
SIG_HTMAP_KEYSIZE <- as.numeric(args[26])
SIG_HTMAP_KEY_XLAB <- args[27]
SIG_HTMAP_KEY_YLAB <- args[28]
SIG_HTMAP_MARGIN <- eval(parse(text = args[29]))
SIG_HTMAP_WIDTH <- as.numeric(args[30])
SIG_HTMAP_HEIGHT <- as.numeric(args[31])

# ------ warning flags ------
if (UNI_FDR) FDR_FAIL_WARNING <- FALSE
NO_SIG_WARNING <- FALSE

###### R script --------
# ------ set the output directory as the working directory ------
setwd(RES_OUT_DIR)  # the folder that all the results will be exports to

# ------ load and processed (from raw mat file) data files ------
raw_sample_dfm <- read.csv(file = DAT_FILE, stringsAsFactors = FALSE, check.names = FALSE)


# ------ Data processing for univariate analysis ------
## data formating
x <- raw_sample_dfm[, -c(1:2)]
y <- raw_sample_dfm$y

# if to log2 transform the data
if (LOG2_TRANS) {
  E <- apply(t(x), c(1, 2), FUN = function(x)log2(x + 2))
} else {
  E <- t(x)
}

colnames(E) <- raw_sample_dfm$sampleid
pair <- data.frame(ProbeName = seq(ncol(raw_sample_dfm) - 2), pair = colnames(raw_sample_dfm)[-c(1:2)])
sample <- paste0(raw_sample_dfm$sampleid, "_", raw_sample_dfm$y)
idx <- data.frame(raw_sample_dfm[, c(1:2)], sample = sample)
rawlist <- list(E = E, genes = pair, targets = idx)

## Normalization
normdata <- rbioarray_PreProc(rawlist = rawlist, offset = 2, normMethod = "quantile", bgMethod = "none")

# ------ all connections clustering analysis -------
# -- hclust --
if (HTMAP_LAB_ROW) {
  rbioarray_hcluster(plotName = paste0(MAT_FILE_NO_EXT, "_hclust_all"),
                     fltlist = normdata, n = "all", fct = factor(y, levels = unique(y)),
                     ColSideCol = FALSE,
                     sampleName = idx$sample,
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
                     fltlist = normdata, n = "all", fct = factor(y, levels = unique(y)),
                     ColSideCol = FALSE,
                     sampleName = idx$sample,
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

# ------ univariate analysis ------
# -- set up contrast --
nsy <- splines::ns(y)
design <- model.matrix(~ nsy)

# -- Stats --
if (UNI_FDR){
  sig.method <- "fdr"
} else {
  sig.method <- "none"
}

rbioarray_DE(objTitle = MAT_FILE_NO_EXT,
             input.outcome.mode = "continuous",
             output.mode = "probe.all",
             fltlist = normdata, annot = normdata$genes,
             design = design, contra = NULL,
             weights = normdata$ArrayWeight,
             geneName = TRUE, genesymbolVar = "pair",
             topgeneLabel = TRUE, nGeneSymbol = VOLCANO_N_TOP_CONNECTION,
             padding = 0.5, FC = UNI_FOLD_CHANGE, ctrlProbe = FALSE,
             ctrlTypeVar = "ControlType", sig.method = sig.method, sig.p = UNI_ALPHA,
             plot = FALSE,
             parallelComputing = FALSE, verbose = FALSE)

fit_dfm <- get(paste0(MAT_FILE_NO_EXT, "_fit"))[, 1:8]
names(fit_dfm)[2] <- "pair"
if (sig.method == "fdr" && length(which(fit_dfm$adj.P.Val < UNI_ALPHA)) == 0 && length(which(fit_dfm$P.Value < UNI_ALPHA)) == 0) {
  NO_SIG_WARNING <- TRUE
} else if (sig.method == "fdr" && length(which(fit_dfm$adj.P.Val < UNI_ALPHA)) == 0){
  FDR_FAIL_WARNING <- TRUE
  sig.method <- "none"
} else if (sig.method == "none" && length(which(fit_dfm$P.Value < UNI_ALPHA)) == 0){
  NO_SIG_WARNING <- TRUE
}
if (UNI_FDR){
  if (FDR_FAIL_WARNING){
    pcutoff <- UNI_ALPHA
  } else {
    pcutoff <- max(fit_dfm$P.Value[which(fit_dfm$adj.P.Val < UNI_ALPHA)])
  }
} else {
  pcutoff <- UNI_ALPHA
}
sig_pairs_fit <- as.character(fit_dfm[fit_dfm$P.Value < pcutoff, "pair"])

# -- sig clustering --
# hcluster
if (!NO_SIG_WARNING){
  rbioarray_hcluster_super(plotName = paste0(MAT_FILE_NO_EXT, "_fit"),
                           fltDOI = normdata, dfmDE = fit_dfm,
                           DE.sig.method = sig.method, FC = UNI_FOLD_CHANGE, DE.sig.p = UNI_ALPHA,
                           clust = "complete",
                           ctrlProbe = FALSE,
                           fct = factor(y, levels = unique(y)), dataProbeVar = "pair",
                           rowLabel = TRUE,
                           annot = normdata$genes, annotProbeVar = "pair", genesymbolVar = "pair",
                           sampleName = idx$sample,
                           ColSideCol = FALSE,
                           trace = "none", offsetCol = 0.2, adjCol = c(1, 0),
                           key.title = "", keysize = SIG_HTMAP_KEYSIZE, scale = c("row"),
                           cexCol = SIG_HTMAP_TEXTSIZE_COL, cexRow = SIG_HTMAP_TEXTSIZE_ROW,
                           srtCol = SIG_HTMAP_TEXTANGLE_COL,
                           key.xlab = SIG_HTMAP_KEY_XLAB, key.ylab = SIG_HTMAP_KEY_YLAB,
                           margin = SIG_HTMAP_MARGIN,
                           plotWidth = SIG_HTMAP_WIDTH, plotHeight = SIG_HTMAP_HEIGHT,
                           verbose = FALSE)
}


####### clean up the mess and export --------
## clean up the mess from Pathview
suppressWarnings(rm(cpd.simtypes, gene.idtype.bods, gene.idtype.list, korg, i))

## export to results files if needed
x_ml <- t(normdata$E)[, sig_pairs_fit]
ml_dfm <- data.frame(sampleid=raw_sample_dfm$sampleid, y, x_ml, check.names = FALSE, stringsAsFactors = FALSE)
write.csv(file = paste0(RES_OUT_DIR, "/", MAT_FILE_NO_EXT, "_ml.csv"), ml_dfm, row.names = FALSE)
# save(list = c("normdata"), file = paste0(RES_OUT_DIR, "/normdata.Rdata"))

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
cat("Clustering analysis: all features\n")
cat("-------------------------------------\n")
cat("Hierarchical clustering heatmap saved to: ", paste0(MAT_FILE_NO_EXT, "_hclust_all.pdf\n"))
# cat("-------------------------------------\n")
cat("\n\n")
cat("Univariate analysis\n")
cat("-------------------------------------\n")
if (UNI_FDR && FDR_FAIL_WARNING) cat("No significant results found with FDR, using raw p value instead.\n\n")
cat("p-value correction: ", sig.method, "\n")
cat("alpha: ", UNI_ALPHA, "\n\n")
cat(paste0("Number of significant features: ", length(sig_pairs_fit), "\n"))
cat(paste0("Significant features: \n"))
sig_pairs_fit
cat("\n")
cat("Univariate analysis results saved to files: \n")
cat("\t", paste0(MAT_FILE_NO_EXT, "_DE_Fstats.csv\n"))
cat("\n\n")
cat("Clustering analysis: significant connections\n")
cat("-------------------------------------\n")
cat("Hierarchical clustering heatmap: \n")
if (NO_SIG_WARNING) {
  cat("No significant results found. \n")
  cat("Check output folder for the results. \n")
} else {
  cat(paste0("\t", MAT_FILE_NO_EXT, "_fit_hclust_sig.pdf\n"))
}
