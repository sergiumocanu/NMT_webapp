###### general info --------
## name: pred_dat_process.R
## purpose: load and process mat file for prediction
## version: 0.3.2
## Make sure to have 3 dimensions for the mat data, even when there is only one matrix, e.g. 90x90x1

## flags from Rscript
# args <- commandArgs(trailingOnly = TRUE)
# print(args)


pred_dat_process <- function(args){

###### load libraries --------
require(foreach)
require(R.matlab) # to read .mat files

###### sys variables --------
# --- file name variables ---
MAT_FILE <- args[1]
MAT_FILE_NO_EXT <- args[2]

# --- mata data input variables ---
ANNOT_FILE <- args[3]
SAMPLEID_VAR <- args[4]

# --- directory variables ---
# FIG_OUT_DIR
RES_OUT_DIR <- args[5]

###### R script --------
# ------ set the output directory as the working directory ------
# setwd(RES_OUT_DIR)  # the folder that all the results will be exports to

# ------ load mat file ------
# setwd("/Users/jingzhang/Documents/git_repo/git_meg_ml_app/data/")
# MAT_FILE <- "/Users/jingzhang/Documents/git_repo/git_meg_ml_app/data/freq_3_alpha.ptsd_mtbi_aec_v2.mat"
raw <- readMat(MAT_FILE)
raw <- raw[[1]]
raw_dim <- dim(raw)

# ------ load annotation file (meta data) ------
annot <- read.csv(file = ANNOT_FILE, stringsAsFactors = FALSE, check.names = FALSE)
if (!all(SAMPLEID_VAR %in% names(annot))) {
  cat("none_existent")
  quit()
}
if (nrow(annot) != raw_dim[3]) {
  cat("unequal_length")
  quit()
}
sampleid <- annot[, SAMPLEID_VAR]

# ------ process the mat file ------
raw_sample <- foreach(i = 1:raw_dim[3], .combine = "rbind") %do% {
  tmp <- raw[, , i]
  colnames(tmp) <- as.character(seq(ncol(tmp)))
  rownames(tmp) <- as.character(seq(ncol(tmp)))
  pair <- paste(rownames(tmp)[row(tmp)[upper.tri(tmp)]], colnames(tmp)[col(tmp)[upper.tri(tmp)]], sep = "_")
  sync.value <- tmp[upper.tri(tmp)]
  names(sync.value) <- pair
  sync.value
}
if (is.null(nrow(raw_sample))){  # one entry only
  raw_sample_dfm <- data.frame(as.list(raw_sample))
  names(raw_sample_dfm) <- names(raw_sample)  
} else {
  raw_sample_dfm <- raw_sample
}
raw_sample_dfm <- data.frame(sampleid = sampleid, raw_sample_dfm, row.names = NULL, check.names = FALSE)

# ------ export and clean up the mess --------
## export to results files if needed
write.csv(file = paste0(RES_OUT_DIR, "/", MAT_FILE_NO_EXT, "_2D.csv"), raw_sample_dfm, row.names = FALSE)

# ------ display messages so far ------
# cat the variables to export to shell script
cat("\tMat file dimensions: ", raw_dim, "\n") # line 2: input mat file dimension
cat("\tSamples to predict: ", nrow(raw_sample_dfm), "\n")
}