###### general info --------
## name: pred_classif.R
## purpose: load and process mat file for prediction
## version: 0.3.2
## Make sure to have 3 dimensions for the mat data, even when there is only one matrix, e.g. 90x90x1

## flags from Rscript
# args <- commandArgs(trailingOnly = TRUE)
# print(args)

pred_classif <- function(args){

###### load libraries --------
library(foreach)
library(RBioFS)
library(R.matlab) # to read .mat files
library(parallel)
library(doParallel)

###### sys variables --------
# --- file name variables ---
DAT_FILE <- args[1] 

# --- model file ---
MODEL_FILE <- args[2]

# --- directory variables ---
# FIG_OUT_DIR
RES_OUT_DIR <- args[3]

# --- SVM setting ---
NEWDATA_CENTER_SCALE <- eval(parse(text = args[4]))
PROBABILITY_METHOD <- args[5]

# --- parallel computing ---
PSETTING <- eval(parse(text = args[6]))
CORES <- as.numeric(args[7])
if (PSETTING && CORES > parallel::detectCores()) {
  CORE_OUT_OF_RANGE <- TRUE
  CORES <- parallel::detectCores() - 1
}
CPU_CLUSTER <- args[8]

# --- plotting settings ---
PIE_WIDTH <- as.numeric(args[9])
PIE_HEIGHT <- as.numeric(args[10])

###### R script --------
# ------ set the output directory as the working directory ------
setwd(RES_OUT_DIR)  # the folder that all the results will be exports to

# ------ load the model file ------
load(MODEL_FILE)

# ------ load data file ------
raw_sample_dfm <- read.csv(file = DAT_FILE, stringsAsFactors = FALSE, check.names = FALSE)

# ------ subset features according to model ----
if (!all(svm_rf_selected_features %in% names(raw_sample_dfm)[-1])){
  cat("feature_error")
  quit()
} else {
  x <- raw_sample_dfm[, names(raw_sample_dfm) %in% svm_rf_selected_features]
  rownames(x) <- raw_sample_dfm$sampleid
  dat_subset <- data.frame(sampleid = rownames(x), x, check.names = FALSE)
  sampleid_pred <- rownames(x)
}

# ------ export and clean up the mess --------
# export to results files if needed
write.csv(file = paste0(RES_OUT_DIR, "/", "data_subset.csv"), dat_subset, row.names = FALSE)

# ------ predict ------
if (PSETTING) {
  cl <- parallel::makeCluster(CORES, type = CPU_CLUSTER)
  registerDoParallel(cl)
  dummy <- foreach(i = 1:nrow(x), .packages = "RBioFS") %dopar% {
    rbioClass_svm_predcit(object = svm_m,
                          newdata = x[i,],
                          export.name = "pred_x", sampleID.vector = rownames(x[i, ]),
                          prob.method = PROBABILITY_METHOD, verbose = FALSE)
    rbioUtil_classplot(pred.obj = pred_x_svm_predict,
                       export.name = rownames(x[i, ]),
                       plot.Width = PIE_WIDTH, plot.Height = PIE_HEIGHT,
                       verbose = FALSE)
    parallel::stopCluster(cl)
  }
} else {
  dummy <- foreach(i = 1:nrow(x), .packages = "RBioFS") %do% {
    rbioClass_svm_predcit(object = svm_m,
                          newdata = x[i,],
                          export.name = "pred_x", sampleID.vector = rownames(x[i, ]),
                          prob.method = "softmax", verbose = FALSE)
    rbioUtil_classplot(pred.obj = pred_x_svm_predict,
                       export.name = rownames(x[i, ]),                      
                       plot.Width = PIE_WIDTH, plot.Height = PIE_HEIGHT,
                       verbose = FALSE)
  }
}

# ------ display messages so far ------
# cat the samples predicted
cat(sampleid_pred, "\n") # line 3: input mat file dimension
}
