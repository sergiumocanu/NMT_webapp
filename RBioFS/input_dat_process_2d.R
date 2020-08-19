###### general info --------
## name: mat_process.R
## purpose: load and process mat files
## version: 0.2.0

## flags from Rscript
# args <- commandArgs()
# print(args)

inputDatProcess2d <- function(args) {
###### load libraries --------
require(foreach)
require(R.matlab) # to read .mat files

###### sys variables --------
# --- file name variables ---
CSV_2D_FILE <- args[6]
CSV_2D_FILE_NO_EXT <- args[7]
# ANNOT_FILE <- args[8]


# --- directory variables ---
# FIG_OUT_DIR
RES_OUT_DIR <- args[10]

# --- mata data input variables ---
SAMPLEID_VAR <- args[8]
GROUP_VAR <- args[9]

###### R script --------
# ------ load file ------
raw_csv <- read.csv(file = CSV_2D_FILE, stringsAsFactors = FALSE, check.names = FALSE)
if (!all(c(SAMPLEID_VAR, GROUP_VAR) %in% names(raw_csv))) {
  cat("none_existent")
  quit()
}
sample_group <- factor(raw_csv[, GROUP_VAR], levels = unique(raw_csv[, GROUP_VAR]))
sampleid <- raw_csv[, SAMPLEID_VAR]


# ------ process the mat file with the mata data ------
group <- foreach(i = 1:length(levels(sample_group)), .combine = "c") %do% rep(levels(sample_group)[i], times = summary(sample_group)[i])
raw_sample_dfm <- data.frame(sampleid = sampleid, group = group, raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, GROUP_VAR)], row.names = NULL)
names(raw_sample_dfm)[-c(1:2)] <- names(raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, GROUP_VAR)])
# raw_sample_dfm_wo_uni <- data.frame(y = group, raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, GROUP_VAR)], row.names = NULL)
# names(raw_sample_dfm_wo_uni)[-1] <- names(raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, GROUP_VAR)])
raw_sample_dfm_wo_uni <- raw_sample_dfm
names(raw_sample_dfm_wo_uni)[2] <- "y"

####### export and clean up the mess --------
## export to results files if needed
write.csv(file = paste0(RES_OUT_DIR, "/", CSV_2D_FILE_NO_EXT, "_2D.csv"), raw_sample_dfm, row.names = FALSE)
write.csv(file = paste0(RES_OUT_DIR, "/", CSV_2D_FILE_NO_EXT, "_2D_wo_uni.csv"), raw_sample_dfm_wo_uni, row.names = FALSE)

## set up additional variables for cat
group_summary <- foreach(i = 1:length(levels(sample_group)), .combine = "c") %do%
  paste0(levels(sample_group)[i], "(", summary(sample_group)[i], ")")

## cat the vairables to export to shell scipt
cat("\tSample groups (size): ", group_summary, "\n") # line 1: input raw_csv file groupping info
# cat("\tMat file dimensions: ", raw_dim, "\n") # line 2: input mat file dimension
}