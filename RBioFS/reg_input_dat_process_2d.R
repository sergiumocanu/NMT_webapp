###### general info --------
## name: mat_process.R
## purpose: load and process mat files
## version: 0.2.0

## flags from Rscript
args <- commandArgs()
# print(args)

###### load libraries --------
require(foreach)

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
Y_VAR <- args[9]

###### R script --------
# ------ load 2d file ------
raw_csv <- read.csv(file = CSV_2D_FILE, stringsAsFactors = FALSE, check.names = FALSE)
if (!all(c(SAMPLEID_VAR, Y_VAR) %in% names(raw_csv))) {
  cat("none_existent")
  quit()
}
y <- raw_csv[, Y_VAR]
sampleid <- raw_csv[, SAMPLEID_VAR]

# ------ process the mat file with the mata data ------
raw_sample_dfm <- data.frame(sampleid = sampleid, y = y, raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, Y_VAR)], row.names = NULL)
names(raw_sample_dfm)[-c(1:2)] <- names(raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, Y_VAR)])
# below: no need as regression data doesn't have a "group" variable
# raw_sample_dfm_wo_uni <- data.frame(y = y, raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, GROUP_VAR)], row.names = NULL)
# names(raw_sample_dfm_wo_uni)[-1] <- names(raw_csv[, !names(raw_csv) %in% c(SAMPLEID_VAR, GROUP_VAR)])

####### export and clean up the mess --------
## export to results files if needed
write.csv(file = paste0(RES_OUT_DIR, "/", CSV_2D_FILE_NO_EXT, "_2D.csv"), raw_sample_dfm, row.names = FALSE)
# write.csv(file = paste0(RES_OUT_DIR, "/", CSV_2D_FILE_NO_EXT, "_2D_wo_uni.csv"), raw_sample_dfm_wo_uni, row.names = FALSE)

## cat the vairables to export to shell scipt
cat("\tMat file dimensions: ", raw_dim, "\n") # line 2: input mat file dimension
