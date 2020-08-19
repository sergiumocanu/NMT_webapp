###### general info --------
## name: mat_process.R
## purpose: load and process mat files
## version: 0.2.0

## flags from Rscript
# args <- commandArgs()
# print(args)
reg_inputDatProcess <- function(args) {
  ###### load libraries --------
  require(foreach)
  require(R.matlab) # to read .mat files
  
  ###### sys variables --------
  # --- file name variables ---
  MAT_FILE <- args[6]
  MAT_FILE_NO_EXT <- args[7]
  ANNOT_FILE <- args[8]
  
  
  # --- directory variables ---
  # FIG_OUT_DIR
  RES_OUT_DIR <- args[11]
  
  # --- mata data input variables ---
  SAMPLEID_VAR <- args[9]
  Y_VAR <- args[10]
  
  ###### R script --------
  # ------ load mat file ------
  raw <- readMat(MAT_FILE)
  raw <- raw[[1]]
  raw_dim <- dim(raw)
  
  # ------ load annotation file (meta data) ------
  annot <-
    read.csv(file = ANNOT_FILE,
             stringsAsFactors = FALSE,
             check.names = FALSE)
  if (!all(c(SAMPLEID_VAR, Y_VAR) %in% names(annot))) {
    cat("none_existent")
    quit()
  }
  if (nrow(annot) != raw_dim[3]) {
    cat("unequal_length")
    quit()
  }
  y <- annot[, Y_VAR]
  sampleid <- annot[, SAMPLEID_VAR]
  
  
  # ------ process the mat file with the mata data ------
  raw_sample <- foreach(i = 1:raw_dim[3], .combine = "rbind") %do% {
    tmp <- raw[, , i]
    colnames(tmp) <- as.character(seq(ncol(tmp)))
    rownames(tmp) <- as.character(seq(ncol(tmp)))
    pair <-
      paste(rownames(tmp)[row(tmp)[upper.tri(tmp)]], colnames(tmp)[col(tmp)[upper.tri(tmp)]], sep = "_")
    sync.value <- tmp[upper.tri(tmp)]
    names(sync.value) <- pair
    sync.value
  }
  raw_sample_dfm <-
    data.frame(sampleid = sampleid,
               y = y,
               raw_sample,
               row.names = NULL)
  colnames(raw_sample_dfm)[-c(1:2)] <- dimnames(raw_sample)[[2]]
  
  
  ####### export and clean up the mess --------
  ## export to results files if needed
  write.csv(
    file = paste0(RES_OUT_DIR, "/", MAT_FILE_NO_EXT, "_2D.csv"),
    raw_sample_dfm,
    row.names = FALSE
  )
  
  ## cat the vairables to export to shell scipt
  cat("\tMat file dimensions: ", raw_dim, "\n") # line 2: input mat file dimension
}
