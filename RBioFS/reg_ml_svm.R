###### general info --------
## name: ml_svm.R
## purpose: svm modelling featuring rRF-FS
## version: 0.2.0

## flags from Rscript
# NOTE: the order of the flags depends on the Rscript command
# args <- commandArgs()
# print(args)
reg_ml_svm <- function(args) {

######  load libraries --------
  require(RBioFS)
  require(RBioArray)
  require(foreach)
  require(parallel)
  require(limma)
  require(splines)


  ###### sys variables --------
  # ------ warning flags ------
  CORE_OUT_OF_RANGE <- FALSE
  SVM_ROC_THRESHOLD_OUT_OF_RANGE <- FALSE
  
  # ------ file name variables ------
  DAT_FILE <- args[6]  # ML file
  MAT_FILE_NO_EXT <-
    args[7]  # from the raw mat file, for naming export data
  
  # ------ directory variables ------
  RES_OUT_DIR <- args[8]
  
  # ------ processing varaibles ------
  # NOTE: convert string to expression using eval(parse(text = "string"))
  # -- from flags --
  PSETTING <- eval(parse(text = args[9]))
  CORES <- as.numeric(args[10])
  if (PSETTING && CORES > parallel::detectCores()) {
    CORE_OUT_OF_RANGE <- TRUE
    CORES <- parallel::detectCores() - 1
  }
  
  # -- (from config file) --
  CPU_CLUSTER <- args[11]
  TRAINING_PERCENTAGE <- as.numeric(args[12])
  if (TRAINING_PERCENTAGE <= options()$ts.eps ||
      TRAINING_PERCENTAGE == 1)
    TRAINING_PERCENTAGE <- 0.8
  
  SVM_CV_CENTRE_SCALE <- eval(parse(text = args[13]))
  SVM_CV_KERNEL <- args[14]
  SVM_CV_CROSS_K <- as.numeric(args[15])
  SVM_CV_TUNE_METHOD <- args[16]
  SVM_CV_TUNE_CROSS_K <- as.numeric(args[17])
  SVM_CV_TUNE_BOOT_N <- as.numeric(args[18])
  SVM_CV_FS_RF_IFS_NTREE <- as.numeric(args[19])
  SVM_CV_FS_RF_SFS_NTREE <- as.numeric(args[20])
  SVM_CV_BEST_MODEL_METHOD <- args[21]
  SVM_CV_FS_COUNT_CUTOFF <- as.numeric(args[22])
  
  SVM_CROSS_K <- as.numeric(args[23])
  SVM_TUNE_CROSS_K <- as.numeric(args[24])
  SVM_TUNE_BOOT_N <- as.numeric(args[25])
  
  SVM_PERM_METHOD <-
    args[26]  # OPTIONS ARE "BY_Y" AND "BY_FEATURE_PER_Y"
  SVM_PERM_N <- as.numeric(args[27])
  SVM_PERM_PLOT_SYMBOL_SIZE <- as.numeric(args[28])
  SVM_PERM_PLOT_LEGEND_SIZE <- as.numeric(args[29])
  SVM_PERM_PLOT_X_LABEL_SIZE <- as.numeric(args[30])
  SVM_PERM_PLOT_X_TICK_LABEL_SIZE <- as.numeric(args[31])
  SVM_PERM_PLOT_Y_LABEL_SIZE <- as.numeric(args[32])
  SVM_PERM_PLOT_Y_TICK_LABEL_SIZE <- as.numeric(args[33])
  SVM_PERM_PLOT_WIDTH <- as.numeric(args[34])
  SVM_PERM_PLOT_HEIGHT <- as.numeric(args[35])
  
  SVM_ROC_THRESHOLD <- as.numeric(args[36])
  SVM_ROC_SMOOTH <- eval(parse(text = args[37]))
  SVM_ROC_SYMBOL_SIZE <- as.numeric(args[38])
  SVM_ROC_LEGEND_SIZE <- as.numeric(args[39])
  SVM_ROC_X_LABEL_SIZE <- as.numeric(args[40])
  SVM_ROC_X_TICK_LABEL_SIZE <- as.numeric(args[41])
  SVM_ROC_Y_LABEL_SIZE <- as.numeric(args[42])
  SVM_ROC_Y_TICK_LABEL_SIZE <- as.numeric(args[43])
  SVM_ROC_WIDTH <- as.numeric(args[44])
  SVM_ROC_HEIGHT <- as.numeric(args[45])
  
  RFFS_HTMAP_TEXTSIZE_COL <- as.numeric(args[46])
  RFFS_HTMAP_TEXTANGLE_COL <- as.numeric(args[47])
  HTMAP_LAB_ROW <- eval(parse(text = args[48]))
  RFFS_HTMAP_TEXTSIZE_ROW <- as.numeric(args[49])
  RFFS_HTMAP_KEYSIZE <- as.numeric(args[50])
  RFFS_HTMAP_KEY_XLAB <- args[51]
  RFFS_HTMAP_KEY_YLAB <- args[52]
  RFFS_HTMAP_MARGIN <- eval(parse(text = args[53]))
  RFFS_HTMAP_WIDTH <- as.numeric(args[54])
  RFFS_HTMAP_HEIGHT <- as.numeric(args[55])
  
  # below: for if to do the univariate redution
  CVUNI <- eval(parse(text = args[56]))
  LOG2_TRANS <- eval(parse(text = args[57]))
  UNI_FDR <- eval(parse(text = args[58]))
  UNI_ALPHA <- as.numeric(args[59])
  
  # random state
  RANDOM_STATE <- as.numeric(args[60])
  
  ###### R script --------
  # ------ set random state if available
  if (RANDOM_STATE) {
    set.seed(RANDOM_STATE)
  }
  
  # ------ set the output directory as the working directory ------
  setwd(RES_OUT_DIR)  # the folder that all the results will be exports to
  
  # ------ load and processed ML data files ------
  ml_dfm <-
    read.csv(file = DAT_FILE,
             stringsAsFactors = FALSE,
             check.names = FALSE)
  ml_dfm_randomized <- ml_dfm[sample(nrow(ml_dfm)),]
  training_n <-
    ceiling(nrow(ml_dfm_randomized) * TRAINING_PERCENTAGE)  # use ceiling to maximize the training set size
  training <- ml_dfm_randomized[1:training_n,]
  training_sampleid <- training$sampleid
  training <- training[,-1]  # remove sampleid
  test <-
    ml_dfm_randomized[(training_n + 1):nrow(ml_dfm_randomized),]
  test <- test[,-1]  # remove sampleid
  # load(file = paste0(RES_OUT_DIR, "/normdata.Rdata"))
  
  # ------ internal nested cross-validation and feature selection ------
  # sink(file = paste0(MAT_FILE_NO_EXT, "_svm_results.txt"),
  #      append = TRUE)
  cat("------ Internal nested cross-validation with rRF-FS ------\n")
  svm_nested_cv <- rbioClass_svm_ncv_fs(
    x = training[,-1],
    y = training$y,
    center.scale = SVM_CV_CENTRE_SCALE,
    univariate.fs = CVUNI,
    uni.log2trans = LOG2_TRANS,
    uni.fdr = UNI_FDR,
    uni.alpha = UNI_ALPHA,
    cross.k = SVM_CV_CROSS_K,
    tune.method = SVM_CV_TUNE_METHOD,
    tune.cross.k = SVM_CV_TUNE_CROSS_K,
    tune.boot.n = SVM_CV_TUNE_BOOT_N,
    fs.method = "rf",
    rf.ifs.ntree = SVM_CV_FS_RF_IFS_NTREE,
    rf.sfs.ntree = SVM_CV_FS_RF_SFS_NTREE,
    fs.count.cutoff = SVM_CV_FS_COUNT_CUTOFF,
    cross.best.model.method = SVM_CV_BEST_MODEL_METHOD,
    parallelComputing = PSETTING,
    n_cores = CORES,
    clusterType = CPU_CLUSTER,
    verbose = TRUE
  )
  # sink()
  svm_rf_selected_pairs <- svm_nested_cv$selected.features
  rffs_selected_dfm <-
    ml_dfm[, colnames(ml_dfm) %in% c("sampleid", "y", svm_rf_selected_pairs)]  # training + testing
  
  # plot SFS results
  for (i in 1:SVM_CV_CROSS_K) {
    # plot SFS curve
    rbioFS_rf_SFS_plot(
      object = get(paste0("svm_nested_iter_", i, "_SFS")),
      n = "all",
      plot.file.title = paste0("svm_nested_iter_", i),
      plot.title = NULL,
      plot.titleSize = 10,
      plot.symbolSize = 2,
      plot.errorbar = c("sem"),
      plot.errorbarWidth = 0.2,
      plot.fontType = "sans",
      plot.xLabel = "Features",
      plot.xLabelSize = SVM_ROC_X_LABEL_SIZE,
      plot.xTickLblSize = SVM_ROC_X_TICK_LABEL_SIZE,
      plot.xAngle = 0,
      plot.xhAlign = 0.5,
      plot.xvAlign = 0.5,
      plot.xTickItalic = FALSE,
      plot.xTickBold = FALSE,
      plot.yLabel = "MSE",
      plot.yLabelSize = SVM_ROC_Y_LABEL_SIZE,
      plot.yTickLblSize = SVM_ROC_Y_TICK_LABEL_SIZE,
      plot.yTickItalic = FALSE,
      plot.yTickBold = FALSE,
      plot.rightsideY = TRUE,
      plot.Width = SVM_ROC_WIDTH,
      plot.Height = SVM_ROC_HEIGHT,
      verbose = FALSE
    )
  }
  
  
  # ------ SVM modelling ------
  # sub set the training/test data using the selected features
  svm_training <- training[, c("y", svm_rf_selected_pairs)]
  svm_test <- test[, c("y", svm_rf_selected_pairs)]
  
  # modelling
  svm_m <- rbioClass_svm(
    x = svm_training[,-1],
    y = svm_training$y,
    center.scale = SVM_CV_CENTRE_SCALE,
    kernel = SVM_CV_KERNEL,
    svm.cross.k = SVM_CROSS_K,
    tune.method = SVM_CV_TUNE_METHOD,
    tune.cross.k = SVM_TUNE_CROSS_K,
    tune.boot.n = SVM_TUNE_BOOT_N,
    verbose = FALSE
  )
  
  # permuation test and plotting
  rbioClass_svm_perm(
    object = svm_m,
    perm.method = SVM_PERM_METHOD,
    nperm = SVM_PERM_N,
    parallelComputing = PSETTING,
    clusterType =  CPU_CLUSTER,
    perm.plot = FALSE,
    verbose = FALSE
  )
  rbioUtil_perm_plot(
    perm_res = svm_m_perm,
    plot.SymbolSize = SVM_PERM_PLOT_SYMBOL_SIZE,
    plot.legendSize = SVM_PERM_PLOT_LEGEND_SIZE,
    plot.xLabelSize = SVM_PERM_PLOT_X_LABEL_SIZE,
    plot.xTickLblSize = SVM_PERM_PLOT_X_TICK_LABEL_SIZE,
    plot.yLabelSize = SVM_PERM_PLOT_Y_LABEL_SIZE,
    plot.yTickLblSize = SVM_PERM_PLOT_Y_TICK_LABEL_SIZE,
    plot.Width = SVM_PERM_PLOT_WIDTH,
    plot.Height = SVM_PERM_PLOT_HEIGHT
  )
  
  # sink(file = paste0(MAT_FILE_NO_EXT, "_svm_results.txt"),
  #      append = TRUE)
  cat("\n\n------ Permutation test ------\n")
  svm_m_perm
  # sink()
  
  # ROC-AUC
  if (any(SVM_ROC_THRESHOLD < 0) ||
      any(SVM_ROC_THRESHOLD > max(svm_m$inputY))) {
    SVM_ROC_THRESHOLD_OUT_OF_RANGE <- TRUE
    SVM_ROC_THRESHOLD <- round(median(svm_m$inputY))
  }
  
  sink(file = paste0(MAT_FILE_NO_EXT, "_svm_results.txt"),
       append = TRUE)
  
  
  # hcluster after nested CV: all data
  rffs_selected_E <-
    rffs_selected_dfm[,-c(1:2)]  # all sample: training + test
  normdata_crosscv <- list(
    E = t(rffs_selected_E),
    genes = data.frame(
      ProbeName = seq(ncol(rffs_selected_E)),
      pair = colnames(rffs_selected_E)
    ),
    targets = data.frame(id = seq(nrow(
      rffs_selected_dfm
    )), sample = rffs_selected_dfm$sampleid),
    ArrayWeight = NULL
  )
  
  if (HTMAP_LAB_ROW) {
    rbioarray_hcluster(
      plotName = paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv_all_samples"),
      fltlist = normdata_crosscv,
      n = "all",
      fct = factor(svm_training$y, levels = unique(svm_training$y)),
      ColSideCol = FALSE,
      sampleName = normdata_crosscv$targets$sample,
      genesymbolOnly = FALSE,
      trace = "none",
      ctrlProbe = FALSE,
      rmControl = FALSE,
      srtCol = RFFS_HTMAP_TEXTANGLE_COL,
      offsetCol = 0,
      key.title = "",
      dataProbeVar = "pair",
      cexCol = RFFS_HTMAP_TEXTSIZE_COL,
      cexRow = RFFS_HTMAP_TEXTSIZE_ROW,
      keysize = RFFS_HTMAP_KEYSIZE,
      key.xlab = RFFS_HTMAP_KEY_XLAB,
      key.ylab = RFFS_HTMAP_KEY_YLAB,
      plotWidth = RFFS_HTMAP_WIDTH,
      plotHeight = RFFS_HTMAP_HEIGHT,
      margin = RFFS_HTMAP_MARGIN
    )
  } else {
    rbioarray_hcluster(
      plotName = paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv_all_samples"),
      fltlist = normdata_crosscv,
      n = "all",
      fct = factor(svm_training$y, levels = unique(svm_training$y)),
      ColSideCol = FALSE,
      sampleName = normdata_crosscv$targets$sample,
      genesymbolOnly = FALSE,
      trace = "none",
      ctrlProbe = FALSE,
      rmControl = FALSE,
      srtCol = RFFS_HTMAP_TEXTANGLE_COL,
      offsetCol = 0,
      key.title = "",
      dataProbeVar = "pair",
      labRow = FALSE,
      cexCol = RFFS_HTMAP_TEXTSIZE_COL,
      cexRow = RFFS_HTMAP_TEXTSIZE_ROW,
      keysize = RFFS_HTMAP_KEYSIZE,
      key.xlab = RFFS_HTMAP_KEY_XLAB,
      key.ylab = RFFS_HTMAP_KEY_YLAB,
      plotWidth = RFFS_HTMAP_WIDTH,
      plotHeight = RFFS_HTMAP_HEIGHT,
      margin = RFFS_HTMAP_MARGIN
    )
  }
  
  # hcluster after nested CV: training data
  svm_training_E <- svm_training[,-1]
  normdata_crosscv_training <- list(
    E = t(svm_training_E),
    genes = data.frame(
      ProbeName = seq(ncol(svm_training_E)),
      pair = colnames(svm_training_E)
    ),
    targets = data.frame(id = seq(nrow(training)), sample =
                           training_sampleid),
    ArrayWeight = NULL
  )
  
  if (HTMAP_LAB_ROW) {
    rbioarray_hcluster(
      plotName = paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv_training"),
      fltlist = normdata_crosscv_training,
      n = "all",
      fct = factor(svm_training$y, levels = unique(svm_training$y)),
      ColSideCol = FALSE,
      sampleName = normdata_crosscv_training$targets$sample,
      genesymbolOnly = FALSE,
      trace = "none",
      ctrlProbe = FALSE,
      rmControl = FALSE,
      srtCol = RFFS_HTMAP_TEXTANGLE_COL,
      offsetCol = 0,
      key.title = "",
      dataProbeVar = "pair",
      cexCol = RFFS_HTMAP_TEXTSIZE_COL,
      cexRow = RFFS_HTMAP_TEXTSIZE_ROW,
      keysize = RFFS_HTMAP_KEYSIZE,
      key.xlab = RFFS_HTMAP_KEY_XLAB,
      key.ylab = RFFS_HTMAP_KEY_YLAB,
      plotWidth = RFFS_HTMAP_WIDTH,
      plotHeight = RFFS_HTMAP_HEIGHT,
      margin = RFFS_HTMAP_MARGIN
    )
  } else {
    rbioarray_hcluster(
      plotName = paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv_training"),
      fltlist = normdata_crosscv_training,
      n = "all",
      fct = factor(svm_training$y, levels = unique(svm_training$y)),
      ColSideCol = FALSE,
      sampleName = normdata_crosscv_training$targets$sample,
      genesymbolOnly = FALSE,
      trace = "none",
      ctrlProbe = FALSE,
      rmControl = FALSE,
      srtCol = RFFS_HTMAP_TEXTANGLE_COL,
      offsetCol = 0,
      key.title = "",
      dataProbeVar = "pair",
      labRow = FALSE,
      cexCol = RFFS_HTMAP_TEXTSIZE_COL,
      cexRow = RFFS_HTMAP_TEXTSIZE_ROW,
      keysize = RFFS_HTMAP_KEYSIZE,
      key.xlab = RFFS_HTMAP_KEY_XLAB,
      key.ylab = RFFS_HTMAP_KEY_YLAB,
      plotWidth = RFFS_HTMAP_WIDTH,
      plotHeight = RFFS_HTMAP_HEIGHT,
      margin = RFFS_HTMAP_MARGIN
    )
  }
  
  
  ####### clean up the mess and export --------
  ## clean up the mess from Pathview
  suppressWarnings(rm(cpd.simtypes, gene.idtype.bods, gene.idtype.list, korg))
  
  # ## variables for display
  # orignal_y <- factor(ml_dfm$y, levels = unique(ml_dfm$y))
  # orignal_y_summary <- foreach(i = 1:length(levels(orignal_y)), .combine = "c") %do%
  #   paste0(levels(orignal_y)[i], "(", summary(orignal_y)[i], ")")
  #
  # training_y <- factor(training$y, levels = unique(training$y))
  # training_summary <- foreach(i = 1:length(levels(training_y)), .combine = "c") %do%
  #   paste0(levels(training_y)[i], "(", summary(training_y)[i], ")")
  # test_y <- factor(test$y, levels = unique(test$y))
  # test_summary <- foreach(i = 1:length(levels(test_y)), .combine = "c") %do%
  #   paste0(levels(test_y)[i], "(", summary(test_y)[i], ")")
  
  ## export to results files if needed
  y_randomized <-
    data.frame(
      `New order` = seq(length(ml_dfm_randomized$y)),
      `Randomized group labels` = ml_dfm_randomized$y,
      check.names = FALSE
    )
  output_for_dl <- ml_dfm[, c("sampleid", "y", svm_rf_selected_pairs)]
  
  write.csv(file = "ml_randomized_group_label_order.csv", y_randomized, row.names = FALSE)
  write.csv(
    file = paste0(MAT_FILE_NO_EXT, "_dl.csv"),
    output_for_dl,
    row.names = FALSE
  )
  save(
    list = c(
      "svm_m",
      "svm_rf_selected_pairs",
      "svm_training",
      "svm_test",
      "svm_nested_cv"
    ),
    file = paste0(MAT_FILE_NO_EXT, "_final_svm_model.Rdata")
  )
  
  
  ## cat the vairables to export to shell scipt
  # cat("\t", dim(raw_sample_dfm), "\n") # line 1: file dimension
  # cat("First five variable names: ", names(raw_sample_dfm)[1:5])
  if (CORE_OUT_OF_RANGE) {
    cat("WARNING: CPU core out of range! Set to maximum cores - 1. \n")
    cat("-------------------------------------\n\n")
  }
  cat("ML data file summary\n")
  cat("-------------------------------------\n")
  cat("ML file dimensions: ", dim(ml_dfm), "\n")
  cat("\n\n")
  cat("Label randomization\n")
  cat("-------------------------------------\n")
  cat("Randomized y order saved to file: ml_randomized_group_label_order.csv\n")
  cat("\n\n")
  cat("Data split\n")
  cat("-------------------------------------\n")
  if (TRAINING_PERCENTAGE <= options()$ts.eps ||
      TRAINING_PERCENTAGE == 1)
    cat("Invalid percentage. Use default instead.\n")
  cat("Training set percentage: ", TRAINING_PERCENTAGE, "\n")
  cat("\n\n")
  cat("SVM nested cross validation with rRF-FS\n")
  cat("-------------------------------------\n")
  svm_nested_cv
  cat("\n\n")
  cat("Clustering analysis: SVM training data upon nested CV\n")
  cat("-------------------------------------\n")
  cat(
    "Hierarchical clustering heatmap saved to: ",
    paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv.pdf\n")
  )
  cat("\n\n")
  cat("SVM modelling\n")
  cat("-------------------------------------\n")
  svm_m
  cat("Total internal cross-validation RMSE: ",
      rbioReg_svm_rmse(object = svm_m),
      "\n")
  cat(
    "Final SVM model saved to file: ",
    paste0(MAT_FILE_NO_EXT, "_final_svm_model.Rdata\n")
  )
  cat("Data with selected features saved to file: ",
      paste0(MAT_FILE_NO_EXT, "_dl.csv\n"))
  cat("\n\n")
  cat("SVM permutation test\n")
  cat("-------------------------------------\n")
  svm_m_perm
  cat("Permutation test results saved to file: svm_m.perm.csv\n")
  cat("Permutation plot saved to file: svm_m_perm.svm.perm.plot.pdf\n")
  cat("\n\n")
  cat("ROC-AUC\n")
  cat("-------------------------------------\n")
  if (SVM_ROC_THRESHOLD_OUT_OF_RANGE)
    cat("ROC threshold(s) out of range: use median y value instead.\n\n")
  cat(
    "NOTE: Check the SVM results file ",
    paste0(MAT_FILE_NO_EXT, "_svm_results.txt"),
    " for AUC values.\n"
  )
  cat("ROC figure saved to file (check SVM result file for AUC value): svm_m.svm.roc.pdf\n")
  cat("\n\n")
  cat("Clustering analysis\n")
  # cat("PCA on SVM selected pairs\n")
  cat("-------------------------------------\n")
  cat("Hierarchical clustering on CV-SVR-rRF-FS selected pairs saved to:\n")
  cat("\tOn all data:\n")
  cat(
    "\t\t",
    paste0(
      MAT_FILE_NO_EXT,
      "_hclust_nestedcv_all_samples_heatmap.pdf"
    ),
    "\n"
  )
  cat("\tOn training data:\n")
  cat("\t\t",
      paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv_training_heatmap.pdf"),
      "\n")
  
}