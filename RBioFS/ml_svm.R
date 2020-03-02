###### general info --------
## name: ml_svm.R
## purpose: svm modelling featuring rRF-FS
## version: 0.2.0

## flags from Rscript
# NOTE: the order of the flags depends on the Rscript command
ml_svm <- function(args){
  # args <- commandArgs()
  # print(args)
  
  ######  load libraries --------
  require(RBioFS)
  require(foreach)
  require(parallel)
  require(limma)
  
  ###### sys variables --------
  # ------ warning flags ------
  CORE_OUT_OF_RANGE <- FALSE
  
  # ------ file name variables ------
  DAT_FILE <- args[6]  # ML file
  print(DAT_FILE)
  MAT_FILE_NO_EXT <- args[7]  # from the raw mat file, for naming export data
  
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
  CPU_CLUSTER <- str_remove_all(args[11], "[\"]")
  TRAINING_PERCENTAGE <- as.numeric(args[12])
  if (TRAINING_PERCENTAGE <= options()$ts.eps || TRAINING_PERCENTAGE == 1) TRAINING_PERCENTAGE <- 0.8
  
  SVM_CV_CENTRE_SCALE <- eval(parse(text = args[13]))
  SVM_CV_KERNEL <- str_remove_all(args[14], "[\"]")
  SVM_CV_CROSS_K <- as.numeric(args[15])
  SVM_CV_TUNE_METHOD <- str_remove_all(args[16], "[\"]")
  SVM_CV_TUNE_CROSS_K <- as.numeric(args[17])
  SVM_CV_TUNE_BOOT_N <- as.numeric(args[18])
  SVM_CV_FS_RF_IFS_NTREE <- as.numeric(args[19])
  SVM_CV_FS_RF_SFS_NTREE <- as.numeric(args[20])
  SVM_CV_BEST_MODEL_METHOD <- str_remove_all(args[21], "[\"]")
  SVM_CV_FS_COUNT_CUTOFF <- as.numeric(args[22])
  
  SVM_CROSS_K <- as.numeric(args[23])
  SVM_TUNE_CROSS_K <- as.numeric(args[24])
  SVM_TUNE_BOOT_N <- as.numeric(args[25])
  
  SVM_PERM_METHOD <- str_remove_all(args[26], "[\"]")  # OPTIONS ARE "BY_Y" AND "BY_FEATURE_PER_Y"
  SVM_PERM_N <- as.numeric(args[27])
  SVM_PERM_PLOT_SYMBOL_SIZE <- as.numeric(args[28])
  SVM_PERM_PLOT_LEGEND_SIZE <- as.numeric(args[29])
  SVM_PERM_PLOT_X_LABEL_SIZE <- as.numeric(args[30])
  SVM_PERM_PLOT_X_TICK_LABEL_SIZE <- as.numeric(args[31])
  SVM_PERM_PLOT_Y_LABEL_SIZE <- as.numeric(args[32])
  SVM_PERM_PLOT_Y_TICK_LABEL_SIZE <- as.numeric(args[33])
  SVM_PERM_PLOT_WIDTH <- as.numeric(args[34])
  SVM_PERM_PLOT_HEIGHT <- as.numeric(args[35])
  
  SVM_ROC_SMOOTH <- eval(parse(text = args[36]))
  SVM_ROC_SYMBOL_SIZE <- as.numeric(args[37])
  SVM_ROC_LEGEND_SIZE <- as.numeric(args[38])
  SVM_ROC_X_LABEL_SIZE <- as.numeric(args[39])
  SVM_ROC_X_TICK_LABEL_SIZE <- as.numeric(args[40])
  SVM_ROC_Y_LABEL_SIZE <- as.numeric(args[41])
  SVM_ROC_Y_TICK_LABEL_SIZE <- as.numeric(args[42])
  SVM_ROC_WIDTH <- as.numeric(args[43])
  SVM_ROC_HEIGHT <- as.numeric(args[44])
  
  PCA_SCALE_DATA <- eval(parse(text = args[45]))
  PCA_CENTRE_DATA <- eval(parse(text = args[46]))
  PCA_BIPLOT_SAMPLELABEL_TYPE <- str_remove_all(args[47], "[\"]")
  PCA_BIPLOT_SAMPLELABEL_SIZE <- as.numeric(args[48])
  PCA_BIPLOT_SYMBOL_SIZE <- as.numeric(args[49])
  PCA_BIPLOT_ELLIPSE <- eval(parse(text = args[50]))
  PCA_BIPLOT_LOADING <- eval(parse(text = args[51]))
  PCA_BIPLOT_LOADING_TEXTSIZE <- as.numeric(args[52])
  PCA_BIPLOT_MULTI_DESITY <- eval(parse(text = args[53]))
  PCA_BIPLOT_MULTI_STRIPLABEL_SIZE <- as.numeric(args[54])
  PCA_RIGHTSIDE_Y <- eval(parse(text = args[55]))
  PCA_X_TICK_LABEL_SIZE <- as.numeric(args[56])
  PCA_Y_TICK_LABEL_SIZE <- as.numeric(args[57])
  PCA_WIDTH <- as.numeric(args[58])
  PCA_HEIGHT <- as.numeric(args[59])
  SVM_RFFS_PCA_PC <- eval(parse(text = args[60]))
  SVM_RFFS_PCA_BIPLOT_ELLIPSE_CONF <- as.numeric(args[61])
  
  # below: for if to do the univariate redution
  CVUNI <- eval(parse(text = args[62]))
  LOG2_TRANS <- eval(parse(text = args[63]))
  CONTRAST <- args[64]
  UNI_FDR <- eval(parse(text = args[65]))
  UNI_ALPHA <- as.numeric(args[66])
  
  ###### R script --------
  # ------ set the output directory as the working directory ------
  setwd(RES_OUT_DIR)  # the folder that all the results will be exports to
  
  # ------ load and processed ML data files ------
  ml_dfm <- read.csv(file = DAT_FILE, stringsAsFactors = FALSE, check.names = FALSE)
  ml_dfm$y <- factor(ml_dfm$y, levels = unique(ml_dfm$y))
  
  # stratified resampling: proportionally sample by groups
  training <- foreach(i = levels(ml_dfm$y), .combine = "rbind") %do% {
    dfm <- ml_dfm[ml_dfm$y == i, ]
    dfm_rand <- dfm[sample(nrow(dfm)), ]
    training_n <- ceiling(nrow(dfm_rand) * TRAINING_PERCENTAGE)
    training <- dfm_rand[1:training_n, ]
  }
  test <- ml_dfm[!rownames(ml_dfm) %in% rownames(training), ]
  
  # ml_dfm_randomized <- ml_dfm[sample(nrow(ml_dfm)), ]
  # training_n <- ceiling(nrow(ml_dfm_randomized) * TRAINING_PERCENTAGE)  # use ceiling to maximize the training set size
  # training <- ml_dfm_randomized[1:training_n, ]
  # test <- ml_dfm_randomized[(training_n + 1):nrow(ml_dfm_randomized), ]
  
  # ------ internal nested cross-validation and feature selection ------
  sink(file = paste0(MAT_FILE_NO_EXT, "_svm_results.txt"), append = TRUE)
  cat("------ Internal nested cross-validation with rRF-FS ------\n")
  svm_nested_cv <- rbioClass_svm_ncv_fs(x = training[, -1],
                                        y = factor(training$y, levels = unique(training$y)),
                                        univariate.fs = CVUNI, uni.log2trans = LOG2_TRANS,
                                        uni.fdr = UNI_FDR, uni.alpha = UNI_ALPHA,
                                        uni.contrast = CONTRAST,
                                        center.scale = SVM_CV_CENTRE_SCALE,
                                        cross.k = SVM_CV_CROSS_K,
                                        tune.method = SVM_CV_TUNE_METHOD,
                                        tune.cross.k = SVM_CV_TUNE_CROSS_K,
                                        tune.boot.n = SVM_CV_TUNE_BOOT_N,
                                        fs.method = "rf",
                                        rf.ifs.ntree = SVM_CV_FS_RF_IFS_NTREE, rf.sfs.ntree = SVM_CV_FS_RF_SFS_NTREE,
                                        fs.count.cutoff = SVM_CV_FS_COUNT_CUTOFF,
                                        cross.best.model.method = SVM_CV_BEST_MODEL_METHOD,
                                        parallelComputing = PSETTING, n_cores = CORES,
                                        clusterType = CPU_CLUSTER,
                                        verbose = TRUE)
  sink()
  svm_rf_selected_pairs <- svm_nested_cv$selected.features
  
  for (i in 1:SVM_CV_CROSS_K){  # plot SFS curve
    rbioFS_rf_SFS_plot(object = get(paste0("svm_nested_iter_", i, "_SFS")),
                       n = "all",
                       plot.file.title = paste0("svm_nested_iter_", i),
                       plot.title = NULL,
                       plot.titleSize = 10, plot.symbolSize = 2, plot.errorbar = c("sem"),
                       plot.errorbarWidth = 0.2, plot.fontType = "sans",
                       plot.xLabel = "Features",
                       plot.xLabelSize = SVM_ROC_X_LABEL_SIZE,
                       plot.xTickLblSize = SVM_ROC_X_TICK_LABEL_SIZE,
                       plot.xAngle = 0,
                       plot.xhAlign = 0.5, plot.xvAlign = 0.5,
                       plot.xTickItalic = FALSE, plot.xTickBold = FALSE,
                       plot.yLabel = "OOB error rate",
                       plot.yLabelSize = SVM_ROC_Y_LABEL_SIZE, plot.yTickLblSize = SVM_ROC_Y_TICK_LABEL_SIZE,
                       plot.yTickItalic = FALSE, plot.yTickBold = FALSE,
                       plot.rightsideY = TRUE,
                       plot.Width = SVM_ROC_WIDTH,
                       plot.Height = SVM_ROC_HEIGHT, verbose = FALSE)
  }
  
  
  # ------ SVM modelling ------
  # sub set the training/test data using the selected features
  svm_training <- training[, c("y", svm_rf_selected_pairs)]
  svm_test <- test[, c("y", svm_rf_selected_pairs)]
  
  # modelling
  svm_m <- rbioClass_svm(x = svm_training[, -1], y = factor(svm_training$y, levels = unique(svm_training$y)),
                         center.scale = SVM_CV_CENTRE_SCALE, kernel = SVM_CV_KERNEL,
                         svm.cross.k = SVM_CROSS_K,
                         tune.method = SVM_CV_TUNE_METHOD,
                         tune.cross.k = SVM_TUNE_CROSS_K, tune.boot.n = SVM_TUNE_BOOT_N,
                         verbose = FALSE)
  
  # permuation test and plotting
  rbioClass_svm_perm(object = svm_m, perm.method = SVM_PERM_METHOD, nperm = SVM_PERM_N,
                     parallelComputing = PSETTING, clusterType =  CPU_CLUSTER, perm.plot = FALSE,
                     verbose = FALSE)
  rbioUtil_perm_plot(perm_res = svm_m_perm,
                     plot.SymbolSize = SVM_PERM_PLOT_SYMBOL_SIZE,
                     plot.legendSize = SVM_PERM_PLOT_LEGEND_SIZE,
                     plot.xLabelSize = SVM_PERM_PLOT_X_LABEL_SIZE,
                     plot.xTickLblSize = SVM_PERM_PLOT_X_TICK_LABEL_SIZE,
                     plot.yLabelSize = SVM_PERM_PLOT_Y_LABEL_SIZE,
                     plot.yTickLblSize = SVM_PERM_PLOT_Y_TICK_LABEL_SIZE,
                     plot.Width = SVM_PERM_PLOT_WIDTH, plot.Height = SVM_PERM_PLOT_HEIGHT)
  
  sink(file = paste0(MAT_FILE_NO_EXT, "_svm_results.txt"), append = TRUE)
  cat("\n\n------ Permutation test ------\n")
  svm_m_perm
  sink()
  
  # ROC-AUC
  sink(file = paste0(MAT_FILE_NO_EXT, "_svm_results.txt"), append = TRUE)
  cat("------ ROC-AUC ------\n")
  rbioClass_svm_roc_auc(object = svm_m, newdata = svm_test[, -1], newdata.label = factor(svm_test$y, levels = unique(svm_test$y)),
                        center.scale.newdata = SVM_CV_CENTRE_SCALE,
                        plot.smooth = SVM_ROC_SMOOTH,
                        plot.legendSize = SVM_ROC_LEGEND_SIZE, plot.SymbolSize = SVM_ROC_SYMBOL_SIZE,
                        plot.xLabelSize = SVM_ROC_X_LABEL_SIZE, plot.xTickLblSize = SVM_ROC_X_TICK_LABEL_SIZE,
                        plot.yLabelSize = SVM_ROC_Y_LABEL_SIZE, plot.yTickLblSize = SVM_ROC_Y_TICK_LABEL_SIZE,
                        plot.Width = SVM_ROC_WIDTH, plot.Height = SVM_ROC_HEIGHT,
                        verbose = FALSE)
  sink()
  
  # FS PCA
  # pca_svm_rffs <- data.frame(row_num = 1:nrow(ml_dfm), ml_dfm[, c("y", svm_rf_selected_pairs)], check.names = FALSE)
  pca_svm_rffs <- data.frame(row_num = 1:nrow(svm_training), svm_training, check.names = FALSE)
  rbioFS_PCA(input = pca_svm_rffs, sampleIDVar = "row_num", groupIDVar = "y",
             scaleData = PCA_SCALE_DATA, centerData = PCA_CENTRE_DATA, boxplot = TRUE,
             boxplot.Title = NULL, boxplot.Width = PCA_WIDTH, boxplot.Height = PCA_HEIGHT,
             biplot = TRUE, biplot.comps = SVM_RFFS_PCA_PC, biplot.Title = NULL,
             biplot.sampleLabel.type = PCA_BIPLOT_SAMPLELABEL_TYPE, biplot.sampleLabelSize = PCA_BIPLOT_SAMPLELABEL_SIZE,
             biplot.sampleLabel.padding = 0.5, biplot.SymbolSize = PCA_BIPLOT_SYMBOL_SIZE,
             biplot.ellipse = PCA_BIPLOT_ELLIPSE, biplot.ellipse_conf = SVM_RFFS_PCA_BIPLOT_ELLIPSE_CONF,
             biplot.xAngle = 0, biplot.xhAlign = 0.5, biplot.xvAlign = 0.5,
             biplot.loadingplot = PCA_BIPLOT_LOADING, biplot.loadingplot.textsize = PCA_BIPLOT_LOADING_TEXTSIZE,
             biplot.mtx.densityplot = PCA_BIPLOT_MULTI_DESITY, biplot.mtx.stripLblSize = PCA_BIPLOT_MULTI_STRIPLABEL_SIZE,
             biplot.Width = PCA_WIDTH, biplot.Height = PCA_HEIGHT, rightsideY = PCA_RIGHTSIDE_Y,
             fontType = "sans", xTickLblSize = PCA_X_TICK_LABEL_SIZE, yTickLblSize = PCA_Y_TICK_LABEL_SIZE,
             verbose = FALSE)
  
  # # hcluster after nested CV (NOTE: uncomment out if needed)
  # svm_training_E <- svm_training[, -1]
  # normdata_crosscv <- list(E = t(svm_training_E),
  #                          genes = data.frame(ProbeName=seq(ncol(svm_training_E)), pair=colnames(svm_training_E)),
  #                          targets = data.frame(id=seq(nrow(training)), sample=training_sampleid),
  #                          ArrayWeight = NULL)
  # if (HTMAP_LAB_ROW) {
  #   rbioarray_hcluster(plotName = paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv"),
  #                      fltlist = normdata_crosscv, n = "all",
  #                      fct = factor(svm_training$y, levels = unique(svm_training$y)),
  #                      ColSideCol = FALSE,
  #                      sampleName = normdata_crosscv$targets$sample,
  #                      genesymbolOnly = FALSE,
  #                      trace = "none", ctrlProbe = FALSE, rmControl = FALSE,
  #                      srtCol = HTMAP_TEXTANGLE_COL, offsetCol = 0,
  #                      key.title = "", dataProbeVar = "pair",
  #                      cexCol = HTMAP_TEXTSIZE_COL, cexRow = HTMAP_TEXTSIZE_ROW,
  #                      keysize = HTMAP_KEYSIZE,
  #                      key.xlab = HTMAP_KEY_XLAB,
  #                      key.ylab = HTMAP_KEY_YLAB,
  #                      plotWidth = HTMAP_WIDTH, plotHeight = HTMAP_HEIGHT,
  #                      margin = HTMAP_MARGIN)
  # } else {
  #   rbioarray_hcluster(plotName = paste0(MAT_FILE_NO_EXT, "_hclust_nestedcv"),
  #                      fltlist = normdata_crosscv, n = "all",
  #                      fct = factor(svm_training$y, levels = unique(svm_training$y)),
  #                      ColSideCol = FALSE,
  #                      sampleName = normdata_crosscv$targets$sample,
  #                      genesymbolOnly = FALSE,
  #                      trace = "none", ctrlProbe = FALSE, rmControl = FALSE,
  #                      srtCol = HTMAP_TEXTANGLE_COL, offsetCol = 0,
  #                      key.title = "", dataProbeVar = "pair", labRow = FALSE,
  #                      cexCol = HTMAP_TEXTSIZE_COL, cexRow= HTMAP_TEXTSIZE_ROW,
  #                      keysize = HTMAP_KEYSIZE,
  #                      key.xlab = HTMAP_KEY_XLAB,
  #                      key.ylab = HTMAP_KEY_YLAB,
  #                      plotWidth = HTMAP_WIDTH, plotHeight = HTMAP_HEIGHT,
  #                      margin = HTMAP_MARGIN)
  # }
  
  ####### clean up the mess and export --------
  ## variables for display
  orignal_y <- factor(ml_dfm$y, levels = unique(ml_dfm$y))
  orignal_y_summary <- foreach(i = 1:length(levels(orignal_y)), .combine = "c") %do%
    paste0(levels(orignal_y)[i], "(", summary(orignal_y)[i], ")")
  
  training_y <- factor(training$y, levels = unique(training$y))
  training_summary <- foreach(i = 1:length(levels(training_y)), .combine = "c") %do%
    paste0(levels(training_y)[i], "(", summary(training_y)[i], ")")
  test_y <- factor(test$y, levels = unique(test$y))
  test_summary <- foreach(i = 1:length(levels(test_y)), .combine = "c") %do%
    paste0(levels(test_y)[i], "(", summary(test_y)[i], ")")
  
  ## export to results files if needed
  # y_randomized <- data.frame(`New order` = seq(length(ml_dfm_randomized$y)), `Randomized group labels` = ml_dfm_randomized$y,
  #                            check.names = FALSE)
  write.csv(file = "ml_training.csv", training, row.names = FALSE)
  write.csv(file = "ml_test.csv", test, row.names = FALSE)
  save(list = c("svm_m", "svm_rf_selected_pairs", "svm_training", "svm_test", "svm_nested_cv"),
       file = paste0(MAT_FILE_NO_EXT, "_final_svm_model.Rdata"))
  
  
  ## cat the vairables to export to shell scipt
  # cat("\t", dim(raw_sample_dfm), "\n") # line 1: file dimension
  # cat("First five variable names: ", names(raw_sample_dfm)[1:5])
  if (CORE_OUT_OF_RANGE) {
    cat("WARNING: CPU core number out of range! Set to maximum cores - 1. \n")
    cat("-------------------------------------\n\n")
  }
  cat("ML data file summary\n")
  cat("-------------------------------------\n")
  cat("ML file dimensions: ", dim(ml_dfm), "\n")
  cat("Group labels (size): ", orignal_y_summary, "\n")
  cat("\n\n")
  cat("Label randomization\n")
  cat("-------------------------------------\n")
  cat("Training and test files saved to: ml_training.csv ml_test.csv\n")
  cat("\n\n")
  cat("Data split\n")
  cat("-------------------------------------\n")
  if (TRAINING_PERCENTAGE <= options()$ts.eps || TRAINING_PERCENTAGE == 1) cat("Invalid percentage. Use default instead.\n")
  cat("Training set percentage: ", TRAINING_PERCENTAGE, "\n")
  cat("Training set: ", training_summary, "\n")
  cat("test set: ", test_summary, "\n")
  cat("\n\n")
  cat("SVM nested cross validation with rRF-FS\n")
  cat("-------------------------------------\n")
  svm_nested_cv
  cat("\n\n")
  cat("SVM modelling\n")
  cat("-------------------------------------\n")
  svm_m
  cat("Total internal cross-validation accuracy: ", svm_m$tot.accuracy/100, "\n")
  cat("Final SVM model saved to file: ", paste0(MAT_FILE_NO_EXT, "_final_svm_model.Rdata\n"))
  cat("\n\n")
  cat("SVM permutation test\n")
  cat("-------------------------------------\n")
  svm_m_perm
  cat("Permutation test results saved to file: svm_m.perm.csv\n")
  cat("Permutation plot saved to file: svm_m_perm.svm.perm.plot.pdf\n")
  cat("\n\n")
  cat("ROC-AUC\n")
  cat("-------------------------------------\n")
  cat("NOTE: Check the SVM results file ", paste0(MAT_FILE_NO_EXT, "_svm_results.txt"), " for AUC values.\n")
  cat("ROC figure saved to file (check SVM result file for AUC value): svm_m.svm.roc.pdf\n")
  cat("\n\n")
  cat("Clustering analysis: SVM training data\n")
  # cat("PCA on SVM selected pairs\n")
  cat("-------------------------------------\n")
  cat("PCA on SVM selected pairs saved to:\n")
  cat("\tbiplot: pca_svm_rffs.pca.biplot.pdf\n")
  cat("\tboxplot: pca_svm_rffs.pca.boxplot.pdf\n")
}