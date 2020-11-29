###### general info --------
## name: cv_reg_plsr_val_svm.R
## purpose: plsr modelling to evaluating SVR results for "cv only" methods
## version: 0.3.0

## flags from Rscript
# NOTE: the order of the flags depends on the Rscript command
args <- commandArgs()
# print(args)

######  load libraries --------
require(RBioFS)
require(foreach)
require(parallel)

###### sys variables --------
# ------ warning flags ------
CORE_OUT_OF_RANGE <- FALSE
NCOMP_WARNING <- FALSE

# ------ file name variables ------
MODEL_FILE <- args[6]  # SVM MODEL R file  
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
CPU_CLUSTER <- args[11]

PLSDA_VALIDATION <- args[12]
PLSDA_VALIDATION_SEGMENT <- as.numeric(args[13])
PLSDA_INIT_NCOMP <- as.numeric(args[14])
PLSDA_NCOMP_SELECT_METHOD <- args[15]
PLSDA_NCOMP_SELECT_PLOT_SYMBOL_SIZE <- as.numeric(args[16])
PLSDA_NCOMP_SELECT_PLOT_LEGEND_SIZE <- as.numeric(args[17])
PLSDA_NCOMP_SELECT_PLOT_X_LABEL_SIZE <- as.numeric(args[18])
PLSDA_NCOMP_SELECT_PLOT_X_TICK_LABEL_SIZE <- as.numeric(args[19])
PLSDA_NCOMP_SELECT_PLOT_Y_LABEL_SIZE <- as.numeric(args[20])
PLSDA_NCOMP_SELECT_PLOT_Y_TICK_LABEL_SIZE <- as.numeric(args[21])

PLSDA_PERM_METHOD <- args[22]
PLSDA_PERM_N <- as.numeric(args[23])
PLSDA_PERM_PLOT_SYMBOL_SIZE <- as.numeric(args[24])
PLSDA_PERM_PLOT_LEGEND_SIZE <- as.numeric(args[25])
PLSDA_PERM_PLOT_X_LABEL_SIZE <- as.numeric(args[26])
PLSDA_PERM_PLOT_X_TICK_LABEL_SIZE <- as.numeric(args[27])
PLSDA_PERM_PLOT_Y_LABEL_SIZE <- as.numeric(args[28])
PLSDA_PERM_PLOT_Y_TICK_LABEL_SIZE <- as.numeric(args[29])
PLSDA_PERM_PLOT_WIDTH <- as.numeric(args[30])
PLSDA_PERM_PLOT_HEIGHT <- as.numeric(args[31])

PLSDA_SCOREPLOT_ELLIPSE_CONF <- as.numeric(args[32])
PCA_BIPLOT_SYMBOL_SIZE <- as.numeric(args[33])
PCA_BIPLOT_ELLIPSE <- eval(parse(text = args[34]))
PCA_BIPLOT_MULTI_DESITY <- eval(parse(text = args[35]))
PCA_BIPLOT_MULTI_STRIPLABEL_SIZE <- as.numeric(args[36])
PCA_RIGHTSIDE_Y <- eval(parse(text = args[37]))
PCA_X_TICK_LABEL_SIZE <- as.numeric(args[38])
PCA_Y_TICK_LABEL_SIZE <- as.numeric(args[39])
PCA_WIDTH <- as.numeric(args[40])
PCA_HEIGHT <- as.numeric(args[41])

PLSDA_ROC_SMOOTH <- eval(parse(text = args[42]))
SVM_ROC_SYMBOL_SIZE <- as.numeric(args[43])
SVM_ROC_LEGEND_SIZE <- as.numeric(args[44])
SVM_ROC_X_LABEL_SIZE <- as.numeric(args[45])
SVM_ROC_X_TICK_LABEL_SIZE <- as.numeric(args[46])
SVM_ROC_Y_LABEL_SIZE <- as.numeric(args[47])
SVM_ROC_Y_TICK_LABEL_SIZE <- as.numeric(args[48])

PLSDA_VIP_ALPHA <- as.numeric(args[49])
PLSDA_VIP_BOOT <- eval(parse(text = args[50]))
PLSDA_VIP_BOOT_N <- as.numeric(args[51])
PLSDA_VIP_PLOT_ERRORBAR <- args[52]  # OPTIONS ARE "SEM" AND "SD"
PLSDA_VIP_PLOT_ERRORBAR_WIDTH <- as.numeric(args[53])
PLSDA_VIP_PLOT_ERRORBAR_LABEL_SIZE <- as.numeric(args[54])
PLSDA_VIP_PLOT_X_TEXTANGLE <- as.numeric(args[55])
PLSDA_VIP_PLOT_X_LABEL_SIZE <- as.numeric(args[56])
PLSDA_VIP_PLOT_X_TICK_LABEL_SIZE <- as.numeric(args[57])
PLSDA_VIP_PLOT_Y_LABEL_SIZE <-as.numeric(args[58])
PLSDA_VIP_PLOT_Y_TICK_LABEL_SIZE <- as.numeric(args[59])
PLSDA_VIP_PLOT_WIDTH <- as.numeric(args[60])
PLSDA_VIP_PLOT_HEIGHT <- as.numeric(args[61])

# random state
RANDOM_STATE <- as.numeric(args[62])

###### R script --------
# ------ set random state if available
if (RANDOM_STATE) {
  set.seed(RANDOM_STATE)
}

# ------ set the output directory as the working directory ------
setwd(RES_OUT_DIR)  # the folder that all the results will be exports to

# ------ load the model file ------
load(file = MODEL_FILE)

# ------ PLs-DA modelling ------
# inital modelling and ncomp optimization
plsr_m <- tryCatch(rbioReg_plsr(x = svm_m$inputX, y = svm_m$inputY,
                                    ncomp = PLSDA_INIT_NCOMP, validation = PLSDA_VALIDATION,
                                    segments = PLSDA_VALIDATION_SEGMENT, maxit = 200,
                                    method = "oscorespls", verbose = FALSE),
                           error = function(w){
                             assign("NCOMP_WARNING", TRUE, envir = .GlobalEnv)
                             rbioReg_plsr(x = svm_m$inputX, y = svm_m$inputY,
                                                        validation = PLSDA_VALIDATION,
                                                        segments = PLSDA_VALIDATION_SEGMENT,
                                                        maxit = 200,
                                                        method = "oscorespls", verbose = FALSE)
                           })

rbioReg_plsr_ncomp_select(plsr_m,
                          ncomp.selection.method = PLSDA_NCOMP_SELECT_METHOD,
                          randomization.nperm = 999,
                          randomization.alpha = 0.05,
                          plot.SymbolSize = PLSDA_NCOMP_SELECT_PLOT_SYMBOL_SIZE,
                          plot.legendSize = PLSDA_NCOMP_SELECT_PLOT_LEGEND_SIZE,
                          plot.yLabel = "RMSEP", verbose = FALSE)

ncomp_select <- max(as.vector(plsr_m_plsr_ncomp_select$ncomp_selected))  # get the maximum ncomp needed
plsr_m_optim <- tryCatch(rbioReg_plsr(x = svm_m$inputX, y = svm_m$inputY,
                                    ncomp = ncomp_select, validation = PLSDA_VALIDATION,
                                    segments = PLSDA_VALIDATION_SEGMENT, maxit = 200,
                                    method = "oscorespls", verbose = FALSE),
                           error = function(w){
                             assign("NCOMP_WARNING", TRUE, envir = .GlobalEnv)
                             rbioReg_plsr(x = svm_m$inputX, y = svm_m$inputY,
                                                        validation = PLSDA_VALIDATION,
                                                        segments = PLSDA_VALIDATION_SEGMENT,
                                                        maxit = 200,
                                                        method = "oscorespls", verbose = FALSE)
                           })

# permutation test
if (length(unique(table(svm_m$inputY))) > 1) {  # if to use adjCV depending on if the data is balanced
  is_adj_cv <- TRUE
} else {
  is_adj_cv <- FALSE
}
rbioReg_plsr_perm(object = plsr_m_optim, nperm = PLSDA_PERM_N,
                     adjCV = is_adj_cv,
                     perm.plot = FALSE,
                     parallelComputing = PSETTING, n_cores = CORES, clusterType = CPU_CLUSTER,
                     verbose = FALSE)
rbioUtil_perm_plot(perm_res = plsr_m_optim_perm, plot.SymbolSize = PLSDA_PERM_PLOT_SYMBOL_SIZE,
                   plot.legendSize = PLSDA_PERM_PLOT_LEGEND_SIZE,
                   plot.xLabelSize = PLSDA_PERM_PLOT_X_LABEL_SIZE, plot.xTickLblSize = PLSDA_PERM_PLOT_X_TICK_LABEL_SIZE,
                   plot.yLabelSize = PLSDA_PERM_PLOT_Y_LABEL_SIZE, plot.yTickLblSize = PLSDA_PERM_PLOT_Y_TICK_LABEL_SIZE,
                   verbose = FALSE)

# # score plot
# rbioClass_plsda_scoreplot(object = plsda_m_optim, comps = 1:ncomp_select,
#                           plot.sampleLabel.type = "none",
#                           plot.ellipse = PCA_BIPLOT_ELLIPSE, plot.ellipse_conf = PLSDA_SCOREPLOT_ELLIPSE_CONF,
#                           plot.SymbolSize = PCA_BIPLOT_SYMBOL_SIZE,
#                           plot.mtx.densityplot = PCA_BIPLOT_MULTI_DESITY,
#                           plot.mtx.stripLblSize = PCA_BIPLOT_MULTI_STRIPLABEL_SIZE,
#                           plot.rightsideY = PCA_RIGHTSIDE_Y,
#                           plot.xTickLblSize = PCA_X_TICK_LABEL_SIZE, plot.yTickLblSize = PCA_Y_TICK_LABEL_SIZE,
#                           plot.Width = PCA_WIDTH, plot.Height = PCA_HEIGHT, verbose = FALSE)


# # ROC-AUC
# sink(file = paste0(MAT_FILE_NO_EXT, "_plsda_results.txt"), append = TRUE)
# cat("------ ROC-AUC ------\n")
# rbioClass_plsda_roc_auc(object = plsda_m_optim,
#                         newdata = svm_test[, -1],
#                         newdata.label = factor(svm_test$y, levels = unique(svm_test$y)),
#                         center.newdata = TRUE,
#                         plot.smooth = PLSDA_ROC_SMOOTH,
#                         plot.SymbolSize = SVM_ROC_SYMBOL_SIZE,
#                         plot.legendSize = SVM_ROC_LEGEND_SIZE,
#                         plot.xLabelSize = SVM_ROC_X_LABEL_SIZE,
#                         plot.xTickLblSize = SVM_ROC_X_TICK_LABEL_SIZE,
#                         plot.yLabelSize = SVM_ROC_Y_LABEL_SIZE,
#                         plot.yTickLblSize = SVM_ROC_Y_TICK_LABEL_SIZE,
#                         plot.Width = 80 * plsda_m_optim$ncomp,
#                         plot.Height = 100,
#                         verbose = TRUE)
# sink()

# VIP plot
rbioReg_plsr_vip(object = plsr_m_optim, comps = 1:plsr_m_optim$ncomp,
                 vip.alpha = PLSDA_VIP_ALPHA, bootstrap = PLSDA_VIP_BOOT,
                 boot.n = PLSDA_VIP_BOOT_N, plot = FALSE,
                 boot.parallelComputing = PSETTING, boot.n_cores = CORES, boot.clusterType = CPU_CLUSTER,
                 verbose = FALSE)
rbioFS_plsda_vip_plot(vip_obj = plsr_m_optim_plsr_vip,
                      plot.errorbar = PLSDA_VIP_PLOT_ERRORBAR,
                      plot.errorbarWidth = PLSDA_VIP_PLOT_ERRORBAR_WIDTH,
                      plot.errorbarLblSize = PLSDA_VIP_PLOT_ERRORBAR_LABEL_SIZE,
                      plot.xAngle = PLSDA_VIP_PLOT_X_TEXTANGLE,
                      plot.xLabelSize = PLSDA_VIP_PLOT_X_LABEL_SIZE,
                      plot.xTickLblSize = PLSDA_VIP_PLOT_X_TICK_LABEL_SIZE,
                      plot.yLabelSize = PLSDA_VIP_PLOT_Y_LABEL_SIZE,
                      plot.yTickLblSize = PLSDA_VIP_PLOT_Y_TICK_LABEL_SIZE,
                      plot.Width = PLSDA_VIP_PLOT_WIDTH,
                      plot.Height = PLSDA_VIP_PLOT_HEIGHT,
                      verbose = FALSE)

####### clean up the mess and export --------
## variables for display

## export to results files if needed
save(list = c("plsr_m_optim", "plsr_m_optim_plsr_vip", "plsr_m_optim_perm"), file = paste0("cv_only_", MAT_FILE_NO_EXT, "_final_plsr_model.Rdata"))


## cat the vairables to export to shell scipt
# cat("\t", dim(raw_sample_dfm), "\n") # line 1: file dimension
# cat("First five variable names: ", names(raw_sample_dfm)[1:5])
if (CORE_OUT_OF_RANGE) {
  cat("WARNING: CPU core number out of range! Set to maximum cores - 1. \n")
  cat("-------------------------------------\n\n")
}
if (NCOMP_WARNING) {
    cat("WARNING: Set ncomp is invalid. Using default: number of classes - 1. \n")
    cat("-------------------------------------\n\n")
}
cat("PLSR ncomp optimization\n")
cat("-------------------------------------\n")
cat("Optimal number of components: ", ncomp_select, "\n")
cat("Final PLSR model saved to file: ", paste0("cv_only_", MAT_FILE_NO_EXT, "_final_plsr_model.Rdata\n"))
cat("RMSEP figure saved to file: plsr_m.plsr.rmsepplot.pdf\n")
cat("\n\n")
cat("PLSR permutation test\n")
cat("-------------------------------------\n")
plsr_m_optim_perm
cat("Permutation figure saved to file: plsr_m_optim_perm.plsr.perm.plot.pdf\n")
cat("\n\n")
cat("PLSR VIP (variable importance) plot\n")
cat("-------------------------------------\n")
cat("VIP alpha: ", PLSDA_VIP_ALPHA, "\n")
cat("VIP Bootstrap: ", PLSDA_VIP_BOOT, "\n")
if (PLSDA_VIP_BOOT) cat("Boostrap iterations: ", PLSDA_VIP_BOOT_N, "\n")
cat("PLSR VIP plot saved to file(s) with the naming format: plsr_m_optim_plsr_vip.<comp>.vip.pdf\n")
