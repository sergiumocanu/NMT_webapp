
uploadFiles <- function(dataID, annotationsID, nodeID){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "Upload Data Files",
    width = 6,
    height = 350,
    fileInput(
      inputId = dataID,
      label = "Upload Data",
      multiple = FALSE,
      accept = c(".mat", ".csv")
    ),
    fileInput(
      inputId = annotationsID,
      label = "Upload Annotations",
      multiple = FALSE,
      accept = ".csv"
    ),
    fileInput(
      inputId = nodeID,
      label = "Upload Node Data",
      multiple = FALSE,
      accept = ".csv"
    )
  )
}

checklist <- function(upload_checkID, variable_checkID, contrast_checkID, inputdata_checkID, 
                      univariate_checkID, svm_checkID, configfilebuttonID){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "Checklist",
    width = 6,
    height = 350,
    useShinyjs(),
    
    disabled(
      prettyCheckbox(
        inputId = upload_checkID,
        label = "Upload Files",
        status = "success",
        fill = TRUE
      ),
      prettyCheckbox(
        inputId = variable_checkID,
        label = "Choose Variables",
        status = "success",
        fill = TRUE
      ),
      prettyCheckbox(
        inputId = contrast_checkID,
        label = "Contrast Variables",
        status = "success",
        fill = TRUE
      ),
      prettyCheckbox(
        inputId = inputdata_checkID,
        label = "Data Process",
        status = "success",
        fill = TRUE
      ),
      prettyCheckbox(
        inputId = univariate_checkID,
        label = "Univariate Analysis",
        status = "success",
        fill = TRUE
      ),
      prettyCheckbox(
        inputId = svm_checkID,
        label = "SVM Analysis",
        status = "success",
        fill = TRUE
      )
    )
  )
}

viewData <- function(trainingID, annotationID, nodeID){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "View Data",
    width = 8,
    height = 650,
    tabsetPanel(
      id = "data",
      tabPanel("Training", DT::dataTableOutput(trainingID)),
      tabPanel("Annotation", DT::dataTableOutput(annotationID)),
      tabPanel("Node", DT::dataTableOutput(nodeID))
    )
  )
}

inputConfigurations <- function(){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "Configuration Settings",
    width = 4,
    height = 650,
    div(
      id = "configurations",
      
      h4("Parallel computing, number of cores to use", align = "center"),
      textInput(
        inputId = "cores",
        label = "Cores",
        value = "2",
        placeholder = "2"
      ),
      
      h4("Random State", align = "center"),
      textInput(
        inputId = "random_state",
        label = "Random State",
        value = "999",
        placeholder = "999"
      ),
      
      h4("Settings for data pre-processing", align = "center"),
      selectInput(
        inputId = "log2_trans",
        label = "Transform data to log2",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      
      hr(),
      h4("Settings for all-connection clustering analysis", align = "center"),
      h5("Hierarchical clustering", align = "center"),
      textInput(
        inputId = "htmap_textsize_col",
        label = "Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "htmap_textangle_col",
        label = "Heatmap Text Angle Column",
        value = "90",
        placeholder = "90"
      ),
      selectInput(
        inputId = "htmap_lab_row",
        choices = c("TRUE", "FALSE"),
        label = "Heatmap Label Row",
        selected = "TRUE"
      ),
      textInput(
        inputId = "htmap_textsize_row",
        label = "Heatmap Text Size Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "htmap_keysize",
        label = "Heatmap Keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "htmap_key_xlab",
        label = "Heatmap Key X-Label",
        value = "Normalized Connectivity",
        placeholder = "Normalized Connectivity"
      ),
      textInput(
        inputId = "htmap_key_ylab",
        label = "Heatmap Key Y-label",
        value = "Pair Count",
        placeholder = "Pair Count"
      ),
      textInput(
        inputId = "htmap_margin",
        label = "Heatmap Margin",
        value = "(6, 3)",
        placeholder = "(6, 3)"
      ),
      textInput(
        inputId = "htmap_width",
        label = "Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "htmap_height",
        label = "Heatmap Height",
        value = "10",
        placeholder = "10"
      ),
      
      
      hr(),
      h5("Principal Component Analysis (PCA)", align = "center"),
      selectInput(
        inputId = "pca_scale_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Scale Data",
        selected = "TRUE"
      ),
      selectInput(
        inputId = "pca_center_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Center Data",
        selected = "TRUE"
      ),
      textInput(
        inputId = "pca_pc",
        label = "PCA Princpal Components",
        value = "(1, 2)",
        placeholder = "(1, 2)"
      ),
      selectInput(
        inputId = "pca_biplot_samplelabel_type",
        label = "PCA Biplot Show Sample Labels on graph",
        choices = c("none", "direct", "indirect"),
        selected = "none"
      ),
      textInput(
        inputId = "pca_biplot_samplelabel_size",
        label = "PCA Biplot Sample Label Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "pca_biplot_symbol_size",
        label = "PCA Biplot Sample Symbol Size",
        value = "5",
        placeholder = "5"
      ),
      selectInput(
        inputId = "pca_biplot_ellipse",
        label = "PCA Biplot Draw Ellipses",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "pca_biplot_ellipse_conf",
        label = "PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      selectInput(
        inputId = "pca_biplot_loading",
        label = "PCA Biplot Loading Plot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "pca_biplot_loading_textsize",
        label = "PCA Biplot Loading Textsize",
        value = "3",
        placeholder = "3"
      ),
      selectInput(
        inputId = "pca_biplot_multi_density",
        label = "PCA Biplot Display Density Plot on Diagonal Correlation Scoreplot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "pca_biplot_multi_striplabel_size",
        label = "PCA Biplot Label Font Size for Correlation Scoreplot",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "pca_rightside_y",
        label = "PCA Show Right Side y-axis for Boxplot and Biplot",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "pca_x_tick_label_size",
        label = "PCA X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "pca_y_tick_label_size",
        label = "PCA Y-tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "pca_width",
        label = "PCA Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "pca_height",
        label = "PCA Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h4("Settings for univariate analysis", align = "center"),
      h5("Univariate Analysis", align = "center"),
      selectInput(
        inputId = "uni_fdr",
        label = "Univariate False Discovery Rate p value thresholding",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "uni_alpha",
        label = "Univariate Alpha Value",
        value = "0.05",
        placeholder = "0.05"
      ),
      textInput(
        inputId = "uni_fold_change",
        label = "Univariate Threshold for Fold Change",
        value = "1",
        placeholder = "1"
      ),
      textInput(
        inputId = "volcano_n_top_connections",
        label = "Volcano Number of Top Connections",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "volcano_symbol_size",
        label = "Volcano Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "volcano_sig_colour",
        label = "Volcano Colour of Significant Values",
        value = "red",
        placeholder = "red"
      ),
      textInput(
        inputId = "volcano_nonsig_colour",
        label = "Volcano Colour of Non-Significant Values",
        value = "gray",
        placeholder = "gray"
      ),
      textInput(
        inputId = "volcano_x_text_size",
        label = "Volcano X-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "volcano_y_text_size",
        label = "Volcano Y-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "volcano_width",
        label = "Volcano Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "volcano_height",
        label = "Volcano Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("Hierarchical clustering: Sig Connections", align = "center"),
      textInput(
        inputId = "sig_htmap_textsize_col",
        label = "Sig Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "sig_htmap_textangle_col",
        label = "Sig Heatmap Textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "sig_htmap_textsize_row",
        label = "Sig Heatmap Text Size Row",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "sig_htmap_keysize",
        label = "Sig Heatmap Key Size",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "sig_htmap_key_xlab",
        label = "Sig Heatmap Key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "sig_htmap_key_ylab",
        label = "Sig Heatmap Key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "sig_htmap_margin",
        label = "Sig heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "sig_htmap_width",
        label = "Sig Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "sig_htmap_height",
        label = "Sig Heatmap Width",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "sig_pca_pc",
        label = "Sig PCA Principal Components",
        value = "(1, 2)",
        placeholder = "(1, 2)"
      ),
      textInput(
        inputId = "sig_pca_biplot_ellipse_conf",
        label = "Sig PCA Biplot Ellipse Confidence",
        value = "0.9",
        placeholder = "0.9"
      ),
      
      hr(),
      h4("Settings for Support Vector Machines (SVM) machine learning analysis", align = "center"),
      h5("General Settings", align = "center"),
      selectInput(
        inputId = "cpu_cluster",
        label = "CPU Cluster for Parallel Computing",
        choices = c("FORK", "PSOCK"),
        selected = "PSOCK"
      ),
      
      hr(),
      h5("Data Processing", align = "center"),
      textInput(
        inputId = "training_percentage",
        label = "Training Percentage",
        value = "0.85",
        placeholder = "0.85"
      ),
      
      hr(),
      h5("SVM internal nested cross-validation (CV) and feature selection"),
      selectInput(
        inputId = "svm_cv_center_scale",
        label = "SVM CV Center Scale the Data",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      selectInput(
        inputId = "svm_cv_kernel",
        label = "SVM CV Kernel",
        choices = c("linear", "polynomial", "radial", "sigmoid"),
        selected = "radial"
      ),
      textInput(
        inputId = "svm_cv_cross_k",
        label = "SVM CV fold of cross validation",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "svm_cv_tune_method",
        label = "SVM CV Parameter Tuning Method",
        choices = c("cross", "boot", "fix"),
        selected = "cross"
      ),
      textInput(
        inputId = "svm_cv_tune_cross_k",
        label = "SVM CV fold number for CV.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_cv_tune_boot_n",
        label = "SVM CV Bootstrat iterations.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_cv_fs_rf_ifs_ntree",
        label = "SVM CV initial feature selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      textInput(
        inputId = "svm_cv_fs_rf_sfs_ntree",
        label = "SVM CV sequation forward selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      selectInput(
        inputId = "svm_cv_best_model_method",
        label = "SVM CV method to select best CV mode for feature selection",
        choices = c("none", "median"),
        selected = "none"
      ),
      textInput(
        inputId = "svm_cv_fs_count_cutoff",
        label = "SVM CV Feature Vote Cutoff",
        value = "2",
        placeholder = "2"
      ),
      
      hr(),
      h5("SVM modelling", align = "center"),
      textInput(
        inputId = "svm_cross_k",
        label = "SVM Fold of Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_tune_cross_k",
        label = "SVM Fold Number for Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_tune_boot_n",
        label = "SVM Bootstrap Iterations",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("SVM permutation test", align = "center"),
      selectInput(
        inputId = "svm_perm_method",
        label = "SVM Permutation Method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "svm_perm_n",
        label = "SVM Number of permutations.",
        value = "99",
        placeholder = "99"
      ),
      textInput(
        inputId = "svm_perm_plot_symbol_size",
        label = "SVM Permutation Plot Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "svm_perm_plot_legend_size",
        label = "SVM Permutation Plot Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "svm_perm_plot_x_label_size",
        label = "SVM Permutation Plot X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_perm_plot_x_tick_label_size",
        label = "SVM Permutation Plot X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_perm_plot_y_label_size",
        label = "SVM Permutation Plot Y-Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_perm_plot_y_tick_label_size",
        label = "SVM Permutation Plot Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_perm_plot_width",
        label = "SVM Permutation Plot Width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "svm_perm_plot_height",
        label = "SVM Permutation Plot Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("SVM Receiver Operating Curve (ROC) Area Under The Curve (AUC)", align = "center"),
      selectInput(
        inputId = "svm_roc_smooth",
        label = "SVM ROC Smooth Curve",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "svm_roc_symbol_size",
        label = "SVM ROC Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "svm_roc_legend_size",
        label = "SVM ROC Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "svm_roc_x_label_size",
        label = "SVM ROC X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_roc_x_tick_label_size",
        label = "SVM ROC X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_roc_y_label_size",
        label = "SVM ROC Y-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_roc_y_tick_label_size",
        label = "SVM ROC Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "svm_roc_width",
        label = "SVM ROC Plot Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "svm_roc_height",
        label = "SVM ROC Plot Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("SVM Recurrent Random Forrest Feature Selection (RFFS) PCA", align = "center"),
      textInput(
        inputId = "svm_rffs_pca_pc",
        label = "SVM RFFS PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "svm_rffs_pca_biplot_ellipse_conf",
        label = "SVM RFFS PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      
      hr(),
      h5("RFFS hierarchical"),
      textInput(
        inputId = "rffs_htmap_textsize_col",
        label = "RFFS hetmap textsize Column",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "rffs_htmap_textangle_col",
        label = "RFFS heatmap textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "rffs_htmap_textsize_row",
        label = "RFFS heatmap textsize Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "rffs_htmap_keysize",
        label = "RFFS heatmap keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "rffs_htmap_key_xlab",
        label = "RFFS heatmap key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "rffs_htmap_key_ylab",
        label = "RFFS heatmap key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "rffs_htmap_margin",
        label = "RFFS heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "rffs_htmap_width",
        label = "RFFS heatmap width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "rffs_htmap_height",
        label = "RFFS heatmap height",
        value = "15",
        placeholder = "15"
      ),
      
      hr(),
      h4("Settings for Partial Least Squares-Discriminant Analysis (PLS-DA) Modelling for evaluating SVM results", align = "center"),
      h5("Global PLS-DA Settings", align = "center"),
      selectInput(
        inputId = "plsda_validation",
        label = "PLS-DA validation type (none, Cross-validation, Leave-One-Out)",
        choices = c("none", "CV", "LOO"),
        selected = "CV"
      ),
      textInput(
        inputId = "plsda_validation_segment",
        label = "PLS-DA Number of validation segments",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("Initial PLS-DA Modelling Settings", align = "center"),
      textInput(
        inputId = "plsda_init_ncomp",
        label = "PLS-DA Initial number of components",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("PLS-DA ncomp selection", align = "center"),
      selectInput(
        inputId = "plsda_ncomp_select_method",
        label = "PLS-DA number of components select method (minimum RMSE, 1 standard error, randomization)",
        choices = c("min", "1err", "randomization"),
        selected = "1err"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_symbol_size",
        label = "PLS-DA number of components selected plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_legend_size",
        label = "PLS-DA number of componets selected plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_x_label_size",
        label = "PLS-DA number of components selected plot X-label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_x_tick_label_size",
        label = "PLS-DA number of components selected plot X-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_y_label_size",
        label = "PLS-DA number of components selected plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_y_tick_label_size",
        label = "PLS-DA number of components selected plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("PLS-DA permutation test", align = "center"),
      selectInput(
        inputId = "plsda_perm_method",
        label = "PLS-DA permutation method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "plsda_perm_n",
        label = "PLS-DA number of permutations",
        value = "999",
        placeholder = "999"
      ),
      textInput(
        inputId = "plsda_perm_plot_symbol_size",
        label = "PLS-DA permutation plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "plsda_perm_plot_legend_size",
        label = "PLS-DA permutation plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "plsda_perm_plot_x_label_size",
        label = "PLS-DA permutation plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_perm_plot_x_tick_label_size",
        label = "PLS-DA permutation plot X-Tick label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "plsda_perm_plot_y_label_size",
        label = "PLS-DA permutation plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_perm_plot_y_tick_label_size",
        label = "PLS-DA permutation plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_perm_plot_width",
        label = "PLS-DA permutation plot width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "plsda_perm_plot_height",
        label = "PLS-DA permutation plot height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("PLS-DA score plot", align = "center"),
      textInput(
        inputId = "plsda_scoreplot_ellipse_conf",
        label = "PLS-DA Score plot ellipse confidence",
        value = "0.95",
        placeholder = "0.95"
      ),
      
      hr(),
      h5("PLS-DA ROC-AUC", align = "center"),
      selectInput(
        inputId = "plsda_roc_smooth",
        label = "PLS-DA ROC Curve Smooth",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      
      hr(),
      h5("PLS-DA Variable Importance in Projection (VIP) Analysis", align = "center"),
      textInput(
        inputId = "plsda_vip_alpha",
        label = "PLS-DA VIP alpha value",
        value = "0.8",
        placeholder = "0.8"
      ),
      selectInput(
        inputId = "plsda_vip_boot",
        label = "PLS-DA VIP bootstrap method",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "plsda_vip_boot_n",
        label = "PLS-DA VIP bootstrap number of iterations",
        value = "50",
        placeholder = "50"
      ),
      selectInput(
        inputId = "plsda_vip_plot_errorbar",
        label = "PLS-DA VIP plot type of errorbar (SEM = Standard Error of the mean; SD = Standard Deviation)",
        choices = c("SEM", "SD"),
        selected = "SEM"
      ),
      textInput(
        inputId = "plsda_vip_plot_errorbar_width",
        label = "PLS-DA VIP plot errorbar width",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "plsda_vip_plot_errorbar_label_size",
        label = "PLS-DA VIP plot errorbar label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "plsda_vip_plot_x_textangle",
        label = "PLS-DA VIP plot X-Textangle",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "plsda_vip_plot_x_label_size",
        label = "PLS-DA VIP plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_vip_plot_x_tick_label_size",
        label = "PLS-DA VIP plot X-Tick label size",
        value = "4",
        placeholder = "4"
      ),
      textInput(
        inputId = "plsda_vip_plot_y_label_size",
        label = "PLS-DA VIP plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_vip_plot_y_tick_label_size",
        label = "PLS-DA VIP plot Y-Tick label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "plsda_vip_plot_width",
        label = "PLS-DA VIP plot width",
        value = "150",
        placeholder = "150"
      ),
      textInput(
        inputId = "plsda_vip_plot_height",
        label = "PLS-DA VIP plot height",
        value = "100",
        placeholder = "100"
      ),
      tags$head(
        tags$style("#configurations{overflow-y:scroll; max-height: 382px;}")
      )
    ),
    
    # tooltips to help with use
    bsTooltip(
      id = "htmap_margin",
      "Format should be: (x, y)",
      placement = "top",
      trigger = "click",
      options = list(container = "body")
    ),
    
    hr(),
    fileInput(inputId = "upload_configs", label = "Upload Configurations"),
    downloadButton(outputId = "download_configs", label = "Download Configurations"),
    actionButton("reset_cnfg", "Reset All")
  )
}

executeSVM <- function(univariate_bttnID, svm_bttnID, modeltypeID, noneID, ksetID, usetID){
  box(
    title = "Execute Commands",
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    width = 4,
    height = 400,
    useShinyjs(),
    disabled(actionButton(
      inputId = univariate_bttnID,
      label = "Run Univariate",
      width = 125
    )),
    disabled(actionButton(
      inputId = svm_bttnID,
      label = "Run SVM model",
      width = 125
    )),
    radioButtons(
      inputId = modeltypeID,
      label = "Run SVM with or without prior knowledge: ",
      choices = c("Do not incorporate univariate prior knowledge during SVM analysis (neither 'k' nor 'u' flag set)" = noneID,
                  "Incorporate univariate prior knowledge during SVM analysis ('k' flag is set)" = ksetID,
                  "Use univarate analysis result during CV-SVM-rRF-FS ('u' flag set)" = usetID),
      selected = usetID
    )
  )
}

consoleOutput <- function(consoleID){
  box(
    title = "Console Output",
    solidHeader = TRUE,
    collapsible = TRUE,
    # background = "olive",
    status = "primary",
    width = 8,
    height = 400,
    
    verbatimTextOutput(outputId = consoleID),
    tags$head(
      tags$style(
        "#console{overflow-y:scroll; max-height: 300px; background: ghostwhite;}",
        "#reg_console{overflow-y:scroll; max-height: 300px; background: ghostwhite;}",
        "#cv_console{overflow-y:scroll; max-height: 300px; background: ghostwhite;}",
        "#cv_reg_console{overflow-y:scroll; max-height: 300px; background: ghostwhite;}"
      )
    )
    
  )
}

results <- function(results_tabID, select_uni_plotID, select_svm_plotID, svm_plotsID, ml_svm_plotsID,
                    dwldreportID, dwldfilesID) {
  tabBox(
    title = "Results",
    id = results_tabID,
    width = 12,
    height = 600,
    tabPanel(
      "Univariate",
      selectInput(
        inputId = select_uni_plotID,
        label = "Univariate Plots",
        choices = NULL,
        width = '100%'
      ),
      imageOutput(svm_plotsID),
      downloadButton(dwldreportID, "Generate Report"),
      downloadButton(dwldfilesID, "Download All Files")
    ),
    tabPanel(
      "SVM",
      selectInput(
        inputId = select_svm_plotID,
        label = "SVM Plots",
        choices = NULL,
        width = '100%'
      ),
      imageOutput(ml_svm_plotsID)
    )
      
    # downloadButton(outputId = dwldreportID, label = "Generate Report")
    # downloadButton(outputID = dwldfilesID, label = "Download All Files")

  )
}

reg_inputConfigurations <- function(){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "Configuration Settings",
    width = 4,
    hr(),
    div(
      id = "configurations",
      
      h4("Parallel computing, number of cores to use", align = "center"),
      textInput(
        inputId = "reg_cores",
        label = "Cores",
        value = "2",
        placeholder = "2"
      ),

      h4("Random State", align = "center"),
      textInput(
        inputId = "reg_random_state",
        label = "Random State",
        value = "999",
        placeholder = "999"
      ),

      h4("Settings for data pre-processing", align = "center"),
      selectInput(
        inputId = "reg_log2_trans",
        label = "Transform data to log2",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),

      hr(),
      h4("Settings for all-connection clustering analysis", align = "center"),
      h5("Hierarchical clustering", align = "center"),
      textInput(
        inputId = "reg_htmap_textsize_col",
        label = "Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "reg_htmap_textangle_col",
        label = "Heatmap Text Angle Column",
        value = "90",
        placeholder = "90"
      ),
      selectInput(
        inputId = "reg_htmap_lab_row",
        choices = c("TRUE", "FALSE"),
        label = "Heatmap Label Row",
        selected = "TRUE"
      ),
      textInput(
        inputId = "reg_htmap_textsize_row",
        label = "Heatmap Text Size Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "reg_htmap_keysize",
        label = "Heatmap Keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "reg_htmap_key_xlab",
        label = "Heatmap Key X-Label",
        value = "Normalized Connectivity",
        placeholder = "Normalized Connectivity"
      ),
      textInput(
        inputId = "reg_htmap_key_ylab",
        label = "Heatmap Key Y-label",
        value = "Pair Count",
        placeholder = "Pair Count"
      ),
      textInput(
        inputId = "reg_htmap_margin",
        label = "Heatmap Margin",
        value = "(6, 3)",
        placeholder = "(6, 3)"
      ),
      textInput(
        inputId = "reg_htmap_width",
        label = "Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "reg_htmap_height",
        label = "Heatmap Height",
        value = "10",
        placeholder = "10"
      ),


      hr(),
      h5("Principal Component Analysis (PCA)", align = "center"),
      selectInput(
        inputId = "reg_pca_scale_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Scale Data",
        selected = "TRUE"
      ),
      selectInput(
        inputId = "reg_pca_center_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Center Data",
        selected = "TRUE"
      ),
      textInput(
        inputId = "reg_pca_pc",
        label = "PCA Princpal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      selectInput(
        inputId = "reg_pca_biplot_samplelabel_type",
        label = "PCA Biplot Show Sample Labels on graph",
        choices = c("none", "direct", "indirect"),
        selected = "none"
      ),
      textInput(
        inputId = "reg_pca_biplot_samplelabel_size",
        label = "PCA Biplot Sample Label Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "reg_pca_biplot_symbol_size",
        label = "PCA Biplot Sample Symbol Size",
        value = "5",
        placeholder = "5"
      ),
      selectInput(
        inputId = "reg_pca_biplot_ellipse",
        label = "PCA Biplot Draw Ellipses",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "reg_pca_biplot_ellipse_conf",
        label = "PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      selectInput(
        inputId = "reg_pca_biplot_loading",
        label = "PCA Biplot Loading Plot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "reg_pca_biplot_loading_textsize",
        label = "PCA Biplot Loading Textsize",
        value = "3",
        placeholder = "3"
      ),
      selectInput(
        inputId = "reg_pca_biplot_multi_density",
        label = "PCA Biplot Display Density Plot on Diagonal Correlation Scoreplot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "reg_pca_biplot_multi_striplabel_size",
        label = "PCA Biplot Label Font Size for Correlation Scoreplot",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "reg_pca_rightside_y",
        label = "PCA Show Right Side y-axis for Boxplot and Biplot",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "reg_pca_x_tick_label_size",
        label = "PCA X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_pca_y_tick_label_size",
        label = "PCA Y-tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_pca_width",
        label = "PCA Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "reg_pca_height",
        label = "PCA Height",
        value = "150",
        placeholder = "150"
      ),

      hr(),
      h4("Settings for univariate analysis", align = "center"),
      h5("Univariate Analysis", align = "center"),
      selectInput(
        inputId = "reg_uni_fdr",
        label = "Univariate False Discovery Rate p value thresholding",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "reg_uni_alpha",
        label = "Univariate Alpha Value",
        value = "0.05",
        placeholder = "0.05"
      ),
      textInput(
        inputId = "reg_uni_fold_change",
        label = "Univariate Threshold for Fold Change",
        value = "1",
        placeholder = "1"
      ),
      textInput(
        inputId = "reg_volcano_n_top_connections",
        label = "Volcano Number of Top Connections",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_volcano_symbol_size",
        label = "Volcano Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "reg_volcano_sig_colour",
        label = "Volcano Colour of Significant Values",
        value = "red",
        placeholder = "red"
      ),
      textInput(
        inputId = "reg_volcano_nonsig_colour",
        label = "Volcano Colour of Non-Significant Values",
        value = "gray",
        placeholder = "gray"
      ),
      textInput(
        inputId = "reg_volcano_x_text_size",
        label = "Volcano X-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_volcano_y_text_size",
        label = "Volcano Y-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_volcano_width",
        label = "Volcano Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "reg_volcano_height",
        label = "Volcano Height",
        value = "150",
        placeholder = "150"
      ),

      hr(),
      h5("Hierarchical clustering: Sig Connections", align = "center"),
      textInput(
        inputId = "reg_sig_htmap_textsize_col",
        label = "Sig Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "reg_sig_htmap_textangle_col",
        label = "Sig Heatmap Textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "reg_sig_htmap_textsize_row",
        label = "Sig Heatmap Text Size Row",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "reg_sig_htmap_keysize",
        label = "Sig Heatmap Key Size",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "reg_sig_htmap_key_xlab",
        label = "Sig Heatmap Key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "reg_sig_htmap_key_ylab",
        label = "Sig Heatmap Key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "reg_sig_htmap_margin",
        label = "Sig heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "reg_sig_htmap_width",
        label = "Sig Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "reg_sig_htmap_height",
        label = "Sig Heatmap Width",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_sig_pca_pc",
        label = "Sig PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "reg_sig_pca_biplot_ellipse_conf",
        label = "Sig PCA Biplot Ellipse Confidence",
        value = "0.9",
        placeholder = "0.9"
      ),

      hr(),
      h4("Settings for Support Vector Machines (SVM) machine learning analysis", align = "center"),
      h5("General Settings", align = "center"),
      selectInput(
        inputId = "reg_cpu_cluster",
        label = "CPU Cluster for Parallel Computing",
        choices = c("FORK", "PSOCK"),
        selected = "PSOCK"
      ),

      hr(),
      h5("Data Processing", align = "center"),
      textInput(
        inputId = "reg_training_percentage",
        label = "Training Percentage",
        value = "0.85",
        placeholder = "0.85"
      ),

      hr(),
      h5("SVM internal nested cross-validation (CV) and feature selection"),
      selectInput(
        inputId = "reg_svm_cv_center_scale",
        label = "SVM CV Center Scale the Data",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      selectInput(
        inputId = "reg_svm_cv_kernel",
        label = "SVM CV Kernel",
        choices = c("linear", "polynomial", "radial", "sigmoid"),
        selected = "radial"
      ),
      textInput(
        inputId = "reg_svm_cv_cross_k",
        label = "SVM CV fold of cross validation",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "reg_svm_cv_tune_method",
        label = "SVM CV Parameter Tuning Method",
        choices = c("cross", "boot", "fix"),
        selected = "cross"
      ),
      textInput(
        inputId = "reg_svm_cv_tune_cross_k",
        label = "SVM CV fold number for CV.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_cv_tune_boot_n",
        label = "SVM CV Bootstrat iterations.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_cv_fs_rf_ifs_ntree",
        label = "SVM CV initial feature selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      textInput(
        inputId = "reg_svm_cv_fs_rf_sfs_ntree",
        label = "SVM CV sequation forward selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      selectInput(
        inputId = "reg_svm_cv_best_model_method",
        label = "SVM CV method to select best CV mode for feature selection",
        choices = c("none", "median"),
        selected = "none"
      ),
      textInput(
        inputId = "reg_svm_cv_fs_count_cutoff",
        label = "SVM CV Feature Vote Cutoff",
        value = "2",
        placeholder = "2"
      ),

      hr(),
      h5("SVM modelling", align = "center"),
      textInput(
        inputId = "reg_svm_cross_k",
        label = "SVM Fold of Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_tune_cross_k",
        label = "SVM Fold Number for Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_tune_boot_n",
        label = "SVM Bootstrap Iterations",
        value = "10",
        placeholder = "10"
      ),

      hr(),
      h5("SVM permutation test", align = "center"),
      selectInput(
        inputId = "reg_svm_perm_method",
        label = "SVM Permutation Method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "reg_svm_perm_n",
        label = "SVM Number of permutations.",
        value = "99",
        placeholder = "99"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_symbol_size",
        label = "SVM Permutation Plot Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_legend_size",
        label = "SVM Permutation Plot Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_x_label_size",
        label = "SVM Permutation Plot X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_x_tick_label_size",
        label = "SVM Permutation Plot X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_y_label_size",
        label = "SVM Permutation Plot Y-Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_y_tick_label_size",
        label = "SVM Permutation Plot Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_width",
        label = "SVM Permutation Plot Width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "reg_svm_perm_plot_height",
        label = "SVM Permutation Plot Height",
        value = "150",
        placeholder = "150"
      ),

      hr(),
      h5("SVM Receiver Operating Curve (ROC) Area Under The Curve (AUC)", align = "center"),
      textInput(
        inputId = "reg_svm_roc_threshold",
        label = "SVM ROC Threshold",
        value = "30",
        placeholder = "30"
      ),
      selectInput(
        inputId = "reg_svm_roc_smooth",
        label = "SVM ROC Smooth Curve",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "reg_svm_roc_symbol_size",
        label = "SVM ROC Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "reg_svm_roc_legend_size",
        label = "SVM ROC Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "reg_svm_roc_x_label_size",
        label = "SVM ROC X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_roc_x_tick_label_size",
        label = "SVM ROC X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_roc_y_label_size",
        label = "SVM ROC Y-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_roc_y_tick_label_size",
        label = "SVM ROC Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_svm_roc_width",
        label = "SVM ROC Plot Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "reg_svm_roc_height",
        label = "SVM ROC Plot Height",
        value = "150",
        placeholder = "150"
      ),

      hr(),
      h5("SVM Recurrent Random Forrest Feature Selection (RFFS) PCA", align = "center"),
      textInput(
        inputId = "reg_svm_rffs_pca_pc",
        label = "SVM RFFS PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "reg_svm_rffs_pca_biplot_ellipse_conf",
        label = "SVM RFFS PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),

      hr(),
      h5("RFFS hierarchical"),
      textInput(
        inputId = "reg_rffs_htmap_textsize_col",
        label = "RFFS hetmap textsize Column",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "reg_rffs_htmap_textangle_col",
        label = "RFFS heatmap textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "reg_rffs_htmap_textsize_row",
        label = "RFFS heatmap textsize Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "reg_rffs_htmap_keysize",
        label = "RFFS heatmap keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "reg_rffs_htmap_key_xlab",
        label = "RFFS heatmap key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "reg_rffs_htmap_key_ylab",
        label = "RFFS heatmap key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "reg_rffs_htmap_margin",
        label = "RFFS heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "reg_rffs_htmap_width",
        label = "RFFS heatmap width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "reg_rffs_htmap_height",
        label = "RFFS heatmap height",
        value = "15",
        placeholder = "15"
      ),

      hr(),
      h4("Settings for Partial Least Squares-Discriminant Analysis (PLS-DA) Modelling for evaluating SVM results", align = "center"),
      h5("Global PLS-DA Settings", align = "center"),
      selectInput(
        inputId = "reg_plsda_validation",
        label = "PLS-DA validation type (none, Cross-validation, Leave-One-Out)",
        choices = c("none", "CV", "LOO"),
        selected = "CV"
      ),
      textInput(
        inputId = "reg_plsda_validation_segment",
        label = "PLS-DA Number of validation segments",
        value = "10",
        placeholder = "10"
      ),

      hr(),
      h5("Initial PLS-DA Modelling Settings", align = "center"),
      textInput(
        inputId = "reg_plsda_init_ncomp",
        label = "PLS-DA Initial number of components",
        value = "150",
        placeholder = "150"
      ),

      hr(),
      h5("PLS-DA ncomp selection", align = "center"),
      selectInput(
        inputId = "reg_plsda_ncomp_select_method",
        label = "PLS-DA number of components select method (minimum RMSE, 1 standard error, randomization)",
        choices = c("min", "1err", "randomization"),
        selected = "1err"
      ),
      textInput(
        inputId = "reg_plsda_ncomp_select_plot_symbol_size",
        label = "PLS-DA number of components selected plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "reg_plsda_ncomp_select_plot_legend_size",
        label = "PLS-DA number of componets selected plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "reg_plsda_ncomp_select_plot_x_label_size",
        label = "PLS-DA number of components selected plot X-label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_ncomp_select_plot_x_tick_label_size",
        label = "PLS-DA number of components selected plot X-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_ncomp_select_plot_y_label_size",
        label = "PLS-DA number of components selected plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_ncomp_select_plot_y_tick_label_size",
        label = "PLS-DA number of components selected plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),

      hr(),
      h5("PLS-DA permutation test", align = "center"),
      selectInput(
        inputId = "reg_plsda_perm_method",
        label = "PLS-DA permutation method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "reg_plsda_perm_n",
        label = "PLS-DA number of permutations",
        value = "999",
        placeholder = "999"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_symbol_size",
        label = "PLS-DA permutation plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_legend_size",
        label = "PLS-DA permutation plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_x_label_size",
        label = "PLS-DA permutation plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_x_tick_label_size",
        label = "PLS-DA permutation plot X-Tick label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_y_label_size",
        label = "PLS-DA permutation plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_y_tick_label_size",
        label = "PLS-DA permutation plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_width",
        label = "PLS-DA permutation plot width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "reg_plsda_perm_plot_height",
        label = "PLS-DA permutation plot height",
        value = "150",
        placeholder = "150"
      ),

      hr(),
      h5("PLS-DA score plot", align = "center"),
      textInput(
        inputId = "reg_plsda_scoreplot_ellipse_conf",
        label = "PLS-DA Score plot ellipse confidence",
        value = "0.95",
        placeholder = "0.95"
      ),

      hr(),
      h5("PLS-DA ROC-AUC", align = "center"),
      selectInput(
        inputId = "reg_plsda_roc_smooth",
        label = "PLS-DA ROC Curve Smooth",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),

      hr(),
      h5("PLS-DA Variable Importance in Projection (VIP) Analysis", align = "center"),
      textInput(
        inputId = "reg_plsda_vip_alpha",
        label = "PLS-DA VIP alpha value",
        value = "0.8",
        placeholder = "0.8"
      ),
      selectInput(
        inputId = "reg_plsda_vip_boot",
        label = "PLS-DA VIP bootstrap method",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "reg_plsda_vip_boot_n",
        label = "PLS-DA VIP bootstrap number of iterations",
        value = "50",
        placeholder = "50"
      ),
      selectInput(
        inputId = "reg_plsda_vip_plot_errorbar",
        label = "PLS-DA VIP plot type of errorbar (SEM = Standard Error of the mean; SD = Standard Deviation)",
        choices = c("SEM", "SD"),
        selected = "SEM"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_errorbar_width",
        label = "PLS-DA VIP plot errorbar width",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_errorbar_label_size",
        label = "PLS-DA VIP plot errorbar label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_x_textangle",
        label = "PLS-DA VIP plot X-Textangle",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_x_label_size",
        label = "PLS-DA VIP plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_x_tick_label_size",
        label = "PLS-DA VIP plot X-Tick label size",
        value = "4",
        placeholder = "4"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_y_label_size",
        label = "PLS-DA VIP plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_y_tick_label_size",
        label = "PLS-DA VIP plot Y-Tick label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_width",
        label = "PLS-DA VIP plot width",
        value = "150",
        placeholder = "150"
      ),
      textInput(
        inputId = "reg_plsda_vip_plot_height",
        label = "PLS-DA VIP plot height",
        value = "100",
        placeholder = "100"
      ),
      tags$head(
        tags$style("#configurations{overflow-y:scroll; max-height: 382px;}")
      )
    ),

    # tooltips to help with use
    bsTooltip(
      id = "htmap_margin",
      "Format should be: (x, y)",
      placement = "top",
      trigger = "click",
      options = list(container = "body")
    ),

    hr(),

    downloadButton(outputId = "reg_download_configs", label = "Download Configurations"),
    actionButton("reg_reset_cnfg", "Reset All")
  )
}

uploadFilesUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        title = "Upload Files",
        width = 6,
        fileInput(
          inputId = ns("data"),
          label = "Upload Data",
          multiple = FALSE,
          accept = c(".mat", ".csv")
        ),
        fileInput(
          inputId = ns("annotations"),
          label = "Upload Annotations",
          multiple = FALSE,
          accept = ".csv"
        ),
        fileInput(
          inputId = ns("node"),
          label = "Upload Node Data",
          multiple = FALSE,
          accept = ".csv"
        )
      )
    )
  )
}

## cv_only configs
cv_inputConfigurations <- function(){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    # collapsed = TRUE,
    title = "Configuration Settings",
    width = 4,
    hr(),
    div(
      id = "cv_configurations",
      
      h4("Parallel computing, number of cores to use", align = "center"),
      textInput(
        inputId = "cv_cores",
        label = "Cores",
        value = "2",
        placeholder = "2"
      ),
      
      h4("Random State", align = "center"),
      textInput(
        inputId = "cv_random_state",
        label = "Random State",
        value = "999",
        placeholder = "999"
      ),
      
      h4("Settings for data pre-processing", align = "center"),
      selectInput(
        inputId = "cv_log2_trans",
        label = "Transform data to log2",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      
      hr(),
      h4("Settings for all-connection clustering analysis", align = "center"),
      h5("Hierarchical clustering", align = "center"),
      textInput(
        inputId = "cv_htmap_textsize_col",
        label = "Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "cv_htmap_textangle_col",
        label = "Heatmap Text Angle Column",
        value = "90",
        placeholder = "90"
      ),
      selectInput(
        inputId = "cv_htmap_lab_row",
        choices = c("TRUE", "FALSE"),
        label = "Heatmap Label Row",
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_htmap_textsize_row",
        label = "Heatmap Text Size Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "cv_htmap_keysize",
        label = "Heatmap Keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "cv_htmap_key_xlab",
        label = "Heatmap Key X-Label",
        value = "Normalized Connectivity",
        placeholder = "Normalized Connectivity"
      ),
      textInput(
        inputId = "cv_htmap_key_ylab",
        label = "Heatmap Key Y-label",
        value = "Pair Count",
        placeholder = "Pair Count"
      ),
      textInput(
        inputId = "cv_htmap_margin",
        label = "Heatmap Margin",
        value = "(6, 3)",
        placeholder = "(6, 3)"
      ),
      textInput(
        inputId = "cv_htmap_width",
        label = "Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "cv_htmap_height",
        label = "Heatmap Height",
        value = "10",
        placeholder = "10"
      ),
      
      
      hr(),
      h5("Principal Component Analysis (PCA)", align = "center"),
      selectInput(
        inputId = "cv_pca_scale_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Scale Data",
        selected = "TRUE"
      ),
      selectInput(
        inputId = "cv_pca_center_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Center Data",
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_pca_pc",
        label = "PCA Princpal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      selectInput(
        inputId = "cv_pca_biplot_samplelabel_type",
        label = "PCA Biplot Show Sample Labels on graph",
        choices = c("none", "direct", "indirect"),
        selected = "none"
      ),
      textInput(
        inputId = "cv_pca_biplot_samplelabel_size",
        label = "PCA Biplot Sample Label Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_pca_biplot_symbol_size",
        label = "PCA Biplot Sample Symbol Size",
        value = "5",
        placeholder = "5"
      ),
      selectInput(
        inputId = "cv_pca_biplot_ellipse",
        label = "PCA Biplot Draw Ellipses",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_pca_biplot_ellipse_conf",
        label = "PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      selectInput(
        inputId = "cv_pca_biplot_loading",
        label = "PCA Biplot Loading Plot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_pca_biplot_loading_textsize",
        label = "PCA Biplot Loading Textsize",
        value = "3",
        placeholder = "3"
      ),
      selectInput(
        inputId = "cv_pca_biplot_multi_density",
        label = "PCA Biplot Display Density Plot on Diagonal Correlation Scoreplot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_pca_biplot_multi_striplabel_size",
        label = "PCA Biplot Label Font Size for Correlation Scoreplot",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "cv_pca_rightside_y",
        label = "PCA Show Right Side y-axis for Boxplot and Biplot",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_pca_x_tick_label_size",
        label = "PCA X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_pca_y_tick_label_size",
        label = "PCA Y-tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_pca_width",
        label = "PCA Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "cv_pca_height",
        label = "PCA Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h4("Settings for univariate analysis", align = "center"),
      h5("Univariate Analysis", align = "center"),
      selectInput(
        inputId = "cv_uni_fdr",
        label = "Univariate False Discovery Rate p value thresholding",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_uni_alpha",
        label = "Univariate Alpha Value",
        value = "0.05",
        placeholder = "0.05"
      ),
      textInput(
        inputId = "cv_uni_fold_change",
        label = "Univariate Threshold for Fold Change",
        value = "1",
        placeholder = "1"
      ),
      textInput(
        inputId = "cv_volcano_n_top_connections",
        label = "Volcano Number of Top Connections",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_volcano_symbol_size",
        label = "Volcano Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_volcano_sig_colour",
        label = "Volcano Colour of Significant Values",
        value = "red",
        placeholder = "red"
      ),
      textInput(
        inputId = "cv_volcano_nonsig_colour",
        label = "Volcano Colour of Non-Significant Values",
        value = "gray",
        placeholder = "gray"
      ),
      textInput(
        inputId = "cv_volcano_x_text_size",
        label = "Volcano X-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_volcano_y_text_size",
        label = "Volcano Y-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_volcano_width",
        label = "Volcano Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "cv_volcano_height",
        label = "Volcano Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("Hierarchical clustering: Sig Connections", align = "center"),
      textInput(
        inputId = "cv_sig_htmap_textsize_col",
        label = "Sig Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "cv_sig_htmap_textangle_col",
        label = "Sig Heatmap Textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "cv_sig_htmap_textsize_row",
        label = "Sig Heatmap Text Size Row",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "cv_sig_htmap_keysize",
        label = "Sig Heatmap Key Size",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "cv_sig_htmap_key_xlab",
        label = "Sig Heatmap Key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "cv_sig_htmap_key_ylab",
        label = "Sig Heatmap Key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "cv_sig_htmap_margin",
        label = "Sig heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "cv_sig_htmap_width",
        label = "Sig Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "cv_sig_htmap_height",
        label = "Sig Heatmap Width",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_sig_pca_pc",
        label = "Sig PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "cv_sig_pca_biplot_ellipse_conf",
        label = "Sig PCA Biplot Ellipse Confidence",
        value = "0.9",
        placeholder = "0.9"
      ),
      
      hr(),
      h4("Settings for Support Vector Machines (SVM) machine learning analysis", align = "center"),
      h5("General Settings", align = "center"),
      selectInput(
        inputId = "cv_cpu_cluster",
        label = "CPU Cluster for Parallel Computing",
        choices = c("FORK", "PSOCK"),
        selected = "PSOCK"
      ),
      
      hr(),
      h5("Data Processing", align = "center"),
      textInput(
        inputId = "cv_training_percentage",
        label = "Training Percentage",
        value = "0.85",
        placeholder = "0.85"
      ),
      
      hr(),
      h5("SVM internal nested cross-validation (CV) and feature selection"),
      selectInput(
        inputId = "cv_svm_cv_center_scale",
        label = "SVM CV Center Scale the Data",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      selectInput(
        inputId = "cv_svm_cv_kernel",
        label = "SVM CV Kernel",
        choices = c("linear", "polynomial", "radial", "sigmoid"),
        selected = "radial"
      ),
      textInput(
        inputId = "cv_svm_cv_cross_k",
        label = "SVM CV fold of cross validation",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "cv_svm_cv_tune_method",
        label = "SVM CV Parameter Tuning Method",
        choices = c("cross", "boot", "fix"),
        selected = "cross"
      ),
      textInput(
        inputId = "cv_svm_cv_tune_cross_k",
        label = "SVM CV fold number for CV.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_cv_tune_boot_n",
        label = "SVM CV Bootstrat iterations.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_cv_fs_rf_ifs_ntree",
        label = "SVM CV initial feature selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      textInput(
        inputId = "cv_svm_cv_fs_rf_sfs_ntree",
        label = "SVM CV sequation forward selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      selectInput(
        inputId = "cv_svm_cv_best_model_method",
        label = "SVM CV method to select best CV mode for feature selection",
        choices = c("none", "median"),
        selected = "none"
      ),
      textInput(
        inputId = "cv_svm_cv_fs_count_cutoff",
        label = "SVM CV Feature Vote Cutoff",
        value = "2",
        placeholder = "2"
      ),
      
      hr(),
      h5("SVM modelling", align = "center"),
      textInput(
        inputId = "cv_svm_cross_k",
        label = "SVM Fold of Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_tune_cross_k",
        label = "SVM Fold Number for Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_tune_boot_n",
        label = "SVM Bootstrap Iterations",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("SVM permutation test", align = "center"),
      selectInput(
        inputId = "cv_svm_perm_method",
        label = "SVM Permutation Method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "cv_svm_perm_n",
        label = "SVM Number of permutations.",
        value = "99",
        placeholder = "99"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_symbol_size",
        label = "SVM Permutation Plot Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_legend_size",
        label = "SVM Permutation Plot Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_x_label_size",
        label = "SVM Permutation Plot X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_x_tick_label_size",
        label = "SVM Permutation Plot X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_y_label_size",
        label = "SVM Permutation Plot Y-Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_y_tick_label_size",
        label = "SVM Permutation Plot Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_width",
        label = "SVM Permutation Plot Width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "cv_svm_perm_plot_height",
        label = "SVM Permutation Plot Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("SVM Receiver Operating Curve (ROC) Area Under The Curve (AUC)", align = "center"),
      selectInput(
        inputId = "cv_svm_roc_smooth",
        label = "SVM ROC Smooth Curve",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_svm_roc_symbol_size",
        label = "SVM ROC Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_svm_roc_legend_size",
        label = "SVM ROC Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_svm_roc_x_label_size",
        label = "SVM ROC X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_roc_x_tick_label_size",
        label = "SVM ROC X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_roc_y_label_size",
        label = "SVM ROC Y-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_roc_y_tick_label_size",
        label = "SVM ROC Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_svm_roc_width",
        label = "SVM ROC Plot Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "cv_svm_roc_height",
        label = "SVM ROC Plot Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("SVM Recurrent Random Forrest Feature Selection (RFFS) PCA", align = "center"),
      textInput(
        inputId = "cv_svm_rffs_pca_pc",
        label = "SVM RFFS PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "cv_svm_rffs_pca_biplot_ellipse_conf",
        label = "SVM RFFS PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      
      hr(),
      h5("RFFS hierarchical"),
      textInput(
        inputId = "cv_rffs_htmap_textsize_col",
        label = "RFFS hetmap textsize Column",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "cv_rffs_htmap_textangle_col",
        label = "RFFS heatmap textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "cv_rffs_htmap_textsize_row",
        label = "RFFS heatmap textsize Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "cv_rffs_htmap_keysize",
        label = "RFFS heatmap keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "cv_rffs_htmap_key_xlab",
        label = "RFFS heatmap key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "cv_rffs_htmap_key_ylab",
        label = "RFFS heatmap key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "cv_rffs_htmap_margin",
        label = "RFFS heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "cv_rffs_htmap_width",
        label = "RFFS heatmap width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "cv_rffs_htmap_height",
        label = "RFFS heatmap height",
        value = "15",
        placeholder = "15"
      ),
      
      hr(),
      h4("Settings for Partial Least Squares-Discriminant Analysis (PLS-DA) Modelling for evaluating SVM results", align = "center"),
      h5("Global PLS-DA Settings", align = "center"),
      selectInput(
        inputId = "cv_plsda_validation",
        label = "PLS-DA validation type (none, Cross-validation, Leave-One-Out)",
        choices = c("none", "CV", "LOO"),
        selected = "CV"
      ),
      textInput(
        inputId = "cv_plsda_validation_segment",
        label = "PLS-DA Number of validation segments",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("Initial PLS-DA Modelling Settings", align = "center"),
      textInput(
        inputId = "cv_plsda_init_ncomp",
        label = "PLS-DA Initial number of components",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("PLS-DA ncomp selection", align = "center"),
      selectInput(
        inputId = "cv_plsda_ncomp_select_method",
        label = "PLS-DA number of components select method (minimum RMSE, 1 standard error, randomization)",
        choices = c("min", "1err", "randomization"),
        selected = "1err"
      ),
      textInput(
        inputId = "plsda_ncomp_select_plot_symbol_size",
        label = "PLS-DA number of components selected plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_plsda_ncomp_select_plot_legend_size",
        label = "PLS-DA number of componets selected plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_plsda_ncomp_select_plot_x_label_size",
        label = "PLS-DA number of components selected plot X-label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_ncomp_select_plot_x_tick_label_size",
        label = "PLS-DA number of components selected plot X-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_ncomp_select_plot_y_label_size",
        label = "PLS-DA number of components selected plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_ncomp_select_plot_y_tick_label_size",
        label = "PLS-DA number of components selected plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("PLS-DA permutation test", align = "center"),
      selectInput(
        inputId = "cv_plsda_perm_method",
        label = "PLS-DA permutation method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "cv_plsda_perm_n",
        label = "PLS-DA number of permutations",
        value = "999",
        placeholder = "999"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_symbol_size",
        label = "PLS-DA permutation plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_legend_size",
        label = "PLS-DA permutation plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_x_label_size",
        label = "PLS-DA permutation plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_x_tick_label_size",
        label = "PLS-DA permutation plot X-Tick label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_y_label_size",
        label = "PLS-DA permutation plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_y_tick_label_size",
        label = "PLS-DA permutation plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_width",
        label = "PLS-DA permutation plot width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "cv_plsda_perm_plot_height",
        label = "PLS-DA permutation plot height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("PLS-DA score plot", align = "center"),
      textInput(
        inputId = "cv_plsda_scoreplot_ellipse_conf",
        label = "PLS-DA Score plot ellipse confidence",
        value = "0.95",
        placeholder = "0.95"
      ),
      
      hr(),
      h5("PLS-DA ROC-AUC", align = "center"),
      selectInput(
        inputId = "cv_plsda_roc_smooth",
        label = "PLS-DA ROC Curve Smooth",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      
      hr(),
      h5("PLS-DA Variable Importance in Projection (VIP) Analysis", align = "center"),
      textInput(
        inputId = "cv_plsda_vip_alpha",
        label = "PLS-DA VIP alpha value",
        value = "0.8",
        placeholder = "0.8"
      ),
      selectInput(
        inputId = "cv_plsda_vip_boot",
        label = "PLS-DA VIP bootstrap method",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_plsda_vip_boot_n",
        label = "PLS-DA VIP bootstrap number of iterations",
        value = "50",
        placeholder = "50"
      ),
      selectInput(
        inputId = "cv_plsda_vip_plot_errorbar",
        label = "PLS-DA VIP plot type of errorbar (SEM = Standard Error of the mean; SD = Standard Deviation)",
        choices = c("SEM", "SD"),
        selected = "SEM"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_errorbar_width",
        label = "PLS-DA VIP plot errorbar width",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_errorbar_label_size",
        label = "PLS-DA VIP plot errorbar label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_x_textangle",
        label = "PLS-DA VIP plot X-Textangle",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_x_label_size",
        label = "PLS-DA VIP plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_x_tick_label_size",
        label = "PLS-DA VIP plot X-Tick label size",
        value = "4",
        placeholder = "4"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_y_label_size",
        label = "PLS-DA VIP plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_y_tick_label_size",
        label = "PLS-DA VIP plot Y-Tick label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_width",
        label = "PLS-DA VIP plot width",
        value = "150",
        placeholder = "150"
      ),
      textInput(
        inputId = "cv_plsda_vip_plot_height",
        label = "PLS-DA VIP plot height",
        value = "100",
        placeholder = "100"
      ),
      tags$head(
        tags$style("#cv_configurations{overflow-y:scroll; max-height: 382px;}")
      )
    ),
    
    # tooltips to help with use
    bsTooltip(
      id = "cv_htmap_margin",
      "Format should be: (x, y)",
      placement = "top",
      trigger = "click",
      options = list(container = "body")
    ),
    
    hr(),
    fileInput(inputId = "cv_upload_configs", label = "Upload Configurations"),
    downloadButton(outputId = "cv_download_configs", label = "Download Configurations"),
    actionButton("cv_reset_cnfg", "Reset All")
  )
}

cv_reg_inputConfigurations <- function(){
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "Configuration Settings",
    width = 4,
    hr(),
    div(
      id = "cv_reg_configurations",
      
      h4("Parallel computing, number of cores to use", align = "center"),
      textInput(
        inputId = "cv_reg_cores",
        label = "Cores",
        value = "2",
        placeholder = "2"
      ),
      
      h4("Random State", align = "center"),
      textInput(
        inputId = "cv_reg_random_state",
        label = "Random State",
        value = "999",
        placeholder = "999"
      ),
      
      h4("Settings for data pre-processing", align = "center"),
      selectInput(
        inputId = "cv_reg_log2_trans",
        label = "Transform data to log2",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      
      hr(),
      h4("Settings for all-connection clustering analysis", align = "center"),
      h5("Hierarchical clustering", align = "center"),
      textInput(
        inputId = "cv_reg_htmap_textsize_col",
        label = "Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "cv_reg_htmap_textangle_col",
        label = "Heatmap Text Angle Column",
        value = "90",
        placeholder = "90"
      ),
      selectInput(
        inputId = "cv_reg_htmap_lab_row",
        choices = c("TRUE", "FALSE"),
        label = "Heatmap Label Row",
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_reg_htmap_textsize_row",
        label = "Heatmap Text Size Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "cv_reg_htmap_keysize",
        label = "Heatmap Keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "cv_reg_htmap_key_xlab",
        label = "Heatmap Key X-Label",
        value = "Normalized Connectivity",
        placeholder = "Normalized Connectivity"
      ),
      textInput(
        inputId = "cv_reg_htmap_key_ylab",
        label = "Heatmap Key Y-label",
        value = "Pair Count",
        placeholder = "Pair Count"
      ),
      textInput(
        inputId = "cv_reg_htmap_margin",
        label = "Heatmap Margin",
        value = "(6, 3)",
        placeholder = "(6, 3)"
      ),
      textInput(
        inputId = "cv_reg_htmap_width",
        label = "Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "cv_reg_htmap_height",
        label = "Heatmap Height",
        value = "10",
        placeholder = "10"
      ),
      
      
      hr(),
      h5("Principal Component Analysis (PCA)", align = "center"),
      selectInput(
        inputId = "cv_reg_pca_scale_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Scale Data",
        selected = "TRUE"
      ),
      selectInput(
        inputId = "cv_reg_pca_center_data",
        choices = c("TRUE", "FALSE"),
        label = "PCA Center Data",
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_reg_pca_pc",
        label = "PCA Princpal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      selectInput(
        inputId = "cv_reg_pca_biplot_samplelabel_type",
        label = "PCA Biplot Show Sample Labels on graph",
        choices = c("none", "direct", "indirect"),
        selected = "none"
      ),
      textInput(
        inputId = "cv_reg_pca_biplot_samplelabel_size",
        label = "PCA Biplot Sample Label Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_reg_pca_biplot_symbol_size",
        label = "PCA Biplot Sample Symbol Size",
        value = "5",
        placeholder = "5"
      ),
      selectInput(
        inputId = "cv_reg_pca_biplot_ellipse",
        label = "PCA Biplot Draw Ellipses",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_reg_pca_biplot_ellipse_conf",
        label = "PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      selectInput(
        inputId = "cv_reg_pca_biplot_loading",
        label = "PCA Biplot Loading Plot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_reg_pca_biplot_loading_textsize",
        label = "PCA Biplot Loading Textsize",
        value = "3",
        placeholder = "3"
      ),
      selectInput(
        inputId = "cv_reg_pca_biplot_multi_density",
        label = "PCA Biplot Display Density Plot on Diagonal Correlation Scoreplot",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_reg_pca_biplot_multi_striplabel_size",
        label = "PCA Biplot Label Font Size for Correlation Scoreplot",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "cv_reg_pca_rightside_y",
        label = "PCA Show Right Side y-axis for Boxplot and Biplot",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_reg_pca_x_tick_label_size",
        label = "PCA X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_pca_y_tick_label_size",
        label = "PCA Y-tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_pca_width",
        label = "PCA Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "cv_reg_pca_height",
        label = "PCA Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h4("Settings for univariate analysis", align = "center"),
      h5("Univariate Analysis", align = "center"),
      selectInput(
        inputId = "cv_reg_uni_fdr",
        label = "Univariate False Discovery Rate p value thresholding",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_reg_uni_alpha",
        label = "Univariate Alpha Value",
        value = "0.05",
        placeholder = "0.05"
      ),
      textInput(
        inputId = "cv_reg_uni_fold_change",
        label = "Univariate Threshold for Fold Change",
        value = "1",
        placeholder = "1"
      ),
      textInput(
        inputId = "cv_reg_volcano_n_top_connections",
        label = "Volcano Number of Top Connections",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_volcano_symbol_size",
        label = "Volcano Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_reg_volcano_sig_colour",
        label = "Volcano Colour of Significant Values",
        value = "red",
        placeholder = "red"
      ),
      textInput(
        inputId = "cv_reg_volcano_nonsig_colour",
        label = "Volcano Colour of Non-Significant Values",
        value = "gray",
        placeholder = "gray"
      ),
      textInput(
        inputId = "cv_reg_volcano_x_text_size",
        label = "Volcano X-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_volcano_y_text_size",
        label = "Volcano Y-axis Text Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_volcano_width",
        label = "Volcano Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "cv_reg_volcano_height",
        label = "Volcano Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("Hierarchical clustering: Sig Connections", align = "center"),
      textInput(
        inputId = "cv_reg_sig_htmap_textsize_col",
        label = "Sig Heatmap Text Size Column",
        value = "0.3",
        placeholder = "0.3"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_textangle_col",
        label = "Sig Heatmap Textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_textsize_row",
        label = "Sig Heatmap Text Size Row",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_keysize",
        label = "Sig Heatmap Key Size",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_key_xlab",
        label = "Sig Heatmap Key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_key_ylab",
        label = "Sig Heatmap Key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_margin",
        label = "Sig heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_width",
        label = "Sig Heatmap Width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "cv_reg_sig_htmap_height",
        label = "Sig Heatmap Width",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_sig_pca_pc",
        label = "Sig PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "cv_reg_sig_pca_biplot_ellipse_conf",
        label = "Sig PCA Biplot Ellipse Confidence",
        value = "0.9",
        placeholder = "0.9"
      ),
      
      hr(),
      h4("Settings for Support Vector Machines (SVM) machine learning analysis", align = "center"),
      h5("General Settings", align = "center"),
      selectInput(
        inputId = "cv_reg_cpu_cluster",
        label = "CPU Cluster for Parallel Computing",
        choices = c("FORK", "PSOCK"),
        selected = "PSOCK"
      ),
      
      hr(),
      h5("Data Processing", align = "center"),
      textInput(
        inputId = "cv_reg_training_percentage",
        label = "Training Percentage",
        value = "0.85",
        placeholder = "0.85"
      ),
      
      hr(),
      h5("SVM internal nested cross-validation (CV) and feature selection"),
      selectInput(
        inputId = "cv_reg_svm_cv_center_scale",
        label = "SVM CV Center Scale the Data",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      selectInput(
        inputId = "cv_reg_svm_cv_kernel",
        label = "SVM CV Kernel",
        choices = c("linear", "polynomial", "radial", "sigmoid"),
        selected = "radial"
      ),
      textInput(
        inputId = "cv_reg_svm_cv_cross_k",
        label = "SVM CV fold of cross validation",
        value = "10",
        placeholder = "10"
      ),
      selectInput(
        inputId = "cv_reg_svm_cv_tune_method",
        label = "SVM CV Parameter Tuning Method",
        choices = c("cross", "boot", "fix"),
        selected = "cross"
      ),
      textInput(
        inputId = "cv_reg_svm_cv_tune_cross_k",
        label = "SVM CV fold number for CV.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_cv_tune_boot_n",
        label = "SVM CV Bootstrat iterations.",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_cv_fs_rf_ifs_ntree",
        label = "SVM CV initial feature selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      textInput(
        inputId = "cv_reg_svm_cv_fs_rf_sfs_ntree",
        label = "SVM CV sequation forward selection in random forest ntree settings",
        value = "501",
        placeholder = "501"
      ),
      selectInput(
        inputId = "cv_reg_svm_cv_best_model_method",
        label = "SVM CV method to select best CV mode for feature selection",
        choices = c("none", "median"),
        selected = "none"
      ),
      textInput(
        inputId = "cv_reg_svm_cv_fs_count_cutoff",
        label = "SVM CV Feature Vote Cutoff",
        value = "2",
        placeholder = "2"
      ),
      
      hr(),
      h5("SVM modelling", align = "center"),
      textInput(
        inputId = "cv_reg_svm_cross_k",
        label = "SVM Fold of Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_tune_cross_k",
        label = "SVM Fold Number for Cross Validation",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_tune_boot_n",
        label = "SVM Bootstrap Iterations",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("SVM permutation test", align = "center"),
      selectInput(
        inputId = "cv_reg_svm_perm_method",
        label = "SVM Permutation Method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_n",
        label = "SVM Number of permutations.",
        value = "99",
        placeholder = "99"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_symbol_size",
        label = "SVM Permutation Plot Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_legend_size",
        label = "SVM Permutation Plot Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_x_label_size",
        label = "SVM Permutation Plot X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_x_tick_label_size",
        label = "SVM Permutation Plot X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_y_label_size",
        label = "SVM Permutation Plot Y-Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_y_tick_label_size",
        label = "SVM Permutation Plot Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_width",
        label = "SVM Permutation Plot Width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "cv_reg_svm_perm_plot_height",
        label = "SVM Permutation Plot Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("SVM Receiver Operating Curve (ROC) Area Under The Curve (AUC)", align = "center"),
      textInput(
        inputId = "cv_reg_svm_roc_threshold",
        label = "SVM ROC Threshold",
        value = "30",
        placeholder = "30"
      ),
      selectInput(
        inputId = "cv_reg_svm_roc_smooth",
        label = "SVM ROC Smooth Curve",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_symbol_size",
        label = "SVM ROC Symbol Size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_legend_size",
        label = "SVM ROC Legend Size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_x_label_size",
        label = "SVM ROC X-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_x_tick_label_size",
        label = "SVM ROC X-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_y_label_size",
        label = "SVM ROC Y-label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_y_tick_label_size",
        label = "SVM ROC Y-Tick Label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_width",
        label = "SVM ROC Plot Width",
        value = "170",
        placeholder = "170"
      ),
      textInput(
        inputId = "cv_reg_svm_roc_height",
        label = "SVM ROC Plot Height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("SVM Recurrent Random Forrest Feature Selection (RFFS) PCA", align = "center"),
      textInput(
        inputId = "cv_reg_svm_rffs_pca_pc",
        label = "SVM RFFS PCA Principal Components",
        value = "(1, 2, 3)",
        placeholder = "(1, 2, 3)"
      ),
      textInput(
        inputId = "cv_reg_svm_rffs_pca_biplot_ellipse_conf",
        label = "SVM RFFS PCA Biplot Ellipse Confidence Value",
        value = "0.9",
        placeholder = "0.9"
      ),
      
      hr(),
      h5("RFFS hierarchical"),
      textInput(
        inputId = "cv_reg_rffs_htmap_textsize_col",
        label = "RFFS hetmap textsize Column",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_textangle_col",
        label = "RFFS heatmap textangle Column",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_textsize_row",
        label = "RFFS heatmap textsize Row",
        value = "0.7",
        placeholder = "0.7"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_keysize",
        label = "RFFS heatmap keysize",
        value = "1.5",
        placeholder = "1.5"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_key_xlab",
        label = "RFFS heatmap key X-label",
        value = "Z score",
        placeholder = "Z score"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_key_ylab",
        label = "RFFS heatmap key Y-label",
        value = "Count",
        placeholder = "Count"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_margin",
        label = "RFFS heatmap margin",
        value = "(6, 9)",
        placeholder = "(6, 9)"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_width",
        label = "RFFS heatmap width",
        value = "15",
        placeholder = "15"
      ),
      textInput(
        inputId = "cv_reg_rffs_htmap_height",
        label = "RFFS heatmap height",
        value = "15",
        placeholder = "15"
      ),
      
      hr(),
      h4("Settings for Partial Least Squares-Discriminant Analysis (PLS-DA) Modelling for evaluating SVM results", align = "center"),
      h5("Global PLS-DA Settings", align = "center"),
      selectInput(
        inputId = "cv_reg_plsda_validation",
        label = "PLS-DA validation type (none, Cross-validation, Leave-One-Out)",
        choices = c("none", "CV", "LOO"),
        selected = "CV"
      ),
      textInput(
        inputId = "cv_reg_plsda_validation_segment",
        label = "PLS-DA Number of validation segments",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("Initial PLS-DA Modelling Settings", align = "center"),
      textInput(
        inputId = "cv_reg_plsda_init_ncomp",
        label = "PLS-DA Initial number of components",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("PLS-DA ncomp selection", align = "center"),
      selectInput(
        inputId = "cv_reg_plsda_ncomp_select_method",
        label = "PLS-DA number of components select method (minimum RMSE, 1 standard error, randomization)",
        choices = c("min", "1err", "randomization"),
        selected = "1err"
      ),
      textInput(
        inputId = "cv_reg_plsda_ncomp_select_plot_symbol_size",
        label = "PLS-DA number of components selected plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_reg_plsda_ncomp_select_plot_legend_size",
        label = "PLS-DA number of componets selected plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_reg_plsda_ncomp_select_plot_x_label_size",
        label = "PLS-DA number of components selected plot X-label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_ncomp_select_plot_x_tick_label_size",
        label = "PLS-DA number of components selected plot X-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_ncomp_select_plot_y_label_size",
        label = "PLS-DA number of components selected plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_ncomp_select_plot_y_tick_label_size",
        label = "PLS-DA number of components selected plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      
      hr(),
      h5("PLS-DA permutation test", align = "center"),
      selectInput(
        inputId = "cv_reg_plsda_perm_method",
        label = "PLS-DA permutation method",
        choices = c("by_y", "by_feature_per_y"),
        selected = "by_y"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_n",
        label = "PLS-DA number of permutations",
        value = "999",
        placeholder = "999"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_symbol_size",
        label = "PLS-DA permutation plot symbol size",
        value = "2",
        placeholder = "2"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_legend_size",
        label = "PLS-DA permutation plot legend size",
        value = "9",
        placeholder = "9"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_x_label_size",
        label = "PLS-DA permutation plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_x_tick_label_size",
        label = "PLS-DA permutation plot X-Tick label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_y_label_size",
        label = "PLS-DA permutation plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_y_tick_label_size",
        label = "PLS-DA permutation plot Y-Tick label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_width",
        label = "PLS-DA permutation plot width",
        value = "250",
        placeholder = "250"
      ),
      textInput(
        inputId = "cv_reg_plsda_perm_plot_height",
        label = "PLS-DA permutation plot height",
        value = "150",
        placeholder = "150"
      ),
      
      hr(),
      h5("PLS-DA score plot", align = "center"),
      textInput(
        inputId = "cv_reg_plsda_scoreplot_ellipse_conf",
        label = "PLS-DA Score plot ellipse confidence",
        value = "0.95",
        placeholder = "0.95"
      ),
      
      hr(),
      h5("PLS-DA ROC-AUC", align = "center"),
      selectInput(
        inputId = "cv_reg_plsda_roc_smooth",
        label = "PLS-DA ROC Curve Smooth",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      ),
      
      hr(),
      h5("PLS-DA Variable Importance in Projection (VIP) Analysis", align = "center"),
      textInput(
        inputId = "cv_reg_plsda_vip_alpha",
        label = "PLS-DA VIP alpha value",
        value = "0.8",
        placeholder = "0.8"
      ),
      selectInput(
        inputId = "cv_reg_plsda_vip_boot",
        label = "PLS-DA VIP bootstrap method",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_boot_n",
        label = "PLS-DA VIP bootstrap number of iterations",
        value = "50",
        placeholder = "50"
      ),
      selectInput(
        inputId = "cv_reg_plsda_vip_plot_errorbar",
        label = "PLS-DA VIP plot type of errorbar (SEM = Standard Error of the mean; SD = Standard Deviation)",
        choices = c("SEM", "SD"),
        selected = "SEM"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_errorbar_width",
        label = "PLS-DA VIP plot errorbar width",
        value = "0.2",
        placeholder = "0.2"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_errorbar_label_size",
        label = "PLS-DA VIP plot errorbar label size",
        value = "6",
        placeholder = "6"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_x_textangle",
        label = "PLS-DA VIP plot X-Textangle",
        value = "90",
        placeholder = "90"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_x_label_size",
        label = "PLS-DA VIP plot X-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_x_tick_label_size",
        label = "PLS-DA VIP plot X-Tick label size",
        value = "4",
        placeholder = "4"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_y_label_size",
        label = "PLS-DA VIP plot Y-Label size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_y_tick_label_size",
        label = "PLS-DA VIP plot Y-Tick label Size",
        value = "10",
        placeholder = "10"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_width",
        label = "PLS-DA VIP plot width",
        value = "150",
        placeholder = "150"
      ),
      textInput(
        inputId = "cv_reg_plsda_vip_plot_height",
        label = "PLS-DA VIP plot height",
        value = "100",
        placeholder = "100"
      ),
      tags$head(
        tags$style("#cv_reg_configurations{overflow-y:scroll; max-height: 382px;}")
      )
    ),
    
    # tooltips to help with use
    bsTooltip(
      id = "cv_reg_htmap_margin",
      "Format should be: (x, y)",
      placement = "top",
      trigger = "click",
      options = list(container = "body")
    ),
    
    hr(),
    
    downloadButton(outputId = "cv_reg_download_configs", label = "Download Configurations"),
    actionButton("cv_reg_reset_cnfg", "Reset All")
  )
}


predictUI <- function(id) {
  ns <- NS(id)
  
  box(
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    title = "Predict classification with trained model",
    width = 12,
    fluidRow(
      useShinyjs(),
    column(width = 6,
      fileInput(
        inputId = ns("data"),
        label = "Upload Input data",
        multiple = FALSE,
        accept = '.mat'
      ),
      fileInput(
        inputId = ns("annotations"),
        label = "Upload Annotations",
        multiple = FALSE,
        accept = ".csv"
      ),
      fileInput(
        inputId = ns("model"),
        label = "Upload Model",
        multiple = FALSE,
        accept = '.Rdata'
      ),
      h4("Select the sample variable"),
      dataTableOutput(ns("varnames")),
      h4(""),
      disabled(actionButton(ns("predict"), "Predict")),
      fluidRow(
        column(
          width = 6,
          h4("Model Information"),
          tableOutput(ns("model"))
        ),
        column(
          width = 6,
          h4("Subject Information"),
          tableOutput(ns("subject"))
        )
      )
    ),
    
    column(
      width = 6,
      selectInput(
        inputId = ns("plotname"),
        label = "Select Plot To View",
        choices = NULL,
        width = '100%'
      ),
      imageOutput(ns("predictionplots"), height = "90%"),
      
      textOutput("blank text"),
      plotOutput(ns("brainConnectivity_top")),
      fluidRow(
        column(
          width = 12,
          radioButtons(ns("connectivity_view"), "Select View:", c("top", "bottom", "left", "right", "front", "back"))
        ),
        column(
          width = 6,
          downloadButton(ns("downloadPredictions"), "Download Prediction Files"),
          downloadButton(ns("downloadPredictionsReport"), "Generate Report")
        )
      )
      
    )
    )
    # column(
    #   width = 12,
    #   plotOutput(ns("brainConnectivity"))
    # )
    
  )
}