require(readr)
require(stringr)
require(shiny)

# list of all variable names
varNames <- function(){
  vars <- list(
    random_state = "random_state",
    log2_trans = "log2_trans",
    htmap_textsize_col = "htmap_textsize_col",
    htmap_textangle_col = "htmap_textangle_col",
    htmap_lab_row = "htmap_lab_row",
    htmap_textsize_row = "htmap_textsize_row",
    htmap_keysize = "htmap_keysize",
    htmap_key_xlab = "htmap_key_xlab",
    htmap_key_ylab = "htmap_key_ylab",
    htmap_margin = "htmap_margin",
    htmap_width = "htmap_width",
    htmap_height = "htmap_height",
    pca_scale_data = "pca_scale_data",
    pca_centre_data = "pca_centre_data",
    pca_pc = "pca_pc",
    pca_biplot_samplelabel_type = "pca_biplot_samplelabel_type",
    pca_biplot_samplelabel_size = "pca_biplot_samplelabel_size",
    pca_biplot_symbol_size = "pca_biplot_symbol_size",
    pca_biplot_ellipse = "pca_biplot_ellipse",
    pca_biplot_ellipse_conf = "pca_biplot_ellipse_conf",
    pca_biplot_loading = "pca_biplot_loading",
    pca_biplot_loading_textsize = "pca_biplot_loading_textsize",
    pca_biplot_multi_density = "pca_biplot_multi_density",
    pca_biplot_multi_striplabel_size = "pca_biplot_multi_striplabel_size",
    pca_rightside_y = "pca_rightside_y",
    pca_x_tick_label_size = "pca_x_tick_label_size",
    pca_y_tick_label_size = "pca_y_tick_label_size",
    pca_width = "pca_width",
    pca_height = "pca_height",
    uni_fdr = "uni_fdr",
    uni_alpha = "uni_alpha",
    uni_fold_change = "uni_fold_change",
    volcano_n_top_connections = "volcano_n_top_connections",
    volcano_symbol_size = "volcano_symbol_size",
    volcano_sig_colour = "volcano_sig_colour",
    volcano_nonsig_colour = "volcano_nonsig_colour",
    volcano_x_text_size = "volcano_x_text_size",
    volcano_y_text_size = "volcano_y_text_size",
    volcano_width = "volcano_width",
    volcano_height = "volcano_height",
    sig_htmap_textsize_col = "sig_htmap_textsize_col",
    sig_htmap_textangle_col = "sig_htmap_textangle_col",
    sig_htmap_textsize_row = "sig_htmap_textsize_row",
    sig_htmap_keysize = "sig_htmap_keysize",
    sig_htmap_key_xlab = "sig_htmap_key_xlab",
    sig_htmap_key_ylab = "sig_htmap_key_ylab",
    sig_htmap_margin = "sig_htmap_margin",
    sig_htmap_width = "sig_htmap_width",
    sig_htmap_height = "sig_htmap_height",
    sig_pca_pc = "sig_pca_pc",
    sig_pca_biplot_ellipse_conf = "sig_pca_biplot_ellipse_conf",
    cpu_cluster = "cpu_cluster",
    training_percentage = "training_percentage",
    svm_cv_centre_scale = "svm_cv_centre_scale",
    svm_cv_kernel = "svm_cv_kernel",
    svm_cv_cross_k = "svm_cv_cross_k",
    svm_cv_tune_method = "svm_cv_tune_method",
    svm_cv_tune_cross_k = "svm_cv_tune_cross_k",
    svm_cv_tune_boot_n = "svm_cv_tune_boot_n",
    svm_cv_fs_rf_ifs_ntree = "svm_cv_fs_rf_ifs_ntree",
    svm_cv_fs_rf_sfs_ntree = "svm_cv_fs_rf_sfs_ntree",
    svm_cv_best_model_method = "svm_cv_best_model_method",
    svm_cv_fs_count_cutoff = "svm_cv_fs_count_cutoff",
    svm_cross_k = "svm_cross_k",
    svm_tune_cross_k = "svm_tune_cross_k",
    svm_tune_boot_n = "svm_tune_boot_n",
    svm_perm_method = "svm_perm_method",
    svm_perm_n = "svm_perm_n",
    svm_perm_plot_symbol_size = "svm_perm_plot_symbol_size",
    svm_perm_plot_legend_size = "svm_perm_plot_legend_size",
    svm_perm_plot_x_label_size = "svm_perm_plot_x_label_size",
    svm_perm_plot_x_tick_label_size = "svm_perm_plot_x_tick_label_size",
    svm_perm_plot_y_label_size = "svm_perm_plot_y_label_size",
    svm_perm_plot_y_tick_label_size = "svm_perm_plot_y_tick_label_size",
    svm_perm_plot_width = "svm_perm_plot_width",
    svm_perm_plot_height = "svm_perm_plot_height",
    svm_roc_smooth = "svm_roc_smooth",
    svm_roc_symbol_size = "svm_roc_symbol_size",
    svm_roc_legend_size = "svm_roc_legend_size",
    svm_roc_x_label_size = "svm_roc_x_label_size",
    svm_roc_x_tick_label_size = "svm_roc_x_tick_label_size",
    svm_roc_y_label_size = "svm_roc_y_label_size",
    svm_roc_y_tick_label_size = "svm_roc_y_tick_label_size",
    svm_roc_width = "svm_roc_width",
    svm_roc_height = "svm_roc_height",
    svm_rffs_pca_pc = "svm_rffs_pca_pc",
    svm_rffs_pca_biplot_ellipse_conf = "svm_rffs_pca_biplot_ellipse_conf",
    rffs_htmap_textsize_col = "rffs_htmap_textsize_col",
    rffs_htmap_textangle_col = "rffs_htmap_textangle_col",
    rffs_htmap_textsize_row = "rffs_htmap_textsize_row",
    rffs_htmap_keysize = "rffs_htmap_keysize",
    rffs_htmap_key_xlab = "rffs_htmap_key_xlab",
    rffs_htmap_key_ylab = "rffs_htmap_key_ylab",
    rffs_htmap_margin = "rffs_htmap_margin",
    rffs_htmap_width = "rffs_htmap_width",
    rffs_htmap_height = "rffs_htmap_height",
    plsda_validation = "plsda_validation",
    plsda_validation_segment = "plsda_validation_segment",
    plsda_init_ncomp = "plsda_init_ncomp",
    plsda_ncomp_select_method = "plsda_ncomp_select_method",
    plsda_ncomp_select_plot_symbol_size = "plsda_ncomp_select_plot_symbol_size",
    plsda_ncomp_select_plot_legend_size = "plsda_ncomp_select_plot_legend_size",
    plsda_ncomp_select_plot_x_label_size = "plsda_ncomp_select_plot_x_label_size",
    plsda_ncomp_select_plot_x_tick_label_size = "plsda_ncomp_select_plot_x_tick_label_size",
    plsda_ncomp_select_plot_y_label_size = "plsda_ncomp_select_plot_y_label_size",
    plsda_ncomp_select_plot_y_tick_label_size = "plsda_ncomp_select_plot_y_tick_label_size",
    plsda_perm_method = "plsda_perm_method",
    plsda_perm_n = "plsda_perm_n",
    plsda_perm_plot_symbol_size = "plsda_perm_plot_symbol_size",
    plsda_perm_plot_legend_size = "plsda_perm_plot_legend_size",
    plsda_perm_plot_x_label_size = "plsda_perm_plot_x_label_size",
    plsda_perm_plot_x_tick_label_size = "plsda_perm_plot_x_tick_label_size",
    plsda_perm_plot_y_label_size = "plsda_perm_plot_y_label_size",
    plsda_perm_plot_y_tick_label_size = "plsda_perm_plot_y_tick_label_size",
    plsda_perm_plot_width = "plsda_perm_plot_width",
    plsda_perm_plot_height = "plsda_perm_plot_height",
    plsda_scoreplot_ellipse_conf = "plsda_scoreplot_ellipse_conf",
    plsda_roc_smooth = "plsda_roc_smooth",
    plsda_vip_alpha = "plsda_vip_alpha",
    plsda_vip_boot = "plsda_vip_boot",
    plsda_vip_boot_n = "plsda_vip_boot_n",
    plsda_vip_plot_errorbar = "plsda_vip_plot_errorbar",
    plsda_vip_plot_errorbar_width = "plsda_vip_plot_errorbar_width",
    plsda_vip_plot_errorbar_label_size = "plsda_vip_plot_errorbar_label_size",
    plsda_vip_plot_x_textangle = "plsda_vip_plot_x_textangle",
    plsda_vip_plot_x_label_size = "plsda_vip_plot_x_label_size",
    plsda_vip_plot_x_tick_label_size = "plsda_vip_plot_x_tick_label_size",
    plsda_vip_plot_y_label_size = "plsda_vip_plot_y_label_size",
    plsda_vip_plot_y_tick_label_size = "plsda_vip_plot_y_tick_label_size",
    plsda_vip_plot_width = "plsda_vip_plot_width",
    plsda_vip_plot_height = "plsda_vip_plot_height"
  )
  return(vars)
}

# getting configs from config file
getConfigsFromFile <- function(configFilePath){
  # configfilepath <- "RBioFS/connectivity_ml_config"
  
  
  configs <- read_delim(configFilePath, 
                        "\t", escape_double = FALSE, col_names = FALSE, 
                        comment = "#", trim_ws = TRUE, quote = "\"",
                        escape_backslash = FALSE)
  config_values <- gsub('"', '', sub(".*=", '', configs[[1]]))      #getting values from right side of '=' sign
  config_var_names <- sub("\\=.*", '', configs[[1]]) #getting variable name from left side of '=' sign
  
  configlist <- list()
  for (i in 1:length(config_values)){
    configlist[[config_var_names[i]]] <- config_values[i]
  }
  
  # checking the variable existence 
  varNames <- varNames()
  missingidx <- vector()
  for (i in 1:length(config_var_names)){
    varexist <- varNames[i] %in% config_var_names
    if (varexist == FALSE)
      missingidx <- append(missingidx, i)       #getting index of missing variables
  }
  
  if (length(missingidx)>0){                    #if there are missing vars in file, return the missing ones
    missingvars = paste(varNames[missingidx], collapse = ", ")
    output <- list(configlist, config_values, config_var_names, missingvars)
  }
  else{
    output <- list(configlist, config_values, config_var_names)
  }
    
  return(output)
}

# setting values in input fields to values from file
configFileToUI <- function(fileinputconfigs, session){
  vars <- varNames()
  updateTextInput(session = session, inputId = vars$random_state, value = fileinputconfigs$random_state)
  updateSelectInput(session = session, inputId = vars$log2_trans, selected = fileinputconfigs$log2_trans)
  updateTextInput(session = session, inputId = vars$htmap_textsize_col, value = fileinputconfigs$htmap_textsize_col)
  updateTextInput(session = session, inputId = vars$htmap_textangle_col, value = fileinputconfigs$htmap_textangle_col)
  updateSelectInput(session = session, inputId = vars$htmap_lab_row, selected = fileinputconfigs$htmap_lab_row)
  updateTextInput(session = session, inputId = vars$htmap_textsize_row, value = fileinputconfigs$htmap_textsize_row)
  updateTextInput(session = session, inputId = vars$htmap_keysize, value = fileinputconfigs$map_keysize)
  updateTextInput(session = session, inputId = vars$htmap_key_xlab, value = fileinputconfigs$htmap_key_xlab)
  updateTextInput(session = session, inputId = vars$htmap_key_ylab, value = fileinputconfigs$htmap_key_ylab)
  updateTextInput(session = session, inputId = vars$htmap_margin, value = fileinputconfigs$htmap_margin)
  updateTextInput(session = session, inputId = vars$htmap_width, value = fileinputconfigs$htmap_width)
  updateTextInput(session = session, inputId = vars$htmap_height, value = fileinputconfigs$htmap_height)
  updateSelectInput(session = session, inputId = vars$pca_scale_data, selected = fileinputconfigs$pca_scale_data)
  updateSelectInput(session = session, inputId = vars$pca_centre_data, selected = fileinputconfigs$pca_centre_data)
  updateTextInput(session = session, inputId = vars$pca_centre_data, value = fileinputconfigs$pca_centre_data)
  updateSelectInput(session = session, inputId = vars$pca_pc, selected = fileinputconfigs$pca_pc)
  updateTextInput(session = session, inputId = vars$pca_biplot_samplelabel_type, value = fileinputconfigs$pca_biplot_samplelabel_type)
  updateTextInput(session = session, inputId = vars$pca_biplot_samplelabel_size, value = fileinputconfigs$pca_biplot_samplelabel_size)
  updateSelectInput(session = session, inputId = vars$pca_biplot_symbol_size, selected = fileinputconfigs$pca_biplot_symbol_size)
  updateTextInput(session = session, inputId = vars$pca_biplot_ellipse, value = fileinputconfigs$pca_biplot_ellipse)
  updateSelectInput(session = session, inputId = vars$pca_biplot_ellipse_conf, selected = fileinputconfigs$pca_biplot_ellipse_conf)
  updateTextInput(session = session, inputId = vars$pca_biplot_loading, value = fileinputconfigs$pca_biplot_loading)
  updateSelectInput(session = session, inputId = vars$pca_biplot_loading_textsize, selected = fileinputconfigs$pca_biplot_loading_textsize)
  updateTextInput(session = session, inputId = vars$pca_biplot_multi_desity, value = fileinputconfigs$pca_biplot_multi_desity)
  updateSelectInput(session = session, inputId = vars$pca_biplot_multi_striplabel_size, selected = fileinputconfigs$pca_biplot_multi_striplabel_size)
  updateTextInput(session = session, inputId = vars$pca_rightside_y, value = fileinputconfigs$pca_rightside_y)
  updateTextInput(session = session, inputId = vars$pca_x_tick_label_size, value = fileinputconfigs$pca_x_tick_label_size)
  updateTextInput(session = session, inputId = vars$pca_y_tick_label_size, value = fileinputconfigs$pca_y_tick_label_size)
  updateTextInput(session = session, inputId = vars$pca_width, value = fileinputconfigs$pca_width)
  updateSelectInput(session = session, inputId = vars$pca_height, selected = fileinputconfigs$pca_height)
  updateTextInput(session = session, inputId = vars$uni_fdr, value = fileinputconfigs$uni_fdr)
  updateTextInput(session = session, inputId = vars$uni_alpha, value = fileinputconfigs$uni_alpha)
  updateTextInput(session = session, inputId = vars$uni_fold_change, value = fileinputconfigs$uni_fold_change)
  updateTextInput(session = session, inputId = vars$volcano_n_top_connection, value = fileinputconfigs$volcano_n_top_connection)
  updateTextInput(session = session, inputId = vars$volcano_symbol_size, value = fileinputconfigs$volcano_symbol_size)
  updateTextInput(session = session, inputId = vars$volcano_sig_colour, value = fileinputconfigs$volcano_sig_colour)
  updateTextInput(session = session, inputId = vars$volcano_nonsig_colour, value = fileinputconfigs$volcano_nonsig_colour)
  updateTextInput(session = session, inputId = vars$volcano_x_text_size, value = fileinputconfigs$volcano_x_text_size)
  updateTextInput(session = session, inputId = vars$volcano_y_text_size, value = fileinputconfigs$volcano_y_text_size)
  updateTextInput(session = session, inputId = vars$volcano_width, value = fileinputconfigs$volcano_width)
  updateTextInput(session = session, inputId = vars$volcano_height, value = fileinputconfigs$volcano_height)
  updateTextInput(session = session, inputId = vars$sig_htmap_textsize_col, value = fileinputconfigs$sig_htmap_textsize_col)
  updateTextInput(session = session, inputId = vars$sig_htmap_textangle_col, value = fileinputconfigs$sig_htmap_textangle_col)
  updateTextInput(session = session, inputId = vars$sig_htmap_textsize_row, value = fileinputconfigs$sig_htmap_textsize_row)
  updateTextInput(session = session, inputId = vars$sig_htmap_keysize, value = fileinputconfigs$sig_htmap_keysize)
  updateTextInput(session = session, inputId = vars$sig_htmap_key_xlab, value = fileinputconfigs$sig_htmap_key_xlab)
  updateTextInput(session = session, inputId = vars$sig_htmap_key_ylab, value = fileinputconfigs$sig_htmap_key_ylab)
  updateTextInput(session = session, inputId = vars$sig_htmap_margin, value = fileinputconfigs$sig_htmap_margin)
  updateTextInput(session = session, inputId = vars$sig_htmap_width, value = fileinputconfigs$sig_htmap_width)
  updateTextInput(session = session, inputId = vars$sig_htmap_height, value = fileinputconfigs$sig_htmap_height)
  updateTextInput(session = session, inputId = vars$sig_pca_pc, value = fileinputconfigs$sig_pca_pc)
  updateTextInput(session = session, inputId = vars$sig_pca_biplot_ellipse_conf, value = fileinputconfigs$sig_pca_biplot_ellipse_conf)
  updateSelectInput(session = session, inputId = vars$cpu_cluster, selected = fileinputconfigs$cpu_cluster)
  updateTextInput(session = session, inputId = vars$training_percentage, value = fileinputconfigs$training_percentage)
  updateSelectInput(session = session, inputId = vars$svm_cv_centre_scale, selected = fileinputconfigs$svm_cv_centre_scale)
  updateSelectInput(session = session, inputId = vars$svm_cv_kernel, selected = fileinputconfigs$svm_cv_kernel)
  updateTextInput(session = session, inputId = vars$svm_cv_cross_k, value = fileinputconfigs$svm_cv_cross_k)
  updateSelectInput(session = session, inputId = vars$svm_cv_tune_method, selected = fileinputconfigs$svm_cv_tune_method)
  updateTextInput(session = session, inputId = vars$svm_cv_tune_cross_k, value = fileinputconfigs$svm_cv_tune_cross_k)
  updateTextInput(session = session, inputId = vars$svm_cv_tune_boot_n, value = fileinputconfigs$svm_cv_tune_boot_n)
  updateTextInput(session = session, inputId = vars$svm_cv_fs_rf_ifs_ntree, value = fileinputconfigs$svm_cv_fs_rf_ifs_ntree)
  updateTextInput(session = session, inputId = vars$svm_cv_fs_rf_sfs_ntree, value = fileinputconfigs$svm_cv_fs_rf_sfs_ntree)
  updateSelectInput(session = session, inputId = vars$svm_cv_best_model_method, selected = fileinputconfigs$svm_cv_best_model_method)
  updateTextInput(session = session, inputId = vars$svm_cv_fs_count_cutoff, value = fileinputconfigs$svm_cv_fs_count_cutoff)
  updateTextInput(session = session, inputId = vars$svm_cross_k, value = fileinputconfigs$svm_cross_k)
  updateTextInput(session = session, inputId = vars$svm_tune_cross_k, value = fileinputconfigs$svm_tune_cross_k)
  updateTextInput(session = session, inputId = vars$svm_tune_boot_n, value = fileinputconfigs$svm_tune_boot_n)
  updateSelectInput(session = session, inputId = vars$svm_perm_method, selected = fileinputconfigs$svm_perm_method)
  updateTextInput(session = session, inputId = vars$svm_perm_n, value = fileinputconfigs$svm_perm_n)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_symbol_size, value = fileinputconfigs$svm_perm_plot_symbol_size)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_legend_size, value = fileinputconfigs$svm_perm_plot_legend_size)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_x_label_size, value = fileinputconfigs$svm_perm_plot_x_label_size)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_x_tick_label_size, value = fileinputconfigs$svm_perm_plot_x_tick_label_size)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_y_label_size, value = fileinputconfigs$svm_perm_plot_y_label_size)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_y_tick_label_size, value = fileinputconfigs$svm_perm_plot_y_tick_label_size)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_width, value = fileinputconfigs$svm_perm_plot_width)
  updateTextInput(session = session, inputId = vars$svm_perm_plot_height, value = fileinputconfigs$svm_perm_plot_height)
  updateTextInput(session = session, inputId = vars$svm_roc_smooth, value = fileinputconfigs$svm_roc_smooth)
  updateTextInput(session = session, inputId = vars$svm_roc_symbol_size, value = fileinputconfigs$svm_roc_symbol_size)
  updateTextInput(session = session, inputId = vars$svm_roc_legend_size, value = fileinputconfigs$svm_roc_legend_size)
  updateTextInput(session = session, inputId = vars$svm_roc_x_label_size, value = fileinputconfigs$svm_roc_x_label_size)
  updateTextInput(session = session, inputId = vars$svm_roc_x_tick_label_size, value = fileinputconfigs$svm_roc_x_tick_label_size)
  updateTextInput(session = session, inputId = vars$svm_roc_y_label_size, value = fileinputconfigs$svm_roc_y_label_size)
  updateTextInput(session = session, inputId = vars$svm_roc_y_tick_label_size, value = fileinputconfigs$svm_roc_y_tick_label_size)
  updateTextInput(session = session, inputId = vars$svm_roc_width, value = fileinputconfigs$svm_roc_width)
  updateTextInput(session = session, inputId = vars$svm_roc_height, value = fileinputconfigs$svm_roc_height)
  updateTextInput(session = session, inputId = vars$svm_rffs_pca_pc, value = fileinputconfigs$svm_rffs_pca_pc)
  updateTextInput(session = session, inputId = vars$svm_rffs_pca_biplot_ellipse_conf, value = fileinputconfigs$svm_rffs_pca_biplot_ellipse_conf)
  updateTextInput(session = session, inputId = vars$rffs_htmap_textsize_col, value = fileinputconfigs$rffs_htmap_textsize_col)
  updateTextInput(session = session, inputId = vars$rffs_htmap_textangle_col, value = fileinputconfigs$rffs_htmap_textangle_col)
  updateTextInput(session = session, inputId = vars$rffs_htmap_textsize_row, value = fileinputconfigs$rffs_htmap_textsize_row)
  updateTextInput(session = session, inputId = vars$rffs_htmap_keysize, value = fileinputconfigs$rffs_htmap_keysize)
  updateTextInput(session = session, inputId = vars$rffs_htmap_key_xlab, value = fileinputconfigs$rffs_htmap_key_xlab)
  updateTextInput(session = session, inputId = vars$rffs_htmap_key_ylab, value = fileinputconfigs$rffs_htmap_key_ylab)
  updateTextInput(session = session, inputId = vars$rffs_htmap_key_margin, value = fileinputconfigs$rffs_htmap_key_margin)
  updateTextInput(session = session, inputId = vars$rffs_htmap_key_width, value = fileinputconfigs$rffs_htmap_key_width)
  updateTextInput(session = session, inputId = vars$rffs_htmap_key_height, value = fileinputconfigs$rffs_htmap_key_height)
  updateTextInput(session = session, inputId = vars$plsda_validation, value = fileinputconfigs$plsda_validation)
  updateTextInput(session = session, inputId = vars$plsda_validation_segment, value = fileinputconfigs$plsda_validation_segment)
  updateTextInput(session = session, inputId = vars$plsda_init_ncomp, value = fileinputconfigs$plsda_init_ncomp)
  updateSelectInput(session = session, inputId = vars$plsda_ncomp_select_method, selected = fileinputconfigs$plsda_ncomp_select_method)
  updateTextInput(session = session, inputId = vars$plsda_ncomp_select_plot_symbol_size, value = fileinputconfigs$plsda_ncomp_select_plot_symbol_size)
  updateTextInput(session = session, inputId = vars$plsda_ncomp_select_plot_legend_size, value = fileinputconfigs$plsda_ncomp_select_plot_legend_size)
  updateTextInput(session = session, inputId = vars$plsda_ncomp_select_plot_x_label_size, value = fileinputconfigs$plsda_ncomp_select_plot_x_label_size)
  updateTextInput(session = session, inputId = vars$plsda_ncomp_select_plot_x_tick_label_size, value = fileinputconfigs$plsda_ncomp_select_plot_x_tick_label_size)
  updateTextInput(session = session, inputId = vars$plsda_ncomp_select_plot_y_label_size, value = fileinputconfigs$plsda_ncomp_select_plot_y_label_size)
  updateTextInput(session = session, inputId = vars$plsda_ncomp_select_plot_y_tick_label_size, value = fileinputconfigs$plsda_ncomp_select_plot_y_tick_label_size)
  updateSelectInput(session = session, inputId = vars$plsda_perm_method, selected = fileinputconfigs$plsda_perm_method)
  updateTextInput(session = session, inputId = vars$plsda_perm_n, value = fileinputconfigs$plsda_perm_n)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_symbol_size, value = fileinputconfigs$plsda_perm_plot_symbol_size)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_legend_size, value = fileinputconfigs$plsda_perm_plot_legend_size)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_x_label_size, value = fileinputconfigs$plsda_perm_plot_x_label_size)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_x_tick_label_size, value = fileinputconfigs$plsda_perm_plot_x_tick_label_size)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_y_label_size, value = fileinputconfigs$plsda_perm_plot_y_label_size)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_y_tick_label_size, value = fileinputconfigs$plsda_perm_plot_y_tick_label_size)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_width, value = fileinputconfigs$plsda_perm_plot_width)
  updateTextInput(session = session, inputId = vars$plsda_perm_plot_height, value = fileinputconfigs$plsda_perm_plot_height)
  updateTextInput(session = session, inputId = vars$plsda_scoreplot_ellipse_conf, value = fileinputconfigs$plsda_scoreplot_ellipse_conf)
  updateSelectInput(session = session, inputId = vars$plsda_roc_smooth, selected = fileinputconfigs$plsda_roc_smooth)
  updateTextInput(session = session, inputId = vars$plsda_vip_alpha, value = fileinputconfigs$plsda_vip_alpha)
  updateSelectInput(session = session, inputId = vars$plsda_vip_boot, selected = fileinputconfigs$plsda_vip_boot)
  updateTextInput(session = session, inputId = vars$plsda_vip_boot_n, value = fileinputconfigs$plsda_vip_boot_n)
  updateSelectInput(session = session, inputId = vars$plsda_vip_plot_errorbar, selected = fileinputconfigs$plsda_vip_plot_errorbar)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_errorbar_width, value = fileinputconfigs$plsda_vip_plot_errorbar_width)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_errorbar_label_size, value = fileinputconfigs$plsda_vip_plot_errorbar_label_size)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_x_textangle, value = fileinputconfigs$plsda_vip_plot_x_textangle)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_x_label_size, value = fileinputconfigs$plsda_vip_plot_x_label_size)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_x_tick_label_size, value = fileinputconfigs$plsda_vip_plot_x_tick_label_size)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_y_label_size, value = fileinputconfigs$plsda_vip_plot_y_label_size)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_y_tick_label_size, value = fileinputconfigs$plsda_vip_plot_y_tick_label_size)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_width, value = fileinputconfigs$plsda_vip_plot_width)
  updateTextInput(session = session, inputId = vars$plsda_vip_plot_height, value = fileinputconfigs$plsda_vip_plot_height)
}

# univariate arguments from inputs
univariateConfigs <- function(configs){
  # remoing all the \ and " characters from inputs
  configs <- lapply(configs, function(x) str_remove_all(x, '\"'))
  
  # setting an empty vector as the arguments
  args <- vector()
  args[10] <- configs$log2_trans
  args[11] <- configs$htmap_textsize_col
  args[12] <- configs$htmap_textangle_col
  args[13] <- configs$htmap_lab_row
  args[14] <- configs$htmap_textsize_row
  args[15] <- configs$htmap_keysize
  args[16] <- configs$htmap_key_xlab
  args[17] <- configs$htmap_key_ylab
  args[18] <- configs$htmap_margin
  args[19] <- configs$htmap_width
  args[20] <- configs$htmap_height
  
  args[21] <- configs$pca_scale_data
  args[22] <- configs$pca_center_data
  args[23] <- configs$pca_pc
  args[24] <- configs$pca_biplot_samplelabel_type
  args[25] <- configs$pca_biplot_samplelabel_size
  args[26] <- configs$pca_biplot_symbol_size
  args[27] <- configs$pca_biplot_ellipse
  args[28] <- configs$pca_biplot_ellipse_conf
  args[29] <- configs$pca_biplot_loading
  args[30] <- configs$pca_biplot_loading_textsize
  args[31] <- configs$pca_biplot_multi_density
  args[32] <- configs$pca_biplot_multi_striplabel_size
  args[33] <- configs$pca_rightside_y
  args[34] <- configs$pca_x_tick_label_size
  args[35] <- configs$pca_y_tick_label_size
  args[36] <- configs$pca_width
  args[37] <- configs$pca_height
  
  args[39] <- configs$uni_fdr
  args[40] <- configs$uni_alpha
  args[41] <- configs$uni_fold_change
  args[42] <- configs$volcano_n_top_connections
  args[43] <- configs$volcano_symbol_size
  args[44] <- configs$volcano_sig_colour
  args[45] <- configs$volcano_nonsig_colour
  args[46] <- configs$volcano_x_text_size
  args[47] <- configs$volcano_y_text_size
  args[48] <- configs$volcano_width
  args[49] <- configs$volcano_height
    
  args[50] <- configs$sig_htmap_textsize_col
  args[51] <- configs$sig_htmap_textangle_col
  args[52] <- configs$sig_htmap_textsize_row
  args[53] <- configs$sig_htmap_keysize
  args[54] <- configs$sig_htmap_key_xlab
  args[55] <- configs$sig_htmap_key_ylab
  args[56] <- configs$sig_htmap_margin
  args[57] <- configs$sig_htmap_width
  args[58] <- configs$sig_htmap_height
  args[59] <- configs$sig_pca_pc
  args[60] <- configs$sig_pca_biplot_ellipse_conf
  
  return(args)
}

# ml_svm arguments from inputs
ml_svmConfigs <- function(configs){
  configs <- lapply(configs, function(x) str_remove_all(x, '\"'))
  
  # setting an empty vector as the arguments
  args <- vector()
  args[11] <- configs$cpu_cluster
  args[12] <- configs$training_percentage
    
  args[13] <- configs$svm_cv_center_scale
  args[14] <- configs$svm_cv_kernel
  args[15] <- configs$svm_cv_cross_k
  args[16] <- configs$svm_cv_tune_method
  args[17] <- configs$svm_cv_tune_cross_k
  args[18] <- configs$svm_cv_tune_boot_n
  args[19] <- configs$svm_cv_fs_rf_ifs_ntree
  args[20] <- configs$svm_cv_fs_rf_sfs_ntree
  args[21] <- configs$svm_cv_best_model_method
  args[22] <- configs$svm_cv_fs_count_cutoff
  
  args[23] <- configs$svm_cross_k
  args[24] <- configs$svm_tune_cross_k
  args[25] <- configs$svm_tune_boot_n
  
  args[26] <- configs$svm_perm_method
  args[27] <- configs$svm_perm_n
  args[28] <- configs$svm_perm_plot_symbol_size
  args[29] <- configs$svm_perm_plot_legend_size
  args[30] <- configs$svm_perm_plot_x_label_size
  args[31] <- configs$svm_perm_plot_x_tick_label_size
  args[32] <- configs$svm_perm_plot_y_label_size
  args[33] <- configs$svm_perm_plot_y_tick_label_size
  args[34] <- configs$svm_perm_plot_width
  args[35] <- configs$svm_perm_plot_height
  
  args[36] <- configs$svm_roc_smooth
  args[37] <- configs$svm_roc_symbol_size
  args[38] <- configs$svm_roc_legend_size
  args[39] <- configs$svm_roc_x_label_size
  args[40] <- configs$svm_roc_x_tick_label_size
  args[41] <- configs$svm_roc_y_label_size
  args[42] <- configs$svm_roc_y_tick_label_size
  args[43] <- configs$svm_roc_width
  args[44] <- configs$svm_roc_height
  
  args[45] <- configs$pca_scale_data
  args[46] <- configs$pca_center_data
  args[47] <- configs$pca_biplot_samplelabel_type
  args[48] <- configs$pca_biplot_samplelabel_size
  args[49] <- configs$pca_biplot_symbol_size
  args[50] <- configs$pca_biplot_ellipse
  args[51] <- configs$pca_biplot_loading
  args[52] <- configs$pca_biplot_loading_textsize
  args[53] <- configs$pca_biplot_multi_density
  args[54] <- configs$pca_biplot_multi_striplabel_size
  args[55] <- configs$pca_rightside_y
  args[56] <- configs$pca_x_tick_label_size
  args[57] <- configs$pca_y_tick_label_size
  args[58] <- configs$pca_width
  args[59] <- configs$pca_height
  args[60] <- configs$svm_rffs_pca_pc
  args[61] <- configs$svm_rffs_pca_biplot_ellipse_conf
  
  # args[62] <- configs$CVUNI
  args[63] <- configs$log2_trans
  # args[64] <- configs$CONTRAST
  args[65] <- configs$uni_fdr
  args[66] <- configs$uni_alpha
  
  args[67] <- configs$rffs_htmap_textsize_col
  args[68] <- configs$rffs_htmap_textangle_col
  args[69] <- configs$htmap_lab_row
  args[70] <- configs$rffs_htmap_textsize_row
  args[71] <- configs$rffs_htmap_keysize
  args[72] <- configs$rffs_htmap_key_xlab
  args[73] <- configs$rffs_htmap_key_ylab
  args[74] <- configs$rffs_htmap_margin
  args[75] <- configs$rffs_htmap_width
  args[76] <- configs$rffs_htmap_width
  
  args[77] <- configs$random_state
  
  return(args)
}

reg_univariateConfigs <- function(configs){
  # removing all the \ and " characters from inputs
  configs <- lapply(configs, function(x) str_remove_all(x, '\"'))
  
  # setting an empty vector as the arguments
  args <- vector()
  
  args[9] <- configs$reg_log2_trans
  args[10] <- configs$reg_htmap_textsize_col
  args[11] <- configs$reg_htmap_textangle_col
  args[12] <- configs$reg_htmap_lab_row
  args[13] <- configs$reg_htmap_textsize_row
  args[14] <- configs$reg_htmap_keysize
  args[15] <- configs$reg_htmap_key_xlab
  args[16] <- configs$reg_htmap_key_ylab
  args[17] <- configs$reg_htmap_margin
  args[18] <- configs$reg_htmap_width
  args[19] <- configs$reg_htmap_height
  
  args[20] <- configs$reg_uni_fdr
  args[21] <- configs$reg_uni_alpha
  args[22] <- configs$reg_uni_fold_change
  
  args[23] <- configs$reg_sig_htmap_textsize_col
  args[24] <- configs$reg_sig_htmap_textangle_col
  args[25] <- configs$reg_sig_htmap_textsize_row
  args[26] <- configs$reg_sig_htmap_keysize
  args[27] <- configs$reg_sig_htmap_key_xlab
  args[28] <- configs$reg_sig_htmap_key_ylab
  args[29] <- configs$reg_sig_htmap_margin
  args[30] <- configs$reg_sig_htmap_width
  args[31] <- configs$reg_sig_htmap_height
  
  return(args)
  
}

reg_ml_svmConfigs <- function(configs){
  # removing all the \ and " characters from inputs
  configs <- lapply(configs, function(x) str_remove_all(x, '\"'))
  
  # setting an empty vector as the arguments
  args <- vector()
  
  args[11] <- configs$reg_cpu_cluster
  args[12] <- configs$reg_training_percentage
    
  args[13] <- configs$reg_svm_cv_center_scale
  args[14] <- configs$reg_svm_cv_kernel
  args[15] <- configs$reg_svm_cv_cross_k
  args[16] <- configs$reg_svm_cv_tune_method
  args[17] <- configs$reg_svm_cv_tune_cross_k
  args[18] <- configs$reg_svm_cv_tune_boot_n
  args[19] <- configs$reg_svm_cv_fs_rf_ifs_ntree
  args[20] <- configs$reg_svm_cv_fs_rf_sfs_ntree
  args[21] <- configs$reg_svm_cv_best_model_method
  args[22] <- configs$reg_svm_cv_fs_count_cutoff
    
  args[23] <- configs$reg_svm_cross_k
  args[24] <- configs$reg_svm_tune_cross_k
  args[25] <- configs$reg_svm_tune_boot_n
  
  args[26] <- configs$reg_svm_perm_method
  args[27] <- configs$reg_svm_perm_n
  args[28] <- configs$reg_svm_perm_plot_symbol_size
  args[29] <- configs$reg_svm_perm_plot_legend_size
  args[30] <- configs$reg_svm_perm_plot_x_label_size
  args[31] <- configs$reg_svm_perm_plot_x_tick_label_size
  args[32] <- configs$reg_svm_perm_plot_y_label_size
  args[33] <- configs$reg_svm_perm_plot_y_tick_label_size
  args[34] <- configs$reg_svm_perm_plot_width
  args[35] <- configs$reg_svm_perm_plot_height
  
  args[36] <- configs$reg_svm_roc_threshold # add this in the input fields
  args[37] <- configs$reg_svm_roc_smooth
  args[38] <- configs$reg_svm_roc_symbol_size
  args[39] <- configs$reg_svm_roc_legend_size
  args[40] <- configs$reg_svm_roc_x_label_size
  args[41] <- configs$reg_svm_roc_x_tick_label_size
  args[42] <- configs$reg_svm_roc_y_label_size
  args[43] <- configs$reg_svm_roc_y_tick_label_size
  args[44] <- configs$reg_svm_roc_width
  args[45] <- configs$reg_svm_roc_height
  
  args[46] <- configs$reg_rffs_htmap_textsize_col
  args[47] <- configs$reg_rffs_htmap_textangle_col
  args[48] <- configs$reg_htmap_lab_row
  args[49] <- configs$reg_rffs_htmap_textsize_row
  args[50] <- configs$reg_rffs_htmap_keysize
  args[51] <- configs$reg_rffs_htmap_key_xlab
  args[52] <- configs$reg_rffs_htmap_key_ylab
  args[53] <- configs$reg_rffs_htmap_margin
  args[54] <- configs$reg_rffs_htmap_width
  args[55] <- configs$reg_rffs_htmap_height
  
  # args[56] <- configs$
  args[57] <- configs$reg_log2_trans
  args[58] <- configs$reg_uni_fdr
  args[59] <- configs$reg_uni_alpha
  
  args[60] <- configs$reg_random_state
  
  return(args)
  }