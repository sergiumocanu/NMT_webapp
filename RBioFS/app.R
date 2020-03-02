#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(R.matlab)
library(plyr)
library(DT)
library(shinyBS)
library(sortable)

source("input_dat_process.R")
source("univariate.R")
source("defaultconfig.R")
source("ml_svm.R")


withConsoleRedirect <- function(containerId, expr) {
    # Change type="output" to type="message" to catch stderr
    # (messages, warnings, and errors) instead of stdout.
    txt <- capture.output(results <- expr, type = "output")
    if (length(txt) > 0) {
        insertUI(
            paste0("#", containerId),
            where = "beforeEnd",
            ui = paste0(txt, "\n", collapse = "")
        )
    }
    results
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Header in top left of window
    dashboardHeader(title = "SVM classification model"),
    
    # sidebar menu items
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Initialize",
            tabName = "init",
            fileInput(
                inputId = "files",
                label = "Upload files",
                multiple = TRUE ,
                accept = c(".mat", '.csv')
            ),
            # selectInput(
            #     inputId = "select_file",
            #     label = "View File",
            #     choices = "",
            #     multiple = FALSE
            # ),
            # conditionalPanel(
            #     'input.data === "training"',
            #     sliderInput(
            #         inputId = "training_3D_choose",
            #         min = 1,
            #         max = 100,
            #         value = 1
            #     )
            # ),
            hr(),
            selectizeInput(
                inputId = "annot",
                label = "Annotation Variables",
                options = list(placeholder = "Please Select an Option"),
                choices = "",
                multiple = TRUE
            ),
            selectizeInput(
                inputId = "node",
                label = "Node Variables",
                options = list(placeholder = "Please Select an Option"),
                choices = "",
                multiple = TRUE
            ),
            
            actionButton("show_vars", "Confirm Variable Choices"),
            hr(),
            div(id = "configfilebutton")
            
        )
        
    )),
    
    
    
    # Dashboard body items
    dashboardBody(
        fluidRow(
            uiOutput("variables")
            
            
        
        ),
        fluidRow(
            textOutput("result"),
            box(
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                title = "View Data",
                # tableOutput(outputId = "contents")
                tabsetPanel(
                    id = "data",
                    tabPanel("Training", DT::dataTableOutput("training")),
                    tabPanel("Annotation", DT::dataTableOutput("annotation")),
                    tabPanel("Node", DT::dataTableOutput("node"))
                )
            ),
            box(
                #     status = "primary",
                # solidHeader = TRUE,
                collapsible = TRUE,
                title = "Config File Settings",
                # tableOutput(outputId = "configfile")
                # h3("Configuration Settings"),
                hr(),
                h4("Settings for data pre-processing", align = "center"),
                selectInput(
                    inputId = "log2_trans",
                    label = "log2_trans",
                    choices = c("TRUE", "FALSE"),
                    selected = "TRUE"
                ),
                
                hr(),
                h4("Settings for all-connection clustering analysis", align = "center"),
                h5("Hierarchical clustering", align = "center"),
                textInput(
                    inputId = "htmap_textsize_col",
                    label = "htmap_textsize_col",
                    value = "0.3"
                ),
                textInput(
                    inputId = "htmap_textangle_col",
                    label = "htmap_textangle_col",
                    value = "90"
                ),
                selectInput(
                    inputId = "htmap_lab_row",
                    choices = c("TRUE", "FALSE"),
                    label = "htmap_lab_row"
                ),
                textInput(
                    inputId = "htmap_textsize_row",
                    label = "htmap_textsize_row",
                    value = "0.7"
                ),
                textInput(
                    inputId = "htmap_keysize",
                    label = "htmap_keysize",
                    value = "1.5"
                ),
                textInput(
                    inputId = "htmap_key_xlab",
                    label = "htmap_key_xlab",
                    value = "Normalized Connectivity"
                ),
                textInput(
                    inputId = "htmap_key_ylab",
                    label = "htmap_key_ylab",
                    value = "Pair Count"
                ),
                textInput(
                    inputId = "htmap_margin",
                    label = "htmap_margin",
                    value = "(6, 3)",
                    placeholder = "(6, 3)"
                ),
                textInput(
                    inputId = "htmap_width",
                    label = "htmap_width",
                    value = "15"
                ),
                textInput(
                    inputId = "htmap_height",
                    label = "htmap_height",
                    value = "10"
                ),
                
                
                hr(),
                h5("PCA", align = "center"),
                selectInput(
                    inputId = "pca_scale_data",
                    choices = c("TRUE", "FALSE"),
                    label = "pca_scale_data"
                ),
                selectInput(
                    inputId = "pca_center_data",
                    choices = c("TRUE", "FALSE"),
                    label = "pca_center_data"
                ),
                textInput(
                    inputId = "pca_pc",
                    label = "pca_pc",
                    value = "(1, 2, 3)"
                ),
                selectInput(
                    inputId = "pca_biplot_samplelabel_type",
                    label = "pca_biplot_samplelabel_type",
                    choices = c("None", "Direct", "Indirect")
                ),
                textInput(
                    inputId = "pca_biplot_samplelabel_size",
                    label = "pca_biplot_samplelabel_size",
                    value = "2"
                ),
                textInput(
                    inputId = "pca_biplot_symbol_size",
                    label = "pca_biplot_symbol_size",
                    value = "5"
                ),
                selectInput(
                    inputId = "pca_biplot_ellipse",
                    label = "pca_biplot_ellipse",
                    choices = c("TRUE", "FALSE"),
                    selected = "FALSE"
                ),
                textInput(
                    inputId = "pca_biplot_ellipse_conf",
                    label = "pca_biplot_ellipse_conf",
                    value = "0.9"
                ),
                selectInput(
                    inputId = "pca_biplot_loading",
                    label = "pca_biplot_loading",
                    choices = c("TRUE", "FALSE")
                ),
                textInput(
                    inputId = "pca_biplot_loading_textsize",
                    label = "pca_biplot_loading_textsize",
                    value = "3"
                ),
                selectInput(
                    inputId = "pca_biplot_multi_density",
                    label = "pca_biplot_multi_density",
                    choices = c("TRUE", FALSE)
                ),
                textInput(
                    inputId = "pca_biplot_multi_striplabel_size",
                    label = "pca_biplot_multi_striplabel_size",
                    value = "10"
                ),
                selectInput(
                    inputId = "pca_rightside_y",
                    label = "pca_rightside_y",
                    choices = c("TRUE", "FALSE"),
                    selected = "FALSE"
                ),
                textInput(
                    inputId = "pca_x_tick_label_size",
                    label = "pca_x_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "pca_y_tick_label_size",
                    label = "pca_y_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "pca_width",
                    label = "pca_width",
                    value = "170"
                ),
                textInput(
                    inputId = "pca_height",
                    label = "pca_height",
                    value = "150"
                ),
                
                hr(),
                h4("Settings for univariate analysis", align = "center"),
                h5("Univariate Analysis", align = "center"),
                selectInput(
                    inputId = "uni_fdr",
                    label = "uni_fdr",
                    choices = c("TRUE", "FALSE"),
                    selected = "FALSE"
                ),
                textInput(
                    inputId = "uni_alpha",
                    label = "uni_alpha",
                    value = "0.05"
                ),
                textInput(
                    inputId = "uni_fold_change",
                    label = "uni_fold_change",
                    value = "1"
                ),
                textInput(
                    inputId = "volcano_n_top_connections",
                    label = "volcano_n_top_connections",
                    value = "10"
                ),
                textInput(
                    inputId = "volcano_symbol_size",
                    label = "volcano_symbol_size",
                    value = "2"
                ),
                textInput(
                    inputId = "volcano_sig_colour",
                    label = "volcano_sig_colour",
                    value = "red"
                ),
                textInput(
                    inputId = "volcano_nonsig_colour",
                    label = "volcano_nonsig_colour",
                    value = "gray"
                ),
                textInput(
                    inputId = "volcano_x_text_size",
                    label = "volcano_x_text_size",
                    value = "10"
                ),
                textInput(
                    inputId = "volcano_y_text_size",
                    label = "volcano_y_text_size",
                    value = "10"
                ),
                textInput(
                    inputId = "volcano_width",
                    label = "volcano_width",
                    value = "170"
                ),
                textInput(
                    inputId = "volcano_height",
                    label = "volcano_height",
                    value = "150"
                ),
                
                hr(),
                h5("Hierarchical clustering: Sig Connections", align = "center"),
                textInput(
                    inputId = "sig_htmap_textsize_col",
                    label = "sig_htmap_textsize_col",
                    value = "0.3"
                ),
                textInput(
                    inputId = "sig_htmap_textangle_col",
                    label = "sig_htmap_textangle_col",
                    value = "90"
                ),
                textInput(
                    inputId = "sig_htmap_textsize_row",
                    label = "sig_htmap_textsize_row",
                    value = "0.2"
                ),
                textInput(
                    inputId = "sig_htmap_keysize",
                    label = "sig_htmap_keysize",
                    value = "1.5"
                ),
                textInput(
                    inputId = "sig_htmap_key_xlab",
                    label = "sig_htmap_key_xlab",
                    value = "Z score"
                ),
                textInput(
                    inputId = "sig_htmap_key_ylab",
                    label = "sig_htmap_key_ylab",
                    value = "Count"
                ),
                textInput(
                    inputId = "sig_htmap_margin",
                    label = "sig_htmap_margin",
                    value = "(6, 9)"
                ),
                textInput(
                    inputId = "sig_htmap_width",
                    label = "sig_htmap_width",
                    value = "15"
                ),
                textInput(
                    inputId = "sig_htmap_height",
                    label = "sig_htmap_height",
                    value = "10"
                ),
                textInput(
                    inputId = "sig_pca_pc",
                    label = "sig_pca_pc",
                    value = "(1, 2, 3)"
                ),
                textInput(
                    inputId = "sig_pca_biplot_ellipse_conf",
                    label = "sig_pca_biplot_ellipse_conf",
                    value = "0.9"
                ),
                
                hr(),
                h4("Settings for SVM machine learning analysis", align = "center"),
                h5("General Settings", align = "center"),
                selectInput(
                    inputId = "cpu_cluster",
                    label = "cpu_cluster",
                    choices = c("FORK", "PSOCK")
                ),
                
                hr(),
                h5("Data Processing", align = "center"),
                textInput(
                    inputId = "training_percentage",
                    label = "training_percentage",
                    value = "0.85"
                ),
                
                hr(),
                h5("SVM internal nested cross-validation and feature selection"),
                selectInput(
                    inputId = "svm_cv_center_scale",
                    label = "svm_cv_center_scale",
                    choices = c("TRUE", "FALSE")
                ),
                selectInput(
                    inputId = "svm_cv_kernel",
                    label = "svm_cv_kernel",
                    choices = c("linear", "polynomial", "radial", "sigmoid"),
                    selected = "radial"
                ),
                textInput(
                    inputId = "svm_cv_cross_k",
                    label = "svm_cv_cross_k",
                    value = "10"
                ),
                selectInput(
                    inputId = "svm_cv_tune_method",
                    label = "svm_cv_tune_method",
                    choices = c("cross", "boot", "fix"),
                    selected = "cross"
                ),
                textInput(
                    inputId = "svm_cv_tune_cross_k",
                    label = "svm_cv_tune_cross_k",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_cv_tune_boot_k",
                    label = "svm_cv_tune_boot_k",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_cv_fs_rf_ifs_ntree",
                    label = "svm_cv_fs_rf_ifs_ntree",
                    value = "501"
                ),
                textInput(
                    inputId = "svm_cv_fs_rf_sfs_ntree",
                    label = "svm_cv_fs_rf_sfs_ntree",
                    value = "501"
                ),
                selectInput(
                    inputId = "svm_cv_best_model_method",
                    label = "svm_cv_best_model_method",
                    choices = c("none", "median"),
                    selected = "none"
                ),
                textInput(
                    inputId = "svm_cv_fs_count_cutoff",
                    label = "svm_cv_fs_count_cutoff",
                    value = "2"
                ),
                
                hr(),
                h5("SVM modelling", align = "center"),
                textInput(
                    inputId = "svm_cross_k",
                    label = "svm_cross_k",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_tune_cross_k",
                    label = "svm_tune_cross_k",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_tune_boot_k",
                    label = "svm_tune_boot_k",
                    value = "10"
                ),
                
                hr(),
                h5("SVM permutation test", align = "center"),
                selectInput(
                    inputId = "svm_perm_method",
                    label = "svm_perm_method",
                    choices = c("by_y", "by_feature_per_y"),
                    selected = "by_y"
                ),
                textInput(
                    inputId = "svm_perm_n",
                    label = "svm_perm_n",
                    value = "99"
                ),
                textInput(
                    inputId = "svm_perm_plot_symbol_size",
                    label = "svm_perm_plot_symbol_size",
                    value = "2"
                ),
                textInput(
                    inputId = "svm_perm_plot_legend_size",
                    label = "svm_perm_plot_legend_size",
                    value = "9"
                ),
                textInput(
                    inputId = "svm_perm_plot_x_label_size",
                    label = "svm_perm_plot_x_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_perm_plot_x_tick_label_size",
                    label = "svm_perm_plot_x_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_perm_plot_y_label_size",
                    label = "svm_perm_plot_y_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_perm_plot_y_tick_label_size",
                    label = "svm_perm_plot_y_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_perm_plot_width",
                    label = "svm_perm_plot_width",
                    value = "250"
                ),
                textInput(
                    inputId = "svm_perm_plot_height",
                    label = "svm_perm_plot_height",
                    value = "150"
                ),
                
                hr(),
                h5("SVM ROC AUC", align = "center"),
                selectInput(
                    inputId = "svm_roc_smooth",
                    label = "svm_roc_smooth",
                    choices = c("TRUE", "FALSE"),
                    selected = "FALSE"
                ),
                textInput(
                    inputId = "svm_roc_symbol_size",
                    label = "svm_roc_symbol_size",
                    value = "2"
                ),
                textInput(
                    inputId = "svm_roc_legend_size",
                    label = "svm_roc_legend_size",
                    value = "9"
                ),
                textInput(
                    inputId = "svm_roc_x_label_size",
                    label = "svm_roc_x_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_roc_x_tick_label_size",
                    label = "svm_roc_x_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_roc_y_label_size",
                    label = "svm_roc_y_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_roc_y_tick_label_size",
                    label = "svm_roc_y_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "svm_roc_width",
                    label = "svm_roc_width",
                    value = "170"
                ),
                textInput(
                    inputId = "svm_roc_height",
                    label = "svm_roc_height",
                    value = "150"
                ),
                
                hr(),
                h5("SVM RFFS PCA", align = "center"),
                textInput(
                    inputId = "svm_rffs_pca_pc",
                    label = "svm_rffs_pca_pc",
                    value = "(1, 2, 3)"
                ),
                textInput(
                    inputId = "svm_rffs_pca_biplot_ellipse_conf",
                    label = "svm_rffs_pca_biplot_ellipse_conf",
                    value = "0.9"
                ),
                
                
                hr(),
                h4("Settings for PLS-DA Modelling for evaluating SVM results", align = "center"),
                h5("Global PLS-DA Settings", align = "center"),
                selectInput(
                    inputId = "plsda_validation",
                    label = "plsda_validation",
                    choices = c("none", "CV", "LOO"),
                    selected = "CV"
                ),
                textInput(
                    inputId = "plsda_validation_segment",
                    label = "plsda_validation_segment",
                    value = "10"
                ),
                
                hr(),
                h5("Initial PLS-DA Modelling Settings", align = "center"),
                textInput(
                    inputId = "plsda_init_ncomp",
                    label = "plsda_init_ncomp",
                    value = "150"
                ),
                
                hr(),
                h5("PLS-DA ncomp selection", align = "center"),
                selectInput(
                    inputId = "plsda_ncomp_select_method",
                    label = "plsda_ncomp_select_method",
                    choices = c("min", "1err", "randomization"),
                    selected = "1err"
                ),
                textInput(
                    inputId = "plsda_ncomp_select_plot_symbol_size",
                    label = "plsda_ncomp_select_plot_symbol_size",
                    value = "2"
                ),
                textInput(
                    inputId = "plsda_ncomp_select_plot_legend_size",
                    label = "plsda_ncomp_select_plot_legend_size",
                    value = "9"
                ),
                textInput(
                    inputId = "plsda_ncomp_select_plot_x_label_size",
                    label = "plsda_ncomp_select_plot_x_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_ncomp_select_plot_x_tick_label_size",
                    label = "plsda_ncomp_select_plot_x_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_ncomp_select_plot_y_label_size",
                    label = "plsda_ncomp_select_plot_y_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_ncomp_select_plot_y_tick_label_size",
                    label = "plsda_ncomp_select_plot_y_tick_label_size",
                    value = "10"
                ),
                
                hr(),
                h5("PLS-DA permutation test", align = "center"),
                selectInput(
                    inputId = "plsda_perm_method",
                    label = "plsda_perm_method",
                    choices = c("by_y", "by_feature_per_y"),
                    selected = "by_y"
                ),
                textInput(
                    inputId = "plsda_perm_n",
                    label = "plsda_perm_n",
                    value = "999"
                ),
                textInput(
                    inputId = "plsda_perm_plot_symbol_size",
                    label = "plsda_perm_plot_symbol_size",
                    value = "2"
                ),
                textInput(
                    inputId = "plsda_perm_plot_legend_size",
                    label = "plsda_perm_plot_legend_size",
                    value = "9"
                ),
                textInput(
                    inputId = "plsda_perm_plot_x_label_size",
                    label = "plsda_perm_plot_x_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_perm_plot_x_tick_label_size",
                    label = "plsda_perm_plot_x_tick_label_size",
                    value = "6"
                ),
                textInput(
                    inputId = "plsda_perm_plot_y_label_size",
                    label = "plsda_perm_plot_y_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_perm_plot_y_tick_label_size",
                    label = "plsda_perm_plot_y_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_perm_plot_width",
                    label = "plsda_perm_plot_width",
                    value = "250"
                ),
                textInput(
                    inputId = "plsda_perm_plot_height",
                    label = "plsda_perm_plot_height",
                    value = "150"
                ),
                
                hr(),
                h5("PLS-DA score plot", align = "center"),
                textInput(
                    inputId = "plsda_scoreplot_ellipse_conf",
                    label = "plsda_scoreplot_ellipse_conf",
                    value = "0.95"
                ),
                
                hr(),
                h5("PLS-DA ROC-AUC", align = "center"),
                selectInput(
                    inputId = "plsda_roc_smooth",
                    label = "plsda_roc_smooth",
                    choices = c("TRUE", "FALSE"),
                    selected = "FALSE"
                ),
                
                hr(),
                h5("PLS-DA VIP Analysis", align = "center"),
                textInput(
                    inputId = "plsda_vip_alpha",
                    label = "plsda_vip_alpha",
                    value = "0.8"
                ),
                selectInput(
                    inputId = "plsda_vip_boot",
                    label = "plsda_vip_boot",
                    choices = c("TRUE", "False"),
                    selected = "TRUE"
                ),
                textInput(
                    inputId = "plsda_vip_boot_n",
                    label = "plsda_vip_boot_n",
                    value = "50"
                ),
                selectInput(
                    inputId = "plsda_vip_plot_errorbar",
                    label = "plsda_vip_plot_errorbar",
                    choices = c("SEM", "SD"),
                    selected = "SEM"
                ),
                textInput(
                    inputId = "plsda_vip_plot_errorbar_width",
                    label = "plsda_vip_plot_errorbar_width",
                    value = "0.2"
                ),
                textInput(
                    inputId = "plsda_vip_plot_errorbar_label_size",
                    label = "plsda_vip_plot_errorbar_label_size",
                    value = "6"
                ),
                textInput(
                    inputId = "plsda_vip_plot_x_textangle",
                    label = "plsda_vip_plot_x_textangle",
                    value = "90"
                ),
                textInput(
                    inputId = "plsda_vip_plot_x_label_size",
                    label = "plsda_vip_plot_x_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_vip_plot_x_tick_label_size",
                    label = "plsda_vip_plot_x_tick_label_size",
                    value = "4"
                ),
                textInput(
                    inputId = "plsda_vip_plot_y_label_size",
                    label = "plsda_vip_plot_y_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_vip_plot_y_tick_label_size",
                    label = "plsda_vip_plot_y_tick_label_size",
                    value = "10"
                ),
                textInput(
                    inputId = "plsda_vip_plot_width",
                    label = "plsda_vip_plot_width",
                    value = "150"
                ),
                textInput(
                    inputId = "plsda_vip_plot_height",
                    label = "plsda_vip_plot_height",
                    value = "100"
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
                downloadButton("dwnld_cnfg", "Download"),
                
                collapsed = TRUE
            ),
            
            
            
            
            
            
            
            
        ),
        
        fluidRow(
            box(
                title = "Execute Commands",
                solidHeader = TRUE,
                collapsible = TRUE,
                background = "yellow",
                width = 4,
                
                actionButton(inputId = "univariate",
                             label = "Run Univariate",
                             width = 125),
                # progressBar(
                #     id = "univar_prog",
                #     value = 0,
                #     total = 100,
                #     title = "Running Univariate",
                #     display_pct = TRUE
                # ),
                actionButton(inputId = "svm",
                             label = "Run SVM model",
                             width = 125),
            ),
            
            box(
                title = "Console Output",
                solidHeader = TRUE,
                collapsible = TRUE,
                background = "olive",
                width = 8,
                
                
                
                verbatimTextOutput(outputId = "console"),
                tags$head(
                    tags$style(
                        "#console{overflow-y:scroll; max-height: 250px; background: ghostwhite;}"
                    )
                )
                
            )
        )
        
    )
)

# =============================================================================
# Define server logic
server <- function(input, output, session) {
    observe({
        files <- input$files$name
        updateSelectInput(session = session,
                          inputId = "select_file",
                          choices = files)
        
    })
    
    # checking and loading only the mat file
    training <- reactive({
        req(input$files)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$files$name))
        for (i in 1:length(input$files$name)) {
            if (grepl(".mat", input$files$name[i])) {
                df <- readMat(input$files$datapath[i])
                df <- df[[1]]
                dater[[1, i]] <- df
            }
        }
        return(dater)
    })
    
    # loading csv files
    csvfiles <- reactive({
        req(input$files)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$files$name))
        for (i in 1:length(input$files$name)) {
            if (grepl(".csv", input$files$name[i])) {
                df <- read.csv(input$files$datapath[i],
                               header = TRUE,
                               sep = ",")
                dater[[1, i]] <- df
            }
        }
        return(dater)
        
    })
    
    # rendering the drag and drop with variable names
    output$variables <- renderUI({
        req(input$files)
        
        tagList(box(
            title = "File Variables",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            bucket_list(
                header = "Please Choose Variables",
                group_name = "annotation_vars",
                orientation = "horizontal",
                add_rank_list(
                    text = "Available Variables:",
                    labels = c(colnames(csvfiles()[[1, 1]]), colnames(csvfiles()[[1, 2]])),
                    input_id = "availablevars"
                ),
                add_rank_list(
                    text = "Annotation Variables:",
                    labels = NULL,
                    input_id = "chosen_annot"
                ),
                add_rank_list(
                    text = "Node Variables:",
                    labels = NULL,
                    input_id = "chosen_node"
                )
            ),
            actionButton(
                inputId = "choose_vars",
                label = "Confirm Variables"
            )
        ))
    })
    
    # getting configurations from input fields (defaults)
    configs <- reactive({
        # test = eval(parse(text = paste0('c',input$htmap_margin)))
        # print(test)
        list(
            log2_trans = input$log2_trans,
            
            htmap_textsize_col = input$htmap_textsize_col,
            htmap_textangle_col = input$htmap_textangle_col,
            htmap_lab_row = input$htmap_lab_row,
            htmap_textsize_row = input$htmap_textsize_row,
            htmap_keysize = input$htmap_keysize,
            htmap_key_xlab = input$htmap_key_xlab,
            htmap_key_ylab = input$htmap_key_xlab,
            htmap_margin = paste0('c', input$htmap_margin),
            htmap_width = input$htmap_width,
            htmap_height = input$htmap_height,
            
            pca_scale_data = input$pca_scale_data,
            pca_center_data = input$pca_center_data,
            pca_pc = paste0('c', input$pca_pc),
            pca_biplot_samplelabel_type = input$pca_biplot_samplelabel_type,
            pca_biplot_samplelabel_size = input$pca_biplot_samplelabel_size,
            pca_biplot_symbol_size = input$pca_biplot_symbol_size,
            pca_biplot_ellipse = input$pca_biplot_ellipse,
            pca_biplot_ellipse_conf = input$pca_biplot_ellipse_conf,
            pca_biplot_loading = input$pca_biplot_loading,
            pca_biplot_loading_textsize = input$pca_biplot_loading_textsize,
            pca_biplot_multi_density = input$pca_biplot_multi_density,
            pca_biplot_multi_striplabel_size = input$pca_biplot_multi_striplabel_size,
            pca_rightside_y = input$pca_rightside_y,
            pca_x_tick_label_size = input$pca_x_tick_label_size,
            pca_y_tick_label_size = input$pca_y_tick_label_size,
            pca_width = input$pca_width,
            pca_height = input$pca_height,
            
            uni_fdr = input$uni_fdr,
            uni_alpha = input$uni_alpha,
            uni_fold_change = input$uni_fold_change,
            volcano_n_top_connections = input$volcano_n_top_connections,
            volcano_symbol_size = input$volcano_symbol_size,
            volcano_sig_colour = input$volcano_sig_colour,
            volcano_nonsig_colour = input$volcano_nonsig_colour,
            volcano_x_text_size = input$volcano_x_text_size,
            volcano_y_text_size = input$volcano_y_text_size,
            volcano_width = input$volcano_width,
            volcano_height = input$volcano_height,
            
            sig_htmap_textsize_col = input$sig_htmap_textsize_col,
            sig_htmap_textangle_col = input$sig_htmap_textangle_col,
            sig_htmap_textsize_row = input$sig_htmap_textsize_row,
            sig_htmap_keysize = input$sig_htmap_keysize,
            sig_htmap_key_xlab = input$sig_htmap_key_xlab,
            sig_htmap_key_ylab = input$sig_htmap_key_ylab,
            sig_htmap_margin = paste0('c', input$sig_htmap_margin),
            sig_htmap_width = input$sig_htmap_width,
            sig_htmap_height = input$sig_htmap_height,
            sig_pca_pc = paste0('c', input$sig_pca_pc),
            sig_pca_biplot_ellipse_conf = input$sig_pca_biplot_ellipse_conf,
            
            cpu_cluster = input$cpu_cluster,
            
            training_percentage = input$training_percentage,
            
            svm_cv_center_scale = input$svm_cv_center_scale,
            svm_cv_kernel = input$svm_cv_kernel,
            svm_cv_cross_k = input$svm_cv_cross_k,
            svm_cv_tune_method = input$svm_cv_tune_method,
            svm_cv_tune_cross_k = input$svm_cv_tune_cross_k,
            svm_cv_tune_boot_k = input$svm_cv_tune_boot_k,
            svm_cv_fs_rf_ifs_ntree = input$svm_cv_fs_rf_ifs_ntree,
            svm_cv_fs_rf_sfs_ntree = input$svm_cv_fs_rf_sfs_ntree,
            svm_cv_best_model_method = input$svm_cv_best_model_method,
            svm_cv_fs_count_cutoff = input$svm_cv_fs_count_cutoff,
            
            svm_cross_k = input$svm_cross_k,
            svm_tune_cross_k = input$svm_tune_cross_k,
            svm_tune_boot_k = input$svm_tune_boot_k,
            
            svm_perm_method = input$svm_perm_method,
            svm_perm_n = input$svm_perm_n,
            svm_perm_plot_symbol_size = input$svm_perm_plot_symbol_size,
            svm_perm_plot_legend_size = input$svm_perm_plot_legend_size,
            svm_perm_plot_x_label_size = input$svm_perm_plot_x_label_size,
            svm_perm_plot_x_tick_label_size = input$svm_perm_plot_x_tick_label_size,
            svm_perm_plot_y_label_size = input$svm_perm_plot_y_label_size,
            svm_perm_plot_y_tick_label_size = input$svm_perm_plot_y_tick_label_size,
            svm_perm_plot_width = input$svm_perm_plot_width,
            svm_perm_plot_height = input$svm_perm_plot_height,
            
            svm_roc_smooth = input$svm_roc_smooth,
            svm_roc_symbol_size = input$svm_roc_symbol_size,
            svm_roc_legend_size = input$svm_roc_legend_size,
            svm_roc_x_label_size = input$svm_roc_x_label_size,
            svm_roc_x_tick_label_size = input$svm_roc_x_tick_label_size,
            svm_roc_y_label_size = input$svm_roc_y_label_size,
            svm_roc_y_tick_label_size = input$svm_roc_y_tick_label_size,
            svm_roc_width = input$svm_roc_width,
            svm_roc_height = input$svm_roc_height,
            
            svm_rffs_pca_pc = paste0('c', input$svm_rffs_pca_pc),
            svm_rffs_pca_biplot_ellipse_conf = input$svm_rffs_pca_biplot_ellipse_conf,
            
            plsda_validation = input$plsda_validation,
            plsda_validation_segment = input$plsda_validation_segment,
            
            plsda_validation_segment = input$plsda_validation_segment,
            
            plsda_ncomp_select_method = input$plsda_ncomp_select_method,
            plsda_ncomp_select_plot_symbol_size = input$plsda_ncomp_select_plot_symbol_size,
            plsda_ncomp_select_plot_legend_size = input$plsda_ncomp_select_plot_legend_size,
            plsda_ncomp_select_plot_x_label_size = input$plsda_ncomp_select_plot_x_label_size,
            plsda_ncomp_select_plot_x_tick_label_size = input$plsda_ncomp_select_plot_x_tick_label_size,
            plsda_ncomp_select_plot_y_label_size = input$plsda_ncomp_select_plot_y_label_size,
            plsda_ncomp_select_plot_y_tick_label_size = input$plsda_ncomp_select_plot_y_tick_label_size,
            
            plsda_perm_method = input$plsda_perm_method,
            plsda_perm_n = input$plsda_perm_n,
            plsda_perm_plot_symbol_size = input$plsda_perm_plot_symbol_size,
            plsda_perm_plot_legend_size = input$plsda_perm_plot_legend_size,
            plsda_perm_plot_x_label_size = input$plsda_perm_plot_x_label_size,
            plsda_perm_plot_x_tick_label_size = input$plsda_perm_plot_x_tick_label_size,
            plsda_perm_plot_y_tick_label_size = input$plsda_perm_plot_y_tick_label_size,
            plsda_perm_plot_width = input$plsda_perm_plot_width,
            plsda_perm_plot_height = input$plsda_perm_plot_height,
            
            plsda_scoreplot_ellipse_conf = input$plsda_scoreplot_ellipse_conf,
            
            plsda_roc_smooth = input$plsda_roc_smooth,
            
            plsda_vip_alpha = input$plsda_vip_alpha,
            plsda_vip_boot = input$plsda_vip_boot,
            plsda_vip_boot_n = input$plsda_vip_boot_n,
            plsda_vip_plot_errorbar = input$plsda_vip_plot_errorbar,
            plsda_vip_plot_errorbar_width = input$plsda_vip_plot_errorbar_width,
            plsda_vip_plot_errorbar_label_size = input$plsda_vip_plot_errorbar_label_size,
            plsda_vip_plot_x_textangle = input$plsda_vip_plot_x_textangle,
            plsda_vip_plot_x_label_size = input$plsda_vip_plot_x_label_size,
            plsda_vip_plot_x_tick_label_size = input$plsda_vip_plot_x_tick_label_size,
            plsda_vip_plot_y_label_size = input$plsda_vip_plot_y_label_size,
            plsda_vip_plot_y_tick_label_size = input$plsda_vip_plot_y_tick_label_size,
            plsda_vip_plot_width = input$plsda_vip_plot_width,
            plsda_vip_plot_height = input$plsda_vip_plot_height
        )
    })
    
    output$dwnld_cnfg <- downloadHandler(
        filename = "configs",
        # config <- data.frame(varnames = rownames(t(configs())), values = t(configs())),
        content = function(file) {
            write.csv(config(), file = file, row.names = FALSE)
        }
    )
    
    
    output$result <- renderText(
        
        input$chosen_annot)
    
    output$training <- DT::renderDataTable({
        datatable(
            training()[[1, 3]][, , 1],
            colnames = NULL,
            options = list(
                autoWidth = TRUE,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollY = TRUE,
                searching = FALSE
            )
        ) %>%
            formatRound(c(1:90), 3)
    })
    
    output$annotation <- DT::renderDataTable({
        datatable(
            csvfiles()[[1, 1]],
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })
    
    output$node <- DT::renderDataTable({
        datatable(
            csvfiles()[[1, 2]],
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })
    
    observe(
        updateSelectizeInput(
            session,
            inputId = 'annot',
            choices = c(colnames(csvfiles()[[1, 1]]), colnames(csvfiles()[[1, 2]])),
            server = TRUE,
            selected = colnames(csvfiles()[[1, 1]][1:2])
        )
    )
    
    observe(
        updateSelectizeInput(
            session,
            inputId = 'node',
            choices = c(colnames(csvfiles()[[1, 1]]), colnames(csvfiles()[[1, 2]])),
            server = TRUE,
            selected = colnames(csvfiles()[[1, 2]][1:2])
        )
    )
    
    observeEvent(input$show_vars, {
        varnames <- c(input$annot, input$node)
        vars = "You chose: "
        for (i in 1:length(varnames)) {
            v = varnames[i]
            vars = paste0(vars, v, "; ")
        }
        showModal(
            modalDialog(
                title = "Loaded Variables",
                sprintf(vars),
                easyClose = FALSE,
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("continue", "Continue")
                )
            )
        )
    })
    
    raw_sample_dfm <- reactiveValues()
    
    observeEvent(input$continue, {
        matfile <- input$files$datapath[3]
        matfilenoext <-
            tools::file_path_sans_ext(input$files$name[3])
        annotfile <- input$files$datapath[1]
        samplesvar <- input$annot[1]
        groupvar <- input$annot[2]
        outdir <- getwd()     #not actually being used
        proc_data <- inputDatProcess(
            c(
                "y",
                "d",
                "c",
                "a",
                "s",
                matfile,
                matfilenoext,
                annotfile,
                samplesvar,
                groupvar,
                outdir
            )
        )
        
        raw_sample_dfm$raw_sample_dfm <- proc_data[1]
        raw_sample_dfm$raw_sample_dfm_wo_uni <- proc_data[2]
        removeModal()
        
        
        insertUI(
            selector = "#configfilebutton",
            where = "afterEnd",
            ui = fileInput(
                inputId = "config",
                label = "Upload Config File",
                multiple = FALSE
            )
        )
        
        
    })
    
    configurations <- reactive({
        req(input$config)
        configs <- defaultConfig(input$config$datapath)
    })
    
    output$configfile <- renderTable({
        req(input$config)
        configurations()[1]
        
    },
    rownames = TRUE)
    
    observeEvent(input$univariate, {
        req(input$files)
        
        matfilenoext <-
            tools::file_path_sans_ext(input$files$name[3])
        dat_file <- paste0(getwd(), "/", matfilenoext, "_2D.csv")
        nodefile <- input$files$datapath[2]
        outdir <- getwd()    #should be changed
        configs <- configurations()[2][[1]]
        contrast <- "ptsd - control"
        node_id_var <- input$node[1]
        region_name_var <- input$node[2]
        inputargs <- c(
            "y",
            "d",
            "c",
            "a",
            "s",
            dat_file,
            matfilenoext,
            nodefile,
            outdir,
            configs[1:28],
            contrast,
            configs[29:50],
            node_id_var,
            region_name_var
        )
        
        
        withConsoleRedirect("console", {
            # invalidateLater(100)
            univariate(inputargs)
        })
        
        
    })
    
    observeEvent(input$svm, {
        req(input$files)
        
        matfilenoext <-
            tools::file_path_sans_ext(input$files$name[3])
        dat_file <- paste0(getwd(), "/", matfilenoext, "_ml.csv")
        nodefile <- input$files$datapath[2]
        outdir <- getwd()    #should be changed
        configs <- configurations()[2][[1]]
        contrast <- "ptsd - control"
        psetting <- 3
        cores <- 2
        cvuni <- FALSE
        inputargs <- c(
            "y",
            "d",
            "c",
            "a",
            "s",
            dat_file,
            matfilenoext,
            outdir,
            psetting,
            cores,
            configs[51:84],
            configs[12:13],
            configs[15:18],
            configs[20:28],
            configs[85:86],
            cvuni,
            configs[1],
            configs[29:30]
            
            
        )
        
        
        ml_svm(inputargs)
        
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
