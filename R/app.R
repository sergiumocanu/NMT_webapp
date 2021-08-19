#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinyjqui)
library(shinybusy)
library(R.matlab)
library(plyr)
library(DT)
library(shinyBS)
library(sortable)
library(data.table)
library(pdftools)
library(rmarkdown)
library(zip)
library(brainconn)
library(plotly)
library(parallel)


file.sources = list.files(c("./backend", "./utils"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)


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

icon_list <- function(x) {
    lapply(x,
           function(x) {
               tags$div(# icon("arrows-alt-h"),

                   class = "well well-sm",
                   tags$strong(x),
                   tags$style(
                       HTML(
                           '
                                .well:hover {
                                background-color: #ddeaf6;
                                cursor: grab;
                                }
                                .well {
                                margin-bottom: 0px;
                                }
                                .well:active{
                                border-color: #000000;
                                cursor: grabbing;
                                }
                                '
                       )
                   ))
           })
}

# Define UI for application 
ui <- dashboardPage(
    
    
    
    # Header in top left of window
    dashboardHeader(title = "Neuro-ML-Tools (NMT)"),
    
    # sidebar menu items
    dashboardSidebar(sidebarMenu(
        menuItem(text = "Classification Analysis", tabName = "svm"),
        menuItem(text = "Regression Analysis", tabName = "regression"),
        menuItem(text = "CV-Only Classification Analysis", tabName = "cv"),
        menuItem(text = "CV-Only Regression Analysis", tabName = "cv_regression"),
        menuItem(text = "Prediction", tabName = "prediction"),
        menuItem(text = "References", tabName = "references")
    )),
    
    
    
    # Dashboard body items
    dashboardBody(tabItems(
        tabItem(
            
            tabName = "svm",
            
            fluidRow(uiOutput("variables")),
            fluidRow(uiOutput("groups")),
            fluidRow(
                uploadFiles("file_data", "file_annotations", "file_node"),
                checklist(
                    "upload_check",
                    "variable_check",
                    "contrast_check",
                    "inputdata_check",
                    "univariate_check",
                    "svm_check"
                )
            ),
            fluidRow(
                viewData("training", "annotation", "node"),
                inputConfigurations()
            ),
            
            fluidRow(
                executeSVM("univariate", "svm", "modeltype", "noneset", "kset", "uset"),
                consoleOutput("console")
            ),
            
            fluidRow(
                results(
                    "results",
                    "select_uni_plot",
                    "select_svm_plot",
                    "uni_plots",
                    "svm_plots",
                    "report",
                    "allfiles"
                )
            ),
            # fluidRow(predictUI("regular")),
            plotlyOutput("brainConnectivity")
            
        ),
        tabItem(
            tabName = "regression",
            # fluidRow(verbatimTextOutput("reg_debug")),
            fluidRow(uiOutput("reg_variables")),
            fluidRow(uiOutput("reg_groups")),
            fluidRow(
                uploadFiles("reg_file_data", "reg_file_annotations", "reg_file_node"),
                checklist(
                    "reg_upload_check",
                    "reg_variable_check",
                    "reg_contrast_check",
                    "reg_inputdata_check",
                    "reg_univariate_check",
                    "reg_svm_check"
                )
            ),
            
            fluidRow(
                viewData("reg_training", "reg_annotation", "reg_node"),
                reg_inputConfigurations()
            ),
            
            fluidRow(
                executeSVM(
                    "reg_univariate",
                    "reg_svm",
                    "reg_modeltype",
                    "reg_noneset",
                    "reg_kset",
                    "reg_uset"
                ),
                consoleOutput("reg_console")
            ),
            
            fluidRow(
                results(
                    "reg_results",
                    "reg_select_uni_plot",
                    "reg_select_svm_plot",
                    "reg_uni_plots",
                    "reg_svm_plots",
                    "reg_report",
                    "reg_allfiles"
                )
            )
        ),
        tabItem(
            tabName = "cv",
            # fluidRow(verbatimTextOutput("reg_debug")),
            fluidRow(uiOutput("cv_variables")),
            fluidRow(uiOutput("cv_groups")),
            fluidRow(
                uploadFiles("cv_file_data", "cv_file_annotations", "cv_file_node"),
                checklist(
                    "cv_upload_check",
                    "cv_variable_check",
                    "cv_contrast_check",
                    "cv_inputdata_check",
                    "cv_univariate_check",
                    "cv_svm_check"
                )
            ),
            
            fluidRow(
                viewData("cv_training", "cv_annotation", "cv_node"),
                cv_inputConfigurations()
            ),
            
            fluidRow(
                executeSVM(
                    "cv_univariate",
                    "cv_svm",
                    "cv_modeltype",
                    "cv_noneset",
                    "cv_kset",
                    "cv_uset"
                ),
                consoleOutput("cv_console")
            ),
            
            fluidRow(
                results(
                    "cv_results",
                    "cv_select_uni_plot",
                    "cv_select_svm_plot",
                    "cv_uni_plots",
                    "cv_svm_plots",
                    "cv_report",
                    "cv_allfiles"
                )
            )
            
            # fluidRow(
            #     box(
            #         status = "primary",
            #         solidHeader = TRUE,
            #         collapsible = TRUE,
            #         title = "Interactive Plots",
            #         width = 6
            #         # plotlyOutput('reg_plot')
            #     )
            # )
            
            
        ),
        tabItem(
            tabName = "cv_regression",
            # fluidRow(verbatimTextOutput("reg_debug")),
            fluidRow(uiOutput("cv_reg_variables")),
            fluidRow(uiOutput("cv_reg_groups")),
            fluidRow(
                uploadFiles("cv_reg_file_data", "cv_reg_file_annotations", "cv_reg_file_node"),
                checklist(
                    "cv_reg_upload_check",
                    "cv_reg_variable_check",
                    "cv_reg_contrast_check",
                    "cv_reg_inputdata_check",
                    "cv_reg_univariate_check",
                    "cv_reg_svm_check"
                )
            ),

            fluidRow(
                viewData("cv_reg_training", "cv_reg_annotation", "cv_reg_node"),
                cv_reg_inputConfigurations()
            ),

            fluidRow(
                executeSVM(
                    "cv_reg_univariate",
                    "cv_reg_svm",
                    "cv_reg_modeltype",
                    "cv_reg_noneset",
                    "cv_reg_kset",
                    "cv_reg_uset"
                ),
                consoleOutput("cv_reg_console")
            ),

            fluidRow(
                results(
                    "cv_reg_results",
                    "cv_reg_select_uni_plot",
                    "cv_reg_select_svm_plot",
                    "cv_reg_uni_plots",
                    "cv_reg_svm_plots",
                    "cv_reg_report",
                    "cv_reg_allfiles"
                )
            )

            # fluidRow(
            #     box(
            #         status = "primary",
            #         solidHeader = TRUE,
            #         collapsible = TRUE,
            #         title = "Interactive Plots",
            #         width = 6
            #         # plotlyOutput('reg_plot')
            #     )
            # )


        ),
        
        tabItem(
            tabName = "prediction",
            fluidRow(predictUI("regular"))
        ),
        
        tabItem(
            tabName = "references",
            h1("If you use our application please cite the following papers: "),
            h3("Version 0.1.2"),
            p("Zhang J, Richardson DJ, Dunkley BT. 2020. Classifying post-traumatic stress disorder using the magnetoencephalographic connectome and machine learning. Scientific Reports. 10(1): 5937. doi: 10.1038/s41598-020-62713-5"),
            p("Zhang J, Hadj-Moussa H, Storey KB. 2016. Current progress of high-throughput microRNA differential expression analysis and random forest gene selection for model and non-model systems: an R implementation. J Integr Bioinform. 13: 306. doi: 10.1515/jib-2016-306")
            
        )
    ))
)



# =============================================================================
# Define server logic
server <- function(input, output, session) {
    
    options(shiny.maxRequestSize=50*1024^2)
    
    observeEvent(input$file_data, {
        enable("upload_check")
        updatePrettyCheckbox(session = session,
                             inputId = "upload_check",
                             value = TRUE)
        
    })
    

    # loading data file .mat/.csv
    trainingData <- reactive({
        req(input$file_data)
        xt <- tools::file_ext(input$file_data$datapath)
        if (xt == "mat") {
            df <- readMat(input$file_data$datapath)
            df <- df[[1]]
            df <- df[, , 1]
        } else if (xt == "csv") {
            df <- read.csv(input$file_data$datapath,
                           header = TRUE,
                           sep = ",")
            df
        }
        
    })
    
    # getting extension used to determine file input type
    xt <- reactive({
        tools::file_ext(input$file_data$datapath)
    })

    # getting annotations
    annotationsData <- reactive({
        req(input$file_data)
        xt <<- tools::file_ext(input$file_data$datapath)
        if (xt == "csv") {
            req(input$file_data)
            print("annotations 2D data!!")
            df <- matrix(trainingData()[, input$training_columns_selected])
        } else if (xt == "mat") {
            req(input$file_annotations)
            df <- read.csv(input$file_annotations$datapath,
                           header = TRUE,
                           sep = ",",
                           fileEncoding = "UTF-8-BOM")
        }
    })
    

    nodeData <- reactive({
        req(input$file_data)
        xt <<- tools::file_ext(input$file_data$datapath)
        if (xt == "csv") {
            print("node 2D data!!!!")
        } else if (xt == "mat") {
            req(input$file_node)
            df <- read.csv(input$file_node$datapath,
                           header = TRUE,
                           sep = ",",
                           fileEncoding = "UTF-8-BOM")
        }
    })
    
    
    output$training <- DT::renderDataTable({
        req(input$file_data)
        datatable(
            trainingData(),
            caption = "If your data is a 2D CSV, click on the column with annotations",
            options = list(
                autoWidth = TRUE,
                lengthChange = FALSE,
                scrollY = TRUE,
                searching = FALSE,
                scrollX = TRUE,
                paging = TRUE,
                pageLength = 10
            ),
            selection = list(target = 'column')
        )
        
    })
    
    output$annotation <- DT::renderDataTable({
        req(input$file_data)
        datatable(
            annotationsData(),
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollY = TRUE,
                searching = FALSE
            )
        )
        
    })
    
    output$node <- DT::renderDataTable({
        req(input$file_data)
        xt <<- tools::file_ext(input$file_data$datapath)
        if(xt == "csv"){

        } else if (xt == "mat") {
            datatable(
                nodeData(),
                options = list(
                    autowidth = TRUE,
                    pageLength = 10,
                    lengthChange = FALSE,
                    scrollX = TRUE,
                    scrollY = TRUE,
                    searching = FALSE
                )
            )
        }
    })
    


    # rendering the drag and drop with variable names
    output$variables_csv <- DT::renderDataTable({
        d <- trainingData()
        
        labs <- c(colnames(d))
        datatable(
            matrix(labs),
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
        
    })
    
    output$samplevar <- renderText({
        d <- trainingData()
        paste("Selected Sample Variable: ", colnames(d)[input$variables_csv_rows_selected[1]])
    })
    output$groupvar <- renderText({
        d <- trainingData()
        paste("Selected Group Variable: ", colnames(d)[input$variables_csv_rows_selected[2]])
    })
    output$variables <- renderUI({
        req(input$confirmcontrast)
        xt <<- tools::file_ext(input$file_data$datapath)
        print(xt)
        if(xt == "csv"){
                    
            box(
                title = "File Variables from column names",
                status = "primary",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                dataTableOutput("variables_csv"),
                verbatimTextOutput("samplevar"),
                verbatimTextOutput("groupvar"),
                actionButton(inputId = "choose_vars", label = "Confirm Variables")
            )

        } else if(xt == "mat"){
            print("file is not 2d")

            req(input$file_annotations, input$file_node)
                    labs <-
                        c(colnames(annotationsData()), colnames(nodeData()))

                    useShinyjs()
                    tagList(
                        box(
                            title = "File Variables",
                            status = "primary",
                            width = 12,
                            collapsible = TRUE,
                            solidHeader = TRUE,


                            fluidRow(
                                class = "panel-body",
                                column(
                                    width = 4,
                                    tags$div(
                                        class = "panel panel-default",
                                        tags$div(class = "panel-heading",
                                                 "File Variables"),
                                        tags$div(class = "panel-body av_vars_body",
                                                 id = "availablevars",
                                                 icon_list(labs)),
                                        tags$style(HTML('
                                        '))
                                    )
                                ),
                                column(
                                    width = 4,
                                    tags$div(
                                        class = "panel panel-default",
                                        tags$div(class = "panel-heading",
                                                 "Sample"),
                                        tags$div(class = "panel-body",
                                                 id = "chosen_sample")
                                    ),
                                    tags$div(
                                        class = "panel panel-default",
                                        tags$div(class = "panel-heading",
                                                 "Target"),
                                        tags$div(class = "panel-body",
                                                 id = "chosen_group")
                                    )
                                ),
                                column(
                                    width = 4,
                                    tags$div(
                                        class = "panel panel-default",
                                        tags$div(class = "panel-heading",
                                                 "Node ID"),
                                        tags$div(class = "panel-body",
                                                 id = "chosen_node")
                                    ),
                                    tags$div(
                                        class = "panel panel-default",
                                        tags$div(class = "panel-heading",
                                                 "Region"),
                                        tags$div(class = "panel-body",
                                                 id = "chosen_region")
                                    )
                                ),
                                #################### sortable code for drag and drop
                                sortable_js(
                                    "availablevars",
                                    options = sortable_options(
                                        group = list(pull = TRUE,
                                                     name = "allvars",
                                                     put = TRUE),
                                        onSort = sortable_js_capture_input("sort_vars")
                                    )

                                ),
                                sortable_js(
                                    "chosen_sample",
                                    options = sortable_options(
                                        group = list(
                                            group = "allvars",
                                            put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                            pull = htmlwidgets::JS(
                                                'function (to) { document.getElementById("chosen_sample").style.backgroundColor = "white"; }'
                                            )
                                        ),
                                        swapClass = "sortable-swap-highlight",
                                        onSort = sortable_js_capture_input("chosen_sample")
                                    )
                                ),
                                sortable_js(
                                    "chosen_group",
                                    options = sortable_options(
                                        group = list(
                                            group = "allvars",
                                            put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                            pull = htmlwidgets::JS(
                                                'function (to) { document.getElementById("chosen_group").style.backgroundColor = "white"; }'
                                            )
                                        ),
                                        swapClass = "sortable-swap-highlight",
                                        onSort = sortable_js_capture_input("chosen_group")
                                        # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
                                    )
                                ),
                                sortable_js(
                                    "chosen_node",
                                    options = sortable_options(
                                        group = list(
                                            group = "allvars",
                                            put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                            pull = htmlwidgets::JS(
                                                'function (to) { document.getElementById("chosen_node").style.backgroundColor = "white"; }'
                                            )
                                        ),
                                        swapClass = "sortable-swap-highlight",
                                        onSort = sortable_js_capture_input("chosen_node")
                                    )
                                ),
                                sortable_js(
                                    "chosen_region",
                                    options = sortable_options(
                                        group = list(
                                            group = "allvars",
                                            put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                            pull = htmlwidgets::JS(
                                                'function (to) { document.getElementById("chosen_region").style.backgroundColor = "white"; }'
                                            )
                                        ),
                                        swapClass = "sortable-swap-highlight",
                                        onSort = sortable_js_capture_input("chosen_region")
                                    )
                                )
                            ),
                            actionButton(inputId = "choose_vars",
                                         label = "Confirm Variables")
                        )


                    )

        } else {
            print("wrong input file")
        }

    })
        

    output$groups <- renderUI({
        req(input$file_data)
        xt <<- tools::file_ext(input$file_data$datapath)
        if(xt == "csv"){
            req(input$training_columns_selected)
            print("contrast !! file 2d!")
            
            d <- read.csv(input$file_data$datapath,
                          header = TRUE,
                          sep = ",")
            d <- matrix(d[, input$training_columns_selected])
            # print(d)
            
            groups <- data.frame(d)
            groups <- count(groups$group)
            groupnames <- unique(d)
            groupcount <- groups$freq
            numgroups <- length(groups)
            print("group names")
            print(groupnames)
            print(unique(d))
            useShinyjs()
            tagList(
                box(
                    title = "Contrast variables",
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    fluidRow(
                        class = "panel-body",
                        column(
                            width = 3,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group Variables"),
                                tags$div(
                                    class = "panel-body",
                                    id = "availablegroups",
                                    icon_list(groupnames)
                                ),
                                tags$style(HTML('
                                '))
                            )
                        ),
                        column(
                            width = 3,
                            tags$div(
                                id = "contrastgroup1",
                                class = "panel panel-default contrast1",
                                tags$div(class = "panel-heading",
                                         "Group 1"),
                                tags$div(class = "panel-body",
                                         id = "contrast1")
                            )
                        ),
                        column(width = 1,
                               tags$div(class = "h1",
                                        "VS.")),
                        column(
                            width = 3,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group 2"),
                                tags$div(class = "panel-body",
                                         id = "contrast2")
                            )
                        ),

                        column(
                            width = 2,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         icon("trash"),
                                         "Bin item"),
                                tags$div(class = "panel-body",
                                         id = "trashbin")
                            )
                        )
                    ),
                    sortable_js(
                        "availablegroups",
                        options = sortable_options(
                            group = list(
                                pull = "clone",
                                name = "allsorts",
                                put = FALSE
                            ),
                            onSort = sortable_js_capture_input("sort_vars")
                        )
                    ),
                    sortable_js(
                        "contrast1",
                        options = sortable_options(
                            group = list(
                                group = "allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = htmlwidgets::JS('function (to) { var count = contrast1.children.length; console.log(count);  var c = contrast1.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";
                                                 }
                                                 console.log("yeeeeeehehehrerhq;lrehq;werh");

                                                 }}'),
                            onAdd = sortable_js_capture_input("contrast_group1")
                        )

                    ),
                    sortable_js(
                        "contrast2",
                        options = sortable_options(
                            group = list(
                                group = "allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = htmlwidgets::JS('function (to) { var count = contrast2.children.length; console.log(count);  var c = contrast2.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";

                                                 }
                                                 console.log("yeeeeeehehehrerhq;lrehq;werh");

                                                 }}'),
                            onAdd = sortable_js_capture_input("contrast_group2")
                        )

                    ),
                    sortable_js(
                        "trashbin",
                        options = sortable_options(
                            group = list(
                                group = "allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
                        )
                    ),
                    actionButton(inputId = "confirmcontrast", label = "Confirm")
                )
            )
        }else{
            req(input$file_annotations, input$file_node)
        groups <- data.frame(annotationsData())
        groups <- count(groups$group)
        groupnames <- unique(groups$x)
        groupcount <- groups$freq
        numgroups <- length(groups)
        useShinyjs()
        tagList(
            box(
                title = "Contrast variables",
                status = "primary",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                fluidRow(
                    class = "panel-body",
                    column(
                        width = 3,
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "Group Variables"),
                            tags$div(
                                class = "panel-body",
                                id = "availablegroups",
                                icon_list(groupnames)
                            ),
                            tags$style(HTML('
                                '))
                        )
                    ),
                    column(
                        width = 3,
                        tags$div(
                            id = "contrastgroup1",
                            class = "panel panel-default contrast1",
                            tags$div(class = "panel-heading",
                                     "Group 1"),
                            tags$div(class = "panel-body",
                                     id = "contrast1")
                        )
                    ),
                    column(width = 1,
                           tags$div(class = "h1",
                                    "VS.")),
                    column(
                        width = 3,
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "Group 2"),
                            tags$div(class = "panel-body",
                                     id = "contrast2")
                        )
                    ),

                    column(
                        width = 2,
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     icon("trash"),
                                     "Bin item"),
                            tags$div(class = "panel-body",
                                     id = "trashbin")
                        )
                    )
                ),
                sortable_js(
                    "availablegroups",
                    options = sortable_options(
                        group = list(
                            pull = "clone",
                            name = "allsorts",
                            put = FALSE
                        ),
                        onSort = sortable_js_capture_input("sort_vars")
                    )
                ),
                sortable_js(
                    "contrast1",
                    options = sortable_options(
                        group = list(
                            group = "allsorts",
                            put = TRUE,
                            pull = TRUE
                        ),
                        swapClass = "sortable-swap-highlight",
                        onSort = htmlwidgets::JS('function (to) { var count = contrast1.children.length; console.log(count);  var c = contrast1.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";
                                                 }
                                                 console.log("yeeeeeehehehrerhq;lrehq;werh");

                                                 }}'),
                        onAdd = sortable_js_capture_input("contrast_group1")
                    )

                ),
                sortable_js(
                    "contrast2",
                    options = sortable_options(
                        group = list(
                            group = "allsorts",
                            put = TRUE,
                            pull = TRUE
                        ),
                        swapClass = "sortable-swap-highlight",
                        onSort = htmlwidgets::JS('function (to) { var count = contrast2.children.length; console.log(count);  var c = contrast2.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";

                                                 }
                                                 }}'),
                        onAdd = sortable_js_capture_input("contrast_group2")
                    )

                ),
                sortable_js(
                    "trashbin",
                    options = sortable_options(
                        group = list(
                            group = "allsorts",
                            put = TRUE,
                            pull = TRUE
                        ),
                        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
                    )
                ),
                actionButton(inputId = "confirmcontrast", label = "Confirm")
            )
        )
        }




    })

    # getting configurations from input fields (defaults)
    inputConfigs <- reactive({
        configs <- list(
            random_state = input$random_state,

            log2_trans = input$log2_trans,

            htmap_textsize_col = input$htmap_textsize_col,
            htmap_textangle_col = input$htmap_textangle_col,
            htmap_lab_row = input$htmap_lab_row,
            htmap_textsize_row = input$htmap_textsize_row,
            htmap_keysize = input$htmap_keysize,
            htmap_key_xlab = paste0('"', input$htmap_key_xlab, '"'),
            htmap_key_ylab =  paste0('"', input$htmap_key_ylab, '"'),
            htmap_margin = paste0('"c', input$htmap_margin, '"'),
            htmap_width = input$htmap_width,
            htmap_height = input$htmap_height,

            pca_scale_data = input$pca_scale_data,
            pca_center_data = input$pca_center_data,
            pca_pc = paste0('"c', input$pca_pc, '"'),
            pca_biplot_samplelabel_type = paste0('"', input$pca_biplot_samplelabel_type, '"'),
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
            volcano_sig_colour = paste0('"', input$volcano_sig_colour, '"'),
            volcano_nonsig_colour = paste0('"', input$volcano_nonsig_colour, '"'),
            volcano_x_text_size = input$volcano_x_text_size,
            volcano_y_text_size = input$volcano_y_text_size,
            volcano_width = input$volcano_width,
            volcano_height = input$volcano_height,

            sig_htmap_textsize_col = input$sig_htmap_textsize_col,
            sig_htmap_textangle_col = input$sig_htmap_textangle_col,
            sig_htmap_textsize_row = input$sig_htmap_textsize_row,
            sig_htmap_keysize = input$sig_htmap_keysize,
            sig_htmap_key_xlab = paste0('"', input$sig_htmap_key_xlab, '"'),
            sig_htmap_key_ylab = paste0('"', input$sig_htmap_key_ylab, '"'),
            sig_htmap_margin = paste0('"c', input$sig_htmap_margin, '"'),
            sig_htmap_width = input$sig_htmap_width,
            sig_htmap_height = input$sig_htmap_height,
            sig_pca_pc = paste0('"c', input$sig_pca_pc, '"'),
            sig_pca_biplot_ellipse_conf = input$sig_pca_biplot_ellipse_conf,

            cpu_cluster = paste0('"', input$cpu_cluster, '"'),

            training_percentage = input$training_percentage,

            svm_cv_center_scale = input$svm_cv_center_scale,
            svm_cv_kernel = paste0('"', input$svm_cv_kernel, '"'),
            svm_cv_cross_k = input$svm_cv_cross_k,
            svm_cv_tune_method = paste0('"', input$svm_cv_tune_method, '"'),
            svm_cv_tune_cross_k = input$svm_cv_tune_cross_k,
            svm_cv_tune_boot_n = input$svm_cv_tune_boot_n,
            svm_cv_fs_rf_ifs_ntree = input$svm_cv_fs_rf_ifs_ntree,
            svm_cv_fs_rf_sfs_ntree = input$svm_cv_fs_rf_sfs_ntree,
            svm_cv_best_model_method = paste0('"', input$svm_cv_best_model_method, '"'),
            svm_cv_fs_count_cutoff = input$svm_cv_fs_count_cutoff,

            svm_cross_k = input$svm_cross_k,
            svm_tune_cross_k = input$svm_tune_cross_k,

            svm_tune_boot_n = input$svm_tune_boot_n,

            svm_perm_method = paste0('"', input$svm_perm_method, '"'),
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

            svm_rffs_pca_pc = paste0('"c', input$svm_rffs_pca_pc, '"'),
            svm_rffs_pca_biplot_ellipse_conf = input$svm_rffs_pca_biplot_ellipse_conf,

            rffs_htmap_textsize_col = input$rffs_htmap_textsize_col,
            rffs_htmap_textangle_col = input$rffs_htmap_textangle_col,
            rffs_htmap_textsize_row = input$rffs_htmap_textsize_row,
            rffs_htmap_keysize = input$rffs_htmap_keysize,
            rffs_htmap_key_xlab = input$rffs_htmap_key_xlab,
            rffs_htmap_key_ylab = input$rffs_htmap_key_ylab,
            rffs_htmap_margin = paste0('"c', input$rffs_htmap_margin, '"'),
            rffs_htmap_width = input$rffs_htmap_width,
            rffs_htmap_height = input$rffs_htmap_height,

            plsda_validation = paste0('"', input$plsda_validation, '"'),
            plsda_validation_segment = input$plsda_validation_segment,

            plsda_init_ncomp = input$plsda_init_ncomp,

            plsda_ncomp_select_method = paste0('"', input$plsda_ncomp_select_method, '"'),
            plsda_ncomp_select_plot_symbol_size = input$plsda_ncomp_select_plot_symbol_size,
            plsda_ncomp_select_plot_legend_size = input$plsda_ncomp_select_plot_legend_size,
            plsda_ncomp_select_plot_x_label_size = input$plsda_ncomp_select_plot_x_label_size,
            plsda_ncomp_select_plot_x_tick_label_size = input$plsda_ncomp_select_plot_x_tick_label_size,
            plsda_ncomp_select_plot_y_label_size = input$plsda_ncomp_select_plot_y_label_size,
            plsda_ncomp_select_plot_y_tick_label_size = input$plsda_ncomp_select_plot_y_tick_label_size,

            plsda_perm_method = paste0('"', input$plsda_perm_method, '"'),
            plsda_perm_n = input$plsda_perm_n,
            plsda_perm_plot_symbol_size = input$plsda_perm_plot_symbol_size,
            plsda_perm_plot_legend_size = input$plsda_perm_plot_legend_size,
            plsda_perm_plot_x_label_size = input$plsda_perm_plot_x_label_size,
            plsda_perm_plot_x_tick_label_size = input$plsda_perm_plot_x_tick_label_size,
            plsda_perm_plot_y_label_size = input$plsda_perm_plot_y_label_size,
            plsda_perm_plot_y_tick_label_size = input$plsda_perm_plot_y_tick_label_size,
            plsda_perm_plot_width = input$plsda_perm_plot_width,
            plsda_perm_plot_height = input$plsda_perm_plot_height,

            plsda_scoreplot_ellipse_conf = input$plsda_scoreplot_ellipse_conf,

            plsda_roc_smooth = input$plsda_roc_smooth,

            plsda_vip_alpha = input$plsda_vip_alpha,
            plsda_vip_boot = input$plsda_vip_boot,
            plsda_vip_boot_n = input$plsda_vip_boot_n,
            plsda_vip_plot_errorbar = paste0('"', input$plsda_vip_plot_errorbar, '"'),
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
        # configs_format(configs)
    })


    downloadConfigsData <- reactive({
        configs <- configs_format(inputConfigs())
    })

    output$download_configs <- downloadHandler(
        # configs <- configs_format(inputConfigs()),
        filename = "configs",
        content = function(file) {
            write.table(
                downloadConfigsData(),
                file = file,
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE
            )
            # file.copy(file.path(tempdir(),'OUTPUT','configs'), file)
        },
        contentType = "application"
    )

    observeEvent(input$reset_cnfg, {
        shinyjs::reset("configurations")
    })


    output$debug <-
        renderText(paste0("Debug: ", input$contrast_group1[2]))

    
    contrast <- reactiveValues()

    observeEvent(input$confirmcontrast, {
        #check that they're the same length
        contrast1 <- input$contrast_group1
        contrast2 <- input$contrast_group2
        output <- 0
        if (length(contrast1) != length(contrast2)) {
            shinyalert(
                title = "Unequal groups!",
                type = "warning",
                text = paste(
                    "Group 1:",
                    length(contrast1),
                    "items; Group 2:",
                    length(contrast2),
                    "items."
                )
            )
        } else if (length(contrast1) == 0 | length(contrast2) == 0) {
            shinyalert(
                title = "No contrast variables chosen!",
                type = "warning",
                text = paste(
                    "Please choose at least one contrast variable per group."
                )
            )
        } else {
            for (i in 1:length(contrast1)) {
                output[i] <- paste0(contrast1[i], " - ", contrast2[i])
            }
            output <- paste(output, collapse = ',', sep = '')
            contrast$groups <- output

        }
        
        enable("contrast_check")
        updatePrettyCheckbox(session = session,
                             inputId = "contrast_check",
                             value = TRUE)

    })


    observeEvent(input$choose_vars, {
        if(xt == "csv"){
            varNames <- c(colnames(trainingData())[input$variables_csv_rows_selected[1]], colnames(trainingData())[input$variables_csv_rows_selected[2]])
        } else if (xt == "mat") {
            varNames <- c(input$chosen_sample, input$chosen_group, input$chosen_node, input$chosen_region)
        }
        
        # varNames <- names(variableNames)
        print(varNames)
        vars = "You chose: "
        for (i in 1:length(varNames)) {
            v = varNames[i]
            vars = paste0(vars, v, "; ", sep = "", collapse = NULL)
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
        print(contrast$groups[1])
        enable("variable_check")
        updatePrettyCheckbox(session = session,
                             inputId = "variable_check",
                             value = TRUE)
    })

    raw_sample_dfm <- reactiveValues()

    # clicking the modal "Continue Button"
    observeEvent(input$continue, {
        matfile <- input$file_data$datapath
        matfilenoext <-
            tools::file_path_sans_ext(input$file_data$name)
        dir.create(file.path(tempdir(), '/OUTPUT'), showWarnings = FALSE)
        
        if (xt == "csv") {
            samplesvar <- colnames(trainingData())[input$variables_csv_rows_selected[1]]
            groupvar <- colnames(trainingData())[input$variables_csv_rows_selected[2]]
        } else if (xt == "mat") {
            annotfile <- input$file_annotations$datapath
            samplesvar <- input$chosen_sample
            groupvar <- input$chosen_group
        }
        
        outdir <-
            paste0(tempdir(), '/OUTPUT')
        
        if (xt == "csv") {
            print("2D file")
            proc_data <- inputDatProcess2d(
                c(
                    "y",
                    "d",
                    "c",
                    "a",
                    "s",
                    matfile,
                    matfilenoext,
                    samplesvar,
                    groupvar,
                    outdir
                )
            )
        } else if (xt == "mat") {
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
            
        }
        
        
        raw_sample_dfm$raw_sample_dfm <- proc_data[1]
        raw_sample_dfm$raw_sample_dfm_wo_uni <- proc_data[2]
        removeModal()

        enable("inputdata_check")
        updatePrettyCheckbox(session = session,
                             inputId = "inputdata_check",
                             value = TRUE)
        
        runjs('document.getElementById(\"univariate\").scrollIntoView(true)')
        enable("univariate")
    })

    # upload configurations from file
    uploaded_configs <- reactive({
        req(input$up_configs)
        configs <- getConfigsFromFile(input$up_configs$datapath)

        if (length(configs) == 4) {
            output$result <-
                renderText(paste0("missing variables: ", configs[4][[1]]))
            shinyalert(
                title = "Missing Variables, Choose from dialog",
                type = "warning",
                text = sprintf(configs[4][[1]])
            )
        }
        return(configs)
    })


    observeEvent(input$up_configs, {
        req(input$up_configs)
        configs <-
            uploaded_configs()[1][[1]]   #column 1 is key/value pair, column 2 is values, column 3 is variable names
        configFileToUI(configs, session)
        # updateTextInput(session=session, inputId = "htmap_textsize_col", value = configs[2])
        # print(configs[2])
    })

    # Univariate button
    observeEvent(input$univariate, {
        if(xt == "mat") {
        req(input$file_data, input$file_annotations, input$file_node)}
        else if (xt == "csv") { req(input$file_data)}

        do.call(file.remove, list(list.files(
            file.path(tempdir(), 'OUTPUT', 'UNIVARIATE'),
            full.names = TRUE, recursive = TRUE
        )))

        matfilenoext <-
            tools::file_path_sans_ext(input$file_data$name)
        dat_file <-
            paste0(tempdir(), "/OUTPUT/", matfilenoext, "_2D.csv")

        nodefile <- input$file_node$datapath
        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'UNIVARIATE'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'UNIVARIATE', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            file.path(tempdir(), 'OUTPUT', 'UNIVARIATE')
        # configs <- uploaded_configs()[2][[1]]
        print(contrast$groups)
        contrast <- contrast$groups
        node_id_var <- input$chosen_node
        region_name_var <- input$chosen_region

        # getting values from input fields into an args[] fromat
        inputArgs <- univariateConfigs(inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[9] <- outdir
        inputArgs[38] <- contrast
        
        if(xt == "mat"){
            inputArgs[8] <- nodefile
            inputArgs[61] <- node_id_var
            inputArgs[62] <- region_name_var
        }
        
        show_modal_spinner(text = "Processing...")
        tryCatch({
            withConsoleRedirect("console", {
                if (xt == "mat") {
                    univariate(inputArgs)
                } else if (xt == "csv") {
                    suppressWarnings(univariate_2D(inputArgs))
                }
                
            })
        },
        error = function(e){
            withConsoleRedirect("console", {
                cat("No significant features detected, univariate feature reduction cannot be performed. 
                    If running the SVM classifier, choose the option: Do not incorporate univariate prior knowledge during SVM analysis. 
                    Note: this will take significantly longer to classify.")
            })
        })
        
        remove_modal_spinner()
        # setwd(tempdir())
        
        enable("univariate_check")
        updatePrettyCheckbox(session = session,
                             inputId = "univariate_check",
                             value = TRUE)

        # converting PDF results to PNGs to display in renderImage()
        if (file.exists(file.path(tempdir(), 'OUTPUT', 'UNIVARIATE', 'Rplots.pdf'))){
            print("Rplots.pdf exists!, deleting now")
        } else {
            print("Rplots.pdf does not exist!")
        }
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'UNIVARIATE'),
                       pattern = ".pdf$", full.names = TRUE)

        lapply(
            pdf.list,
            FUN = function(files) {
                if (basename(files) == "Rplots.pdf") {
                    NULL
                } else {
                    pdf_convert(
                        files,
                        format = "png",
                        filenames = paste0(
                            dirname(files),
                            "/PNGFILES/" ,
                            tools::file_path_sans_ext(basename(files)),
                            ".png"
                        )
                    )
                }
                
            }
        )
        
        # #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = normalizePath(file.path(tempdir(), 'OUTPUT', 'UNIVARIATE', 'PNGFILES')),
                pattern = ".png$",
                full.names = FALSE
            )
        print(filenames)
        updateSelectInput(session = session,
                          inputId = "select_uni_plot",
                          choices = filenames)
        
        enable("svm")

    })

    # SVM button
    observeEvent(input$svm, {
        if(xt == "mat") {req(input$file_data, input$file_annotations, input$file_node)}
        else if (xt == "csv") { req(input$file_data)}

        matfilenoext <-
            tools::file_path_sans_ext(input$file_data$name)

        dat_file <- switch(
            input$modeltype,
            noneset = paste0(tempdir(), "/OUTPUT/", matfilenoext, "_2D_wo_uni.csv"),
            kset = paste0(tempdir(), "/OUTPUT/UNIVARIATE/", matfilenoext, "_ml.csv"),
            uset = paste0(tempdir(), "/OUTPUT/", matfilenoext, "_2D_wo_uni.csv")
        )
        
        cvuni <- switch(
            input$modeltype,
            noneset = FALSE,
            kset = FALSE,
            uset = TRUE
        )

        do.call(file.remove, list(list.files(
            file.path(tempdir(), 'OUTPUT', 'ML_SVM'),
            full.names = TRUE, recursive = TRUE
        )))

        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'ML_SVM'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'ML_SVM', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            file.path(tempdir(), 'OUTPUT', 'ML_SVM')

        nodefile <- input$file_node$datapath
        # configs <- uploaded_configs()[2][[1]]
        contrast <- contrast$groups[1]
        psetting = FALSE
        if (isTruthy(input$cores)){
            paste0("parallel computing", input$cores)
            psetting <- TRUE
            cores <- input$cores
        } else {
            print("no parallel computing")
        }
        

        # getting values from input fields into an args[] fromat
        inputArgs <- ml_svmConfigs(inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        inputArgs[9] <- psetting
        inputArgs[10] <- cores
        inputArgs[62] <- cvuni
        inputArgs[64] <- contrast
        
        show_modal_spinner(text = "Running classifier...")
        withConsoleRedirect("console", {
            ml_svm(inputArgs)
        })
        
        print("PLSDA ANALYSIS")
        plsdainputArgs <- plsdaConfigs(inputConfigs())
        plsdainputArgs[6] <- paste0(outdir, '/', matfilenoext, '_final_svm_model.Rdata')
        plsdainputArgs[7] <- matfilenoext
        plsdainputArgs[8] <- outdir
        plsdainputArgs[9] <- psetting
        plsdainputArgs[10] <- cores
        
        withConsoleRedirect("console", {
            plsda_val_svm(plsdainputArgs)
        })
        
        remove_modal_spinner()

        # converting PDF results to PNGs to display in renderImage()
        if (file.exists(file.path(tempdir(), 'OUTPUT', 'ML_SVM', 'Rplots.pdf'))){
            print("Rplots.pdf exists!, deleting now")
        } else {
            print("Rplots.pdf does not exist!")
        }
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'ML_SVM'),
                       pattern = ".pdf$", full.names = TRUE)
        lapply(
            pdf.list,
            FUN = function(files) {
                if (basename(files) == "Rplots.pdf") {
                    NULL
                } else {
                    pdf_convert(
                        files,
                        format = "png",
                        filenames = paste0(
                            dirname(files),
                            "/PNGFILES/" ,
                            tools::file_path_sans_ext(basename(files)),
                            ".png"
                        )
                    )
                }
                
            }
        )

        #updating the Select Input with the png filenames
        files <-
            list.files(
                path = file.path(tempdir(), 'OUTPUT', 'ML_SVM', 'PNGFILES'),
                pattern = ".png$",
                full.names = FALSE
            )
        print(files)
        updateSelectInput(session = session,
                          inputId = "select_svm_plot",
                          choices = files)

        enable("svm_check")
        updatePrettyCheckbox(session = session,
                             inputId = "svm_check",
                             value = TRUE)


    })

    # viewing plots
    output$uni_plots <- renderImage({
        req(input$univariate)
        setwd(tempdir())
        suppressWarnings(list(
            src = normalizePath(file.path(
                tempdir(),
                'OUTPUT',
                'UNIVARIATE',
                'PNGFILES',
                input$select_uni_plot
            )),
            alt = "Univariate plots",
            width = 400,
            height = 400
        ))

    }, deleteFile = FALSE)

    output$svm_plots <- renderImage({
        req(input$svm)
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'ML_SVM',
                'PNGFILES',
                input$select_svm_plot
            ),
            alt = "SVM plots",
            width = 400,
            height = 400
        )

    }, deleteFile = FALSE)
    

    output$report <- downloadHandler(
        
        filename = function() {
            paste("report-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
            setwd('/srv/shiny-server/')
            tempReport <- file.path(tempdir(), 'results.Rmd')
            file.copy(file.path('srv', 'shiny-server', 'results.Rmd'), tempReport, overwrite = TRUE)
            rmarkdown::render(input = 'results.Rmd')
            # tinytex::pdflatex('results.tex')
            file.copy(file.path('results.pdf'), file)
        }
        
    )

    output$allfiles <- downloadHandler(
        
        filename = function() {
            paste("allfiles-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
            tempzip <- file.path(tempdir(), "results.zip")
            zipr(tempzip, file.path(tempdir(), 'OUTPUT'))
            file.copy(tempzip, file)
        }
    )
    
    predictServer("regular")


    ################################### Regression #####################################
    # checking and loading only the mat file
    output$reg_debug <-
        renderText(paste0("Debug: ", input$reg_cpu_cluster))

    reg_training <- reactive({
        req(input$reg_file_data)
        reg_xt <<- tools::file_ext(input$reg_file_data$datapath)
        print(reg_xt)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$reg_file_data$name))
        if (reg_xt == "mat") {
            df <- readMat(input$reg_file_data$datapath)
            df <- df[[1]]
            dater[[1, 1]] <- df
            # return(dater)
        } else if (reg_xt == "csv") {
            df <- read.csv(input$reg_file_data$datapath,
                           header = TRUE,
                           sep = ",")
            # return(df)
            dater[[1, 1]] <- df
        }
        return(dater)
    })

    reg_annotations <- reactive({
        reg_xt <<- tools::file_ext(input$reg_file_data$datapath)
        if (reg_xt == "csv"){
            req(input$reg_file_data)
            print("annotations 2D data!!")
            dater <- data.frame(training()[2])
            return(dater)
        }
        else {
            req(input$reg_file_annotations)
            dater = matrix(list(),
                           nrow = 1,
                           ncol = 1)
            df <- read.csv(input$reg_file_annotations$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
            return(dater)
        }
        
    })

    reg_node <- reactive({
        req(reg_xt)
        if(reg_xt == "csv"){
            print("node 2D data!!")
        }
        else{
            req(input$reg_file_node)
            dater = matrix(list(),
                           nrow = 1,
                           ncol = 1)
            df <- read.csv(input$reg_file_node$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
            return(dater)
        }
        
    })

    output$reg_training <- DT::renderDataTable({
        reg_xt <<- tools::file_ext(input$reg_file_data$datapath)
        req(input$reg_file_data)
        if (reg_xt == "csv"){
            d <- reg_training()[[1, 1]]
        } else if (reg_xt == "mat"){
            d <- reg_training()[[1, 1]][, , 1]
        }
        datatable(
            d,
            colnames = NULL,
            options = list(
                autoWidth = TRUE,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollY = TRUE,
                searching = FALSE
            ),
            selection = list(target = 'column')
        ) 
        # %>% formatRound(c(1:90), 3)
    })

    output$reg_annotation <- DT::renderDataTable({
        req(reg_xt)
        datatable(
            if(reg_xt == "csv"){
                reg_annotations()
            } else {
                reg_annotations()[[1, 1]]
            },
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })

    output$reg_node <- DT::renderDataTable({
        req(reg_xt)
        datatable(
            if(reg_xt == "csv"){
                
            } else {
                reg_node()[[1, 1]]
            },
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })

    # rendering the drag and drop with variable names
    
    output$reg_variables_csv <- DT::renderDataTable({
        d <- reg_training()[[1, 1]]
        
        labs <- c(colnames(d))
        datatable(
            matrix(labs),
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
        
    })
    
    output$reg_samplevar <- renderText({
        d <- reg_training()[[1, 1]]
        paste("Selected Sample Variable: ", colnames(d)[input$reg_variables_csv_rows_selected[1]])
    })
    output$reg_groupvar <- renderText({
        d <- reg_training()[[1, 1]]
        paste("Selected Group Variable: ", colnames(d)[input$reg_variables_csv_rows_selected[2]])
    })
    
    output$reg_variables <- renderUI({
        req(input$reg_file_data)
        reg_xt <<- tools::file_ext(input$reg_file_data$datapath)
        if (reg_xt == "csv"){
            
            box(
                title = "File Variables from column names",
                status = "primary",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                dataTableOutput("reg_variables_csv"),
                verbatimTextOutput("reg_samplevar"),
                verbatimTextOutput("reg_groupvar"),
                actionButton(inputId = "reg_choose_vars", label = "Confirm Variables")
            )
        } else if(reg_xt == "mat") {
        req(input$reg_file_annotations, input$reg_file_node)
        labs <-
            c(colnames(reg_annotations()[[1, 1]]), colnames(reg_node()[[1, 1]]))
        print(labs)
        useShinyjs()
        tagList(
            box(
                title = "File Variables",
                status = "primary",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,

                fluidRow(
                    class = "panel-body",
                    column(
                        width = 4,
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "File Variables"),
                            tags$div(class = "panel-body av_vars_body",
                                     id = "reg_availablevars",
                                     icon_list(labs)),
                            tags$style(HTML('
                                '))
                        )
                    ),
                    column(
                        width = 4,
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "Sample"),
                            tags$div(class = "panel-body",
                                     id = "reg_chosen_sample")
                        ),
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "Target"),
                            tags$div(class = "panel-body",
                                     id = "reg_chosen_group")
                        ),
                    ),
                    column(
                        width = 4,
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "Node ID"),
                            tags$div(class = "panel-body",
                                     id = "reg_chosen_node")
                        ),
                        tags$div(
                            class = "panel panel-default",
                            tags$div(class = "panel-heading",
                                     "Region"),
                            tags$div(class = "panel-body",
                                     id = "reg_chosen_region")
                        )
                    ),
                    #################### sortable code for drag and drop #####################
                    sortable_js(
                        "reg_availablevars",
                        options = sortable_options(
                            group = list(
                                pull = TRUE,
                                name = "reg_allvars",
                                put = TRUE
                            ),
                            onSort = sortable_js_capture_input("reg_sort_vars")
                        )

                    ),
                    sortable_js(
                        "reg_chosen_sample",
                        options = sortable_options(
                            group = list(
                                group = "reg_allvars",
                                put = htmlwidgets::JS(
                                    'function (to) { return to.el.children.length < 1; }'
                                ),
                                pull = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_sample").style.backgroundColor = "white"; }'
                                )
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = sortable_js_capture_input("reg_chosen_sample")
                        )
                    ),
                    sortable_js(
                        "reg_chosen_group",
                        options = sortable_options(
                            group = list(
                                group = "reg_allvars",
                                put = htmlwidgets::JS(
                                    'function (to) { return to.el.children.length < 1; }'
                                ),
                                pull = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_group").style.backgroundColor = "white"; }'
                                )
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = sortable_js_capture_input("reg_chosen_group")
                            # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
                        )
                    ),
                    sortable_js(
                        "reg_chosen_node",
                        options = sortable_options(
                            group = list(
                                group = "reg_allvars",
                                put = htmlwidgets::JS(
                                    'function (to) { return to.el.children.length < 1; }'
                                ),
                                pull = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_node").style.backgroundColor = "white"; }'
                                )
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = sortable_js_capture_input("reg_chosen_node")
                        )
                    ),
                    sortable_js(
                        "reg_chosen_region",
                        options = sortable_options(
                            group = list(
                                group = "reg_allvars",
                                put = htmlwidgets::JS(
                                    'function (to) { return to.el.children.length < 1; }'
                                ),
                                pull = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_region").style.backgroundColor = "white"; }'
                                )
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = sortable_js_capture_input("reg_chosen_region")
                        )
                    )
                ),
                actionButton(inputId = "reg_choose_vars",
                             label = "Confirm Variables")
            )
        )
        }
    })

    observeEvent(input$reg_choose_vars, {
        print("WE ARE CHOOSING VARIABLES!!!")
        print(reg_xt)
        print(colnames(reg_training()[[1, 1]][input$reg_variables_csv_rows_selected[1]]))
        if(reg_xt == "csv"){
            print(reg_xt)
            varNames <- c(colnames(reg_training()[[1, 1]])[input$reg_variables_csv_rows_selected[1]], 
                          colnames(reg_training()[[1, 1]])[input$reg_variables_csv_rows_selected[2]])
            paste(varNames)
        } else if(reg_xt == "mat"){
            varNames <- c(input$reg_chosen_sample, input$reg_chosen_group, input$reg_chosen_node, input$reg_chosen_region)
        }
        
        print(varNames)
        vars = "You chose: "
        for (i in 1:length(varNames)) {
            v = varNames[i]
            vars = paste0(vars, v, "; ", sep = "", collapse = NULL)
        }
        showModal(
            modalDialog(
                title = "Loaded Variables",
                sprintf(vars),
                easyClose = FALSE,
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("reg_continue", "Continue")
                )
            )
        )
        # print(reg_contrast$groups)
        enable("reg_variable_check")
        updatePrettyCheckbox(session = session,
                             inputId = "reg_variable_check",
                             value = TRUE)
    })

    observeEvent(input$reg_continue, {
        matfile <- input$reg_file_data$datapath
        matfilenoext <-
            tools::file_path_sans_ext(input$reg_file_data$name)
        dir.create(file.path(tempdir(), '/OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), '/OUTPUT', '/REGRESSION'), showWarnings = FALSE)
        
        if (reg_xt == "csv"){
            samplesvar <- colnames(reg_training()[[1, 1]])[input$reg_variables_csv_rows_selected[1]]
            groupvar <- colnames(reg_training()[[1, 1]])[input$reg_variables_csv_rows_selected[2]]
        } else if (reg_xt == "mat"){
            annotfile <- input$reg_file_annotations$datapath
            samplesvar <- input$reg_chosen_sample
            groupvar <- input$reg_chosen_group
        }
        
        
        outdir <- paste0(tempdir(), '/OUTPUT/REGRESSION')
        
        if(reg_xt == "mat") {
            proc_data <- reg_inputDatProcess(
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
        } else if (reg_xt == "csv") {
            proc_data <- reg_input_dat_process_2D(
                c(
                    "y",
                    "d",
                    "c",
                    "a",
                    "s",
                    matfile,
                    matfilenoext,
                    samplesvar,
                    groupvar,
                    outdir
                )
            )
        }

        

        raw_sample_dfm$raw_sample_dfm <- proc_data[1]
        raw_sample_dfm$raw_sample_dfm_wo_uni <- proc_data[2]
        removeModal()

        enable("reg_inputdata_check")
        updatePrettyCheckbox(session = session,
                             inputId = "reg_inputdata_check",
                             value = TRUE)
    })

    #### configurations
    reg_inputConfigs <- reactive({
        configs <- list(
            reg_random_state = input$reg_random_state,

            reg_log2_trans = input$reg_log2_trans,

            reg_htmap_textsize_col = input$reg_htmap_textsize_col,
            reg_htmap_textangle_col = input$reg_htmap_textangle_col,
            reg_htmap_lab_row = input$reg_htmap_lab_row,
            reg_htmap_textsize_row = input$reg_htmap_textsize_row,
            reg_htmap_keysize = input$reg_htmap_keysize,
            reg_htmap_key_xlab = paste0('"', input$reg_htmap_key_xlab, '"'),
            reg_htmap_key_ylab =  paste0('"', input$reg_htmap_key_ylab, '"'),
            reg_htmap_margin = paste0('"c', input$reg_htmap_margin, '"'),
            reg_htmap_width = input$reg_htmap_width,
            reg_htmap_height = input$reg_htmap_height,

            reg_pca_scale_data = input$reg_pca_scale_data,
            reg_pca_center_data = input$reg_pca_center_data,
            reg_pca_pc = paste0('"c', input$reg_pca_pc, '"'),
            reg_pca_biplot_samplelabel_type = paste0('"', input$reg_pca_biplot_samplelabel_type, '"'),
            reg_pca_biplot_samplelabel_size = input$pca_biplot_samplelabel_size,
            reg_pca_biplot_symbol_size = input$reg_pca_biplot_symbol_size,
            reg_pca_biplot_ellipse = input$reg_pca_biplot_ellipse,
            reg_pca_biplot_ellipse_conf = input$reg_pca_biplot_ellipse_conf,
            reg_pca_biplot_loading = input$reg_pca_biplot_loading,
            reg_pca_biplot_loading_textsize = input$reg_pca_biplot_loading_textsize,
            reg_pca_biplot_multi_density = input$reg_pca_biplot_multi_density,
            reg_pca_biplot_multi_striplabel_size = input$reg_pca_biplot_multi_striplabel_size,
            reg_pca_rightside_y = input$reg_pca_rightside_y,
            reg_pca_x_tick_label_size = input$reg_pca_x_tick_label_size,
            reg_pca_y_tick_label_size = input$reg_pca_y_tick_label_size,
            reg_pca_width = input$reg_pca_width,
            reg_pca_height = input$reg_pca_height,

            reg_uni_fdr = input$reg_uni_fdr,
            reg_uni_alpha = input$reg_uni_alpha,
            reg_uni_fold_change = input$reg_uni_fold_change,
            reg_volcano_n_top_connections = input$reg_volcano_n_top_connections,
            reg_volcano_symbol_size = input$reg_volcano_symbol_size,
            reg_volcano_sig_colour = paste0('"', input$reg_volcano_sig_colour, '"'),
            reg_volcano_nonsig_colour = paste0('"', input$reg_volcano_nonsig_colour, '"'),
            reg_volcano_x_text_size = input$reg_volcano_x_text_size,
            reg_volcano_y_text_size = input$reg_volcano_y_text_size,
            reg_volcano_width = input$reg_volcano_width,
            reg_volcano_height = input$reg_volcano_height,

            reg_sig_htmap_textsize_col = input$reg_sig_htmap_textsize_col,
            reg_sig_htmap_textangle_col = input$reg_sig_htmap_textangle_col,
            reg_sig_htmap_textsize_row = input$reg_sig_htmap_textsize_row,
            reg_sig_htmap_keysize = input$reg_sig_htmap_keysize,
            reg_sig_htmap_key_xlab = paste0('"', input$reg_sig_htmap_key_xlab, '"'),
            reg_sig_htmap_key_ylab = paste0('"', input$reg_sig_htmap_key_ylab, '"'),
            reg_sig_htmap_margin = paste0('"c', input$reg_sig_htmap_margin, '"'),
            reg_sig_htmap_width = input$reg_sig_htmap_width,
            reg_sig_htmap_height = input$reg_sig_htmap_height,
            reg_sig_pca_pc = paste0('"c', input$reg_sig_pca_pc, '"'),
            reg_sig_pca_biplot_ellipse_conf = input$reg_sig_pca_biplot_ellipse_conf,

            reg_cpu_cluster = paste0('"', input$reg_cpu_cluster, '"'),

            reg_training_percentage = input$reg_training_percentage,

            reg_svm_cv_center_scale = input$reg_svm_cv_center_scale,
            reg_svm_cv_kernel = paste0('"', input$reg_svm_cv_kernel, '"'),
            reg_svm_cv_cross_k = input$reg_svm_cv_cross_k,
            reg_svm_cv_tune_method = paste0('"', input$reg_svm_cv_tune_method, '"'),
            reg_svm_cv_tune_cross_k = input$reg_svm_cv_tune_cross_k,
            reg_svm_cv_tune_boot_n = input$reg_svm_cv_tune_boot_n,
            reg_svm_cv_fs_rf_ifs_ntree = input$reg_svm_cv_fs_rf_ifs_ntree,
            reg_svm_cv_fs_rf_sfs_ntree = input$reg_svm_cv_fs_rf_sfs_ntree,
            reg_svm_cv_best_model_method = paste0('"', input$reg_svm_cv_best_model_method, '"'),
            reg_svm_cv_fs_count_cutoff = input$svm_cv_fs_count_cutoff,

            reg_svm_cross_k = input$reg_svm_cross_k,
            reg_svm_tune_cross_k = input$reg_svm_tune_cross_k,

            reg_svm_tune_boot_n = input$reg_svm_tune_boot_n,

            reg_svm_perm_method = paste0('"', input$reg_svm_perm_method, '"'),
            reg_svm_perm_n = input$reg_svm_perm_n,
            reg_svm_perm_plot_symbol_size = input$reg_svm_perm_plot_symbol_size,
            reg_svm_perm_plot_legend_size = input$reg_svm_perm_plot_legend_size,
            reg_svm_perm_plot_x_label_size = input$reg_svm_perm_plot_x_label_size,
            reg_svm_perm_plot_x_tick_label_size = input$reg_svm_perm_plot_x_tick_label_size,
            reg_svm_perm_plot_y_label_size = input$reg_svm_perm_plot_y_label_size,
            reg_svm_perm_plot_y_tick_label_size = input$reg_svm_perm_plot_y_tick_label_size,
            reg_svm_perm_plot_width = input$reg_svm_perm_plot_width,
            reg_svm_perm_plot_height = input$reg_svm_perm_plot_height,

            reg_svm_roc_threshold = input$reg_svm_roc_threshold,
            reg_svm_roc_smooth = input$reg_svm_roc_smooth,
            reg_svm_roc_symbol_size = input$reg_svm_roc_symbol_size,
            reg_svm_roc_legend_size = input$reg_svm_roc_legend_size,
            reg_svm_roc_x_label_size = input$reg_svm_roc_x_label_size,
            reg_svm_roc_x_tick_label_size = input$reg_svm_roc_x_tick_label_size,
            reg_svm_roc_y_label_size = input$reg_svm_roc_y_label_size,
            reg_svm_roc_y_tick_label_size = input$reg_svm_roc_y_tick_label_size,
            reg_svm_roc_width = input$reg_svm_roc_width,
            reg_svm_roc_height = input$reg_svm_roc_height,

            reg_svm_rffs_pca_pc = paste0('"c', input$reg_svm_rffs_pca_pc, '"'),
            reg_svm_rffs_pca_biplot_ellipse_conf = input$reg_svm_rffs_pca_biplot_ellipse_conf,

            reg_rffs_htmap_textsize_col = input$reg_rffs_htmap_textsize_col,
            reg_rffs_htmap_textangle_col = input$reg_rffs_htmap_textangle_col,
            reg_rffs_htmap_textsize_row = input$reg_rffs_htmap_textsize_row,
            reg_rffs_htmap_keysize = input$reg_rffs_htmap_keysize,
            reg_rffs_htmap_key_xlab = input$reg_rffs_htmap_key_xlab,
            reg_rffs_htmap_key_ylab = input$reg_rffs_htmap_key_ylab,
            reg_rffs_htmap_margin = paste0('"c', input$reg_rffs_htmap_margin, '"'),
            reg_rffs_htmap_width = input$reg_rffs_htmap_width,
            reg_rffs_htmap_height = input$reg_rffs_htmap_height,

            reg_plsda_validation = paste0('"', input$reg_plsda_validation, '"'),
            reg_plsda_validation_segment = input$reg_plsda_validation_segment,

            reg_plsda_init_ncomp = input$reg_plsda_init_ncomp,

            reg_plsda_ncomp_select_method = paste0('"', input$reg_plsda_ncomp_select_method, '"'),
            reg_plsda_ncomp_select_plot_symbol_size = input$reg_plsda_ncomp_select_plot_symbol_size,
            reg_plsda_ncomp_select_plot_legend_size = input$reg_plsda_ncomp_select_plot_legend_size,
            reg_plsda_ncomp_select_plot_x_label_size = input$reg_plsda_ncomp_select_plot_x_label_size,
            reg_plsda_ncomp_select_plot_x_tick_label_size = input$reg_plsda_ncomp_select_plot_x_tick_label_size,
            reg_plsda_ncomp_select_plot_y_label_size = input$reg_plsda_ncomp_select_plot_y_label_size,
            reg_plsda_ncomp_select_plot_y_tick_label_size = input$reg_plsda_ncomp_select_plot_y_tick_label_size,

            reg_plsda_perm_method = paste0('"', input$reg_plsda_perm_method, '"'),
            reg_plsda_perm_n = input$reg_plsda_perm_n,
            reg_plsda_perm_plot_symbol_size = input$reg_plsda_perm_plot_symbol_size,
            reg_plsda_perm_plot_legend_size = input$reg_plsda_perm_plot_legend_size,
            reg_plsda_perm_plot_x_label_size = input$reg_plsda_perm_plot_x_label_size,
            reg_plsda_perm_plot_x_tick_label_size = input$reg_plsda_perm_plot_x_tick_label_size,
            reg_plsda_perm_plot_y_label_size = input$reg_plsda_perm_plot_y_label_size,
            reg_plsda_perm_plot_y_tick_label_size = input$reg_plsda_perm_plot_y_tick_label_size,
            reg_plsda_perm_plot_width = input$reg_plsda_perm_plot_width,
            reg_plsda_perm_plot_height = input$reg_plsda_perm_plot_height,

            reg_plsda_scoreplot_ellipse_conf = input$reg_plsda_scoreplot_ellipse_conf,

            reg_plsda_roc_smooth = input$reg_plsda_roc_smooth,

            reg_plsda_vip_alpha = input$reg_plsda_vip_alpha,
            reg_plsda_vip_boot = input$reg_plsda_vip_boot,
            reg_plsda_vip_boot_n = input$reg_plsda_vip_boot_n,
            reg_plsda_vip_plot_errorbar = paste0('"', input$reg_plsda_vip_plot_errorbar, '"'),
            reg_plsda_vip_plot_errorbar_width = input$reg_plsda_vip_plot_errorbar_width,
            reg_plsda_vip_plot_errorbar_label_size = input$reg_plsda_vip_plot_errorbar_label_size,
            reg_plsda_vip_plot_x_textangle = input$reg_plsda_vip_plot_x_textangle,
            reg_plsda_vip_plot_x_label_size = input$reg_plsda_vip_plot_x_label_size,
            reg_plsda_vip_plot_x_tick_label_size = input$reg_plsda_vip_plot_x_tick_label_size,
            reg_plsda_vip_plot_y_label_size = input$reg_plsda_vip_plot_y_label_size,
            reg_plsda_vip_plot_y_tick_label_size = input$reg_plsda_vip_plot_y_tick_label_size,
            reg_plsda_vip_plot_width = input$reg_plsda_vip_plot_width,
            reg_plsda_vip_plot_height = input$reg_plsda_vip_plot_height
        )
        # configs_format(configs)
    })

    normdata <- reactiveValues()

    # Univariate button
    observeEvent(input$reg_univariate, {
        if(reg_xt == "mat"){
            req(input$reg_file_data, input$reg_file_annotations, input$reg_file_node)
        } else if (reg_xt == "csv"){
            print("UNIVARIATE  csv 2D")
            req(input$reg_file_data)
        }
        
        
        # unlink(file.path(tempdir(), 'OUTPUT'), recursive = TRUE, force = TRUE)

        # do.call(file.remove, list(list.files(
        #     file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE'),
        #     full.names = TRUE, recursive = TRUE
        # )))

        matfilenoext <-
            tools::file_path_sans_ext(input$reg_file_data$name)
        dat_file <-
            paste0(tempdir(), "/OUTPUT/REGRESSION/", matfilenoext, "_2D.csv")

        nodefile <- input$reg_file_node$datapath
        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'REGRESSION'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT','REGRESSION', 'UNIVARIATE'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            normalizePath(file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE'))
        node_id_var <- input$reg_chosen_node
        region_name_var <- input$reg_chosen_region

        # getting values from input fields into an args[] fromat
        inputArgs <- reg_univariateConfigs(reg_inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        
        
        if(reg_xt == "mat"){
            inputArgs[32] <- nodefile
            inputArgs[33] <- node_id_var
            inputArgs[34] <- region_name_var
        }

        show_modal_spinner(text = "Processing...")
        withConsoleRedirect("reg_console", {
            if(reg_xt == "mat"){
                out <- reg_univariate(inputArgs)
            } else if (reg_xt == "csv"){
                reg_univariate_2D(inputArgs)
            }
        })
        remove_modal_spinner() 

        enable("reg_univariate_check")
        updatePrettyCheckbox(session = session,
                             inputId = "reg_univariate_check",
                             value = TRUE)
        
        
        # converting PDF results to PNGs to display in renderImage()
        if (file.exists(file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE', 'Rplots.pdf'))){
            print("Rplots.pdf exists!, deleting now")
        } else {
            print("Rplots.pdf does not exist!")
        }
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                if (basename(files) == "Rplots.pdf") {
                    NULL
                } else {
                    pdf_convert(
                        files,
                        format = "png",
                        filenames = paste0(
                            dirname(files),
                            "/PNGFILES/" ,
                            tools::file_path_sans_ext(basename(files)),
                            ".png"
                        )
                    )
                }
            }
        )
        #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = normalizePath(file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE', 'PNGFILES')),
                pattern = ".png$",
                full.names = FALSE
            )
        updateSelectInput(session = session,
                          inputId = "reg_select_uni_plot",
                          choices = filenames)

    })

    # svm button
    observeEvent(input$reg_svm, {
        if (reg_xt == "mat"){
            req(input$reg_file_data, input$reg_file_annotations, input$reg_file_node)
        } else if (reg_xt == "csv"){
            req(input$reg_file_data)
        }
        

        matfilenoext <-
            tools::file_path_sans_ext(input$reg_file_data$name)

        dat_file <- switch(
            input$reg_modeltype,
            reg_noneset = paste0(tempdir(), "/OUTPUT/REGRESSION/", matfilenoext, "_2D.csv"),
            reg_kset = paste0(tempdir(), "/OUTPUT/REGRESSION/UNIVARIATE/", matfilenoext, "_ml.csv"),
            reg_uset = paste0(tempdir(), "/OUTPUT/REGRESSION/", matfilenoext, "_2D.csv")
        )
        
        # cvuni <- FALSE
        cvuni <- switch(
            input$reg_modeltype,
            reg_noneset = FALSE,
            reg_kset = FALSE,
            reg_uset = TRUE
        )
        print(cvuni)
        
        do.call(file.remove, list(list.files(
            file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM'),
            full.names = TRUE, recursive = TRUE
        )))

        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'REGRESSION'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM')

        nodefile <- input$reg_file_node$datapath
        # configs <- uploaded_configs()[2][[1]]
        # contrast <- reg_contrast$groups[1]
        
        if (isTruthy(input$reg_cores)){
            paste0("parallel computing", input$reg_cores)
            psetting <- TRUE
            cores <- input$reg_cores
        } else {
            print("no parallel computing")
        }
        
        # getting values from input fields into an args[] format
        inputArgs <- reg_ml_svmConfigs(reg_inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        inputArgs[9] <- psetting
        inputArgs[10] <- cores
        inputArgs[56] <- cvuni
        
        # if(reg_xt == "mat"){
        #     inputArgs[64] <- contrast
        # }
        
        
        show_modal_spinner(text = "Running classifier...")
        # withConsoleRedirect("reg_console", {
            # invalidateLater(100)
            reg_ml_svm(inputArgs)

        # })
        remove_modal_spinner()

        # converting PDF results to PNGs to display in renderImage()
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                if (basename(files) == "Rplots.pdf") {
                    NULL
                } else {
                    pdf_convert(
                        files,
                        format = "png",
                        filenames = paste0(
                            dirname(files),
                            "/PNGFILES/" ,
                            tools::file_path_sans_ext(basename(files)),
                            ".png"
                        )
                    )
                }
            }
        )

        #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM', 'PNGFILES'),
                pattern = ".png$",
                full.names = FALSE
            )
        updateSelectInput(session = session,
                          inputId = "reg_select_svm_plot",
                          choices = filenames)

        enable("reg_svm_check")
        updatePrettyCheckbox(session = session,
                             inputId = "reg_svm_check",
                             value = TRUE)
        closeAllConnections()

    })

    # viewing plots
    output$reg_uni_plots <- renderImage({
        req(input$reg_univariate)
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'REGRESSION',
                'UNIVARIATE',
                'PNGFILES',
                input$reg_select_uni_plot
            ),
            alt = "Univariate plots",
            width = 400,
            height = 400
        )

    }, deleteFile = FALSE)

    output$reg_svm_plots <- renderImage({
        req(input$reg_svm)
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'REGRESSION',
                'ML_SVM',
                'PNGFILES',
                input$reg_select_svm_plot
            ),
            alt = "SVM plots",
            width = 400,
            height = 400
        )

    }, deleteFile = FALSE)
    
    output$reg_report <- downloadHandler(
        
        filename = function() {
            paste("regression_report-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
            setwd('/srv/shiny-server/')
            tempReport <- file.path(tempdir(), 'reg_results.Rmd')
            file.copy(file.path('srv', 'shiny-server', 'reg_results.Rmd'), tempReport, overwrite = TRUE)
            rmarkdown::render(input = 'reg_results.Rmd')
            file.copy(file.path('reg_results.pdf'), file)
        }
        
    )
    
    output$reg_allfiles <- downloadHandler(
        
        filename = function() {
            paste("allfiles-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
            tempzip <- file.path(tempdir(), "results.zip")
            zipr(tempzip, file.path(tempdir(), 'OUTPUT'))
            file.copy(tempzip, file)
        }
    )
    
    
    ######################### CV-ONLY ANALYSIS ################################################
    
    observeEvent(input$cv_file_data, {
        enable("cv_upload_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_upload_check",
                             value = TRUE)
        
    })
    
    # module test code
    # datafiles <- uploadFilesServer("regular")
    # 
    # output$training <- renderDataTable({
    #     datafiles$data()[, , 1]
    # })
    
    
    
    # checking and loading only the mat file
    cv_training <- reactive({
        req(input$cv_file_data)
        cv_xt <<- tools::file_ext(input$cv_file_data$datapath)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$cv_file_data$name))
        
        if (cv_xt == "mat") {
            df <- readMat(input$cv_file_data$datapath)
            df <- df[[1]]
            dater[[1, 1]] <- df
            return(dater)
        } else if (cv_xt == "csv") {
            df <- read.csv(input$cv_file_data$datapath,
                           header = TRUE,
                           sep = ",")
            # dater[[1, 1]] <- df
            return(df)
        }
        
    })
    
    cv_annotations <- reactive({
        req(cv_xt)
        if (cv_xt == "csv"){
            req(input$cv_file_data)
            print("annotations 2D data!!")
            dater <- data.frame(cv_training()[2])
            return(dater)
        } else if (cv_xt == "mat") {
            req(input$cv_file_annotations)
            dater = matrix(list(),
                           nrow = 1,
                           ncol = 1)
            df <- read.csv(input$cv_file_annotations$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
            return(dater)
        }
        
    })
    
    
    cv_node <- reactive({
        req(cv_xt)
        if(cv_xt == "csv"){
            print("node 2D data!!!!")
        } else {
            req(input$cv_file_node)
            dater = matrix(list(),
                           nrow = 1,
                           ncol = 1)
            df <- read.csv(input$cv_file_node$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
            return(dater)
        }
    })
    output$cv_training <- DT::renderDataTable({
        cv_xt <<- tools::file_ext(input$cv_file_data$datapath)
        req(input$cv_file_data, cv_xt)
        if (cv_xt == "csv") {
            d <- cv_training()
        } else if (cv_xt == "mat") {
            d <- cv_training()[[1, 1]][, , 1]
            # d <- readMat(input$file_data$datapath)
            # d <- df[[1]]
        }
        datatable(
            # training()[[1, 1]][, , 1],
            # training()[[1, 1]][, , 1], ######### change this after
            d,
            options = list(
                autoWidth = TRUE,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollY = TRUE,
                searching = FALSE
            ),
            selection = list(target = 'column')
        )
        # %>% formatRound(columns=c(1:ncol(training()[[1, 1]][, ,1])), 3)
    })
    
    output$cv_annotation <- DT::renderDataTable({
        cv_xt <<- tools::file_ext(input$cv_file_data$datapath)
        if(cv_xt == "csv"){
            # print(input$training_columns_selected)
            d <- training()
            d <- d[, input$cv_training_columns_selected]
        } else if (cv_xt == "mat"){
            d <- cv_annotations()[[1, 1]]
        }
        datatable(
            matrix(d),
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })
    
    output$cv_node <- DT::renderDataTable({
        if(cv_xt == "csv"){
            
        } else {
            datatable(
                cv_node()[[1, 1]],
                options = list(
                    autowidth = TRUE,
                    pageLength = 10,
                    lengthChange = FALSE,
                    searching = FALSE
                )
            )
        }
    })
    
    
    
    # rendering the drag and drop with variable names
    output$cv_variables_csv <- DT::renderDataTable({
        d <- cv_training()

        labs <- c(colnames(d))
        datatable(
            matrix(labs),
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )

    })

    output$cv_samplevar <- renderText({
        d <- cv_training()
        paste("Selected Sample Variable: ", colnames(d)[input$cv_variables_csv_rows_selected[1]])
    })
    output$cv_groupvar <- renderText({
        d <- cv_training()
        paste("Selected Group Variable: ", colnames(d)[input$cv_variables_csv_rows_selected[2]])
    })
    output$cv_variables <- renderUI({
        req(input$cv_confirmcontrast)
        cv_xt <<- tools::file_ext(input$cv_file_data$datapath)
        print(cv_xt)
        if(cv_xt == "csv"){

            box(
                title = "File Variables from column names",
                status = "primary",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                dataTableOutput("cv_variables_csv"),
                verbatimTextOutput("cv_samplevar"),
                verbatimTextOutput("cv_groupvar"),
                actionButton(inputId = "cv_choose_vars", label = "Confirm Variables")
            )

        } else if(cv_xt == "mat"){
            print("file is not 2d")

            req(input$cv_file_annotations, input$cv_file_node)
            labs <-
                c(colnames(cv_annotations()[[1, 1]]), colnames(cv_node()[[1, 1]]))

            useShinyjs()
            tagList(
                box(
                    title = "File Variables",
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,


                    fluidRow(
                        class = "panel-body",
                        column(
                            width = 4,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "File Variables"),
                                tags$div(class = "panel-body av_vars_body",
                                         id = "cv_availablevars",
                                         icon_list(labs)),
                                tags$style(HTML('
                                        '))
                            )
                        ),
                        column(
                            width = 4,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Sample"),
                                tags$div(class = "panel-body",
                                         id = "cv_chosen_sample")
                            ),
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group"),
                                tags$div(class = "panel-body",
                                         id = "cv_chosen_group")
                            )
                        ),
                        column(
                            width = 4,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Node ID"),
                                tags$div(class = "panel-body",
                                         id = "cv_chosen_node")
                            ),
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Region"),
                                tags$div(class = "panel-body",
                                         id = "cv_chosen_region")
                            )
                        ),
                        #################### sortable code for drag and drop
                        sortable_js(
                            "cv_availablevars",
                            options = sortable_options(
                                group = list(pull = TRUE,
                                             name = "cv_allvars",
                                             put = TRUE),
                                onSort = sortable_js_capture_input("cv_sort_vars")
                            )

                        ),
                        sortable_js(
                            "cv_chosen_sample",
                            options = sortable_options(
                                group = list(
                                    group = "cv_allvars",
                                    put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_chosen_sample").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_chosen_sample")
                            )
                        ),
                        sortable_js(
                            "cv_chosen_group",
                            options = sortable_options(
                                group = list(
                                    group = "cv_allvars",
                                    put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_chosen_group").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_chosen_group")
                                # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
                            )
                        ),
                        sortable_js(
                            "cv_chosen_node",
                            options = sortable_options(
                                group = list(
                                    group = "cv_allvars",
                                    put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_chosen_node").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_chosen_node")
                            )
                        ),
                        sortable_js(
                            "cv_chosen_region",
                            options = sortable_options(
                                group = list(
                                    group = "cv_allvars",
                                    put = htmlwidgets::JS('function (to) { return to.el.children.length < 1; }'),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_chosen_region").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_chosen_region")
                            )
                        )
                    ),
                    actionButton(inputId = "cv_choose_vars",
                                 label = "Confirm Variables")
                )


            )

        } else {
            print("wrong input file")
        }

    })


    output$cv_groups <- renderUI({
        req(input$cv_file_data)
        cv_xt <<- tools::file_ext(input$cv_file_data$datapath)
        if(cv_xt == "csv"){
            req(input$cv_training_columns_selected)
            print("contrast !! file 2d!")

            d <- read.csv(input$cv_file_data$datapath,
                          header = TRUE,
                          sep = ",")
            d <- matrix(d[, input$cv_training_columns_selected])
            # print(d)

            groups <- data.frame(d)
            groups <- count(groups$group)
            groupnames <- unique(d)
            groupcount <- groups$freq
            numgroups <- length(groups)
            print("group names")
            print(groupnames)
            print(unique(d))
            useShinyjs()
            tagList(
                box(
                    title = "Contrast variables",
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    fluidRow(
                        class = "panel-body",
                        column(
                            width = 3,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group Variables"),
                                tags$div(
                                    class = "panel-body",
                                    id = "cv_availablegroups",
                                    icon_list(groupnames)
                                ),
                                tags$style(HTML('
                                '))
                            )
                        ),
                        column(
                            width = 3,
                            tags$div(
                                id = "cv_contrastgroup1",
                                class = "panel panel-default cv_contrast1",
                                tags$div(class = "panel-heading",
                                         "Group 1"),
                                tags$div(class = "panel-body",
                                         id = "cv_contrast1")
                            )
                        ),
                        column(width = 1,
                               tags$div(class = "h1",
                                        "VS.")),
                        column(
                            width = 3,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group 2"),
                                tags$div(class = "panel-body",
                                         id = "cv_contrast2")
                            )
                        ),

                        column(
                            width = 2,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         icon("trash"),
                                         "Bin item"),
                                tags$div(class = "panel-body",
                                         id = "cv_trashbin")
                            )
                        )
                    ),
                    sortable_js(
                        "cv_availablegroups",
                        options = sortable_options(
                            group = list(
                                pull = "clone",
                                name = "cv_allsorts",
                                put = FALSE
                            ),
                            onSort = sortable_js_capture_input("cv_sort_vars")
                        )
                    ),
                    sortable_js(
                        "cv_contrast1",
                        options = sortable_options(
                            group = list(
                                group = "cv_allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = htmlwidgets::JS('function (to) { var count = cv_contrast1.children.length; console.log(count);  var c = cv_contrast1.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";
                                                 }
                                                 console.log("yeeeeeehehehrerhq;lrehq;werh");

                                                 }}'),
                            onAdd = sortable_js_capture_input("cv_contrast_group1")
                        )

                    ),
                    sortable_js(
                        "cv_contrast2",
                        options = sortable_options(
                            group = list(
                                group = "cv_allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = htmlwidgets::JS('function (to) { var count = cv_contrast2.children.length; console.log(count);  var c = cv_contrast2.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";

                                                 }
                                                 console.log("yeeeeeehehehrerhq;lrehq;werh");

                                                 }}'),
                            onAdd = sortable_js_capture_input("cv_contrast_group2")
                        )

                    ),
                    sortable_js(
                        "trashbin",
                        options = sortable_options(
                            group = list(
                                group = "cv_allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
                        )
                    ),
                    actionButton(inputId = "cv_confirmcontrast", label = "Confirm")
                )
            )
        }else if (cv_xt == "mat"){
            req(input$cv_file_annotations, input$cv_file_node)
            groups <- data.frame(cv_annotations()[[1, 1]])
            groups <- count(groups$group)
            groupnames <- unique(groups$x)
            print("contrast variables")
            print(groupnames)
            groupcount <- groups$freq
            numgroups <- length(groups)
            useShinyjs()
            tagList(
                box(
                    title = "Contrast variables",
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    fluidRow(
                        class = "panel-body",
                        column(
                            width = 3,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group Variables"),
                                tags$div(
                                    class = "panel-body",
                                    id = "cv_availablegroups",
                                    icon_list(groupnames)
                                ),
                                tags$style(HTML('
                                '))
                            )
                        ),
                        column(
                            width = 3,
                            tags$div(
                                id = "cv_contrastgroup1",
                                class = "panel panel-default contrast1",
                                tags$div(class = "panel-heading",
                                         "Group 1"),
                                tags$div(class = "panel-body",
                                         id = "cv_contrast1")
                            )
                        ),
                        column(width = 1,
                               tags$div(class = "h1",
                                        "VS.")),
                        column(
                            width = 3,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group 2"),
                                tags$div(class = "panel-body",
                                         id = "cv_contrast2")
                            )
                        ),

                        column(
                            width = 2,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         icon("trash"),
                                         "Bin item"),
                                tags$div(class = "panel-body",
                                         id = "cv_trashbin")
                            )
                        )
                    ),
                    sortable_js(
                        "cv_availablegroups",
                        options = sortable_options(
                            group = list(
                                pull = "clone",
                                name = "cv_allsorts",
                                put = FALSE
                            ),
                            onSort = sortable_js_capture_input("cv_sort_vars")
                        )
                    ),
                    sortable_js(
                        "cv_contrast1",
                        options = sortable_options(
                            group = list(
                                group = "cv_allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = htmlwidgets::JS('function (to) { var count = cv_contrast1.children.length; console.log(count);  var c = cv_contrast1.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";
                                                 }
                                                 console.log("yeeeeeehehehrerhq;lrehq;werh");

                                                 }}'),
                            onAdd = sortable_js_capture_input("cv_contrast_group1")
                        )

                    ),
                    sortable_js(
                        "cv_contrast2",
                        options = sortable_options(
                            group = list(
                                group = "cv_allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = htmlwidgets::JS('function (to) { var count = cv_contrast2.children.length; console.log(count);  var c = cv_contrast2.children;
                                                 var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]

                                                 if (count >= 1){
                                                 for (i = 0; i < count; i++){
                                                 if (i%5 == 0) {
                                                 var j = 0;
                                                 }
                                                 var color = colors[j]
                                                 c[i].style.backgroundColor = color;
                                                 j += 1;
                                                 var text = c[i].innerText.match(/[a-zA-Z]+/g);
                                                 c[i].innerText = i.toString().concat(". ", text);
                                                 c[i].style.fontWeight = "bold";

                                                 }
                                                 }}'),
                            onAdd = sortable_js_capture_input("cv_contrast_group2")
                        )

                    ),
                    sortable_js(
                        "cv_trashbin",
                        options = sortable_options(
                            group = list(
                                group = "cv_allsorts",
                                put = TRUE,
                                pull = TRUE
                            ),
                            onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
                        )
                    ),
                    actionButton(inputId = "cv_confirmcontrast", label = "Confirm")
                )
            )
        }




    })

    # getting configurations from input fields (defaults)
    cv_inputConfigs <- reactive({
        configs <- list(
            cv_random_state = input$cv_random_state,

            cv_log2_trans = input$cv_log2_trans,

            cv_htmap_textsize_col = input$cv_htmap_textsize_col,
            cv_htmap_textangle_col = input$cv_htmap_textangle_col,
            cv_htmap_lab_row = input$cv_htmap_lab_row,
            cv_htmap_textsize_row = input$cv_htmap_textsize_row,
            cv_htmap_keysize = input$cv_htmap_keysize,
            cv_htmap_key_xlab = paste0('"', input$cv_htmap_key_xlab, '"'),
            cv_htmap_key_ylab =  paste0('"', input$cv_htmap_key_ylab, '"'),
            cv_htmap_margin = paste0('"c', input$cv_htmap_margin, '"'),
            cv_htmap_width = input$cv_htmap_width,
            cv_htmap_height = input$cv_htmap_height,

            cv_pca_scale_data = input$cv_pca_scale_data,
            cv_pca_center_data = input$cv_pca_center_data,
            cv_pca_pc = paste0('"c', input$cv_pca_pc, '"'),
            cv_pca_biplot_samplelabel_type = paste0('"', input$cv_pca_biplot_samplelabel_type, '"'),
            cv_pca_biplot_samplelabel_size = input$cv_pca_biplot_samplelabel_size,
            cv_pca_biplot_symbol_size = input$cv_pca_biplot_symbol_size,
            cv_pca_biplot_ellipse = input$cv_pca_biplot_ellipse,
            cv_pca_biplot_ellipse_conf = input$cv_pca_biplot_ellipse_conf,
            cv_pca_biplot_loading = input$cv_pca_biplot_loading,
            cv_pca_biplot_loading_textsize = input$cv_pca_biplot_loading_textsize,
            cv_pca_biplot_multi_density = input$cv_pca_biplot_multi_density,
            cv_pca_biplot_multi_striplabel_size = input$cv_pca_biplot_multi_striplabel_size,
            cv_pca_rightside_y = input$cv_pca_rightside_y,
            cv_pca_x_tick_label_size = input$cv_pca_x_tick_label_size,
            cv_pca_y_tick_label_size = input$cv_pca_y_tick_label_size,
            cv_pca_width = input$cv_pca_width,
            cv_pca_height = input$cv_pca_height,

            cv_uni_fdr = input$cv_uni_fdr,
            cv_uni_alpha = input$cv_uni_alpha,
            cv_uni_fold_change = input$cv_uni_fold_change,
            cv_volcano_n_top_connections = input$cv_volcano_n_top_connections,
            cv_volcano_symbol_size = input$cv_volcano_symbol_size,
            cv_volcano_sig_colour = paste0('"', input$cv_volcano_sig_colour, '"'),
            cv_volcano_nonsig_colour = paste0('"', input$cv_volcano_nonsig_colour, '"'),
            cv_volcano_x_text_size = input$cv_volcano_x_text_size,
            cv_volcano_y_text_size = input$cv_volcano_y_text_size,
            cv_volcano_width = input$cv_volcano_width,
            cv_volcano_height = input$cv_volcano_height,

            cv_sig_htmap_textsize_col = input$cv_sig_htmap_textsize_col,
            cv_sig_htmap_textangle_col = input$cv_sig_htmap_textangle_col,
            cv_sig_htmap_textsize_row = input$cv_sig_htmap_textsize_row,
            cv_sig_htmap_keysize = input$cv_sig_htmap_keysize,
            cv_sig_htmap_key_xlab = paste0('"', input$cv_sig_htmap_key_xlab, '"'),
            cv_sig_htmap_key_ylab = paste0('"', input$cv_sig_htmap_key_ylab, '"'),
            cv_sig_htmap_margin = paste0('"c', input$cv_sig_htmap_margin, '"'),
            cv_sig_htmap_width = input$cv_sig_htmap_width,
            cv_sig_htmap_height = input$cv_sig_htmap_height,
            cv_sig_pca_pc = paste0('"c', input$cv_sig_pca_pc, '"'),
            cv_sig_pca_biplot_ellipse_conf = input$cv_sig_pca_biplot_ellipse_conf,

            cv_cpu_cluster = paste0('"', input$cv_cpu_cluster, '"'),

            cv_training_percentage = input$cv_training_percentage,

            cv_svm_cv_center_scale = input$cv_svm_cv_center_scale,
            cv_svm_cv_kernel = paste0('"', input$cv_svm_cv_kernel, '"'),
            cv_svm_cv_cross_k = input$cv_svm_cv_cross_k,
            cv_svm_cv_tune_method = paste0('"', input$cv_svm_cv_tune_method, '"'),
            cv_svm_cv_tune_cross_k = input$cv_svm_cv_tune_cross_k,
            cv_svm_cv_tune_boot_n = input$cv_svm_cv_tune_boot_n,
            cv_svm_cv_fs_rf_ifs_ntree = input$cv_svm_cv_fs_rf_ifs_ntree,
            cv_svm_cv_fs_rf_sfs_ntree = input$cv_svm_cv_fs_rf_sfs_ntree,
            cv_svm_cv_best_model_method = paste0('"', input$cv_svm_cv_best_model_method, '"'),
            cv_svm_cv_fs_count_cutoff = input$cv_svm_cv_fs_count_cutoff,

            cv_svm_cross_k = input$cv_svm_cross_k,
            cv_svm_tune_cross_k = input$cv_svm_tune_cross_k,

            cv_svm_tune_boot_n = input$cv_svm_tune_boot_n,

            cv_svm_perm_method = paste0('"', input$cv_svm_perm_method, '"'),
            cv_svm_perm_n = input$cv_svm_perm_n,
            cv_svm_perm_plot_symbol_size = input$cv_svm_perm_plot_symbol_size,
            cv_svm_perm_plot_legend_size = input$cv_svm_perm_plot_legend_size,
            cv_svm_perm_plot_x_label_size = input$cv_svm_perm_plot_x_label_size,
            cv_svm_perm_plot_x_tick_label_size = input$cv_svm_perm_plot_x_tick_label_size,
            cv_svm_perm_plot_y_label_size = input$cv_svm_perm_plot_y_label_size,
            cv_svm_perm_plot_y_tick_label_size = input$cv_svm_perm_plot_y_tick_label_size,
            cv_svm_perm_plot_width = input$cv_svm_perm_plot_width,
            cv_svm_perm_plot_height = input$cv_svm_perm_plot_height,

            cv_svm_roc_smooth = input$cv_svm_roc_smooth,
            cv_svm_roc_symbol_size = input$cv_svm_roc_symbol_size,
            cv_svm_roc_legend_size = input$cv_svm_roc_legend_size,
            cv_svm_roc_x_label_size = input$cv_svm_roc_x_label_size,
            cv_svm_roc_x_tick_label_size = input$cv_svm_roc_x_tick_label_size,
            cv_svm_roc_y_label_size = input$cv_svm_roc_y_label_size,
            cv_svm_roc_y_tick_label_size = input$cv_svm_roc_y_tick_label_size,
            cv_svm_roc_width = input$cv_svm_roc_width,
            cv_svm_roc_height = input$cv_svm_roc_height,

            cv_svm_rffs_pca_pc = paste0('"c', input$cv_svm_rffs_pca_pc, '"'),
            cv_svm_rffs_pca_biplot_ellipse_conf = input$cv_svm_rffs_pca_biplot_ellipse_conf,

            cv_rffs_htmap_textsize_col = input$cv_rffs_htmap_textsize_col,
            cv_rffs_htmap_textangle_col = input$cv_rffs_htmap_textangle_col,
            cv_rffs_htmap_textsize_row = input$cv_rffs_htmap_textsize_row,
            cv_rffs_htmap_keysize = input$cv_rffs_htmap_keysize,
            cv_rffs_htmap_key_xlab = input$cv_rffs_htmap_key_xlab,
            cv_rffs_htmap_key_ylab = input$cv_rffs_htmap_key_ylab,
            cv_rffs_htmap_margin = paste0('"c', input$cv_rffs_htmap_margin, '"'),
            cv_rffs_htmap_width = input$cv_rffs_htmap_width,
            cv_rffs_htmap_height = input$cv_rffs_htmap_height,

            cv_plsda_validation = paste0('"', input$cv_plsda_validation, '"'),
            cv_plsda_validation_segment = input$cv_plsda_validation_segment,

            cv_plsda_init_ncomp = input$cv_plsda_init_ncomp,

            cv_plsda_ncomp_select_method = paste0('"', input$cv_plsda_ncomp_select_method, '"'),
            cv_plsda_ncomp_select_plot_symbol_size = input$cv_plsda_ncomp_select_plot_symbol_size,
            cv_plsda_ncomp_select_plot_legend_size = input$cv_plsda_ncomp_select_plot_legend_size,
            cv_plsda_ncomp_select_plot_x_label_size = input$cv_plsda_ncomp_select_plot_x_label_size,
            cv_plsda_ncomp_select_plot_x_tick_label_size = input$cv_plsda_ncomp_select_plot_x_tick_label_size,
            cv_plsda_ncomp_select_plot_y_label_size = input$cv_plsda_ncomp_select_plot_y_label_size,
            cv_plsda_ncomp_select_plot_y_tick_label_size = input$cv_plsda_ncomp_select_plot_y_tick_label_size,

            cv_plsda_perm_method = paste0('"', input$cv_plsda_perm_method, '"'),
            cv_plsda_perm_n = input$cv_plsda_perm_n,
            cv_plsda_perm_plot_symbol_size = input$cv_plsda_perm_plot_symbol_size,
            cv_plsda_perm_plot_legend_size = input$cv_plsda_perm_plot_legend_size,
            cv_plsda_perm_plot_x_label_size = input$cv_plsda_perm_plot_x_label_size,
            cv_plsda_perm_plot_x_tick_label_size = input$cv_plsda_perm_plot_x_tick_label_size,
            cv_plsda_perm_plot_y_label_size = input$cv_plsda_perm_plot_y_label_size,
            cv_plsda_perm_plot_y_tick_label_size = input$cv_plsda_perm_plot_y_tick_label_size,
            cv_plsda_perm_plot_width = input$cv_plsda_perm_plot_width,
            cv_plsda_perm_plot_height = input$cv_plsda_perm_plot_height,

            cv_plsda_scoreplot_ellipse_conf = input$cv_plsda_scoreplot_ellipse_conf,

            cv_plsda_roc_smooth = input$cv_plsda_roc_smooth,

            cv_plsda_vip_alpha = input$cv_plsda_vip_alpha,
            cv_plsda_vip_boot = input$cv_plsda_vip_boot,
            cv_plsda_vip_boot_n = input$cv_plsda_vip_boot_n,
            cv_plsda_vip_plot_errorbar = paste0('"', input$cv_plsda_vip_plot_errorbar, '"'),
            cv_plsda_vip_plot_errorbar_width = input$cv_plsda_vip_plot_errorbar_width,
            cv_plsda_vip_plot_errorbar_label_size = input$cv_plsda_vip_plot_errorbar_label_size,
            cv_plsda_vip_plot_x_textangle = input$cv_plsda_vip_plot_x_textangle,
            cv_plsda_vip_plot_x_label_size = input$cv_plsda_vip_plot_x_label_size,
            cv_plsda_vip_plot_x_tick_label_size = input$cv_plsda_vip_plot_x_tick_label_size,
            cv_plsda_vip_plot_y_label_size = input$cv_plsda_vip_plot_y_label_size,
            cv_plsda_vip_plot_y_tick_label_size = input$cv_plsda_vip_plot_y_tick_label_size,
            cv_plsda_vip_plot_width = input$cv_plsda_vip_plot_width,
            cv_plsda_vip_plot_height = input$cv_plsda_vip_plot_height
        )
        # configs_format(configs)
    })


    cv_downloadConfigsData <- reactive({
        configs <- configs_format(cv_inputConfigs())
    })

    output$cv_download_configs <- downloadHandler(
        # configs <- configs_format(inputConfigs()),
        filename = "configs",
        content = function(file) {
            write.table(
                cv_downloadConfigsData(),
                file = file,
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE
            )
            # file.copy(file.path(tempdir(),'OUTPUT','configs'), file)
        },
        contentType = "application"
    )

    observeEvent(input$cv_reset_cnfg, {
        shinyjs::reset("cv_configurations")
    })


    output$cv_debug <-
        renderText(paste0("Debug: ", input$cv_contrast_group1[2]))


    cv_contrast <- reactiveValues()

    observeEvent(input$cv_confirmcontrast, {
        #check that they're the same length
        contrast1 <- input$cv_contrast_group1
        contrast2 <- input$cv_contrast_group2
        print("CONTRAST BUTTON!!!")
        print(contrast1)
        output <- 0
        if (length(contrast1) != length(contrast2)) {
            shinyalert(
                title = "Unequal groups!",
                type = "warning",
                text = paste(
                    "Group 1:",
                    length(contrast1),
                    "items; Group 2:",
                    length(contrast2),
                    "items."
                )
            )
        } else if (length(contrast1) == 0 | length(contrast2) == 0) {
            shinyalert(
                title = "No contrast variables chosen!",
                type = "warning",
                text = paste(
                    "Please choose at least one contrast variable per group."
                )
            )
        } else {
            for (i in 1:length(contrast1)) {
                output[i] <- paste0(contrast1[i], " - ", contrast2[i])
            }
            output <- paste(output, collapse = ',', sep = '')
            cv_contrast$groups <- output

        }

        enable("cv_contrast_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_contrast_check",
                             value = TRUE)

    })


    observeEvent(input$cv_choose_vars, {
        if(cv_xt == "csv"){
            varNames <- c(colnames(cv_training())[input$cv_variables_csv_rows_selected[1]], colnames(cv_training())[input$cv_variables_csv_rows_selected[2]])
        } else if (cv_xt == "mat") {
            varNames <- c(input$cv_chosen_sample, input$cv_chosen_group, input$cv_chosen_node, input$cv_chosen_region)
        }

        # varNames <- names(variableNames)
        print(varNames)
        vars = "You chose: "
        for (i in 1:length(varNames)) {
            v = varNames[i]
            vars = paste0(vars, v, "; ", sep = "", collapse = NULL)
        }
        showModal(
            modalDialog(
                title = "Loaded Variables",
                sprintf(vars),
                easyClose = FALSE,
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("cv_continue", "Continue")
                )
            )
        )
        print(cv_contrast$groups[1])
        enable("cv_variable_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_variable_check",
                             value = TRUE)
    })

    cv_raw_sample_dfm <- reactiveValues()

    # clicking the modal "Continue Button"
    observeEvent(input$cv_continue, {
        matfile <- input$cv_file_data$datapath
        matfilenoext <-
            tools::file_path_sans_ext(input$cv_file_data$name)
        dir.create(file.path(tempdir(), '/OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), '/OUTPUT', 'CV_ONLY'), showWarnings = FALSE)

        if (cv_xt == "csv") {
            samplesvar <- colnames(cv_training())[input$cv_variables_csv_rows_selected[1]]
            groupvar <- colnames(cv_training())[input$cv_variables_csv_rows_selected[2]]
        } else if (cv_xt == "mat") {
            annotfile <- input$cv_file_annotations$datapath
            samplesvar <- input$cv_chosen_sample
            groupvar <- input$cv_chosen_group
        }

        outdir <-
            paste0(tempdir(), '/OUTPUT/CV_ONLY')

        if (cv_xt == "csv") {
            print("2D file")
            proc_data <- inputDatProcess2d(
                c(
                    "y",
                    "d",
                    "c",
                    "a",
                    "s",
                    matfile,
                    matfilenoext,
                    samplesvar,
                    groupvar,
                    outdir
                )
            )
        } else if (cv_xt == "mat") {
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

        }


        raw_sample_dfm$cv_raw_sample_dfm <- proc_data[1]
        raw_sample_dfm$cv_raw_sample_dfm_wo_uni <- proc_data[2]
        removeModal()

        enable("cv_inputdata_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_inputdata_check",
                             value = TRUE)
    })

    # upload configurations from file
    cv_uploaded_configs <- reactive({
        req(input$cv_up_configs)
        configs <- getConfigsFromFile(input$cv_up_configs$datapath)

        if (length(configs) == 4) {
            output$cv_result <-
                renderText(paste0("missing variables: ", configs[4][[1]]))
            shinyalert(
                title = "Missing Variables, Choose from dialog",
                type = "warning",
                text = sprintf(configs[4][[1]])
            )
        }
        return(configs)
    })


    observeEvent(input$cv_up_configs, {
        req(input$cv_up_configs)
        configs <-
            cv_uploaded_configs()[1][[1]]   #column 1 is key/value pair, column 2 is values, column 3 is variable names
        configFileToUI(configs, session)
        # updateTextInput(session=session, inputId = "htmap_textsize_col", value = configs[2])
        # print(configs[2])
    })

    # Univariate button
    observeEvent(input$cv_univariate, {
        if(cv_xt == "mat") {
            req(input$cv_file_data, input$cv_file_annotations, input$cv_file_node)}
        else if (cv_xt == "csv") { req(input$cv_file_data)}

        do.call(file.remove, list(list.files(
            file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE'),
            full.names = TRUE, recursive = TRUE
        )))

        matfilenoext <-
            tools::file_path_sans_ext(input$cv_file_data$name)
        dat_file <-
            paste0(tempdir(), "/OUTPUT/CV_ONLY/", matfilenoext, "_2D.csv")

        nodefile <- input$cv_file_node$datapath
        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE')
        # configs <- uploaded_configs()[2][[1]]
        print(cv_contrast$groups)
        contrast <- cv_contrast$groups
        node_id_var <- input$cv_chosen_node
        region_name_var <- input$cv_chosen_region

        # getting values from input fields into an args[] fromat
        inputArgs <- univariateConfigs(inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[9] <- outdir
        inputArgs[38] <- contrast

        if(cv_xt == "mat"){
            inputArgs[8] <- nodefile
            inputArgs[61] <- node_id_var
            inputArgs[62] <- region_name_var
        }

        show_modal_spinner(text = "Processing...")
        withConsoleRedirect("cv_console", {
            if(cv_xt == "mat"){
                univariate(inputArgs)
            } else if (cv_xt == "csv") {
                suppressWarnings(univariate_2D(inputArgs))

            }

        })
        remove_modal_spinner()
        setwd(tempdir())

        enable("cv_univariate_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_univariate_check",
                             value = TRUE)

        # converting PDF results to PNGs to display in renderImage()
        if (file.exists(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE', 'Rplots.pdf'))){
            print("Rplots.pdf exists!, deleting now")
        } else {
            print("Rplots.pdf does not exist!")
        }
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE'),
                       pattern = ".pdf$", full.names=TRUE)
        
        lapply(
            pdf.list,
            FUN = function(files) {
                if (basename(files) == "Rplots.pdf") {
                    NULL
                } else {
                    pdf_convert(
                        files,
                        format = "png",
                        filenames = paste0(
                            dirname(files),
                            "/PNGFILES/" ,
                            tools::file_path_sans_ext(basename(files)),
                            ".png"
                        )
                    )
                }
            }
        )

        # #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = normalizePath(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'UNIVARIATE', 'PNGFILES')),
                pattern = ".png$",
                full.names = FALSE
            )
        print(filenames)
        updateSelectInput(session = session,
                          inputId = "cv_select_uni_plot",
                          choices = filenames)

    })
    
    # SVM button
    observeEvent(input$cv_svm, {
        if(cv_xt == "mat") {req(input$cv_file_data, input$cv_file_annotations, input$cv_file_node)}
        else if (cv_xt == "csv") { req(input$cv_file_data)}

        matfilenoext <-
            tools::file_path_sans_ext(input$cv_file_data$name)

        dat_file <- switch(
            input$cv_modeltype,
            cv_noneset = paste0(tempdir(), "/OUTPUT/CV_ONLY/", matfilenoext, "_2D_wo_uni.csv"),
            cv_kset = paste0(tempdir(), "/OUTPUT/CV_ONLY/UNIVARIATE/", matfilenoext, "_ml.csv"),
            cv_uset = paste0(tempdir(), "/OUTPUT/CV_ONLY/", matfilenoext, "_2D_wo_uni.csv")
        )

        cvuni <- switch(
            input$cv_modeltype,
            cv_noneset = FALSE,
            cv_kset = FALSE,
            cv_uset = TRUE
        )

        do.call(file.remove, list(list.files(
            file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM'),
            full.names = TRUE, recursive = TRUE
        )))

        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM')

        nodefile <- input$cv_file_node$datapath
        # configs <- uploaded_configs()[2][[1]]
        contrast <- cv_contrast$groups[1]
        psetting = FALSE
        if (isTruthy(input$cv_cores)){
            paste0("parallel computing", input$cv_cores)
            psetting <- TRUE
            cores <- input$cv_cores
        } else {
            print("no parallel computing")
        }


        # getting values from input fields into an args[] fromat
        inputArgs <- ml_svmConfigs(inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        inputArgs[9] <- psetting
        inputArgs[10] <- cores
        inputArgs[62] <- cvuni
        inputArgs[64] <- contrast

        show_modal_spinner(text = "Running classifier...")
        withConsoleRedirect("cv_console", {
            cv_ml_svm(inputArgs)
        })

        print("PLSDA ANALYSIS")
        plsdainputArgs <- plsdaConfigs(inputConfigs())
        plsdainputArgs[6] <- paste0(outdir, '/', 'cv_only_', matfilenoext, '_final_svm_model.Rdata')
        plsdainputArgs[7] <- matfilenoext
        plsdainputArgs[8] <- outdir
        plsdainputArgs[9] <- psetting
        plsdainputArgs[10] <- cores

        cv_plsda_val_svm(plsdainputArgs)

        remove_modal_spinner()


        # converting PDF results to PNGs to display in renderImage()
        if (file.exists(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM', 'Rplots.pdf'))){
            print("Rplots.pdf exists!, deleting now")
        } else {
            print("Rplots.pdf does not exist!")
        }
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM'),
                       pattern = ".pdf$", full.names = TRUE)
        
        lapply(
            pdf.list,
            FUN = function(files) {
                if (basename(files) == "Rplots.pdf") {
                    NULL
                } else {
                    pdf_convert(
                        files,
                        format = "png",
                        filenames = paste0(
                            dirname(files),
                            "/PNGFILES/" ,
                            tools::file_path_sans_ext(basename(files)),
                            ".png"
                        )
                    )
                }
            }
        )

        #updating the Select Input with the png filenames
        files <-
            list.files(
                path = file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'ML_SVM', 'PNGFILES'),
                pattern = ".png$",
                full.names = FALSE
            )
        print(files)
        updateSelectInput(session = session,
                          inputId = "cv_select_svm_plot",
                          choices = files)

        enable("cv_svm_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_svm_check",
                             value = TRUE)


    })

    # viewing plots
    output$cv_uni_plots <- renderImage({
        req(input$cv_univariate)
        setwd(tempdir())
        suppressWarnings(list(
            src = normalizePath(file.path(
                tempdir(),
                'OUTPUT',
                'CV_ONLY',
                'UNIVARIATE',
                'PNGFILES',
                input$cv_select_uni_plot
            )),
            alt = "Univariate plots",
            width = 400,
            height = 400
        ))

    }, deleteFile = FALSE)

    output$cv_svm_plots <- renderImage({
        req(input$cv_svm)
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'CV_ONLY',
                'ML_SVM',
                'PNGFILES',
                input$cv_select_svm_plot
            ),
            alt = "SVM plots",
            width = 400,
            height = 400
        )

    }, deleteFile = FALSE)


    output$cv_report <- downloadHandler(

        filename = function() {
            paste("report-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
            setwd('/srv/shiny-server/')
            tempReport <- file.path(tempdir(), 'results.Rmd')
            file.copy(file.path('srv', 'shiny-server', 'results.Rmd'), tempReport, overwrite = TRUE)
            rmarkdown::render(input = 'results.Rmd')
            # tinytex::pdflatex('results.tex')
            file.copy(file.path('results.pdf'), file)
        }

    )

    output$cv_allfiles <- downloadHandler(

        filename = function() {
            paste("allfiles-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
            tempzip <- file.path(tempdir(), "results.zip")
            zipr(tempzip, file.path(tempdir(), 'OUTPUT'))
            file.copy(tempzip, file)
        }
    )
    
    ################################### CV-ONLY Regression ANALYSIS #####################################
    # checking and loading only the mat file
    output$cv_reg_debug <-
        renderText(paste0("Debug: ", input$cv_reg_cpu_cluster))
    
    cv_reg_training <- reactive({
        req(input$cv_reg_file_data)
        cv_reg_xt <<- tools::file_ext(input$cv_reg_file_data$datapath)
        print(cv_reg_xt)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$cv_reg_file_data$name))
        if (cv_reg_xt == "mat") {
            df <- readMat(input$cv_reg_file_data$datapath)
            df <- df[[1]]
            dater[[1, 1]] <- df
            # return(dater)
        } else if (cv_reg_xt == "csv") {
            df <- read.csv(input$cv_reg_file_data$datapath,
                           header = TRUE,
                           sep = ",")
            # return(df)
            dater[[1, 1]] <- df
        }
        return(dater)
    })
    
    cv_reg_annotations <- reactive({
        cv_reg_xt <<- tools::file_ext(input$cv_reg_file_data$datapath)
        if (cv_reg_xt == "csv"){
            req(input$cv_reg_file_data)
            print("annotations 2D data!!")
            dater <- data.frame(cv_training()[2])
            return(dater)
        }
        else {
            req(input$cv_reg_file_annotations)
            dater = matrix(list(),
                           nrow = 1,
                           ncol = 1)
            df <- read.csv(input$cv_reg_file_annotations$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
            return(dater)
        }
        
    })
    
    cv_reg_node <- reactive({
        req(cv_reg_xt)
        if(cv_reg_xt == "csv"){
            print("node 2D data!!")
        }
        else{
            req(input$cv_reg_file_node)
            dater = matrix(list(),
                           nrow = 1,
                           ncol = 1)
            df <- read.csv(input$cv_reg_file_node$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
            return(dater)
        }
        
    })
    
    output$cv_reg_training <- DT::renderDataTable({
        cv_reg_xt <<- tools::file_ext(input$cv_reg_file_data$datapath)
        req(input$cv_reg_file_data)
        if (cv_reg_xt == "csv"){
            d <- cv_reg_training()[[1, 1]]
        } else if (cv_reg_xt == "mat"){
            d <- cv_reg_training()[[1, 1]][, , 1]
        }
        datatable(
            d,
            colnames = NULL,
            options = list(
                autoWidth = TRUE,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollY = TRUE,
                searching = FALSE
            ),
            selection = list(target = 'column')
        ) 
        # %>% formatRound(c(1:90), 3)
    })
    
    output$cv_reg_annotation <- DT::renderDataTable({
        req(cv_reg_xt)
        datatable(
            if(cv_reg_xt == "csv"){
                cv_reg_annotations()
            } else {
                cv_reg_annotations()[[1, 1]]
            },
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })
    
    output$cv_reg_node <- DT::renderDataTable({
        req(cv_reg_xt)
        datatable(
            if(cv_reg_xt == "csv"){
                
            } else {
                cv_reg_node()[[1, 1]]
            },
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })
    
    # rendering the drag and drop with variable names
    
    output$cv_reg_variables_csv <- DT::renderDataTable({
        d <- cv_reg_training()[[1, 1]]
        
        labs <- c(colnames(d))
        datatable(
            matrix(labs),
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
        
    })
    
    output$cv_reg_samplevar <- renderText({
        d <- cv_reg_training()[[1, 1]]
        paste("Selected Sample Variable: ", colnames(d)[input$cv_reg_variables_csv_rows_selected[1]])
    })
    output$reg_groupvar <- renderText({
        d <- reg_training()[[1, 1]]
        paste("Selected Group Variable: ", colnames(d)[input$cv_reg_variables_csv_rows_selected[2]])
    })
    
    output$cv_reg_variables <- renderUI({
        req(input$cv_reg_file_data)
        cv_reg_xt <<- tools::file_ext(input$cv_reg_file_data$datapath)
        if (cv_reg_xt == "csv"){
            
            box(
                title = "File Variables from column names",
                status = "primary",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                dataTableOutput("cv_reg_variables_csv"),
                verbatimTextOutput("cv_reg_samplevar"),
                verbatimTextOutput("cv_reg_groupvar"),
                actionButton(inputId = "cv_reg_choose_vars", label = "Confirm Variables")
            )
            
            # d <- read.csv(input$reg_file_data$datapath,
            #               header = TRUE,
            #               sep = ",")
            # labs <- c(colnames(d[1:2]))
            # # print(colnames(d))
            # useShinyjs()
            # tagList(
            #     box(
            #         title = "File Variables",
            #         status = "primary",
            #         width = 12,
            #         collapsible = TRUE,
            #         solidHeader = TRUE,
            # 
            #         div ( style = 'overflow-y:scroll; max-height: 382px;',
            #               fluidRow(
            #                   class = "panel-body",
            #                   column(
            #                       width = 4,
            #                       tags$div(
            #                           class = "panel panel-default",
            #                           tags$div(class = "panel-heading",
            #                                    "File Variables"),
            #                           tags$div(class = "panel-body av_vars_body",
            #                                    id = "reg_availablevars",
            #                                    icon_list(labs)),
            #                           tags$style(HTML('
            #                             '))
            #                       )
            #                   ),
            #                   column(
            #                       width = 4,
            #                       tags$div(
            #                           class = "panel panel-default",
            #                           tags$div(class = "panel-heading",
            #                                    "Sample"),
            #                           tags$div(class = "panel-body",
            #                                    id = "reg_chosen_sample")
            #                       ),
            #                       tags$div(
            #                           class = "panel panel-default",
            #                           tags$div(class = "panel-heading",
            #                                    "Group"),
            #                           tags$div(class = "panel-body",
            #                                    id = "reg_chosen_group")
            #                       )
            #                   ),
            #                   # column(
            #                   #     width = 4,
            #                   #     tags$div(
            #                   #         class = "panel panel-default",
            #                   #         tags$div(class = "panel-heading",
            #                   #                  "Node ID"),
            #                   #         tags$div(class = "panel-body",
            #                   #                  id = "chosen_node")
            #                   #     ),
            #                   #     tags$div(
            #                   #         class = "panel panel-default",
            #                   #         tags$div(class = "panel-heading",
            #                   #                  "Region"),
            #                   #         tags$div(class = "panel-body",
            #                   #                  id = "chosen_region")
            #                   #     )
            #                   # ),
            #                   #################### sortable code for drag and drop
            #                   sortable_js(
            #                       "reg_availablevars",
            #                       options = sortable_options(
            #                           group = list(
            #                               pull = TRUE,
            #                               name = "reg_allvars",
            #                               put = TRUE
            #                           ),
            #                           onSort = sortable_js_capture_input("sort_vars")
            #                       )
            # 
            #                   ),
            #                   sortable_js(
            #                       "reg_chosen_sample",
            #                       options = sortable_options(
            #                           group = list(
            #                               group = "reg_allvars",
            #                               put = htmlwidgets::JS(
            #                                   'function (to) { return to.el.children.length < 1; }'
            #                               ),
            #                               pull = htmlwidgets::JS(
            #                                   'function (to) { document.getElementById("reg_chosen_sample").style.backgroundColor = "white"; }'
            #                               )
            #                           ),
            #                           swapClass = "sortable-swap-highlight",
            #                           onSort = sortable_js_capture_input("reg_chosen_sample")
            #                       )
            #                   ),
            #                   sortable_js(
            #                       "reg_chosen_group",
            #                       options = sortable_options(
            #                           group = list(
            #                               group = "reg_allvars",
            #                               put = htmlwidgets::JS(
            #                                   'function (to) { return to.el.children.length < 1; }'
            #                               ),
            #                               pull = htmlwidgets::JS(
            #                                   'function (to) { document.getElementById("reg_chosen_group").style.backgroundColor = "white"; }'
            #                               )
            #                           ),
            #                           swapClass = "sortable-swap-highlight",
            #                           onSort = sortable_js_capture_input("reg_chosen_group")
            #                           # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
            #                       )
            #                   )
            #                   # sortable_js(
            #                   #     "chosen_node",
            #                   #     options = sortable_options(
            #                   #         group = list(
            #                   #             group = "allvars",
            #                   #             put = htmlwidgets::JS(
            #                   #                 'function (to) { return to.el.children.length < 1; }'
            #                   #             ),
            #                   #             pull = htmlwidgets::JS(
            #                   #                 'function (to) { document.getElementById("chosen_node").style.backgroundColor = "white"; }'
            #                   #             )
            #                   #         ),
            #                   #         swapClass = "sortable-swap-highlight",
            #                   #         onSort = sortable_js_capture_input("chosen_node")
            #                   #     )
            #                   # ),
            #                   # sortable_js(
            #                   #     "chosen_region",
            #                   #     options = sortable_options(
            #                   #         group = list(
            #                   #             group = "allvars",
            #                   #             put = htmlwidgets::JS(
            #                   #                 'function (to) { return to.el.children.length < 1; }'
            #                   #             ),
            #                   #             pull = htmlwidgets::JS(
            #                   #                 'function (to) { document.getElementById("chosen_region").style.backgroundColor = "white"; }'
            #                   #             )
            #                   #         ),
            #                   #         swapClass = "sortable-swap-highlight",
            #                   #         onSort = sortable_js_capture_input("chosen_region")
            #                   #     )
            #                   # )
            #               ),
            #               actionButton(inputId = "reg_choose_vars",
            #                            label = "Confirm Variables"),
            #               # tags$head(
            #               #     tags$style(
            #               #         "#box-body{overflow-y:scroll; max-height: 250px; background: ghostwhite;}"
            #               #     )
            #               # )
            #         )
            #     )
            # )
            
        } else if(cv_reg_xt == "mat") {
            req(input$cv_reg_file_annotations, input$cv_reg_file_node)
            labs <-
                c(colnames(cv_reg_annotations()[[1, 1]]), colnames(cv_reg_node()[[1, 1]]))
            print(labs)
            useShinyjs()
            tagList(
                box(
                    title = "File Variables",
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    
                    fluidRow(
                        class = "panel-body",
                        column(
                            width = 4,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "File Variables"),
                                tags$div(class = "panel-body av_vars_body",
                                         id = "cv_reg_availablevars",
                                         icon_list(labs)),
                                tags$style(HTML('
                                '))
                            )
                        ),
                        column(
                            width = 4,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Sample"),
                                tags$div(class = "panel-body",
                                         id = "cv_reg_chosen_sample")
                            ),
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Group"),
                                tags$div(class = "panel-body",
                                         id = "cv_reg_chosen_group")
                            ),
                        ),
                        column(
                            width = 4,
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Node ID"),
                                tags$div(class = "panel-body",
                                         id = "cv_reg_chosen_node")
                            ),
                            tags$div(
                                class = "panel panel-default",
                                tags$div(class = "panel-heading",
                                         "Region"),
                                tags$div(class = "panel-body",
                                         id = "cv_reg_chosen_region")
                            )
                        ),
                        #################### sortable code for drag and drop #####################
                        sortable_js(
                            "cv_reg_availablevars",
                            options = sortable_options(
                                group = list(
                                    pull = TRUE,
                                    name = "cv_reg_allvars",
                                    put = TRUE
                                ),
                                onSort = sortable_js_capture_input("cv_reg_sort_vars")
                            )
                            
                        ),
                        sortable_js(
                            "cv_reg_chosen_sample",
                            options = sortable_options(
                                group = list(
                                    group = "cv_reg_allvars",
                                    put = htmlwidgets::JS(
                                        'function (to) { return to.el.children.length < 1; }'
                                    ),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_reg_chosen_sample").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_reg_chosen_sample")
                            )
                        ),
                        sortable_js(
                            "cv_reg_chosen_group",
                            options = sortable_options(
                                group = list(
                                    group = "cv_reg_allvars",
                                    put = htmlwidgets::JS(
                                        'function (to) { return to.el.children.length < 1; }'
                                    ),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_reg_chosen_group").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_reg_chosen_group")
                                # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
                            )
                        ),
                        sortable_js(
                            "cv_reg_chosen_node",
                            options = sortable_options(
                                group = list(
                                    group = "cv_reg_allvars",
                                    put = htmlwidgets::JS(
                                        'function (to) { return to.el.children.length < 1; }'
                                    ),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_reg_chosen_node").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_reg_chosen_node")
                            )
                        ),
                        sortable_js(
                            "cv_reg_chosen_region",
                            options = sortable_options(
                                group = list(
                                    group = "cv_reg_allvars",
                                    put = htmlwidgets::JS(
                                        'function (to) { return to.el.children.length < 1; }'
                                    ),
                                    pull = htmlwidgets::JS(
                                        'function (to) { document.getElementById("cv_reg_chosen_region").style.backgroundColor = "white"; }'
                                    )
                                ),
                                swapClass = "sortable-swap-highlight",
                                onSort = sortable_js_capture_input("cv_reg_chosen_region")
                            )
                        )
                    ),
                    actionButton(inputId = "cv_reg_choose_vars",
                                 label = "Confirm Variables")
                )
            )
        }
    })
    
    # output$reg_groups <- renderUI({
    #     req(input$reg_file_data)
    #     reg_xt <<- tools::file_ext(input$reg_file_data$datapath)
    #     
    #     if(reg_xt == "csv"){
    #         groups <- data.frame(reg_annotations())
    #         groups <- count(groups$group)
    #         groupnames <- unique(groups$x)
    #         groupcount <- groups$freq
    #         numgroups <- length(groups)
    #         useShinyjs()
    #         tagList(
    #             box(
    #                 title = "Contrast variables",
    #                 status = "primary",
    #                 width = 12,
    #                 collapsible = TRUE,
    #                 solidHeader = TRUE,
    #                 fluidRow(
    #                     class = "panel-body",
    #                     column(
    #                         width = 3,
    #                         tags$div(
    #                             class = "panel panel-default",
    #                             tags$div(class = "panel-heading",
    #                                      "Group Variables"),
    #                             tags$div(
    #                                 class = "panel-body",
    #                                 id = "reg_availablegroups",
    #                                 icon_list(groupnames)
    #                             ),
    #                             tags$style(HTML('
    #                             '))
    #                         )
    #                     ),
    #                     column(
    #                         width = 3,
    #                         tags$div(
    #                             id = "reg_contrastgroup1",
    #                             class = "panel panel-default contrast1",
    #                             tags$div(class = "panel-heading",
    #                                      "Group 1"),
    #                             tags$div(class = "panel-body",
    #                                      id = "reg_contrast1")
    #                         )
    #                     ),
    #                     column(width = 1,
    #                            tags$div(class = "h1",
    #                                     "VS.")),
    #                     column(
    #                         width = 3,
    #                         tags$div(
    #                             class = "panel panel-default",
    #                             tags$div(class = "panel-heading",
    #                                      "Group 2"),
    #                             tags$div(class = "panel-body",
    #                                      id = "reg_contrast2")
    #                         )
    #                     ),
    #                     
    #                     column(
    #                         width = 2,
    #                         tags$div(
    #                             class = "panel panel-default",
    #                             tags$div(class = "panel-heading",
    #                                      icon("trash"),
    #                                      "Bin item"),
    #                             tags$div(class = "panel-body",
    #                                      id = "reg_trashbin")
    #                         )
    #                     )
    #                 ),
    #                 sortable_js(
    #                     "reg_availablegroups",
    #                     options = sortable_options(
    #                         group = list(
    #                             pull = "clone",
    #                             name = "reg_allsorts",
    #                             put = FALSE
    #                         ),
    #                         onSort = sortable_js_capture_input("reg_sort_vars")
    #                     )
    #                 ),
    #                 sortable_js(
    #                     "reg_contrast1",
    #                     options = sortable_options(
    #                         group = list(
    #                             group = "reg_allsorts",
    #                             put = TRUE,
    #                             pull = TRUE
    #                         ),
    #                         swapClass = "sortable-swap-highlight",
    #                         onSort = sortable_js_capture_input("reg_contrast_group1")
    #                         # onSort = htmlwidgets::JS('function (to) { var count = contrast1.children.length; console.log(count);  var c = contrast1.children;
    #                         #                      var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]
    #                         # 
    #                         #                      if (count >= 1){
    #                         #                      for (i = 0; i < count; i++){
    #                         #                      if (i%5 == 0) {
    #                         #                      var j = 0;
    #                         #                      }
    #                         #                      var color = colors[j]
    #                         #                      c[i].style.backgroundColor = color;
    #                         #                      j += 1;
    #                         #                      var text = c[i].innerText.match(/[a-zA-Z]+/g);
    #                         #                      c[i].innerText = i.toString().concat(". ", text);
    #                         #                      c[i].style.fontWeight = "bold";
    #                         #                      }
    #                         #                      console.log("yeeeeeehehehrerhq;lrehq;werh");
    #                         # 
    #                         #                      }}'),
    #                         # onAdd = sortable_js_capture_input("contrast_group1")
    #                     )
    #                     
    #                 ),
    #                 sortable_js(
    #                     "reg_contrast2",
    #                     options = sortable_options(
    #                         group = list(
    #                             group = "reg_allsorts",
    #                             put = TRUE,
    #                             pull = TRUE
    #                         ),
    #                         swapClass = "sortable-swap-highlight",
    #                         onSort = sortable_js_capture_input("reg_contrast_group2")
    #                         # onSort = htmlwidgets::JS('function (to) { var count = contrast2.children.length; console.log(count);  var c = contrast2.children;
    #                         #                      var colors = ["khaki", "indianred", "steelblue", "seagreen", "plum"]
    #                         # 
    #                         #                      if (count >= 1){
    #                         #                      for (i = 0; i < count; i++){
    #                         #                      if (i%5 == 0) {
    #                         #                      var j = 0;
    #                         #                      }
    #                         #                      var color = colors[j]
    #                         #                      c[i].style.backgroundColor = color;
    #                         #                      j += 1;
    #                         #                      var text = c[i].innerText.match(/[a-zA-Z]+/g);
    #                         #                      c[i].innerText = i.toString().concat(". ", text);
    #                         #                      c[i].style.fontWeight = "bold";
    #                         # 
    #                         #                      }
    #                         #                      console.log("yeeeeeehehehrerhq;lrehq;werh");
    #                         # 
    #                         #                      }}'),
    #                         # onAdd = sortable_js_capture_input("contrast_group2")
    #                     )
    #                     
    #                 ),
    #                 sortable_js(
    #                     "reg_trashbin",
    #                     options = sortable_options(
    #                         group = list(
    #                             group = "reg_allsorts",
    #                             put = TRUE,
    #                             pull = TRUE
    #                         ),
    #                         onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
    #                     )
    #                 ),
    #                 actionButton(inputId = "reg_confirmcontrast", label = "Confirm")
    #             )
    #         )
    #         
    #     } else if (reg_xt == "mat") {
    #     
    #     req(input$reg_file_annotations, input$reg_file_node)
    #     groups <- data.frame(reg_annotations()[[1, 1]])
    #     groups <- count(groups$group)
    #     groupnames <- unique(groups$x)
    #     groupcount <- groups$freq
    #     numgroups <- length(groups)
    #     useShinyjs()
    #     tagList(
    #         box(
    #             title = "Contrast variables",
    #             status = "primary",
    #             width = 12,
    #             collapsible = TRUE,
    #             solidHeader = TRUE,
    #             fluidRow(
    #                 class = "panel-body",
    #                 column(
    #                     width = 3,
    #                     tags$div(
    #                         class = "panel panel-default",
    #                         tags$div(class = "panel-heading",
    #                                  "Group Variables"),
    #                         tags$div(
    #                             class = "panel-body",
    #                             id = "reg_availablegroups",
    #                             icon_list(groupnames)
    #                         ),
    #                         tags$style(HTML('
    #                             '))
    #                     )
    #                 ),
    #                 column(
    #                     width = 3,
    #                     tags$div(
    #                         id = "reg_contrastgroup1",
    #                         class = "panel panel-default contrast1",
    #                         tags$div(class = "panel-heading",
    #                                  "Group 1"),
    #                         tags$div(class = "panel-body",
    #                                  id = "reg_contrast1"),
    #                     )
    #                 ),
    #                 column(width = 1,
    #                        tags$div(class = "h1",
    #                                 "VS.")),
    #                 column(
    #                     width = 3,
    #                     tags$div(
    #                         class = "panel panel-default",
    #                         tags$div(class = "panel-heading",
    #                                  "Group 2"),
    #                         tags$div(class = "panel-body",
    #                                  id = "reg_contrast2")
    #                     )
    #                 ),
    # 
    #                 column(
    #                     width = 2,
    #                     tags$div(
    #                         class = "panel panel-default",
    #                         tags$div(class = "panel-heading",
    #                                  icon("trash"),
    #                                  "Bin item"),
    #                         tags$div(class = "panel-body",
    #                                  id = "reg_trashbin")
    #                     )
    #                 )
    #             ),
    #             sortable_js(
    #                 "reg_availablegroups",
    #                 options = sortable_options(
    #                     group = list(
    #                         pull = "clone",
    #                         name = "reg_allsorts",
    #                         put = FALSE
    #                     ),
    #                     onSort = sortable_js_capture_input("reg_sort_vars")
    #                 )
    #             ),
    #             sortable_js(
    #                 "reg_contrast1",
    #                 options = sortable_options(
    #                     group = list(
    #                         group = "reg_allsorts",
    #                         put = TRUE,
    #                         pull = TRUE
    #                     ),
    #                     swapClass = "sortable-swap-highlight",
    #                     onSort = sortable_js_capture_input("reg_contrast_group1")
    #                     # onSort = htmlwidgets::JS('function (to) { var count = reg_contrast1.children.length; console.log(count);  var c = reg_contrast1.children;
    #                     #                          var colors = ["yellow", "indianred", "steelblue", "green", "magenta"]
    #                     #
    #                     #                          if (count >= 1){
    #                     #                          for (i = 0; i < count; i++){
    #                     #                          if (i%5 == 0) {
    #                     #                          var j = 0;
    #                     #                          }
    #                     #                          var color = colors[j]
    #                     #                          c[i].style.backgroundColor = color;
    #                     #                          j += 1;
    #                     #                          var text = c[i].innerText.match(/[a-zA-Z]+/g);
    #                     #                          c[i].innerText = i.toString().concat(". ", text);
    #                     #                          c[i].style.fontWeight = "bold";
    #                     #                          }
    #                     #                          console.log("yeeeeeehehehrerhq;lrehq;werh");
    #                     #
    #                     #                          }}')
    #                 )
    # 
    #             ),
    #             sortable_js(
    #                 "reg_contrast2",
    #                 options = sortable_options(
    #                     group = list(
    #                         group = "reg_allsorts",
    #                         put = TRUE,
    #                         pull = TRUE
    #                     ),
    #                     swapClass = "sortable-swap-highlight",
    #                     onSort = sortable_js_capture_input("reg_contrast_group2")
    #                     # onSort = htmlwidgets::JS('function (to) { var count = reg_contrast2.children.length; console.log(count);  var c = reg_contrast2.children;
    #                     #                          var colors = ["yellow", "indianred", "steelblue", "green", "magenta"]
    #                     #
    #                     #                          if (count >= 1){
    #                     #                          for (i = 0; i < count; i++){
    #                     #                          if (i%5 == 0) {
    #                     #                          var j = 0;
    #                     #                          }
    #                     #                          var color = colors[j]
    #                     #                          c[i].style.backgroundColor = color;
    #                     #                          j += 1;
    #                     #                          var text = c[i].innerText.match(/[a-zA-Z]+/g);
    #                     #                          c[i].innerText = i.toString().concat(". ", text);
    #                     #                          c[i].style.fontWeight = "bold";
    #                     #                          }
    #                     #                          console.log("yeeeeeehehehrerhq;lrehq;werh");
    #                     #
    #                     #                          }}')
    #                 )
    # 
    #             ),
    #             sortable_js(
    #                 "reg_trashbin",
    #                 options = sortable_options(
    #                     group = list(
    #                         group = "reg_allsorts",
    #                         put = TRUE,
    #                         pull = TRUE
    #                     ),
    #                     onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
    #                 )
    #             ),
    #             actionButton(inputId = "reg_confirmcontrast", label = "Confirm")
    #         )
    #     )
    #     }
    #     
    # })
    
    # reg_contrast <- reactiveValues()
    
    # observeEvent(input$reg_confirmcontrast, {
    #     #check that they're the same length
    #     contrast1 <- input$reg_contrast_group1
    #     contrast2 <- input$reg_contrast_group2
    #     output <- 0
    #     if (length(contrast1) != length(contrast2)) {
    #         shinyalert(
    #             title = "Unequal groups!",
    #             type = "warning",
    #             text = paste(
    #                 "Group 1:",
    #                 length(contrast1),
    #                 "items; Group 2:",
    #                 length(contrast2),
    #                 "items."
    #             )
    #         )
    #     } else if (length(contrast1) == 0 | length(contrast2) == 0) {
    #         shinyalert(
    #             title = "No contrast variables chosen!",
    #             type = "warning",
    #             text = paste(
    #                 "Please choose at least one contrast variable per group."
    #             )
    #         )
    #     } else {
    #         for (i in 1:length(contrast1)) {
    #             output[i] <- paste0(contrast1[i], " - ", contrast2[i])
    #         }
    #         output <- paste(output, collapse = ',', sep = '')
    #         reg_contrast$groups <- output
    # 
    #     }
    #     enable("reg_contrast_check")
    #     updatePrettyCheckbox(session = session,
    #                          inputId = "reg_contrast_check",
    #                          value = TRUE)
    # 
    # })
    
    observeEvent(input$cv_reg_choose_vars, {
        print("WE ARE CHOOSING VARIABLES!!!")
        print(cv_reg_xt)
        print(colnames(cv_reg_training()[[1, 1]][input$cv_reg_variables_csv_rows_selected[1]]))
        if(cv_reg_xt == "csv"){
            print(cv_reg_xt)
            varNames <- c(colnames(cv_reg_training()[[1, 1]])[input$cv_reg_variables_csv_rows_selected[1]], 
                          colnames(cv_reg_training()[[1, 1]])[input$cv_reg_variables_csv_rows_selected[2]])
            paste(varNames)
        } else if(cv_reg_xt == "mat"){
            varNames <- c(input$cv_reg_chosen_sample, input$cv_reg_chosen_group, input$cv_reg_chosen_node, input$cv_reg_chosen_region)
        }
        
        print(varNames)
        vars = "You chose: "
        for (i in 1:length(varNames)) {
            v = varNames[i]
            vars = paste0(vars, v, "; ", sep = "", collapse = NULL)
        }
        showModal(
            modalDialog(
                title = "Loaded Variables",
                sprintf(vars),
                easyClose = FALSE,
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("cv_reg_continue", "Continue")
                )
            )
        )
        # print(reg_contrast$groups)
        enable("cv_reg_variable_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_reg_variable_check",
                             value = TRUE)
    })
    
    observeEvent(input$cv_reg_continue, {
        matfile <- input$cv_reg_file_data$datapath
        matfilenoext <-
            tools::file_path_sans_ext(input$cv_reg_file_data$name)
        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION'), showWarnings = FALSE)
        
        if (cv_reg_xt == "csv"){
            samplesvar <- colnames(cv_reg_training()[[1, 1]])[input$cv_reg_variables_csv_rows_selected[1]]
            groupvar <- colnames(cv_reg_training()[[1, 1]])[input$cv_reg_variables_csv_rows_selected[2]]
        } else if (cv_reg_xt == "mat"){
            annotfile <- input$cv_reg_file_annotations$datapath
            samplesvar <- input$cv_reg_chosen_sample
            groupvar <- input$cv_reg_chosen_group
        }
        
        
        outdir <- paste0(tempdir(), '/OUTPUT/CV_ONLY/REGRESSION')
        
        if(cv_reg_xt == "mat") {
            proc_data <- reg_inputDatProcess(
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
        } else if (cv_reg_xt == "csv") {
            proc_data <- reg_input_dat_process_2D(
                c(
                    "y",
                    "d",
                    "c",
                    "a",
                    "s",
                    matfile,
                    matfilenoext,
                    samplesvar,
                    groupvar,
                    outdir
                )
            )
        }
        
        
        
        raw_sample_dfm$raw_sample_dfm <- proc_data[1]
        raw_sample_dfm$raw_sample_dfm_wo_uni <- proc_data[2]
        removeModal()
        
        
        # insertUI(
        #     selector = "#reg_configfilebutton",
        #     where = "afterEnd",
        #     ui = fileInput(
        #         inputId = "reg_up_configs",
        #         label = "Upload Config File",
        #         multiple = FALSE
        #     )
        # )
        enable("cv_reg_inputdata_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_reg_inputdata_check",
                             value = TRUE)
    })
    
    
    cv_normdata <- reactiveValues()
    
    # Univariate button
    observeEvent(input$cv_reg_univariate, {
        if(cv_reg_xt == "mat"){
            req(input$cv_reg_file_data, input$cv_reg_file_annotations, input$cv_reg_file_node)
        } else if (reg_xt == "csv"){
            print("UNIVARIATE  csv 2D")
            req(input$cv_reg_file_data)
        }
        
        
        # unlink(file.path(tempdir(), 'OUTPUT'), recursive = TRUE, force = TRUE)
        
        # do.call(file.remove, list(list.files(
        #     file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE'),
        #     full.names = TRUE, recursive = TRUE
        # )))
        
        matfilenoext <-
            tools::file_path_sans_ext(input$cv_reg_file_data$name)
        dat_file <-
            paste0(tempdir(), "/OUTPUT/CV_ONLY/REGRESSION/", matfilenoext, "_2D.csv")
        
        nodefile <- input$cv_reg_file_node$datapath
        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'UNIVARIATE'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'UNIVARIATE', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            normalizePath(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'UNIVARIATE'))
        node_id_var <- input$cv_reg_chosen_node
        region_name_var <- input$cv_reg_chosen_region
        
        # getting values from input fields into an args[] fromat
        inputArgs <- reg_univariateConfigs(reg_inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        
        
        if(cv_reg_xt == "mat"){
            inputArgs[32] <- nodefile
            inputArgs[33] <- node_id_var
            inputArgs[34] <- region_name_var
        }
        
        show_modal_spinner(text = "Processing...")
        withConsoleRedirect("cv_reg_console", {
            if(cv_reg_xt == "mat"){
                out <- reg_univariate(inputArgs)
            } else if (reg_xt == "csv"){
                reg_univariate_2D(inputArgs)
            }
        })
        remove_modal_spinner()
        
        # print(out)
        # setwd(dirname(rstudioapi::getSourceEditorContext()$path))
        # normdata$n <- data
        #
        # output$reg_plot <- renderPlotly({
        #     normdata <- out[[1]]
        #     fit_dfm <- out[[2]]
        
        #     dfm <- data.frame(normdata$genes, normdata$E)
        #     pcutoff <- input$reg_uni_alpha
        #     pb_name <- fit_dfm[fit_dfm$P.Value < pcutoff, 'ProbeName']
        #     dfm <- dfm[dfm[, 'ProbeName'] %in% pb_name, ]
        #     ogNcol <- dim(normdata$E)[2]
        #     annoNcol <- dim(dfm)[2]
        #     s <- (annoNcol - ogNcol + 1):annoNcol
        #     mtx <- as.matrix(dfm[, s])
        
        #     heatmaply(mtx)
        # })
        
        enable("cv_reg_univariate_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_reg_univariate_check",
                             value = TRUE)
        
        # converting PDF results to PNGs to display in renderImage()
        if (file.exists(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'UNIVARIATE', 'Rplots.pdf'))){
            print("Rplots.pdf exists!, deleting now")
        } else {
            print("Rplots.pdf does not exist!")
        }
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'UNIVARIATE'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                pdf_convert(
                    files,
                    format = "png",
                    filenames = paste0(
                        sep = "",
                        tools::file_path_sans_ext(files),
                        ".png"
                    )
                )
            }
        )
        #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = normalizePath(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'UNIVARIATE')),
                pattern = ".png$",
                full.names = FALSE
            )
        updateSelectInput(session = session,
                          inputId = "cv_reg_select_uni_plot",
                          choices = filenames)
        
    })
    
    # svm button
    observeEvent(input$cv_reg_svm, {
        if (cv_reg_xt == "mat"){
            req(input$cv_reg_file_data, input$cv_reg_file_annotations, input$cv_reg_file_node)
        } else if (cv_reg_xt == "csv"){
            req(input$cv_reg_file_data)
        }
        
        
        matfilenoext <-
            tools::file_path_sans_ext(input$cv_reg_file_data$name)
        
        dat_file <- switch(
            input$cv_reg_modeltype,
            cv_reg_noneset = paste0(tempdir(), "/OUTPUT/CV_ONLY/REGRESSION/", matfilenoext, "_2D.csv"),
            cv_reg_kset = paste0(tempdir(), "/OUTPUT/CV_ONLY/REGRESSION/UNIVARIATE/", matfilenoext, "_ml.csv"),
            cv_reg_uset = paste0(tempdir(), "/OUTPUT/CV_ONLY/REGRESSION/", matfilenoext, "_2D.csv")
        )
        
        # cvuni <- FALSE
        cvuni <- switch(
            input$cv_reg_modeltype,
            cv_reg_noneset = FALSE,
            cv_reg_kset = FALSE,
            cv_reg_uset = TRUE
        )
        print(cvuni)
        
        do.call(file.remove, list(list.files(
            file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'ML_SVM'),
            full.names = TRUE, recursive = TRUE
        )))
        
        dir.create(file.path(tempdir(), 'OUTPUT'), showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'ML_SVM'),
                   showWarnings = FALSE)
        dir.create(file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'ML_SVM', 'PNGFILES'),
                   showWarnings = FALSE)
        outdir <-
            file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'ML_SVM')
        
        nodefile <- input$cv_reg_file_node$datapath
        # configs <- uploaded_configs()[2][[1]]
        # contrast <- reg_contrast$groups[1]
        
        if (isTruthy(input$reg_cores)){
            paste0("parallel computing", input$reg_cores)
            psetting <- TRUE
            cores <- input$reg_cores
        } else {
            print("no parallel computing")
        }
        
        # getting values from input fields into an args[] format
        inputArgs <- reg_ml_svmConfigs(reg_inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        inputArgs[9] <- psetting
        inputArgs[10] <- cores
        inputArgs[56] <- cvuni
        
        # if(reg_xt == "mat"){
        #     inputArgs[64] <- contrast
        # }
        
        
        show_modal_spinner(text = "Running classifier...")
        # withConsoleRedirect("reg_console", {
        # invalidateLater(100)
        reg_ml_svm(inputArgs)
        
        # })
        remove_modal_spinner()
        
        # converting PDF results to PNGs to display in renderImage()
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'ML_SVM'),
                       pattern = ".pdf$")
        # 
        pdf_convert(pdf.list[1], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[1]), ".png"))
        pdf_convert(pdf.list[2], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[2]), ".png"))
        # pdf_convert(pdf.list[3], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[3]), ".png"))
        pdf_convert(pdf.list[4], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[4]), ".png"))
        pdf_convert(pdf.list[5], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[5]), ".png"))
        pdf_convert(pdf.list[6], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[6]), ".png"))
        pdf_convert(pdf.list[7], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[7]), ".png"))
        pdf_convert(pdf.list[8], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[8]), ".png"))
        pdf_convert(pdf.list[9], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[9]), ".png"))
        pdf_convert(pdf.list[10], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[10]), ".png"))
        pdf_convert(pdf.list[11], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[11]), ".png"))
        pdf_convert(pdf.list[12], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[12]), ".png"))
        pdf_convert(pdf.list[13], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[13]), ".png"))
        # pdf_convert(pdf.list[14], format="png", filenames=paste0(tools::file_path_sans_ext(pdf.list[14]), ".png"))
        # lapply(
        #     pdf.list,
        #     FUN = function(files) {
        #         pdf_convert(
        #             files,
        #             format = "png",
        #             filenames = paste0(
        #                 sep = "",
        #                 "PNGFILES/" ,
        #                 tools::file_path_sans_ext(files),
        #                 ".png"
        #             )
        #         )
        #     }
        # )
        
        #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = file.path(tempdir(), 'OUTPUT', 'CV_ONLY', 'REGRESSION', 'ML_SVM'),
                pattern = ".png$",
                full.names = FALSE
            )
        updateSelectInput(session = session,
                          inputId = "cv_reg_select_svm_plot",
                          choices = filenames)
        
        enable("cv_reg_svm_check")
        updatePrettyCheckbox(session = session,
                             inputId = "cv_reg_svm_check",
                             value = TRUE)
        closeAllConnections()
        
    })
    
    # viewing plots
    output$cv_reg_uni_plots <- renderImage({
        req(input$cv_reg_univariate)
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'CV_ONLY',
                'REGRESSION',
                'UNIVARIATE',
                input$cv_reg_select_uni_plot
            ),
            alt = "Univariate plots",
            width = 400,
            height = 400
        )
        
    }, deleteFile = FALSE)
    
    output$cv_reg_svm_plots <- renderImage({
        req(input$cv_reg_svm)
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'CV_ONLY',
                'REGRESSION',
                'ML_SVM',
                input$cv_reg_select_svm_plot
            ),
            alt = "SVM plots",
            width = 400,
            height = 400
        )
        
    }, deleteFile = FALSE)
    
    output$cv_reg_report <- downloadHandler(
        
        filename = function() {
            paste("regression_report-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
            setwd('/srv/shiny-server/')
            tempReport <- file.path(tempdir(), 'reg_results.Rmd')
            file.copy(file.path('srv', 'shiny-server', 'reg_results.Rmd'), tempReport, overwrite = TRUE)
            # print("YOOOSSS")
            # params <- list(n = 100)
            rmarkdown::render(input = 'reg_results.Rmd')
            file.copy(file.path('reg_results.pdf'), file)
        }
        
    )
    
    output$cv_reg_allfiles <- downloadHandler(
        
        filename = function() {
            paste("allfiles-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
            tempzip <- file.path(tempdir(), "results.zip")
            zipr(tempzip, file.path(tempdir(), 'OUTPUT'))
            file.copy(tempzip, file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)