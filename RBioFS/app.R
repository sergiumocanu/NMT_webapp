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
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinyjqui)
library(R.matlab)
library(plyr)
library(DT)
library(shinyBS)
library(sortable)
library(data.table)
library(pdftools)
library(rmarkdown)
library(zip)
# library(d3heatmap)
# library(heatmaply)
# library(threeBrain)
# library(plotly)

source("input_dat_process.R")
source("univariate.R")
source("defaultconfig.R")
source("ml_svm.R")
source("configs_format.R")
source("R/uiElements.R")
source("reg_input_dat_process.R")
source("reg_univariate.R")
source("reg_ml_svm.R")
source("input_dat_process_2d.R")
source("univariate_2D.R")


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
    dashboardHeader(title = "Connectivity Classification"),
    
    # sidebar menu items
    dashboardSidebar(sidebarMenu(
        menuItem(text = "SVM Analysis", tabName = "svm"),
        menuItem(text = "Regression Analysis", tabName = "regression")
    )),
    
    
    
    # Dashboard body items
    dashboardBody(tabItems(
        tabItem(
            tabName = "svm",
            # fluidRow(verbatimTextOutput("debug")),
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
                executeSVM("univariate", "svm", "modeltype", "withp", "withoutp"),
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
            
            fluidRow(
                box(
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Interactive Plots",
                    width = 6
                    # plotlyOutput('plot')
                )
            )
            
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
                    "reg_withp",
                    "reg_withoutp"
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
            ),
            
            fluidRow(
                box(
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Interactive Plots",
                    width = 6
                    # plotlyOutput('reg_plot')
                )
            )
            
            
        )
    ))
)



# =============================================================================
# Define server logic
server <- function(input, output, session) {
    
    
    
    # observe({
    #     files <- input$files$name
    #     updateSelectInput(session = session,
    #                       inputId = "select_file",
    #                       choices = files)

    # })
    # 
    # #updating the upload files checkbox after files have been uploaded
    # files <- reactive({
    #     list(input$file_data, input$file_annotations, input$file_node)
    # })
    # 
    # observeEvent(input$files, {
    #     enable("upload_check")
    #     updatePrettyCheckbox(session = session,
    #                          inputId = "upload_check",
    #                          value = TRUE)
    # })

    # checking and loading only the mat file
    # xt <<- ""
    training <- reactive({
        req(input$file_data)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$file_data$name))
        # for (i in 1:length(input$file_data$name)) {
            if (tools::file_ext(input$file_data$datapath) == "mat") {
                xt <<- tools::file_ext(input$file_data$datapath)
                df <- readMat(input$file_data$datapath)
                df <- df[[1]]
                dater[[1, 1]] <- df
                # return(dater)
            } else if (tools::file_ext(input$file_data$datapath) == "csv") {
                xt <<- tools::file_ext(input$file_data$datapath)
                # inputFile2d <<- TRUE
                # print("loading data FILE IS 2D!!!!!")
                df <- read.csv(input$file_data$datapath,
                               header = TRUE,
                               sep = ",")
                dater[[1, 1]] <- df
                
                # return(dater)
            }
        # }
        # return(dater)
    })

    annotations <- reactive({
        
        if (xt == "csv"){
            req(input$file_data)
            print("annotations 2D data!!")
            dater <- data.frame(training()[2])
            return(dater)
        } else {
            req(input$file_annotations)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = 1)
        df <- read.csv(input$file_annotations$datapath,
                       header = TRUE,
                       sep = ",")
        dater[[1, 1]] <- df
        return(dater)
        }
        
    })

    node <- reactive({
        if(xt == "csv"){
            print("node 2D data!!!!")
        } else {
        # req(input$file_node)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = 1)
        df <- read.csv(input$file_node$datapath,
                       header = TRUE,
                       sep = ",")
        dater[[1, 1]] <- df
        return(dater)
        }
    })


    # rendering the drag and drop with variable names

    output$variables <- renderUI({
        req(input$file_data)

        print(xt)
        if(xt == "csv"){

            print("file is 2d")

            labs <- c(colnames(training()[1:2]))
                    print("FILE IS 2D!!!!!!!!!!!!!")

                    useShinyjs()
                    tagList(
                        box(
                            title = "File Variables",
                            status = "primary",
                            width = 12,
                            collapsible = TRUE,
                            solidHeader = TRUE,

                            div ( style = 'overflow-y:scroll; max-height: 382px;',
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
                                                 "Group"),
                                        tags$div(class = "panel-body",
                                                 id = "chosen_group")
                                    )
                                ),
                                # column(
                                #     width = 4,
                                #     tags$div(
                                #         class = "panel panel-default",
                                #         tags$div(class = "panel-heading",
                                #                  "Node ID"),
                                #         tags$div(class = "panel-body",
                                #                  id = "chosen_node")
                                #     ),
                                #     tags$div(
                                #         class = "panel panel-default",
                                #         tags$div(class = "panel-heading",
                                #                  "Region"),
                                #         tags$div(class = "panel-body",
                                #                  id = "chosen_region")
                                #     )
                                # ),
                                #################### sortable code for drag and drop
                                sortable_js(
                                    "availablevars",
                                    options = sortable_options(
                                        group = list(
                                            pull = TRUE,
                                            name = "allvars",
                                            put = TRUE
                                        ),
                                        onSort = sortable_js_capture_input("sort_vars")
                                    )

                                ),
                                sortable_js(
                                    "chosen_sample",
                                    options = sortable_options(
                                        group = list(
                                            group = "allvars",
                                            put = htmlwidgets::JS(
                                                'function (to) { return to.el.children.length < 1; }'
                                            ),
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
                                            put = htmlwidgets::JS(
                                                'function (to) { return to.el.children.length < 1; }'
                                            ),
                                            pull = htmlwidgets::JS(
                                                'function (to) { document.getElementById("chosen_group").style.backgroundColor = "white"; }'
                                            )
                                        ),
                                        swapClass = "sortable-swap-highlight",
                                        onSort = sortable_js_capture_input("chosen_group")
                                        # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
                                    )
                                )
                                # sortable_js(
                                #     "chosen_node",
                                #     options = sortable_options(
                                #         group = list(
                                #             group = "allvars",
                                #             put = htmlwidgets::JS(
                                #                 'function (to) { return to.el.children.length < 1; }'
                                #             ),
                                #             pull = htmlwidgets::JS(
                                #                 'function (to) { document.getElementById("chosen_node").style.backgroundColor = "white"; }'
                                #             )
                                #         ),
                                #         swapClass = "sortable-swap-highlight",
                                #         onSort = sortable_js_capture_input("chosen_node")
                                #     )
                                # ),
                                # sortable_js(
                                #     "chosen_region",
                                #     options = sortable_options(
                                #         group = list(
                                #             group = "allvars",
                                #             put = htmlwidgets::JS(
                                #                 'function (to) { return to.el.children.length < 1; }'
                                #             ),
                                #             pull = htmlwidgets::JS(
                                #                 'function (to) { document.getElementById("chosen_region").style.backgroundColor = "white"; }'
                                #             )
                                #         ),
                                #         swapClass = "sortable-swap-highlight",
                                #         onSort = sortable_js_capture_input("chosen_region")
                                #     )
                                # )
                            ),
                            actionButton(inputId = "choose_vars",
                                         label = "Confirm Variables"),
                            # tags$head(
                            #     tags$style(
                            #         "#box-body{overflow-y:scroll; max-height: 250px; background: ghostwhite;}"
                            #     )
                            # )
                        )
                        )
                    )

        } else if(xt == "mat"){
            print("file is not 2d")

            req(input$file_annotations, input$file_node)
                    labs <-
                        c(colnames(annotations()[[1, 1]]), colnames(node()[[1, 1]]))

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
                                                 "Group"),
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
        if(xt == "csv"){
             print("contrast !! file 2d!")
            
            groups <- data.frame(annotations())
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
        groups <- data.frame(annotations()[[1, 1]])
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

    output$training <- DT::renderDataTable({
        datatable(
            # training()[[1, 1]][, , 1],
            training()[,1:40,1], ######### change this after
            
            options = list(
                autoWidth = TRUE,
                lengthChange = FALSE,
                scrollX = TRUE,
                scrollY = TRUE,
                searching = FALSE
            )
        ) %>%
            formatRound(columns = c(2:length(training())), 3)
    })

    output$annotation <- DT::renderDataTable({
        datatable(
            if(xt == "csv"){
                annotations()
            } else {
                annotations()[[1, 1]]
            },
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })

    output$node <- DT::renderDataTable({
        if(xt == "csv"){

        } else {
        datatable(
            node()[[1, 1]],
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
        }
    })

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
        varNames <- c(input$chosen_sample, input$chosen_group, input$chosen_node, input$chosen_region)
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
        annotfile <- input$file_annotations$datapath
        samplesvar <- input$chosen_sample
        groupvar <- input$chosen_group
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


        insertUI(
            selector = "#configfilebutton",
            where = "afterEnd",
            ui = fileInput(
                inputId = "up_configs",
                label = "Upload Config File",
                multiple = FALSE
            )
        )
        enable("inputdata_check")
        updatePrettyCheckbox(session = session,
                             inputId = "inputdata_check",
                             value = TRUE)
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

        withConsoleRedirect("console", {
            if(xt == "mat"){
                univariate(inputArgs)
            } else if (xt == "csv") {
                print("she's 2D!")
                univariate_2D(inputArgs)
            }
            
        })

        enable("univariate_check")
        updatePrettyCheckbox(session = session,
                             inputId = "univariate_check",
                             value = TRUE)

        # converting PDF results to PNGs to display in renderImage()
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'UNIVARIATE'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                pdf_convert(
                    files,
                    format = "png",
                    filenames = paste0(
                        sep = "",
                        "PNGFILES/" ,
                        tools::file_path_sans_ext(files),
                        ".png"
                    )
                )
            }
        )
        #updating the Select Input with the png filenames
        filenames <-
            list.files(
                path = file.path(tempdir(), 'OUTPUT', 'UNIVARIATE', 'PNGFILES'),
                pattern = ".png$",
                full.names = FALSE
            )
        updateSelectInput(session = session,
                          inputId = "select_uni_plot",
                          choices = filenames)

    })

    # SVM button
    observeEvent(input$svm, {
        if(xt == "mat") {req(input$file_data, input$file_annotations, input$file_node)}
        else if (xt == "csv") { req(input$file_data)}

        matfilenoext <-
            tools::file_path_sans_ext(input$file_data$name)

        dat_file <- switch(
            input$modeltype,
            withp = paste0(tempdir(), "/OUTPUT/UNIVARIATE/", matfilenoext, "_ml.csv"),
            withoutp = paste0(tempdir(), "/OUTPUT/", matfilenoext, "_2D_wo_uni.csv")
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
        psetting <- 3
        cores <- 2
        cvuni <- FALSE

        # getting values from input fields into an args[] fromat
        inputArgs <- ml_svmConfigs(inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        inputArgs[9] <- psetting
        inputArgs[10] <- cores
        inputArgs[62] <- cvuni
        inputArgs[64] <- contrast

        ml_svm(inputArgs)

        # converting PDF results to PNGs to display in renderImage()
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'ML_SVM'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                pdf_convert(
                    files,
                    format = "png",
                    filenames = paste0(
                        sep = "",
                        "PNGFILES/" ,
                        tools::file_path_sans_ext(files),
                        ".png"
                    )
                )
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
        list(
            src = file.path(
                tempdir(),
                'OUTPUT',
                'UNIVARIATE',
                'PNGFILES',
                input$select_uni_plot
            ),
            alt = "Univariate plots",
            width = 400,
            height = 400
        )

    }, deleteFile = FALSE)

    output$svm_plots <- renderImage({
        # req(input$svm)
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
            tempReport <- normalizePath(file.path(tempdir(), 'results.Rmd'))
            print("YOS!!!")
            file.copy(normalizePath(file.path('results.Rmd')), tempReport, overwrite = TRUE)
            params <- list(n = 100)
            rmarkdown::render(input = tempReport)
            file.copy(file.path(tempdir(), 'results.pdf'), file)
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
    
    


    ################################### Regression #####################################
    # checking and loading only the mat file
    output$reg_debug <-
        renderText(paste0("Debug: ", input$reg_cpu_cluster))

    reg_training <- reactive({
        req(input$reg_file_data)
        reg_xt <<- tools::file_ext(input$file_data$datapath) 
        dater = matrix(list(),
                       nrow = 1,
                       ncol = length(input$reg_file_data$name))
        # for (i in 1:length(input$file_data$name)) {
        if (reg_xt == "mat") {
            
            df <- readMat(input$reg_file_data$datapath)
            df <- df[[1]]
            dater[[1, 1]] <- df
        } else if (reg_xt == "csv") {
            df <- read.csv(input$file_data$datapath,
                           header = TRUE,
                           sep = ",")
            dater[[1, 1]] <- df
        }
        # }
        return(dater)
    })

    reg_annotations <- reactive({
        req(input$reg_file_annotations)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = 1)
        df <- read.csv(input$reg_file_annotations$datapath,
                       header = TRUE,
                       sep = ",")
        dater[[1, 1]] <- df
        return(dater)
    })

    reg_node <- reactive({
        req(input$reg_file_node)
        dater = matrix(list(),
                       nrow = 1,
                       ncol = 1)
        df <- read.csv(input$reg_file_node$datapath,
                       header = TRUE,
                       sep = ",")
        dater[[1, 1]] <- df
        return(dater)
    })

    output$reg_training <- DT::renderDataTable({
        datatable(
            reg_training()[[1, 1]][, , 1],
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

    output$reg_annotation <- DT::renderDataTable({
        datatable(
            reg_annotations()[[1, 1]],
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })

    output$reg_node <- DT::renderDataTable({
        datatable(
            reg_node()[[1, 1]],
            options = list(
                autowidth = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            )
        )
    })

    # rendering the drag and drop with variable names
    output$reg_variables <- renderUI({
        req(input$reg_file_annotations, input$reg_file_node)
        labs <-
            c(colnames(reg_annotations()[[1, 1]]), colnames(reg_node()[[1, 1]]))

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
                                     "Group"),
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
                                group = "allvars",
                                put = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_sample").style.backgroundColor = "#86c29c"; return to.el.children.length < 1; }'
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
                                    'function (to) { document.getElementById("reg_chosen_group").style.backgroundColor = "#86c29c"; return to.el.children.length < 1; }'
                                ),
                                pull = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_group").style.backgroundColor = "white"; }'
                                )
                            ),
                            swapClass = "sortable-swap-highlight",
                            onSort = sortable_js_capture_input("reg_chosen_group"),
                            # onMove = htmlwidgets::JS('document.getElementById("chosen_group").style.backgroundColor = "lightblue";')
                        )
                    ),
                    sortable_js(
                        "reg_chosen_node",
                        options = sortable_options(
                            group = list(
                                group = "reg_allvars",
                                put = htmlwidgets::JS(
                                    'function (to) { document.getElementById("reg_chosen_node").style.backgroundColor = "#86c29c"; return to.el.children.length < 1; }'
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
                                    'function (to) { document.getElementById("reg_chosen_region").style.backgroundColor = "#86c29c"; return to.el.children.length < 1; }'
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
    })

    output$reg_groups <- renderUI({
        req(input$reg_file_annotations, input$reg_file_node)
        groups <- data.frame(reg_annotations()[[1, 1]])
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
                                id = "reg_availablegroups",
                                icon_list(groupnames)
                            ),
                            tags$style(HTML('
                                '))
                        )
                    ),
                    column(
                        width = 3,
                        tags$div(
                            id = "reg_contrastgroup1",
                            class = "panel panel-default contrast1",
                            tags$div(class = "panel-heading",
                                     "Group 1"),
                            tags$div(class = "panel-body",
                                     id = "reg_contrast1"),
                            tags$style(HTML(
                                '.contrast1:hover {
                            background-color: #a29293
                            }
                                '
                            ))
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
                                     id = "reg_contrast2")
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
                                     id = "reg_trashbin")
                        )
                    )
                ),
                sortable_js(
                    "reg_availablegroups",
                    options = sortable_options(
                        group = list(
                            pull = "clone",
                            name = "reg_allsorts",
                            put = FALSE
                        ),
                        onSort = sortable_js_capture_input("reg_sort_vars")
                    )
                ),
                sortable_js(
                    "reg_contrast1",
                    options = sortable_options(
                        group = list(
                            group = "reg_allsorts",
                            put = TRUE,
                            pull = TRUE
                        ),
                        swapClass = "sortable-swap-highlight",
                        onSort = sortable_js_capture_input("reg_contrast_group1")
                        # onSort = htmlwidgets::JS('function (to) { var count = reg_contrast1.children.length; console.log(count);  var c = reg_contrast1.children;
                        #                          var colors = ["yellow", "indianred", "steelblue", "green", "magenta"]
                        #
                        #                          if (count >= 1){
                        #                          for (i = 0; i < count; i++){
                        #                          if (i%5 == 0) {
                        #                          var j = 0;
                        #                          }
                        #                          var color = colors[j]
                        #                          c[i].style.backgroundColor = color;
                        #                          j += 1;
                        #                          var text = c[i].innerText.match(/[a-zA-Z]+/g);
                        #                          c[i].innerText = i.toString().concat(". ", text);
                        #                          c[i].style.fontWeight = "bold";
                        #                          }
                        #                          console.log("yeeeeeehehehrerhq;lrehq;werh");
                        #
                        #                          }}')
                    )

                ),
                sortable_js(
                    "reg_contrast2",
                    options = sortable_options(
                        group = list(
                            group = "reg_allsorts",
                            put = TRUE,
                            pull = TRUE
                        ),
                        swapClass = "sortable-swap-highlight",
                        onSort = sortable_js_capture_input("reg_contrast_group2")
                        # onSort = htmlwidgets::JS('function (to) { var count = reg_contrast2.children.length; console.log(count);  var c = reg_contrast2.children;
                        #                          var colors = ["yellow", "indianred", "steelblue", "green", "magenta"]
                        #
                        #                          if (count >= 1){
                        #                          for (i = 0; i < count; i++){
                        #                          if (i%5 == 0) {
                        #                          var j = 0;
                        #                          }
                        #                          var color = colors[j]
                        #                          c[i].style.backgroundColor = color;
                        #                          j += 1;
                        #                          var text = c[i].innerText.match(/[a-zA-Z]+/g);
                        #                          c[i].innerText = i.toString().concat(". ", text);
                        #                          c[i].style.fontWeight = "bold";
                        #                          }
                        #                          console.log("yeeeeeehehehrerhq;lrehq;werh");
                        #
                        #                          }}')
                    )

                ),
                sortable_js(
                    "reg_trashbin",
                    options = sortable_options(
                        group = list(
                            group = "reg_allsorts",
                            put = TRUE,
                            pull = TRUE
                        ),
                        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
                    )
                ),
                actionButton(inputId = "reg_confirmcontrast", label = "Confirm")
            )
        )


    })

    reg_contrast <- reactiveValues()

    observeEvent(input$reg_confirmcontrast, {
        #check that they're the same length
        contrast1 <- input$reg_contrast_group1
        contrast2 <- input$reg_contrast_group2
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
            reg_contrast$groups <- output

        }
        enable("reg_contrast_check")
        updatePrettyCheckbox(session = session,
                             inputId = "reg_contrast_check",
                             value = TRUE)

    })

    observeEvent(input$reg_choose_vars, {
        varNames <- c(input$reg_chosen_sample, input$reg_chosen_group, input$reg_chosen_node, input$reg_chosen_region)
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
                    actionButton("reg_continue", "Continue")
                )
            )
        )
        print(reg_contrast$groups)
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
        annotfile <- input$reg_file_annotations$datapath
        samplesvar <- input$reg_chosen_sample
        groupvar <- input$reg_chosen_group
        outdir <- paste0(tempdir(), '/OUTPUT/REGRESSION')

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

        raw_sample_dfm$raw_sample_dfm <- proc_data[1]
        raw_sample_dfm$raw_sample_dfm_wo_uni <- proc_data[2]
        removeModal()


        insertUI(
            selector = "#reg_configfilebutton",
            where = "afterEnd",
            ui = fileInput(
                inputId = "reg_up_configs",
                label = "Upload Config File",
                multiple = FALSE
            )
        )
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
        req(input$reg_file_data, input$reg_file_annotations, input$reg_file_node)
        
        unlink(file.path(temdir(), 'OUTPUT'), recursive = TRUE, force = TRUE)

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
        # configs <- uploaded_configs()[2][[1]]
        print(reg_contrast$groups)
        contrast <- reg_contrast$groups[1]
        node_id_var <- input$reg_chosen_node
        region_name_var <- input$reg_chosen_region

        # getting values from input fields into an args[] fromat
        inputArgs <- reg_univariateConfigs(reg_inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[32] <- nodefile
        inputArgs[8] <- outdir
        inputArgs[33] <- node_id_var
        inputArgs[34] <- region_name_var

        withConsoleRedirect("reg_console", {
            # invalidateLater(100)
            out <- reg_univariate(inputArgs)

        })
        print(out)
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

        enable("reg_univariate_check")
        updatePrettyCheckbox(session = session,
                             inputId = "reg_univariate_check",
                             value = TRUE)

        # converting PDF results to PNGs to display in renderImage()
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'UNIVARIATE'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                pdf_convert(
                    files,
                    format = "png",
                    filenames = paste0(
                        sep = "",
                        "PNGFILES/" ,
                        tools::file_path_sans_ext(files),
                        ".png"
                    )
                )
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
        req(input$reg_file_data, input$reg_file_annotations, input$reg_file_node)

        matfilenoext <-
            tools::file_path_sans_ext(input$reg_file_data$name)

        dat_file <- switch(
            input$reg_modeltype,
            reg_withp = paste0(tempdir(), "/OUTPUT/REGRESSION/UNIVARIATE/", matfilenoext, "_ml.csv"),
            reg_withoutp = paste0(tempdir(), "/OUTPUT/REGRESSION/", matfilenoext, "_2D.csv")
        )

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
        contrast <- reg_contrast$groups[1]
        psetting <- 3
        cores <- 2
        cvuni <- FALSE

        # getting values from input fields into an args[] format
        inputArgs <- reg_ml_svmConfigs(reg_inputConfigs())
        inputArgs[6] <- dat_file
        inputArgs[7] <- matfilenoext
        inputArgs[8] <- outdir
        inputArgs[9] <- psetting
        inputArgs[10] <- cores
        inputArgs[56] <- cvuni
        inputArgs[64] <- contrast

        reg_ml_svm(inputArgs)

        # converting PDF results to PNGs to display in renderImage()
        pdf.list <-
            list.files(path = file.path(tempdir(), 'OUTPUT', 'REGRESSION', 'ML_SVM'),
                       pattern = ".pdf$")
        lapply(
            pdf.list,
            FUN = function(files) {
                pdf_convert(
                    files,
                    format = "png",
                    filenames = paste0(
                        sep = "",
                        "PNGFILES/" ,
                        tools::file_path_sans_ext(files),
                        ".png"
                    )
                )
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
            alt = "Uniariate plots",
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
            tempReport <- file.path(tempdir(), 'reg_results.Rmd')
            file.copy(file.path('reg_results.Rmd'), tempReport, overwrite = TRUE)
            print("YOOOSSS")
            params <- list(n = 100)
            rmarkdown::render(input = tempReport)
            file.copy(file.path(tempdir(), 'reg_results.pdf'), file)
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

    # output$brain <- renderBrain({
    #     brain = threeBrain::freesurfer_brain(
    #         fs_subject_folder = '~/rave_data/others/three_brain/N27/',
    #         subject_name = 'N27',
    #         additional_surfaces = c('white', 'smoothwm')
    #     )
    #     plot(brain)
    # })

}

# Run the application
shinyApp(ui = ui, server = server)

# library(threeBrain)
# library(shiny)
# library(shinydashboard)
# 
# ui <-dashboardPage(dashboardHeader(),
#                    dashboardSidebar(
#                        sidebarMenu(
#                            menuItem("Brain", tabName = 'brainout'),
#                            menuItem("Plot", tabName = 'plotout')
#                        )
#                        
#                        
#                    ),
#                    dashboardBody(tabItems(tabItem(
#                        tabName = "brainout",
#                        box(
#                            status = "primary",
#                            solidHeader = TRUE,
#                            collapsible = TRUE,
#                            title = "BrainViewer",
#                            width = 12,
#                            threejsBrainOutput(outputId = 'brain', height = '100vh')
#                        )
#                    ),
#                    tabItem(
#                        tabName = "plotout",
#                        box(
#                            status = "primary",
#                            title = "Test",
#                            solidHeader = TRUE,
#                            collapsible = TRUE,
#                            width = 12,
#                            plotOutput("plot1", click = "plot_click"),
#                        )
#                    ))))
# 
# 
# server <- function(input, output, session) {
# 
#     output$plot1 <- renderPlot({
#         plot(mtcars$wt, mtcars$mpg)
#     })
# 
#     output$brain <- renderBrain({
#         brain = threeBrain::freesurfer_brain(
#             fs_subject_folder = '~/rave_data/others/three_brain/N27/', subject_name = 'N27',
#             additional_surfaces = c('white', 'smoothwm')
#         )
#         plot( brain )
#     })
# }
# 
# shinyApp(ui, server, options = list(launch.browser = TRUE))