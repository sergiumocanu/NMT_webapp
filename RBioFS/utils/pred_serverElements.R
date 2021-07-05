uploadFilesServer <- function(id){
  moduleServer(
    id,
    function(input, output, sesssion){
      dataFile <- reactive({
        validate(need(input$data, message = FALSE))
        input$data
      })
      
      # data <- reactive({
      #   dater = matrix(list(), nrow = 1, ncol = 1)
      #   df <- readMat(dataFile()$datapath)
      #   df <- df[[1]]
      #   dater[[1, 1]] <- df
      # })
      # 
      annotationsFile <- reactive({
        validate(need(input$annotations, message = FALSE))
        input$annotations
      })

      nodeFile <- reactive({
        validate(need(input$node, message = FALSE))
        input$node
      })

      # features data file input
      data <- reactive({
        xt <- tools::file_ext(dataFile()$datapath)
        if (xt == "mat"){
          print("file is MAT FILES")
          dat = matrix(list(), nrow = 1, ncol = 1)
          df <- readMat(dataFile()$datapath)
          df <- df[[1]]
        } else if (xt == "csv"){
          df <- read.csv(dataFile()$datapath,
                         header = TRUE,
                         sep = ",")
        }
      })
      # annotations file input
      annotations <- reactive({
        xt <- tools::file_ext(dataFile()$datapath)
        if (xt == "mat"){
          print("annotations is 3D!")
          # dater = matrix(list(),
          #                nrow = 1,
          #                ncol = 1)
          df <- read.csv(annotationsFile()$datapath,
                         header = TRUE,
                         sep = ",")
          # dater[[1, 1]] <- df
        } else if (xt == "csv") {
          dater <- data.frame(data()[2]) # will have to change this later!!!
        }
      })
      # node file input
      node <- reactive({
        xt <- tools::file_ext(dataFile()$datapath)
        if(xt == "mat"){
          dater = matrix(list(),
                         nrow = 1,
                         ncol = 1)
          df <- read.csv(nodeFile$datapath,
                         header = TRUE,
                         sep = ",")
          dater[[1, 1]] <- df
        } else if (xt == "csv"){

        }
      })

      # output <- reactive({
      #   # print(dim(data()))
      #   xt <- tools::file_ext(dataFile()$datapath)
      #   if (xt == "mat"){
      #     print("FILE IS 3D")
      #     print(dim(annotations()))
      #     list(data = data, 
      #          annotations = annotations, 
      #          node = node)
      #   } else if (xt == "csv"){
      #     list(data = data, 
      #          annotations = annotations)
      #   }
      # })
      # 
      # return(output)
      
      return(list(data = data,
                  annotations = annotations,
                  node = node))
    }
  )
}

# prediction element

# source("../backend/pred_dat_process.R")
# source("../backend/pred_classif.R")
predictServer <- function(id){
  
  moduleServer(
    id,
    
    function(input, output, session){
      # getting the input data to predict
      inputFile <- reactive({
        validate(need(input$data, message = FALSE))
        input$data
      })
      predict_data <- reactive({
        readMat(inputFile()$datapath)
      })
      
      # getting the input annotations
      inputAnnotations <- reactive({
        validate(need(input$annotations, message = FALSE))
        input$annotations
      })
      annotations_data <- reactive({
        read.csv(inputAnnotations()$datapath)
      })
      
      # getting the model
      inputModel <- reactive({
        validate(need(input$model, message = FALSE))
        input$model
      })
      model_data <- reactive({
        load(inputModel()$datapath)
      })
      
      columnVars <- reactive({
        colnames(annotations_data())
      })
      
      output$varnames <- renderDataTable(datatable(
        matrix(columnVars()),
        colnames = NULL,
        options = list(
          searching = FALSE,
          paging = FALSE,
          bsort = FALSE,
          bInfo = FALSE,
          selection = 'single'
        )
      ))
      
      chosenSampleVar <- reactive({
        columnVars()[input$varnames_row_last_clicked]
      })
      
      inputArgs <- reactive({
        req(input$annotations)
        mat_file <- inputFile()$datapath
        mat_file_no_ext <- tools::file_path_sans_ext(inputFile()$name)
        annot_file <- inputAnnotations()$datapath
        sampleid_var <- chosenSampleVar()
        # do.call(file.remove, list(list.files(
        #   file.path(tempdir(), 'OUTPUT', 'PREDICTION'),
        #   full.names = TRUE, recursive = TRUE
        # )))
        dir.create(
          file.path(tempdir(), 'OUTPUT', 'PREDICTION'),
          showWarnings = FALSE,
          recursive = TRUE
        )
        out_dir <- file.path(tempdir(), 'OUTPUT', 'PREDICTION')
        # out_dir <- getwd()
        
        inputArgs <- c(mat_file,
                       mat_file_no_ext,
                       annot_file,
                       sampleid_var,
                       out_dir)
      })

      observeEvent(input$varnames_row_last_clicked, {
        browser()
        cat(" WE JUST SELECTED THE ROW")
        # file.copy("predResults.Rmd", file.path(tempdir(), 'OUTPUT','PREDICTION','predResults.Rmd'))
        pred_dat_process(inputArgs())
        enable("predict")
      })
      
      
      predInputArgs <- reactive({
        
        mat_file_no_ext <- tools::file_path_sans_ext(inputFile()$name)
        dat_2d_file <-
          paste0(tempdir(), '/OUTPUT/PREDICTION/', mat_file_no_ext, "_2D.csv")
        # dat_2d_file <- paste0(getwd(), mat_file_no_ext, "_2D.csv")
        model_file <- inputModel()$datapath
        out_dir <- file.path(tempdir(), 'OUTPUT', 'PREDICTION')
        # out_dir <- getwd()
        newdata_center_scale <- TRUE
        probability_method <- "softmax"
        pie_width <- 170
        pie_height <- 150
        cpu_cluster <- "FORK"
        psetting <- FALSE
        cores <- 2
        
        predInputArgs <- c(
          dat_2d_file,
          model_file,
          out_dir,
          newdata_center_scale,
          probability_method,
          psetting,
          cores,
          cpu_cluster,
          pie_width,
          pie_height
        )
      })
      
      observeEvent(input$predict, {
        browser()
        show_modal_spinner(text = "Running classifier...")
        
        file.copy(inputModel()$datapath, file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'model.Rdata'))
        file.copy(inputAnnotations()$datapath, file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'annot.csv'))
        
        
        
        pred_classif(predInputArgs())
        
        dir.create(
          file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'PNGFILES', 'CONNECTIVITY'),
          showWarnings = FALSE,
          recursive = TRUE
        )
        pdf.list <-
          list.files(path = file.path(tempdir(), 'OUTPUT', 'PREDICTION'),
                     pattern = ".pdf$")
        # pdf.list <- list.files(path = getwd(), pattern = ".pdf$")
        lapply(
          pdf.list,
          FUN = function(files) {
            if (files == "Rplots.pdf") {
              NULL
            } else {
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
            
          }
        )
        
        files <-
          gtools::mixedsort(list.files(
            path = file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'PNGFILES'),
            # path = file.path(getwd(), "PNGFILES"),
            pattern = ".png$",
            full.names = FALSE
          ))
        updateSelectInput(session = session,
                          inputId = "plotname",
                          choices = files)
        setwd("/srv/shiny-server")

        rmarkdown::render(input = "/srv/shiny-server/predResults.Rmd")

        removeModal()

      })
      
      output$predictionplots <- renderImage({
        req(input$predict)
        list(
          src = file.path(
            tempdir(),
            'OUTPUT',
            'PREDICTION',
            'PNGFILES',
            input$plotname
          ),
          alt = "Prediction Plots",
          width = 481,
          height = 425
        )
        
      }, deleteFile = FALSE)
      
      output$downloadPredictions <- downloadHandler(
        
        filename = function() {
          paste("allfiles-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
          tempzip <- file.path(tempdir(), "results.zip")
          zipr(tempzip, file.path(tempdir(), 'OUTPUT'))
          file.copy(tempzip, file)
        }
      )
      
      pred_data <- reactive({
          req(input$predict)
          x <- read.csv(file.path(tempdir(),'OUTPUT','PREDICTION', 'data_subset.csv'), check.names = FALSE)
          mtx <- matrix(0, nrow = 90, ncol = 90)
          row_idx <- as.numeric(sub("\\_.*", "", colnames(x)[2:length(colnames(x))]))
          col_idx <- as.numeric(sub("^[^_]*_", "", colnames(x)[2:length(colnames(x))]))
          patient <- as.numeric(sub("\\..*", "", input$plotname))
          d <- unlist(unname(x[patient, 2:length(colnames(x))]))
          mtx[cbind(row_idx, col_idx)] <- d
          mtx[cbind(col_idx, row_idx)] <- d
          mtx
      })
      
      output$brainConnectivity_top <- renderPlot({
        req(input$predict)
        brain <- brainconn(atlas ="aal90", conmat=pred_data(), view = input$connectivity_view, edge.color.weighted = T)
        brain + scale_edge_color_gradient2(low = "blue", mid = "blue", high = "red")
        # brainconn3D("aal90", conmat = r, show.legend = T)
      })
      
      model_data <- reactive({
        req(input$predict)
        model_env <- new.env()
        load(inputModel()$datapath, envir = model_env)
        svm_m <- get('svm_m', model_env)
        kernels <- c("linear", "polynomial", "radial", "sigmoid")
        if('svm_m_test_svm_roc_auc' %in% ls(model_env)){
          a <- auc <- get('svm_m_test_svm_roc_auc', model_env)
          auc <- a$svm.roc_object$control$auc[1]
        } else {
          auc <- " "
        }
        # annotations <- read.csv(inputAnnotations()$datapath, check.names = FALSE)
        model_data <- data.frame(property = c("SVM-Type:", "SVM-Kernel:", "Cost:", "Gamma:", "Number of Support Vectors:", "AUC:"), 
                                 value = c(svm_m$model.type, kernels[svm_m$kernel+1], svm_m$cost, svm_m$gamma, svm_m$tot.nSV, auc), row.names = NULL)
      })
      
      output$model <- renderTable(
        model_data(),
        colnames = FALSE
      )
      
      subject_data <- reactive({
        req(input$predict)
        patient <- as.numeric(sub("\\..*", "", input$plotname))
        annotations <- read.csv(inputAnnotations()$datapath, check.names = FALSE)
        ann <- unname(as.matrix(annotations))[patient, ]
        subject_data <- data.frame(cnames = colnames(annotations),
                                   vals = unlist(ann, use.names = FALSE))
        subject_data
      })

      output$subject <- renderTable(
        subject_data(),
        colnames = FALSE
      )
    
      

      output$downloadPredictionsReport <- downloadHandler(
        filename = function() {
          paste("prediction_report-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          
          # tempReport <- paste0(tempdir(), '\\OUTPUT\\PREDICTION\\predResults.Rmd')
          # setwd("/srv/shiny-server/")
          # tempReport <- "predResults.Rmd"
          # show_modal_spinner(text = "Generating Report...")
          # rmarkdown::render(input = tempReport)
          # removeModal()
          # file.copy(file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'predResults.pdf'), file)
          file.copy("/srv/shiny-server/predResults.pdf", file)
        })
    }
  )
  
}