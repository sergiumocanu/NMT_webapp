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

source("pred_dat_process.R")
source("pred_classif.R")
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
        dir.create(
          file.path(tempdir(), 'OUTPUT', 'PREDICTION'),
          showWarnings = FALSE,
          recursive = TRUE
        )
        out_dir <- file.path(tempdir(), 'OUTPUT', 'PREDICTION')
        
        inputArgs <- c(mat_file,
                       mat_file_no_ext,
                       annot_file,
                       sampleid_var,
                       out_dir)
      })

      observeEvent(input$varnames_row_last_clicked, {
        pred_dat_process(inputArgs())
      })
      
      
      predInputArgs <- reactive({
        
        mat_file_no_ext <- tools::file_path_sans_ext(inputFile()$name)
        dat_2d_file <-
          paste0(tempdir(), '/OUTPUT/PREDICTION/', mat_file_no_ext, "_2D.csv")
        model_file <- inputModel()$datapath
        out_dir <- file.path(tempdir(), 'OUTPUT', 'PREDICTION')
        newdata_center_scale <- TRUE
        probability_method <- "softmax"
        pie_width <- 170
        pie_height <- 150
        cpu_cluster <- "PSOCK"
        psetting <- TRUE
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
        
        show_modal_spinner(text = "Running classifier...")
        
        pred_classif(predInputArgs())
        
        dir.create(
          file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'PNGFILES'),
          showWarnings = FALSE,
          recursive = TRUE
        )
        pdf.list <-
          list.files(path = file.path(tempdir(), 'OUTPUT', 'PREDICTION'),
                     pattern = ".pdf$")
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
          list.files(
            path = file.path(tempdir(), 'OUTPUT', 'PREDICTION', 'PNGFILES'),
            pattern = ".png$",
            full.names = FALSE
          )
        updateSelectInput(session = session,
                          inputId = "plotname",
                          choices = files)
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
    }
  )
  
}