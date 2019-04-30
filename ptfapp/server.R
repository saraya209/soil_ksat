shinyServer(function(input,output){
  # Soil Separates -------------------
  readCl = reactive({as.numeric(input$clayInput)})
  readSi = reactive({as.numeric(input$siltInput)})
  #calculate sand fraction
  readS <- reactive({
    100 - sum(readCl(),readSi())
  })
  
  output$sandInput <- renderText({
    readS()
  })
  
  
  # Create single input data frame
  s.dt <<- reactive({
    # Create data
    data.frame(Clay = input$clayInput,
               Silt = input$siltInput,
               Sand = readS(),
               VCOS = input$vcsInput,
               COS = input$csInput,
               MS = input$msInput,
               FS = input$fsInput,
               VFS = input$vfsInput,
               BD = input$bdInput,
               OC = input$ocInput)
  })
  

observe({
    display_mode<<-input$single_or_file
    })

  # Model Details ------------------
  output$modelID <- renderText({paste0(input$select_model, 
                                      ". ","(A gradient boosted model with gaussian loss function.)")})
  output$model_inputs <- renderText({
    paste(model.inputs[[input$select_model]],
          collapse = ", ")
    })
  
  # Upload Model GLOBALLY ------------
  loaded_m <<- eventReactive(input$load_model,{
    withProgress(message = "Loading Model", value = 1,
                 {
                   m <- readRDS(file.path("Models",model.names[[input$select_model]]))
                   m
                 })
  })
  
  # # Run Model on Single Run GLOBALLY --------
  s_predicted_dt <<- eventReactive(input$single_run, {
    display <- input$single_or_file
    display
    if (display == "single_mode"){
      # Run Prediction if file and model exist
      if (class(loaded_m()) =="gbm" & 
          !is.na(s.dt()$Clay) & 
          !is.na(s.dt()$Silt)){
        withProgress(message = 'Running PTF Model', value = 1, 
                     {
                       
                       s_run_ptf_fun(input$select_model,
                                     s.dt()) # Files named this may cause error
                       
                     })
      }
    }
    
  })
  
  # # Run Model on File Run GLOBALLY --------
  predicted_dt <<- eventReactive(input$file_run, {
    display <- input$single_or_file
    display
    if(display == "file_mode"){
      inFile <<- input$upload_file
      # Run Prediction if file and model exist
      if (class(loaded_m()) =="gbm" & !is.null(inFile) ){
        
        withProgress(message = 'Running PTF Model', value = 1, 
                     {
                       # upload file
                       u.dt <- readr::read_csv(file = inFile$datapath,
                                               col_names  = TRUE,
                                               col_types = cols(.default = "d"))
                       run_ptf_fun(input$select_model,
                                   u.dt)
                       
                     })
      }
    }
    
    
    
  })
  
  
  
  # Show Predicted File --------------
  output$in_out_table <- DT::renderDataTable({
    display <- input$single_or_file
    display
    if (display == "single_mode" & 
        !is.na(s.dt()$Clay) & 
        !is.na(s.dt()$Silt)){
      #if (!is.data.frame(s_predicted_dt())){
        datatable(s_predicted_dt(),
                  rownames = FALSE) %>% 
          formatRound("Predicted_Ks", digits = 5)
      #}
    }else {
      inFile <- input$upload_file
      # Don't show anything if no predicted file
      if(is.null(inFile) &
         !is.data.frame(predicted_dt())){
        return(NULL)
      }else{
        datatable(predicted_dt(), 
                  rownames = FALSE) %>% 
          formatRound("Predicted_Ks", digits = 5)
      }
      
      
    }
    
  })
  
 
  # Download File ----------------
  output$downloadData <- downloadHandler(
    filename = function() {
      "Predicted_KSAT_Data.csv"
    },
    content = function(file) {
      
      display <- input$single_or_file
      if (display == "single_mode"){
        write.csv(s_predicted_dt(), file, row.names = FALSE)
      }else {
        write.csv(predicted_dt(), file, row.names = FALSE)
      }
      
    })
  
  output$run_mode_text <- renderText({
    display <- input$single_or_file
    if(display=="single_mode"){
      "Manual Input."
    }else{
      "File Upload."
    }
  })

  
  output$test_model_load <- renderText({
    class(loaded_m()) =="gbm"
  })
  
  # Remark text output -----------------
  output$message <- renderText({
    if(exists("num.msg")){
      paste(num.msg)
      
    }
    
  })
  
  # *************** Testing Output **********************
  # output$testing <- renderPrint({
  #   # display <- input$single_or_file
  #   # display
  #   !is.na(s.dt()$Clay)
  # 
  #   # inFile <- input$upload_file
  #   # class(sel.m()) =="gbm" & !is.null(inFile)
  # })
})