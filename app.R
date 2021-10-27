#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(reshape)
library(yaml)
library(romero.gateway)
library(shiny)


source("helpers.R")

file.with.all.key.values <- ""
omero.path <- "/home/ldelisle/.conda/envs/omero/bin/omero"
# This conda environment has been created by:
# conda create -n omero -c ome python=3.6 zeroc-ice36-python omero-py pandas
prefix.path <- "/home/ldelisle/mountDuboule/"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Annotate data from OMERO Duboule"),
  
  # Sidebar with login details
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  # Login tabPanel which will disappear when you are logged in:
                  tabPanel("Login",
                           helpText("Use your omero credentials:"),
                           textInput("username", placeholder="Username", label = tagList(icon("user"), "Username")),
                           passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                           actionButton("login", label = "LOG IN"),
                           uiOutput("loginUI") # This will give you a message with the state of your login
                  ),
                  # Project Dataset where you select your project / dataset
                  tabPanel("Project Dataset",
                           checkboxInput("useronly", "Display only the data from the logged-in user", value = T),
                           actionButton("update", label = "Update choices in Project/Dataset lists"),
                           uiOutput("projectSelect"),
                           uiOutput("datasetSelect"),
                           textOutput("lastUpload"), # Gives you the status of the last upload
                           textOutput("currentKeyval"), # Gives you the Project/Dataset used in the Annotation tab
                           actionButton("updateKeyValues", label = "Reset choices in key values lists"),
                           actionButton("disconnect", label = "Disconnect")
                  )
      )
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(id = "tabsMain",
                  tabPanel("Upload",
                           h3("Choose file or directory"),
                           helpText("Put here the path for file or folder to upload to OMERO:"),
                           textInput("fileOrDirToUpload", label = "Input file or directory"),
                           numericInput("depth", label = "Number of directories to scan down", value = 4),
                           h3("Check path (no need to login)"),
                           actionButton("checkUpload", "Check what will be uploaded."),
                           helpText("It can be long... Be patient. Once done an output will be printed below."),
                           verbatimTextOutput("outputF"),
                           uiOutput("UploadIfPossible") # Upload button on output only if logged in
                  ), 
                  tabPanel("Annotation",
                           uiOutput("prepareDFIfPossible"), # Only display the button if the project and dataset exists 
                           uiOutput("addUploadInfoIfPossible"), # Only display the button if it is the good project, the good dataset and there was a successful upload
                           h3("Add annotations"),
                           # First the images needs to be selected
                           selectInput("imagesSel",
                                       "Select images to add annotations",
                                       choices = c('All images'="all",
                                                   'Only some images'="some"),
                                       selected = "all",
                                       multiple = F),
                           uiOutput("selectImagesIfNeeded"), # If it is 'some' extra UI are rendered
                           # Key values are set
                           h3("New key values to add:"),
                           uiOutput("selectKeyIfPossible"), # Only display if the toMergeDF is not empty
                           uiOutput("selectionValueIfPossible"), # Same
                           uiOutput("selectValueIfPossible"), # Here it displays different select and button depending on selectionValue
                           h3("On going table"),
                           dataTableOutput("toMergeDF"),
                           uiOutput("mergeDFIfPossible"), # Only display the button if there is new info
                           # They are integrated to the data to upload
                           h3("Table to upload to OMERO"),
                           dataTableOutput("currentDF"),
                           downloadButton("downloadDF", "Download this table"),
                           uiOutput("currentDFstatus"),
                           uiOutput("uploadDFIfNeeded"), # Only display the button if there is a change
                           verbatimTextOutput("outputFUploadDF") # Get the output of the upload key values script
                  ),
                  tabPanel("Debug",
                           checkboxInput("debugMode", "Print to the standard out", value = F),
                           verbatimTextOutput("debug")
                  )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # my.ome contains all the reactive Values
  my.ome <- reactiveValues(server = NULL, # Is either NULL or contains a valid connected server
                           valid.login = FALSE, # Is FALSE except if server contains a valid connected server
                           failed.login = FALSE, # Is FALSE except if the user clicked on the login button but it failed to create a valid connected server
                           update = 0, # Counter to detect when the projects should be updated
                           update.keyval = 0, # Counter to detect when the key values should be fetched from OMERO
                           projects = NULL, # List of projects obtained from GetProjects.
                           projects.ids = NULL, # Named vector with ids of projects in projects, names are project names
                           datasets.ids = NULL, # Named vector with ids of datasets in input$projectSelected, names are dataset names
                           upload.project = "", # Project name used at upload
                           upload.dataset = "", # Dataset name used at upload
                           upload.path = "", # Path of dir or files to upload
                           original.dataframe = data.frame(), # Dataframe obtained from OMERO for the project.df and dataset.df
                           current.dataframe = data.frame(), # Dataframe enriched from original.dataframe with new key values to upload to OMERO
                           project.df = "", # Project name corresponding to all dataframes
                           dataset.df = "", # Dataset name corresponding to all dataframes
                           toMerge.dataframe = data.frame(), # Dataframe with potentially a subset of rows compare to other dataframe and new columns to be added to current.dataframe
                           current.is.ori = TRUE, # If there are differences between original.dataframe and current.dataframe
                           existing.key.values = list(), # A named list where names are keys and items are vectors with used values in OMERO by the group
                           lastKeySelected = "image.name", # Just used to keep what has been selected before
                           lastSelectionValue = "fixed", # Just used to keep what has been selected before
                           lastSelectedColumnValue = "image.name", # Just used to keep what has been selected before
                           lastSplitCharacter = "_", # Just used to keep what has been selected before
                           lastSplitPosition = 1, # Just used to keep what has been selected before
                           debug.mode = FALSE # Logical whether there should be print
                           
  )
  
  # When the user click on the login button
  # my.ome$server is updated as well as valid.login and failed.login
  # It also modified the 2 counters to update the projects and key/values
  observeEvent(input$login, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "login\n")
    }
    my.ome$server <- tryCatch(connect(OMEROServer(host = "omero-server.epfl.ch", username = input$username, password = input$password, port = as.integer(4064))),
                              error = function(e) {
                                cat(file = stderr(), e, "\n")
                                NULL
                              })
    if (! is.null(my.ome$server)){
      my.ome$valid.login <- TRUE
      my.ome$failed.login <- FALSE
      my.ome$update <- my.ome$update + 1
      my.ome$update.keyval <- my.ome$update.keyval + 1
    } else {
      my.ome$valid.login <- FALSE
      my.ome$failed.login <- TRUE
    }
  })
  
  # When the user click on disconnect
  # The server is disconnected and 
  # valid.login is updated
  observeEvent(input$disconnect, {
    disconnect(my.ome$server)
    my.ome$server <- NULL
    my.ome$valid.login <- FALSE
  })
  
  # When the counter update is modified
  # the projects are updated
  observeEvent(my.ome$update, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "update\n")
    }
    if (my.ome$valid.login){
      if (my.ome$debug.mode){
        cat(file = stderr(), "UPDATE PROJECT AFTER SOMETHING\n")
      }
      my.ome$projects <- getProjects(my.ome$server)
      if (input$useronly){
        my.ome$projects <- subsetObjectByOwner(my.ome$projects, my.ome$server@user$getId())
      }
      all.projects.obj <- sapply(my.ome$projects, slot, name = "dataobject")
      my.choices <- sapply(all.projects.obj, function(ob){ob$getId()})
      names(my.choices) <- sapply(all.projects.obj, function(ob){ob$getName()})
      my.ome$projects.ids <- my.choices
      if (my.ome$debug.mode){
        cat(file = stderr(), length(my.ome$projects), "\n")
      }
    }
  })
  
  # When the counter update.keyval is updated
  # my.ome$existing.key.values is updated
  # using a python script
  # It goes much quicker than through R
  observeEvent(my.ome$update.keyval, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "UPDATE KEYVAL\n")
    }
    if (my.ome$valid.login){
      my.ome$existing.key.values <- read_yaml(text = 
                                                system(
                                                  paste0(gsub("omero$", "python", omero.path),
                                                         " external_scripts/get_all_omero_key_values_in_yaml.py",
                                                         " --server omero-server.epfl.ch --user \'",
                                                         input$username, "\' --password \'",
                                                         input$password, "\'"),
                                                  intern = T))
      
      if (my.ome$debug.mode){
        cat(file = stderr(), my.ome$existing.key.values, "\n")
      }
    }
  })
  
  # Change the Tab hidden and shown
  # when user connect or disconnect
  observeEvent(my.ome$valid.login, {
    if (my.ome$valid.login){
      showTab(inputId = "tabs", target = "Project Dataset")
      hideTab(inputId = "tabs", target = "Login")
      showTab(inputId = "tabsMain", target = "Annotation")
    } else {
      hideTab(inputId = "tabs", target = "Project Dataset")
      showTab(inputId = "tabs", target = "Login")
      hideTab(inputId = "tabsMain", target = "Annotation")
    }
  })
  
  # Simple text for login status
  output$loginUI <- renderText({
    if (my.ome$failed.login){
      "Logged in failed."
    } else if (! my.ome$valid.login) {
      "You are not logged in."
    } else {
      paste("You are logged in as", my.ome$server@user$getFirstName())
    }
  })
  
  # When the button update is clicked
  # The counter update is incremented
  observeEvent(input$update, {
    my.ome$update <- my.ome$update + 1
  })
  
  # When the user change useronly
  # The counter update is incremented so projects are updated
  observeEvent(input$useronly, {
    my.ome$update <- my.ome$update + 1
  })
  
  # When the button updateKeyValues is clicked
  # The counter update.keyval is incremented
  observeEvent(input$updateKeyValues, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "click updateKeyValues\n")
    }
    my.ome$update.keyval <- my.ome$update.keyval + 1
    if (my.ome$debug.mode){
      cat(file = stderr(), my.ome$update.keyval, "\n")
    }
  })
  
  # Choices for projectSelected are customized
  output$projectSelect <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "projectSelect\n")
    }
    my.choices <- ""
    if (! is.null(my.ome$projects.ids)){
      my.choices <- sort(names(my.ome$projects.ids))
    }
    if (my.ome$debug.mode){
      cat(file = stderr(), my.choices, "\n")
    }
    selectizeInput("projectSelected",
                   "Select existing project or write a new one",
                   choices = my.choices,
                   selected = my.ome$upload.project,
                   options = list(create = TRUE),
                   multiple = F)
  })
  
  # The dataset.ids are updated when input$projectSelected
  # is modified
  # NOTE: Maybe this should be a observeEvent
  observe({
    if (my.ome$debug.mode){
      cat(file = stderr(), "observe project selected or projects\n")
    }
    if (is.null(input$projectSelected)){
      my.ome$datasets.ids <- NULL
    } else {
      if (my.ome$debug.mode){
        cat(file = stderr(), names(my.ome$projects.ids), "\n")
        cat(file = stderr(), input$projectSelected, "\n")
      }
      if (input$projectSelected %in% names(my.ome$projects.ids)){
        if (my.ome$debug.mode){
          cat(file = stderr(), "EXISTING PROJECT\n")
          cat(file = stderr(), which(names(my.ome$projects.ids) == input$projectSelected), "\n")
        }
        my.datasets <- getDatasets(my.ome$projects[[which(names(my.ome$projects.ids) == input$projectSelected)]])
        all.datasets.obj <- sapply(my.datasets, slot, name = "dataobject")
        my.choices <- sapply(all.datasets.obj, function(ob){ob$getId()})
        names(my.choices) <- sapply(all.datasets.obj, function(ob){ob$getName()})
        my.ome$datasets.ids <- my.choices
      } else {
        my.ome$datasets.ids <- NULL
      }
    }
  })
  
  
  # Choices for datasetSelected are customized
  output$datasetSelect <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "datasetSelect\n")
    }
    my.choices <- ""
    if (! is.null(my.ome$datasets.ids)){
      my.choices <- sort(names(my.ome$datasets.ids))
    }
    selectizeInput("datasetSelected",
                   "Select existing dataset or write a new one",
                   choices = my.choices,
                   options = list(create = TRUE),
                   selected = my.ome$upload.dataset,
                   multiple = F)
  })
  
  # importF() contains the output of omero import
  importF <- eventReactive(input$checkUpload, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "importF\n")
    }
    if (input$fileOrDirToUpload == ""){
      "Set the file or dir"
    } else {
      # system(paste0(omero.path, " import --depth ", input$depth, " -f \'", prefix.path, "/", input$fileOrDirToUpload, "\' 2>&1"), intern = T,
      #        ignore.stdout = F, ignore.stderr = F)
      system(paste0(omero.path, " import --depth ", input$depth, " -f \'", prefix.path, "/", input$fileOrDirToUpload, "\'"), intern = T,
             ignore.stdout = F, ignore.stderr = F)
    }
  })
  
  # importF is launched only if this output is active
  output$outputF <- renderPrint({
    cat(importF(), sep = "\n")
  })
  
  # button upload and output text if logged in
  output$UploadIfPossible <- renderUI({
    if(! my.ome$valid.login){
      HTML("")
    } else {
      list(
        h3("Upload"),
        actionButton("upload", "Upload using parameters above."),
        helpText("It can be super long... Be patient. Once done an output will be printed below."),
        verbatimTextOutput("shortOutputUpload"))
    }
  })
  
  # importO() contains the std out of omero import
  importO <- eventReactive(input$upload, {
    if (! my.ome$valid.login){
      "Login first"
    } else if (input$fileOrDirToUpload == ""){
      "Set the file or dir"
    } else {
      if (my.ome$debug.mode){
        cat(file = stderr(), paste0(omero.path, " import -s ",  my.ome$server@host,
                     " -u \'", my.ome$server@username, "\' -w \'", input$password,
                     "\' --depth ", input$depth, " -T Project:name:\"", input$projectSelected,
                     "\"/Dataset:name:\"", input$datasetSelected, "\" \'", prefix.path, "/",
                     input$fileOrDirToUpload, "\' 2>&1\n"))
      }
      std.output <- system(paste0(omero.path, " import -s ",  my.ome$server@host,
                                  " -u \'", my.ome$server@username, "\' -w \'", input$password,
                                  "\' --depth ", input$depth, " -T Project:name:\"", input$projectSelected,
                                  "\"/Dataset:name:\"", input$datasetSelected, "\" \'", prefix.path, "/",
                                  input$fileOrDirToUpload, "\' 2>&1"), intern = T)
      my.ome$update <- my.ome$update + 1
      return(std.output)
    }
  })
  
  # Text with summary of importO()
  output$shortOutputUpload <- renderText({
    if (my.ome$debug.mode){
      cat(file = stderr(), "IMPORTO CHANGED\n")
    }
    if (is.null(importO())){
      ""
    } else {
      if ("==> Summary" %in% importO()){
        importO()[which(importO() == "==> Summary") + 1]
      } else {
        paste(importO(), collapse = "\n")
      }
    }
  })
  
  # When the user click on upload
  # The project and dataset selected are stored
  observeEvent(input$upload, {
    my.ome$upload.project <- input$projectSelected
    my.ome$upload.dataset <- input$datasetSelected
    my.ome$upload.path <- input$fileOrDirToUpload
  })
  
  # success.upload() is FALSE except if the upload worked
  success.upload <- reactive({
    if (my.ome$upload.project != "" &&
        my.ome$upload.dataset != "" &&
        my.ome$upload.path != "" &&
        "==> Summary" %in% importO()){
      TRUE
    } else {
      FALSE
    }
  })
  
  # Text on upload info
  output$lastUpload <- renderText({
    if (success.upload()){
      paste0("Last upload is ", my.ome$upload.path,
             " in ", my.ome$upload.project, "/",
             my.ome$upload.dataset)
    } else {
      "No successful upload"
    }
  })
  
  # Text on project and datasets in Annotation
  output$currentKeyval <- renderText({
    if (nrow(my.ome$current.dataframe) == 0){
      "No current dataframe"
    } else {
      paste0("The current dataframe corresponds to ",
             my.ome$project.df, "/", my.ome$dataset.df)
    }
  })
  
  # HTML text and/or button to prepareDF from an existing project and dataset
  output$prepareDFIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "prepareDFIfPossible\n")
    }
    if (! my.ome$valid.login){
      HTML("<h1>To annotate your data you need to login first.</h1>")
      # } else if (is.null(input$projectSelected) || is.null(input$datasetSelected)){
      #   HTML("<h1>Go to the Project Dataset tab (next to 'Login') to choose your dataset.</h1>")
    } else if (! input$projectSelected %in% names(my.ome$projects.ids)){
      HTML("You need to choose an existing project")
    } else if (! input$datasetSelected %in% names(my.ome$datasets.ids)){
      HTML("You need to choose an existing dataset")
    } else {
      output <- NULL
      if (nrow(my.ome$current.dataframe) == 0){
        output <- list(HTML("<h3> First click here </h3>"))
      }
      return(c(output,
               list(actionButton("prepareDF", "Generate the dataframe from existing key values"))
      ))
    }
  })
  
  # If the user click on prepareDF
  # All key values for each image of the dataset
  # are fitted in current.dataframe and original.dataframe
  observeEvent(input$prepareDF, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "prepareDF changed\n")
    }
    # Get the dataset object
    my.datasets <- getDatasets(my.ome$projects[[which(names(my.ome$projects.ids) == input$projectSelected)]])
    my.datasets.names <- sapply(my.datasets, function(my.d){my.d@dataobject$getName()})
    my.ome$current.dataframe <- initiateDF(my.ome$server, my.datasets[[which(my.datasets.names == input$datasetSelected)]])
    if (my.ome$debug.mode){
      cat(file = stderr(), "initiateDF OK\n")
    }
    my.ome$original.dataframe <- my.ome$current.dataframe
    my.ome$project.df <- input$projectSelected
    my.ome$dataset.df <- input$datasetSelected
    if (my.ome$debug.mode){
      cat(file = stderr(), "OK\n")
    }
  })
  
  # Button to add upload path for corresponding image ids
  # Only if possible
  output$addUploadInfoIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "addUploadInfoIfPossible\n")
      cat(file = stderr(), nrow(my.ome$current.dataframe), "\n")
    }
    if (! success.upload()){
      HTML("")
    } else if (nrow(my.ome$current.dataframe) == 0 || 
               input$projectSelected != my.ome$upload.project ||
               input$datasetSelected != my.ome$upload.dataset){
      HTML("To add upload info, you need to generate the dataframe from existing values with the corresponding project and dataset.")
      # } else if (my.ome$project.df != input$projectSelected ||
      #            my.ome$dataset.df != input$datasetSelected) {
      #   HTML("The project/dataset used to generate the current dataframe is not the same as the one selected on top, regenerate the dataframe.")
    } else if (my.ome$project.df != my.ome$upload.project || my.ome$dataset.df != my.ome$upload.dataset) {
      HTML("The project/dataset used in the current dataframe is not the same as the one from last upload so no upload info can be added.")
    } else {
      actionButton("addUploadInfo", "Add info from upload")
    }
  })
  
  # If the user click on addUploadInfo
  # the dataframes are merged
  observeEvent(input$addUploadInfo, {
    # We assume that my.ome$current.dataframe corresponds to what is in importO()
    my.ome$current.dataframe <- merge(my.ome$current.dataframe, parseImportOutput(importO()), all.x = T)
  })
  
  # Render the dataframes
  output$toMergeDF <- renderDataTable(my.ome$toMerge.dataframe)
  
  output$currentDF <- renderDataTable(my.ome$current.dataframe)
  
  # Select and Text input if needed to select images
  output$selectImagesIfNeeded <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "setColumn\n")
      cat(file = stderr(), input$imagesSel, "\n")
    }
    if (is.null(input$imagesSel) || input$imagesSel == "all"){
      HTML("")
    } else {
      list(selectInput("selectColumn", "Select the column used to select your images",
                       choices = colnames(my.ome$current.dataframe)),
           textInput("patternImages",
                     "Put here a word which is specific to the group of images to select",
                     value = ""))
      
    }
  })
  
  # Update toMerge.dataframe to
  # Be sure it contains the selected images
  observe({
    if (is.null(input$imagesSel)){
      return()
    }
    if (nrow(my.ome$current.dataframe) == 0){
      return()
    }
    if (input$imagesSel == "all" && !all(my.ome$current.dataframe$id %in% my.ome$toMerge.dataframe$id)){
      my.ome$toMerge.dataframe <- subset(my.ome$current.dataframe, select = c(id, image.name))
    } else if(input$imagesSel == "some"){
      if (is.null(input$patternImages) || input$patternImages == ""){
        return()
      }
      # I put a tryCatch if something is wrong with grep
      matching.ids <- tryCatch(my.ome$current.dataframe$id[grep(input$patternImages, my.ome$current.dataframe[, input$selectColumn])],
                               error = function(e){
                                 cat(file = stderr(), e, "\n")
                                 my.ome$current.dataframe$id
                               })
      if (! all(matching.ids %in% my.ome$toMerge.dataframe$id) || ! all(my.ome$toMerge.dataframe$id %in% matching.ids)){
        my.ome$toMerge.dataframe <- subset(my.ome$current.dataframe, subset = id %in% matching.ids, select = c(id, image.name))
      }
    }
  })
  
  # Display the selectKey UI if possible with custom choices
  output$selectKeyIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "setKey\n")
    }
    if (nrow(my.ome$toMerge.dataframe) == 0){
      HTML("")
    } else {
      selectizeInput("selectKey", "Select the key for your selected images",
                     choices = unique(c(names(my.ome$existing.key.values), my.ome$lastKeySelected)),
                     options = list(create = TRUE),
                     selected = my.ome$lastKeySelected
      )
    }
  })
  
  # Only display UI if relevent
  output$selectionValueIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "selectVals\n")
    }
    if (nrow(my.ome$toMerge.dataframe) == 0 || is.null(input$selectKey)){
      HTML("")
    } else {
      radioButtons("selectionValue", "How to select values:",
                   choices = list("Same value for all"="fixed", "From name"="split"),
                   inline = T, selected = my.ome$lastSelectionValue)
    }
  })
  
  # Only display UIs depending on other select choices
  output$selectValueIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "setVals\n")
      cat(file = stderr(), nrow(my.ome$toMerge.dataframe), "\n")
      cat(file = stderr(), input$selectKey, "\n")
    }
    if (nrow(my.ome$toMerge.dataframe) == 0 || is.null(input$selectKey)){
      HTML("")
    } else{if(is.null(input$selectionValue) || input$selectionValue == "fixed") {
      my.choices <- NULL
      if (my.ome$debug.mode){
        cat(file = stderr(), input$selectKey, "\n")
        cat(file = stderr(), names(my.ome$existing.key.values), "\n")
      }
      if(input$selectKey %in% names(my.ome$existing.key.values)){
        my.choices <- sort(my.ome$existing.key.values[[input$selectKey]])
      }
      list(
        selectizeInput("selectValue",
                       "Select the value for your selected images",
                       choices = my.choices,
                       options = list(create = TRUE)
        ),
        actionButton("addKeyVal", "Add this key/value")
      )
    } else {
      list(selectInput("selectColumnValue", "Select the column used to guess your value",
                       choices = colnames(my.ome$current.dataframe),
                       selected = my.ome$lastSelectedColumnValue),
           textInput("splitCharacter",
                     "What is the character to use to split the column value (you can use | to split with multiple).",
                     value = my.ome$lastSplitCharacter),
           numericInput("splitPos",
                        "Which position?",
                        value = my.ome$lastSplitPosition),
           actionButton("addKeyValSplit", "Fill the dataframe with this info"))
    }
      
    } 
  })
  
  # When the user click on addKeyVal (same value)
  # Update the toMerge.dataframe
  observeEvent(input$addKeyVal, {
    my.ome$toMerge.dataframe[, input$selectKey] <- input$selectValue
    my.ome$lastKeySelected <- input$selectKey
    my.ome$lastSelectionValue <- "fixed"
  })
  
  # When the user click on addKeyValSplit (value based on split)
  # Update the toMerge.dataframe
  observeEvent(input$addKeyValSplit, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "From split\n")
    }
    my.values.to.split <- my.ome$toMerge.dataframe[, input$selectColumnValue]
    # I put a tryCath because strsplit may raise an error
    my.splitted.values <- tryCatch(strsplit(my.values.to.split, input$splitCharacter),
                                   error = function(e) {
                                     cat(file = stderr(), e, "\n")
                                     NA
                                   })
    my.values <- sapply(my.splitted.values, function(v){
      if (length(v) < input$splitPos){
        return(NA)
      } else {
        return(v[input$splitPos])
      }
    })
    if (my.ome$debug.mode){
      cat(file = stderr(), my.values, "\n")
    }
    if (all(is.na(my.values))){
      my.ome$lastSplitCharacter <- paste("Choose something else than:",input$splitCharacter, "(it failed)")
    } else {
      my.ome$toMerge.dataframe[, input$selectKey] <- my.values
      my.ome$lastSplitCharacter <- input$splitCharacter
    }
    my.ome$lastKeySelected <- input$selectKey
    my.ome$lastSelectionValue <- "split"
    my.ome$lastSelectedColumnValue <- input$selectColumnValue
    my.ome$lastSplitPosition <- input$splitPos
  })
  
  # Display the button mergeDF if appropriate
  # Add a warning message if values would be erased.
  output$mergeDFIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "UPDATE MERGE\n")
    }
    if (nrow(my.ome$toMerge.dataframe) == 0){
      return()
    } else {
      if (ncol(my.ome$toMerge.dataframe) == 2){
        return()
      } else {
        extra.cols <- setdiff(colnames(my.ome$toMerge.dataframe), c("id", "image.name"))
        values <- unlist(na.omit(my.ome$current.dataframe[my.ome$current.dataframe$id %in% my.ome$toMerge.dataframe$id,
                                                          intersect(extra.cols, colnames(my.ome$current.dataframe))]))
        output <- NULL
        if (length(values) > 0){
          output <- list(HTML("<h3>Warning: This will erase existing key values from the current data frame.</h3><br>"))
        }
        return(c(output,
                 list(
                   actionButton("mergeToCurrent", "Merge with the existing key values."))))
      }
    }
  })
  
  # When the user click on mergeToCurrent
  # merge toMerge.dataframe to current.datafame
  # if columns are common do it nicely
  observeEvent(input$mergeToCurrent, {
    extra.cols <- setdiff(colnames(my.ome$toMerge.dataframe), c("id", "image.name"))
    existing.extra.cols <- intersect(extra.cols, colnames(my.ome$current.dataframe))
    if (my.ome$debug.mode){
      cat(file = stderr(), "MERGE\n")
      cat(file = stderr(), extra.cols, "\n")
      cat(file = stderr(), existing.extra.cols, "\n")
    }
    if (length(existing.extra.cols) == 0){
      my.ome$current.dataframe <- merge(my.ome$current.dataframe, my.ome$toMerge.dataframe, all.x = T)
    } else {
      if (my.ome$debug.mode){
        cat(file = stderr(), "MERGE COMMON COLS\n")
      }
      my.ome$current.dataframe <- merge(my.ome$current.dataframe,
                                        my.ome$toMerge.dataframe[, setdiff(colnames(my.ome$toMerge.dataframe),
                                                                           existing.extra.cols)], all.x = T)
      my.ids <- my.ome$toMerge.dataframe$id
      for (my.col in existing.extra.cols){
        if (my.ome$debug.mode){
          cat(file = stderr(), my.col, "\n")
        }
        my.ome$current.dataframe[match(my.ids, my.ome$current.dataframe$id), my.col] <- my.ome$toMerge.dataframe[, my.col]
      }
    }
    # Update the existing key values:
    for (my.col in extra.cols){
      if (my.col %in% names(my.ome$existing.key.values)){
        new.vals <- setdiff(unique(na.omit(my.ome$toMerge.dataframe[, my.col])), my.ome$existing.key.values[[my.col]])
        if (length(new.vals) > 0){
          my.ome$existing.key.values[[my.col]] <- c(my.ome$existing.key.values[[my.col]], new.vals)
        }
      } else {
        my.ome$existing.key.values[[my.col]] <- unique(na.omit(my.ome$toMerge.dataframe[, my.col]))
      }
    }
    my.ome$toMerge.dataframe <- data.frame()
  })
  
  # Display messages on differences
  # between current.dataframe and original.dataframe
  output$currentDFstatus <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "UPDATE DF STATUS\n")
    }
    final.output.text <- NULL
    if (nrow(my.ome$original.dataframe) == 0){
      final.output.text <- ""
      my.ome$current.is.ori <- TRUE
    } else {
      if (ncol(my.ome$original.dataframe) == ncol(my.ome$current.dataframe) &&
          all(is.na(my.ome$original.dataframe) == is.na(my.ome$current.dataframe[match(my.ome$original.dataframe$id, my.ome$current.dataframe$id),
                                                                                 colnames(my.ome$original.dataframe)])) &&
          all(my.ome$original.dataframe == my.ome$current.dataframe[match(my.ome$original.dataframe$id, my.ome$current.dataframe$id),
                                                                    colnames(my.ome$original.dataframe)],
              na.rm = T)){
        final.output.text <- c("Nothing to upload", "No need to click")
        my.ome$current.is.ori <- TRUE
      } else {
        if (my.ome$debug.mode){
          cat(file = stderr(), "ELSE\n")
        }
        extra.cols <- setdiff(colnames(my.ome$current.dataframe), c("id", "image.name"))
        existing.extra.cols <- intersect(extra.cols, colnames(my.ome$original.dataframe))
        new.extra.cols <- setdiff(extra.cols, colnames(my.ome$original.dataframe))
        output <- ""
        if (length(new.extra.cols) > 0){
          output <- paste("This will add new keys:", paste(new.extra.cols, collapse = ", "))
        }
        if (my.ome$debug.mode){
          cat(file = stderr(), existing.extra.cols, "\n")
        }
        if (all(is.na(my.ome$original.dataframe[, existing.extra.cols]) == 
                is.na(my.ome$current.dataframe[match(my.ome$original.dataframe$id, my.ome$current.dataframe$id),
                                               existing.extra.cols])) &&
            all(my.ome$original.dataframe[, existing.extra.cols] == 
                my.ome$current.dataframe[match(my.ome$original.dataframe$id, my.ome$current.dataframe$id),
                                         existing.extra.cols],
                na.rm = T)){
          final.output.text <- output
          my.ome$current.is.ori <- FALSE
        } else {
          changed.values.coo <- which(! is.na(my.ome$original.dataframe[, existing.extra.cols]) &
                                        my.ome$original.dataframe[, existing.extra.cols] != 
                                        my.ome$current.dataframe[match(my.ome$original.dataframe$id, my.ome$current.dataframe$id),
                                                                 existing.extra.cols],
                                      arr.ind = T)
          
          if (my.ome$debug.mode){
            cat(file = stderr(), changed.values.coo, "\n")
          }
          if (length(existing.extra.cols) == 1){
            if (length(changed.values.coo) == 0){
              final.output.text <- c("This will add values for the existing key",
                                     output)
              my.ome$current.is.ori <- FALSE
            } else {
              final.output.text <- c("Warning: this will erase some key values:",
                                     paste(existing.extra.cols, "for image",
                                           paste(my.ome$original.dataframe[changed.values.coo, "image.name"],
                                                 collapse = ", ")),
                                     output)
              my.ome$current.is.ori <- FALSE
            }
          } else {
            if (nrow(changed.values.coo) == 0){
              final.output.text <- c("This will add values for existing keys",
                                     output)
              my.ome$current.is.ori <- FALSE
            } else {
              final.output.text <- c("Warning: this will erase some key values:",
                                     apply(aggregate(list(image = my.ome$original.dataframe[changed.values.coo[, "row"], "image.name"]),
                                                     by = list(key = existing.extra.cols[changed.values.coo[, "col"]]),
                                                     paste, collapse = ", "),
                                           1, paste, collapse = " for image "),
                                     output)
              my.ome$current.is.ori <- FALSE
            }
          }
        }
      }
    }
    HTML(paste(final.output.text, collapse = "<br/>"))
  })
  
  # Substitute NA by "" before downloading
  # Use a file name with all info
  output$downloadDF <- downloadHandler(
    filename = function() {
      paste0(my.ome$project.df, "__", my.ome$dataset.df, "__", gsub(" ", "_", Sys.time()), ".csv")
    },
    content = function(file) {
      df <- my.ome$current.dataframe
      df[is.na(df)] <- ""
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Display button only if relevent
  output$uploadDFIfNeeded <- renderUI({
    if (my.ome$current.is.ori){
      HTML("")
    } else {
      actionButton("uploadDFtoOMERO", "Upload this data frame to OMERO and update the key values")
    }
  })
  
  # When the user click on the button
  # This will attach a csv to the dataset
  observeEvent(input$uploadDFtoOMERO, {
    # The csv should not have the "id" col but the "picture.name"
    # If there is a duplicated picture name no csv will be attached.
    output <- NULL
    df <- my.ome$current.dataframe
    df[is.na(df)] <- ""
    if (anyDuplicated(my.ome$current.dataframe$picture.name) != 0){
      output <- "There are duplicated picture name. No csv will be attached to the dataset"
    } else {
      tmp.fn <- file.path(tempdir(), paste0(gsub(" ", "_", Sys.time()), "_key_values.txt"))
      write.csv(df[, c("image.name", 
                       setdiff(colnames(df), c("id", "image.name")))],
                file = tmp.fn, row.names = FALSE)
      my_dataset <- loadObject(my.ome$server, "DatasetData", unname(my.ome$datasets.ids[my.ome$dataset.df]))
      invisible(attachFile(my_dataset, tmp.fn))
    }
  })
  
  # When the user click on the button
  # This will add the key values to images
  # This uses a python script
  outputUploadDF <- eventReactive(input$uploadDFtoOMERO, {
    output <- NULL
    df <- my.ome$current.dataframe
    df[is.na(df)] <- ""
    tmp.fn <- tempfile()
    write.csv(df, file = tmp.fn, row.names = FALSE)
    # Contrary to the batch annotation this will erase
    # current key values.
    system(
      paste0(gsub("omero$", "python", omero.path),
             " external_scripts/update_key_values_from_file.py",
             " --server omero-server.epfl.ch --user \'",
             input$username, "\' --password \'",
             input$password, "\' --file \'",
             tmp.fn, "\' --sep ','  2>&1"),
      intern = T)
  })
  
  # uploadDFtoOMERO is launched only if this output is active
  output$outputFUploadDF <- renderPrint({
    cat(outputUploadDF(), sep = "\n")
  })
  
  # If the user click on the debug mode
  # A lot of prints to the stderr
  eventReactive(input$debugMode,{
    my.ome$debug.mode <- input$debugMode
  })
  
  # This is a print in the debug tab
  output$debug <- renderPrint({
    for(key in names(my.ome)){
      cat("---\n", key, "\n---\n")
      print(my.ome[[key]])
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
