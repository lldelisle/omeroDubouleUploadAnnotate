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
library(promises)
library(future)
plan(multisession)

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
                           actionButton("login", label = div(icon("registered"), icon("python"), "LOG IN")),
                           uiOutput("loginUI") # This will give you a message with the state of your login
                  ),
                  # Project Dataset where you select your project / dataset
                  tabPanel("Project Dataset",
                           checkboxInput("useronly", "Display only the data from the logged-in user", value = T),
                           actionButton("update", label = "Update choices in Project/Dataset lists", icon = icon("registered")),
                           uiOutput("projectSelect"),
                           uiOutput("datasetSelect"),
                           textOutput("lastUpload"), # Gives you the status of the last upload
                           textOutput("currentKeyval"), # Gives you the Project/Dataset used in the Annotation tab
                           actionButton("updateKeyValues", label = "Reset choices in key values lists", icon = icon("python")),
                           actionButton("disconnect", label = "Disconnect", icon = icon("registered"))
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
                           actionButton("checkUpload", "Check what will be uploaded.", icon = icon("hashtag"),
                                        style = "background-color : #ffd56a"),
                           helpText("It can be long... Be patient. Once done an output will be printed below."),
                           verbatimTextOutput("outputF"),
                           uiOutput("UploadIfPossible"), # Upload button on output only if logged in
                           uiOutput("CheckCurrentUploadIfPossible") # CheckUpload button only if logged in
                  ), 
                  tabPanel("Annotation",
                           h3("First choose the level to annotate"),
                           selectInput("annotationScale",
                                       "At which scale do you want to annotate:",
                                       choices = c("All my projects"="user",
                                                   "All datasets in the selected project"="project",
                                                   "All images in the selected dataset"="dataset"),
                                       selected = "dataset",
                                       multiple = F),
                           uiOutput("prepareDFIfPossible"), # Only display the button if the project and/or dataset exists 
                           h3("Info from upload"),
                           p("Info from upload goes directly to the bottom dataframe."),
                           uiOutput("addUploadInfoIfPossible"), # Only display the button if it is the good project, the good dataset and there was a successful upload
                           uiOutput("addPreviousUploadInfoIfPossible"), # Only display choices and button if the dataset exists and there are upload files
                           h3("Acquisition date"),
                           p("Info from acquisition date goes directly to the bottom dataframe."),
                           uiOutput("addAcquisitionDateIfPossible"), # Only display the button if a current.df exists
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
                           downloadButton("downloadDF", div(icon("download"), icon("registered"), "Download this table"), icon = NULL),
                           uiOutput("currentDFstatus"),
                           uiOutput("uploadDFIfNeeded"), # Only display the button if there is a change
                           helpText("Once the key values are updated an output will be printed below."),
                           verbatimTextOutput("outputFUploadDF") # Get the output of the upload key values script
                  ),
                  tabPanel("Simple Search",
                           uiOutput("nbKVUI"), # UI with the number of KV to use to filter
                           fluidRow(column(width = 4, uiOutput("searchKselect")), # One column with the keys
                                    column(width = 4, uiOutput("searchVselect"))), # One column with the values
                           actionButton("searchKV", "Search", icon = icon("python")), # Launch the query
                           dataTableOutput("foundKV"), # dataframe with results
                           downloadButton("downloadFoundDF", div(icon("download"), icon("registered"), "Download this table"), icon = NULL)
                  ), 
                  tabPanel("Download",
                           helpText("Mind the checkbox above the project selection to see if you want all or only yours."),
                           downloadButton("downloadAllKV", div(icon("download"), icon("python"), "Download table with all images with key values"), icon = NULL)
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
                           tmp.fn.password = NULL, # Path with the file where the password is stored
                           update = 0, # Counter to detect when the projects should be updated
                           update.keyval = 0, # Counter to detect when the key values should be fetched from OMERO
                           projects = NULL, # List of projects obtained from GetProjects.
                           projects.ids = NULL, # Named vector with ids of projects in projects, names are project names
                           datasets.ids = NULL, # Named vector with ids of datasets in input$projectSelected, names are dataset names
                           upload.project = "", # Project name used at upload
                           upload.dataset = "", # Dataset name used at upload
                           upload.path = "", # Path of dir or files to upload
                           lastUploadFile = "", # Path to temp file with the stdout of last upload
                           original.dataframe = data.frame(), # Dataframe obtained from OMERO for the project.df and dataset.df
                           current.dataframe = data.frame(), # Dataframe enriched from original.dataframe with new key values to upload to OMERO
                           project.df = "", # Project name corresponding to all dataframes
                           dataset.df = "", # Dataset name corresponding to all dataframes
                           toMerge.dataframe = data.frame(), # Dataframe with potentially a subset of rows compare to other dataframe and new columns to be added to current.dataframe
                           current.is.ori = TRUE, # If there are differences between original.dataframe and current.dataframe
                           existing.key.values = list(), # A named list where names are keys and items are vectors with used values in OMERO by the group
                           lastKeySelected = "image.name", # Just used to keep what has been selected before
                           lastSelectedColumn = "image.name", # Just used to keep what has been selected before
                           lastSelectedPattern = "", # Just used to keep what has been selected before
                           lastSelectionValue = "fixed", # Just used to keep what has been selected before
                           lastSelectedColumnValue = "image.name", # Just used to keep what has been selected before
                           lastSplitCharacter = "_", # Just used to keep what has been selected before
                           lastSplitPosition = 1, # Just used to keep what has been selected before
                           debug.mode = FALSE # Logical whether there should be print
                           
  )
  # This reactive value is separated as it depends on future promise
  # It is NULL, FALSE or TRUE
  successUpload <- reactiveVal()
  
  # When the user click on the login button
  # my.ome$server is updated as well as valid.login and failed.login
  # It also modified the 2 counters to update the projects and key/values
  observeEvent(input$login, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "login\n")
    }
    my.ome$server <- tryCatch(connect(OMEROServer(host = "omero-server.epfl.ch", username = input$username, password = input$password, port = as.integer(4064))),
                              error = function(e) {
                                cat(file = stderr(), str(e), "\n")
                                NULL
                              })
    if (! is.null(my.ome$server)){
      my.ome$valid.login <- TRUE
      my.ome$failed.login <- FALSE
      my.ome$update <- my.ome$update + 1
      my.ome$update.keyval <- my.ome$update.keyval + 1
      my.ome$tmp.fn.password <- tempfile()
      cat(input$password, file = my.ome$tmp.fn.password)
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
    my.ome$original.dataframe <- data.frame()
    my.ome$current.dataframe <- data.frame()
    my.ome$project.df <- ""
    my.ome$dataset.df <- ""
    my.ome$toMerge.dataframe <- data.frame()
    my.ome$current.is.ori <- TRUE
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
                                                         my.ome$tmp.fn.password, "\'"),
                                                  intern = T))
      
      if (my.ome$debug.mode){
        cat(file = stderr(), str(my.ome$existing.key.values), "\n")
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
      showTab(inputId = "tabsMain", target = "Simple Search")
      showTab(inputId = "tabsMain", target = "Download")
    } else {
      hideTab(inputId = "tabs", target = "Project Dataset")
      showTab(inputId = "tabs", target = "Login")
      hideTab(inputId = "tabsMain", target = "Annotation")
      hideTab(inputId = "tabsMain", target = "Simple Search")
      hideTab(inputId = "tabsMain", target = "Download")
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
      cat(file = stderr(), str(my.choices), "\n")
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
          cat(file = stderr(), str(which(names(my.ome$projects.ids) == input$projectSelected)), "\n")
        }
        my.datasets <- unlist(lapply(which(names(my.ome$projects.ids) == input$projectSelected),
                                     function(i){getDatasets(my.ome$projects[[i]])}))
        all.datasets.obj <- sapply(my.datasets, slot, name = "dataobject")
        my.choices <- sapply(all.datasets.obj, function(ob){ob$getId()})
        names(my.choices) <- sapply(all.datasets.obj, function(ob){ob$getName()})
        my.ome$datasets.ids <- my.choices
        if (nrow(my.ome$current.dataframe) > 0 && 
            ! my.ome$dataset.df %in% names(my.choices) &&
            input$annotationScale == "dataset"){
          # The project changed we reset all dataframes
          my.ome$original.dataframe <- data.frame()
          my.ome$current.dataframe <- data.frame()
          my.ome$project.df <- ""
          my.ome$dataset.df <- ""
          my.ome$toMerge.dataframe <- data.frame()
          my.ome$current.is.ori <- TRUE
        }
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
    if ( my.ome$dataset.df != ""){
      my.selected <- my.ome$dataset.df
    } else {
      my.selected <- my.ome$upload.dataset
    }
    selectizeInput("datasetSelected",
                   "Select existing dataset or write a new one",
                   choices = my.choices,
                   options = list(create = TRUE),
                   selected = my.selected,
                   multiple = F)
  })
  
  # importF() contains the output of omero import
  importF <- eventReactive(input$checkUpload, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "importF\n")
    }
    depth <- input$depth
    fileOrDirToUpload <- input$fileOrDirToUpload
    future_promise({ 
      if (fileOrDirToUpload == ""){
        "Set the file or dir"
      } else {
        # system(paste0(omero.path, " import --depth ", input$depth, " -f \'", prefix.path, "/", input$fileOrDirToUpload, "\' 2>&1"), intern = T,
        #        ignore.stdout = F, ignore.stderr = F)
        system(paste0(omero.path, " import --depth ", depth, " -f \'", prefix.path, "/", fileOrDirToUpload, "\'"), intern = T,
               ignore.stdout = F, ignore.stderr = F)
      }
    })
  })
  
  # importF is launched only if this output is active
  output$outputF <- renderPrint({
    importF() %...>% cat(sep = "\n")
  })
  
  # button upload and output text if logged in
  output$UploadIfPossible <- renderUI({
    if(! my.ome$valid.login){
      HTML("")
    } else if (input$fileOrDirToUpload == "") {
      HTML("")
    } else {
      list(
        h3("Upload"),
        actionButton("upload", "Upload using parameters above.", icon = icon("hashtag"),
                     style = "background-color : #ffd56a"),
        helpText("It can be super long... Be patient. Once done an output will be printed below."),
        verbatimTextOutput("shortOutputUpload"))
    }
  })
  
  # successUpload() contains NULL (On going), TRUE (success)  or FALSE (failure)
  observeEvent(input$upload, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "Click on upload will launch the commandline\n")
    }
    fileOrDirToUpload <- input$fileOrDirToUpload
    depth <- input$depth
    to_create <- "none"
    if (input$projectSelected %in% names(my.ome$projects.ids)){
      project_name_or_id <- my.ome$projects.ids[which(names(my.ome$projects.ids) == input$projectSelected)[1]]
      if (input$datasetSelected %in% names(my.ome$datasets.ids)){
        dataset_name_or_id <- my.ome$datasets.ids[which(names(my.ome$datasets.ids) == input$datasetSelected)[1]]
      } else {
        dataset_name_or_id <- input$datasetSelected
        to_create <- "dataset"
      }
    } else {
      project_name_or_id <- input$projectSelected
      dataset_name_or_id <- input$datasetSelected
      to_create <- "both"
    }
    valid.login <- my.ome$valid.login
    debug.mode <- my.ome$debug.mode
    host <- my.ome$server@host
    username <- my.ome$server@username
    successUpload(NULL)
    my.ome$lastUploadFile <- file.path(tempdir(), paste0(gsub(" ", "_", Sys.time()), "_upload.log"))
    lastUploadFile <- my.ome$lastUploadFile
    tmp.fn.output.and.error <- tempfile()
    tmp.fn.password <- my.ome$tmp.fn.password
    if (debug.mode){
      cat(file = stderr(), paste0("bash external_scripts/upload_and_add_log.sh \"",
                                  paste(omero.path, host, username, tmp.fn.password,
                                        depth, project_name_or_id, dataset_name_or_id,
                                        paste0(prefix.path, "/", fileOrDirToUpload), lastUploadFile,
                                        sep = "\" \""),
                                  "\" ", to_create, " 2>&1 > \"", tmp.fn.output.and.error, "\""),
          "\n")
    }
    future_promise({
      # I don't know why I don't see that.
      if (debug.mode){
        cat(file = stderr(), "HERE\n")
      }
      system(paste0("bash external_scripts/upload_and_add_log.sh \"",
                    paste(omero.path, host, username, tmp.fn.password,
                          depth, project_name_or_id, dataset_name_or_id,
                          paste0(prefix.path, "/", fileOrDirToUpload), lastUploadFile,
                          sep = "\" \""),
                    "\" ", to_create, " 2>&1 > \"", tmp.fn.output.and.error, "\"")
      )
      if (! file.exists(lastUploadFile)){
        cat("Something went wrong.\n", file = lastUploadFile)
      }
      std.output <- readLines(lastUploadFile)
      if ("==> Summary" %in% std.output){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }) %...>% successUpload()
  })
  
  observeEvent(successUpload(), {
    if (my.ome$debug.mode){
      cat(file = stderr(), "SUCCESSUPLOAD CHANGED\n")
    }
    if (is.null(req(successUpload()))){
      return()
    }
    if (req(successUpload()) && ! my.ome$upload.dataset %in% names(my.ome$datasets.ids)){
      if (my.ome$debug.mode){
        cat(file = stderr(), "THIS IS A SUCCESS UPLOAD TO A NEW DATASET\n")
      }
      my.ome$update <- my.ome$update + 1
    }
  })
  
  # Text with summary of successUpload()
  output$shortOutputUpload <- renderText({
    if (my.ome$debug.mode){
      cat(file = stderr(), "CHANGING THE OUTPUT OF UPLOAD\n")
      if (my.ome$upload.project != ""){
        cat(file = stderr(), req(successUpload()))
      }
    }
    if (is.null(req(successUpload()))){
      ""
    } else {
      std.output <- readLines(my.ome$lastUploadFile)
      if (my.ome$upload.project != ""){
        std.output <- readLines(my.ome$lastUploadFile)
        std.output[which(std.output == "==> Summary") + 1]
      } else {
        paste(std.output, collapse = "\n")
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
  
  # Text on upload info
  output$lastUpload <- renderText({
    if (is.null(req(successUpload()))){
      "No successful upload"
    }
    if (req(successUpload())){
      paste0("Last upload is ", my.ome$upload.path,
             " in ", my.ome$upload.project, "/",
             my.ome$upload.dataset)
    } else {
      "No successful upload"
    }
  })
  
  # button checkCurrentUpload and output verbatim if logged in
  output$CheckCurrentUploadIfPossible <- renderUI({
    if(! my.ome$valid.login){
      HTML("")
    } else {
      list(
        h3("On going uploads"),
        actionButton("checkCurrentUpload", "Check which are the uploads on going.", icon = icon("hashtag")),
        dataTableOutput("currentUpload"))
    }
  })
  
  currentUploadDF <- eventReactive(input$checkCurrentUpload,{
    if (my.ome$debug.mode){
      cat(file = stderr(), "getCurrentUpload\n")
    }
    csv.text <- system(paste0("ps aux |",
                              " grep \"sh -c bash external_scripts/upload_and_add_log.sh\" |",
                              " grep -v grep |",
                              " awk -F \"\\\"\" '{split($1, a, \" \"); print a[9]\",\"$6\",\"$12\",\"$14}'"),
                       intern = T)
    if (my.ome$debug.mode){
      cat(file = stderr(), csv.text, "\n")
    }
    df <- read.csv(text = csv.text,
                   col.names = c("started", "omero-user", "project", "dataset"),
                   header = F)
    return(df)
  })
  
  output$currentUpload <- renderDataTable(currentUploadDF())
  
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
    } else if (input$annotationScale == "user" && ! input$useronly){
      HTML("You first need to uncheck the box 'Display only the data from the logged-in user'")
    } else if (input$annotationScale == "user" && length(my.ome$projects.ids) == 0){
      HTML("You first need to upload data")
    } else if (! input$projectSelected %in% names(my.ome$projects.ids) && input$annotationScale %in% c("project", "dataset")){
      HTML("You need to choose an existing project")
    } else if (! input$datasetSelected %in% names(my.ome$datasets.ids) && input$annotationScale == "dataset"){
      HTML("You need to choose an existing dataset")
    } else {
      if (nrow(my.ome$current.dataframe) == 0){
        header.text <- "<h3> Then click here </h3>"
        button.text <- "Generate the dataframe from existing key values"
      } else {
        header.text <- paste0("To (re)generate the dataframe at the bottom. Click here <br/>")
        button.text <- paste0("(Re-)generate")
      }
      return(list(HTML(header.text),
                  actionButton("prepareDF", button.text, icon = icon("python"))))
    }
  })
  
  # If the user click on prepareDF
  # All key values for each image of the dataset
  # are fitted in current.dataframe and original.dataframe
  observeEvent(input$prepareDF, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "prepareDF changed\n")
    }
    if (input$annotationScale == "user"){
      suffix <- " --onlyUserProjects"
      my.ome$project.df <- ""
      my.ome$dataset.df <- ""
    } else if(input$annotationScale == "project"){
      suffix <- paste(" --selectedProject", my.ome$projects.ids[input$projectSelected][1])
      my.ome$project.df <- input$projectSelected
      my.ome$dataset.df <- ""
    } else{ #} if(input$annotationScale == "dataset"){
      suffix <- paste(" --selectedDataset", my.ome$datasets.ids[input$datasetSelected][1])
      my.ome$project.df <- input$projectSelected
      my.ome$dataset.df <- input$datasetSelected
    }
    if (my.ome$debug.mode){
      cat(file = stderr(), paste0(gsub("omero$", "python", omero.path),
                                  " external_scripts/get_all_key_values_per_image.py",
                                  " --server omero-server.epfl.ch --user \'",
                                  input$username, "\' --password \'",
                                  my.ome$tmp.fn.password, "\'", suffix),
          "\n")
    }
    my.text <- system(
      paste0(gsub("omero$", "python", omero.path),
             " external_scripts/get_all_key_values_per_image.py",
             " --server omero-server.epfl.ch --user \'",
             input$username, "\' --password \'",
             my.ome$tmp.fn.password, "\'", suffix),
      intern = T)
    if (length(my.text) > 0){
      df <- read.csv(text = my.text)
      colnames(df) <-strsplit(my.text[1], ",")[[1]]
    } else {
      if (my.ome$debug.mode){
        cat(file = stderr(), "No image\n")
      }
      df <- data.frame()
    }
    my.ome$current.dataframe <- df
    if (my.ome$debug.mode){
      cat(file = stderr(), "initiateDF OK\n")
    }
    my.ome$original.dataframe <- df
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
    if ( is.null(req(successUpload()))){
      HTML("")
    }
    if (! req(successUpload())){
      HTML("")
    } else if (nrow(my.ome$current.dataframe) == 0 || 
               is.null(my.ome$dataset.df) ||
               input$projectSelected != my.ome$upload.project ||
               input$datasetSelected != my.ome$upload.dataset){
      HTML("To add upload info, you need to generate the dataframe from existing values with the corresponding project and dataset.")
      # } else if (my.ome$project.df != input$projectSelected ||
      #            my.ome$dataset.df != input$datasetSelected) {
      #   HTML("The project/dataset used to generate the current dataframe is not the same as the one selected on top, regenerate the dataframe.")
    } else if (my.ome$project.df != my.ome$upload.project || my.ome$dataset.df != my.ome$upload.dataset) {
      HTML("The project/dataset used in the current dataframe is not the same as the one from last upload so no upload info can be added.")
    } else {
      actionButton("addUploadInfo", "Add info from upload", icon = icon("registered"))
    }
  })
  
  # If the user click on addUploadInfo
  # the dataframes are merged
  observeEvent(input$addUploadInfo, {
    # We assume that my.ome$current.dataframe corresponds to what is in my.ome$lastUploadFile
    std.output <- readLines(my.ome$lastUploadFile)
    my.ome$current.dataframe <- mergeNicely(my.ome$current.dataframe, parseImportOutput(std.output), my.ome$debug.mode)
  })
  
  # Button to add upload path for corresponding image ids
  # For previous uplaod
  # Only if possible
  output$addPreviousUploadInfoIfPossible <- renderUI({
    if (my.ome$debug.mode){
      cat(file = stderr(), "addPreviousUploadInfoIfPossible\n")
      cat(file = stderr(), nrow(my.ome$current.dataframe), "\n")
    }
    if (nrow(my.ome$current.dataframe) == 0){
      # HTML("To add upload info from previous uplaods, you need to generate the dataframe from existing values.")
      HTML("")
    } else if (my.ome$dataset.df == ""){
      HTML("To add upload info from previous uploads, you need to generate the dataframe with a dataset.")
    }else {
      my_dataset <- loadObject(my.ome$server, "DatasetData", unname(my.ome$datasets.ids[my.ome$dataset.df]))
      my_annotations <- getAnnotations(my_dataset)
      if (nrow(my_annotations) == 0){
        HTML("")
      } else {
        upload.files.ids <- my_annotations$FileID[grep("upload.log$", my_annotations$Name)]
        if (length(upload.files.ids) == 0){
          HTML("")
        } else {
          names(upload.files.ids) <- grep("upload.log$", my_annotations$Name, value = T)
          upload.files.ids <- upload.files.ids[order(names(upload.files.ids))]
          list(selectInput("previousUploadFileID", "Select the file for which you want to add the info",
                           choices = upload.files.ids),
               actionButton("addPreviousUploadInfo", "Add info from this upload", icon = icon("registered")))
        }
      }
    }
  })
  
  # If the user click on addPreviousUploadInfo
  # the dataframes are merged
  observeEvent(input$addPreviousUploadInfo, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "addPreviousUploadInfo\n")
      cat(file = stderr(), input$previousUploadFileID, "\n")
    }
    df <- loadCSV(my.ome$server, input$previousUploadFileID,
                  header = F, sep = ";")
    if (my.ome$debug.mode){
      cat(file = stderr(), str(df), "\n")
    }
    my.ome$current.dataframe <- mergeNicely(my.ome$current.dataframe, parseImportOutput(df$V1), my.ome$debug.mode)
  })
  
  output$addAcquisitionDateIfPossible <- renderUI({
    if (nrow(my.ome$current.dataframe) == 0){
      # HTML("To add upload info from previous uplaods, you need to generate the dataframe from existing values.")
      HTML("")
    } else {
      actionButton("addAcquisitionDate", "Add info from acquisition date", icon = icon("python"))
    }
  })
  
  # If the user click on addAcquisitionDate
  # the dataframes are merged
  observeEvent(input$addAcquisitionDate, {
    # First get the new dataframe with the python script
    
    if (my.ome$debug.mode){
      cat(file = stderr(), "addAcquisitionDate changed\n")
    }
    if (input$annotationScale == "user"){
      suffix <- " --onlyUserProjects"
    } else if(input$annotationScale == "project"){
      suffix <- paste(" --selectedProject", my.ome$projects.ids[input$projectSelected][1])
    } else{ #} if(input$annotationScale == "dataset"){
      suffix <- paste(" --selectedDataset", my.ome$datasets.ids[input$datasetSelected][1])
    }
    if (my.ome$debug.mode){
      cat(file = stderr(), paste0(gsub("omero$", "python", omero.path),
                                  " external_scripts/get_acquisition_date_per_image.py",
                                  " --server omero-server.epfl.ch --user \'",
                                  input$username, "\' --password \'",
                                  my.ome$tmp.fn.password, "\'", suffix),
          "\n")
    }
    my.text <- system(
      paste0(gsub("omero$", "python", omero.path),
             " external_scripts/get_acquisition_date_per_image.py",
             " --server omero-server.epfl.ch --user \'",
             input$username, "\' --password \'",
             my.ome$tmp.fn.password, "\'", suffix),
      intern = T)
    if (length(my.text) > 0){
      df <- read.csv(text = my.text)
      colnames(df) <-strsplit(my.text[1], ",")[[1]]
    } else {
      if (my.ome$debug.mode){
        cat(file = stderr(), "No image\n")
      }
      return()
    }
    my.ome$current.dataframe <- mergeNicely(my.ome$current.dataframe, df, my.ome$debug.mode)
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
      list(HTML("You can use columns in the dataframe of the bottom"),
           selectInput("selectColumn", "Select the column used to select your images",
                       choices = colnames(my.ome$current.dataframe),
                       selected = my.ome$lastSelectedColumn),
           textInput("patternImages",
                     "Put here a word which is specific to the group of images to select",
                     value = my.ome$lastSelectedPattern))
      
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
                                 cat(file = stderr(), str(e), "\n")
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
        actionButton("addKeyVal", "Add this key/value", icon = icon("registered"))
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
           actionButton("addKeyValSplit", "Fill the dataframe with this info", icon = icon("registered"))
           )
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
    my.values.to.split <- my.ome$current.dataframe[match(my.ome$toMerge.dataframe$id, my.ome$current.dataframe$id), input$selectColumnValue]
    # I put a tryCath because strsplit may raise an error
    my.splitted.values <- tryCatch(strsplit(my.values.to.split, input$splitCharacter),
                                   error = function(e) {
                                     cat(file = stderr(), str(e), "\n")
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
                   actionButton("mergeToCurrent", "Merge with the existing key values.", icon = icon("registered"))
                   )
                 )
               )
      }
    }
  })
  
  # When the user click on mergeToCurrent
  # merge toMerge.dataframe to current.datafame
  # if columns are common do it nicely
  observeEvent(input$mergeToCurrent, {
    extra.cols <- setdiff(colnames(my.ome$toMerge.dataframe), c("id", "image.name"))
    my.ome$current.dataframe <- mergeNicely(my.ome$current.dataframe, my.ome$toMerge.dataframe, my.ome$debug.mode)
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
        extra.cols <- setdiff(colnames(my.ome$current.dataframe),
                              c("id", "image.name", "dataset.name", "dataset.id", "project.name", "user.omename"))
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
            cat(file = stderr(), str(changed.values.coo), "\n")
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
      prefix <- ""
      if (input$annotationScale == "user"){
        prefix <- input$username
      } else if (input$annotationScale == "project"){
        prefix <- my.ome$project.df
      } else { # dataset
        prefix <- paste0(my.ome$project.df, "__", my.ome$dataset.df)
      }
      return(paste0(prefix, "__", gsub(" ", "_", Sys.time()), ".csv"))
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
      actionButton("uploadDFtoOMERO", div(icon("registered"), icon("python"), "Upload this data frame to OMERO and update the key values"),
                   style = "background-color : #ffd56a")
    }
  })
  
  # When the user click on the button
  # This will attach a csv to each of the dataset
  observeEvent(input$uploadDFtoOMERO, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "Trying to attach files.\n")
    }
    # The csv should not have the "id" col, nor the one related to the dataset or project
    # but the "image.name"
    # If there is a duplicated picture name no csv will be attached.
    # The best would be to check for each dataset if something changed before uploading.
    columns.to.ignore <- c("id", "image.name", "dataset.name", "dataset.id", "project.name", "user.omename")
    for (my.dataset.id in unique(my.ome$current.dataframe$dataset.id)){
      if (my.ome$debug.mode){
        cat(file = stderr(), "Trying to attach to ", my.dataset.id, ".\n")
      }
      df <- my.ome$current.dataframe[my.ome$current.dataframe$dataset.id == my.dataset.id, ]
      df[is.na(df)] <- ""
      if (anyDuplicated(df$image.name) != 0){
        if (my.ome$debug.mode){
          cat(file = stderr(), "There are duplicated picture name. No csv will be attached to the dataset", my.dataset.id, ".\n")
        }
      } else {
        tmp.fn <- file.path(tempdir(), paste0(gsub(" ", "_", Sys.time()), "_key_values.txt"))
        write.csv(df[, c("image.name", 
                         setdiff(colnames(df), columns.to.ignore))],
                  file = tmp.fn, row.names = FALSE)
        my_dataset <- loadObject(my.ome$server, "DatasetData", my.dataset.id)
        invisible(attachFile(my_dataset, tmp.fn))
      }
    }
  })
  
  # When the user click on the button
  # This will add the key values to images
  # This uses a python script
  outputUploadDF <- eventReactive(input$uploadDFtoOMERO, {
    if (my.ome$debug.mode){
      cat(file = stderr(), "outputUploadDF\n")
    }
    df <- my.ome$current.dataframe
    df[is.na(df)] <- ""
    tmp.fn <- tempfile()
    write.csv(df, file = tmp.fn, row.names = FALSE)
    tmp.fn.password <- my.ome$tmp.fn.password
    username <- input$username
    # Contrary to the batch annotation this will erase
    # current key values.
    future_promise({ 
      system(
        paste0(gsub("omero$", "python", omero.path),
               " external_scripts/update_key_values_from_file.py",
               " --server omero-server.epfl.ch --user \'",
               username, "\' --password \'",
               tmp.fn.password, "\' --file \'",
               tmp.fn, "\' --sep ','  2>&1"),
        intern = T)
    })
  })
  
  # uploadDFtoOMERO is launched only if this output is active
  output$outputFUploadDF <- renderPrint({
    outputUploadDF() %...>% cat(sep = "\n")
  })
  
  # UI with the number of key to use. Not more than possible keys
  output$nbKVUI <- renderUI({
    numericInput("nbKV", label = "Number of key values to use", value = 1, min = 1, max = length(my.ome$existing.key.values))
  })
  
  # UI with the input to select keys
  # Do not display keys already used
  output$searchKselect <- renderUI({
    if (is.null(input$nbKV)){
      return()
    }
    lapply(1:input$nbKV, function(id){
      my.choices <- names(my.ome$existing.key.values)
      if (id > 1){
        my.choices <- tryCatch(setdiff(names(my.ome$existing.key.values), sapply(1:(id-1), function(previd){isolate(input[[paste0("searchKey", previd)]])})),
                               error = function(e){
                                 print(e)
                                 return(names(my.ome$existing.key.values))
                               })
      } 
      tryCatch(selectizeInput(paste0("searchKey", id), paste("Select the key", id),
                              choices = my.choices,
                              selected = NULL),
               error = function(e){
                 print(e)
                 return(NULL)
               })
    })
  })
  
  # UI with the input to select values
  output$searchVselect <- renderUI({
    if (is.null(input$nbKV)){
      return()
    }
    lapply(1:input$nbKV, function(id){
      tryCatch(selectizeInput(paste0("searchValue", id), paste("Select the value for key", id),
                              choices = sort(my.ome$existing.key.values[[input[[paste0("searchKey", id)]]]]),
                              selected = NULL),
               error = function(e){
                 print(e)
                 return(NULL)
               })
    })
  })
  
  # Dataframe with the result
  foundKVDF <- eventReactive(input$searchKV, {
    key.vals.selected <- # key=value;key=value...
      paste(sapply(1:input$nbKV, function(id){paste(input[[paste0("searchKey", id)]], input[[paste0("searchValue", id)]],
                                                    sep = "=")}), collapse = ";")
    suffix <- ""
    if (input$useronly){
      suffix <- " --onlyUserProjects"
    }
    if (my.ome$debug.mode){
      cat(file = stderr(), paste0(gsub("omero$", "python", omero.path),
                                  " external_scripts/get_images_from_key_values.py",
                                  " --server omero-server.epfl.ch --user \'",
                                  input$username, "\' --password \'",
                                  my.ome$tmp.fn.password, "\' --keyvalues \'", key.vals.selected,
                                  "\'", suffix, "\n"))
    }
    read.csv(text = system(
      paste0(gsub("omero$", "python", omero.path),
             " external_scripts/get_images_from_key_values.py",
             " --server omero-server.epfl.ch --user \'",
             input$username, "\' --password \'",
             my.ome$tmp.fn.password, "\' --keyvalues \'", key.vals.selected,
             "\'", suffix),
      intern = T))
  })
  
  output$foundKV <- renderDataTable(foundKVDF())
  
  # Substitute NA by "" before downloading
  # Use a file name with date
  output$downloadFoundDF <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", Sys.time()), "_search_key_values.csv")
    },
    content = function(file) {
      df <- foundKVDF()
      df[is.na(df)] <- ""
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Get all kv
  # Substitute NA by "" before downloading
  # Use a file name with date
  output$downloadAllKV <- downloadHandler(
    filename = function() {
      if (input$useronly){
        who <- input$username
      } else {
        who <- "all"
      }
      return(paste0(gsub(" ", "_", Sys.time()), "_", who, "_key_values.csv"))
    },
    content = function(file) {
      suffix <- ""
      if (input$useronly){
        suffix <- " --onlyUserProjects"
      }
      my.text <- system(
        paste0(gsub("omero$", "python", omero.path),
               " external_scripts/get_all_key_values_per_image.py",
               " --server omero-server.epfl.ch --user \'",
               input$username, "\' --password \'",
               my.ome$tmp.fn.password, "\'", suffix),
        intern = T)
      df <- read.csv(text = my.text)
      colnames(df) <-strsplit(my.text[1], ",")[[1]]
      df[is.na(df)] <- ""
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # If the user click on the debug mode
  # A lot of prints to the stderr
  observeEvent(input$debugMode,{
    cat(file = stderr(), "DEBUG MODE\n")
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
