library(shiny)
library(shinyFiles)
library(readr)

options(shiny.maxRequestSize = 40000*1024^2)  # Set maximum upload size to 20 GB

ui <- fluidPage(
  titlePanel("Video Cutter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("videoFile", "Select Video File"),
      fileInput("csvFile", "Upload CSV", accept = c(".csv")),
      shinyDirButton("outputFolder", "Select Output Folder", "Please select a folder"),
      textOutput("selectedFolder"),  # Text output to display selected folder
      numericInput("fps", "Frames Per Second (FPS)", value = 30, min = 1),
      textInput("prefix", "Prefix for Output Filenames", value = ""),
      actionButton("cutVideo", "Cut Video")
    ),
    mainPanel(
      verbatimTextOutput("log")
    )
  )
)

server <- function(input, output, session) {
  # Adjust roots to include the entire file system
  roots <- c(home = normalizePath("~"), root = "/")
  shinyDirChoose(input, "outputFolder", roots = roots, filetypes = c(''))
  
  observe({
    req(input$outputFolder)
    # Get the output folder path
    output_folder <- parseDirPath(roots = roots, input$outputFolder)
    # Display the selected folder path
    output$selectedFolder <- renderText({
      paste("Selected output folder:", output_folder)
    })
  })
  
  observeEvent(input$cutVideo, {
    req(input$videoFile)
    req(input$outputFolder)
    req(input$csvFile)
    
    # Read CSV file
    csv_data <- tryCatch({
      read_csv2(input$csvFile$datapath, col_types = cols(
        start_frame = col_double(),
        end_frame = col_double(),
        filename = col_character()
      ))
    }, error = function(e) {
      output$log <- renderPrint({ paste("Error reading CSV file:", e$message) })
      return(NULL)
    })
    
    # Check if CSV data is valid
    if (is.null(csv_data)) {
      return(NULL)
    }
    
    # Print the structure of the CSV data for debugging
    output$log <- renderPrint({ str(csv_data) })
    
    # Get the selected video file
    video_file <- normalizePath(input$videoFile$datapath, winslash = "\\")
    
    # Get the output folder
    output_folder <- normalizePath(parseDirPath(roots = roots, input$outputFolder), winslash = "\\")
    
    # Ensure output folder is a valid path
    if (is.null(output_folder) || output_folder == "") {
      output$log <- renderPrint({"Please select a valid output folder."})
      return(NULL)
    }
    
    # Get the FPS and prefix input
    fps <- input$fps
    prefix <- input$prefix
    
    # Process each row in the CSV with progress
    log_messages <- c()
    withProgress(message = 'Cutting videos', value = 0, {
      for (i in 1:nrow(csv_data)) {
        start_frame <- csv_data$start_frame[i]
        end_frame <- csv_data$end_frame[i]
        output_filename <- csv_data$filename[i]
        
        # Calculate start time and duration from frames
        start_time <- start_frame / fps
        duration <- (end_frame - start_frame) / fps
        
        # Create the output file path with prefix and .mov extension
        if (prefix != "") {
          output_file <- file.path(output_folder, paste0(prefix, "_", output_filename, ".mov"))
        } else {
          output_file <- file.path(output_folder, paste0(output_filename, ".mov"))
        }
        
        # Add detailed logging
        log_messages <- c(log_messages, paste("Processing:", output_filename))
        log_messages <- c(log_messages, paste("Start frame:", start_frame, "End frame:", end_frame))
        log_messages <- c(log_messages, paste("Start time:", start_time, "Duration:", duration))
        log_messages <- c(log_messages, paste("Output file:", output_file))
        
        # Ensure the output directory exists
        if (!dir.exists(dirname(output_file))) {
          dir.create(dirname(output_file), recursive = TRUE)
        }
        
        # Construct the ffmpeg command
        ffmpeg_command <- sprintf(
          "ffmpeg -i %s -ss %.4f -t %.4f -c copy %s",
          shQuote(video_file), start_time, duration, shQuote(output_file)
        )
        
        # Add the ffmpeg command to the log
        log_messages <- c(log_messages, paste("FFmpeg command:", ffmpeg_command))
        
        # Use system() to call ffmpeg directly
        tryCatch({
          system(ffmpeg_command, intern = TRUE)
          
          if (file.exists(output_file)) {
            log_messages <- c(log_messages, paste("Successfully created:", output_filename))
          } else {
            log_messages <- c(log_messages, paste("File not created:", output_filename))
          }
        }, error = function(e) {
          log_messages <- c(log_messages, paste("Error processing", output_filename, ":", e$message))
        })
        
        # Update progress bar
        incProgress(1 / nrow(csv_data), detail = paste("Processing", output_filename))
      }
    })
    
    output$log <- renderPrint({
      paste(log_messages, collapse = "\n")
    })
  })
}

shinyApp(ui = ui, server = server)

