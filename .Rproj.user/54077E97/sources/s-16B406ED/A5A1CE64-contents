library(shiny)
library(tidyr)
library(dplyr)

report_path <- tempfile(fileext = "Rmd")
file.copy("Report.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
    markdown::render(input, output_file=output, params = params, envir = new.env(parent = globalenv()))
    
}

# Define UI for application 
ui <- fluidPage(

   fileInput("file_input", "select the file ocntaining gene list", accept = c("csv", "tsv", "xlsx", "xls")),
   selectInput("gene_column", "select the gene column", choices = NULL),
   actionButton("go", "go!"),
   
   tableOutput("gene_review"),
   tableOutput("data_review"),
   
   downloadButton("report", "Generate report")
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ###########################################################################
    
  # implement get_db_connection() function to return a db connection and
  # and close_connection(cnn) function to close the db connection
  # put all these function in a R file in R folder
  #
  # cnn <- get_db_connection()
  # 
  # session$onSessionEnded(function(){
  #     if (!is.null(cnn)){
  #         
  #         # implement close_connection(cnn) function to close db connection
  #         close_connection(cnn)
  #     }
  # })
  ############################################################################
     
   input_df <-eventReactive(input$file_input, {
       req(input$file_input)
       ext = tools::file_ext(input$file_input$name)
       switch(ext,
              csv =vroom::vroom(input$file_input$datapath, delim = ","),
              tsv =vroom::vroom(input$file_input$datapath, delim = "\t"),
              xlsx = read_file(input$file_input$datapath),
              xls = read_file(input$file_input$datapath)
              )
   })
   
   observeEvent(input$file_input, {
       updateSelectInput(session, "gene_column", choices=find_column_names(input_df()))
   })
   
   gene_list <- reactive({
       req(input$gene_column, input$file_input)
       get_genelist_from_df(input_df(), input$gene_column)
   })
   
   merged_df <- eventReactive(input$go, {
       req(gene_list())
       
       # using input_df for dummy test. You need to implement the output 
       # data frame here
       
       input_df()
       
       ###################################################################
       # define the function get_df() to use the gene list to query the back-end
       # and return the results as data frame
       # put this function inside a R file in R folder
       #
       # df1 <- get_df(gene_list())
       ####################################################################
   })
   
   output$report <- downloadHandler(
       filename = function(){
           fname <- gsub("\\..$", "", input$file_input$name)
           paste0(fname, "_report.xlsx")
       },
       content = function(file){
           id <- showNotification(
           "Rendering report...",
           duration = NULL,
           closeButton = FALSE
           )
           on.exit(removeNotification(id), add = TRUE)
           
           write_excel(merged_df(), file)
       }
   )
   
   output$gene_review <- renderTable({
       data.frame(gene_list_example = head(gene_list()))
   })
   
   output$data_review <- renderTable({
       data_frame(head(merged_df()))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
