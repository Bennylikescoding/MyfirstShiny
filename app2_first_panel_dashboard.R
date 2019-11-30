library(shiny)


#install.packages("ggfortify",dependencies = TRUE)
#install.packages("ggplot2",dependencies = TRUE)

#install.packages("gtable")
#install.packages("colorspace")
library(ggplot2)
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("backports")
library(ggfortify)
library(reshape2)




# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  hr(),
  
  fluidRow(
    column(2,
           fileInput("file1", "Choose CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           
           # Horizontal line ----
           tags$hr(),
           
           # Input: Checkbox if file has header ----
           checkboxInput("header", "Header", TRUE),
           
           # Input: Select separator ----
           radioButtons("sep", "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ","),
           
           # Input: Select quotes ----
           radioButtons("quote", "Quote",
                        choices = c(None = "",
                                    "Double Quote" = '"',
                                    "Single Quote" = "'"),
                        selected = '"'),
           
           # Horizontal line ----
           tags$hr(),
           
           # Input: Select number of rows to display ----
           radioButtons("disp", "Display",
                        choices = c(Head = "head",
                                    All = "all"),
                        selected = "all"),
           
           # Horizontal line-----self made----
           tags$hr(),
           
           # Input: Select number of rows to display ----
           selectInput("groupselect", h4("PCA Group for visualize"),
                       choices = list("UniqueSampleName" = "unique_Sample_name",
                                      "Group_condition1" = "group_condition1",
                                      "Group_condition2" = "group_condition2",
                                      "Group_condition3" = "group_condition3",
                                      "Group_condition4" = "group_condition4",
                                      "Group_condition5" = "group_condition5",
                                      "Group_condition6" = "group_condition6",
                                      "Assemble_group_name" = "assemble_group_name"), selected = "assemble_group_name"),
           
           sliderInput("pointsize", h4("Point size"),
                       min = 0, max = 10, value = 5),
           
           checkboxInput("eigenvector_checkbox", "Show eigenvectors", value = FALSE)
           
    ),
    column(10,

           fluidRow(class = "a",
                    column(4, plotOutput("pca")),
                    column(4, plotOutput("heatmap")),
                    column(4, plotOutput("correlation"))
           ),
           
           
           fluidRow(class = "b",
                    column(4, plotOutput("pca2")),# contents in plotOutput MUST be Different, i.e., pca2 vs. pca, or the content will not show up
                    column(4, plotOutput("heatmap2")),
                    column(4, plotOutput("correlation2"))
           )

           
           
           
           
           # tabsetPanel(
           #   tabPanel("Heatmap", plotOutput("heatmap")),
           #   
           #   tabPanel("PCA", plotOutput("pca")),
           #   
           #   tabPanel("Correlation", plotOutput("correlation")),
           #   
           #   # Output: Data file ----
           #   tabPanel("Table", tableOutput("contents"))
             
           )
    )
)
# Define server logic to read selected file ----
server <- function(input, output) {
  
  observeEvent(input$groupselect,
                 showNotification("Warning! Change group action has no effect on input data selection, it only changes color representation!", duration = NULL, type = "warning")
    )

  
  output$heatmap <- renderPlot({
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    rnames <- df[,input$groupselect]
    df_graph <- as.matrix(df[,9:ncol(df)])
    rownames(df_graph) <- rnames
    
    heatmap(df_graph)
  })
  
  
  
  output$pca <- renderPlot({
    
    df_pca <- read.csv(input$file1$datapath,
                       header = input$header)
    
    autoplot(prcomp(df_pca[,9:ncol(df_pca)]), data = df_pca,
             frame = TRUE, frame.type = 'norm',
             colour = input$groupselect,
             loadings = input$eigenvector_checkbox, loadings.colour = 'blue',
             loadings.label = input$eigenvector_checkbox, loadings.label.size = 3,
             size = input$pointsize)
    
  })
  
  
  
  output$correlation <- renderPlot({
    
    df_cor <- read.csv(input$file1$datapath,
                       header = input$header)
    
    df_cormat <- round(cor(df_cor[,9:ncol(df_cor)]),2)
    melted_df_cormat <- melt(df_cormat)
    
    ggplot(data = melted_df_cormat, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$heatmap2 <- renderPlot({
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    rnames <- df[,1]
    df_graph <- as.matrix(df[,9:ncol(df)])
    rownames(df_graph) <- rnames
    
    heatmap(df_graph)
  })
  
  
  
  output$pca2 <- renderPlot({
    
    df_pca <- read.csv(input$file1$datapath,
                       header = input$header)
    
    autoplot(prcomp(df_pca[,9:ncol(df_pca)]), 
             frame = TRUE, frame.type = 'norm',
             label = TRUE, label.size = 5)
    
    
  })
  
  
  
  output$correlation2 <- renderPlot({
    
    df_cor <- read.csv(input$file1$datapath,
                       header = input$header)
    
    df_cormat <- round(cor(df_cor[,9:ncol(df_cor)]),2)
    melted_df_cormat <- melt(df_cormat)
    
    ggplot(data = melted_df_cormat, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
