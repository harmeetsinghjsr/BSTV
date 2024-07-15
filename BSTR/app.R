library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application
ui <- fluidPage(
  titlePanel("Binary Search Tree Visualization"),
  sidebarLayout(
    sidebarPanel(
      textInput("number", "Enter any number:", ""),
      actionButton("add", "Add me"),
      actionButton("find", "Find me"),
      actionButton("delete", "Delete me"),
      actionButton("reset", "Reset All")
    ),
    mainPanel(
      plotlyOutput("treePlot"),
      textOutput("status")
    )
  ),
  
  # CSS for custom styling
  tags$style(
    HTML("
      .plot-container {
        width: 100%;
        height: 600px;
        overflow: scroll;
      }
      
      .node-labels {
        font-family: 'Arial', sans-serif;
        font-size: 12px;
        font-weight: bold;
        fill: white;
        text-shadow: 1px 1px 1px black;
      }
      
      .status-text {
        font-family: 'Arial', sans-serif;
        font-size: 14px;
        font-weight: bold;
        color: navy;
        margin-top: 10px;
      }
    ")
  )
)

# Define server logic for BST operations and plotting
server <- function(input, output, session) {
  
  values <- reactiveValues(tree = NULL, status = "")
  
  # Define the BST node class
  Node <- R6::R6Class(
    "Node",
    public = list(
      value = NULL,
      left = NULL,
      right = NULL,
      x = NULL,
      y = NULL,
      initialize = function(value, x, y) {
        self$value <- value
        self$x <- x
        self$y <- y
      }
    )
  )
  
  # Function to insert a node in the BST
  insertNode <- function(root, value, x, y, dx, dy) {
    if (is.null(root)) {
      return(Node$new(value, x, y))
    } else if (value < root$value) {
      root$left <- insertNode(root$left, value, x - dx, y - dy, dx/2, dy)
    } else {
      root$right <- insertNode(root$right, value, x + dx, y - dy, dx/2, dy)
    }
    return(root)
  }
  
  # Function to find a node in the BST
  findNode <- function(root, value) {
    if (is.null(root)) {
      return(FALSE)
    } else if (root$value == value) {
      return(TRUE)
    } else if (value < root$value) {
      return(findNode(root$left, value))
    } else {
      return(findNode(root$right, value))
    }
  }
  
  # Function to delete a node in the BST
  deleteNode <- function(root, value) {
    if (is.null(root)) {
      return(NULL)
    } else if (value < root$value) {
      root$left <- deleteNode(root$left, value)
    } else if (value > root$value) {
      root$right <- deleteNode(root$right, value)
    } else {
      if (is.null(root$left) && is.null(root$right)) {
        return(NULL)
      } else if (is.null(root$left)) {
        return(root$right)
      } else if (is.null(root$right)) {
        return(root$left)
      } else {
        minNode <- root$right
        while (!is.null(minNode$left)) {
          minNode <- minNode$left
        }
        root$value <- minNode$value
        root$right <- deleteNode(root$right, minNode$value)
      }
    }
    return(root)
  }
  
  # Function to create a dataframe for plotting
  createTreeDF <- function(root) {
    if (is.null(root)) {
      return(data.frame(x = numeric(), y = numeric(), value = numeric()))
    } else {
      leftDF <- createTreeDF(root$left)
      rightDF <- createTreeDF(root$right)
      return(rbind(leftDF, rightDF, data.frame(x = root$x, y = root$y, value = root$value)))
    }
  }
  
  # Function to update the plot
  updatePlot <- function(tree) {
    treeDF <- createTreeDF(tree)
    p <- ggplot(treeDF, aes(x = x, y = y, label = value)) +
      geom_point() +
      geom_text(vjust = -0.5, family = "Arial", size = 10, color = "black") +
      theme_void() +
      xlim(range(treeDF$x, na.rm = TRUE) + c(-1, 1)) +
      ylim(range(treeDF$y, na.rm = TRUE) + c(-1, 1))
    return(ggplotly(p))
  }
  
  # Add number to BST
  observeEvent(input$add, {
    value <- as.numeric(input$number)
    if (!is.na(value)) {
      values$tree <- insertNode(values$tree, value, 0, 0, 1, 1)
      values$status <- paste("Added", value)
      output$treePlot <- renderPlotly({
        updatePlot(values$tree)
      })
    }
  })
  
  # Find number in BST
  observeEvent(input$find, {
    value <- as.numeric(input$number)
    if (!is.na(value)) {
      found <- findNode(values$tree, value)
      if (found) {
        values$status <- paste(value, "is found in the tree")
      } else {
        values$status <- paste(value, "is not found in the tree")
      }
    }
  })
  
  # Delete number from BST
  observeEvent(input$delete, {
    value <- as.numeric(input$number)
    if (!is.na(value)) {
      values$tree <- deleteNode(values$tree, value)
      values$status <- paste("Deleted", value)
      output$treePlot <- renderPlotly({
        updatePlot(values$tree)
      })
    }
  })
  
  # Reset the tree
  observeEvent(input$reset, {
    values$tree <- NULL
    values$status <- "Tree is reset"
    output$treePlot <- renderPlotly({
      updatePlot(NULL)
    })
  })

  # Render the plot initially
  output$treePlot <- renderPlotly({
    updatePlot(values$tree)
  })
  
  # Render the status
  output$status <- renderText({
    values$status
  })
}

# Run the application
shinyApp(ui = ui, server = server)
