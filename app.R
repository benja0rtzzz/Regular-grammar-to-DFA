#Created by Benjamin Ortiz, Juan Avalos, Alberto Mentado

library(shiny)
library(igraph)
library(stringr)

ui <- fluidPage(
    
    titlePanel("Regular Grammar to DFA Converter"),
    textOutput("name"),
    
    sidebarLayout(
        sidebarPanel(
            textAreaInput("userInput",
                        label = "",
                        placeholder = "Write your regular grammar here: ",
                        rows = 20,
                        width = "250px"
                        ),
            textOutput("message"),
            actionButton("do", "Click!"),
            actionButton("reset", "Clear!"),
        ),
    
        mainPanel(
           plotOutput("distPlot"),
           textOutput("warning"),
           imageOutput("myImage")
        )
        
    )
)

# Server logic
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      boxInput <- str_split_1(input$userInput, "\n")
      connections <- c()
      states <- c()
      edges <- c()
      
      getConnections <- function(receivedInput){
        m <- str_extract_all(receivedInput, regex("[A-Z]"))
        return(m)
      }
      
      getEdges <- function(receivedInput){
        m <- str_extract_all(receivedInput, regex("[a-z]|ε"))
        if(length(m) == 2){
          break
        }
        return (m)
      }
      
      getUniqueStates <- function(connectionsVector){
        tempStates = c()
        for(i in 1:length(connectionsVector)){
          if(length(connectionsVector[[i]]) == 2){
            tempStates <- append(tempStates, connectionsVector[[i]][1])
            tempStates <- append(tempStates, connectionsVector[[i]][2])
            
          }else if(length(connectionsVector[[i]] == 1)){
            tempStates <- append(tempStates, connectionsVector[[i]][1])
            
          }else{
            print("No data")
          }
        }
        return(unique(tempStates))
      }
      
      getNodeTypes <- function(connectionsVector){
        tempVector <- c()
        for(i in 1:length(connectionsVector)){
          if(connectionsVector[[i]][1] == "S"){
            tempVector <- append(tempVector, 1)
          }
          else if(connectionsVector[[i]][1] == "Z"){
            tempVector <- append(tempVector, 3)
          }else{
            tempVector <- append(tempVector, 2)
          }
          
        }
        return(tempVector)
      }
      
      checkOnes <- function(lengthConnections, connectionsVector){
        for(i in 1:lengthConnections){
          m <- str_detect(boxInput[i], regex(" *[A-Z] *-> *[a-z] *[A-Z] *| *[A-Z] *-> *[a-z] *"))
          if(length(connectionsVector[[i]]) == 1 & m == TRUE){
            connectionsVector[[i]] <- append(connectionsVector[[i]], "Z")
          }
        }
        return(connectionsVector)
      }
      
      validateString <- function(boxInputVector, lengthBoxInput){
        for(i in 1:lengthBoxInput){
          m <- str_detect(boxInputVector[i], regex(" *[A-Z] *-> *[a-z] *[A-Z] *| *[A-Z] *-> *[a-z] *"))
          if(m){
            next
          }else{
            return(FALSE)
            break
          }
        }
        return(TRUE) 
      }
      
      
      tryCatch(
        expr = {
          if(validateString(boxInput, length(boxInput))){
            for(i in 1:length(boxInput)){
              connections <- append(connections, getConnections(boxInput[i]))
              edges <- append(edges, getEdges(boxInput[i]))
            }
            
            connections <- checkOnes(length(connections), connections)
            
            states <- getUniqueStates(connections)
            nodeTypes <- getNodeTypes(states)
            g2 <- graph(unlist(connections), directed = T)
            curves <- curve_multiple(g2)
            mapping_colors <- c("green","gray","red")
            node_colors <- mapping_colors[nodeTypes]
            
            plot(g2, 
                 edge.curved=curves, 
                 edge.label = edges, 
                 vertex.color = node_colors,
                 edge.arrow.size = 1.2,
                 edge.label.cex = 2,
                 vertex.size = 40,
                 vertex.label.cex = 2
            )
            output$warning <- renderText({
              paste("")
            })
            
            
            
            if(boxInput[1] == "C->aT" ){
              output$myImage <- renderImage({
                filename <- normalizePath(file.path("./assets/", paste(boxInput[1], ' .png', sep='')))
                list(src = filename,
                     width = 400,
                     height = 500,
                     alt = paste(""))
                
              }, deleteFile = FALSE)
              
            }else{
              
              output$myImage <- renderImage({
                filename <- normalizePath(file.path("./assets/", paste(' .png', sep='')))
                list(src = filename,
                     width = 400,
                     height = 500,
                     alt = paste(""))
                
              }, deleteFile = FALSE)
            }
            
  
          }else{
            throw()
          }
          
          
        },
        error = function(e){ 
          g2 <- graph(c( c("I","n"), c("n","v"), c("v","a"), c("a","l"), c("l","i"), c("i","d"), c("d","!")))
          plot(g2, 
               vertex.color = "red", 
               vertex.label.color = "white",
               layout = layout_in_circle(g2), 
               edge.arrow.size = 1.2,
               vertex.size = 40,
               vertex.label.cex = 2
          )
          output$warning <- renderText({
            paste("Check your syntax!")
          })
          
        },
        warning = function(w){
          g2 <- graph(c( c("I","n"), c("n","v"), c("v","a"), c("a","l"), c("l","i"), c("i","d"), c("d","!")))
          plot(g2, 
               vertex.color = "red", 
               vertex.label.color = "white",
               layout = layout_in_circle(g2), 
               edge.arrow.size = 1.2,
               vertex.size = 40,
               vertex.label.cex = 2
          )
          output$warning <- renderText({
            paste("Check your syntax!")
          })
          
        }
      )

    })
    
    output$name <- renderText({
      paste("Created by Benjamin Ortiz, Juan Avalos, Alberto Mentado")
    })
    
    observeEvent(input$do, {
      output$name <- renderText({
        paste("Try typing C->aT ;)")
      })
      output$message <- renderText({
        paste("Check below the title!")
      })
    })
    
    observeEvent(input$reset, {
      output$name <- renderText({
        paste("Created by Benjamin Ortiz, Juan Avalos, Alberto Mentado")

      })
      output$myImage <- renderImage({
        filename <- normalizePath(file.path("./assets/", paste(' .png', sep='')))
        list(src = filename,
             width = 400,
             height = 500,
             alt = paste(""))
        
      }, deleteFile = FALSE)
      output$message <- renderText({
        paste("")
      })
      
      updateTextAreaInput(inputId = "userInput", value = "")
      
    })  
    

}

# Run the application 
shinyApp(ui = ui, server = server)
