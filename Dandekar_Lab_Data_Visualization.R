#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# pacman::p_load(shiny, ggplot2, dplyr, tidyr, cowplot, shinythemes, shinydashboard, tidytext, data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(tidytext)
library(data.table)
library(DT)

options(shiny.maxRequestSize=500*1024^2)

## Create user interface variable ----
ui = dashboardPage(
  
  dashboardHeader(title = "Dandekar Lab App"),
  
  ##* Sidebar content----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplot/Barplot", tabName = "P", icon = icon('bar-chart-o'))
    )
  ),
  
  ##* Body content----
  dashboardBody(
    tabItems(
      
      ## Boxplot tab----
      tabItem(tabName = "P",
              # Application title
              titlePanel("Boxplot/Barplot"),
              
              # Sidebar with select inputs for plotting. Most of these have a reactive component on the server side that updates based on user input.
              
              fluidRow(
                
                box(
                  selectInput('Data', 'Select datset to visualize', 
                              choices = list.dirs(path = 'Projects/',
                                                  full.names = F),
                              selected = 'SCRI_Root'),
                  
                  selectInput('PlotType', 'Select plot type:',
                              c('Boxplot', 'Barplot')),
                  
                  textOutput('x'),
                  
                  width = 3),
                
                box(
                  selectInput('feat', 'What do you want to plot?', 
                              choices = NULL), 
                  
                  selectizeInput('filter', 'Which variable (gene, protein, metabolite, counts, etc.)',
                                 choices = NULL, multiple = T),
                  
                  width = 3),
                
                box(
                  title = 'About this data:',
                  verbatimTextOutput('methods')
                )
                
              ),
              
              fluidRow(
                
                box(
                  selectInput('x', 'Select plot x-axis:', choices = NULL),
                  
                  selectizeInput('xsub', 'Subset x-axis (optional):', choices = NULL),
                  
                  width = 3),
                
                box(
                  selectInput('y', 'Select plot y-axis:', choices = NULL),
                  
                  selectizeInput('ysub', 'Subset y-axis (optional):', choices = NULL),
                  
                  width = 3),
                
                box(
                  selectInput('facet', 'Facet by:', choices = NULL),
                  
                  selectizeInput('fill', 'Fill by:', choices = NULL),
                  width = 3),
                
                box(
                  selectInput('datsub', 'Subset Dataset (optional):', choices = NULL),
                  
                  selectizeInput('datsubfeat', 'Which feature(s)?', choices = NULL, multiple = T),
                  
                  width = 3)
                
              ),
              
              fluidRow(
                
                downloadButton("downloadPlot", "Download This Plot"),
                
                downloadButton("downloadPlotData", "Download Plotting Data"),
                
                box(
                  numericInput('FacetRows', 'Change column number of facet', value = 1, width = '100px'),
                  width = 2),
                
                box(
                  numericInput('SaveWidth', 'Change size of saved plot', value = 8, width = '100px'),
                  width = 2)
                
              ),
              
              fluidRow(
                
                box(
                  plotOutput('Bplot', width = 'auto'),
                  width = 10),
                
                  DTOutput('plotData')
                
              )
          )
      ),
    
    )
)




# Define server logic---- 
server <- function(input, output, session) {
  
  
  ## This defines the dataset to be worked on in the server based on user selection
  
  
  ##* Plotting app----
  dataset = reactive({
    
    req(input$Data)
    
    fread(paste0('Projects/',
                 input$Data,
                '/plotting_data.csv')) %>% 
      rename_with(~ gsub('GeneID|COMP ID', 'Feature_ID', .x)) %>% 
      mutate(Feature_ID = as.numeric(Feature_ID))
  })
  
  metadata = reactive({
    
    fread(paste0('Projects/',
                 input$Data,
                 '/metadata.csv')) %>% 
      mutate(Sample = as.character(Sample))
  })
  
  annotation = reactive({
    
    fread(paste0('Projects/',
                 input$Data,
                 '/annotation.csv')) %>% 
      rename_with(~ paste0('Annotation_', .x) %>% 
                    gsub('Annotation_GeneID|Annotation_COMP ID', 'Feature_ID', .)) %>% 
      mutate(Feature_ID = as.numeric(Feature_ID))
      
  })
  
  plot_choices = reactive({
    
    list = append(as.list(metadata()),
                  as.list(annotation()))
    
  })
  
  ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.
  observe({
    updateSelectInput(session, 'feat', 'What do you want to plot?', 
                      choices = names(plot_choices()),
                      selected = 'Feature_ID')
  })
  
  observe({
    updateSelectizeInput(session, 'filter', 'Which feature(s)? If multiple, you should facet by variable selected above.', choices = unique(plot_choices()[[input$feat]]), server = T)
  })
  
  output$methods = renderText({
    
    paste(readLines(paste0('Projects/',
                           input$Data,
                           '/READ_ME.txt')), collapse = '\n')
    
  })
  
  ## Filtering the data based on user inputs
  dataset2 = reactive({
    
    req(c(input$feat, input$filter))
    
    dataset() %>% 
      left_join(annotation()) %>%
      filter(.data[[input$feat]] %in% input$filter) %>% 
      pivot_longer(!matches('Feature_ID|Annotation_'),
                   names_to = 'Sample',
                   values_to = 'Expression') %>% 
      left_join(metadata())
      
    
  })
  
  output$plotData = DT::renderDT({
    
    datatable(dataset2(), selection = list(target = 'column'))
  })
  
  
  ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.    
  
  observe({
    updateSelectizeInput(session, 'x',
                         'Select plot x-axis:',
                         choices = names(plot_choices()), 
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session,
                         'xsub',
                         'Subset x-axis (optional):',
                         choices = c('None',
                                     unique(plot_choices()[[input$x]])), 
                         selected = 'None',
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session,
                         'y', 
                         'Select plot y-axis:', 
                         choices = c('Expression',
                                     names(plot_choices())),
                         selected = 'Expression',
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session, 
                         'ysub',
                         'Subset y-axis (optional):', 
                         choices = c('None', 
                                     unique(plot_choices()[[input$y]])),
                         selected = 'None', 
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session, 
                         'facet', 
                         'Facet by:', 
                         choices = c('None', 
                                     names(plot_choices())), 
                         selected = 'None', 
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session, 
                         'fill', 
                         'Fill by:',
                         choices = c(names(plot_choices())),
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session, 
                         'datsub', 
                         'Subset Dataset (optional):', 
                         choices = c('None',
                                     names(plot_choices())),
                         selected = 'None',
                         server = T)
  })
  
  observe({
    updateSelectizeInput(session, 
                         'datsubfeat',
                         'Which feature(s)?',
                         choices = c('None',
                                     unique(plot_choices()[[input$datsub]])), 
                         selected = 'None', 
                         server = T)
    
  })
  
  ## Filtering the data based on user inputs
  dataset3 = reactive({
    
    req(c(input$datsub, input$datsubfeat))
    
    dataset2() %>% 
      filter(.data[[input$datsub]] %in% input$datsubfeat)
    
  })
  
  
  ## Plot variable in server to be displayed in ui based on inputs----
  plotInput = reactive({
    
    if(input$datsub != 'None') {
      
      dat = dataset3()
      
    } else {
      
      dat = dataset2()
    }
    
    ## Conditional statement for input boxplot
    if(input$PlotType == 'Boxplot') {
      if(input$facet == 'None') {
        p = ggplot(dat, aes(reorder(.data[[input$x]], .data[[input$y]]),
                            .data[[input$y]], fill = .data[[input$fill]]))+
          geom_boxplot()+
          scale_x_reordered()+
          theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
          guides(fill = guide_legend(title = input$fill))+
          ggtitle(input$filter)+
          labs(x = input$x, y = input$y)
        
      } else {
        
        p = ggplot(dat, aes(reorder_within(.data[[input$x]], .data[[input$y]], .data[[input$facet]]),
                            .data[[input$y]], fill = .data[[input$fill]]))+
          geom_boxplot()+
          scale_x_reordered()+
          theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
          guides(fill = guide_legend(title = input$fill))+
          labs(x = input$x, y = input$y)+
          facet_wrap(~.data[[input$facet]], scales = "free", ncol = input$FacetRows)
      }
      
    } else {
      ## conditional input for input barplot
      if(input$PlotType == 'Barplot') {
        if(input$facet == 'None') {
          p = ggplot(dat, aes(reorder(.data[[input$x]], .data[[input$y]]),
                              .data[[input$y]], fill = .data[[input$fill]]))+
            geom_bar(stat = 'identity')+
            scale_x_reordered()+
            theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
            guides(fill = guide_legend(title = input$fill))+
            labs(x = input$x, y = input$y)
          
        } else {
          
          p = ggplot(dat, aes(reorder_within(.data[[input$x]], .data[[input$y]], .data[[input$facet]]),
                              .data[[input$y]], fill = .data[[input$fill]]))+
            geom_bar(stat = 'identity')+
            scale_x_reordered()+
            theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
            guides(fill = guide_legend(title = input$fill))+
            labs(x = input$x, y = input$y)+
            facet_wrap(~.data[[input$facet]], scales = "free_x", ncol = input$FacetRows)
        }
        
      }
      
    }
    
    
  })
  ## This is required to set the plot to a variable of 'output'
  output$Bplot <- renderPlot({
    print(plotInput())
  })
  ## This creates a button to downnload the current plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(gsub('[[:punct:]]', '_', input$filter), '.png', sep='_') },
    content = function(file) {
      save_plot(file, plotInput(), base_height = input$SaveWidth)
    }
  )
  
  output$downloadPlotData <- downloadHandler(
    filename = function() { paste(input$Data, '_joined_with_metadata_or_annotation.csv', sep='') },
    content = function(file) {
      fwrite(datasetMod$x, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)