# Names: Kieran Clark, Rushil Banjeree, Xavier Schore, Andrew Cohen

# Load in libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(scales)


# Load in and clean data
d <- read.csv('laptopData.csv') %>%
  select(-1) %>%
  filter(
    str_detect(Company, "\\w")
  ) %>%
  mutate(
    Ram = as.numeric(str_remove(Ram, "GB")),
    Memory2 = ifelse(
      str_detect(Memory, "\\+"), str_replace(Memory, ".*\\+(.*)", "\\1"), NA
    ),
    Memory = ifelse(
      str_detect(Memory, "\\+"), str_replace(Memory, "(.*)\\+.*", "\\1"), Memory
    ),
    Memory = str_remove_all(Memory, "^\\s+|\\s+$"),
    Memory2 = str_remove_all(Memory2, "^\\s+|\\s+$"),
    Memory_Type = str_extract(Memory, "\\s(.*)"),
    Memory_Type = str_remove(Memory_Type, "^\\s+|\\s+$"),
    Memory = str_remove(Memory, "\\s(.*)$"),
    Memory = ifelse(
      str_detect(Memory, "(GB)"), as.numeric(str_remove(Memory, "(GB)")), 
      as.numeric(str_remove(Memory, "(TB)")) * 1024
    ),
    Memory2_Type = str_extract(Memory2, "\\s(.*)"),
    Memory2_Type = str_remove(Memory2_Type, "^\\s+|\\s+$"),
    Memory2 = str_remove(Memory2, "\\s(.*)$"),
    Memory2 = ifelse(
      str_detect(Memory2, "(GB)"), as.numeric(str_remove(Memory2, "(GB)")), 
      as.numeric(str_remove(Memory2, "(TB)")) * 1024
    ),
    Inches = as.numeric(Inches),
    Weight = as.numeric(str_remove(Weight, "(kg)$")) * 2.20462,
    Price = round(Price / 83.83, 2), #Prices are in Indian Currency
    Cpu_Type = ifelse(
      str_detect(Cpu, "Intel"), "Intel", ifelse(
        str_detect(Cpu, "AMD"), "AMD", "Other"
      )
    ),
    OpSys = ifelse(
      str_detect(OpSys, regex("mac", ignore_case = TRUE)), "MacOS", ifelse(
        str_detect(OpSys, regex("Windows", ignore_case = TRUE)), "Windows", OpSys
      )
    ),
    GPU_Type = ifelse(
      str_detect(Gpu, "Intel"), "Intel", ifelse(
        str_detect(Gpu, "AMD"), "AMD", ifelse(
          str_detect(Gpu, "Nvidia"), "Nvidia", "Other"
        )
      )
    ),
    Inches_bucket = (
      cut(Inches, 
          breaks = c(0, 13, 15, 18, 20, 25, 30, 40), 
          labels = c("10-13", "13-15", "15-18", "18-20", "20-25", "25-30", "Over 30"), 
          include.lowest = TRUE)
    ),
    Inches_bucket = as.character(Inches_bucket)
  ) %>%
  rename(
    `Ram (GB)` = Ram,
    `Memory (GB)` = Memory,
    `Memory 2 (GB)` = Memory2,
    `Weight (lbs)` = Weight,
    `Price ($)` = Price
  ) %>%
  mutate(Ram_Bucket = case_when(
    `Ram (GB)` == 2 ~ "2 GB",
    `Ram (GB)` == 4 ~ "4 GB",
    `Ram (GB)` == 8 ~ "8 GB",
    `Ram (GB)` == 12 ~ "12 GB",
    `Ram (GB)` == 16 ~ "16 GB",
    `Ram (GB)` == 32 ~ "32 GB",
    `Ram (GB)` == 64 ~ "64 GB",
    `Ram (GB)` > 64 ~ ">64 GB",
    TRUE ~ "Other"
  )) %>%
  mutate(Ram_Bucket = factor(Ram_Bucket, levels = c("2 GB", "4 GB", "8 GB", "12 GB", "16 GB", "32 GB", "64 GB", ">64 GB", "Other"))) %>%
  filter(!str_detect(Cpu_Type, "Other"))

# Build app

# ui

ui = fluidPage(
  
  titlePanel('Laptop Analysis'),
  
  sidebarLayout(
    
    sidebarPanel(
      # Take inputs from user
      conditionalPanel(condition = "input.tabs == 'tab1' || input.tabs == 'tab2' || input.tabs == 'tab3'",
        selectInput('Company', 'Select a Company:', choices= c('All', unique(d$Company))),
        selectInput('Memory', 'Select Memory Capacity:', choices = c('All', unique(d$`Memory (GB)`))),
        selectInput('Inches', 'Select a Size (Inches):', choices = c('All', sort(unique(d$Inches_bucket))
      ))), # closes conditional panel
      
      conditionalPanel(condition = "input.tabs == 'tab4'",
                       selectInput("cpu_filter", "CPU Type:",
                                   choices = c("All", sort(unique(d$Cpu_Type))),
                                   selected = "All"),
                       selectInput("gpu_filter", "GPU Type:",
                                   choices = c("All", sort(unique(d$GPU_Type))),
                                   selected = "All")             
      
      ), # closes conditional panel
      # Inputs for the RAM Comparison tab
      conditionalPanel(
        condition = "input.tabs == 'tab5'",
        selectInput("company_ram", "Select Company for RAM Comparison:", 
                    choices = c("All", unique(d$Company))),
        sliderInput("price_range_ram", "Select Price Range for RAM Comparison (USD):",
                    min = min(d$`Price ($)`, na.rm = TRUE),
                    max = max(d$`Price ($)`, na.rm = TRUE),
                    value = c(min(d$`Price ($)`, na.rm = TRUE), max(d$`Price ($)`, na.rm = TRUE)))
      ) # closes conditional panel
      
    ), # closes sidebarPanel
    
    mainPanel(
      # Display outputs to user
      
      tabsetPanel(id = 'tabs',
        tabPanel('Price by Model', plotOutput('price_by_type'), value='tab1'),
        tabPanel('Price vs. Weight', plotOutput('weight_price'), value='tab2'),
        tabPanel('Price by Operating System', plotOutput('ops_price'), value='tab3'),
        tabPanel('Prices and Processing Unit', plotOutput("boxplot"),
                 plotOutput("barplot"), value='tab4'),
        tabPanel('RAM Comparison', plotOutput('ram_comparison'), value = 'tab5')
        
      ) # closes tabsetPanel
      
    ) #closes mainPanel
    
  ) # closes sidebarLayout
  
) # closes ui


# server

server = function(input, output){
  
  # Plot 1
  output$price_by_type = renderPlot({
    d %>%
      filter(
        Company == input$Company | input$Company == 'All',
        `Memory (GB)` == input$Memory | input$Memory == 'All',
        Inches_bucket == input$Inches | input$Inches == 'All'
      ) %>%
      group_by(TypeName) %>%
      summarise(
        avg_price = mean(`Price ($)`)
      ) %>%
      ggplot(aes(x=avg_price, y=reorder(TypeName, avg_price))) +
      geom_col(fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = "Average Price by Model", 
           x = "Average Price", 
           y = "Model") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
      geom_text(aes(label = round(avg_price, 2)), hjust = -0.1, color = "black") +
      scale_x_continuous(limits = c(0, 1750))
    
  }) # closes graph
  
  # Plot 2
  output$weight_price = renderPlot({
    d %>%
      filter(
        Company == input$Company | input$Company == 'All',
        `Memory (GB)` == input$Memory | input$Memory == 'All',
        Inches_bucket == input$Inches | input$Inches == 'All'
      ) %>%
      ggplot( aes(y = `Weight (lbs)`, x = `Price ($)`, col = OpSys)) +
        geom_point(size = 3, alpha = 0.7) +
        theme_minimal() +
        labs(title = "Price vs. Weight by Operating System", 
              x = "Price ($)", 
              y = "Weight (lbs)", 
              color = "Operating System") +
        scale_color_brewer(palette = "Set1") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
    
  }) # closes graph
  
  # Plot 3 
  output$ops_price = renderPlot({
    d %>%
      filter(
        Company == input$Company | input$Company == 'All',
        `Memory (GB)` == input$Memory | input$Memory == 'All',
        Inches_bucket == input$Inches | input$Inches == 'All'
      ) %>%
      group_by(OpSys) %>%
      summarise(
        avg_price = mean(`Price ($)`)
      ) %>%
      ggplot(aes(x=reorder(OpSys, avg_price), y=avg_price)) +
        geom_col(fill = "lightblue", color = "black") +
        theme_minimal() +
        labs(title = "Average Price by Operating System", 
            x = "Operating System", 
            y = "Average Price ($)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14)) +
        geom_text(aes(label = round(avg_price, 2)), vjust = -0.3, color = "black")
    
  }) # closes graph
  
  # Box plot of laptop prices by laptop type
  output$boxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = TypeName, y = `Price ($)`)) +
      geom_boxplot(fill = "darkgreen", color = "black") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Laptop Type", y = "Price (USD)", 
           title = "Laptop Prices by Type") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_viridis_d()
  }) # closes graph
  
  # Filtered data based on user input
  filtered_data <- reactive({
    data <- d
    if (input$cpu_filter != "All") {
      data <- data %>% filter(Cpu_Type == input$cpu_filter)
    }
    if (input$gpu_filter != "All") {
      data <- data %>% filter(GPU_Type == input$gpu_filter)
    }
    data
  })
  
  # Filtered data for RAM comparison
  filtered_data_ram <- reactive({
    d %>%
      filter(
        Company == input$company_ram | input$company_ram == 'All',
        `Price ($)` >= input$price_range_ram[1],
        `Price ($)` <= input$price_range_ram[2]
      )
  })
  
  # Bar plot of average laptop price by company
  output$barplot <- renderPlot({
    avg_prices <- filtered_data() %>%
      group_by(Company) %>%
      summarize(avg_price = mean(`Price ($)`, na.rm = TRUE)) %>%
      arrange(desc(avg_price))
    
    ggplot(avg_prices, aes(x = reorder(Company, avg_price), y = avg_price)) +
      geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Company", y = "Average Price (USD)", 
           title = "Average Laptop Price by Company") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_viridis_d()
  }) # closes graph
  
  # Plot 6: RAM Comparison
  output$ram_comparison <- renderPlot({
    ggplot(filtered_data_ram(), aes(x = Ram_Bucket)) +
      geom_bar(fill = "darkblue", color = "black") +
      labs(title = "RAM Size Distribution", x = "RAM (GB)", y = "Count") +
      theme_minimal()
  }) # closes graph
  
} # closes server

# shiny App
shinyApp(ui, server)
