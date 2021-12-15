library(tidymodels)
library(tidyverse)
library(lubridate)
library(numbers)
library(patchwork)
library(repr)

set.seed(123)

#Taking the model output from my
#final project and organizing it to be
#ggplot friendly


#Main goals of the shiny app:

#Making it possible for the user to filter thru the dataset:
#by company, by sector, and by model (or some combination)

#Allowing sorting of this dataset

#Presenting a form of this filtered data as a barplot

compare_models <- read_rds(file = "data/compare_models.rds")
company_names <- read_csv(file = "data/constituents.csv")

#used rmse for model comparison
#mean absolute error to assess model

#creating the dataset with each company's data

shiny_individual <- compare_models %>%
  rename("avg_dif" = X0, "ticker" = X.Company.name., "model" = X.Model.)

shiny_individual <- inner_join(shiny_individual, company_names, 
                               c("ticker" = "Symbol")) %>%
  mutate(Sector = factor(Sector), model = factor(model))

ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel("Intro",
  
  # App title ----
  
    titlePanel("Can We Predict Stock Prices With Basic Models?"),
    h4("by Edwin Chalas Cuevas"),
    h5("A project for STAT-302, Data Visualization, Spring 2021"),
    
    # Sidebar layout with input and output definitions ----
    
    p("
    One of the most tantalizing challenges in modeling is trying to model
    stock prices - the more accurately you can predict a normally
    unpredictable marketplace, the more great trades you can make that
    could lead to financial success. Banks, brokerages, and other players
    in the stock market have spent years (especially now in the age of
    robo-investing) trying to accurately predict the markets. 
    "),
    p("
I've already generated some data for this - albeit, on a smaller and more narrow scale.
Focusing on the stocks that were in the S&P 500 from 2013 to 2017,
and  on getting a sense of what type of model works best for each sector and company,
I created a dataset of values that you can look through using the tabs above.
    "),
p("
I created three models for each company in the index,
focusing them all on predicting the percent change in a stock's price
at two-week intervals. The output variable in the dataset is RMSE, or root mean squared error.
RMSE is a way of gauging model accuracy - the lower the value, the more accurate the model is.
    "),
p("
By looking at this data, we can begin to answer some key questions in creating a highly
accurate stock model - which model should you use? Does it matter based on the company? Based on the company's sector?
    ")


  
),

tabPanel("By Sector & Model",
         
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             
             p("On this tab, you can compare the RMSE of the dataset, looking by 
        sector and model type."),
        p(),
        p("This graph is at a fixed 700*800 resolution - you might have to adjust your window."),
        p(),
        p("When looking across models (All), the bars are sorted from highest to lowest RMSE."),
        p(),
        strong("The smaller the value, the more accurate the model!"),
        p(),
        
        selectInput(
          "sector_input",
          label = "Pick a sector:",
          choices = list("Communication Services", "Consumer Discretionary",
                         "Consumer Staples", "Energy", "Financials",
                         "Health Care", "Industrials", "Information Technology",
                         "Materials", "Real Estate", "Utilities"),
          selected = "Energy"
        ),
        
        selectInput(
          "model_input",
          label = "Pick a model:",
          choices = list("Boosted tree", 
                         "Nearest neighbors model", "Linear regression", "All"),
          selected = "Linear Regression"
        )
        
           ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Histogram ----
          plotOutput(outputId = "distPlot")
          
        )
         )
         
         ),

tabPanel("By Company",
         
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             
             p("On this tab, you can compare the RMSE across models for an individual company. I've selected some well-known ones."),
             p(),
             strong("The smaller the value, the more accurate the model!"),
             p(),
        
        selectInput(
          "comp_input",
          label = "Pick a company:",
          choices = list("Apple" = "AAPL",
                         "Amazon" = "AMZN",
                         "AT&T" = "T",
                         "Chipotle" = "CMG", 
                         "Coca-Cola" = "KO",
                         "Comcast" = "CMCSA",
                         "Costco" = "COST",
                         "Disney" = "DIS",
                         "Dollar Tree" = "DLTR",
                         "Dollar General" = "DG",
                         "EA" = "EA",
                         "Facebook" = "FB",
                         "Ford" = "F",
                         "General Motors" = "GM",
                         "Google" = "GOOG",
                         "Hershey" = "HSY",
                         "Kellogg's" = "K",
                         "Kraft Heinz" = "KHC",
                         "McDonald's" = "MCD", 
                         "Microsoft" = "MSFT",
                         "Target" = "TGT", 
                         "TJMaxx" = "TJX",
                         "PepsiCo" = "PEP",
                         "PayPal" = "PYPL",
                         "Ralph Lauren" = "RL",
                         "Ross Dress For Less" = "ROST",
                         "Netflix" = "NFLX",
                         "Nike" = "NKE",
                         "Starbucks" = "SBUX",
                         "Ulta" = "ULTA",
                         "Under Armour" = "UA",
                         "Verizon" = "VZ",
                         "Visa" = "V",
                         "Walgreens" = "WBA",
                         "Walmart" = "WMT" ),
          selected = "Apple"
        )

        ),
        
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Histogram ----
          plotOutput(outputId = "secondPlot")
          
        )
         )
        
),


tabPanel("Summary",
         
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             h4("Data Citations"),
             p("* S&P 500 stock data by Cam Nugent, uploaded on kaggle.com ", 
               span(a("(linky)", href = "https://www.kaggle.com/camnugent/sandp500"))),
             p("* S&P 500 Companies with Financial Information by by Datahub.io ", 
               span(a("(linky)", href = "https://datahub.io/core/s-and-p-500-companies")))
           ),
           
           
           # Main panel for displaying outputs ----
           mainPanel(
             
             # Output: Histogram ----
             titlePanel("What can we learn from this?"),
             
             p("
    We can glean a core idea from both visualizations: 
    there is no one-size fits all model for stock price predictions. 
    "),
    p("
      Looking at the data by sector, for example, we can see that there
      is lots of variation even with very similar companies, or even
      classes of stock. For this reason, any broad generalization
      about stock accuracy will likely be incorrect - on a company level the best model differs.
      We can also see the totallity of RMSE values in the 'All' setting - 
      and get a sense of which companies are comparably easier to predict. 
      For example, in Consumer Discretionary, Advance Auto Parts seems to be
      one of the easier companies to predict versus many of the others,
      given their low RMSE values relative to the others in their sector.
      "
    ),
    p("
      Looking at the data by company, comparing it across models,
      we can see the same information from a different angle. Different
      companies have different stock price trends - some are more of a linear regression,
      while others fit best with a nearest neighbors (Target)  or boosted tree.
      This gives us an insight into how that company's growth can be modeled - 
      is it a company with exponential growth, or one that's slow and steady?
      Knowing this insight is essential for creating a highly accurate model for a company's stock price.
      ")
    )
         )
         
)

))

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #making the reactive dataset
  
  output$distPlot <- renderPlot({
  
  finalInput <- shiny_individual %>%
    filter(Sector == input$sector_input) %>%
    filter(if (input$model_input != "All")
      model == input$model_input else !is.na(model))
    
    p1 <- ggplot(finalInput, aes(x = Name, y = avg_dif, fill = model)) +
      geom_bar(stat = "Identity", aes(order = avg_dif)) +
      labs(x = "Company", y = "RMSE", fill = "Model", title = "The best model for stock price predictions", subtitle = input$sector_input) +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Dark2") +
      theme(axis.text.x=element_blank (), axis.ticks.x=element_blank (),
            axis.text.y = element_text(size = 11)) +
      coord_flip()
    
    p1
      
  }, height = 700, width = 800)
  

  
  output$secondPlot <- renderPlot({
    
    height = 30
    
    finalInput <- shiny_individual %>%
      filter(ticker == input$comp_input)
    
    p2 <- ggplot(finalInput, aes(x = model, y = avg_dif, fill = model)) +
      geom_bar(stat = "Identity") +
      labs(x = "Model Type", y = "RMSE", fill = "Model", title = "The best model for stock price predictions", subtitle = input$comp_input) +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Dark2") +
      theme(axis.text.x=element_blank (), axis.ticks.x=element_blank ()) +
      coord_flip()
    
    p2
    
  })
  
  
}


shinyApp(ui = ui, server = server)