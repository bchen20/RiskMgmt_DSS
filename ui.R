
#setwd("D:/Stevens_Institute_Technology/2016Fall/sys660/sys660_demo/sys660")

library(shiny)
library(shinydashboard)


shinyUI(dashboardPage(
  skin = "green",
  dashboardHeader(title = "DSS for Travel Destination"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",
               tabName = "Intro",
               icon = icon("home")),
      menuItem(
        "UtilityFunction",
        icon = icon("th"),
        tabName = "UF",
        
        menuSubItem(
          "Assess Utility Functions",
          tabName = "Assess_UF",
          icon = shiny::icon("pencil-square-o")
        ),
        
        
        menuSubItem(
          "Show Curve",
          tabName = "Curve",
          icon = icon("line-chart")
        ),
        
        menuSubItem(
          "Show Weights",
          tabName = "Weight",
          icon = icon("hourglass-end")
        )
      ),
      menuItem("Show Result",
               tabName = "Result",
               icon = icon("plane")),
      
      menuItem(
        "View Data",
        icon = icon("table"),
        tabName = "datafile",
        
        menuSubItem(
          "Original Consequence Table",
          tabName = "con_table",
          icon = shiny::icon("table")
        ),
        
        
        menuSubItem(
          "Multi-Attributes Untility Table",
          tabName = "utl_table",
          icon = shiny::icon("table")
        )),
      
      menuItem("Reference",
               tabName = "Ref",
               icon = icon("file-text-o"))
    )),
  
  
  dashboardBody(
    
    # Dashboard favicon and title
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$title("DSS for Travel Destination")
    ),
    
    tabItems(
      
      # Introduction tab item
      tabItem(
        tabName = "Intro",
        fluidRow(
          
          # Welcome box
          box(
            title = "",
            status = "primary",
            width = 8,
            shiny::img(
              src = "favicon.png",
              height = 100,
              width = 100
            ),
            h3("Welcome to DSS for Travel Destination!"),
            br(),
            h4(
              "DSS is a ",
              "decision support system, which contains databases, models and the user interface."),
            h4(
              "When you want to take a trip but have no idea where to go, this Desicion 
              Support System can give you some idea and help you make the decision. You only 
              need to answer several questions, after which you will get a list of top 5 
              recommended destinations."),
            #       br(),
            h4(
              "Yet there is a prerequisite for this DSS: it is only applied to people 
              who do not have very specific purpose or tendency on their travel destination."),
            br(),
            h4(
              HTML('&copy'),
              '2016 By Team3(Lanjun, Yikai, & Bohong) of SYS-660',br(),
              'Stevens Institute of Technology',
              br(),
              a(href = 'https://termsfeed.com/terms-conditions/2d06520a98747795203d427f52369fc8', 'Terms and Conditions.')
            )
            )
          )
      ),
      
      tabItem(
        tabName = "Assess_UF",
        fluidRow(
          box(
            title = "Select your preference",
            status = "primary",
            width = 12,
            collapsible = F),
          
          # Question 1 Age
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_age", 
                         label = "What's your age?", 
                         choices = c("<18" = "age1",
                                     "18~25" = "age2",
                                     "26~35" = "age3",
                                     "36~45" = "age4",
                                     "46~55" = "age5",
                                     ">56" = "age6"),
                         selected = "age2")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_continent", 
                         label = "Which continent below is out of your consideration?", 
                         choices = c("Africa" = "cont1",
                                     "Aisa" = "cont2",
                                     "Europe" = "cont3",
                                     "North America" = "cont4",
                                     "South America" = "cont5",
                                     "Oceania" = "cont6",
                                     "None of these" = "cont7"),
                         selected = "cont7")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_city", 
                         label = "What's your preference about city style?", 
                         choices = c("Love metropolis very much" = "ans_city1",
                                     "Prefer metropolis" = "ans_city2",
                                     "Love small cities very much" = "ans_city3",
                                     "Prefer small cities" = "ans_city4"),
                         selected = "ans_city2")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_serurity", 
                         label = "When you walk alone on the street in the night, you heard steps behind you, what do you think...?", 
                         choices = c("Regardless of it" = "ans_serurity1",
                                     "Just someone who is heading back home " = "ans_serurity2",
                                     "Might be a bad guy, but I am not afraid" = "ans_serurity3",
                                     "Might be a bad guy, I need to speed up" = "ans_serurity4",
                                     "Must be a bad guy, I am very scare" = "ans_serurity5"),
                         selected = "ans_serurity1")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_cost", 
                         label = "How much would you think it is reasonable to a daily dinner when travelling?", 
                         choices = c("$5-$15" = "ans_cost1",
                                     "$15-$25" = "ans_cost2",
                                     "$25-$40" = "ans_cost3",
                                     "$40-$60" = "ans_cost4",
                                     ">$60" = "ans_cost5"),
                         selected = "ans_cost1")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_cuisine", 
                         label = "What are you most likely to choose as your dinner when you are traveling?", 
                         choices = c("Hamburger or Pizza" = "ans_cuisine1",
                                     "Fried chicken or Salad" = "ans_cuisine2",
                                     "Pasta" = "ans_cuisine3",
                                     "Japanese cuisine" = "ans_cuisine4",
                                     "French cuisine" = "ans_cuisine5"),
                         selected = "ans_cuisine1")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_entertainment", 
                         label = "Which amusement ride do you like best?", 
                         choices = c("Ferris wheel" = "ans_entertainment1",
                                     "Bumper boats" = "ans_entertainment2",
                                     "Pirate ship" = "ans_entertainment3",
                                     "Roller coaster" = "ans_entertainment4",
                                     "Don't like go to amusement park" = "ans_entertainment5"),
                         selected = "ans_entertainment1")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_culture", 
                         label = "How many books did you read last year?", 
                         choices = c("0" = "ans_culture1",
                                     "1~3" = "ans_culture2",
                                     "4~6" = "ans_culture3",
                                     "7~9" = "ans_culture4",
                                     ">9" = "ans_culture5"),
                         selected = "ans_culture1")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_nature", 
                         label = "Choose your most favorite activity when you're traveling?", 
                         choices = c("Sleeping on the bed" = "ans_nature1",
                                     "Reading book in the cottage" = "ans_nature2",
                                     "Enjoying sunshine on the beach" = "ans_nature3",
                                     "Riding elephant in the forest" = "ans_nature4",
                                     "Looking for special animals and plants" = "ans_nature5"),
                         selected = "ans_nature1")
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            radioButtons(inputId = "q_environment", 
                         label = "Which weather do you like best?", 
                         choices = c("Foggy" = "ans_environment1",
                                     "Snowy" = "ans_environment2",
                                     "Rainy" = "ans_environment3",
                                     "Cloudy" = "ans_environment4",
                                     "Clear" = "ans_environment5"),
                         selected = "ans_environment1")
          ),
          
          #Swing Weights
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_securtiy_weight", 
                        label = "How much do you care about security of the city you're going to travel?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_cost_weight", 
                        label = "How much do you care about total cost when you're traveling?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_cuisine_weight", 
                        label = "How much do you care about food when you're traveling?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_entertainment_weight", 
                        label = "How much do you care about entertainment when you're traveling(including amusement park and night life)?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_culture_weight", 
                        label = "How much do you care about culture and history of the city you're going to travel?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_nature_weight", 
                        label = "How much do you care about nature sources of the city you're going to travel?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          ),
          
          box(
            status = "primary",
            width = 12,
            collapsible = T,
            
            sliderInput(inputId = "q_environment_weight", 
                        label = "How much do you care about weather and environment of the city you're going to travel?(high=10,low=1)", 
                        min=1,max=10,value=5,step=1)
          )
          
        )
        
      ),
      
      tabItem(
        tabName = "Curve",
        fluidRow(
          box(
            title = "Your Utility Curve for each attributes",
            status = "primary",
            width = 12,
            collapsible = F)),
        
        fluidRow(
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func1"),
            plotOutput('curve1')
          ),
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func2"),
            plotOutput('curve2')
          ),
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func3"),
            plotOutput('curve3')
          ),
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func4"),
            plotOutput('curve4')
          ),
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func5"),
            plotOutput('curve5')
          ),
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func6"),
            plotOutput('curve6')
          ),
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = T,
            
            textOutput("func7"),
            plotOutput('curve7')
          )
        )
      ),
      
      tabItem(
        tabName = "Weight",
        fluidRow(
          box(
            title = "Show the result of swing weights",
            status = "primary",
            width = 12,
            collapsible = F)),
        
        fluidRow(
          box(
            title = "",
            status = "primary",
            width = 12,
            collapsible = F,
            
            textOutput("weight1"),
            textOutput("weight2"),
            textOutput("weight3"),
            textOutput("weight4"),
            textOutput("weight5"),
            textOutput("weight6"),
            textOutput("weight7")
          ) 
        )
      ),
      
      
      tabItem(
        tabName = "Result",
        fluidRow(
          box(
            title = "Here we go!",
            status = "primary",
            width = 12,
            collapsible = F)),
        
        fluidRow(
          box(
            title = "Top 5 destinations we recommend are",
            status = "primary",
            width = 12,
            collapsible = T,
            
            actionButton(inputId = "Update",
                         label = "Update..."),
            textOutput("city1"),
            textOutput("city2"),
            textOutput("city3"),
            textOutput("city4"),
            textOutput("city5"),
            # textOutput("city6"),
            # textOutput("city7"),
            # textOutput("city8"),
            # textOutput("city9"),
            # textOutput("city10"),
            plotOutput('map1'),
            plotOutput('rad1'),
            plotOutput('rad2'),
            plotOutput('rad3'),
            plotOutput('rad4'),
            plotOutput('rad5')
          ) 
        )
      ),
      
      tabItem(
        tabName = "Ref",
        
        fluidRow(
          box(
            title = "References",
            status = "primary",
            width = 12,
            collapsible = T,
            h5('tripadvisor.com --- refer the data of restaurants and things to do'),
            br(),
            h5('numbeo.com --- refer the data of cost of living, pollution and security'), 
            br(),
            h5('citymayors.com --- refer the data of population of major cities in the world')
          ) 
        )
      ),
      
      tabItem(tabName = "con_table", 
              style = "overflow-y:scroll;", 
              box(width = 12,  
                  title = "Consequence Table",  
                  DT::dataTableOutput("dtrt")
                  
              ) 
      ),
      
      tabItem(tabName = "utl_table", 
              style = "overflow-y:scroll;", 
              box(width = 12,  
                  title = "Multi-attributes Utility Table",  
                  DT::dataTableOutput("dtut")
                  
              ) 
      )
      
    )
)


))
