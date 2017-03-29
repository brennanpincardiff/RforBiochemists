# ui.R

shinyUI(fluidPage(
 # titlePanel("Can exercise help you?"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Please describe yourself"),
      
      # radio Buttons for Gender given at Birth
      radioButtons("gender", 
                   label = "Gender Given at Birth",
                   choices = c("No choice yet", "Women", "Men"),
                   selected = "No choice yet"),
      
      # slider input for Smoking
      sliderInput("smoking", 
                  label = "Cigerattes per day",
                  min = 0, max = 40, value = 0),
      
      # calculator for Body Mass Index
      numericInput('weight', 'Weight (kg)', 75, min = 10, max = 200, step = 1),
      numericInput('height', 'Height (cm)', 170, min = 25, max = 250, step = 1),
      
      # Select box for Job Activity 
      selectInput("jobAct", 
                  label = "Job Activity Level", 
                  choices = c("No info yet", "Sedentary", "Standing", "Physical",
                              "Heavy Manual"), 
                  selected = "Sedentary"),
      
      
      # Select box for Recreational Activity 
      selectInput("exercise", 
                  label = "Physical activity per day", 
                  choices = c("No info yet", "None", "Under Half hour a day", "Half to one hour a day",
                              "More than an hour a day"), 
                  selected = "None")
      
      
    ),
    
    mainPanel(
      h2(helpText("Which habit should you think about?")),
# smoking:
      h3(textOutput("text1")),
# bmi:
      h3(textOutput("text2")),
      h3(textOutput("text3")),
# physical activity:
      h3(textOutput("text5")),

# show some data:
      br(),
      plotOutput("plot"),
      br(),
      plotOutput("bmifocusplot")
    )
  )
))