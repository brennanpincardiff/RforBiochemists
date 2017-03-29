# server.R
library(RCurl)
library(ggplot2)

x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/exercise_hzRatio2.csv")

# turn the data into a useful data.frame
data <- read.csv(text = x, header = TRUE)

x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/activityLevelCalculator.csv")

# turn the data into a useful data.frame
activityLevelCalc <- read.csv(text = x, header = TRUE)

readActLev <- function(jobAct, exercise){
  actLevelSubset <- activityLevelCalc[(activityLevelCalc$jobAct==jobAct) &
                                        (activityLevelCalc$exercise==exercise), ]
  return(as.character(actLevelSubset[,3]))
}

# activity categories
actCats <- c("Moderately inactive", "Moderately active", "Active")

bmi_no2 <- factor(data$bmi_no, levels = c("18.5-24.9", "25-30", ">30"))
data$bmi_no <- bmi_no2
g <- ggplot(data,
            aes(x = ActivityLevel,
                y = AddedYearsPos,
                fill = bmi_no)) +
  geom_bar(stat="identity", position="dodge")  +
  ylab("Added Years Possible") +
  xlab("Activity level (change relative to Inactive)") +
  scale_x_discrete(limits = actCats) + 
  theme_bw()

# calculate BMI function
calculateBMI <- function(weight, height) {
  return((weight / (height/100 * height/100)))
}


shinyServer(
  function(input, output) {
    
    output$text1 <- renderText({ 
      if ((input$smoking > 5) & (input$smoking < 20)){
        paste("Exercise could help but really you should try giving up smoking")
      }else if(input$smoking >19){
        paste("Cutting down on cigaretttes should probably be the focus!")
      }else {paste("Well done on avoiding too many cigarettes")}
    })

    
    # report calculated bmi
    output$text2 <- renderText({ 
      paste("Your bmi is: ", round(calculateBMI(input$weight, input$height), 1))
    }) 

    # comment on bmi 
   output$text3 <- renderText({ 
     if ((calculateBMI(input$weight, input$height) > 25) & (calculateBMI(input$weight, input$height) < 30)){
        paste("Exercise could help and maybe help control your weight")
     }else if(calculateBMI(input$weight, input$height) >29){
        paste("Weight control could improve your future!")
     }else {paste("Well done on keeping control of your weight")}
   })

    
# calculate activity level    
    output$text4 <- renderText({ 
      paste("Your activity level is ", readActLev(input$jobAct, input$exercise))
    }) 
    
    output$text5 <- renderText({ 
     if (readActLev(input$jobAct, input$exercise) == "Inactive"){
        paste("You're", readActLev(input$jobAct, input$exercise), "and could benefit from exercising")
        
      } else if (readActLev(input$jobAct, input$exercise) == "Moderately inactive"){
        paste("You're", readActLev(input$jobAct, input$exercise), "and could benefit from exercising")
        
      } else if (readActLev(input$jobAct, input$exercise) == "Moderately active"){
        paste("You're ", readActLev(input$jobAct, input$exercise), "and could benefit from exercising")
      
      } else if (readActLev(input$jobAct, input$exercise) == "Active") {
       paste("You're ", readActLev(input$jobAct, input$exercise), "so more exercise won't help") 
      
      } else {paste("Please update activity details")}
    })  
 

    # import a table of activity
    # then select the level from the table...
    
# generating a customised graph       
    output$plot <- renderPlot({
      if (input$gender == "No choice yet"){
        print(g + facet_wrap(~gender) +
                ggtitle("Potential for gained years relative to being inactive"))
      }
      else {
        print(g %+% data[data$gender == input$gender, ] +
                ggtitle(paste(input$gender, "- Potential for gained years relative to being inactive")))
      }
    })
# end of plot part. 
    
    
    # generating a second customised graph       
    output$bmifocusplot <- renderPlot({
      if (calculateBMI(input$weight, input$height) < 25){
        print(g + scale_fill_manual(values = c("red","grey90", "grey90")))
      }
      else if (calculateBMI(input$weight, input$height) > 25 & calculateBMI(input$weight, input$height) <30){
        print(g + scale_fill_manual(values = c("grey90","red", "grey90")))
      }
      else {print(g + scale_fill_manual(values = c("grey90","grey90", "red")))}
    })
    # end of second plot part. 
  }
)


