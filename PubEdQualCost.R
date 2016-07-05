#
# This is my project for The JHU Data Scientist Course Developing Data Products
# It uses some data I have been collecting on Public Education Costs and Quality
# It is developed using Shiny in R Studio
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Public Education Costs, Socioeconomic Status and Education Quality"),
   
   p('This is a review of national census data on school district revenues and expenses.  NPR did a recent series on education.  It made me start to wonder.  What does it cost to educate a student?  How much is reasonable?  Education, like healthcare seems to operate on the principle that Quality = Cost.  The more spent, the better the education.  Clearly, as healthcare is learning, ', em('more'), ' is not always', em('better.'),  'Is there an optimum amount that would provide for a solid K-12 education and guarantee that the high school graduate is well prepared for college and for the world?  How do the socioeconomic factors for the schools districts environment affect the degree of educational attainment?  Are we wasting public dollars in some schools districts and not providing enough in others?'),
   
   p('These are not easy questions to answer.  Funding for public education is provided at the Federal, State, and Local level.  It varies from school district to school district. There are differences in the size, makeup, and services offered from district to district and from state to state.  Furthermore, there is no easy metric for measuring quality.  The data is different and is not always easily available.'),
   
   p('School district revenue and expense data were obtained from the ', a(href= 'https://www.census.gov/govs/school/', 'US Census Bureau web site.  '), 'For the purposes of this investigation, data was limited to public school districts providing secondary education or both elementary and secondary education.  Massachusetts school performance data, including graduation rates, mean SAT scores, Persent attending college, and state standardized test scores in English, Math and Science were obtained from the ', a(href='http://profiles.doe.mass.edu',  'Massachusetts Department of Elementary and Secondary Education'), 'web site.'),
   
   hr(),


   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          selectInput("independent", label = h3("Select Independent (Financial) Variable"), 
                 choices = list("Mean Revenue per Student" = 1,
                                "Median Household Income and Benefits" = 2,
                                "Percent of Children Living in Poverty" = 3), 
                          selected = 1),
          selectInput("dependent", label = h3("Select Dependant (Quality) Variable"), 
                      choices = list("Graduation Rate" = 1,
                                     "Mean SAT Scores" = 2,
                                     "% of Students Attending College" = 3,
                                     "% Proficient on MCAS Language Scores" = 4,
                                     "% Proficient on MCAS Math Scores" = 5,
                                     "% Proficient on MCAS Science Scores" =  6 ), 
                      selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         hr()

      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
#First, load the data
    Data <- readRDS('Data/finalMAdata.Rda')
    # select only districts with secondary (either elementary plus secondoary or secondary only
    data <- filter(Data, SCHLEV == '02' | SCHLEV == '03')
    
# Now prepare the plot    
   output$distPlot <- renderPlot({

           #First select independent variable
           if (input$independent == 1){
                   x <- as.numeric(data$REVPER)
                   xmain <- c('$ Revenue per Student (in thousands)')
                   xlabel <- c('Revenue Per Student (in thousands)')
           } else if (input$independent == 2) {
                   x <- as.numeric(data$MedIncBenF)
                   xmain <- c('Median Family Household Income and Benefits ($)')
                   xlabel <- c('Median Family Income & Benefits')
           } else if (input$independent == 3) {
                   x <- as.numeric(data$PctInPovChil)
                   xmain <- c('Percent of Children In Poverty')
                   xlabel <- c('% Children in Poverty')
           } else {
                   x <- as.numeric(data$REVPER)
                   xmain <- c('$ Revenue per Student (in thousands')
                   xlabel <- c('Revenue Per Student (in thousands)')
           }  
           
           #Then select dependent (quality) vsariable
           if (input$dependent == 1) {
                   y <- as.numeric(data$PctGrad)
                   ymain <- c('Graduation Rate')
                   ylabel <- c('Percent of Students Graduating')
           } else if (input$dependent == 2) {
                   y <- as.numeric(data$SATTotal)
                   ymain <- c('Mean SAT Scores')
                   ylabel <- c('Mean SAT Scores (total score)')
           } else if (input$dependent == 3) {
                   y <- as.numeric(data$PctColl)
                   ymain <- c('Percent Attending College')
                   ylabel <- c('% Attending College')
           } else if (input$dependent == 4) {
                   y <- as.numeric(data$ELAPctAPlus)
                   ymain <- c('Proficiency in MCAS Language Test')
                   ylabel <- c('% Proficient in MCAS Language')
           } else if (input$dependent == 5) {
                   y <- as.numeric(data$MTHPctAPlus)
                   ymain <- c('Proficiency in MCAS Math')
                   ylabel <- c('% Proficient in MCAS Math')
           } else if (input$dependent == 6) {
                   y <- as.numeric(data$SCIPctAPlus)
                   ymain <- c('Proficiency in MCAS Science')
                   ylabel <- c('% Proficient in MCAS Science')
            } else {
                   y <- as.numeric(data$PctGrad)
                   ymain <- c('Graduation Rate')
                   ylabel <- c(' Percent of Students Graduating')
           }
           
           g1 <- ggplot ( data,  aes(x,y ))
           g1 <- g1 + ylab(ylabel) + xlab(xlabel)
           g1 <- g1 + ggtitle(paste(ymain, ' vs.', xmain, sep=''))
           g1 <- g1 + geom_point(na.rm=TRUE, shape=1, color='blue', aes(size=ENROLL))
           g1
   })
   hr()
})


# Run the application 
shinyApp(ui = ui, server = server)

