#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(grid)
library(RColorBrewer)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

data3<-read.csv("raw data/emission_annual.csv", stringsAsFactors = F)
data2<-read.csv("raw data/annual_generation_state.csv", stringsAsFactors = F)

data2<-data2[,1:5]
data2$GENERATION..Megawatthours.<-as.numeric(gsub(",","", data2$GENERATION..Megawatthours.))
data3$CO2..Metric.Tons.<-as.numeric(gsub(",","", data3$CO2..Metric.Tons.))
data3.1<-subset(data3, Energy.Source != "All Sources")
data3.1<-subset(data3.1, Producer.Type == "Total Electric Power Industry")
data3.1$Energy.Source<-ifelse(data3.1$Energy.Source == "Wood and Wood Derived Fuels", "Wood", data3.1$Energy.Source)
data3.1$Energy.Source<-ifelse(data3.1$Energy.Source == "Hydroelectric Conventional", "Hydroelectric", data3.1$Energy.Source)
data3.1$Energy.Source<-ifelse(data3.1$Energy.Source == "Solar Thermal and Photovoltaic", "Solar", data3.1$Energy.Source)
#data3.1$Energy.Source<-as.factor(data3.1$Energy.Source)

States<-unique(data3.1$State)
data2.1<-subset(data2, ENERGY.SOURCE != "Total")
data2.1$ENERGY.SOURCE<-ifelse(data2.1$ENERGY.SOURCE == "Wood and Wood Derived Fuels", "Wood", data2.1$ENERGY.SOURCE)
data2.1$ENERGY.SOURCE<-ifelse(data2.1$ENERGY.SOURCE == "Wood and Wood Derived Fuels", "Wood", data2.1$ENERGY.SOURCE)
data2.1$ENERGY.SOURCE<-ifelse(data2.1$ENERGY.SOURCE == "Hydroelectric Conventional", "Hydroelectric", data2.1$ENERGY.SOURCE)
data2.1$ENERGY.SOURCE<-ifelse(data2.1$ENERGY.SOURCE == "Solar Thermal and Photovoltaic", "Solar", data2.1$ENERGY.SOURCE)
#data2.1$ENERGY.SOURCE<-as.factor(data2.1$ENERGY.SOURCE)

fills = brewer.pal(8, "Set1") 
fills = colorRampPalette(fills)(13)

sourcecol<-union(data3.1$Energy.Source, data2.1$ENERGY.SOURCE)

sourcecol.col <- fills

names(sourcecol.col)<-sourcecol
colScale <- scale_fill_manual(name = "Legend",values = sourcecol.col)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("State Power Supply Comparison"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("State1",
                    label = "Choose State 1",
                    choices = States,
                    #choices = c("Week 1","Week 2"),
                    selected = "AK"
        ),
      
        selectInput("State2",
                    label = "Choose State 2",
                    choices = States,
                    #choices = c("Week 1","Week 2"),
                    selected = "NY"
        )
         
         , width = 2),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Pwr"),
         plotOutput("CO2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$CO2 <- renderPlot({
     s1<-ggplot(subset(data3.1, State == input$State1), aes(x = Year, y = CO2..Metric.Tons., fill = Energy.Source))+
       geom_bar(stat = "identity") +
       ylab("CO2: Metric Tons")+
       ggtitle(paste(input$State1, " CO2 Emissions", sep = ""))+
       theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
       colScale
     
     s2<-ggplot(subset(data3.1, State == input$State2), aes(x = Year, y = CO2..Metric.Tons., fill = Energy.Source))+
       geom_bar(stat = "identity")+
       ylab("CO2: Metric Tons")+
       ggtitle(paste(input$State2, " CO2 Emissions", sep = ""))+
       theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
       colScale
     
     multiplot(s1, s2, cols = 2)
     
   })
   
   output$Pwr <- renderPlot({
   s1<-ggplot(subset(data2.1, STATE == input$State1), aes(x = YEAR, y = GENERATION..Megawatthours./100, fill = ENERGY.SOURCE))+
     geom_bar(stat = "identity")+
     ylab("Megawatthours")+
     ggtitle(paste(input$State1, " Energy Sources", sep = ""))+
     theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
     colScale
   
   s2<-ggplot(subset(data2.1, STATE == input$State2), aes(x = YEAR, y = GENERATION..Megawatthours./100, fill = ENERGY.SOURCE))+
       geom_bar(stat = "identity")+
     ylab("Megawatthours")+
     ggtitle(paste(input$State2, " Energy Sources", sep = ""))+
     theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
     colScale
  
   multiplot(s1, s2, cols = 2)
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

