#importing the packages
library(nycflights13)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(shiny)
library(plotly)
library(scales)
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }


# set the theme
theme_set(theme_test())

#Loading the data

fly<-as.data.frame(flights)
carrier<-unique(fly$carrier)

UI<-dashboardPage(
  dashboardHeader(title = "Flights Dashboard",titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(id="sidebarid",
                menuItem("dash",tabName = "fly",icon = icon("plane")),
                conditionalPanel(condition = "input.sidebarid==fly",
                                 pickerInput("airline","Choose Airline",choices = carrier,selected = "AA",
                                             options = list(`actions-box` = TRUE),multiple = T),
                                 pickerInput("mth","Select Month",choices = 1:12,selected = 2,
                                             options = list(`actions-box` = TRUE),multiple = T)
                                 
                                 )
                
                )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "fly",
              box(width = 12,
                  valueBoxOutput("val1"),
                  valueBoxOutput("val2"),
                  valueBoxOutput("val3")
                  ),
              box(collapsible = T,width = 6,plotlyOutput("bar2")),
              box(collapsible = T,width = 6,plotlyOutput("bar3")),
              box(collapsible = T,width=12,plotlyOutput("bar1"))
              
              
              
              )
    )
  )
)

server<-function(input,output,session){
  
  
  data<-reactive({
    return(fly)
  })
  
  # flights by destinations
  output$bar2<-renderPlotly({
    s<-data()%>%
      filter(carrier==input$airline &
              month==input$mth)%>%
      group_by(dest)%>%
      summarise(Total_Flights=sum(flight))%>%
      arrange(desc(Total_Flights))%>%
      head(n=10)%>%
      ggplot(aes(x=dest,y=Total_Flights))+
      geom_col(fill="red")+
      coord_flip()+
      geom_text(aes(label=ks(Total_Flights)))+
      labs(title = "Top Ten Destinations",x="Destinations",y="Flights")+
      theme(legend.position = "none",
            plot.title = element_text(family="bold",hjust = 0.5,size=17),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            panel.border = element_blank()
            )
      ggplotly(s)%>% config(displayModeBar = F)
    
  })
  
  # Flights by Day
  output$bar1<-renderPlotly({
    s<-data()%>%
      filter(carrier==input$airline &
               month==input$mth)%>%
      group_by(day)%>%
      summarise(Total_Flights=sum(flight))%>%
      ggplot(aes(x=day,y=Total_Flights))+
      geom_col(fill="blue")+
      geom_text(aes(label=ks(Total_Flights)))+
      labs(title = "Flights by Day",x="Day of Month",y="Flights")+
      theme(legend.position = "none",
            plot.title = element_text(family="bold",hjust = 0.5,size=17),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
            )
    ggplotly(s)%>% config(displayModeBar = F)
    
  })
  
  #Flights by Distance
  output$bar3<-renderPlotly({
    s<-data()%>%
      filter(carrier==input$airline &
               month==input$mth)%>%
      group_by(distance)%>%
      summarise(Total_Flights=sum(flight))%>%
      ggplot(aes(x=distance,y=Total_Flights))+
      geom_point(size=3)+
      geom_line(size=1)+
      scale_y_continuous(labels = ks)+
    labs(title = "Flights by Distance",x="Distance",y="Flights")+
    theme(legend.position = "none",
          plot.title = element_text(family="bold",hjust = 0.5,size=17),
          panel.border = element_blank()
          )
    ggplotly(s)%>% config(displayModeBar = F)
    
  })
    
  # Total Flights
  output$val1<-renderValueBox({
    data()%>%
      filter(
        carrier==input$airline &
        month==input$mth
      )%>%
      tally()%>%
      as.integer()%>%
      valueBox(
        width = 4,
        color="blue",
        icon=icon("chart-line"),
        subtitle = "Number of Flights"
      )
    
  })
  
  output$val2<-renderValueBox({
    data()%>%
      filter(
        carrier==input$airline &
          month==input$mth
      )%>%
      
      summarise(average=sum(flight)/sum(day))%>%
      as.integer()%>%
      valueBox(
        width = 4,
        color = "green",
        icon=icon("balance-scale"),
        subtitle = "Mean Flights Per Day"
      )
  })
  
  output$val3<-renderValueBox({
    data()%>%
      filter(
        carrier==input$airline &
          month==input$mth
      )%>%
      filter(!is.na(dep_delay))%>%
      mutate(delayed=if_else(dep_delay>=15,1,0))%>%
      summarise(delayed=sum(delayed,na.rm = T),
                total=n())%>%
      collect()%>%
      mutate(per=(delayed/total)*100)%>%
      select(per)%>%
      round()%>%
      paste0("%")%>%
      valueBox(subtitle = "Flights Delayed",
               color="red",
               width=4,
               icon=icon("percent")
               )
  })
  
}

shinyApp(UI,server)
