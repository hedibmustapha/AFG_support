library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

data <- read.csv("./input/data.csv")
data<-data %>% filter(consent == "yes")
sectors <- c("DEMOGRAPHICS", "MOVEMENT & INTENTIONS", "SHELTER", "HOUSING LAND & PROPERTY", "WASH", "HEALTH", "PROTECTION",
             "FOOD SECURITY & LIVELIHOOD")

ui <- fluidPage(
  titlePanel( "ISETs Round 1 May-June 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput("province", label = "Please Choose a Province:", choices = data[["province"]])
    ),
    mainPanel(
      tabsetPanel(id = "sectors",
                  type = "tabs",
                  tabPanel("DEMOGRAPHICS",br(),br(),
                           h3(span(textOutput("nbsites"),style= hypegrammaR:::reach_style_color_darkgreys()[1])),
                           br(),
                           dataTableOutput("pop_group_size"),
                           br(),
                           plotOutput("location_fs"),
                           br(),
                           plotOutput("nohost"),
                           br(),
                           dataTableOutput("forcibly"),
                           br()),
                  tabPanel("MOVEMENT & INTENTIONS",br(),br(),
                           plotOutput("relocate"),
                           br(),
                           dataTableOutput("relocate_reason"),
                           br(),
                           plotOutput("five_years"),
                           br(),
                           h3(span(textOutput("idp_3mo_hhs"),style= hypegrammaR:::reach_style_color_darkgreys()[1])),
                           br(),
                           dataTableOutput("idp_prov_origin"),
                           br(),
                           h3(span(textOutput("returnee_3mo_hhs"),style= hypegrammaR:::reach_style_color_darkgreys()[1])),
                           br(),
                           dataTableOutput("recentreturn_challenge"),
                           br()),
                  tabPanel("SHELTER", br(),br(),
                           dataTableOutput("shelter_type"),
                           br(),
                           plotOutput("tenure_insecure"),
                           br(),
                           h3(span(textOutput("hh_rooms_space_isolate"),style= hypegrammaR:::reach_style_color_darkgreys()[1])),
                           br(),
                           dataTableOutput("accom"),
                           br()),
                  tabPanel("HOUSING LAND & PROPERTY"),
                  tabPanel("WASH"),
                  tabPanel("HEALTH"),
                  tabPanel("PROTECTION"),
                  tabPanel("FOOD SECURITY & LIVELIHOOD")
                  
      )
      )
    )
  )

server <- function(input, output, session) {
  province_data <- reactive(data %>% filter(province==input$province))
  output$nbsites<-renderText({
    req(input$province)
    paste0(province_data() %>% count(), " site(s) assessed in this district")
    })
  output$pop_group_size<- renderDataTable({
    req(input$province)
    datatable(data.frame(population_type = c("Recent IDP", "Protracted IDP", "Prolonged IDP","Refugee","Returnee","Economic migrant","Nomad","Host Community"),
                                                               household = c(sum(province_data()$idp_hhs,na.rm = T), sum(province_data()$protracted_hhs,na.rm = T),
                                                                             sum(province_data()$prolonged_hhs,na.rm = T), sum(province_data()$refugee_hhs,na.rm = T),
                                                                             sum(province_data()$returnee_hhs,na.rm = T), sum(province_data()$economic_hhs,na.rm = T),
                                                                             sum(province_data()$nomad_hhs,na.rm = T),sum(province_data()$host_hhs,na.rm = T)),
                                                               individual = c(sum(province_data()$idp_indvl,na.rm = T), sum(province_data()$protracted_indvl,na.rm = T),
                                                                              sum(province_data()$prolonged_indvl,na.rm = T), sum(province_data()$refugee_indvl,na.rm = T),
                                                                              sum(province_data()$returnee_indvl,na.rm = T), sum(province_data()$economic_indvl,na.rm = T),
                                                                              sum(province_data()$nomad_indvl,na.rm = T),sum(province_data()$host_indvl,na.rm = T))),
              rownames = F,
              caption = "Total population group size of assessed sites")
    })
  output$location_fs <-renderPlot({
    req(input$province)
    horizental_plot(province_data(),"location_fs","% of assessed sites by reported location:")
  })
  output$nohost <-renderPlot({
    req(input$province)
    horizental_plot(province_data(),"iset_population","% of assessed sites, KIs reported no host community households living within the site boundaries.")
  })
  output$forcibly<-renderDataTable({
    req(input$province)
    datatable(data.frame(forcibly_displaced_groups = c("Recent IDP", "Protracted IDP", "Prolonged IDP","Refugee","Returnee"),
                         Percentage = c(round_pct(province_data(),"migrant_population.idp"), round_pct(province_data(),"migrant_population.protract"),
                                        round_pct(province_data(),"migrant_population.protract"), round_pct(province_data(),"migrant_population.refugee"),
                                        round_pct(province_data(),"migrant_population.returnee"))),
              rownames = F,
              caption = "Percentage of assessed sites reported to be containing forcibly displaced groups")
    })
  output$relocate <-renderPlot({
    req(input$province)
    horizental_plot(province_data(),"relocate_plan","% residents reportedly planning to move elsewhere in the month following data collection")
  })
  output$relocate_reason <- renderDataTable({
    req(input$province)
    top_3_dt(province_data(),"relocate_pushpull","Top 3 reported reasons for planned movement","reasons",survey,choices)
  })
  output$five_years<- renderPlot({
    req(input$province)
    horizental_plot(province_data(),"five_years","% of assessed  sites,  KIs  reported  that  most  households have lived in the site 5 years or longer.")
  })
  output$idp_3mo_hhs<-renderText({
    req(input$province)
    paste0(sum(province_data()$idp_3mo_hhs,na.rm = T)," IDP households were reported to have arrived in the 3 months prior")
    })
  output$idp_prov_origin<-renderDataTable({
    req(input$province)
    top_3_dt(province_data(),"idp_prov_origin","Top 3 reported  provinces from which most came","province",survey,choices)
  })
  output$returnee_3mo_hhs<-renderText({
    req(input$province)
    paste0(sum(province_data()$returnee_3mo_hhs,na.rm = T)," returnee households were reported to have arrived in the 3 months prior")
    })
  output$recentreturn_challenge<-renderDataTable({
    req(input$province)
    top_3_dt(province_data(),"recentreturn_challenge","Top 3 reported challenges faced by returnees","challenges",survey,choices)
  })
  output$shelter_type<-renderDataTable({
    req(input$province)
    top_3_dt(province_data(),"shelter_type","Top 3 shelter types reported to be most common within assessed sites","shelter_type",survey,choices)
  })
  output$tenure_insecure<- renderPlot({
    req(input$province)
    horizental_plot(province_data(),"tenure_insecure","% of  assessed  sites,  KIs  reported  that  most  residents have verbal or no tenure for their shelters")
  })
  output$hh_rooms_space_isolate<-renderText({
    req(input$province)
    paste0("On average, there are ",round(mean(province_data()$hh_rooms_space_isolate,na.rm = T))," room/s  reported  in  most  shelters  in  which  household members sleep")
  })
  output$accom<-renderDataTable({
    req(input$province)
    top_3_dt(province_data(),"accom","Top 3 reported accommodation arrangements that most residents held in order to occupy their shelter","Accommodation",survey,choices)
  })
}

shinyApp(ui, server)
