
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(sf)
library(arules)
library(arulesViz)
library(ggplot2)
library(reshape2)
library(DT)


ui <- dashboardPage(
  dashboardHeader(title = " SideBar "),
        dashboardSidebar(
          tags$aside(
            class = "sidebar",
            sidebarMenu(
              id = "tabs",
              menuItem("Produits/services par région", tabName = "Tendances", icon = icon("globe")),
              tags$br(), 
              menuItem("Groupes de Consommateurs", tabName = "Groupes_de_Consommateurs", icon = icon("users")),
              tags$br(),  
              menuItem("Les règles d'association", tabName = "Tendance", icon = icon("exchange")),
              tags$br(), 
              menuItem("Visualisations", tabName = "Groupes", icon = icon("bar-chart"))
            )
          )
        ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Tendances",
              fluidRow(
                tabsetPanel(
                  tabPanel("Base de données",
                           box(dataTableOutput("dataTable"), width = 12),            
                        
                  ),
                  tabPanel("Consommation Régionale", 
                           box( plotlyOutput("myhist")),
                           box("Chaque région a ses propres services et produits qu'elle consomme le plus. Choisissez la région dont vous voulez en savoir plus. ",
                               selectInput("region", label = "region", choices = c("Tanger-Tétouan-Al-Hoceïma"=1, "Oriental"=2, "Fès-Meknès"=3, "Rabat-Salé-Kénitra"=4, "Béni Mellal-Khénifra"=5, "Casablanca-Settat"=6, "Marrakech-Safi"=7, "Drâa-Tafilalet"=8, "Souss-Massa"=9, "Guelmim-Oued Noun"=10, "Laâyoune-Sakia El Hamra"=11, "Dakhla-Oued Ed Dahab"=12))),
                           box("Voici les cinq produits les plus consommés dans cette région", tableOutput("myreg")),
                  
                           box("Voici le dictionnaire des produits et services. Choisissez le nom du produit/service
                               pour savoir à quoi il ressemble.", selectInput("produit", label = "Region", choices = c("DAM_G01" = 1, "DAM_G02" = 2, "DAM_G03" = 3, "DAM_G04" = 4, "DAM_G05" = 5,
                                                                                                                                             "DAM_G06" = 6, "DAM_G07" = 7, "DAM_G08" = 8, "DAM_G09" = 9, "DAM_G10" = 10,
                                                                                                                                             "DAM_G11" = 11, "DAM_G12" = 12, "DAM_G13" = 13, "DAM_G14" = 14, "DAM_G15" = 15,
                                                                                                                                             "DAM_G17" = 16, "DAM_G18" = 17, "DAM_G21" = 18, "DAM_G22" = 19, "DAM_G23" = 20,
                                                                                                                                             "DAM_G24" = 21, "DAM_G25" = 22, "DAM_G26" = 23, "DAM_G27" = 24, "DAM_G28" = 25,
                                                                                                                                             "DAM_G31" = 26, "DAM_G32" = 27, "DAM_G33" = 28, "DAM_G34" = 29, "DAM_G41" = 30,
                                                                                                                                             "DAM_G42" = 31, "DAM_G43" = 32, "DAM_G44" = 33, "DAM_G45" = 34, "DAM_G46" = 35,
                                                                                                                                             "DAM_G47" = 36, "DAM_G51" = 37, "DAM_G52" = 38, "DAM_G53" = 39, "DAM_G61" = 40,
                                                                                                                                             "DAM_G62" = 41, "DAM_G63" = 42, "DAM_G71" = 43, "DAM_G72" = 44, "DAM_G73" = 45,
                                                                                                                                             "DAM_G74" = 46, "DAM_G75" = 47, "DAM_G76" = 48, "DAM_G77" = 49, "DAM_G81" = 50,
                                                                                                                                             "DAM_G82" = 51, "DAM_G83" = 52, "DAM_G84" = 53, "DAM_G85" = 54, "DAM_G86" = 55,
                                                                                                                                             "DAM_G87" = 56, "DAM_G88" = 57, "DAM_G89" = 58, "DAM_G91" = 59, "DAM_G92" = 60,
                                                                                                                                             "DAM_G93" = 61, "DAM_G94" = 62, "DAM_G95" = 63)), tableOutput("mydictionary"))),
               
                  tabPanel("Les Secteurs les Plus Consommés",
                           box("Pour voir chaque produit/service et la région où il est consommé, sélectionnez le produit/service qui vous intéresse dans la liste ci-dessous. Vous pourrez ensuite voir les régions où il est consommé et ses dépenses dans ces régions.",br()
                               ,selectInput("proser", label = "choisir un produit ou service", choices = c("DAM_G01", "DAM_G02", "DAM_G03", "DAM_G04", "DAM_G05", "DAM_G06", "DAM_G07", "DAM_G08", "DAM_G09", "DAM_G10",
                                                                                                           "DAM_G11", "DAM_G12", "DAM_G13", "DAM_G14", "DAM_G15", "DAM_G17", "DAM_G18", "DAM_G21", "DAM_G22", "DAM_G23",
                                                                                                           "DAM_G24", "DAM_G25", "DAM_G26", "DAM_G27", "DAM_G28", "DAM_G31", "DAM_G32", "DAM_G33", "DAM_G34", "DAM_G41",
                                                                                                           "DAM_G42", "DAM_G43", "DAM_G44", "DAM_G45", "DAM_G46", "DAM_G47", "DAM_G51", "DAM_G52", "DAM_G53", "DAM_G61",
                                                                                                           "DAM_G62", "DAM_G63", "DAM_G71", "DAM_G72", "DAM_G73", "DAM_G74", "DAM_G75", "DAM_G76", "DAM_G77", "DAM_G81",
                                                                                                           "DAM_G82", "DAM_G83", "DAM_G84", "DAM_G85", "DAM_G86", "DAM_G87", "DAM_G88", "DAM_G89", "DAM_G91", "DAM_G92",
                                                                                                           "DAM_G93", "DAM_G94", "DAM_G95")), tableOutput("dictionayTwo") , width = 12),
                           box(plotlyOutput("map"),width = 12),
                          )),
              )
      ),
   
    
    
    tabItem(tabName = "Groupes_de_Consommateurs",
            fluidRow(
              tabsetPanel(
                tabPanel("Comparaison de dépense", 
                         box(plotlyOutput("famillyhist")),
                         box("Pour comparer votre consommation annuelle avec celle des autres dans les zones urbaines et rurales, veuillez entrer votre consommation mensuelle et Sélectionnez votre région et votre milieu de vie habituel.", br(), selectInput("regionTwo", label = "region", choices = c("Tanger-Tétouan-Al-Hoceïma" = 1, "Oriental" = 3, "Fès-Meknès" = 5, "Rabat-Salé-Kénitra" = 7, 
                                                                                                                                                                                                                                                                                                            "Béni Mellal-Khénifra" = 9, "Casablanca-Settat" = 11, "Marrakech-Safi" = 13, 
                                                                                                                                                                                                                                                                                                            "Drâa-Tafilalet" = 15, "Souss-Massa" = 17, "Guelmim-Oued Noun" = 19, 
                                                                                                                                                                                                                                                                                                            "Laâyoune-Sakia El Hamra" = 21, "Dakhla-Oued Ed Dahab" = 23))),
                         box(selectInput("milieu", label = "melieu", choices = c("urbain" = 0, "rural" = 1)), numericInput("depense", label = "depense par mois", value = 0, min = 10, step = 1), tableOutput("DamF")),
                         box(plotlyOutput("chart_DAM_REGION"), width = 12)),
                        
                
                tabPanel("Classement par Cluster",
                         box("Pour connaître le cluster auquel vous appartenez en fonction du nombre de membres de votre famille, de votre consommation annuelle, de votre région et de votre milieu de vie, veuillez entrer ces informations.",numericInput("numeroF", label = "identifient de famille", value = 0, min = 30, max = 32, step = 1), 
                             selectInput("regionThree", label = "region", choices = c("Tanger-Tétouan-Al-Hoceïma" = 1, "Oriental" = 2, "Fès-Meknès" = 3, "Rabat-Salé-Kénitra" = 4,
                                                                                      "Béni Mellal-Khénifra" = 5, "Casablanca-Settat" = 6, "Marrakech-Safi" = 7,
                                                                                      "Drâa-Tafilalet" = 8, "Souss-Massa" = 9, "Guelmim-Oued Noun" = 10,
                                                                                      "Laâyoune-Sakia El Hamra" = 11, "Dakhla-Oued Ed Dahab" = 12)),
                             selectInput("milieuTwo", label = "milieu", choices = c("Urbain" = 1, "Rural" = 2)),
                             numericInput("TailleF", label = "Taille de famille", value = 1, min = 1, step = 1),
                             numericInput("depenseTwo", label = "depense par mois", value = 1, min = 100, step = 1)),
                         box(title = "Cluster Visualization", plotlyOutput("cluster_plot"))
                ),
                tabPanel("Données des Clusters",
                         box(title = "Cluster Visualization", DTOutput("Clusteringdata"), width = 12)
                )
              )
            )
    ),
    
    tabItem(tabName = "Tendance",
            fluidRow(
              tabsetPanel(
               
                tabPanel("Le graphique de l'algorithme d'Apriori ",
                         box(plotOutput("Apriorigraph"), width = 12, height = 900)
                ),
                tabPanel(" Le graphique de confidence Vs  support",
                         box(plotOutput("scatterPlot"), width = 12, height = 700)
                ),
                tabPanel("tableau d'association",
                         box(DTOutput("rulesOutput"), width = 12)
                ),
              )
            )
    ),
    tabItem(tabName = "Groupes",
            fluidRow(
              tabsetPanel(
                
                tabPanel("taille de ménage par région",
                         box(plotlyOutput("realscatterplot"), width = 12, height = 700)
                ),
                tabPanel("Nombre moyen d'enfants par région",
                         box(plotlyOutput("chartofProd_01"), width = 12 , height = 700),
                )
              )
            )
    )
  )
))

  



 server <- function(input, output) {
    dataset <- read.csv(file = "C:\\Users\\hp\\Desktop\\stage hcp\\mydataFormR.csv")
    dictionary<- read.csv(file = "C:\\Users\\hp\\Desktop\\stage hcp\\dictionary.csv")
    regions <- read.csv(file = "C:\\Users\\hp\\Desktop\\stage hcp\\mean of the products by region.csv")
    DAM <- read.csv(file = "C:\\Users\\hp\\Desktop\\stage hcp\\mean_by_group_DAM.csv")
    nc <-read.csv("C:/Users/hp/Desktop/stage hcp/mean of the products by region.csv")
    Clusdf <- read.csv(file = "C:\\Users\\hp\\Desktop\\stage hcp\\Cluste_normalized2.csv")
          transactions <- read.csv("C:\\Users\\hp\\Desktop\\stage hcp\\market_basket_analysis.csv")
   
  
########## comparaison of the products and services consumption from region to another using user input #####################
 
          output$dataTable <- renderDataTable({
            datatable(dataset, options = list(scrollX = TRUE))
          })
 
          
  
            
            ## had l code dyl les 5 most consumed products 
            output$myreg <- renderTable({reg <- as.numeric(input$region)
            seq<-65:69
            regions[reg , seq]})
  
            
            
           ## mn be3d zidi l design o l ui costumization  
           output$myhist <- renderPlotly({ reg <- as.numeric(input$region)
           seq <- 2:64
           columns_name <- c(
      "DAM_G01", "DAM_G02", "DAM_G03", "DAM_G04", "DAM_G05", "DAM_G06", "DAM_G07", "DAM_G08", "DAM_G09", "DAM_G10",
      "DAM_G11", "DAM_G12", "DAM_G13", "DAM_G14", "DAM_G15", "DAM_G17", "DAM_G18", "DAM_G21", "DAM_G22", "DAM_G23",
      "DAM_G24", "DAM_G25", "DAM_G26", "DAM_G27", "DAM_G28", "DAM_G31", "DAM_G32", "DAM_G33", "DAM_G34", "DAM_G41",
      "DAM_G42", "DAM_G43", "DAM_G44", "DAM_G45", "DAM_G46", "DAM_G47", "DAM_G51", "DAM_G52", "DAM_G53", "DAM_G61",
      "DAM_G62", "DAM_G63", "DAM_G71", "DAM_G72", "DAM_G73", "DAM_G74", "DAM_G75", "DAM_G76", "DAM_G77", "DAM_G81",
      "DAM_G82", "DAM_G83", "DAM_G84", "DAM_G85", "DAM_G86", "DAM_G87", "DAM_G88", "DAM_G89", "DAM_G91", "DAM_G92",
      "DAM_G93", "DAM_G94", "DAM_G95")
           x <- as.numeric(regions[reg, seq])  
           data <- data.frame(columns_name, x)
           p <- plot_ly(data, x = ~columns_name, y = ~x, type = "bar", marker = list(color = "blue")) %>%
           layout(xaxis = list(title = "Les produit et les services"), yaxis = list(title = "annuel consummation"), title = "Les produit et les services dans chaque region")
           p})
           
           
           output$map <- renderPlotly({
             p <- ggplot(regions, aes(x = Région_12, y = !!sym(input$proser), fill = Région_12)) +
               geom_bar(stat = "identity") +
               labs(title = paste("Variation de", input$proser, "d'une région à l'autre")) +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
             
             ggplotly(p, tooltip = c("Région_12", input$proser))
           })
  

  
  
          output$mydictionary<- renderTable({ produit<-as.numeric(input$produit)
          dictionary[produit , 1:2]})
  
  
          ##tab THree dyl rural o urbain
          output$DamF <- renderTable({ region2 <- as.numeric(input$regionTwo)
          milieu <- as.numeric(input$milieu)
          depense <- as.numeric(input$depense)
          row <- region2 + milieu
          depense_annuel <- depense * 12
          data.frame(Depense_Annuelle = DAM[row, 3], votre_Depense_Annuelle = depense_annuel)})
  
  
         output$famillyhist <- renderPlotly({region2 <- as.numeric(input$regionTwo)
         milieu <- as.numeric(input$milieu)
         depense <- as.numeric(input$depense)
         # Create a Plotly histogram
         plot_ly(DAM, x = ~DAM[region2, 3], type = "histogram" ,name = "depense annuel urbain") %>%
         add_trace(x = ~DAM[region2 + 1, 3], type = "histogram", name="depense annuel rural") %>%
         add_trace(x = ~depense * 12, type = "histogram", name = "votre depense annuel") %>%
         layout(xaxis = list(title="depense annuel",tickvals = c(region2, region2 + 1, depense * 12)), title="depense annuel par region")})
  
    
         
         
         
         
         
######## CLUSTERING ALGORITHM I USED K-MEANS AND NEW NORMALIWED DATASET ###################################################         
        
         output$chart_DAM_REGION<-renderPlotly({
           
           ggplot(DAM, aes(x = Région_12, y = mean_value_DAM,color=Milieu, group = Milieu)) +
             geom_line(linetype = "line") +
             geom_point(size = 3) +
             labs(title = "Valeurs moyennes de DAM par région et environnement",
                  x = "région",
                  y = "Valeurs moyennes de la dépense annuelle",
                  color = "Environment",
                  shape = "Environment") +
             theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
             scale_color_manual(values = c("red", "blue")) +
             scale_shape_manual(values = c(16, 17))
           
         })
         
         
       
  
       observe({
       
         numeroF <- input$numeroF
         milieuTwo <- input$milieuTwo
         regionThree <- input$regionThree
         TailleF <- input$TailleF
         depenseTwo <- input$depenseTwo
         depense_annuel <- (depenseTwo * 12) / TailleF
         
       
         additional_input <- c(numeroF, milieuTwo, regionThree, TailleF, depense_annuel)
 
         Clusdf <- rbind(additional_input, Clusdf)
         k <- 4
         set.seed(123)
         result <- kmeans(Clusdf, centers = k)
         Clusdf$Cluster <- factor(result$cluster)
         output$cluster_output <- renderTable(Clusdf)
         
         output$cluster_plot <- renderPlotly({
           plot_ly(data = Clusdf, x = ~DAM, y = ~N_ménage, color = ~Cluster, type = "scatter", mode = "markers") %>%
             layout(title = "Clusters Visualization",
                    xaxis = list(title = "DAM"),
                    yaxis = list(title = "N_ménage"),
                    showlegend = TRUE)
         })
         
         output$Clusteringdata <- renderDT({
          datatable(Clusdf)
         })
       })
       

   
######ML APRIORI ALGORITHM FOR THE ASSOCIATON RULES BASED ON MY DATASET THAT I EXTRACTED FROM THE MEAN DATASET ##########       
       

       
       output$Apriorigraph <- renderPlot({
         transactions <- as(transactions, "transactions")
         rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.1, maxlen = 3))
         plot(rules, method = "grouped")
       }, width = 1000, height = 800) 
       
       
       
       output$scatterPlot = renderPlot({ 
         transactions <- as(transactions, "transactions")
         rules.all <- reactive({
           apriori( transactions , parameter=list(support=0.1, confidence=0.1 , maxlen=2))
         })
         plot(rules.all(), method = 'scatterplot')
       } ,width = 800, height = 500)
       
       
       output$rulesOutput <- renderDT({
         transactions <- as(transactions, "transactions")
         rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.2, maxlen = 2))
         rules_df <- as(rules, "data.frame")
         datatable(rules_df, options = list(pageLength = 10))  
       })
       
       
############### THE VISUALIZATION OF THE DATABASE THAT WAS PROVIDED TO ME NBR FAMILIES AND MEAN OF CHILDREN #################       
       
       
       
       output$realscatterplot<-renderPlotly({
         p <- ggplot(dataset, aes(x = Région_12, fill = Milieu)) +
           geom_bar(position = "dodge") +
           labs(title = "Taille du ménage par région et environnement",
                x = "région",
                y = "Nombre",
                fill = "Environment")+
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
         p <- ggplotly(p, tooltip = c("Région_12", "Milieu"))
         p
         
         
       
       })
     
       
       
       
       
        output$chartofProd_01 <- renderPlotly({
         library(viridis)
         
         mean_children <- aggregate(Taille_ménage ~ Région_12, data = dataset, FUN = mean)
         
         ggplot(mean_children, aes(x = Région_12, y = Taille_ménage, fill = Région_12)) +
           geom_bar(stat = "identity") +
           scale_fill_viridis_d() + 
           labs(title = "Nombre moyen d'enfants dans les familles par région",
                x = "région",
                y = "Nombre moyen d'enfants",
                fill = "Region") +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
       })
       
       
  

  
  
}


shinyApp(ui = ui, server = server)


