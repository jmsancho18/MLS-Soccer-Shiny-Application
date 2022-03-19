#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(fmsb)
library(cluster)
library(FactoMineR)
library(factoextra)
library(tibble)
library(NbClust)
library(corrplot)
library(DT)



getwd()
setwd("C:/Users/niede/Documents/American_Soccer/")
soccer <- read.csv("ASAshootertable.csv")  # 2015 - 2018 MLS Season Statistics
attach(soccer)


futbol <- soccer %>% select(-First, - Last) %>% mutate_if(is.numeric, funs(round(., 2))) %>% arrange(Season)


# EXTRACT ACTIVE INDIVIDUALS AND VARIABLES FOR PCA AND PAM
analysis <- 
  soccer %>%
  select(Player, Season, Shots, SoT, Goals, Assts, KeyP) %>%
  filter(Season == "2018") %>%
  remove_rownames() %>%
  column_to_rownames(var = "Player")

analysis_stats <- data.frame(
  Min = apply(analysis, 2, min), # minimum
  Q1 = apply(analysis, 2, quantile, 1/4), # First quartile
  Med = apply(analysis, 2, median), # median
  Mean = apply(analysis, 2, mean), # mean
  Q3 = apply(analysis, 2, quantile, 3/4), # Third quartile
  Max = apply(analysis, 2, max) # Maximum
)
analysis_stats <- round(analysis_stats, 1)
head(analysis_stats)

# CORRELATION MATRIX AND PLOT

cor.mat <- round(cor(analysis), 2)
head(cor.mat)

corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

# PRINCIPAL COMPONENT ANALYSIS 

soccer_pca <- PCA(analysis)

fviz_pca_var(soccer_pca, col.var="contrib") +
  scale_color_gradient2(low = "white", mid = "blue", 
                        high = "red") +
  theme_bw()

# ELLIPSES ARE USEFUL IF YOU HAVE GROUPS - POSITION/TEAM/ETC - OTHERWISE NOT
fviz_pca_ind(soccer_pca, addEllipses = T, ellipse.level = 0.95)

# OR

fviz_pca_biplot(soccer_pca, geom = "text")

fviz_pca_ind(soccer_pca,  col.ind = "cos2") +
  scale_color_gradient2(low = "white", mid = "blue", 
                        high = "red") +
  theme_minimal()

# SCALED DATA FRAME FOR USE IN PAM
analysis_scaled <- 
  scale(analysis)

analysis_scaled <-
  analysis_scaled[, -1]



# Determine number of clusters with silhouette method
fviz_nbclust(analysis_scaled, pam, method = "silhouette") +
  theme_classic()

# Determine optimal number of clusters with 30 indices
nb <- NbClust(analysis_scaled, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")



pam.res <- pam(analysis_scaled, 5)

fviz_cluster(pam.res, 
             palette = "Set2", # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             geom = "point",
             ggtheme = theme_minimal()
)



# DEFINE UI FOR APP THAT CREATES RADAR PLOT -------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("MLS Athlete Comparisons"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "filter",              # Select variables to filter on for analysis
                  label = "Table Variables:",
                  choices = c("Player", "Season", "Team", "Min", "Goals", "Shots", "SoT", "Dist", "Solo", "xG", "xPlace", "G.xG", "KeyP", "Dist.key",
                              "Assts", "xA", "A.xA", "xG.xA", "xGperShot", "xAperPass", "GmxGperShot", "AmxAperPass"),
                  multiple = T,
                  selectize = T,
                  selected = c("Player", "Team", "Season", "Goals", "Shots", "SoT", "Assts", "KeyP")
      ),
      checkboxGroupInput(inputId = "years",
                         label = "Select Table Seasons:",
                         choices = c("2018", "2017", "2016", "2015"),
                         selected = c("2018")
      ),
      numericInput(inputId = "goals",
                   label = "Minimum Goals:",
                   value = 0,
                   step = 5),
      selectInput(inputId = "cluster",
                  label = "Cluster Variables:",
                  choices = c("Season", "Player", "Goals", "Shots", "SoT", "Dist", "Solo", "xG", "xPlace", "G.xG", "KeyP",
                              "Assts", "xA", "A.xA", "xG.xA", "xGperShot", "xAperPass", "GmxGperShot", "AmxAperPass"),
                  multiple = T,
                  selectize = T,
                  selected = c("Season", "Player", "Goals", "Shots", "SoT", "KeyP", "Assts")
      ),
      selectInput(inputId = "clusteryears",
                  label = "Clustering Season:",
                  choices = c("2018", "2017", "2016", "2015"),
                  selected = c("2018")),
      sliderInput(inputId = "k",
                  label = "Number of Clusters:",
                  min = 2,
                  max = 10,
                  value = 4)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # Create tabs for table, clustering, and PCA
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("table")),
                  tabPanel("Cluster", 
                           textOutput("text"),
                           plotOutput("cluster"),
                           tableOutput("medoids")),
                  tabPanel("Explanation",
                           plotOutput("PCA"),
                           plotOutput("silhouette"))
      )
    )
  )
)

# DEFINE SERVER LOGIC TO CREATE DT AND CLUSTER --------------------------------------
server <- function(input, output) {
  
  output$table <- DT::renderDataTable({
    
    futbol_filter <- 
      futbol %>%
      select(input$filter) %>%
      filter(Season %in% input$years, Goals >= input$goals) %>%
      arrange(Season)
    
    DT::datatable(futbol_filter, rownames = F,
                  options = list(autoWidth = T))
    
    
  })
  
  output$cluster <- renderPlot({
    
    analysis <-
      futbol %>%
      select(input$cluster) %>%
      filter(Season %in% input$clusteryears, Goals >= input$goals)
    
    analysis_scaled <- scale(analysis[, -c(1:2)])
    
    pam.res <- pam(analysis_scaled, input$k) # SLIDER INPUT TO CHOOSE NUMBER OF CLUSTERS
    
    fviz_cluster(pam.res, 
                 palette = "Set2", # color palette
                 ellipse.type = "t", # Concentration ellipse
                 repel = TRUE, # Avoid label overplotting (slow)
                 geom = "point",
                 ggtheme = theme_minimal()
    )
    
  })
  
  output$medoids <- renderTable({
    
    analysis <-
      futbol %>%
      select(input$cluster) %>%
      filter(Season %in% input$clusteryears, Goals >= input$goals) %>%
      remove_rownames() %>%
      column_to_rownames(var = "Player")
    
    analysis_scaled <- scale(analysis[, -1])
    
    
    pam.res <- pam(analysis_scaled, input$k)
    
    pam.res$medoids
    
    
  })
  
  output$PCA <- renderPlot({
    
    analysis <-
      futbol %>%
      select(input$cluster) %>%
      filter(Season %in% input$clusteryears, Goals >= input$goals)
    
    soccer_pca <- PCA(analysis[, -c(1:2)])
    
    fviz_pca_var(soccer_pca, col.var="contrib") +
      scale_color_gradient2(low = "white", mid = "blue", 
                            high = "red") +
      theme_bw()
    
  })
  
  
  output$silhouette <- renderPlot({
    
    analysis <-
      futbol %>%
      select(input$cluster) %>%
      filter(Season %in% input$clusteryears, Goals >= input$goals)
    
    analysis_scaled <- scale(analysis[, -c(1:2)])
    
    fviz_nbclust(analysis_scaled, pam, method = "silhouette") +
      theme_classic()
    
  })
  
  
  
  
}
# RUN THE APP ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
