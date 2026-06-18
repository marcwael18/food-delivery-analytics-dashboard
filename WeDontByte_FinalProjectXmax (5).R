library(arules)
library(arulesViz)
library(dplyr)
library(factoextra)
library(shiny)
library(bslib)

ui = page_sidebar(
  input_dark_mode(id = "mode"), 
  "Food delivery Analysis" ,
  sidebar = sidebar(
    title = "Adjust" ,
    sliderInput(
      "support" ,
      label = "support" ,
      min = 0.01 ,
      max = 0.26 ,
      value = 0.01
    ) ,
    sliderInput(
      "confidence" ,
      label = "confidence" ,
      min = 0.4 ,
      max = 0.735 ,
      value = 0.4
    ) ,
    sliderInput(
      "center" ,
      label = "Centers" ,
      min = 1 ,
      max = 10 ,
      value = 1
    ) ,
    fileInput("fi", "Browse Files"),
    
  ),
  mainPanel(navset_tab(
    nav_panel("KMeans" , card(
      card_header("Kmeans") , card_body(plotOutput("Km"))
    )), nav_panel("Apriori" , card(
      card_header("Apriori") , card_body(plotOutput("Ar"))
    )), nav_panel("Visualization" , card(tabsetPanel(
      tabPanel("Delivery Time Distribution", plotOutput("DTd")),
      tabPanel("Vehicle Type Comparison", plotOutput("VTc")),
      tabPanel("Rating vs Delivery", plotOutput("RvsD")),
      tabPanel("Distance Distribution", plotOutput("DD")),
      tabPanel("Weather Effect", plotOutput("WE")),
      tabPanel("Orders by Weather", plotOutput("ObW")),
      tabPanel("Delays Over Day", plotOutput("DoD")),
      tabPanel("Heatmap",plotOutput("Hm")))
    ))
  ))
)

server = function(input, output) {
  dataload <- reactiveVal()
  dataset <- reactive({Foodd <- dataload()
    duplicated(Foodd)
    sum(duplicated(Foodd))
    str(Foodd)
    # All columns are saved with the correct data types
    is.na(Foodd)
    sum(is.na(Foodd))
    Foodd$Courier_Experience_yrs[is.na(Foodd$Courier_Experience_yrs)] = median(Foodd$Courier_Experience_yrs, na.rm = TRUE)
    Foodd$Distance_km[is.na(Foodd$Distance_km)] <- mean(Foodd$Distance_km, na.rm = TRUE)
    Foodd$Preparation_Time_min[is.na(Foodd$Preparation_Time_min)] <- mean(Foodd$Preparation_Time_min, na.rm = TRUE)
    Foodd$Delivery_Time_min[is.na(Foodd$Delivery_Time_min)] <- mean(Foodd$Delivery_Time_min, na.rm = TRUE)
    which.max(table(Foodd$Traffic_Level))
    which.max(table(Foodd$Time_of_Day))
    which.max(table(Foodd$Vehicle_Type))
    which.max(table(Foodd$Weather))
    Foodd$Traffic_Level[is.na(Foodd$Traffic_Level)] <- "Medium"
    Foodd$Time_of_Day[is.na(Foodd$Time_of_Day)] <- "Morning"
    Foodd$Vehicle_Type[is.na(Foodd$Vehicle_Type)] <- "Bike"
    Foodd$Weather[is.na(Foodd$Weather)] <- "Clear"
    num_cols <- sapply(Foodd, is.numeric)
    df_numeric <- Foodd[, num_cols]
    boxplot(df_numeric)
    outlier=boxplot(Foodd$Delivery_Time_min)$out
    outlier
    Foodd[which(Foodd$Delivery_Time_min %in% outlier),]
    Foodd1 =Foodd[-which(Foodd$Delivery_Time_min %in% outlier),]
    Foodd1
    boxplot(Foodd1$Delivery_Time_min)
    total_time = Foodd1$Preparation_Time_min+Foodd1$Delivery_Time_min
    speed = Foodd1$Distance_km/total_time
    Foodd2 = cbind(Foodd1,speed)
    summary(Foodd2$speed)
    #Mean = 0.132917
    Late_Delivery_Flag = ifelse (Foodd2$speed<0.132917,
                                 "LATE",
                                 "ON TIME")
    FOOD = cbind(Foodd2,Late_Delivery_Flag)
    Customer_rating = ifelse(FOOD$speed<=0.0086080,
                             0,
                      ifelse(FOOD$speed<=0.0437072,
                             0.5,
                      ifelse(FOOD$speed<=0.0788064,
                             1,
                      ifelse(FOOD$speed<=0.1139056,
                             1.5,
                      ifelse(FOOD$speed<=0.1490048,
                             2,
                      ifelse(FOOD$speed<=0.1841040,
                             2.5,
                      ifelse(FOOD$speed<=0.2192032,
                             3,
                      ifelse(FOOD$speed<=0.2543024,
                             3.5,
                      ifelse(FOOD$speed<=0.2894016,
                             4,
                      ifelse(FOOD$speed<=0.3245008,
                             4.5,
                      ifelse(FOOD$speed<=0.3596000,
                             5,5        )))))))))))
    FOOD = cbind(FOOD,Customer_rating)
    FOOD})
    
  output$Ar <- renderPlot({
    req(dataset())
    FOOD <- dataset()
    df = FOOD %>% select(Weather,         
                         Traffic_Level,
                         Time_of_Day,
                         Vehicle_Type,
                         Late_Delivery_Flag) # Apriori only takes factors
    data_trans = as(df , "transactions") # you converted the data from factors to transactions
    apr = apriori(
      data_trans,
      parameter = list(
        support = input$support ,
        confidence = input$confidence
      ) ,
      appearance = list(rhs = grep(
        "^Late_Delivery_Flag=" , itemLabels(data_trans) , value = TRUE
      ))
    ) # THIS NUMBER IS TEMPORARY
    plot(apr, method = "group")
  })
  
  output$Km <- renderPlot({
    req(dataset())
    FOOD <- dataset()
    df_num = FOOD %>% select(
      Distance_km,
      Delivery_Time_min,
      Preparation_Time_min,
      Courier_Experience_yrs,
      speed
    )
    scaleDf = scale(df_num)
    kfood = kmeans(scaleDf , centers = input$center , nstart = 40)
    fviz_cluster(
      kfood ,
      data = scaleDf ,
      geom = "point" ,
      ellipse.type = "convex" ,
      palette = "jco",
      ggtheme = theme_minimal() ,
      show.clust.cent = TRUE
    )
  })
  
  output$DTd <- renderPlot({
    req(dataset())
    FOOD <- dataset()
    
    hist(FOOD$Delivery_Time_min,
         main = "Distribution of Delivery Time",
         xlab = "Delivery Time (minutes)",
         col = "lightblue",
         border = "black")
    #-------------------------------------------------------------
    # 1) Histogram: Distribution of Delivery Time
    #    — يوضح توزيع وقت التوصيل وهل أغلب الطلبات سريعة أو بطيئة
    # ------------------------------------------------------------
})
  output$VTc <- renderPlot({
    req(dataset())
    FOOD <- dataset()
    
    boxplot(Delivery_Time_min ~ Vehicle_Type,
            data = FOOD,
            main = "Delivery Time by Vehicle Type",
            xlab = "Vehicle Type",
            ylab = "Delivery Time (min)",
            col = "orange")
    # ------------------------------------------------------------
    # 2) Boxplot: Delivery Time by Vehicle Type
    #    — يقارن بين أنواع المركبات ويكشف أي نوع بيسبب تأخير
    # ------------------------------------------------------------
  })
  output$RvsD <- renderPlot({
    req(dataset())
    FOOD <- dataset()
    
    plot(FOOD$Customer_rating,
         FOOD$Delivery_Time_min,
         main = "Rating vs Delivery Time",
         xlab = "Customer Rating",
         ylab = "Delivery Time (min)",
         pch = 16,
         col = "blue")
    # ------------------------------------------------------------
    # 3) Scatter Plot: Rating vs Delivery Time
    #    — يكشف العلاقة بين تقييم العميل والوقت اللي أخذته الطلبية
    # ------------------------------------------------------------
  })
  
    
    

   output$DD <- renderPlot({
    req(dataset())
    FOOD <- dataset()
    
    hist(FOOD$Distance_km,
         main = "Distribution of Distance",
         xlab = "Distance (km)",
         col = "green",
         border = "white")
    # ------------------------------------------------------------
    # 4) Histogram: Distribution of Distance
    #    — يظهر المسافات الشائعة وهل أغلب الطلبات قريبة ولا بعيدة
    # ------------------------------------------------------------
    })
  
    
    
    
  
   output$WE <- renderPlot({
     req(dataset())
     FOOD <- dataset()
     
     boxplot(Delivery_Time_min ~ Weather,
             data = FOOD,
             main = "Delivery Time by Weather Conditions",
             xlab = "Weather",
             ylab = "Delivery Time",
             col = "lightgreen")
    # ------------------------------------------------------------
    # 5) Boxplot: Delivery Time by Weather Conditions
    #    — يقارن وقت التوصيل حسب حالة الطقس ويظهر تأثيره على التأخير
    # ------------------------------------------------------------
   })
   
    
    
  
   output$ObW <- renderPlot({
     req(dataset())
     FOOD <- dataset()
     
     barplot(table(FOOD$Weather),
             main = "Number of Orders by Weather",
             xlab = "Weather",
             ylab = "Count",
             col = "purple")  
    # ------------------------------------------------------------
    # 6) Barplot: Number of Orders by Weather
    #    — يوضح عدد الطلبات في كل حالة طقس وأي حالة هي الأكثر انتشارًا
    # ------------------------------------------------------------
   })
   
    
   
   output$DoD <- renderPlot({
     req(dataset())
     FOOD <- dataset()
     
     boxplot(Delivery_Time_min ~ Time_of_Day,
             data = FOOD,
             main = "Delays Across the Day",
             xlab = "Time of Day",
             ylab = "Delivery Time",
             col = "red")
    # ------------------------------------------------------------
    # 8) Line Plot: Delivery Time Across the Day
    # ------------------------------------------------------------
    # ⚠ لازم يكون عندك عمود Hour في الداتا
   })
   
  
    
    output$Hm <- renderPlot({
      req(dataset())
      FOOD <- dataset()
          # ------------------------------------------------------------
    # 7) Correlation Heatmap
    #    — يرسم خريطة ألوان تبيّن قوة العلاقة بين المتغيرات الرقمية
    # ------------------------------------------------------------
    numeric_cols <- FOOD[, c("Distance_km",
                             "Preparation_Time_min",
                             "Delivery_Time_min")]
    
    cor_matrix <- cor(numeric_cols)
    
    heatmap(cor_matrix,
            main = "Correlation Heatmap")
      
    })
  
  observeEvent(input$fi , {
    req(input$fi)
    userdata = read.csv(input$fi$datapath, na.strings = c("","NA"))
    dataload(rbind(dataload() , userdata))
  })
  

}

shinyApp(ui = ui, server = server)





