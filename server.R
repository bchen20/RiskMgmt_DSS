
library(shiny)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(splines)
library(polynom)
library(ggmap) 
library(maptools) 
library(maps)
library(ggrepel)
library(tidyr)
library(fmsb)

#mydata11 <- read.csv(file = "alternatives.csv", stringsAsFactors = FALSE)

attr_serurity <- data.frame(U_adv = c(0,1,0.5),V_adv = c(0,10,5))
#reverse
attr_cost <- data.frame(U_adv = c(0,1,0.5),V_adv = c(10,0,5))
attr_cuisine <- data.frame(U_adv = c(0,1,0.5),V_adv = c(0,10,5))
attr_entertainment <- data.frame(U_adv = c(0,1,0.5),V_adv = c(0,10,5))
attr_culture <- data.frame(U_adv = c(0,1,0.5),V_adv = c(0,10,5))
attr_nature <- data.frame(U_adv = c(0,1,0.5),V_adv = c(0,10,5))
#reverse
attr_environment <- data.frame(U_adv = c(0,1,0.5),V_adv = c(10,0,5))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  coe.age = reactive({
    if (input$q_age=="age1") {
      risk_v = 0.3
    }
    else if(input$q_age=="age2") {
      risk_v = 0.4
    }
    else if(input$q_age=="age3") {
      risk_v = 0.45
    }
    else if(input$q_age=="age4") {
      risk_v = 0.5
    }
    else if(input$q_age=="age5") {
      risk_v = 0.55
    }
    else if(input$q_age=="age6") {
      risk_v = 0.65
    }
    #    return (risk_v)
  })
  
  data.weight = reactive({
    total_weight = input$q_securtiy_weight + 
      input$q_cost_weight+
      input$q_cuisine_weight+
      input$q_entertainment_weight+
      input$q_culture_weight+
      input$q_nature_weight+
      input$q_environment_weight
    
    weight_list = c(input$q_securtiy_weight/total_weight,
                    input$q_cost_weight/total_weight,
                    input$q_cuisine_weight/total_weight,
                    input$q_entertainment_weight/total_weight,
                    input$q_culture_weight/total_weight,
                    input$q_nature_weight/total_weight,
                    input$q_environment_weight/total_weight)
    
    return (weight_list)
  })
  
  
  data.citysize = reactive({
    if (input$q_city=="ans_city1") {
      sz = 1
    }
    else if(input$q_city=="ans_city2"){
      sz = 2
    }
    else if(input$q_city=="ans_city3"){
      sz = 3
    }
    else if(input$q_city=="ans_city4"){
      sz = 4
    }
    return (sz)
  })
  
  data.serurity = reactive({
    if (input$q_serurity=="ans_serurity1") {
      attr_serurity[3,2] = 7.27
      attr_serurity[4,1] = 0.25
      attr_serurity[4,2] = 0 + attr_serurity[3,2] * (1-coe.age())
      attr_serurity[5,1] = 0.75
      attr_serurity[5,2] = attr_serurity[3,2] + (10 - attr_serurity[3,2]) * (1-coe.age())
    }
    else if(input$q_serurity=="ans_serurity2"){
      attr_serurity[3,2] = 6.34
      attr_serurity[4,1] = 0.25
      attr_serurity[4,2] = 0 + attr_serurity[3,2] * (1-coe.age())
      attr_serurity[5,1] = 0.75
      attr_serurity[5,2] = attr_serurity[3,2] + (10 - attr_serurity[3,2]) * (1-coe.age())
    }
    else if(input$q_serurity=="ans_serurity3"){
      attr_serurity[3,2] = 5.2
      attr_serurity[4,1] = 0.25
      attr_serurity[4,2] = 0 + attr_serurity[3,2] * (1-coe.age())
      attr_serurity[5,1] = 0.75
      attr_serurity[5,2] = attr_serurity[3,2] + (10 - attr_serurity[3,2]) * (1-coe.age())
    }
    else if(input$q_serurity=="ans_serurity4"){
      attr_serurity[3,2] = 3.23
      attr_serurity[4,1] = 0.25
      attr_serurity[4,2] = 0 + attr_serurity[3,2] * (1-coe.age())
      attr_serurity[5,1] = 0.75
      attr_serurity[5,2] = attr_serurity[3,2] + (10 - attr_serurity[3,2]) * (1-coe.age())
    }
    else if(input$q_serurity=="ans_serurity5"){
      attr_serurity[3,2] = 1.58
      attr_serurity[4,1] = 0.25
      attr_serurity[4,2] = 0 + attr_serurity[3,2] * (1-coe.age())
      attr_serurity[5,1] = 0.75
      attr_serurity[5,2] = attr_serurity[3,2] + (10 - attr_serurity[3,2]) * (1-coe.age())
    }
    return (arrange(attr_serurity,U_adv))
  })
  
  data.cost = reactive({
    if (input$q_cost=="ans_cost1") {
      attr_cost[3,2] = 1.84
      attr_cost[5,1] = 0.25
      attr_cost[4,2] = 0 + attr_cost[3,2] * coe.age()
      attr_cost[4,1] = 0.75
      attr_cost[5,2] = attr_cost[3,2] + (10 - attr_cost[3,2]) * coe.age()
    }
    else if(input$q_cost=="ans_cost2"){
      attr_cost[3,2] = 3.31
      attr_cost[5,1] = 0.25
      attr_cost[4,2] = 0 + attr_cost[3,2] * coe.age()
      attr_cost[4,1] = 0.75
      attr_cost[5,2] = attr_cost[3,2] + (10 - attr_cost[3,2]) * coe.age()
    }
    else if(input$q_cost=="ans_cost3"){
      attr_cost[3,2] = 5.15
      attr_cost[5,1] = 0.25
      attr_cost[4,2] = 0 + attr_cost[3,2] * coe.age()
      attr_cost[4,1] = 0.75
      attr_cost[5,2] = attr_cost[3,2] + (10 - attr_cost[3,2]) * coe.age()
    }
    else if(input$q_cost=="ans_cost4"){
      attr_cost[3,2] = 7.36
      attr_cost[5,1] = 0.25
      attr_cost[4,2] = 0 + attr_cost[3,2] * coe.age()
      attr_cost[4,1] = 0.75
      attr_cost[5,2] = attr_cost[3,2] + (10 - attr_cost[3,2]) * coe.age()
    }
    else if(input$q_cost=="ans_cost5"){
      attr_cost[3,2] = 8.09
      attr_cost[5,1] = 0.25
      attr_cost[4,2] = 0 + attr_cost[3,2] * coe.age()
      attr_cost[4,1] = 0.75
      attr_cost[5,2] = attr_cost[3,2] + (10 - attr_cost[3,2]) * coe.age()
    }
    return (arrange(attr_cost,U_adv))
  })
  
  
  
  data.cuisine = reactive({
    if (input$q_cuisine=="ans_cuisine1") {
      attr_cuisine[3,2] = 8
      attr_cuisine[4,1] = 0.25
      attr_cuisine[4,2] = 0 + attr_cuisine[3,2] * (1-coe.age())
      attr_cuisine[5,1] = 0.75
      attr_cuisine[5,2] = attr_cuisine[3,2] + (10 - attr_cuisine[3,2]) * (1-coe.age())
    }
    else if(input$q_cuisine=="ans_cuisine2"){
      attr_cuisine[3,2] = 6.5
      attr_cuisine[4,1] = 0.25
      attr_cuisine[4,2] = 0 + attr_cuisine[3,2] * (1-coe.age())
      attr_cuisine[5,1] = 0.75
      attr_cuisine[5,2] = attr_cuisine[3,2] + (10 - attr_cuisine[3,2]) * (1-coe.age())
    }
    else if(input$q_cuisine=="ans_cuisine3"){
      attr_cuisine[3,2] = 4.5
      attr_cuisine[4,1] = 0.25
      attr_cuisine[4,2] = 0 + attr_cuisine[3,2] * (1-coe.age())
      attr_cuisine[5,1] = 0.75
      attr_cuisine[5,2] = attr_cuisine[3,2] + (10 - attr_cuisine[3,2]) * (1-coe.age())
    }
    else if(input$q_cuisine=="ans_cuisine4"){
      attr_cuisine[3,2] = 3
      attr_cuisine[4,1] = 0.25
      attr_cuisine[4,2] = 0 + attr_cuisine[3,2] * (1-coe.age())
      attr_cuisine[5,1] = 0.75
      attr_cuisine[5,2] = attr_cuisine[3,2] + (10 - attr_cuisine[3,2]) * (1-coe.age())
    }
    else if(input$q_cuisine=="ans_cuisine5"){
      attr_cuisine[3,2] = 1.5
      attr_cuisine[4,1] = 0.25
      attr_cuisine[4,2] = 0 + attr_cuisine[3,2] * (1-coe.age())
      attr_cuisine[5,1] = 0.75
      attr_cuisine[5,2] = attr_cuisine[3,2] + (10 - attr_cuisine[3,2]) * (1-coe.age())
    }
    return (arrange(attr_cuisine,U_adv))
  })
  
  data.entertainment = reactive({
    if (input$q_entertainment=="ans_entertainment1") {
      attr_entertainment[3,2] = 6.5
      attr_entertainment[4,1] = 0.25
      attr_entertainment[4,2] = 0 + attr_entertainment[3,2] * coe.age()
      attr_entertainment[5,1] = 0.75
      attr_entertainment[5,2] = attr_entertainment[3,2] + (10 - attr_entertainment[3,2]) * coe.age()
    }
    else if(input$q_entertainment=="ans_entertainment2"){
      attr_entertainment[3,2] = 4.5
      attr_entertainment[4,1] = 0.25
      attr_entertainment[4,2] = 0 + attr_entertainment[3,2] * coe.age()
      attr_entertainment[5,1] = 0.75
      attr_entertainment[5,2] = attr_entertainment[3,2] + (10 - attr_entertainment[3,2]) * coe.age()
    }
    else if(input$q_entertainment=="ans_entertainment3"){
      attr_entertainment[3,2] = 3
      attr_entertainment[4,1] = 0.25
      attr_entertainment[4,2] = 0 + attr_entertainment[3,2] * coe.age()
      attr_entertainment[5,1] = 0.75
      attr_entertainment[5,2] = attr_entertainment[3,2] + (10 - attr_entertainment[3,2]) * coe.age()
    }
    else if(input$q_entertainment=="ans_entertainment4"){
      attr_entertainment[3,2] = 1.5
      attr_entertainment[4,1] = 0.25
      attr_entertainment[4,2] = 0 + attr_entertainment[3,2] * coe.age()
      attr_entertainment[5,1] = 0.75
      attr_entertainment[5,2] = attr_entertainment[3,2] + (10 - attr_entertainment[3,2]) * coe.age()
    }
    else if(input$q_entertainment=="ans_entertainment5"){
      attr_entertainment[3,2] = 8.5
      attr_entertainment[4,1] = 0.25
      attr_entertainment[4,2] = 0 + attr_entertainment[3,2] * coe.age()
      attr_entertainment[5,1] = 0.75
      attr_entertainment[5,2] = attr_entertainment[3,2] + (10 - attr_entertainment[3,2]) * coe.age()
    }
    return (arrange(attr_entertainment,U_adv))
  })
  
  data.culture = reactive({
    if (input$q_culture=="ans_culture1") {
      attr_culture[3,2] = 8.27
      attr_culture[4,1] = 0.25
      attr_culture[4,2] = 0 + attr_culture[3,2] * (1-coe.age())
      attr_culture[5,1] = 0.75
      attr_culture[5,2] = attr_culture[3,2] + (10 - attr_culture[3,2]) * (1-coe.age())
    }
    else if(input$q_culture=="ans_culture2"){
      attr_culture[3,2] = 6.67
      attr_culture[4,1] = 0.25
      attr_culture[4,2] = 0 + attr_culture[3,2] * (1-coe.age())
      attr_culture[5,1] = 0.75
      attr_culture[5,2] = attr_culture[3,2] + (10 - attr_culture[3,2]) * (1-coe.age())
    }
    else if(input$q_culture=="ans_culture3"){
      attr_culture[3,2] = 5.12
      attr_culture[4,1] = 0.25
      attr_culture[4,2] = 0 + attr_culture[3,2] * (1-coe.age())
      attr_culture[5,1] = 0.75
      attr_culture[5,2] = attr_culture[3,2] + (10 - attr_culture[3,2]) * (1-coe.age())
    }
    else if(input$q_culture=="ans_culture4"){
      attr_culture[3,2] = 3.43
      attr_culture[4,1] = 0.25
      attr_culture[4,2] = 0 + attr_culture[3,2] * (1-coe.age())
      attr_culture[5,1] = 0.75
      attr_culture[5,2] = attr_culture[3,2] + (10 - attr_culture[3,2]) * (1-coe.age())
    }
    else if(input$q_culture=="ans_culture5"){
      attr_culture[3,2] = 1.87
      attr_culture[4,1] = 0.25
      attr_culture[4,2] = 0 + attr_culture[3,2] * (1-coe.age())
      attr_culture[5,1] = 0.75
      attr_culture[5,2] = attr_culture[3,2] + (10 - attr_culture[3,2]) * (1-coe.age())
    }
    return (arrange(attr_culture,U_adv))
  })
  
  data.nature = reactive({
    if (input$q_nature=="ans_nature1") {
      attr_nature[3,2] = 7
      attr_nature[4,1] = 0.25
      attr_nature[4,2] = 0 + attr_nature[3,2] * coe.age()
      attr_nature[5,1] = 0.75
      attr_nature[5,2] = attr_nature[3,2] + (10 - attr_nature[3,2]) * coe.age()
    }
    else if(input$q_nature=="ans_nature2"){
      attr_nature[3,2] = 5.5
      attr_nature[4,1] = 0.25
      attr_nature[4,2] = 0 + attr_nature[3,2] * coe.age()
      attr_nature[5,1] = 0.75
      attr_nature[5,2] = attr_nature[3,2] + (10 - attr_nature[3,2]) * coe.age()
    }
    else if(input$q_nature=="ans_nature3"){
      attr_nature[3,2] = 4.5
      attr_nature[4,1] = 0.25
      attr_nature[4,2] = 0 + attr_nature[3,2] * coe.age()
      attr_nature[5,1] = 0.75
      attr_nature[5,2] = attr_nature[3,2] + (10 - attr_nature[3,2]) * coe.age()
    }
    else if(input$q_nature=="ans_nature4"){
      attr_nature[3,2] = 3.5
      attr_nature[4,1] = 0.25
      attr_nature[4,2] = 0 + attr_nature[3,2] * coe.age()
      attr_nature[5,1] = 0.75
      attr_nature[5,2] = attr_nature[3,2] + (10 - attr_nature[3,2]) * coe.age()
    }
    else if(input$q_nature=="ans_nature5"){
      attr_nature[3,2] = 2.5
      attr_nature[4,1] = 0.25
      attr_nature[4,2] = 0 + attr_nature[3,2] * coe.age()
      attr_nature[5,1] = 0.75
      attr_nature[5,2] = attr_nature[3,2] + (10 - attr_nature[3,2]) * coe.age()
    }
    return (arrange(attr_nature,U_adv))
  })
  
  data.environment = reactive({
    if (input$q_environment=="ans_environment1") {
      attr_environment[3,2] = 6.5
      attr_environment[5,1] = 0.25
      attr_environment[4,2] = 0 + attr_environment[3,2] * (1-coe.age())
      attr_environment[4,1] = 0.75
      attr_environment[5,2] = attr_environment[3,2] + (10 - attr_environment[3,2]) * (1-coe.age())
    }
    else if(input$q_environment=="ans_environment2"){
      attr_environment[3,2] = 6
      attr_environment[5,1] = 0.25
      attr_environment[4,2] = 0 + attr_environment[3,2] * (1-coe.age())
      attr_environment[4,1] = 0.75
      attr_environment[5,2] = attr_environment[3,2] + (10 - attr_environment[3,2]) * (1-coe.age())
    }
    else if(input$q_environment=="ans_environment3"){
      attr_environment[3,2] = 5.5
      attr_environment[5,1] = 0.25
      attr_environment[4,2] = 0 + attr_environment[3,2] * (1-coe.age())
      attr_environment[4,1] = 0.75
      attr_environment[5,2] = attr_environment[3,2] + (10 - attr_environment[3,2]) * (1-coe.age())
    }
    else if(input$q_environment=="ans_environment4"){
      attr_environment[3,2] = 4.35
      attr_environment[5,1] = 0.25
      attr_environment[4,2] = 0 + attr_environment[3,2] * (1-coe.age())
      attr_environment[4,1] = 0.75
      attr_environment[5,2] = attr_environment[3,2] + (10 - attr_environment[3,2]) * (1-coe.age())
    }
    else if(input$q_environment=="ans_environment5"){
      attr_environment[3,2] = 3
      attr_environment[5,1] = 0.25
      attr_environment[4,2] = 0 + attr_environment[3,2] * (1-coe.age())
      attr_environment[4,1] = 0.75
      attr_environment[5,2] = attr_environment[3,2] + (10 - attr_environment[3,2]) * (1-coe.age())
    }
    return (arrange(attr_environment,U_adv))
  })
  
  
  data.original <- reactive({
    isolate({
      mydata <- read.csv(file = "contb.csv", stringsAsFactors = FALSE)
      arrange(mydata,city)
      # #      mydata1 <- unite
      # if(input$q_continent == "cont1"){
      #   mydata1 = subset(mydata, continent!="Asia")
      # }
      # else{
      #   mydata1 = mydata
      # }
        
      return (mydata)
    })
    
  })
  
  data.normal <- reactive({
      mydata <- read.csv(file = "contb_norm.csv", stringsAsFactors = FALSE)
      arrange(mydata,city)
      #      mydata1 <- unite(mydata,city_country,city,country,sep=", ")
      if(input$q_continent == "cont2"){
        mydata1 = subset(mydata, continent!="Asia")
      }
      else if(input$q_continent == "cont1"){
        mydata1 = subset(mydata, continent!="Africa")
      }
      else if(input$q_continent == "cont3"){
        mydata1 = subset(mydata, continent!="Europe")
      }
      else if(input$q_continent == "cont4"){
        mydata1 = subset(mydata, continent!="North America")
      }
      else if(input$q_continent == "cont5"){
        mydata1 = subset(mydata, continent!="South America")
      }
      else if(input$q_continent == "cont6"){
        mydata1 = subset(mydata, continent!="Oceania")
      }
      else{
        mydata1 = mydata
      }
      
      return (mydata1)
    
  })
  
  
  
  data.top5 <- reactive({
    
    #form serurity utility function        
    func1_x <- data.serurity()$V_adv;  
    func1_x2<-func1_x^2
    func1_y<-data.serurity()$U_adv
    my_regression1<- lm(func1_y~ func1_x+func1_x2)
    coef(my_regression1)
    
    c1<- round(coef(my_regression1)[1],4)
    b1<- round(coef(my_regression1)[2],4)
    a1<- round(coef(my_regression1)[3],4)
    r1 <- 0.2
    #form cost utility function    
    func2_x <- data.cost()$V_adv;  
    func2_x2<-func2_x^2
    func2_y<-data.cost()$U_adv
    my_regression2<- lm(func2_y~ func2_x+func2_x2)
    coef(my_regression2)
    
    c2<- round(coef(my_regression2)[1],4)
    b2<- round(coef(my_regression2)[2],4)
    a2<- round(coef(my_regression2)[3],4)
    r2 <- 0.2
    #form cuisine utility function
    func3_x <- data.cuisine()$V_adv;  
    func3_x2<-func3_x^2
    func3_y<-data.cuisine()$U_adv
    my_regression3<- lm(func3_y~ func3_x+func3_x2)
    coef(my_regression3)
    
    c3<- round(coef(my_regression3)[1],4)
    b3<- round(coef(my_regression3)[2],4)
    a3<- round(coef(my_regression3)[3],4)
    r3 <- 0.1
    
    #form entertainment utility function
    func4_x <- data.entertainment()$V_adv;  
    func4_x2<-func4_x^2
    func4_y<-data.entertainment()$U_adv
    my_regression4<- lm(func4_y~ func4_x+func4_x2)
    coef(my_regression4)
    
    c4<- round(coef(my_regression4)[1],4)
    b4<- round(coef(my_regression4)[2],4)
    a4<- round(coef(my_regression4)[3],4)
    r4 <- 0.1
    
    #form culture utility function
    func5_x <- data.culture()$V_adv;  
    func5_x2<-func5_x^2
    func5_y<-data.culture()$U_adv
    my_regression5<- lm(func5_y~ func5_x+func5_x2)
    coef(my_regression5)
    
    c5<- round(coef(my_regression5)[1],4)
    b5<- round(coef(my_regression5)[2],4)
    a5<- round(coef(my_regression5)[3],4)
    r5 <- 0.1
    
    #form nature utility function
    func6_x <- data.nature()$V_adv;  
    func6_x2<-func6_x^2
    func6_y<-data.nature()$U_adv
    my_regression6<- lm(func6_y~ func6_x+func6_x2)
    coef(my_regression6)
    
    c6<- round(coef(my_regression6)[1],4)
    b6<- round(coef(my_regression6)[2],4)
    a6<- round(coef(my_regression6)[3],4)
    r6 <- 0.1
    
    #form environment utility function
    func7_x <- data.environment()$V_adv;  
    func7_x2<-func7_x^2
    func7_y<-data.environment()$U_adv
    my_regression7<- lm(func7_y~ func7_x+func7_x2)
    coef(my_regression7)
    
    c7<- round(coef(my_regression7)[1],4)
    b7<- round(coef(my_regression7)[2],4)
    a7<- round(coef(my_regression7)[3],4)
    r7 <- 0.2
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #    isolate({
    mydata <- data.normal()
    mydata1 = unite(mydata,city_country,city,country,sep=", ")
    mydata2 = subset(mydata1,met == 1)
    mydata3 = subset(mydata1, met == 0)
    if (data.citysize() == 1){
      mydata2$value_securtiy = data.weight()[1]*(a1*mydata2$securtiy_norm*mydata2$securtiy_norm + 
                                                  b1*mydata2$securtiy_norm + c1)
      mydata2$value_cost = data.weight()[2]*(a2*mydata2$cost_norm*mydata2$cost_norm + 
                                              b2*mydata2$cost_norm + c2)
      mydata2$value_cuisine = data.weight()[3]*(a3*mydata2$cuisine_norm *mydata2$cuisine_norm  + 
                                                 b3*mydata2$cuisine_norm  + c3)
      mydata2$value_entertainment = data.weight()[4]*(a4*mydata2$entertainment_norm *mydata2$entertainment_norm  + 
                                                       b4*mydata2$entertainment_norm  + c4)
      mydata2$value_culture = data.weight()[5]*(a5*mydata2$culture_norm *mydata2$culture_norm  + 
                                                 b5*mydata2$culture_norm  + c5)
      mydata2$value_nature = data.weight()[6]*(a6*mydata2$nature_norm *mydata2$nature_norm  + 
                                                b6*mydata2$nature_norm  + c6)
      mydata2$value_environment = data.weight()[7]*(a7*mydata2$environment_norm *mydata2$environment_norm  + 
                                                     b7*mydata2$environment_norm  + c7)
      mydata2$uv = mydata2$value_securtiy +mydata2$value_cost+mydata2$value_cuisine +
        mydata2$value_entertainment+ mydata2$value_culture+mydata2$value_nature+mydata2$value_environment
      
      mydata2 <- arrange(mydata2,desc(uv))
      xxx <- c(mydata2[1,1], mydata2[2,1],mydata2[3,1],mydata2[4,1],mydata2[5,1],
               mydata2[6,1], mydata2[7,1],mydata2[8,1],mydata2[9,1],mydata2[10,1])
      
    }
    else if (data.citysize() == 2){
      mydata1$value_securtiy = data.weight()[1]*(a1*mydata1$securtiy_norm*mydata1$securtiy_norm + 
                                                  b1*mydata1$securtiy_norm + c1)
      mydata1$value_cost = data.weight()[2]*(a2*mydata1$cost_norm*mydata1$cost_norm + 
                                              b2*mydata1$cost_norm + c2)
      mydata1$value_cuisine = data.weight()[3]*(a3*mydata1$cuisine_norm *mydata1$cuisine_norm  + 
                                                 b3*mydata1$cuisine_norm  + c3)
      mydata1$value_entertainment = data.weight()[4]*(a4*mydata1$entertainment_norm *mydata1$entertainment_norm  + 
                                                       b4*mydata1$entertainment_norm  + c4)
      mydata1$value_culture = data.weight()[5]*(a5*mydata1$culture_norm *mydata1$culture_norm  + 
                                                 b5*mydata1$culture_norm  + c5)
      mydata1$value_nature = data.weight()[6]*(a6*mydata1$nature_norm *mydata1$nature_norm  + 
                                                b6*mydata1$nature_norm  + c6)
      mydata1$value_environment = data.weight()[7]*(a7*mydata1$environment_norm *mydata1$environment_norm  + 
                                                     b7*mydata1$environment_norm  + c7)
      mydata1$uv = mydata1$value_securtiy +mydata1$value_cost+mydata1$value_cuisine +
        mydata1$value_entertainment+ mydata1$value_culture+mydata1$value_nature+mydata1$value_environment
      
      mydata1 <- arrange(mydata1,desc(uv))
      xxx <- c(mydata1[1,1], mydata1[2,1],mydata1[3,1],mydata1[4,1],mydata1[5,1],
               mydata1[6,1], mydata1[7,1],mydata1[8,1],mydata1[9,1],mydata1[10,1])
    }
    else if (data.citysize() == 3){
      mydata3$value_securtiy = data.weight()[1]*(a1*mydata3$securtiy_norm*mydata3$securtiy_norm + 
                                                  b1*mydata3$securtiy_norm + c1)
      mydata3$value_cost = data.weight()[2]*(a2*mydata3$cost_norm*mydata3$cost_norm + 
                                              b2*mydata3$cost_norm + c2)
      mydata3$value_cuisine = data.weight()[3]*(a3*mydata3$cuisine_norm *mydata3$cuisine_norm  + 
                                                 b3*mydata3$cuisine_norm  + c3)
      mydata3$value_entertainment = data.weight()[4]*(a4*mydata3$entertainment_norm *mydata3$entertainment_norm  + 
                                                       b4*mydata3$entertainment_norm  + c4)
      mydata3$value_culture = data.weight()[5]*(a5*mydata3$culture_norm *mydata3$culture_norm  + 
                                                 b5*mydata3$culture_norm  + c5)
      mydata3$value_nature = data.weight()[6]*(a6*mydata3$nature_norm *mydata3$nature_norm  + 
                                                b6*mydata3$nature_norm  + c6)
      mydata3$value_environment = data.weight()[7]*(a7*mydata3$environment_norm *mydata3$environment_norm  + 
                                                     b7*mydata3$environment_norm  + c7)
      mydata3$uv = mydata3$value_securtiy +mydata3$value_cost+mydata3$value_cuisine +
        mydata3$value_entertainment+ mydata3$value_culture+mydata3$value_nature+mydata3$value_environment
      
      mydata3 <- arrange(mydata3,desc(uv))
      xxx <- c(mydata3[1,1], mydata3[2,1],mydata3[3,1],mydata3[4,1],mydata3[5,1],
               mydata3[6,1], mydata3[7,1],mydata3[8,1],mydata3[9,1],mydata3[10,1])
    }
    else if (data.citysize() == 4){
      mydata1$value_securtiy = data.weight()[1]*(a1*mydata1$securtiy_norm*mydata1$securtiy_norm + 
                                                  b1*mydata1$securtiy_norm + c1)
      mydata1$value_cost = data.weight()[2]*(a2*mydata1$cost_norm*mydata1$cost_norm + 
                                              b2*mydata1$cost_norm + c2)
      mydata1$value_cuisine = data.weight()[3]*(a3*mydata1$cuisine_norm *mydata1$cuisine_norm  + 
                                                 b3*mydata1$cuisine_norm  + c3)
      mydata1$value_entertainment = data.weight()[4]*(a4*mydata1$entertainment2_norm *mydata1$entertainment2_norm  + 
                                                       b4*mydata1$entertainment2_norm  + c4)
      mydata1$value_culture = data.weight()[5]*(a5*mydata1$culture_norm *mydata1$culture_norm  + 
                                                 b5*mydata1$culture_norm  + c5)
      mydata1$value_nature = data.weight()[6]*(a6*mydata1$nature2_norm *mydata1$nature2_norm  + 
                                                b6*mydata1$nature2_norm  + c6)
      mydata1$value_environment = data.weight()[7]*(a7*mydata1$environment_norm *mydata1$environment_norm  + 
                                                     b7*mydata1$environment_norm  + c7)
      mydata1$uv = mydata1$value_securtiy +mydata1$value_cost+mydata1$value_cuisine +
        mydata1$value_entertainment+ mydata1$value_culture+mydata1$value_nature+mydata1$value_environment
      
      mydata1 <- arrange(mydata1,desc(uv))
      xxx <- c(mydata1[1,1], mydata1[2,1],mydata1[3,1],mydata1[4,1],mydata1[5,1],
               mydata1[6,1], mydata1[7,1],mydata1[8,1],mydata1[9,1],mydata1[10,1])
    }
    
    
    
    return (xxx)
    #    })
    
  })
  
  data.uv <- reactive({
    
    #form serurity utility function        
    func1_x <- data.serurity()$V_adv;  
    func1_x2<-func1_x^2
    func1_y<-data.serurity()$U_adv
    my_regression1<- lm(func1_y~ func1_x+func1_x2)
    coef(my_regression1)
    
    c1<- round(coef(my_regression1)[1],4)
    b1<- round(coef(my_regression1)[2],4)
    a1<- round(coef(my_regression1)[3],4)
    r1 <- 0.2
    #form cost utility function    
    func2_x <- data.cost()$V_adv;  
    func2_x2<-func2_x^2
    func2_y<-data.cost()$U_adv
    my_regression2<- lm(func2_y~ func2_x+func2_x2)
    coef(my_regression2)
    
    c2<- round(coef(my_regression2)[1],4)
    b2<- round(coef(my_regression2)[2],4)
    a2<- round(coef(my_regression2)[3],4)
    r2 <- 0.2
    #form cuisine utility function
    func3_x <- data.cuisine()$V_adv;  
    func3_x2<-func3_x^2
    func3_y<-data.cuisine()$U_adv
    my_regression3<- lm(func3_y~ func3_x+func3_x2)
    coef(my_regression3)
    
    c3<- round(coef(my_regression3)[1],4)
    b3<- round(coef(my_regression3)[2],4)
    a3<- round(coef(my_regression3)[3],4)
    r3 <- 0.1
    
    #form entertainment utility function
    func4_x <- data.entertainment()$V_adv;  
    func4_x2<-func4_x^2
    func4_y<-data.entertainment()$U_adv
    my_regression4<- lm(func4_y~ func4_x+func4_x2)
    coef(my_regression4)
    
    c4<- round(coef(my_regression4)[1],4)
    b4<- round(coef(my_regression4)[2],4)
    a4<- round(coef(my_regression4)[3],4)
    r4 <- 0.1
    
    #form culture utility function
    func5_x <- data.culture()$V_adv;  
    func5_x2<-func5_x^2
    func5_y<-data.culture()$U_adv
    my_regression5<- lm(func5_y~ func5_x+func5_x2)
    coef(my_regression5)
    
    c5<- round(coef(my_regression5)[1],4)
    b5<- round(coef(my_regression5)[2],4)
    a5<- round(coef(my_regression5)[3],4)
    r5 <- 0.1
    
    #form nature utility function
    func6_x <- data.nature()$V_adv;  
    func6_x2<-func6_x^2
    func6_y<-data.nature()$U_adv
    my_regression6<- lm(func6_y~ func6_x+func6_x2)
    coef(my_regression6)
    
    c6<- round(coef(my_regression6)[1],4)
    b6<- round(coef(my_regression6)[2],4)
    a6<- round(coef(my_regression6)[3],4)
    r6 <- 0.1
    
    #form environment utility function
    func7_x <- data.environment()$V_adv;  
    func7_x2<-func7_x^2
    func7_y<-data.environment()$U_adv
    my_regression7<- lm(func7_y~ func7_x+func7_x2)
    coef(my_regression7)
    
    c7<- round(coef(my_regression7)[1],4)
    b7<- round(coef(my_regression7)[2],4)
    a7<- round(coef(my_regression7)[3],4)
    r7 <- 0.2
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #    isolate({
    mydata <- data.normal()
    mydata1 <- mydata
    mydata2 = subset(mydata,met == 1)
    mydata3 = subset(mydata,met == 0)
    if (data.citysize() == 1){
      mydata2$value_securtiy = data.weight()[1]*(a1*mydata2$securtiy_norm*mydata2$securtiy_norm + 
                                                   b1*mydata2$securtiy_norm + c1)
      mydata2$value_cost = data.weight()[2]*(a2*mydata2$cost_norm*mydata2$cost_norm + 
                                               b2*mydata2$cost_norm + c2)
      mydata2$value_cuisine = data.weight()[3]*(a3*mydata2$cuisine_norm *mydata2$cuisine_norm  + 
                                                  b3*mydata2$cuisine_norm  + c3)
      mydata2$value_entertainment = data.weight()[4]*(a4*mydata2$entertainment_norm *mydata2$entertainment_norm  + 
                                                        b4*mydata2$entertainment_norm  + c4)
      mydata2$value_culture = data.weight()[5]*(a5*mydata2$culture_norm *mydata2$culture_norm  + 
                                                  b5*mydata2$culture_norm  + c5)
      mydata2$value_nature = data.weight()[6]*(a6*mydata2$nature_norm *mydata2$nature_norm  + 
                                                 b6*mydata2$nature_norm  + c6)
      mydata2$value_environment = data.weight()[7]*(a7*mydata2$environment_norm *mydata2$environment_norm  + 
                                                      b7*mydata2$environment_norm  + c7)
      mydata2$uv = mydata2$value_securtiy +mydata2$value_cost+mydata2$value_cuisine +
        mydata2$value_entertainment+ mydata2$value_culture+mydata2$value_nature+mydata2$value_environment
      
      mydata2 <- arrange(mydata2,desc(uv))
      xxx <- mydata2
      
    }
    else if (data.citysize() == 2){
      mydata$value_securtiy = data.weight()[1]*(a1*mydata1$securtiy_norm*mydata1$securtiy_norm + 
                                                  b1*mydata1$securtiy_norm + c1)
      mydata$value_cost = data.weight()[2]*(a2*mydata1$cost_norm*mydata1$cost_norm + 
                                              b2*mydata1$cost_norm + c2)
      mydata$value_cuisine = data.weight()[3]*(a3*mydata1$cuisine_norm *mydata1$cuisine_norm  + 
                                                 b3*mydata1$cuisine_norm  + c3)
      mydata$value_entertainment = data.weight()[4]*(a4*mydata1$entertainment_norm *mydata1$entertainment_norm  + 
                                                       b4*mydata1$entertainment_norm  + c4)
      mydata$value_culture = data.weight()[5]*(a5*mydata1$culture_norm *mydata1$culture_norm  + 
                                                 b5*mydata1$culture_norm  + c5)
      mydata$value_nature = data.weight()[6]*(a6*mydata1$nature_norm *mydata1$nature_norm  + 
                                                b6*mydata1$nature_norm  + c6)
      mydata$value_environment = data.weight()[7]*(a7*mydata1$environment_norm *mydata1$environment_norm  + 
                                                     b7*mydata1$environment_norm  + c7)
      mydata$uv = mydata$value_securtiy +mydata$value_cost+mydata$value_cuisine +
        mydata$value_entertainment+ mydata$value_culture+mydata$value_nature+mydata$value_environment
      
      mydata <- arrange(mydata,desc(uv))
      xxx <- mydata
    }
    else if (data.citysize() == 3){
      mydata3$value_securtiy = data.weight()[1]*(a1*mydata3$securtiy_norm*mydata3$securtiy_norm + 
                                                   b1*mydata3$securtiy_norm + c1)
      mydata3$value_cost = data.weight()[2]*(a2*mydata3$cost_norm*mydata3$cost_norm + 
                                               b2*mydata3$cost_norm + c2)
      mydata3$value_cuisine = data.weight()[3]*(a3*mydata3$cuisine_norm *mydata3$cuisine_norm  + 
                                                  b3*mydata3$cuisine_norm  + c3)
      mydata3$value_entertainment = data.weight()[4]*(a4*mydata3$entertainment_norm *mydata3$entertainment_norm  + 
                                                        b4*mydata3$entertainment_norm  + c4)
      mydata3$value_culture = data.weight()[5]*(a5*mydata3$culture_norm *mydata3$culture_norm  + 
                                                  b5*mydata3$culture_norm  + c5)
      mydata3$value_nature = data.weight()[6]*(a6*mydata3$nature_norm *mydata3$nature_norm  + 
                                                 b6*mydata3$nature_norm  + c6)
      mydata3$value_environment = data.weight()[7]*(a7*mydata3$environment_norm *mydata3$environment_norm  + 
                                                      b7*mydata3$environment_norm  + c7)
      mydata3$uv = mydata3$value_securtiy +mydata3$value_cost+mydata3$value_cuisine +
        mydata3$value_entertainment+ mydata3$value_culture+mydata3$value_nature+mydata3$value_environment
      
      mydata3 <- arrange(mydata3,desc(uv))
      xxx <- mydata3
    }
    else if (data.citysize() == 4){
      mydata$value_securtiy = data.weight()[1]*(a1*mydata1$securtiy_norm*mydata1$securtiy_norm + 
                                                  b1*mydata1$securtiy_norm + c1)
      mydata$value_cost = data.weight()[2]*(a2*mydata1$cost_norm*mydata1$cost_norm + 
                                              b2*mydata1$cost_norm + c2)
      mydata$value_cuisine = data.weight()[3]*(a3*mydata1$cuisine_norm *mydata1$cuisine_norm  + 
                                                 b3*mydata1$cuisine_norm  + c3)
      mydata$value_entertainment = data.weight()[4]*(a4*mydata1$entertainment2_norm *mydata1$entertainment2_norm  + 
                                                       b4*mydata1$entertainment2_norm  + c4)
      mydata$value_culture = data.weight()[5]*(a5*mydata1$culture_norm *mydata1$culture_norm  + 
                                                 b5*mydata1$culture_norm  + c5)
      mydata$value_nature = data.weight()[6]*(a6*mydata1$nature2_norm *mydata1$nature2_norm  + 
                                                b6*mydata1$nature2_norm  + c6)
      mydata$value_environment = data.weight()[7]*(a7*mydata1$environment_norm *mydata1$environment_norm  + 
                                                     b7*mydata1$environment_norm  + c7)
      mydata$uv = mydata$value_securtiy +mydata$value_cost+mydata$value_cuisine +
        mydata$value_entertainment+ mydata$value_culture+mydata$value_nature+mydata$value_environment
      
      mydata <- arrange(mydata,desc(uv))
      xxx <- mydata
    } 
    
    
    return (xxx)
    #    })
    
  })
  
  
  output$curve1 <- renderPlot({
    
    ggplot(data.serurity(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("serurity Utility Value")+
      xlab("Value of serurity")+
      ggtitle("Utility Function of serurity")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  output$curve2 <- renderPlot({
    
    ggplot(data.cost(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("Cost Utility Value")+
      xlab("Value of Cost")+
      ggtitle("Utility Function of Cost")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  
  
  output$curve3 <- renderPlot({
    
    ggplot(data.cuisine(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("cuisine Utility Value")+
      xlab("Value of cuisine")+
      ggtitle("Utility Function of cuisine")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  output$curve4 <- renderPlot({
    
    ggplot(data.entertainment(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("entertainment Utility Value")+
      xlab("Value of entertainment")+
      ggtitle("Utility Function of entertainment")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  output$curve5 <- renderPlot({
    
    ggplot(data.culture(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("culture Utility Value")+
      xlab("Value of culture")+
      ggtitle("Utility Function of culture")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  output$curve6 <- renderPlot({
    
    ggplot(data.nature(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("nature Utility Value")+
      xlab("Value of nature")+
      ggtitle("Utility Function of nature")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  output$curve7 <- renderPlot({
    
    ggplot(data.environment(),aes(x=V_adv, y=U_adv))+
      geom_point()+
      theme_bw()+
      ylab("environment Utility Value")+
      xlab("Value of environment")+
      ggtitle("Utility Function of environment")+
      geom_smooth(method = "glm", formula = y~poly(x,degree = 2,raw = T),se=F)+
      stat_poly_eq(formula = y~poly(x,degree = 2,raw = T),
                   aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
  })
  
  
  #   output$func0 <- renderText({
  #     
  #     func0_x <- data.r()$V_adv;  
  #     func0_x2<-func0_x^2
  #     func0_y<-data.r()$U_adv
  #     
  #     my_regression0<- lm(func0_y~ func0_x+func0_x2)
  #     
  #     coef(my_regression0)
  #     
  #     c0<- round(coef(my_regression0)[1],4)
  #     b0<- round(coef(my_regression0)[2],4)
  #     a0<- round(coef(my_regression0)[3],4)
  #     function0 <- c("U(a) = ",a0,"x^2 + ",b0,"x + ",c0)
  #     print (function0)
  # })
  
  output$rad1 <- renderPlot({
    
    city = arrange(data.uv(),desc(uv))
    city0 = city[,14:20]
    city1 = city0[1,] 
    radar_min = apply(city0, 2, min)
    radar_max = apply(city0, 2, max)
    mydata_radar = rbind(radar_max,radar_min,city0[1,])
    
    colors_border=c( rgb(0.1,0.1,0.5,0.9), rgb(0.8,0.2,0.5,0.9) ,
                     rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.1,0.1,0.5,0.6), rgb(0.8,0.2,0.5,0.4) ,
                 rgb(0.7,0.5,0.1,0.3))
    
    radarchart( mydata_radar, axistype=1 ,
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                caxislabels=c("worst", "", "", "", "best"),
                vlcex=0.8,title=city[1,1]
    )
    
  })
  
  output$rad2 <- renderPlot({
    
    city = arrange(data.uv(),desc(uv))
    city0 = city[,14:20]
    city1 = city0[2,] 
    radar_min = apply(city0, 2, min)
    radar_max = apply(city0, 2, max)
    mydata_radar = rbind(radar_max,radar_min,city0[2,])
    print (city[1:5,])
    
    colors_border=c( rgb(0.1,0.1,0.5,0.9), rgb(0.8,0.2,0.5,0.9) ,
                     rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.1,0.1,0.5,0.6), rgb(0.8,0.2,0.5,0.4) ,
                 rgb(0.7,0.5,0.1,0.3))
    
    radarchart( mydata_radar, axistype=1 ,
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                caxislabels=c("worst", "", "", "", "best"),
                vlcex=0.8,title=city[2,1]
    )
    
  })
  
  output$rad3 <- renderPlot({
    
    city = arrange(data.uv(),desc(uv))
    city0 = city[,14:20]
    city1 = city0[3,] 
    radar_min = apply(city0, 2, min)
    radar_max = apply(city0, 2, max)
    mydata_radar = rbind(radar_max,radar_min,city0[3,])
    
    colors_border=c( rgb(0.1,0.1,0.5,0.9), rgb(0.8,0.2,0.5,0.9) ,
                     rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.1,0.1,0.5,0.6), rgb(0.8,0.2,0.5,0.4) ,
                 rgb(0.7,0.5,0.1,0.3))
    
    radarchart( mydata_radar, axistype=1 ,
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                caxislabels=c("worst", "", "", "", "best"),
                vlcex=0.8,title=city[3,1]
    )
    
  })
  
  output$rad4 <- renderPlot({
    
    city = arrange(data.uv(),desc(uv))
    city0 = city[,14:20]
    city1 = city0[4,] 
    radar_min = apply(city0, 2, min)
    radar_max = apply(city0, 2, max)
    mydata_radar = rbind(radar_max,radar_min,city0[4,])
    
    colors_border=c( rgb(0.1,0.1,0.5,0.9), rgb(0.8,0.2,0.5,0.9) ,
                     rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.1,0.1,0.5,0.6), rgb(0.8,0.2,0.5,0.4) ,
                 rgb(0.7,0.5,0.1,0.3))
    
    radarchart( mydata_radar, axistype=1 ,
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                caxislabels=c("worst", "", "", "", "best"),
                vlcex=0.8,title=city[4,1]
    )
    
  })
  
  output$rad5 <- renderPlot({
    
    city = arrange(data.uv(),desc(uv))
    city0 = city[,14:20]
    city1 = city0[5,] 
    radar_min = apply(city0, 2, min)
    radar_max = apply(city0, 2, max)
    mydata_radar = rbind(radar_max,radar_min,city0[5,])
    
    colors_border=c( rgb(0.1,0.1,0.5,0.9), rgb(0.8,0.2,0.5,0.9) ,
                     rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.1,0.1,0.5,0.6), rgb(0.8,0.2,0.5,0.4) ,
                 rgb(0.7,0.5,0.1,0.3))
    
    radarchart( mydata_radar, axistype=1 ,
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                caxislabels=c("worst", "", "", "", "best"),
                vlcex=0.8,title=city[5,1]
    )
    
  })
  
  output$map1 <- renderPlot({
    
    #    visited <- c("SFO", "Chennai", "London", "Melbourne", "Johannesbury South Afric") 
    visited <- data.top5()[1:5]
    ll.visited <- geocode(visited) 
    visit.x <- ll.visited$lon 
    visit.y <- ll.visited$lat 
    
    mp <- NULL 
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders 
    mp <- ggplot() +   mapWorld 
    
    
    
    mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
    mp <- mp + geom_label_repel(aes(x=visit.x, y=visit.y,label=visited),
                                #                          nudge_x = visit.x, nudge_y = visit.y,
                                size = 3.5,colour = "purple", fontface = "bold")
    mp
  })
  
  output$weight1 <- renderText({
    
    weightofsercuity = c("Weight of sercuity is ",data.weight()[1])
    print (weightofsercuity)
  })
  
  output$weight2 <- renderText({
    
    weightofsercuity = c("Weight of cost is ",data.weight()[2])
    print (weightofsercuity)
  })
  
  output$weight3 <- renderText({
    
    weightofsercuity = c("Weight of cuisine is ",data.weight()[3])
    print (weightofsercuity)
  })
  
  output$weight4 <- renderText({
    
    weightofsercuity = c("Weight of entertainment is ",data.weight()[4])
    print (weightofsercuity)
  })
  
  output$weight5 <- renderText({
    
    weightofsercuity = c("Weight of culture and history is ",data.weight()[5])
    print (weightofsercuity)
  })
  
  output$weight6 <- renderText({
    
    weightofsercuity = c("Weight of nature resource is ",data.weight()[6])
    print (weightofsercuity)
  })
  
  output$weight7 <- renderText({
    
    weightofsercuity = c("Weight of environemnt is ",data.weight()[7])
    print (weightofsercuity)
  })
  
  output$city1 <- renderText({
    
    recom_city1 = c("--------     ",data.top5()[1])
    print (recom_city1)
  })
  
  output$city2 <- renderText({
    
    recom_city2 = c("--------     ",data.top5()[2])
    print (recom_city2)
  })
  
  output$city3 <- renderText({
    
    recom_city3 = c("--------     ",data.top5()[3])
    print (recom_city3)
  })
  
  
  output$city4 <- renderText({
    
    recom_city4 = c("--------     ",data.top5()[4])
    print (recom_city4)
  })
  
  output$city5 <- renderText({
    
    recom_city5 = c("--------     ",data.top5()[5])
    print (recom_city5)
  })
  
  # output$city6 <- renderText({
  #   
  #   recom_city6 = c("--------     ",data.top5()[6])
  #   print (recom_city6)
  # })
  # 
  # output$city7 <- renderText({
  #   
  #   recom_city7 = c("--------     ",data.top5()[7])
  #   print (recom_city7)
  # })
  # 
  # output$city8 <- renderText({
  #   
  #   recom_city8 = c("--------     ",data.top5()[8])
  #   print (recom_city8)
  # })
  # 
  # output$city9 <- renderText({
  #   
  #   recom_city9 = c("--------     ",data.top5()[9])
  #   print (recom_city9)
  # })
  # 
  # output$city10 <- renderText({
  #   
  #   recom_city10 = c("--------     ",data.top5()[10])
  #   print (recom_city10)
  # })
  
  output$dtrt <- DT::renderDataTable(arrange(data.original(),city),  
                                     rownames = FALSE, server = FALSE, 
                                     options = list(pageLength = 10, dom = 'tp'), 
                                     selection = list(mode = "single", target = "column", 
                                                      selected = 0) 
  )
  
  output$dtut <- DT::renderDataTable(arrange(data.uv(),desc(uv)),  
                                     rownames = FALSE, server = FALSE, 
                                     options = list(pageLength = 10, dom = 'tp'), 
                                     selection = list(mode = "single", target = "column", 
                                                      selected = 0) 
  )
  
  
  
})
