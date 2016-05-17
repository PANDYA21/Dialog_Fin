library(shiny)
library(shinydashboard)
library(ggplot2) 
# library(DT) 

# load the racket database
load("prereqs.RData")

# translate headsize data "points" variable to "player-type" variable values
heads$points[which(heads$points == "0-5 points")] <- "beginner"
heads$points[which(heads$points == "6-10 points")] <- "hobby"
heads$points[which(heads$points == "11-15 points")] <- "LK 1-8"
heads$points[which(heads$points == "16-20 points")] <- "LK 9-16"
heads$points[which(heads$points == "> 20 points")] <- "LK 17-23"

## the reactive function
getFinDf <- function(In.age, In.sex, In.weight, In.lev, In.ptype, In.err, In.head, In.stroke){
  
  ################
  if(In.sex == ""){
    In.sex <- "male"
  } else {
    In.sex <- tolower(In.sex)
  }
  
  # print(In.age)
  if(is.na(as.numeric(In.age))){
    In.age <- "13-18"
  } else {
    if(as.numeric(In.age) <= 18){
      In.age <- "13-18"
    } else if(as.numeric(In.age) > 18 & as.numeric(In.age) <= 35){
      In.age <- "19-35"
    } else if(as.numeric(In.age) > 36 & as.numeric(In.age) <= 60){
      In.age <- "36-60"
    } else if(as.numeric(In.age) > 60){
      In.age <- "over 60"
    }
  }
  
  if (In.lev == ""){
    In.lev <- "beginner"
  } else if(grepl("hobby", In.lev, ignore.case = T)){
    In.lev <- tolower("hobby")
  } else if(grepl("lk[ ]*1-8", In.lev, ignore.case = T)){
    In.lev <- "LK 1-8"
  } else if(grepl("lk[ ]*9-16", In.lev, ignore.case = T)){
    In.lev <- "LK 9-16"
  } else if(grepl("lk[ ]*17-23", In.lev, ignore.case = T)){
    In.lev <- "LK 17-23"
  } 
  
  if(In.ptype == ""){
    In.ptype <- "aggressive"
  } else if(grepl("know", In.ptype, ignore.case = T)){
    In.ptype <- "aggressive"
  } else{
    In.ptype <- tolower(In.ptype)
  }
  
  if(In.stroke == ""){
    In.stroke <- "topspin"
  } else if(grepl("know", In.stroke, ignore.case = T)){
    In.stroke <- "topspin"
  } else{
    In.stroke <- tolower(In.stroke)
  }
  
  if(In.err == ""){
    In.err <- "too low accuracy"
  } else if(grepl("know", In.err, ignore.case = T)){
    In.err <- "too low accuracy"
  } else if(grepl("out[ ]*of[ ]*bounds", In.err, ignore.case = T)) {
    In.err <- tolower("too many balls out of bounds")
  } else if(grepl("low[ ]*swing", In.err, ignore.case = T)){
    In.err <- tolower("too low swing speed")
  } else if(grepl("many[ ]*frame[ ]*hits", In.err, ignore.case = T)){
    In.err <- tolower("too many frame hits")
  } else if(grepl("low[ ]*accuracy", In.err, ignore.case = T)){
    In.err <- tolower("too low accuracy")
  }
  
  if (grepl("no|No|NO|nO", In.weight, ignore.case = T)){
    In.weight <- "middle (260-300g)"
  } else{
    if(is.na(as.numeric(In.weight))){
      In.weight <- "middle (260-300g)"
    } else{
      if (as.numeric(In.weight) <= 260){
        In.weight <- "light (under 260g)"#,	"middle (260-300g)", "heavy (over 300g)"
      } else if (as.numeric(In.weight) > 260 & as.numeric(In.weight) <= 300){
        In.weight <- "middle (260-300g)"
      } else if (as.numeric(In.weight) > 300){
        In.weight <- "heavy (over 300g)"
      } 
    }
  }
  
  if(In.head == ""){
    In.head <- "middle (630-660 cm²)"
  } else if (is.na(as.numeric(In.head))){
    if (grepl("no|No|NO|nO", In.head)){
      In.head <- "middle (630-660 cm²)"
    } 
  } else if (as.numeric(In.head) <= 630){
    In.head <- "small (under 630 cm²)"# , "middle (630-660 cm²)", "large (over 660 cm²)"
  } else if (as.numeric(In.head) > 630 & as.numeric(In.head) <= 660){
    In.head <- "middle (630-660 cm²)"
  } else if (as.numeric(In.head) > 660){
    In.head <- "large (over 660 cm²)"
  } 
  ################
  
  
  # weight recommendation
  wghts.op <<- wghts$Output[which((wghts$Age == In.age) & (wghts$Gender == In.sex) & 
                                    (wghts$Weight == unlist(strsplit(In.weight, " "))[1]) & (wghts$Play.level == In.lev))]
  if(wghts.op == "light"){
    wghts.val <<- mean(Rdb$Weight)-sd(Rdb$Weight)
  } else if(wghts.op == "heavy"){
    wghts.val <<- mean(Rdb$Weight)+sd(Rdb$Weight)
  } else {
    wghts.val <<- mean(Rdb$Weight)
  }
  
  # headsize recommendation
  heads.op <<- heads$Output[which((heads$Play.level == In.lev) & (heads$Common.error == In.err) & 
                                    (heads$Prefered.head.size == unlist(strsplit(In.head, " "))[1]) & (heads$points == In.lev))]
  if(heads.op == "small"){
    heads.val <<- mean(Rdb$Headsize)-sd(Rdb$Headsize)
  } else if(heads.op == "large"){
    heads.val <<- mean(Rdb$Headsize)+sd(Rdb$Headsize)
  } else {
    heads.val <<- mean(Rdb$Headsize)
  }
  
  # stringsize rec ommendation
  string.op <<- string$Output[which((string$Play.style == In.ptype) & (string$Stroke.style == In.stroke) &
                                      (string$Common.error == In.err))]
  
  
  ### calculating average match for the inputs
  ## headsize
  norm.head <<- 1-((abs(Rdb$Headsize-heads.val)/10)^1.5)/10
  ## weight
  norm.weight <<- 1-((abs(Rdb$Weight-wghts.val)/10)^1.5)*0.07
  ## string
  norm.string <<- 1-(abs(Rdb$Mains-string.op))*0.07
  ## average of the three
  mean.match <<- (norm.head+norm.weight+norm.string)/3
  
  # fin.df <<- data.frame("Value" = c(wghts.op, wghts.val, heads.op, heads.val, string.op, sort(mean.match, decreasing = T)[1:5]), 
  #                       row.names = c("Weight category", "Recommended weight", "Headsize category", "Recommended headsize",
  #                                     "Recommended string", Rdb$Model[which(mean.match %in% sort(mean.match, decreasing = T)[1:5])]), 
  #                       stringsAsFactors = F)
  fin.df <<- data.frame("Value" = c(wghts.op, wghts.val, heads.op, heads.val, string.op, sort(mean.match, decreasing = T)), 
                        row.names = c("Weight category", "Recommended weight", "Headsize category", "Recommended headsize",
                                      "Recommended string", Rdb$Model[order(mean.match, decreasing = T)]), 
                        stringsAsFactors = F)
  return(fin.df)
}


body <- dashboardBody(
  h3("Tell me about yourself and I will recommend you some tennis rackets."),
  h4("First tell me your gender:"),
  fluidRow(
    column(12, align="center", offset = 0,
           textInput("inText1", label="",value = "", width = "60%"),
           tags$style(type="text/css", "#inText1 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
    )
  ),
  
  h4(textOutput("outText1")),
  conditionalPanel("(/male/gi.test(input.inText1) | /female/gi.test(input.inText1))", 
                   # conditionalPanel("input.inText1.match(/male/gi) == 'male' | input.inText1.match(/male/gi) == 'Male' | 
                   # input.inText1.match(/male/gi) == 'MALE' ", 
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText2", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText2 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )), 
  
  h4(htmlOutput("outText2")),
  conditionalPanel("!isNaN(parseFloat(input.inText2)) && isFinite(input.inText2 )", 
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText3", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText3 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )), 
  
  h4(htmlOutput("outText3")), 
  #   conditionalPanel("input.inText3 != '' && (input.inText3 == 'hobby'|| input.inText3 == 'LK 1-8' ||
  #                    input.inText3 == 'LK 9-16' || input.inText3 == 'LK 17-23')",
  conditionalPanel("input.inText3 != '' && (/hobby/gi.test(input.inText3) | /lk[ ]*1-8/gi.test(input.inText3) | 
                   /lk[ ]*9-16/gi.test(input.inText3) | /lk[ ]*17-23/gi.test(input.inText3))",
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText4", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText4 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )),
  
  h4(textOutput("outText4")),
  #   conditionalPanel("(input.inText4 == 'defensive'|| input.inText4 == 'aggressive' ||
  #                    input.inText4 == 'allround' || input.inText4.match(/know/gi) == 'know')",
  conditionalPanel("(/defensive/gi.test(input.inText4) | /aggressive/gi.test(input.inText4) | 
                   /allround/gi.test(input.inText4) | /know/gi.test(input.inText4))",
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText5", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText5 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )),
  
  h4(uiOutput("outText5")),
  #   conditionalPanel("(input.inText5 == 'topspin'|| input.inText5 == 'slice' ||
  #                    input.inText5 == 'flat' || input.inText5.match(/know/gi) == 'know')",
  conditionalPanel("(/topspin/gi.test(input.inText5) | /slice/gi.test(input.inText5) | 
                   /flat/gi.test(input.inText5) | /know/gi.test(input.inText5))",
                   
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText6", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText6 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )),
  
  h4(uiOutput("outText6")),
  # conditionalPanel("(input.inText6.match(/out\ of\ bounds|low\ swing|many\ frame\ hits|low\ accuracy/gi) || /know/gi.test(input.inText6))",
  conditionalPanel("( /out[ ]*of[ ]*bounds|low[ ]*swing|many[ ]*frame[ ]*hits|low[ ]*accuracy/gi.test(input.inText6) || /know/gi.test(input.inText6))",
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText7", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText7 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )),
  
  h4(uiOutput("outText7")),
  conditionalPanel("(input.inText7.match(/^\\d+$/) || input.inText7.match(/no/gi) == 'no' || input.inText7.match(/no/gi) == 'NO' || input.inText7.match(/no/gi) == 'No' )",
                   fluidRow(
                     column(12, align="center", offset = 0,
                            textInput("inText8", label="",value = "", width = "60%"),
                            tags$style(type="text/css", "#inText8 { height: 50px; width: 100%; text-align:right; font-size: 30px; display: block;}")
                     )
                   )),
  
  h4(uiOutput("outText8")) 
  
  )


# ui2 <- dashboardPage(title =  "Racket Recommendation", 
#                     dashboardHeader(title = "Racket Recommendation", titleWidth = "100%", disable = F),
#                     dashboardSidebar(disable = T), body)

ui2 <- dashboardPage(dashboardHeader(disable = T), dashboardSidebar(disable = T), body)

