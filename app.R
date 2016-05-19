# Sys.setlocale(, "en_us")
library(shiny)
library(shinydashboard)
library(grDevices)
library(knitr)
source("app_ui.R")
source("gui_dashboard.R")
options(bitmapType='cairo')

Logged <<- FALSE
tex <<- "tex"

# my_username <- c("test", "admin", "root")
# my_password <- c("test", "admin", "root")

# read the user names and password .csv file
credentials <- read.csv(file = "users.csv", stringsAsFactors = F)
my_username <- tolower(credentials$username)
my_password <- credentials$password
#

ui1 <- function(){
  tagList(
    div(id = "logpan",
        fluidRow(column(12, align="center", offset = 2,
                        # wellPanel(style = "background-color: #367fa9;",
                        wellPanel(style = "background-color: #000000;",
                                  textInput("userName", "Username"),
                                  tags$style(type="text/css", "#userName {text-align:left;}"),
                                  passwordInput("passwd", "Password"),
                                  tags$style(type="text/css", "#passwd {text-align:left;}"),
                                  br(),
                                  fluidRow(
                                    column(12, align="center", offset = 0,
                                           actionButton("Login", "Log in"),
                                           # tags$style(type="text/css", "#Login { height: 30px; width: 100%; text-align:center; font-size: 14px; display: block;}")
                                           tags$style(type="text/css", "#Login {width: 50%; text-align:center; font-size: 20px;}")
                                    )
                                  ),
                                  textOutput("loginText"),
                                  tags$style(type="text/css", "#loginText {color: #ffa050; text-align:center; font-size: 18px;}")
                        )))
    ),
    tags$style(type="text/css", 
               "#logpan {font-size:14px;color: #ffffff;text-align:left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}


myHeader <<- dashboardHeader(title = "Racket Recommendation", titleWidth = "100%")
myHeader$children[[2]]$children <<-  tags$a(href='http://www.head.com/de/home',
                                           tags$img(src='http://www.head.com/fileadmin/templates/images/logo.png',
                                                    height='25',width='100'))


# ui = (htmlOutput("page"))
ui = dashboardPage(myHeader,
                   dashboardSidebar(disable = T), 
                   body = dashboardBody({
                     htmlOutput("page")
                   },
                     tags$head(tags$style(HTML('
                                               /* logo */
                                               .skin-blue .main-header .logo {
                                               background-color: #000000;
                                               }
                                               
                                               /* logo when hovered */
                                               .skin-blue .main-header .logo:hover {
                                               background-color: #888888;
                                               }
                                               
                                               /* navbar (rest of the header) */
                                               .skin-blue .main-header .navbar {
                                               background-color: #000001;
                                               }        

                                               .content-wrapper,
                                               .right-side {
                                               background-color: #ffffff;
                                               }
                                               
                                               ')))
                   ))


server = (function(input, output,session) {
  
  USER <<- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <<- isolate(tolower(input$userName))
          Password <<- isolate(input$passwd)
          if(Username %in% my_username){
            uid <- which(my_username == Username)
            if(Password == my_password[uid]){
              USER$Logged <- TRUE
              output$loginText <- renderText({
                print("login success!")
              })
            } else {
              output$loginText <- renderText({
                print("Wrong password")
              })
            }
          } else {
            output$loginText <- renderText({
              print("User not recognized")
            })
          }
        }
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        # div(ui3)
        ui3
      })
    }
  })
  
  #### the app
  output$body1 <- renderUI({ui2})
  
  output$outText1 <- renderText({
    x1 <<- input$inText1
    if(grepl("female", x1, ignore.case = T)){
      "Good. Tell me your age (I won't disclose it!):"
    } else if ((!grepl("female", x1, ignore.case = T)) && (grepl("male", x1, ignore.case = T))){
      "Okay. Tell me your age:"
    } else if(x1 != "") {
      "I didn't quite get that..."
    } else if (x1 == ""){
      x1 <<- "female"
      print("")
    }
  })
  
  output$outText2 <- renderUI({
    x2 <<- reactive({input$inText2})
    if (input$inText2 == ""){
      x2 <<- "19-35"
      HTML(paste(""))
    } else if(!grepl("[[:digit:]]", input$inText2) ){
      HTML(paste("I didn't quite get that..."))
    } else {
      if(as.numeric(input$inText2) <= 18){
        x2 <<- "13-18"
      }
      if(as.numeric(input$inText2) > 18 & as.numeric(input$inText2) <= 35){
        x2 <<- "19-35"
      }
      if(as.numeric(input$inText2) > 36 & as.numeric(input$inText2) <= 60){
        x2 <<- "36-60"
      }
      if(as.numeric(input$inText2) > 60){
        x2 <<- "over 60"
      }
      HTML(paste("How good is your playing experience? You can choose one from these options: ",
                 "beginner, hobby, LK 1-8, LK 9-16, LK 17-23", sep="<br/>"))
    }
  })
  
  output$outText3 <- renderUI({
    x3 <<- input$inText3
    if(grepl("beginner", input$inText3, ignore.case = T)){
      HTML(paste("Move to next step..."))
    } else if (input$inText3 == ""){
      x3 <<- "beginner"
      print("")
      # } else if(input$inText3 %in% c("hobby", "LK 1-8", "LK 9-16", "LK 17-23")){
    } else if(grepl("hobby", input$inText3, ignore.case = T) || grepl("lk[ ]*1-8", input$inText3, ignore.case = T) ||
              grepl("lk[ ]*9-16", input$inText3, ignore.case = T) || grepl("lk[ ]*17-23", input$inText3, ignore.case = T)){
      HTML(paste("What is your play type? e.g. defensive, aggressive or allround?",
                 "P.S. you can say 'I dont know ...'", sep="<br/>"))
    } else {
      HTML(paste("I am afraid I don't understand..."))
    }
  })
  
  output$outText4 <- renderText({
    x4 <<- input$inText4
    if(input$inText4 == ""){
      x4 <<- "aggressive"
      print("")
      # } else if (input$inText4 %in%  c("defensive", "aggressive", "allround")){
    } else if (grepl("defensive", input$inText4, ignore.case = T) || grepl("aggressive", input$inText4, ignore.case = T) ||
               grepl("allround", input$inText4, ignore.case = T)){
      print(paste("Cool! What is your stroke type? e.g. ", "topspin, ",	"slice, ",	"flat"))
    } else if (grepl("know", input$inText4, ignore.case = T)){
      x4 <<- "aggressive"
      print(paste("No problem. What is your stroke type? e.g. ", "topspin, ",	"slice, ",	"flat"))
    } else {
      print("I am afraid I don't understand...")
    }
  })
  
  
  output$outText5 <- renderUI({
    x5 <<- input$inText5
    if(input$inText5 == ""){
      x5 <<- "topspin"
      HTML(paste(""))
      # } else if (input$inText5 %in% c("topspin",	"slice",	"flat")){
    } else if (grepl("topspin|slice|flat", input$inText5, ignore.case = T)){
      HTML(paste("Good. Do you know what is our common error?",
                 "e.g. too many balls out of bounds",	"too low swing speed",
                 "too many frame hits", "too low accuracy", sep = "<br/>"))
    } else if (grepl("know", input$inText5, ignore.case = T)){
      x5 <<- "topspin"
      HTML(paste("Okay. Do you know what is our common error?",
                 "e.g. too many balls out of bounds",	"too low swing speed",
                 "too many frame hits", "too low accuracy", sep = "<br/>"))
    } else {
      HTML(paste("No idea what you are saying about."))
    }
  })
  
  output$outText6 <- renderUI({
    x6 <<- input$inText6
    if(x6 == ""){
      x6 <<- "too low accuracy"
      HTML(paste(""))
    } else if (grepl("out[ ]*of[ ]*bounds|low[ ]*swing|many[ ]*frame[ ]*hits|low[ ]*accuracy", x6, ignore.case = T)){
      HTML(paste("Good. Do you have any preference for the racket's weight?",
                 "You can enter a value (in gms) or say 'No'", sep = "<br/>"))
    } else if (grepl("know", x6, ignore.case = T)){
      x6 <<- "too low accuracy"
      HTML(paste("Okay. Do you have any preference for the racket's weight?",
                 "You can enter a value (in gms) or say 'No'", sep = "<br/>"))
    } else {
      HTML(paste("This is not a common error, all I know."))
    }
  })
  
  output$outText7 <- renderUI({
    x7 <<- input$inText7
    if(input$inText7 == ""){
      x7 <<- "middle (260-300g)"
      HTML(paste(""))
    } else if (is.na(as.numeric(input$inText7))){
      if (grepl("no|No|NO|nO", input$inText7, ignore.case = T)){
        x7 <<- "middle (260-300g)"
        HTML(paste("Okay. Do you have any preference for the racket's head-size?",
                   "You can enter a value (in cm²) or say 'No'.", sep = "<br/>"))
      } else {
        HTML(paste("I dont understand.", sep = "<br/>"))
      }
    } else if (as.numeric(input$inText7) <= 260){
      x7 <<- "light (under 260g)"#,	"middle (260-300g)", "heavy (over 300g)"
      HTML(paste("clust1. Do you have any preference for the racket's head-size?",
                 "You can enter a value (in cm²) or say 'No'.", sep = "<br/>"))
    } else if (as.numeric(input$inText7) > 260 & as.numeric(x7) <= 300){
      x7 <<- "middle (260-300g)"
      HTML(paste("clust2. Do you have any preference for the racket's head-size?",
                 "You can enter a value (in cm²) or say 'No'.", sep = "<br/>"))
    } else if (as.numeric(input$inText7) > 300){
      x7 <<- "heavy (over 300g)"
      HTML(paste("clust3. Do you have any preference for the racket's head-size?",
                 "You can enter a value (in cm²) or say 'No'.", sep = "<br/>"))
    } else {
      HTML(paste(""))
    }
  })
  
  output$outText8 <- renderUI({
    x8 <<- input$inText8
    if(input$inText8 == ""){
      x8 <<- "middle (630-660 cm²)"
      HTML(paste(""))
    } else if (is.na(as.numeric(input$inText8))){
      if (grepl("no|No|NO|nO", input$inText8, ignore.case = T)){
        x8 <<- "middle (630-660 cm²)"
        HTML(paste("Okay. Let's goto step-2.", sep = "<br/>"))
      } else {
        HTML(paste("I dont understand.", sep = "<br/>"))
      }
    } else if (as.numeric(input$inText8) <= 630){
      x8 <<- "small (under 630 cm²)"# , "middle (630-660 cm²)", "large (over 660 cm²)"
      HTML(paste("clust1. Let's goto step-2.", sep = "<br/>"))
    } else if (as.numeric(input$inText8) > 630 & as.numeric(input$inText8) <= 660){
      x8 <<- "middle (630-660 cm²)"
      HTML(paste("clust2. Let's goto step-2.", sep = "<br/>"))
    } else if (as.numeric(input$inText8) > 660){
      x8 <<- "large (over 660 cm²)"
      HTML(paste("clust3. Let's goto step-2.", sep = "<br/>"))
    } else {
      HTML(paste(""))
    }
  })
  
  output$match <- DT::renderDataTable({
    
    df1 <<- head(getFinDf(In.age = input$inText2,
                          In.sex = input$inText1,
                          In.weight = input$inText7,
                          In.lev = input$inText3,
                          In.ptype = input$inText4,
                          In.err = input$inText6,
                          In.head = input$inText8,
                          In.stroke = input$inText5), 5)
    df1 <<- data.frame(t(df1), stringsAsFactors = F)
    row.names(df1) <- NULL
    df1
    
  },
  options = list(pageLength = 1, lengthChange = FALSE, searching = F, paging = F, info = F))
  
  
  output$match_2 <- DT::renderDataTable({
    df2 <- tail(getFinDf(In.age = input$inText2,
                         In.sex = input$inText1,
                         In.weight = input$inText7,
                         In.lev = input$inText3,
                         In.ptype = input$inText4,
                         In.err = input$inText6,
                         In.head = input$inText8,
                         In.stroke = input$inText5), 71)
    
    df2[,1] <- round(as.numeric(df2[,1])*100, digits = 2)
    df2[,2] <- df2[,1]
    df2[,1] <- rownames(df2)
    names(df2) <- c("Racket", "% Match")
    row.names(df2) <- NULL # only for the DT package
    DF2 <<- df2
    df2
  },
  options = list(pageLength = 5, lengthChange = FALSE, processing = FALSE)
  )
  
  
  output$plt <- renderImage({
    s1 <- input$match_2_rows_current  # rows on the current page
    s2 <<- input$match_2_rows_selected
    # s3 <- input$match_2_rows_all      # rows on all pages (after being filtered)
    s3 <- 1:nrow(DF2) # select all rows
    
    if(is.null(s2)){
      s2 <<- s1
    }
    
    ### save the user profile
    # info from the sessionData
    cdata <<- session$clientData
    usr.tkn <<- session$token
    #
    usr.df <<- data.frame(time_stamp = Sys.time(),
                          Usr = Username,
                          In.age = input$inText2,
                          In.sex = input$inText1,
                          In.weight = input$inText7,
                          In.lev = input$inText3,
                          In.ptype = input$inText4,
                          In.err = input$inText6,
                          In.head = input$inText8,
                          In.stroke = input$inText5,
                          usr.token = usr.tkn,
                          sel.sugg = paste(s2, collapse = ","))
    
    # create "user_profiles" named directory if doesn't exist already
    if(!dir.exists("user_profiles")){
      dir.create("user_profiles")
    }
    # create the user specific .csv file to store records
    oldwd <<- getwd()
    setwd("user_profiles")
    usr.file <- paste0(Username, ".csv")
    if(!file.exists(usr.file)){
      write.table(usr.df, file = usr.file, append = F, row.names = F, sep = ",", dec = ".")
      setwd("..")
    } else {
      write.table(usr.df, file = usr.file, append = T, col.names = F, row.names = F, sep = ",", dec = ".")
      setwd("..")
    }
    ###
    
    #### sort norm.head and the others by rank of match
    norm.head <- norm.head[order(mean.match, decreasing = T)]
    norm.weight <- norm.weight[order(mean.match, decreasing = T)]
    norm.string <- norm.string[order(mean.match, decreasing = T)]
    
    #### gg section
    theDF <- data.frame(DF2[s2,], norm.head[as.numeric(s2)]*100,
                        norm.string[as.numeric(s2)]*100, norm.weight[as.numeric(s2)]*100)
    # theDF[5,] <- c(NA, 1:4)
    names(theDF)[2:5] <- c("match", "head", "string", "weight")
    the.DF <- data.frame("Racket" = rep(NA, nrow(theDF)*3),
                         "Match" = rep(NA, nrow(theDF)*3),
                         "yAxis" = rep(NA, nrow(theDF)*3),
                         "fac" = rep(NA, nrow(theDF)*3))
    the.DF$Racket <- rep(theDF$Racket, 3)
    the.DF$Match <- rep(theDF$match, 3)
    the.DF$yAxis <- c(theDF$head, theDF$string, theDF$weight)
    the.DF$fac <- rep(names(theDF)[3:5], each = nrow(theDF))
    
    if(nrow(the.DF) != 0){
      # the.DF[1:3,] <- rep(-100, 3)
      the.DF$fac <- names(theDF)[3:5]
      the.DF$Match <- c(0,50,100)
      # gg <- qplot(x = Match, y = yAxis, data = the.DF) + facet_grid(~ fac) +
      #   ylim(c(0,100)) + ylab("") + xlab("Average match")
      # gg
      
      ### the good plot similar to trade-off analytics
      
      ## old method
      # var1 <- the.DF$yAxis[1:(nrow(the.DF)/3)]
      # var2 <- the.DF$yAxis[(nrow(the.DF)/3 + 1):(nrow(the.DF)*2/3)]
      # var3 <- the.DF$yAxis[(nrow(the.DF)*2/3 + 1):(nrow(the.DF))]
      # 
      # var2 <- 100-var2
      # 
      # x.ax <- (var2+var3)/2
      # y.ax <- (x.ax-50+var1)/2
      ##
      
      ## correction
      data.points <<- data.frame(matrix(unlist(lapply(as.numeric(s2), 
                                                      function(ii){
                                                        ax1 <- norm.head[ii]
                                                        ax2 <- norm.weight[ii]
                                                        ax3 <- norm.string[ii]
                                                        
                                                        x.dat <- c(ax1*0.5, 1-(ax2*0.5), 0.5)
                                                        # x.axis <- c(x.axis, x.dat)
                                                        y.dat <- c(ax1*0.5, ax2*0.5, 1-(ax3*0.5))
                                                        # y.axis <- c(y.axis, y.dat)
                                                        
                                                        return(c(sum(x.dat)/3, sum(y.dat)/3))
                                                      })), ncol = 2, byrow = T))
      x.ax <- data.points[,1]*100
      y.ax <- data.points[,2]*100
      ##
      
      # add the perfact match point to the plot
      x.ax <- c(x.ax, 50)
      y.ax <- c(y.ax, 33.33)
      
      # for triangle
      tri.df <- data.frame("xx" = c(0,50,100), "yy" = c(0,100,0)) 
      tri.df2 <- data.frame("xx" = c(0,50,50,100), "yy" = c(0,100,33.33,0))
      tribase.df <- data.frame("x2" = c(0:100), "y2" = rep(0,101))
      tritext.df <- data.frame("xx" = c(-13,50,113), "yy" = c(0,113,0))
      #
      
      # Selected_Rackets <<- the.DF$Racket[1:(nrow(the.DF)/3)]
      Selected_Rackets <<- c(the.DF$Racket[1:(nrow(the.DF)/3)], "Ideal Match")
      # print(Selected_Rackets)
      gg3 <<- ggplot() + 
        geom_line(data = tri.df, mapping = aes(x = xx, y = yy), color = "lightblue", size = 2) +
        geom_line(data = tribase.df, mapping = aes(x = x2, y = y2), color = "lightblue", size = 2) +
        geom_point(data = tri.df2, mapping = aes(x = xx, y = yy), colour = "blue", shape=21,
                   size = 7, fill = "white", stroke = 2) +
        geom_text(data = tritext.df, mapping = aes(x = xx, y = yy, label = c("Head", "String", "Weight" )),
                  colour = "black", size = 5) +
        geom_point(data = data.frame(x.ax, y.ax), 
                   aes(x = x.ax, y = y.ax, colour = Selected_Rackets), 
                   size = 5) +
        xlim(-15,115) + ylim(0,115) + theme_bw() +
        theme(axis.line = element_blank(), #element_line(colour = "black"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank() )+ #, panel.background = element_blank()) +
        xlab("") + ylab("") +
        theme(
#           panel.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5"), # or theme_blank() for transperency
#           panel.grid.minor = element_blank(), 
#           panel.grid.major = element_blank(),
#           plot.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5"),
#           legend.background = element_rect(fill = "#ecf0f5",colour = "#ecf0f5")
          panel.background = element_rect(fill = "#ffffff",colour = "#ffffff"), # or theme_blank() for transperency
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "#ffffff",colour = "#ffffff"),
          legend.background = element_rect(fill = "#ffffff",colour = "#ffffff")
        ) # +
      # geom_point(data = data.frame(xp = 50, yp = 50), mapping = aes(x = xp, y = yp, colour = "black", size = 5))
      # print(gg3)
      
      wid <<- session$clientData$output_plt_width
      hgt <<- session$clientData$output_plt_width/1.4
      
      wid2 <<- wid/100
      hgt2 <<- hgt/1.5/100
      
      # filename_png <- tempfile(fileext='.png')
      # ggsave(filename=filename_png, height = hgt/100, width = wid/100, gg3)
      
      filename_png <- tempfile(fileext='.png')
      # print(filename_png)
      ggsave(filename=filename_png, height = hgt/1.5/100, width = wid/100,
             units = "in", type="cairo", gg3, limitsize = F, dpi = 300)
      
      # ggsave(filename=filename_png, height=session$clientData$output_plt_width*0.5, 
      #        width=session$clientData$output_plt_width, type="cairo", gg3)
      
      # ggsave(filename=filename_png, height = hgt/100, width = wid/100, type="cairo", gg3)
      
      # # check the device capabilities
      # print(capabilities())
      
      # Return a list containing the filename
      list(src = filename_png, # filename_png,
           width = wid,
           height = hgt/(2/1.4),
           alt = "Bluemix has some issues with cairo or X-server :(")
      
      ###
    } 
    ####
    
    # }, height = function() {
    #   session$clientData$output_plt_width*0.5
    # }, deleteFile = FALSE)
    
  }, deleteFile = FALSE)
  
  
  # the report
  output$report <- downloadHandler(
    # filename = paste0("HEAD_racket_recommendation_", Username, ".pdf"),
    filename = paste0("HEAD_racket_recommendation_", Username, ".htm"),
    
    content = function(file) {
      # Logged <<- TRUE
      # Sweave2knitr('input.Rnw')
      # out = knit2pdf('input-knitr.Rnw', clean = TRUE)
      out = rmarkdown::render(input = 'input.Rmd', output_format = "html_document")
      file.rename(out, file) # move pdf to file for downloading
    }# ,
    
    # contentType = 'application/pdf'
  )
  #### the app finished
})

# runApp(list(ui = ui, server = server), launch.browser = T)
shinyApp(ui = ui, server = server)
