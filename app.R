library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)

#load data
#setwd('C:/Users/jpueyo/OneDrive - ICRA/Edicitnet_JPR/NBS_scores/shiny')
scores_df <- read.csv2('data/NBS_scores.csv', encoding = 'UTF-8')
descr_df <- read.csv2('data/NBS_descr.csv', encoding = 'UTF-8')

#scores_df$edicitnet_code == descr_df$X.U.FEFF.edicitnet_code

#as.character(descr_df$NBS) == as.character(scores_df$NBS)
scores_df$NBS <- descr_df$NBS

#plot function
grafic <- function(data, xlab, color= "dark green"){
  ggplot(na.omit(data), aes(x=Factor,y=Performance))+
    geom_col(fill=color)+
    coord_polar()+theme_minimal()+
    theme(plot.title=element_text(size=12, hjust=0.5),
          axis.ticks.y=element_blank(), axis.text.y=element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14, face="bold"),
          plot.margin = margin(0,0,0,0, "cm"))+
    xlab(xlab)+ylab("")+
    scale_y_continuous(limits=c(0,1))#+
    #geom_text_repel(aes(label = round(Performance,2)), bg.color = "white",
                    #bg.r = 0.05)
}

#Prepare data for UC plot

colnames(scores_df)[17:26] <- c("Climate\nResilience", 
                                "Water\nManagement","Coastal\nResilience",
                                "Green Space\nManagement", "Air\nQuality",
                                "Urban\nRegeneration", "Public\nParticipation",
                                "Social\nJustice","Public\nHealth", 
                                "Economic\nOpportunities")

#Prepare data for catES plot

colnames(scores_df)[27:30] <- c("Regulation", "Cultural","Support", "Provision")

#Prepara data for ES

colnames(scores_df)[31:49] <- c("Climate\nregulation",
                                "Water\nregulation",
                                 "Water\npurification\nand waste treatment",
                                 "Air quality\nregulation",
                                 "Pollination", 
                                 "Disease and pest\nregulation",
                                 "Erosion\nregulation",
                                 "Aesthetic\nvalues",
                                 "Recreation\nand ecotourism",
                                 "Spiritual and\nreligious values",
                                 "Social\nrelations",
                                 "Habitat\nfor species", 
                                 "Soil\nformation",
                                 "Nutrient\ncycling", 
                                 "Primary\nproduction",
                                 "Food and\nfibers",
                                 "Fuel",
                                 "Biochemical,\nnatural medicines\nand pharmaceuticals",
                                 "Fresh\nwater")

#prepare NBS labels for custom plot
scores_df$NBS_labels <- gsub(" ", "\n", scores_df$NBS)

#NBS list
NBS_list <- as.vector(scores_df$NBS)

#Methodology text
methodology <- paste("<p><B><center>Methodology</B></center></p>",
               '<p style="text-align:justify">The information displayed is the result of a participative
              method that crossed information from four NBS-based projects:
              <a href="https://www.urbangreenup.eu"; target="_blank">UrbanGreenUp</a>, 
              <a href="https://unalab.eu"; target="_blank">UNaLAB</a>, 
              <a href="https://www.nature4cities.eu"; target="_blank">Nature4Cities</a> 
              and <a href="https://www.think-nature.eu"; target="_blank">ThinkNature</a>. 
              In total, more than 250 NBS were reviewed and summed up in this 
              comprehensive list of 32 NBS.</p>',
                             "<p>The performance assessment also came from the assessment of
              the projects mentioned above. </p>",
                             '<br><p><center>Click <a href="https://doi.org/10.1016/j.scitotenv.2021.146237">here</a> for more information about methodology and the source citation</center></p>')

###USER INTERFACE
ui <- fluidPage(
  titlePanel("Nature-based solutions performance assessment"),
  
  sidebarLayout(
    sidebarPanel(
    radioButtons(inputId = "selection",
                 label = "Select what you want to do",
                 choices = c("Explore the facets of an NBS" = "NBS_selec",
                             "Compare a facet among different NBS" = "facet_selec")
                 ),
    
    #if NBS_selec
    conditionalPanel(condition = "input.selection.includes('NBS_selec')",
      selectInput(inputId = "NBS", 
                  label = "Select one NBS",
                  choices = c(" ", as.character(NBS_list)),
                  multiple = FALSE,
                  width = '100%',
                  selectize = TRUE,
                  #size=32,
                  selected = " "),
      checkboxGroupInput(inputId = "info",
                         label = "Select the information you want to visualize",
                         choices = c("Description of NBS" = "descr",
                                     "Urban Challenges" = "UC",
                                     "Categories of ecosystem services" = "catES",
                                     "Regulating services" = "regES",
                                     "Cultural services" = "cultES",
                                     "Supporting services" = "suppES",
                                     "Provisioning services" = "provES",
                                     "Custom selection" = "custom"),
                         selected = 'descr'
                         ),
      #Custom selection
      conditionalPanel(condition = "input.info.includes('custom')",
                       selectizeInput(inputId = "custom_sel_UC",
                                   label = "Select the Urban challenges you want to visualize (or type a keyword)",
                                   choices = colnames(scores_df[17:26]),
                                   multiple = TRUE),
                       
                       selectizeInput(inputId = "custom_sel_ES",
                                      label = "Select the Ecosystem services you want to visualize (or type a keyword)",
                                      choices = colnames(scores_df[27:49]),
                                      multiple = TRUE)
      )
    ), #close conditional panel (NBS_select)

    #if facet_selec
    conditionalPanel(condition = "input.selection.includes('facet_selec')",

        radioButtons(inputId = "uc_es",
                     label = "Select the kind of facet",
                     choices = c("Urban Challenges" = "UC_facet",
                                 "Ecosystem Services" = "ES_facet")),
            
        conditionalPanel(condition = "input.uc_es.includes('UC_facet')",
                     selectizeInput(inputId = "UC_facet_selec",
                                    label = "Select the Urban challenge you want to visualize",
                                    choices = c(" ", colnames(scores_df[17:26])),
                                    multiple = FALSE,
                                    selected = " "), 
            ),
        conditionalPanel(condition = "input.uc_es.includes('ES_facet')",
                       selectizeInput(inputId = "ES_facet_selec",
                                      label = "Select the Ecosystem service you want to visualize",
                                      choices = c(" ", colnames(scores_df[27:49])),
                                      multiple = FALSE,
                                      selected = " "), 
            ),
        radioButtons(inputId = "top_custom",
                     label = "Select an option",
                     choices = c("The best NBS in a facet" = "top_NBS",
                                 "Custom selection" = "custom_NBS")
        ),
        
        conditionalPanel(condition = "input.top_custom.includes('top_NBS')",
                         
                         uiOutput("slider_ui"),
                         HTML('<p style="font-size:13px">The minimum is set 
                              in line with number of NBS with 
                              maximum value in the selected facet</p>')
                         ),
        
        conditionalPanel(condition = "input.top_custom.includes('custom_NBS')",
        
            selectizeInput(inputId = "facet_NBS", 
                        label = "Select the NBS to compare 
                        (or type a keyword)",
                        choices = c(" ", as.character(NBS_list)),
                        multiple = TRUE,
                        width = '100%',
                        selected = " ")
        )
    ),
    
    HTML('<p><br></p>'),
    img(src="EC.jpg", width = '15%'),
    HTML('<p></p>
           <p style="font-size:13px">EdiCitNet is funded by European Union H2020 I&A n. 776665</p>'),
    width = 3
    
    ), #close side bar panel
    

    #main panel    
    mainPanel(
      HTML('<p style="text-align:right;vertical-align:text-bottom">
          <a href="https://www.edicitnet.com"; target="_blank">
          <img src="edicitnet-logo.png", height = 80></a>&nbsp;&nbsp;&nbsp;&nbsp;
          <a href="http://icra.cat/index.php?lang=3"; target="_blank">
          <img src="icra.jpg", height = 80></a>&nbsp;&nbsp;&nbsp;&nbsp;
          <a href="https://cerca.cat/en"; target="_blank">
          <img src="cerca.jpg", height = 40></a></p><p><br></p>
          <hr style="width:100%;text-align:left;margin-left:0">'),

      
      #if NBS select
      conditionalPanel(condition = "input.selection.includes('NBS_selec')",
        
        htmlOutput("NBS_selected"),
      
        conditionalPanel(condition = "input.info.includes('UC')",
                   plotOutput("plot_UC", height = "500px")),
        
        conditionalPanel(condition = "input.info.includes('catES')",
                        plotOutput("plot_catES", height = "500px")),
        
        conditionalPanel(condition = "input.info.includes('regES')",
                        plotOutput("plot_regES", height = "500px")),
        
        conditionalPanel(condition = "input.info.includes('cultES')",
                        plotOutput("plot_cultES", height = "500px")),
        
        conditionalPanel(condition = "input.info.includes('suppES')",
                        plotOutput("plot_suppES", height = "500px")),
        
        conditionalPanel(condition = "input.info.includes('provES')",
                        plotOutput("plot_provES", height = "500px")),
        
        conditionalPanel(condition = "input.info.includes('custom')",
                        plotOutput("plot_custom", height = "500px"),
                        HTML('<p><br></p><p style="text-align:center;font-size:12px; font-style:italic">
                        *Some selected facets might not be visualized
                         if the selected NBS does not have a performance on that 
                             facet</p>')       
                         )
      ), #close conditional panel NBS_selec
      
      #if facet selected
      conditionalPanel(condition = "input.selection.includes('facet_selec')",
        
          conditionalPanel(condition = "input.uc_es.includes('UC_facet')",
                           htmlOutput("text_UC_facet")
          ),
          conditionalPanel(condition = "input.uc_es.includes('ES_facet')",
                           htmlOutput("text_ES_facet")
          ),
          
          #if top NBS is selected
          conditionalPanel(condition = "input.top_custom.includes('top_NBS')",
                           plotOutput("plot_top", height = "500px")
          ),

          #if custom NBS is selected
          conditionalPanel(condition = "input.top_custom.includes('custom_NBS')",
          plotOutput("plot_facet", height = "500px")
          )
      ) #close conditional panel facet_selec
      
  ) #close main panel
  ) #close sidebarlayout
) #close fluid page



###DEFINE SERVER LOGIC
server <- function(input, output) {
  
  
  #define the description field in descr df
    output$NBS_selected <- renderText({
      if (input$NBS == " "){
        methodology
      }else{
        if ('descr' %in% input$info){
          description <- descr_df$DESCRIPTION[descr_df$NBS == input$NBS]
          other_names <- descr_df$OTHER.NAMES[descr_df$NBS == input$NBS]
          paste('<p><B><center>',input$NBS,'</B></center></p> ',
                '<p style="text-align:justify">',description,'</p>',
                '<p style="text-align:justifiy"><B>Other names: </B>',
                other_names,'</p>', sep="")
        } else{paste('<p><center><B>',
                     input$NBS,'</B></center></p>')}
      }
  })

    #plot UC
    output$plot_UC <- renderPlot({
      (if ('UC' %in% input$info){
          x_position <- match(input$NBS, NBS_list)
          UC_vector <- colnames(scores_df)[17:26]
          Value_UC <- t(scores_df[x_position,17:26])
          UC_df <- data.frame(Factor = UC_vector, Performance = Value_UC[,1])
           
          #grafic traient els NA
          grafic(data=UC_df, xlab = "Urban challenges")
      })
    })
    #plot categories of ES
    output$plot_catES <- renderPlot({
      (if ('catES' %in% input$info){
        x_position <- match(input$NBS, NBS_list)
        catES_vector <- colnames(scores_df)[27:30]
        Value_catES <- t(scores_df[x_position,27:30])
        catES_df <- data.frame(Factor = catES_vector, Performance = Value_catES[,1])
        
        #grafic traient els NA
        grafic(data=catES_df, xlab = "Categories of Ecosystem Services")
      })
    })
    
    #plot regES
    output$plot_regES <- renderPlot({
      (if ('regES' %in% input$info){
        x_position <- match(input$NBS, NBS_list)
        regES_vector <- colnames(scores_df)[31:37]
        Value_regES <- t(scores_df[x_position,31:37])
        regES_df <- data.frame(Factor = regES_vector, Performance = Value_regES[,1])
        
        #grafic traient els NA
        grafic(data=regES_df, xlab = "Regulating Services")
      })
    })
    
    #plot cultES
    output$plot_cultES <- renderPlot({
      (if ('cultES' %in% input$info){
        x_position <- match(input$NBS, NBS_list)
        cultES_vector <- colnames(scores_df)[38:41]
        Value_cultES <- t(scores_df[x_position,38:41])
        cultES_df <- data.frame(Factor = cultES_vector, Performance = Value_cultES[,1])
        
        #grafic traient els NA
        grafic(data=cultES_df, xlab = "Cultural Services")
      })
    })
    
    #plot suppES
    output$plot_suppES <- renderPlot({
      (if ('suppES' %in% input$info){
        x_position <- match(input$NBS, NBS_list)
        suppES_vector <- colnames(scores_df)[42:45]
        Value_suppES <- t(scores_df[x_position,42:45])
        suppES_df <- data.frame(Factor = suppES_vector, Performance = Value_suppES[,1])
        
        #grafic traient els NA
        grafic(data=suppES_df, xlab = "Supporting Services")
      })
    })
    
    #plot provES
    output$plot_provES <- renderPlot({
      (if ('provES' %in% input$info){
        x_position <- match(input$NBS, NBS_list)
        provES_vector <- colnames(scores_df)[46:49]
        Value_provES <- t(scores_df[x_position,46:49])
        provES_df <- data.frame(Factor = provES_vector, Performance = Value_provES[,1])
        
        #grafic traient els NA
        grafic(data=provES_df, xlab = "Provisioning Services")
      })
    })
    
    #plot custom selection
    output$plot_custom <- renderPlot({
      (if ('custom' %in% input$info){
        scores_custom <- select(scores_df, input$custom_sel_UC,input$custom_sel_ES)
        x_position <- match(input$NBS, NBS_list)
        custom_vector <- colnames(scores_custom)
        Value_custom <- t(scores_custom[x_position,])
        custom_df <- data.frame(Factor = custom_vector, Performance = Value_custom[,1])
        
        #grafic traient els NA
        grafic(data=custom_df, xlab = "Custom selection")
      })
    })

    
    #output UC_facet selection
    output$text_UC_facet <- renderText({
      if (input$UC_facet_selec == " " & input$ES_facet_selec ==" "){
        methodology
      }else{paste('<p><center><B>',input$UC_facet_selec,'</B></center></p>')}
      })

    output$text_ES_facet <- renderText({
      if (input$UC_facet_selec == " "& input$ES_facet_selec ==" "){
        methodology
      }else{paste('<p><center><B>',input$ES_facet_selec,'</B></center></p>')}
    })
    
    #slider render
    output$slider_ui <- renderUI({
      if (input$uc_es == "UC_facet"){facet_sel <- input$UC_facet_selec}
      if (input$uc_es == "ES_facet"){facet_sel <- input$ES_facet_selec}
      if (facet_sel !=" "){
        facet <- select(scores_df, NBS, facet_sel, NBS_labels)
        facet <- na.omit(facet)
        min_input <-max(nrow(filter(facet, facet[,2] == max(facet[,2]))),2) 
        #NBS_top <- tail(facet[order(facet[,2]),],input$slider)

      }else{min_input <- 1}
      sliderInput(inputId = "slider",
                  label = "Select the number of NBS to visualize",
                  min = min_input,
                  max = (min_input+9),
                  value = min_input,
                  step = 1
      )
    }) # close slider_ui

    #plot facet top NBS
    output$plot_top <- renderPlot({
      
      (if (input$top_custom == "top_NBS"){
        if (input$uc_es == "UC_facet"){facet_sel <- input$UC_facet_selec}
        if (input$uc_es == "ES_facet"){facet_sel <- input$ES_facet_selec}
        if (facet_sel !=" "){
          facet <- select(scores_df, NBS, facet_sel, NBS_labels)
          facet <- na.omit(facet)
          NBS_top <- tail(facet[order(facet[,2]),],input$slider)
          colnames(NBS_top)[2]<- "Facet"
  
          ggplot(na.omit(NBS_top), aes(x=NBS_labels, y=Facet))+
            geom_col(fill="Orange")+
            coord_polar()+theme_minimal()+
            theme(plot.title=element_text(hjust=0.5, size=20),
                  axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                  axis.text.x =element_text(size=12),
                  axis.title.x = element_text(size=14, face="bold"),
                  plot.margin = margin(0,0,0,0, "cm"))+
            xlab(paste("Top ",input$slider, " NBS"))+ylab("")+
            scale_y_continuous(limits=c(0,1))
        }
      })
    }) #Close plot_top
    

        #plot facet custom NBS
        output$plot_facet <- renderPlot({

        (if (input$top_custom == "custom_NBS"){
          if (input$uc_es == "UC_facet"){facet_sel <- input$UC_facet_selec}
          if (input$uc_es == "ES_facet"){facet_sel <- input$ES_facet_selec}
          if (facet_sel !=" "){
            facet <- select(scores_df, NBS, facet_sel, NBS_labels)
            NBS_facet <- filter(facet, NBS %in% input$facet_NBS)
            colnames(NBS_facet)[2]<- "Facet"
  
            ggplot(na.omit(NBS_facet), aes(x=NBS_labels, y=Facet))+
              geom_col(fill="Orange")+
              coord_polar()+theme_minimal()+
              theme(plot.title=element_text(hjust=0.5, size=20),
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    axis.text.x = element_text(size=12),
                    axis.title.x = element_text(size=14, face="bold"),
                    plot.margin = margin(0,0,0,0, "cm"))+
              xlab("Custom selection")+ylab("")+
              scale_y_continuous(limits=c(0,1))
          }
        })
    })
}



#Falta posar \n als noms de les NBS dins de plot facet_selection

###RUN THE APP
shinyApp(ui = ui, server = server)


#install.packages("testthat")                  
