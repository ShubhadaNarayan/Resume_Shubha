library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library("readxl")
library(shinyjs)
library(shinyWidgets)
library(magrittr)
library(tidyr)
library(shinythemes)
library(ggfortify)

df<- data.frame(Skills=c("Python","R","R shiny","Bash","MySQL","Advanced Excel"),Score=c(2,2,1.5,0.5,2,1),level=c("2yr","2yr","1.5yr","0.5yr","2yr","1yr"))
df<- df[order(-df$Score),]
df2<- data.frame(Skills=c("Data Visualization","Data Analysis","Bioinformatics","NGS","Machine Learning"),Score=c(90,90,90,80,70),level=c("2yr","2yr","1yr","0.5yr","1yr"))
df2<- df2[order(-df2$Score),]

#Website1<- paste0("<a href='","mastersprojectapp.shinyapps.io/blast_app/","' target='_blank'>","mastersprojectapp.shinyapps.io/blast_app/","</a>")
#Website2<- paste0("<a href='","https://www.linkedin.com/in/shubhada-narayan-97227b19/","' target='_blank'>","https://www.linkedin.com/in/shubhada-narayan-97227b19/","</a>")


b64 <- base64enc::dataURI(file="/Users/shubha/Downloads/IMG_0028_2.png", mime="image/png")

title1<- tags$a(tags$img(src=b64,height= "330px", width="330px",style="position:absolute:right:500px"))#style="position:relative;top:-20px;left:-20%;"))
ui<-fluidPage(
  navbarPage(title=shiny::span(tags$u('RShiny Resume of Shubhada Narayan',style = "color:purple; font-size: 25px;font-weight: bold;position:relative;right:-500px;top:-5%")),
             id="selector",
             setBackgroundColor(
               color = c("#FFFFFF")
             ),
             tags$head(
               tags$style(HTML(' .navbar {
                          height: 84px;
                          min-height:25px !important;
               }
               .navbar-nav > li > a, .navbar-brand {
                            padding-top:30px !important; 
                            padding-bottom:1px !important;
                            height: 84px;
                            }'))),
             tags$head(tags$style(HTML('.navbar-static-top {background-color: #FAC898;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: #FAC898;}'))),
             tags$head(tags$style("#PlotTitle{color: purple;
                                 font-size: 20px;
                                 font-weight: bold;text-align:center;
                                 }"
                                  )
             ),
             tags$head(tags$style("#PlotTitle1{color: purple;
                                 font-size: 20px;
                                 font-weight: bold;text-align:center;}")),
             #splitLayout(style = "border: 1px solid silver:;position:relative;left: 70px;", cellWidths = c(750,450),
                    #     wellPanel(span(uiOutput("about_me"),style="color:purple")),
                      #   shinydashboard::box(
                       #    title1)
                       #  ),
            # splitLayout(style="border: 1px solid silver:;position:relative;left: 70px;",cellWidths = c(750),
                       #  wellPanel(span(uiOutput("Experience_me"),style="color:purple"))
                        # )
            fluidRow(
              column(width=5,
                tags$div(wellPanel(style="background: #FAC898",
                  width = 10,
                  span(uiOutput("about_me"),style="color:purple"))),
                tags$br(),
                tags$div(wellPanel(style="background:#FAC898",span(uiOutput("Experience_me"),style="color:purple"),
                tags$br(),
                span(uiOutput("Academic_Proj"),style="color:purple")
                ))),
                #tags$div(wellPanel(id="textarea",
                #width = 10,
                 #span(uiOutput("Experience_me"),style="color:purple")))
                #),
              column(3,
                     textOutput("PlotTitle"),
                    plotlyOutput("Skills_me",height="350px",width="300px"),
                     tags$br(),
                     textOutput("PlotTitle1"),
                     plotlyOutput("Skills_me1",height="350px",width="300px")
                       #span(uiOutput("Skills_me"),style="color:purple")))
              ),
              column(4,
                wellPanel(style="background:#FAC898",
                  column(3,
                         title1
                ),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                #df_descol$Website<- paste0("<a href='",df_descol$Website,"' target='_blank'>",df_descol$Website,"</a>")
                div(class="social",
                 #span(Website1,icon("th",'fa-2x'),style = "color: #2c3e50;position:relative;right:-180px;"),
                  a(href="https://www.linkedin.com/in/shubhada-narayan-97227b19/",icon('linkedin','fa-2x'),style = "color: #2c3e50;position:relative;right:-180px;"),
                 a(href="https://github.com/shubha410",icon("fab fa-github","fa-2x"), style = "color: #2c3e50;position:relative;right:-180px;")
                    ),
                span(uiOutput("Edu_me"),style="color:purple"),
                span(uiOutput("publication_me"),style="color:purple"),
                span(uiOutput("Certificate"),style="color:purple"),
                span(uiOutput("Langauges"),style="color:purple")
              )
              )
            ),
            
          #fluidRow(
              #column(
               # 3,
               # tags$div(
                # wellPanel(
                 #   width=10,
                  #  span(uiOutput("Experience_me"),style="color:purple; font size:10px;padding:0px 0px;margin-top=:-2em"))
                 # )
               # )
              #)
            #)
  )
)

server <- function(input, output,session){
  output$about_me<- renderUI({
    a<-"Demonstrated history of working in the research field. Strong skills in Python Programming, R programming.
    Strong research professional with a Master’s Degree focused in Biology, General from New York University."
    p(strong(h3(tags$u("About me"))), h5(tags$em(a)))
  })
  
  output$Experience_me<- renderUI({
    p(strong(h3(tags$u("Experience"))),strong(h3("Junior Scientist 2")),h4(strong(("Aganitha Cognitive Solutions"))),
      h5("Nov 2021- Present",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
         HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
         HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Hyderabad, Telangana, India"),tags$br(),
      strong(h3("Data Analyst")),h4(strong("72 Dragons")),
      h5("May 2020- Nov 2021",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
         HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
         HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
         HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
         HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Mumbai, India"),
      tags$ul(
        h5(tags$li(tags$em("Web Scraped Health and Art Data using beautiful soup library, selenium package and REST API in order to identify our potential clients"))),
        h5(tags$li(tags$em("Built web application analysing the Art Data such as Art Museums, Art Galleries etc for South East Asian countries using R shiny"))),
        h5(tags$li(tags$em("Built web application analysing the Social Media Data using R shiny"))),
        h5(tags$li(tags$em("Built web application analysing the health data consisting of Doctors and hospitals from South American Countries using R shiny")))
        )
      )
  })
  
  output$PlotTitle<- renderText({
    "Top Tools"
  })
  
  output$Skills_me<- renderPlotly({
    Noax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )

    fig<- plot_ly(
      y=df$Skills,
      x=df$Score,
      text=df$level,
      type='bar',marker=list(color="purple"),orientation="h",textposition="auto"
    ) %>% layout(xaxis=Noax,bargap=0.8,uniformtext=list(minsize=8, mode='show'))
    fig
    #b<- "Python Programming, R programming, R Shiny, Bash, MySQL"
    #p(strong(h4("Skills")), h5(b))
  })
  
  output$PlotTitle1<- renderText({
    "Top Skills"
  })
  
  output$Skills_me1<- renderPlotly({
    Noax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    fig<- plot_ly(
      y=df2$Skills,
      x=df2$Score,
      text=df2$level,
      type='bar',marker=list(color="purple"),orientation="h",textposition="auto"
    ) %>% layout(xaxis=Noax,bargap=0.8,uniformtext=list(minsize=8, mode='show'))
    fig
    #b<- "Python Programming, R programming, R Shiny, Bash, MySQL"
    #p(strong(h4("Skills")), h5(b))
  })
  
  output$Academic_Proj<- renderUI({
    p(strong(h3(tags$u("Academic Projects"))),h4(strong("NYU School of Medicine, Applied Bioinformatics Laboratories.")),h5(tags$em(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"04/2018 - 07/2018")),
      tags$ul(
        h5(tags$li(tags$em(
          "My project was to determine the quality of genomes of different Staphylococcus Aureus strains deposited in Genebank assembled at the level of complete assembly, scaffold and contig."
        )))
      ),tags$br(),
      strong(h4("NYU School of Medicine, Skirball Institute of Biomolecular Medicine.")),
      h5(tags$em(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"05/2017 - 12/2017")),
      tags$ul(
        h5(tags$li(tags$em(
          "Worked on understanding chromosomal rearrangements underlying hypomethylation of Satellite 2 sequences."
          )
        )
        ),
        tags$br(),
      ),
      strong(h4("Indian Institute of Science")),h5(tags$em(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"06/2012 - 06/2013")),
      tags$ul(
        h5(tags$li(tags$em(
          "My work involved purification and expression of mutant proteins G326A using Ni-Nta His resin chromatography. Succsessfully crystallized the G326A mutant in the presence of ATP."))))
      )
  })
  
  output$Edu_me<- renderUI({
    p(strong(h3(tags$u("Education"))),strong(h4("Master of Science, General Biology")),h5("New York University, Graduate School Of Arts and Science"),h5(tags$em("02/2016 - 12/2017",
                                                                                                       HTML('&nbsp;'),HTML('&nbsp;'),
                                                                                                        HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),tags$em("New York, USA, 3.667"))),
                                                                                                strong(h4("Master of Technology, Industrial Biotechnology")),strong(h5("National Institute of Technology, Karnataka")),
      h5(tags$em("06/2011 - 06/2013",
                 HTML('&nbsp;'),HTML('&nbsp;'),
                 HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Surathkal, Karnataka")),
      strong(h4("Bachelor of Engineering, Biotechnology")),h5("Visvesvaraya Technological University, Karnataka"),h5(tags$em("06/2006 - 06/2010",HTML('&nbsp;'),HTML('&nbsp;'),
                                                                                                                             HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Bengaluru, Karnataka")))
  })
  
  output$publication_me<- renderUI({
    p(strong(h3(tags$u("Publication"))),strong(tags$ul(h5(tags$li(tags$em(
      "Yannick Delpu, Thomas F. McNamara, Patrick Griffin, Suhail Kaleem, Shubhada Narayan, Carl Schildkraut, Karen H. Miga,Mamta Tahiliani- (2019)",HTML('&nbsp;'),HTML('&nbsp;'),
      HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Chromosomal rearrangements at hypomethylated Satellite 2 sequences are associated with impaired replication efficiency and increased fork stalling"
    ))))))
    
    output$Certificate<- renderUI({
      p(strong(h3(tags$u("Certificate"))),strong(tags$ul(h5(tags$li(tags$em("Post graduate Diploma in Data Science from IIIT Bangalore/UpGrad",HTML('&nbsp;'),HTML('&nbsp;'),
                                                                    HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"06/2019 - 06/2020"))))))
    })
    
    output$Languages<- renderUI({
      p(strong(h3(tags$u("Language"))), strong(tags$ul(h5(tags$li(tags$em("English"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Full Professional Efficency")),
                                               h5(tags$li(tags$em("Kannda"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Native or bilingual Efficency")),
                                               h5(tags$li(tags$em("Hindi"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Native or bilingual Efficency")))))
    })
    
  })

}

shinyApp(ui=ui,server=server)
  