
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(theme = "/css/bootstrap.css",
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "/css/style.css")
  ),
  
  tags$header(h2("Aplicación de técnicas de minería de texto para el agrupamiento de componentes académicos en base a los contenidos de planes docentes")
              ),
  
  navbarPage("", class="navbar navbar-default",
             tabPanel("Inicio",
                      fluidRow(
                        column(12,
                               h3(tags$strong("Ingrese contenido del documento"))
                        )
                      ),
                      tags$blockquote(
                      sidebarLayout(
                        sidebarPanel(width = "5", height="14px",
                                     #fileInput(inputId = "archivosId", label = "Cargar archivos", multiple = TRUE, accept = c("text/plain", ".txt")),
                                     textInput(inputId ="texto", label = "Pegue el contenido")
                        ),
                        mainPanel(textOutput("savecontenido")) #tableOutput("contents"), textOutput("savefiles"),
                      ),
                      
                      fluidRow(
                        column(3,
                               actionButton(inputId ="procesamiento", label = "Procesar", class="btn btn-primary"),
                               br(),
                               br()
                        ))),
                      
                      fluidRow(
                        column(12,
                               h3(tags$strong("Matriz términos por documentos"))
                        )
                      ),
                      
                      tags$blockquote(
                        fluidRow(
                          column(12,
                                 tags$div(class = "tmtd", tableOutput("mtd"))
                          )
                        )),
                      ##########Frecuencia de Términos#########
                      fluidRow(
                        column(10,
                               h3(tags$strong("Frecuencia de los Términos"))
                        )
                      ),
                      
                      tags$blockquote(
                        fluidRow(
                          column(12,
                                 plotOutput("frecuenciawords")
                          ),
                          column(4,
                                 actionButton(inputId ="frecuenciapalabras", label = "Frecuencia de palabras", class="btn btn-primary"),
                                 br(),
                                 br()
                          )
                        )),
                      ############Algoritmo K-means##################
                      fluidRow(
                        column(12,
                               h3(tags$strong("Algoritmo K-means"))
                        )
                      ),
                      
                      tags$blockquote(
                        sidebarLayout(
                          sidebarPanel(
                            numericInput(inputId = "numclusters", label = "Número de grupos", value = 3, min = 1, max = 10, width = "50%"),
                            actionButton(inputId ="Kmeans", label = "Algoritmo K-means", class="btn btn-primary")
                          ),
                          mainPanel(
                            plotOutput("plotkmeans")
                          )
                        )
                        ),
                      
                      #########SVD#################
                      fluidRow(
                        column(12,
                               h3(tags$strong("Descomposicón en Valores Singulares de la matriz términos por documentos"))
                        )
                      ),
                      
                      tags$blockquote(
                        
                        fluidRow(
                          column(12,
                                 plotOutput("plotsvd", height = "400px", width = "100%")
                          ),
                          column(4,
                                 actionButton(inputId ="btnsvd", label = "Descomposicón en Valores Singulares", class="btn btn-primary"),
                                 br(),
                                 br()
                          )
                        ))
                      ),
             
             #--------Menú opción Descripción.----_#
             tabPanel("Descripción",
                      h2(class = "descripcion", "Aplicación de técnicas de minería de texto para el agrupamiento de componentes académicos en base a los contenidos de planes docentes"),
                      tags$p("Descripción del proyecto Descripción del proyecto Descripción del proyecto Descripción del proyecto")
                      )
             ),
  
tags$footer(class="footer",
            tags$p("Desarrollado por: Robert Caraguay"))
))

