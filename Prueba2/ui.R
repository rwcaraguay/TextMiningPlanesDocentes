#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# Prueba N: 2

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "/css/bootstrap.css",
                  
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "/css/style.css")
                  ),
                  
                  tags$header(h2("Minería de Texto a los contenidos de los planes docentes de la UTPL")
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
                                               h3(tags$strong("Indexación Semántica Latente"))
                                        )
                                      ),
                                      
                                      tags$blockquote(
                                        
                                        fluidRow(
                                          column(12,
                                                 plotOutput("plotsvd", height = "400px", width = "100%")
                                          ),
                                          column(4,
                                                 actionButton(inputId ="btnsvd", label = "Algoritmo LSI", class="btn btn-primary"),
                                                 br(),
                                                 br()
                                          )
                                        ))
                             ),
                             
                             #--------Menú opción Descripción.----_#
                             tabPanel("Descripción",
                                      h3(class = "descripcion", "Aplicación de técnicas de minería de texto para el agrupamiento de componentes académicos en base a los contenidos de planes docentes"),
                                      tags$p(class = "texto_descripcion","La Minería de Texto es un área de investigación de las 
                                             ciencias de la computación que aplica la lingüística computacional y el 
                                             procesamiento de textos para la identificación y extracción de nuevo 
                                             conocimiento a partir de colecciones de documentos, es decir, de información no estructurada. 
                                             La Universidad Técnica Particular de Loja dispone de una gran cantidad de asignaturas que crean planes docentes 
                                             en diferentes periodos académicos, generando un alto volumen de información por lo que se requiere una evaluación 
                                             periódica de los contenidos empleados en cada plan docente ciclo a ciclo, con el propósito de comprobar que no exista 
                                             un solapamiento de contenidos. En el presente trabajo se da solución a esta problemática, a través de la aplicación 
                                             de técnicas de clustering de Minería de Texto a los contenidos de los planes docentes de la titulación de 
                                             Ingeniería en Sistemas Informáticos y Computación, con el fin de validar que la información sea consistente y no 
                                             existan contenidos semejantes entre áreas de estudio.")
                             )
                  ),
                  
                  tags$footer(class="footer",
                              tags$p("Desarrollado por: Robert Caraguay"))
))