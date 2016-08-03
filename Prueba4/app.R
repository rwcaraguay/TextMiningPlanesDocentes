#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#Prueba 4

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = "/css/bootstrap.css",
                        
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
                                                  numericInput(inputId = "numclusters", label = "Número de grupos", value = 4, min = 1, max = 10, width = "50%"),
                                                  actionButton(inputId ="Kmeans", label = "Algoritmo K-means", class="btn btn-primary")
                                                ),
                                                mainPanel(
                                                  plotOutput("plotkmeans")
                                                )
                                              )
                                            ),
                                            
                                            #########LSI#################
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

# Define server logic required to draw a histogram
setwd("C:/Users/RobertWladimir/Documents/R/AppTextMining")

server <- shinyServer(function(input, output) {
   
  output$savecontenido = renderPrint({
    cont=input$texto
    
    if (is.null(cont))
      return(NULL)
    write.table(x = cont, "Prueba4/corpus/pdn")
  })
  
  output$mtd = renderTable({
    input$procesamiento
    
    limpieza = isolate({
      documentos = Corpus(DirSource("Prueba4/corpus/", encoding = "UTF-8"))
      espacio <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      corpusclean = tm_map(documentos, espacio, "[[:punct:]]+")
      corpusclean = tm_map(corpusclean, removePunctuation)
      corpusclean = tm_map(corpusclean, content_transformer(tolower))
      corpusclean = tm_map(corpusclean, removeNumbers)
      corpusclean = tm_map(corpusclean, stripWhitespace)
      
      corpuslimpio = corpusclean
      
      #Eliminar palabras vac?as a partir de un documento
      corpusprocesado = tm_map(corpuslimpio, removeWords, c(stopwords("spanish"), "corpus"))
      archivopalabrasvacias = readLines("files/palabrasvacias", encoding = "UTF-8")#Cargar archivo de palabras vac?as en espa?ol y convertir a ASCII
      archivopalabrasvacias = iconv(archivopalabrasvacias, to = "ASCII//TRANSLIT")
      corpusprocesado = tm_map(corpusprocesado, removeWords, archivopalabrasvacias)#Eliminar las palabras vac?as a partir del documento
      corpusprocesado = tm_map(corpusprocesado, stripWhitespace)#Eliminar espacios en blanco adicionales
      
      #Lematizar las palabras de los documentos
      corpusprocesado = tm_map(corpusprocesado, stemDocument, language="spanish") #Lematizaci?n en el idioma espa?ol
      
      corpusmatriz = corpusprocesado
      writeCorpus(corpusmatriz, path = "Prueba4/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba4/archivos/vocabulariop4", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba4/resultados/mtdp4.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      matriz
    })
    limpieza
  })
  
  output$frecuenciawords = renderPlot({
    
    input$frecuenciapalabras
    
    frecuanciaPalabras = isolate({
      documentos = Corpus(DirSource("Prueba4/corpus/", encoding = "UTF-8"))
      espacio <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      corpusclean = tm_map(documentos, espacio, "[[:punct:]]+")
      corpusclean = tm_map(corpusclean, removePunctuation)
      corpusclean = tm_map(corpusclean, content_transformer(tolower))
      corpusclean = tm_map(corpusclean, removeNumbers)
      corpusclean = tm_map(corpusclean, stripWhitespace)
      
      corpuslimpio = corpusclean
      
      #Eliminar palabras vac?as a partir de un documento
      corpusprocesado = tm_map(corpuslimpio, removeWords, c(stopwords("spanish"), "corpus"))
      archivopalabrasvacias = readLines("files/palabrasvacias", encoding = "UTF-8")#Cargar archivo de palabras vac?as en espa?ol y convertir a ASCII
      archivopalabrasvacias = iconv(archivopalabrasvacias, to = "ASCII//TRANSLIT")
      corpusprocesado = tm_map(corpusprocesado, removeWords, archivopalabrasvacias)#Eliminar las palabras vac?as a partir del documento
      corpusprocesado = tm_map(corpusprocesado, stripWhitespace)#Eliminar espacios en blanco adicionales
      
      #Lematizar las palabras de los documentos
      corpusprocesado = tm_map(corpusprocesado, stemDocument, language="spanish") #Lematizaci?n en el idioma espa?ol
      
      corpusmatriz = corpusprocesado
      writeCorpus(corpusmatriz, path = "Prueba4/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba4/archivos/vocabulariop4", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba4/resultados/mtdp4.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      
      frecuencia = matriz
      frecuencia = colSums(frecuencia) #Frecuencia de los t?rminos, organiza de acuerdo a la frecuencia.
      frecuencia = subset(frecuencia, frecuencia >= 1)#Palabras con frecuencia > 3
      frecuencia = sort(frecuencia, decreasing = TRUE)#Ordenar descendente con el n?mero de frecuencia de t?rminos
      frecupalab = data.frame(palabras = (names(frecuencia)), frecuencia = frecuencia)
      ggplot(subset(frecupalab, frecuencia>=1), aes(palabras, frecuencia))+geom_bar(stat = "identity", fill = "steelblue")+theme_minimal()+
        theme(axis.text.x=element_text(angle = 90, hjust = 1, color = "black"))+geom_text(aes(label = frecuencia), vjust=-0.3, size=3.5)
    })
    frecuanciaPalabras
  })
  
  output$plotkmeans = renderPlot({
    input$Kmeans
    algoritmokmeans = isolate({
      
      documentos = Corpus(DirSource("Prueba4/corpus/", encoding = "UTF-8"))
      espacio <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      corpusclean = tm_map(documentos, espacio, "[[:punct:]]+")
      corpusclean = tm_map(corpusclean, removePunctuation)
      corpusclean = tm_map(corpusclean, content_transformer(tolower))
      corpusclean = tm_map(corpusclean, removeNumbers)
      corpusclean = tm_map(corpusclean, stripWhitespace)
      
      corpuslimpio = corpusclean
      
      #Eliminar palabras vac?as a partir de un documento
      corpusprocesado = tm_map(corpuslimpio, removeWords, c(stopwords("spanish"), "corpus"))
      archivopalabrasvacias = readLines("files/palabrasvacias", encoding = "UTF-8")#Cargar archivo de palabras vac?as en espa?ol y convertir a ASCII
      archivopalabrasvacias = iconv(archivopalabrasvacias, to = "ASCII//TRANSLIT")
      corpusprocesado = tm_map(corpusprocesado, removeWords, archivopalabrasvacias)#Eliminar las palabras vac?as a partir del documento
      corpusprocesado = tm_map(corpusprocesado, stripWhitespace)#Eliminar espacios en blanco adicionales
      
      #Lematizar las palabras de los documentos
      corpusprocesado = tm_map(corpusprocesado, stemDocument, language="spanish") #Lematizaci?n en el idioma espa?ol
      
      corpusmatriz = corpusprocesado
      writeCorpus(corpusmatriz, path = "Prueba4/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba4/archivos/vocabulariop4", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba4/resultados/mtdp4.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      
      matrizk = matriz #Matriz término documento
      distancia = dist(t(matrizk), method = "euclidean") #Distancia euclidiana
      k = input$numclusters #Número de clusters
      agrupamiento = kmeans(distancia, k) #Algoritmo kmeans
      agrupamiento
      
      dataframe = as.data.frame(agrupamiento$cluster) #crear dataframe entre los t?rminos y clusters
      names(dataframe) = c("cluster")
      dataframe #muestra a que cluster pertenece cada palabra
      #Plot con los clusters
      clusplot(as.matrix(distancia),agrupamiento$cluster, main = "K Means Clustering",
               xlab = "Términos", ylab = "Clusters", color = TRUE, shade = TRUE, labels = 4, lines = 0) #Plot kmeans
      write.csv(dataframe,file = "Prueba4/resultados/clusters-prueba4.csv")
    })
    algoritmokmeans
  })
  
  output$plotsvd = renderPlot({
    
    input$btnsvd
    
    svdmatriz = isolate({
      
      documentos = Corpus(DirSource("Prueba4/corpus/", encoding = "UTF-8"))
      espacio <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      corpusclean = tm_map(documentos, espacio, "[[:punct:]]+")
      corpusclean = tm_map(corpusclean, removePunctuation)
      corpusclean = tm_map(corpusclean, content_transformer(tolower))
      corpusclean = tm_map(corpusclean, removeNumbers)
      corpusclean = tm_map(corpusclean, stripWhitespace)
      
      corpuslimpio = corpusclean
      
      #Eliminar palabras vac?as a partir de un documento
      corpusprocesado = tm_map(corpuslimpio, removeWords, c(stopwords("spanish"), "corpus"))
      archivopalabrasvacias = readLines("files/palabrasvacias", encoding = "UTF-8")#Cargar archivo de palabras vac?as en espa?ol y convertir a ASCII
      archivopalabrasvacias = iconv(archivopalabrasvacias, to = "ASCII//TRANSLIT")
      corpusprocesado = tm_map(corpusprocesado, removeWords, archivopalabrasvacias)#Eliminar las palabras vac?as a partir del documento
      corpusprocesado = tm_map(corpusprocesado, stripWhitespace)#Eliminar espacios en blanco adicionales
      
      #Lematizar las palabras de los documentos
      corpusprocesado = tm_map(corpusprocesado, stemDocument, language="spanish") #Lematizaci?n en el idioma espa?ol
      
      corpusmatriz = corpusprocesado
      writeCorpus(corpusmatriz, path = "Prueba4/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba4/archivos/vocabulariop4", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba4/resultados/mtdp4.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      
      matrizsvd = matriz #Matriz término documento
      algsvd =  svd(matrizsvd) #Algoritmo SVD
      matrizu <- algsvd$u[,1:5] #Reducción de la dimensión del espacio
      plot(matrizu, asp=1, main = "Representación de los documentos en el plano", pch=16, col=34);
      abline(h = 0, v = 0, lty =5, col="chartreuse3") #Gráfica de los documentos
      text(matrizu, labels=rownames(matrizsvd), pos=4, col="blue", font=10, cex=0.9)
      for(i in 1:nrow(matrizu))#Trazar los vectores
      {arrows(x0=0, y0=0, x1=matrizu[i,1], y1=matrizu[i,2], length=0.1, angle=15, col="black", lwd=1)}
    })
    svdmatriz
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

