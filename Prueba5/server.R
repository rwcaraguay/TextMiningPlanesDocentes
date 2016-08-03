#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# Prueba 5
setwd("C:/Users/RobertWladimir/Documents/R/AppTextMining")

library(shiny)
library(NLP) #librer?a para el procesamiento del lenguaje natural
library(tm) #librer?a para text mining
library(SnowballC) #Para lematizar las palabras
library(cluster)
library(fpc)
library(RWeka) #Librería para formar n-gramas
library(stats) #Libreria para el K-mean
library(ggplot2) #Para dibugar histograma

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$savecontenido = renderPrint({
    cont=input$texto
    
    if (is.null(cont))
      return(NULL)
    write.table(x = cont, "Prueba5/corpus/pd5")
  })
  
  output$mtd = renderTable({
    input$procesamiento
    
    limpieza = isolate({
      documentos = Corpus(DirSource("Prueba5/corpus/", encoding = "UTF-8"))
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
      writeCorpus(corpusmatriz, path = "Prueba5/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba5/archivos/vocabulariop5", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba5/resultados/mtdp5.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      matriz
    })
    limpieza
  })
  
  output$frecuenciawords = renderPlot({
    
    input$frecuenciapalabras
    
    frecuanciaPalabras = isolate({
      documentos = Corpus(DirSource("Prueba5/corpus/", encoding = "UTF-8"))
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
      writeCorpus(corpusmatriz, path = "Prueba5/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba5/archivos/vocabulariop5", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba5/resultados/mtdp5.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      
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
      
      documentos = Corpus(DirSource("Prueba5/corpus/", encoding = "UTF-8"))
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
      writeCorpus(corpusmatriz, path = "Prueba5/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba5/archivos/vocabulariop5", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba5/resultados/mtdp5.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      
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
      write.csv(dataframe,file = "Prueba5/resultados/clusters-prueba5.csv")
    })
    algoritmokmeans
  })
  
  output$plotsvd = renderPlot({
    
    input$btnsvd
    
    svdmatriz = isolate({
      
      documentos = Corpus(DirSource("Prueba5/corpus/", encoding = "UTF-8"))
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
      writeCorpus(corpusmatriz, path = "Prueba5/preprocesados/") #guardar los archivos pre-procesados
      
      vocabulario = readLines("Prueba5/archivos/vocabulariop5", encoding = "UTF-8")#Cargar archivo con el vocabulario
      vocabulario = iconv(vocabulario, to = "ASCII//TRANSLIT")
      matrizterminodocumento = DocumentTermMatrix(corpusmatriz, control = list(dictionary = c(vocabulario), tokenize = NGramTokenizer)) #Crear MTD apartir del vocabulario
      matriz = as.matrix(matrizterminodocumento)#Crear matriz de caracteres apartir de los t?rminos
      write.csv(matriz, file = "Prueba5/resultados/mtdp5.csv", row.names = TRUE, fileEncoding = "ASCII//TRANSLIT")#guardar la mtd en formato csv  
      
      matrizsvd = matriz #Matriz término documento
      algsvd =  svd(matrizsvd) #Algoritmo SVD
      matrizu <- algsvd$u[,1:2] #Reducción de la dimensión del espacio
      plot(matrizu, asp=1, main = "Representación de los documentos en el plano", pch=16, col=34);
      abline(h = 0, v = 0, lty =5, col="chartreuse3") #Gráfica de los documentos
      text(matrizu, labels=rownames(matrizsvd), pos=2, col="blue", font=10, cex=0.9)
      for(i in 1:nrow(matrizu))#Trazar los vectores
      {arrows(x0=0, y0=0, x1=matrizu[i,1], y1=matrizu[i,2], length=0.1, angle=15, col="black", lwd=1)}
    })
    svdmatriz
  })
})
