tags$nav(class="navbar navbar-inverse",
         #tags$div(class="container-fluid",
         tags$div(class="collapse navbar-collapse", id="bs-example-navbar-collapse-2"),
         tags$ul(class="nav navbar-nav",
                 tags$li(tags$a(href="#", "Inicio")
                 ),
                 tags$li(tags$a(href="descripcion.R", "Descripción")
                 )
         )#)
),





output$descargargrupos = downloadHandler(
  filename = "clusters.csv",
  content = function(){
    leer = read.csv(file = "prueba1.csv", header = TRUE)
    write.csv(x = leer, file = "clusters.csv")
  }
)




fluidRow(
  column(12,
         #downloadHandler("descargarclusters", "Descargars Clusters")
         downloadButton(outputId = "descargargrupos", label = "Descargar Clusters")
  )
)