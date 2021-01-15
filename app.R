library(shiny)
library(shinydashboard)
library(rgdal)
library(DT)
library(xts)
library(leaflet)

#Datos
datos_estaciones <- read.csv("data/Datos_Finales.csv", header=TRUE)
datos_estaciones1<-datos_estaciones
x=as.POSIXct(datos_estaciones[,1],format="%Y-%m-%d %H:%M")
datos_estaciones[,1]<-x



#ui object
ui <- dashboardPage(
    
    skin = "blue",
    dashboardHeader(title= "Red Pluviografica", titleWidth="300px", disable=TRUE),
    dashboardSidebar(width = "300px",
        

        
        fluidRow(
            column(width=12,offset =3,
                tags$img(src="logo_sihu.png",width="120px",height="96px")
            )
        ),
        
        #Logo SIHU
        # Diseño de la barra de inputs de los pluviografos 
        selectInput(
            inputId = "var",
            label= "Pluviografo",
            choices= c("P01 (Hemeroteca)"=1,
                       "P02 (Hemeroteca)"=2,
                       "P03 (Concha A.)"=3,
                       "P04 (Concha A.)"=4,
                       "P05 (Posgrado CH.)"=5,
                       "P06 (Posgrado CH.)"=6,
                       "P07 (Medicina)"=7,
                       "P08 (Medicina)"=8,
                       "P09 (Capilla)"=9,
                       "P10 (Capilla)"=10,
                       "P11 (Hidraulica)"=11,
                       "P12 (Hidraulica)"=12,
                       "P13 (Genetica)"=13,
                       "P14 (Genetica)"=14),
            selected=11
        ),
        
        #Diseño de la barra de inputs asociada a las fechas
        dateRangeInput(inputId = "date",
                       label ="Fecha",
                       start = "2007-10-08 00:00:00",
                       end = "2007-10-09 00:00:00",
                       format = "yyyy/mm/dd",
                       separator = " - ",
                       min = min(datos_estaciones$Fecha),
                       max = max(datos_estaciones$Fecha)
                       
        ),
        
        #Diseño de el boton de descargas de la aplicacion
        
        br(),
        
        fluidRow(
            column(width=12,offset =3,
                downloadButton("downloadData","Download", label ="Descargar Datos")
            )
        ),
        
        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
        
        #Diseño de la autoria de la aplicacion
        p("Hecho por:", a("GIREH",href = "http://gireh.unal.edu.co/" ),"y",a("SIHU",href = "http://gireh.unal.edu.co/sihu/" ), ".")
        
        
    ),
    
    dashboardBody(
        
        fluidRow(width=12,
            column(width=12,offset =3,
                box(width=6,title="Mapa Universidad Nacional",status="primary",solidHeader = TRUE,leafletOutput(outputId = "map"))
            )

            
        ),
        
        
        fluidRow(width=12,
            column(width = 12, offset = 2,
                box(width=8,title="Hietograma",status="primary",solidHeader = TRUE,plotOutput("plot1"),align="center")
            )
            
                        
            
        )
        
                
    )
    
    
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    

    
    
    # Diseño del mapa en leaflet
    output$map <- renderLeaflet({
        
        #Shapefile de las estaciones
        shape_estaciones<-readOGR("data/Coordenadas_Fi.shp")
        # Lugar de la estacion
        lugar_estacion <- shape_estaciones$Ubicacion %>% unique() %>% length()
        # Nombre de la estacion
        nombre_estacion <- shape_estaciones$Ubicacion %>% unique() 
        # Colores de la leyenda
        # se sustraen de "colorbrewer2.org" 
        colores <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')
        ## Paleta de colores
        pal <- colorFactor(colores,domain=nombre_estacion)
        # Crear el Mapa
        m <- leaflet() %>%
            addProviderTiles(providers$Stamen.Toner) %>%
            setView(lng = -74.083777, lat = 4.63756, zoom = 15.3) %>%
            addCircleMarkers(data = shape_estaciones,
                             radius = 5,
                             color= ~pal(Ubicacion),
                             fillOpacity = 1,
                             popup = ~Ubicacion,
                             label= ~Pluviograf)
        
        # Generar una leyenda
        m <- m %>% leaflet::addLegend(data=shape_estaciones, 
                                      "bottomright", 
                                      pal = pal,
                                      values=~Ubicacion,
                                      title = "Estacón",
                                      opacity = 2,
                                      group= "Leyenda")
        
        # Control de grupos
        m <-m %>% addLayersControl(overlayGroups = c("Leyenda"),
                                   options = layersControlOptions(collapsed = TRUE))
        
        
        
        
        
    })
    
    # Diseño de los hietogramas
    
    
    output$plot1 <- renderPlot({
        
        x1=as.POSIXct(input$date[1])
        x2=as.POSIXct(input$date[2])
        
        colm<-as.numeric(input$var)
        
        limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
        lims=match(as.POSIXct(x2),datos_estaciones$Fecha)
        
        xs=datos_estaciones$Fecha[(limi+300):(lims+300)]
        ys=datos_estaciones[,colm+1][(limi+300):(lims+300)]
        
        datos_ter<-data.frame(xs,ys)
        

        plot(datos_estaciones$Fecha[(limi+300):(lims+300)],
             datos_estaciones[,colm+1][(limi+300):(lims+300)],
             type="h",
             xlab="Fecha",
             ylab="Precipitacion (mm)",
             col="cornflowerblue") 
    })
    
    # Diseño de el boton de descarga de datos
    
    data_react<-reactive({
        
        data.frame(fechas_prec=(datos_estaciones$Fecha[((match(as.POSIXct(input$date[1]),datos_estaciones$Fecha))+300):((match(as.POSIXct(input$date[2]),datos_estaciones$Fecha))+300)]),
                   precip_mm=(datos_estaciones[,(as.numeric(input$var))+1][((match(as.POSIXct(input$date[1]),datos_estaciones$Fecha))+300):((match(as.POSIXct(input$date[2]),datos_estaciones$Fecha))+300)]))
        
        
    })
    
    
    output$downloadData <- downloadHandler(
        
        
        filename=function() {
            
            paste("datos",".csv",sep = "")
            
        },
        
        content = function(file) {
            
            write.csv(data_react(),file,row.names = FALSE)
            
        }
        
        
    )
    


    
}


# Run the application 
shinyApp(ui = ui, server = server)


