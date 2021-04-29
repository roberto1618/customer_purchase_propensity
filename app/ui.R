# Paquetes necesarios
if('shiny' %in% rownames(installed.packages()) == FALSE) {install.packages('shiny')}
library(shiny)
if('shinythemes' %in% rownames(installed.packages()) == FALSE) {install.packages('shinythemes')}
library(shinythemes)
if('shinydashboard' %in% rownames(installed.packages()) == FALSE) {install.packages('shinydashboard')}
library(shinydashboard)
if('DT' %in% rownames(installed.packages()) == FALSE) {install.packages('DT')}
library(DT)
if('tidyverse' %in% rownames(installed.packages()) == FALSE) {install.packages('tidyverse')}
library(tidyverse)

sidebar <- dashboardSidebar(collapsed = TRUE, width = 270,
  sidebarMenu(
    menuItem('Calcular propensión', tabName = 'propension', icon = icon('chart-line')),
    menuItem('Resumen de resultados', tabName = 'resumen', icon = icon('clipboard')),
    menuItem('Descripción e impacto de variables', tabName = 'visualizar', icon = icon('chart-bar')),
    menuItem('Perfiles de clientes', tabName = 'perfiles', icon = icon('users')),
    menuItem('Info', tabName = 'info', icon = icon('info-circle'))
  )
)

body <-   dashboardBody(
  tabItems(
    tabItem(tabName = 'propension',
            fluidRow(
              box(title = 'Datos históricos', status = 'primary', solidHeader = TRUE,
                fileInput('datos_train','Introduce tus datos para entrenar el modelo'),
                # Botón para compilar
                actionButton('train','Compilar datos',
                             style='color: #FFFFFF; background-color: #4D4848; border-color: #000000'),
                conditionalPanel(
                  condition = 'output.fileUploadedtrain && input.train',
                  helpText('Datos listos para la creación del modelo.', style = 'color: #0D9D16')
                ),
                helpText('Suba un archivo .csv que contenga la información de cada cliente en cada fila y como columnas: distintas variables a considerar en el análisis y al menos una variable de estudio
                                    binaria (0 ó 1) con el resultado de compra, cuyo nombre debe ser "compra". Puede incluir más variable de estudio de otro tipo. Además, debe existir una columna denominada
                                    "id_cliente" que identifique unívocamente a cada cliente.')
              ),
              box(title = 'Datos para calcular propensión', status = 'success', solidHeader = TRUE,
                fileInput('datos_test','Introduce tus datos para predecir la propensión de compra'),
                #Botón para compilar
                actionButton('test','Compilar datos',
                             style='color: #FFFFFF; background-color: #4D4848; border-color: #000000'),
                # Si se han compilado los datos, mostrar un mensaje:
                conditionalPanel(
                  condition = 'output.fileUploadedtest && input.test',
                  helpText('Datos compilados.', style = 'color: #0D9D16')
                ),
                helpText('Suba un archivo .csv que contenga exactamente las mismas variables, y en el mismo orden, que los
                                  datos de entrenamiento. Todas excepto las variables de estudio introducidas en los datos históricos.')
              )
            ),
            fluidRow(
              box(title = 'Ejecutar cálculo',
                  actionButton('Go','Obtener propensión de compra',icon('chart-line'),
                               style='color: #fff; background-color: #E54646; border-color: #000000'),
                  br(),
                  br(),
                  conditionalPanel(
                    condition = 'input.Go',
                    downloadButton('downloadData', 'Descargar propensión de compra',
                                   style='color: #fff; background-color: #537DFF; border-color: #000000')
                  )
              ),
              box(plotOutput('plot_propension'))
            )
    ),
    tabItem(tabName = 'resumen',
            fluidRow(
              #box(title = 'Datos de entrenamiento', status = 'info', solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput('tabla_datos_train'))),
              box(title = 'Propensión para nuevos clientes', status = 'info', solidHeader = TRUE, div(style = 'overflow-x: scroll', DT::dataTableOutput('tabla_propension')))
            ),
            fluidRow(
              box(title = 'Grupos de clientes', status = 'info', solidHeader = TRUE, width = 12, plotOutput('plot_piramide'))
            )),
    tabItem(tabName = 'visualizar',
            fluidRow(
              box(title = 'Descripción de datos históricos e impacto de variables', status = 'info', solidHeader = TRUE, width = 12, uiOutput('tb'))
            )),
    tabItem(tabName = 'perfiles',
            fluidRow(
              box('Perfiles de clientes', status = 'success', solidHeader = TRUE, width = 12,
                  selectInput('perfil', 'Seleccione grupo de propensión', choices = list('1: Más de 90%' = 1, '2: Entre 65% y 90%' = 2, '3: Entre 35% y 65%' = 3, '4: Menos de 35%' = 4)),
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('perfiles')))
            )),
    tabItem(tabName = 'info',
            p('Esta aplicación permite obtener la propensión de compra y el valor esperado de los clientes que sean
              introducidos en la base de datos. La base de datos debe contener información de los clientes (donde cada
              fila se corresponda con un cliente) con un identificador único para cada uno de ellos cuya columna debe
              nombrarse como "id_cliente". La base de datos puede contener distintas variables que el usuario considere
              que aportan información relevante en el análisis.', style = 'font-family: "Aileron"; font-si16pt ;text-align:justify'),
            p('Se deben introducir dos bases de datos: la primera,
              para entrenar el modelo, debe contener dos columnas adicionales nombradas como "compra" y "valor", donde
              se guarde si el cliente ha comprado tras el período de recogida de datos o no (con un 1 o un 0), y el
              importe gastado por el cliente en el mismo intervalo de tiempo, respectivamente; la segunda, deben ser los datos para la obtención de las nuevas
              propensiones de compra, y deben contener las mismas columnas (y en el mismo orden) que los datos de entrenamiento, excepto
              las variables "compra" y "valor".', style = 'font-family: "Aileron"; font-si16pt ;text-align:justify'),
            p('Es importante saber que los períodos
              en los que se ha obtenido la información de ambas tablas deberá ser el mismo. Por ejemplo, si se quiere obtener
              la propensión de compra y el valor de un cliente con vista a 3 meses y con datos estudiados durante 1 año, los datos
              de entrenamiento también deben contener datos históricos de 1 año y con las variables "compra" y "valor" con información
              obtenida en los 3 meses posteriores, exactamente.',
              style = 'font-family: "Aileron"; font-si16pt ;text-align:justify')
    )
  )
)

shinyUI(dashboardPage(skin = 'black',
  dashboardHeader(title = 'Propensión de compra'),
  sidebar,
  body
  )
)