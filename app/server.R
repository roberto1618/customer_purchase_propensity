# Paquetes necesarios
if('shiny' %in% rownames(installed.packages()) == FALSE) {install.packages('shiny')}
library(shiny)
if('data.table' %in% rownames(installed.packages()) == FALSE) {install.packages('data.table')}
library(data.table)
if('ggplot2' %in% rownames(installed.packages()) == FALSE) {install.packages('ggplot2')}
library(ggplot2)
if('stats' %in% rownames(installed.packages()) == FALSE) {install.packages('stats')}
library(stats)
if('shinydashboard' %in% rownames(installed.packages()) == FALSE) {install.packages('shinydashboard')}
library(shinydashboard)
if('DT' %in% rownames(installed.packages()) == FALSE) {install.packages('DT')}
library(DT)
if('tidyverse' %in% rownames(installed.packages()) == FALSE) {install.packages('tidyverse')}
library(tidyverse)
if('Amelia' %in% rownames(installed.packages()) == FALSE) {install.packages('Amelia')}
library(Amelia)
if('corrplot' %in% rownames(installed.packages()) == FALSE) {install.packages('corrplot')}
library(corrplot)
if('psych' %in% rownames(installed.packages()) == FALSE) {install.packages('psych')}
library(psych)
if('plotly' %in% rownames(installed.packages()) == FALSE) {install.packages('plotly')}
library(plotly)

# Función para realizar el análisis y mostrar en pantalla los resultados
shinyServer(function(input,output,session){
  datos_train <- eventReactive(input$train,{
    file1 <- input$datos_train
    if(is.null(file1)){return()}
    showModal(modalDialog('Leyendo datos de entrenamiento...', footer=NULL))
    aux <- fread(file = file1$datapath)
    removeModal()
    as.data.frame(aux)
  })
  
  # Crear modelo
  modelo_logit <- eventReactive(input$Go,{
    datos_train <- datos_train()
    colnames(datos_train) <- tolower(colnames(datos_train))
    if(sum(grepl('compra',colnames(datos_train)))!=1|is.null(datos_train())){return()}
    showModal(modalDialog('Creando modelo para propensión de compra...', footer=NULL))
    datos_train_logit <- datos_train[,-which(names(datos_train) %in% c('id_cliente', 'valor'))]
    modelo_logit <- step(glm(compra ~ ., data = datos_train_logit, family = 'binomial'), direction = 'backward')
    showModal(modalDialog('¡Modelo creado con éxito!', footer=NULL))
    Sys.sleep(3)
    removeModal()
    modelo_logit
  })

  modelo_reg <- eventReactive(input$Go,{
    datos_train <- datos_train()
    colnames(datos_train) <- tolower(colnames(datos_train))
    if(sum(grepl('valor',colnames(datos_train)))!=1|is.null(datos_train())){return()}
    showModal(modalDialog('Creando modelo para valor del cliente...', footer=NULL))
    datos_train_lineal <- datos_train[,-which(names(datos_train) %in% c('id_cliente', 'compra'))]
    modelo_lineal <- step(glm(valor ~ ., data = datos_train_lineal, family = 'gaussian'), direction = 'backward')
    showModal(modalDialog('¡Modelo creado con éxito!', footer=NULL))
    Sys.sleep(3)
    removeModal()
    modelo_lineal
  })

  datos_test <- eventReactive(input$test,{
    file2 <- input$datos_test
    if(is.null(file2)){return()}
    showModal(modalDialog('Compilando datos...', footer=NULL))
    datos_test = as.data.frame(fread(file = file2$datapath))
    colnames(datos_test) <- tolower(colnames(datos_test))
    removeModal()
    datos_test
  })
  
  # Función que ejecuta el análisis y calcula la predicción
  propension <- eventReactive(input$Go,{
    if(is.null(modelo_logit())|is.null(datos_test())){return()}
    showModal(modalDialog('Estimando la propensión de compra. Esto puede tardar varios minutos...', footer=NULL))
    modelo <- modelo_logit()
    datos_test <- datos_test()
    datos_test <- datos_test[,-which(names(datos_test) %in% c('id_cliente'))]
    removeModal()
    predict(modelo, newdata = datos_test, type = 'response')
  })
  
  valor_cliente <- eventReactive(input$Go,{
    if(is.null(modelo_reg())|is.null(datos_test())){return()}
    showModal(modalDialog('Estimando el valor del cliente. Esto puede tardar varios minutos...', footer=NULL))
    modelo <- modelo_reg()
    datos_test <- datos_test()
    datos_test <- datos_test[,-which(names(datos_test) %in% c('id_cliente'))]
    removeModal()
    predict(modelo, newdata = datos_test)
  })
  
  grupo_cliente <- eventReactive(input$Go,{
    if(is.null(modelo_logit())|is.null(datos_test())){return()}
    showModal(modalDialog('Calculando el grupo al que pertenece cada cliente. Esto puede tardar varios minutos...', footer=NULL))
    propension <- propension()
    grupo <- rep(0, length(propension))
    grupo[which(propension>0.9)] <- 1
    grupo[which(propension<0.9&propension>0.65)] <- 2
    grupo[which(propension<0.65&propension>0.35)] <- 3
    grupo[which(propension<0.35)] <- 4
    removeModal()
    grupo
  })
  
  tabla_propension <- eventReactive(input$Go,{
    if(is.null(propension())){return()}
    datos_test <- datos_test()
    propension <- propension()
    grupo <- grupo_cliente()
    valor <- valor_cliente()
    valor <- round(ifelse(valor<0, 0, valor), 2)
    tabla_propension <- data.frame(ID_cliente = datos_test[,'id_cliente'], Propension = propension, Valor = if(sum(valor)==0) '-' else {valor}, Grupo = grupo)
    tabla_propension
  })

  # Mostrar las predicciones en una tabla con las transacciones por fecha
  output$tabla_propension <- DT::renderDataTable({
    if(is.null(tabla_propension())){return()}
    tabla_propension <- tabla_propension()
    tabla_propension$Propension = paste0(round(tabla_propension$Propension, digits = 4)*100,'%')
    DT::datatable(tabla_propension, rownames = FALSE)%>%formatStyle(columns=colnames(tabla_propension), background = 'white', color='black')
  })
  
  output$tabla_datos_train <- DT::renderDataTable({
    tabla_datos_train <- datos_train()
    colnames(tabla_datos_train) <- tolower(colnames(tabla_datos_train))
    DT::datatable(tabla_datos_train, rownames = FALSE)%>%formatStyle(columns=colnames(tabla_datos_train),background = 'white',color='black')
  })
  
  # Mostrar el gráfico con la predicción
  output$plot_propension <- renderPlot({
    if(is.null(propension())){return()}
    propension <- propension()
    cutoff <- 0.65
    class <- ifelse(propension>=cutoff, 1, 0)
    num_compran <- sum(class==1)
    prop_compran <- num_compran/length(class)*100
    num_no_compran <- sum(class==0)
    prop_no_compran <- num_no_compran/length(class)*100
    tabla_plot <- data.frame(index = 1:length(propension), propension = propension, compra = class)
    theme.textos <- element_text(face = "bold.italic", color = "blue",size = 15)
    theme.ejes.y <- element_text(face = "bold", color = "black", size = 14)
    theme.ejes.x <- element_text(face = "bold", color = "black", size = 10)
    ggplot(data = tabla_plot, aes(x = index, y = propension))+
      geom_point(aes(colour = factor(class)))+
      geom_hline(yintercept = cutoff, color = 'darkorange', size = 1)+
      scale_fill_manual(values = c('green','red'))+
      scale_colour_discrete(name = 'Propensión', labels = c('No compra', 'Compra'))+
      xlab('Clientes')+
      ylab('Propensión de compra')+
      ggtitle(paste0('Clientes que compran: ', num_compran, ' (', prop_compran, '%).\nClientes que no compran: ', num_no_compran, ' (', prop_no_compran, '%).'))+
      theme(
        title = theme.textos,
        axis.title = theme.textos,
        axis.text.x = theme.ejes.x,
        axis.text.y = theme.ejes.y,
        panel.background = element_rect(fill = "#DEF6FF", colour = "#6D9EC1", size = 2, linetype = "solid"),
        legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(color = "black", size = 14)
      )
  })
  
  # Mostrar el gráfico con la pirámide
  output$plot_piramide <- renderPlot({
    if(is.null(propension())){return()}
    propension <- propension()
    valor <- valor_cliente()
    valor <- ifelse(valor<0, 0, valor)
    total <- length(propension)
    prop_class <- c(sum(propension>0.9)/total, sum(propension<0.9&propension>0.65)/total, sum(propension<0.65&propension>0.35)/total, sum(propension<0.35&propension>0)/total)
    valor_class <- c(sum(valor[which(propension>0.9)])/sum(valor), sum(valor[which(propension<0.9&propension>0.65)])/sum(valor), sum(valor[which(propension<0.65&propension>0.35)])/sum(valor), sum(valor[which(propension<0.35&propension>0)])/sum(valor))
    variable <- rep(c('Proporción de clientes', 'Valor del grupo'), each = 4)
    rango_propension <- factor(rep(c('1:Más de 90%', '2:Entre 65% y 90%', '3:Entre 35% y 65%', '4:Menos de 35%'), times = 2), levels = c('4:Menos de 35%', '3:Entre 35% y 65%', '2:Entre 65% y 90%', '1:Más de 90%'))
    tabla_piramide <- tibble(y = c(-prop_class, valor_class), variable = variable, rango_propension = rango_propension)
    ctr_width = 0.2
    grid <- seq(0, 1, by = 0.1)
    theme.textos <- element_text(face = "bold.italic", color = "blue",size = 15)
    theme.ejes.y <- element_text(face = "bold", color = "black", size = 14)
    theme.ejes.x <- element_text(face = "bold", color = "black", size = 10)
    tabla_piramide %>%
      mutate(start = if_else(variable == 'Proporción de clientes', 0, ctr_width),
             end = y + if_else(variable == 'Proporción de clientes', 0, ctr_width),
             mid = (start + end)/2,
             wid = abs(end - start))  %>%
      ggplot(data = .) +
      geom_tile(aes(mid, rango_propension, fill = variable, width = wid), height = 0.8) +
      # geom_text(data = . %>% distinct(rango_propension),
      #           aes(ctr_width/2, rango_propension, label = rango_propension)) +
      scale_x_continuous(breaks = c(-grid, ctr_width + grid),
                         labels = c(paste0(grid*100,'%'), paste0(grid*100,'%'))) +
      scale_fill_manual(values = c('darkblue','limegreen')) +
      theme(
        title = theme.textos,
        axis.title = theme.textos,
        axis.text.x = theme.ejes.x,
        axis.text.y = theme.ejes.y,
        panel.background = element_rect(fill = "#DEF6FF", colour = "#6D9EC1", size = 2, linetype = "solid"),
        legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(color = "black", size = 14)
      )+
      xlab('Porcentaje')+
      ylab('Propensión de compra')
  })
  
  # Mostrar todo en una interfaz interactiva con predicciones y con gráfico
  # output$tb <- renderUI({
  #   # Si no se ha podido ejecutar el analisis, muestra un mensaje, y en caso contrario, muestra los resultados
  #   if(is.null(propension()))
  #   {
  #     removeModal()
  #     list(strong('No es posible realizar el análisis. Compruebe si ha realizado correctamente lo siguiente:',style = 'font-family: "Aileron"; font-si16pt ;text-align:justify ; color: #E54646'),
  #          tags$li('Revisar que las columnas de sus bases de datos coincidan en nombre y posición.',style = 'font-family: "Aileron"; font-si12pt ;text-align:justify ; color: #E54646'),
  #          tags$li('Crear el modelo y compilar los datos correctamente',style = 'font-family: "Aileron"; font-si12pt ;text-align:justify ; color: #E54646'))
  #     #tags$li('Comprobar si existen datos de los filtros que ha seleccionado',style = 'font-family: "Aileron"; font-si12pt ;text-align:justify ; color: #E54646'))
  #   }
  #   else
  #   {
      # removeModal()
  #     tabsetPanel(tabPanel('Tabla de propensión',tableOutput('tabla_propension')), tabPanel('Gráfico resumen',plotOutput('plot_propension')))
  #   }
  # })
  
  # Descargar alertas para todas las metricas
  output$downloadData <- downloadHandler(
    # Cambiar el nombre del archivo descargado
    filename = function() {
      paste0('propension', '.csv')
    },
    content = function(file) {
      tabla_propension <- tabla_propension()
      tabla_propension$Propension <- round(tabla_propension$Propension, digits = 4)*100
      # Guardar los datos en un csv
      write.csv2(tabla_propension, file, row.names = FALSE)
      removeModal()
    })
  
  ####################################Analisis descriptivo
  ####################################
  ####################################
  ####################################
  ####################################
  descriptivo <- eventReactive(input$train,{
    if(is.null(datos_train())){return()}
    showModal(modalDialog("Analizando datos...", footer=NULL))
    datos <- datos_train()
    nvar <- ncol(datos)
    
    # Si el numero de valores diferentes es inferior a un numero concreto, convertir a categórica
    lim_cat <- 7
    for(j in 1:nvar)
    {
      if(nlevels(as.factor(datos[,j])) <= lim_cat)
      {
        datos[,j] <- as.factor(datos[,j])
      }
    }
    clases <- table(sapply(datos,class))
    nobs <- nrow(datos)
    missing <- sum(is.na(datos))
    
    # Tabla de info general
    general <- data.frame(Info = c('Núm. Variables','Numéricas','Categóricas','Texto','Núm. Datos','Perdidos'),Cant = c(nvar,0,0,0,nobs,missing))
    general$Cant[general$Info=='Numéricas'] <- ifelse(is.na(unname(clases['numeric'])),0,unname(clases['numeric'])) + ifelse(is.na(unname(clases['integer'])),0,unname(clases['integer']))
    general$Cant[general$Info=='Categóricas'] <- ifelse(is.na(unname(clases['factor'])),0,unname(clases['factor']))
    general$Cant[general$Info=='Texto'] <- ifelse(is.na(unname(clases['character'])),0,unname(clases['character']))
    #general$Cant[general$Info=='Otra'] <- ifelse(is.na(unname(clases['character'])),0,unname(clases['character']))
    
    # Estudio variable a variable
    cada_variable <- data.frame(variable=c(0),tipo=c(0),perdidos=c(0),media=c(0),desv=c(0),minimo=c(0),maximo=c(0),num_categorias=c(0),detalle_categorica=c(0))
    for(j in 1:nvar)
    {
      varAux <- datos[,j]
      variable <- colnames(datos)[j]
      tipo <- ifelse(class(varAux)=='numeric'|class(varAux)=='integer','Numérica',ifelse(class(varAux)=='factor','Categórica',ifelse(class(varAux)=='character','Texto','Otra')))
      perdidos <- sum(is.na(varAux))
      if(class(varAux)=='numeric'|class(varAux)=='integer')
      {
        media <- round(mean(varAux, na.rm = T), 2)
        desv <- round(sd(varAux, na.rm = T), 2)
        minimo <- round(min(varAux, na.rm = T), 2)
        maximo <- round(max(varAux, na.rm = T), 2)
      }else{
        media <- '-'
        desv <- '-'
        minimo <- '-'
        maximo <- '-'
      }
      
      if(class(varAux)=='factor')
      {
        num_categorias <- nlevels(varAux)
        detalle_categorica <- ''
        for(i in 1:num_categorias)
        {
          detalle_categorica <- paste0(detalle_categorica,' ',levels(varAux)[i],' (',sum(varAux==levels(varAux)[i]),')')
        }
      }else{
        num_categorias <- '-'
        detalle_categorica <- '-'
      }
      cada_variable <- rbind(cada_variable,c(variable,tipo,perdidos,media,desv,minimo,maximo,num_categorias,detalle_categorica))
    }
    
    cada_variable <- cada_variable[-1,]
    colnames(cada_variable) <- c('Variable', 'Tipo de variable', 'Perdidos', 'Media', 'Desv. Típica', 'Valor mínimo', 'Valor máximo', 'Categorías', 'Recuento categorías')
    
    cada_variable <- DT::datatable(cada_variable, rownames = FALSE)%>%formatStyle(columns=colnames(cada_variable),background = 'white',color='black')
    datos_amelia <- as.data.table(datos)
    datos_num <- as.matrix(dplyr::select_if(datos, is.numeric))
    datos_cat <- as.matrix(dplyr::select_if(datos, is.factor))
    corr_aux <- cor(datos_num, method = 'pearson', use = 'complete.obs')
    colnames(datos) <- tolower(colnames(datos))
    cabecero <- DT::datatable(datos, rownames = FALSE)%>%formatStyle(columns=colnames(datos),background = 'white',color='black')
    
    list(general, cada_variable, datos_amelia, corr_aux, cabecero, datos_num, datos_cat)
  })
  
  output$general <- renderTable({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    general <- descriptivo[[1]]
    general$Cant <- as.integer(general$Cant)
    general
  })
  
  output$cada_variable <- DT::renderDataTable({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    cada_variable <- descriptivo[[2]]
    cada_variable
  })
  
  output$mapa_perdidos <- renderPlot({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    datos_amelia <- descriptivo[[3]]
    mapa_perdidos <- missmap(datos_amelia,col=c("black","grey"),legend=FALSE)
    mapa_perdidos
  })
  
  output$correlaciones <- renderPlot({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    corr_aux <- descriptivo[[4]]
    if(ncol(corr_aux)<2){
      return()
    }else{
      corrplot(corr_aux,order = 'hclust',type = 'upper',addCoef.col = "white")
    }
  },height = 800, width = 800)
  
  output$cabecero <- DT::renderDataTable({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    cabecero <- descriptivo[[5]]
    cabecero
  })
  
  output$histogramas <- renderPlot({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    datos_num <- as.data.frame(descriptivo[[6]])
    multi.hist(datos_num, bcol='deepskyblue1', dcol = c('black','darkred'), main = if(ncol(datos_num)>=2) NULL else {colnames(datos_num)})
  }, height = 1000, width = 1000)
  
  output$barplots <- renderPlot({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    datos_cat <- as.data.frame(descriptivo[[7]])
    par(mfrow = c(ceiling(ncol(datos_cat)/2),2))
    for(i in 1:ncol(datos_cat))
    {
      counts <- table(datos_cat[i])
      barplot(counts, col = 'darkorange', main = colnames(datos_cat)[i])
    }
  }, height = 700, width = 1000)
  
  output$boxplots <- renderPlot({
    if(is.null(descriptivo())){return()}
    descriptivo <- descriptivo()
    datos_num <- as.data.frame(descriptivo[[6]])
    par(mfrow = c(ceiling(ncol(datos_num)/2),2))
    for(i in 1:ncol(datos_num))
    {
      boxplot(datos_num[i],col = 'olivedrab3', main = colnames(datos_num)[i])
    }
  }, height = 4000, width = 800)
  
  output$impacto <- renderText({
    if(is.null(descriptivo())|is.null(modelo_logit())){return()}
    modelo_logit <- modelo_logit()
    summary_logit <- summary(modelo_logit)
    pvalues <- as.data.frame(summary_logit$coefficients[-1, c(1,4), drop = F])
    colnames(pvalues) <- c('estimate', 'p_value')
    pvalues <- pvalues[which(pvalues$p_value<0.05),]
    pvalues <- pvalues[order(pvalues$p_value),]
    impacto <- ''
    for(i in 1:nrow(pvalues))
    {
      impacto <- paste0(impacto, '<br/><br/>',i,' - <b>', rownames(pvalues)[i], '</b>: Tiene una relación ', ifelse(sign(pvalues$estimate[i])<0, '<font color = "red">inversa</font>', '<font color = "green">directa</font>'), ' con la propensión de compra.')
    }
    paste0('<font size = 3 face = "Aileron"> Las variables están ordenadas de mayor a menor importancia:<br/>',impacto,'</font>')
  })
  
  output$tb <- renderUI({
    if(is.null(descriptivo()))
    {
      removeModal()
      list(strong('No es posible realizar el análisis descriptivo. Compruebe que su base de datos tiene el formato adecuado.', style = "font-family: 'Aileron'; font-si16pt ;text-align:justify ; color: #E54646"))
    }
    else
    {
      removeModal()
      tabsetPanel(tabPanel('General',tableOutput('general')), tabPanel('Tipo variables',div(style = 'overflow-x: scroll', DT::dataTableOutput('cada_variable'))),tabPanel('Mapa perdidos',plotOutput('mapa_perdidos')),tabPanel('Correlación',div(style='max-height:500px; overflow-y: scroll; position: relative',plotOutput('correlaciones'))),tabPanel('Muestra',div(style = 'overflow-x: scroll', DT::dataTableOutput('cabecero'))),tabPanel('Distribuciones',div(style='max-height:500px; overflow-y: scroll; position: relative',plotOutput('histogramas')),div(style='max-height:500px; overflow-y: scroll; position: relative',plotOutput('barplots'))),tabPanel('Atípicos',div(style='max-height:500px; overflow-y: scroll; position: relative',plotOutput('boxplots'))), tabPanel('Impacto', div(style='max-height:500px; overflow-y: scroll; position: relative', htmlOutput('impacto'))))
    }
  })
  
  output$perfiles <- DT::renderDataTable({
    if(is.null(modelo_logit())){return()}
    perfil <- input$perfil
    tabla_propension <- tabla_propension()
    datos_test <- datos_test()
    datos_test$Grupo <- tabla_propension$Grupo
    datos <- datos_test[datos_test$Grupo==perfil,]
    nvar <- ncol(datos)
    
    # Si el numero de valores diferentes es inferior a un numero concreto, convertir a categórica
    lim_cat <- 4
    for(j in 1:nvar)
    {
      if(nlevels(as.factor(datos[,j])) <= lim_cat)
      {
        datos[,j] <- as.factor(datos[,j])
      }
    }
    
    # Estudio variable a variable
    cada_variable <- data.frame(variable=c(0),media=c(0),minimo=c(0),maximo=c(0),detalle_categorica=c(0))
    for(j in 1:nvar)
    {
      varAux <- datos[,j]
      variable <- colnames(datos)[j]
      if(class(varAux)=='numeric'|class(varAux)=='integer')
      {
        media <- round(mean(varAux, na.rm = T), 2)
        minimo <- round(min(varAux, na.rm = T), 2)
        maximo <- round(max(varAux, na.rm = T), 2)
      }else{
        media <- '-'
        minimo <- '-'
        maximo <- '-'
      }
      
      if(class(varAux)=='factor')
      {
        num_categorias <- nlevels(varAux)
        detalle_categorica <- ''
        for(i in 1:num_categorias)
        {
          detalle_categorica <- paste0(detalle_categorica,' ',levels(varAux)[i],' (',round(sum(varAux==levels(varAux)[i])/nrow(datos), 4)*100,'%)')
        }
      }else{
        detalle_categorica <- '-'
      }
      cada_variable <- rbind(cada_variable,c(variable,media,minimo,maximo,detalle_categorica))
    }
    
    cada_variable <- cada_variable[-c(1, which(cada_variable$variable=='id_cliente')),]
    colnames(cada_variable) <- c('Variable', 'Media', 'Valor mínimo', 'Valor máximo', 'Recuento categorías')
    cada_variable <- DT::datatable(cada_variable, rownames = FALSE)%>%formatStyle(columns=colnames(cada_variable),background = '#E2FBFE',color='#223DE9')
    cada_variable
  })
  
  # Para el mensaje de datos compilados
  output$fileUploadedtest <- reactive({
    return(!is.null(datos_test()))
  })
  output$fileUploadedtrain <- reactive({
    return(!is.null(datos_train()))
  })
  outputOptions(output, 'fileUploadedtest', suspendWhenHidden=FALSE)
  outputOptions(output, 'fileUploadedtrain', suspendWhenHidden=FALSE)
})