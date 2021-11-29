
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(tercen)
library(dplyr)
library(jsonlite)

library(tidyr)
library(ggplot2)
library(ggcyto)
library(flowWorkspace)
library(flowCore)
library(scales)
library(ggallin)
library(ggh4x)
library(RColorBrewer)


# http://127.0.0.1:5402/admin/w/77d52bb01bd3676e779828d5a50047ae/ds/36600030-7fb6-4e61-a25c-fd421ec60367
# options("tercen.workflowId"= "77d52bb01bd3676e779828d5a50047ae")
# options("tercen.stepId"= "36600030-7fb6-4e61-a25c-fd421ec60367")

server <- shinyServer(function(input, output, session) {
  dataInput <- reactive({
    getValues(session)
  })
  
  #output$biaxial <- renderUI({
  #  plotOutput("main_plot",
  #             height = input$plotHeight,
  #             width = input$plotWidth)
  #})
  
  output$biaxial <- renderPlot({
    df <- dataInput()
    
    logticks_flag = ""
    
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    if (length(levels(df$colors)) > 74) {
      qual_col_pals = brewer.pal.info
    } else {
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    }
    
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    # ggplot object
    plt = ggplot()
    
    if (input$x_trans_type == "biexponential") {
      
      trans.fun = flowjo_biexp(pos = input$pos_decades_x, 
                               neg = input$neg_decades_x, 
                               widthBasis = input$width_basis_x)
      inv.fun = flowjo_biexp(pos = input$pos_decades_x, 
                             neg = input$neg_decades_x, 
                             widthBasis = input$width_basis_x, 
                             inverse = TRUE)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)
      
      plt = plt +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
    } else if (input$x_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = 0.5, 
                                    t = 262144,
                                    m = 4.5, 
                                    a = 0)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)
      
      plt = plt +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
    } else if (input$x_trans_type == "log10") {
      df$x_trans = df$.x
      
      plt =  plt + 
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
      
      logticks_flag = "b"
    } else if (input$x_trans_type == "linear") {
      df$x_trans = df$.x
      
      plt = plt +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x
        )
    }
    
    if (input$y_trans_type == "biexponential") {
      
      trans.fun = flowjo_biexp(pos = input$pos_decades_y, 
                               neg = input$neg_decades_y, 
                               widthBasis = input$width_basis_y)
      inv.fun = flowjo_biexp(pos = input$pos_decades_y, 
                             neg = input$neg_decades_y, 
                             widthBasis = input$width_basis_y, 
                             inverse = TRUE)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      plt = plt +
        scale_y_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = 0.5, 
                                    t = 262144,
                                    m = 4.5, 
                                    a = 0)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      plt = plt +
        scale_y_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "log10") {
      df$y_trans = df$.y
      
      plt = plt +
        scale_y_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
      
      logticks_flag = paste0(logticks_flag, "l")
    } else if (input$y_trans_type == "linear") {
      df$y_trans = df$.y
      
      plt = plt +
        scale_y_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
        )
    }
    
    
    plt = plt + geom_point(
      data = df,
      mapping = aes(x = x_trans, y = y_trans, colour = colors),
      size = 5#input$pointSize
    ) +
      # labels
      labs(x = input$x_label,
           y = input$y_label,
           color = input$legend) +
      ggtitle(input$title) +
      scale_color_manual(values=col_vector[1:length(levels(df$colors))]) +
      
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 1
        )
      )
    
    if (logticks_flag != "")
    {
      plt = plt + annotation_logticks(sides = logticks_flag, 
                                      outside = TRUE) + #, 
                                      #short = unit(.05, "cm"),
                                      #mid = unit(0.1, "cm"),
                                      #long = unit(0.15, "cm")) +
        coord_cartesian(clip = "off") 
    }
    
    plt
  })

  output$distribution_x <- renderPlot({
    df <- dataInput()
    
    breaks_x <- as.numeric(unlist(strsplit(input$breaks_x, ",")))
    
    # ggplot object
    hist_x = ggplot()
    if (input$x_trans_type == "biexponential") {

      trans.fun = flowjo_biexp(pos = input$pos_decades_x, 
                               neg = input$neg_decades_x, 
                               widthBasis = input$width_basis_x)
      inv.fun = flowjo_biexp(pos = input$pos_decades_x, 
                             neg = input$neg_decades_x, 
                             widthBasis = input$width_basis_x, 
                             inverse = TRUE)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)

      hist_x = hist_x + geom_density(data = df, mapping = aes(x_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
      
    } else if (input$x_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = 0.5, 
                                    t = 262144,
                                    m = 4.5, 
                                    a = 0)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      x.breaks = custom_logicle_breaks(breaks_x, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$x_trans = trans.fun(df$.x)
      
      hist_x = hist_x + geom_density(data = df, mapping = aes(x_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_x), max(breaks_x))), 
          breaks = x.breaks[[1]],
          labels = x.breaks[[2]]
        )
      
    } else if (input$x_trans_type == "log10") {

      hist_x =  hist_x + geom_density(data = df, mapping = aes(.x)) +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
    } else if (input$x_trans_type == "linear") {

      hist_x = hist_x + geom_density(data = df, mapping = aes(.x)) +
        scale_x_continuous(
          limits = c(min(breaks_x), max(breaks_x)),
          breaks = breaks_x,
        )
    }
    
    plt = hist_x +
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 2
        )
      )
    
    plt
  })
  
  output$distribution_y <- renderPlot({
    df <- dataInput()
    
    print(head(df))
    print(levels(df$colors))
    
    breaks_y <- as.numeric(unlist(strsplit(input$breaks_y, ",")))
    
    # ggplot object
    hist_y = ggplot()
    if (input$y_trans_type == "biexponential") {
      
      trans.fun = flowjo_biexp(pos = input$pos_decades_y, 
                               neg = input$neg_decades_y, 
                               widthBasis = input$width_basis_y)
      inv.fun = flowjo_biexp(pos = input$pos_decades_y, 
                             neg = input$neg_decades_y, 
                             widthBasis = input$width_basis_y, 
                             inverse = TRUE)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      hist_y = hist_y + geom_density(data = df, mapping = aes(y_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "logicle") {
      
      trans.fun <- logicleTransform(w = 0.5, 
                                t = 262144,
                                m = 4.5, 
                                a = 0)
      inv.fun <- inverseLogicleTransform(trans = trans.fun)
      
      y.breaks = custom_logicle_breaks(breaks_y, 
                                       trans.fun = trans.fun, 
                                       inverse.fun = inv.fun)
      df$y_trans = trans.fun(df$.y)
      
      hist_y = hist_y + geom_density(data = df, mapping = aes(y_trans)) +
        scale_x_continuous(
          limits = trans.fun(c(min(breaks_y), max(breaks_y))), 
          breaks = y.breaks[[1]],
          labels = y.breaks[[2]]
        )
      
    } else if (input$y_trans_type == "log10") {
      
      hist_y =  hist_y + geom_density(data = df, mapping = aes(.y)) +
        scale_x_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
          trans = ggallin::pseudolog10_trans,
          labels = custom_log10
        )
    } else if (input$y_trans_type == "linear") {
      
      hist_y = hist_y + geom_density(data = df, mapping = aes(.y)) +
        scale_x_continuous(
          limits = c(min(breaks_y), max(breaks_y)),
          breaks = breaks_y,
        )
    }
    
    plt = hist_y +
      # theme stuff
      theme_classic() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 2
        )
      )
    
    plt
  })
  
})

shinyServer(function(input, output, session) {
  
  dataInput = reactive({getValues(session)})
  mode = reactive({getMode(session)})
  settingsValue = reactiveValues()
  settingsValue$isInitialized = FALSE
  msgReactive = reactiveValues(msg = "")
 
  
  observeEvent(input$saveSettingsBtn, {
    showModal(modalDialog(
      title='Saving',
      span('Saving settings, please wait ...'),
      footer = NULL
    ))
    settings = list(bins=input$bins)
    setSettings(session,settings)
    removeModal()
  })
  
  observeEvent(input$bins, {
    if (settingsValue$isInitialized){
      settingsValue$value = list(bins=input$bins)
    } else {
      settingsValue$value = getSettings(session)
      settingsValue$isInitialized = TRUE
      
      # update ui
      updateSliderInput(session, 'bins', value=settingsValue$value$bins)
      shinyjs::show("bins")
    }
  })
  
  observeEvent(input$runBtn, {
    
    shinyjs::disable("runBtn")
    
    msgReactive$msg = "Running ... please wait ..."

    tryCatch({
      ctx = getCtx(session)
      ctx %>%
        select(.y, .ci, .ri) %>%
        group_by(.ci, .ri) %>%
        summarise(mean = mean(.y)) %>%
        ctx$addNamespace() %>%
        ctx$save()
      
      msgReactive$msg = "Done"
      
    }, error = function(e) {
      msgReactive$msg = paste0("Failed : ", toString(e))
      print(paste0("Failed : ", toString(e)))
    })
  })
  
  output$mode = renderText({ 
    mode()
  })
  
  output$msg = renderText({ 
    msgReactive$msg
  })
  
  output$distPlot <- renderPlot({
    m = mode()
    
    if (!is.null(m) && m == 'run'){
      shinyjs::enable("runBtn")
    }
    
    shinyjs::enable("saveSettingsBtn")
    
    if (is.null(settingsValue$value)){
      settingsValue$value = getSettings(session)
    } 
    
    settings = settingsValue$value
    
    # generate bins based on input$bins from ui.R
    x    <- dataInput()[['.y']]
    bins <- seq(min(x), max(x), length.out = settings$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
})

getSettings = function(session) {
  fileSettings = getFileSettings(session)
  if (is.null(fileSettings)){
    settings = list(bins=30)
    return(settings)
  }
  ctx = getCtx(session)
  bytes = ctx$client$fileService$download(fileSettings$id)
  settings = fromJSON(rawToChar(bytes))
  return(settings)
}

setSettings = function(session, settings){
  ctx = getCtx(session)
  fileSettings = getFileSettings(session)
  if (!is.null(fileSettings)){
    ctx$client$fileService$delete(fileSettings$id,fileSettings$rev)
  }
  
  workflowId = getWorkflowId(session)
  stepId = getStepId(session)
  workflow = ctx$client$workflowService$get(workflowId)
  
  fileDoc = FileDocument$new()
  fileDoc$name = 'webapp-operator-settings'
  fileDoc$projectId = workflow$projectId
  fileDoc$acl$owner = workflow$acl$owner
  fileDoc$metadata$contentType = 'application/octet-stream'
  
  metaWorkflowId = Pair$new()
  metaWorkflowId$key = 'workflow.id'
  metaWorkflowId$value = workflowId
  
  metaStepId = Pair$new()
  metaStepId$key = 'step.id'
  metaStepId$value = stepId
  
  fileDoc$meta = list(metaWorkflowId, metaStepId)
  
  content = toJSON(settings)
  bytes = charToRaw(content)
  fileDoc = ctx$client$fileService$upload(fileDoc, bytes)
  fileDoc
}

getFileSettings = function(session) {
  ctx = getCtx(session)
  workflowId = getWorkflowId(session)
  stepId = getStepId(session)
  
  files = ctx$client$fileService$findFileByWorkflowIdAndStepId(
    startKey=list(workflowId,stepId),
    endKey=list(workflowId,''),
    descending=TRUE, limit=1 )
  
  if (length(files) > 0) {
    return (files[[1]])
  } 
  
  return (NULL)
}

getMode = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}

getWorkflowId = function(session){
  workflowId = getOption("tercen.workflowId")
  if (!is.null(workflowId)) return(workflowId)
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["workflowId"]])
}

getStepId = function(session){
  stepId = getOption("tercen.stepId")
  if (!is.null(stepId)) return(stepId)
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["stepId"]])
}

getCtx = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)

  token = query[["token"]]
  taskId = query[["taskId"]]

  # create a Tercen context object using the token
  ctx = tercenCtx(taskId=taskId, authToken=token)
  
  # dev
  # ctx = tercenCtx()
  
  return(ctx)
}

getValues = function(session){
  ctx = getCtx(session)
  data = ctx %>% select(.y , .ci , .ri )
  return(data)
}
