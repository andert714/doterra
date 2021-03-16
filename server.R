function(input, output){
  df <- reactive({
    if(input$tab == 'current' & input$measure == 'Offered Calls'){
      df <- dfs$`Offered Calls`
    } else if(input$tab == 'current' & input$measure == 'AHT'){
      df <- dfs$AHT
    } else if(input$tab == 'past' & input$measure == 'Offered Calls'){
      df <- dfs$`Past Offered Calls`
    } else if(input$tab == 'past' & input$measure == 'AHT'){
      df <- dfs$`Past AHT`
    }
    filter(df, market == input$market)
  })
  output$horizon <- renderUI({
    validate(need(input$tab == 'past' & nrow(df()) > 0, ''))
    max <- max(as.numeric(df()$ds - df()$cutoff))
    sliderInput('horizon', 'Horizon', 1, min(max, 90), 1)
  })
  output$dates <- renderUI({
    validate(need(input$tab == 'past' & nrow(df()) > 0, ''))
    start_date <- min(df()$ds)
    dateRangeInput('dates', 'Accuracy from', start_date, today() - 1, start_date, today() - 1)
  })
  output$graph <- renderDygraph({
    validate(need(input$password == 'password', 'Please input password.'))
    validate(need(nrow(df()) > 0, 'This market does not have an AHT forecast yet.'))
    if(input$tab == 'current'){
      ts_graph(df())
    } else if(input$tab == 'past'){
      validate(need(!is.null(input$horizon) & !is.null(input$dates), ""))
      df() %>% 
        filter(ds - cutoff == input$horizon) %>% 
        ts_graph(input$dates)
    }
  })
  output$table <- renderTable({
    validate(need(input$password == 'password' & nrow(df()) > 0, ''))
    if(input$tab == 'current'){
      ts_max(df())
    } else if(input$tab == 'past'){
      validate(need(!is.null(input$horizon) & !is.null(input$dates), ""))
      df() %>%
        filter(ds - cutoff == input$horizon, between(ds, input$dates[1], input$dates[2])) %>%
        ts_accuracy %>%
        pivot_longer(everything(), names_to = 'Metric', values_to = 'Value')
    }
  })
}

