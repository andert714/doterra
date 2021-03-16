dashboardPage(
  dashboardHeader(title = 'Forecast Accuracy'),
  dashboardSidebar(
    passwordInput('password', 'Password'),
    selectInput('measure', 'Measure', c('Offered Calls', 'AHT')),
    selectInput('market', 'Market', drop_na(market_df, 'start_oc')$market, 'US'),
    sidebarMenu(
      menuItem('Current Forecast', tabName = 'current'),
      menuItem('Past Forecasts', tabName = 'past'),
      id = 'tab'
    ),
    uiOutput('horizon'),
    uiOutput('dates')
  ),
  dashboardBody(
    box(dygraphOutput('graph', height = '600px'), width = 10, height = '600px'),
    box(tableOutput('table'), width = 2, height = '600px')
  )
)


