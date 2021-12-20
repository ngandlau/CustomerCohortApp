server <- function(input, output, session){
  

  
  customerEquity <- reactive({
    CustomerEquity(
      t = input$valuationPeriod,
      profitMatrix = profitMatrix(),
      discRate = input$discRate
    )
  })
  
# Update Dynamic UI Inputs -----------------------------------------------------------

  observe({
    updateNumericInput(
      session,
      inputId = "revMax",
      value = input$revIntercept
    )
    updateNumericInput(
      session,
      inputId = "revMin",
      value = input$revIntercept
    )
    updateNumericInput(
      session,
      inputId = "nNewMin",
      value = input$nNew
    )
    updateNumericInput(
      session,
      inputId = "nNewMax",
      value = input$nNew
    )
    updateNumericInput(
      session,
      inputId = "retRateMin",
      value = input$retRate
    )
    updateNumericInput(
      session,
      inputId = "retRateMax",
      value = input$retRate
    )
    updateNumericInput(
      session,
      inputId = "cacMax",
      value = input$cac
    )
    updateNumericInput(
      session,
      inputId = "cacMin",
      value = input$cac
    )
  })
  
# Data-Generating Process -----------------------------------------------------------

  dim <- reactive({
    nPeriodsToZero <- NPeriodsToZero(retRate = input$retRate, precision = 1e-5)
    nPeriodsToZero * 4
  })
  
  custMatrix <- reactive({
    if (input$advancedSettings == F){
      custMatrix <- GenConstantValues(input$nNew, dim())
    } else {
      nAcquired <- sapply(1:dim(), function(t) input$nNew + (t-1)*input$nNewSlope)
      
      nAcquired[nAcquired < input$nNewMin] <- input$nNewMin
      nAcquired[nAcquired > input$nNewMax] <- input$nNewMax
      nAcquired[nAcquired < 0] <- 0

      custMatrix <- GenConstantValues(1, dim()) * nAcquired
    }
    retainedCustMatrix <- custMatrix * survMatrix()
  })
  
  retMatrix <- reactive({
    if (input$advancedSettings == F){
      retMatrix <- GenConstantValues(value = input$retRate, dim = dim())
    } else {
      ageEffects <- sapply(0:(dim()-1), function(a) a*input$retRateAgeSlope)
      cohortEffects <- sapply(0:(dim()-1), function(c) c*input$retRateCohortSlope)
      periodEffects <- rep(0, dim())
      retMatrix <- GenAPCMatrix(
        constant = input$retRate,
        ageEffects = ageEffects,
        periodEffects = periodEffects,
        cohortEffects = cohortEffects
      )

      # Retention rates must be within the user-specified min and max
      retMatrix[retMatrix < input$retRateMin] <- input$retRateMin
      retMatrix[retMatrix > input$retRateMax] <- input$retRateMax
      
      # Retention rates must be within [0, 1]
      retMatrix[retMatrix > 1] <- 1
      retMatrix[retMatrix < 0] <- 0
    }
    # Assumption: Customers stay for at least one full period.
    diag(retMatrix) <- 1
    return(retMatrix)
  })
  
  survMatrix <- reactive({
    SurvivalRates(retMatrix = retMatrix())
  })
  
  revMatrix <- reactive({
    if (input$advancedSettings == F){
      revMatrix <- GenConstantValues(input$revIntercept, dim())
    } else {
      ageEffects <- sapply(0:(dim()-1), function(a) a*input$revAgeSlope)
      cohortEffects <- sapply(0:(dim()-1), function(c) c*input$revCohortSlope)
      periodEffects <- rep(0, dim())
      revMatrix <- GenAPCMatrix(
        constant = input$revIntercept,
        ageEffects = ageEffects,
        periodEffects = periodEffects,
        cohortEffects = cohortEffects
      )
      
      revMatrix[revMatrix > input$revMax] <- input$revMax
      revMatrix[revMatrix < input$revMin] <- input$revMin
    }
    return(revMatrix * custMatrix())
  })
  
  prodCostMatrix <- reactive({
    prodCostMatrix <- revMatrix() * (1-input$contributionMargin)
  })
  
  retCostMatrix <- reactive({
    retCostMatrix <- GenConstantValues(input$retCost, dim())
    retCostMatrix * custMatrix()
  })
  
  acqCostMatrix <- reactive({
    if (input$advancedSettings == F){
      cacMatrix <- GenDiagonalValues(input$cac, dim())
    } else {
      cac <- sapply(1:dim(), function(t) input$cac + (t-1)*input$cacSlope)
      
      # cac must be within the user-specified min and max
      cac[cac < input$cacMin] <- input$cacMin
      cac[cac > input$cacMax] <- input$cacMax
      
      cacMatrix <- GenDiagonalValues(cac, dim = length(cac))
    }
    cacMatrix * custMatrix()
  })
  
  varProdCostMatrix <- reactive({
    revMatrix() * input$contributionMargin
  })
  
  costMatrix <- reactive({
    costMatrix <- prodCostMatrix() + retCostMatrix() + acqCostMatrix()
  })
  
  profitMatrix <- reactive({
    revMatrix() - costMatrix()
  })
  

  revRetMatrix <- reactive({
    if (input$revRetType == "prev"){
      revRetMatrix <- NetRevenueRetentionRateMatrix(
        revMatrix = revMatrix(),
        relativeToPreviousYear = T
      )
    } else if (input$revRetType == "acq"){
      revRetMatrix <- NetRevenueRetentionRateMatrix(
        revMatrix = revMatrix(),
        relativeToPreviousYear = F
      )
    }
    return(revRetMatrix)
  })
  
  discMatrix <- reactive({
    GenDiscountMatrix(
      discRate = input$discRate,
      dim = dim(),
      startFromPeriod = input$valuationPeriod
    )
  })
  
  # Tab: Valuation  ----------------------------------------------------
  
  output$illustrativeValuationTable <- renderDT({
    PlotIllustrativeValuationTable(
      dt = IllustrativeValuationTable(
        t = input$valuationPeriod,
        profitMatrix = profitMatrix(), 
        discMatrix = discMatrix(), 
        isDiscounted = input$isDiscounted, 
        fixedCost = input$fixedCost,
        discRate = input$discRate,
        ce = customerEquity()[["ce"]],
        noa = input$noa,
        debt = input$debt
      )
    )
  })
  
  output$stepByStepCBCVTable <- renderUI({
    ceCurr <- customerEquity()[["ceCurrent"]]
    ceFut <- customerEquity()[["ceFuture"]]
    terminalValueFixedCost <- input$fixedCost + input$fixedCost * (1/input$discRate)
    
    PlotStepByStepCBCVTable(
      ceCurr = round(ceCurr, 2),
      ceFuture = round(ceFut, 2),
      tvFixedCost = round(terminalValueFixedCost, 2),
      noa = round(input$noa, 2),
      debt = round(input$debt, 2)
    )
  })
  
  output$stepByStepDCFTable <- renderUI({
    PlotStepByStepDCFTable(
      valuationPeriod = input$valuationPeriod, 
      profitMatrix = profitMatrix(), 
      discMatrix = discMatrix(), 
      discRate = input$discRate, 
      fixCost = input$fixedCost, 
      noa = input$noa, 
      debt = input$debt
    )
  })
  
  output$valuation_discProfitsPerPeriod <- renderPlot({
    PlotDiscountedProfitContributionPerPeriod(
      DiscountedProfitContributionPerPeriod(
        valuationPeriod = input$valuationPeriod,
        n = 14,
        profitMatrix = profitMatrix(),
        discMatrix = discMatrix()
      )
    )
  })
  
  output$valuation_discProfitsPerCohort <- renderPlot({
    PlotDiscountedProfitContributionPerCohort(
      DiscountedProfitContributionPerCohort(
        valuationPeriod = input$valuationPeriod,
        n = 14,
        profitMatrix = profitMatrix(),
        discMatrix = discMatrix()
      )
    )
    
  })
  

  # Tab: Customer Equity Reporting ------------------------------------------
  
  output$ce_customerEquityReporting <- renderDT({
    PlotCustomerEquityReporting(
      t = input$valuationPeriod, 
      custMatrix = custMatrix(), 
      revMatrix = revMatrix(),
      profitMatrix = profitMatrix(),
      discMatrix = discMatrix(),
      discRate = input$discRate
    )
  })
  
  
  # Tab: Period View ------------------------------------------------------
  
  output$periodView_profitLossStatement <- renderDT({
    dataProfitLoss <- ComputeProfitLossStatement(
      n = input$valuationPeriod,
      revMatrix = revMatrix(),
      prodCostMatrix = prodCostMatrix(), 
      acqCostMatrix = acqCostMatrix(), 
      retCostMatrix = retCostMatrix(), 
      fixedCost = input$fixedCost
    )
    PlotProfitLossStatement(dataProfitLoss)
  })
  
  # Tab: Cohort View ------------------------------------------------------
  
  output$cohortView_cohortViewTable <- renderDT({
    PlotCohortViewTable(
      CohortViewTable(
        revMatrix = revMatrix(),
        costMatrix = costMatrix(),
        acqCostMatrix = acqCostMatrix(),
        retCostMatrix = retCostMatrix(),
        prodCostMatrix = prodCostMatrix(),
        profitMatrix = profitMatrix(),
        discMatrix = discMatrix(),
        n = input$valuationPeriod,
        isDiscounted = F
      ),
      digits = 0
    )
  })
  
  # Tab: Cohort Tables --------------------------------------------------
  
  n <- reactive(input$valuationPeriod)
  
  cohortChartsServer(
    id = "nCustomers",
    X = custMatrix,
    n = n
  )
  
  custRetMatrix <- reactive({
    if (input$retentionType == "retentionRate"){
      custRetMatrix <- retMatrix()
    } else if (input$retentionType == "survivalRate"){
      custRetMatrix <- survMatrix()
    }
    return(custRetMatrix)
  })
  
  cohortChartsServer(
    id = "customerRetention", 
    X = custRetMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "revenue",
    X = arpuMatrix,
    n = n
  )
  
  arpuMatrix <- reactive({
    if (input$arrays_revenue_metric == "revenue"){
      X <- revMatrix()
    } else if (input$arrays_revenue_metric == "arpu1"){
      X <- revMatrix() / custMatrix()
    } else if (input$arrays_revenue_metric == "arpu2"){
      nCustAtAcquisition <- custMatrix() %>% TransformView() %>% .[, 1]
      X <- revMatrix() / nCustAtAcquisition
    }
    return(X)
  })
  
  output$arrays_revenue <- renderDT({
    if (input$arrays_revenue_metric == "revenue"){
      if (input$arrays_revenue_arrayType == "cohort-period"){
        addRowSums <- T
        addColSums <- T
      } else {
        addRowSums <- T
        addColSums <- F
      }
      Xf <- FormatCohortTableDT(
        X = revMatrix(),
        to = input$valuationPeriod,
        addRowSums = addRowSums,
        addColSums = addColSums,
        view = input$arrays_revenue_arrayType
      )
      plt <- PlotCohortTableDT(
        X = Xf,
        isPercentage = F,
        isCurrency = T,
        digits = 0
      )
    } else if (input$arrays_revenue_metric == "arpu1"){
      # revenue relative to previous period
      arpuMatrix <- revMatrix() / custMatrix()
      Xf <- FormatCohortTableDT(
        X = arpuMatrix,
        to = input$valuationPeriod,
        addRowSums = F,
        addColSums = F,
        view = input$arrays_revenue_arrayType
      )
      plt <- PlotCohortTableDT(
        X = Xf,
        isPercentage = F,
        isCurrency = T,
        digits = 0
      )
    } else if (input$arrays_revenue_metric == "arpu2"){
      # revenue relative to acq period
      nCustAtAcquisition <- custMatrix() %>% TransformView() %>% .[, 1]
      arpuMatrix <- revMatrix() / nCustAtAcquisition
      Xf <- FormatCohortTableDT(
        X = arpuMatrix,
        to = input$valuationPeriod,
        addRowSums = F,
        addColSums = F,
        view = input$arrays_revenue_arrayType
      )
      plt <- PlotCohortTableDT(
        X = Xf,
        isPercentage = F,
        isCurrency = T,
        digits = 0
      )
      
    }
  })
  
  cohortChartsServer(
    id = "revRet",
    X = revRetMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "profits",
    X = profitMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "cost",
    X = costMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "cac",
    X = helperAcqCostMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "retCost",
    X = retCostMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "prodCost",
    X = prodCostMatrix,
    n = n
  )
  
  cohortTableServer(
    id = "custMatrix",
    X = custMatrix,
    rowColSumsCP = c(F, T),
    rowColSumsCA = c(F, F),
    n = n,
    isPercentage = F,
    isCurrency = F,
    digits = 2
  )
  
  cohortTableServer(
    id = "revRetMatrix",
    X = revRetMatrix,
    rowColSumsCP = c(F, F),
    rowColSumsCA = c(F, F),
    n = n,
    isPercentage = T,
    isCurrency = F,
    digits = 2
  )

  cohortTableServer(
    id = "custRetMatrix",
    X = custRetMatrix,
    rowColSumsCP = c(F, F),
    rowColSumsCA = c(F, F),
    n = n,
    isPercentage = T,
    isCurrency = F,
    digits = 2
  )

  cohortTableServer(
    id = "profitMatrix",
    X = profitMatrix,
    rowColSumsCP = c(T, T),
    rowColSumsCA = c(T, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
  )
  
  cohortTableServer(
    id = "costMatrix",
    X = costMatrix,
    rowColSumsCP = c(T, T),
    rowColSumsCA = c(T, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
  )

  helperAcqCostMatrix <- reactive({
    if (input$acqCostType == "acqCostPerCohort"){
      acqCostMatrix <- acqCostMatrix()
    } else if (input$acqCostType == "acqCostPerCustomer"){
      acqCostMatrix <- acqCostMatrix() / custMatrix()
    }
    return(acqCostMatrix)
  })
  
  output$arrays_acqCost <- renderDT({
    acqCostMatrix <- helperAcqCostMatrix()
    cohortTable <- FormatCohortTableDT(
      X = acqCostMatrix,
      to = input$valuationPeriod,
      addRowSums = F,
      addColSums = F,
      view = input$arrays_acqCost_arrayType
    )
    plt <- PlotCohortTableDT(
      X = cohortTable,
      isPercentage = F,
      isCurrency = T,
      digits = 0
    )
  })

  cohortTableServer(
    id = "retCostMatrix",
    X = retCostMatrix,
    rowColSumsCP = c(T, T),
    rowColSumsCA = c(T, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
  )

  cohortTableServer(
    id = "prodCostMatrix",
    X = prodCostMatrix,
    rowColSumsCP = c(T, T),
    rowColSumsCA = c(T, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
  )
  
# Tab: Dashboard --------------------------------------------------------
  
  output$dashboard_nCustomers <- renderPlot({
    PlotDashboardNCustomers(
      custMatrix = custMatrix(),
      toPeriod = input$valuationPeriod,
      plotType = input$dashboard_nCustomersSwitch
    )
  })
  
  output$dashboard_revenues <- renderPlot({
    PlotDashboardRevenues(
      revMatrix = revMatrix(),
      toPeriod = input$valuationPeriod,
      plotType = input$dashboard_revSwitch
    )
  })
  
  output$dashboard_ARPU <- renderPlot({
    PlotDashboardARPU(
      arpuMatrix = revMatrix() / custMatrix(),
      toPeriod = input$valuationPeriod,
      plotType = input$dashboard_arpuSwitch
    )
  })
  
  output$dashboard_costs <- renderPlot({
    PlotDashboardCosts(
      prodCostMatrix = prodCostMatrix(),
      retCostMatrix = retCostMatrix(),
      acqCostMatrix = acqCostMatrix(),
      fixedCost = input$fixedCost,
      toPeriod = input$valuationPeriod,
      plotType = input$dashboard_costSwitch
    )
  })
  
  output$dashboard_profits <- renderPlot({
    PlotDashboardProfits(
      revMatrix = revMatrix(),
      profitMatrix = profitMatrix(),
      fixedCost = input$fixedCost,
      toPeriod = input$valuationPeriod,
      plotType = input$dashboard_profitsSwitch
    )  
  })
  
  output$dashboard_cohortLifetimeCurves <- renderPlot({
    PlotDashboardCohortLifetimeCurves(
      revMatrix = revMatrix(),
      profitMatrix = profitMatrix(),
      prodCostMatrix = prodCostMatrix(),
      retCostMatrix = retCostMatrix(),
      acqCostMatrix = acqCostMatrix(),
      toPeriod = input$valuationPeriod,
      plotType = input$dashboard_cohortLifetimeCurvesSwitch
    )
  })
  
}

