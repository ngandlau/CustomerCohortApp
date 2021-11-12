server <- function(input, output, session){
  
  # Misc  ------------------------------------------------------------
  
  dim <- reactive({
    nPeriodsToZero <- NPeriodsToZero(retRate = input$retRate, precision = 1e-5)
    nPeriodsToZero * 4
  })
  
  customerEquity <- reactive({
    CustomerEquity(
      t = input$valuationPeriod,
      profitMatrix = profitMatrix(),
      discRate = input$discRate
    )
  })
  
  

# Update Inputs -----------------------------------------------------------

  observe({
    updateNumericInput(
      session,
      inputId = "revMax",
      value = input$rev
    )
    updateNumericInput(
      session,
      inputId = "nNewMax",
      value = input$nNew
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
  })
  
  
# Cohort Matrices -----------------------------------------------------------

  custMatrix <- reactive({
    if (input$advancedSettings == F){
      custMatrix <- GenConstantValues(input$nNew, dim())
    } else {
      nAcquired <- sapply(1:dim(), function(t) input$nNew + (t-1)*input$nNewIncrease)
      nAcquired[nAcquired > input$nNewMax] <- input$nNewMax
      custMatrix <- GenConstantValues(1, dim()) * nAcquired
    }
    retainedCustMatrix <- custMatrix * survMatrix()
  })
  
  retMatrix <- reactive({
    retMatrix <- GenConstantValues(value = input$retRate, dim = dim())
    diag(retMatrix) <- 1
    if (input$advancedSettings == T){
      retGrowthMatrix <- GenLinearAgeEffectMatrix(
        slope = input$retRateIncrease,
        dim = dim()
      )
      retMatrix <- retMatrix + retGrowthMatrix
      retMatrix[retMatrix > input$retRateMax] <- input$retRateMax
      diag(retMatrix) <- 1
    }
    return(retMatrix)
  })
  
  survMatrix <- reactive({
    SurvivalRates(retMatrix = retMatrix())
  })
  
  revMatrix <- reactive({
    if (input$advancedSettings == F){
      revMatrix <- GenConstantValues(input$rev, dim())
    } else {
      revMatrix <- GenConstantValues(input$rev, dim())
      revGrowthMatrix <- GenLinearAgeEffectMatrix(
        slope = input$revIncrease,
        dim = dim()
      )
      revMatrix <- revMatrix + revGrowthMatrix
      revMatrix[revMatrix > input$revMax] <- input$revMax
    }
    revMatrix * custMatrix()
  })
  
  varCostMatrix <- reactive({
    contributionMargin <- input$cm
    varCost <- revMatrix() * (1-contributionMargin)
  })
  
  retCostMatrix <- reactive({
    retCostMatrix <- GenConstantValues(input$retCost, dim())
    retCostMatrix * custMatrix()
  })
  
  cacMatrix <- reactive({
    if (input$advancedSettings == F){
      cacMatrix <- GenDiagonalValues(input$cac, dim())
    } else {
      cac <- sapply(1:dim(), function(t) input$cac + (t-1)*input$cacIncrease)
      cac[cac > input$cacMax] <- input$cacMax
      cacMatrix <- GenDiagonalValues(cac, dim = length(cac))
    }
    cacMatrix * custMatrix()
  })
  
  varProdCostMatrix <- reactive({
    revMatrix() * input$cm
  })
  
  costMatrix <- reactive({
    costMatrix <- varCostMatrix() + retCostMatrix() + cacMatrix()
  })
  
  profitMatrix <- reactive({
    revMatrix() - costMatrix()
  })
  
  arpuMatrix <- reactive({
    if (input$revenueType == "revenue"){
      return(revMatrix())
    } else if (input$revenueType == "arpu1"){
      arpuMatrix <- revMatrix() / custMatrix()
      return(arpuMatrix)
    } else if (input$revenueType == "arpu2"){
      nCustAtAcquisition <- custMatrix() %>% TransformView() %>% .[, 1]
      arpuMatrix <- revMatrix() / nCustAtAcquisition
      return(arpuMatrix)
    }
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
  
  # Tab: Period View ------------------------------------------------------
  
  output$periodView_profitLossStatement <- renderDT({
    dataProfitLoss <- ComputeProfitLossStatement(
      showNPeriods = input$valuationPeriod,
      revMatrix = revMatrix(),
      varCostMatrix = varCostMatrix(), 
      cacMatrix = cacMatrix(), 
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
        cacMatrix = cacMatrix(),
        retCostMatrix = retCostMatrix(),
        varCostMatrix = varCostMatrix(),
        profitMatrix = profitMatrix(),
        discMatrix = discMatrix(),
        n = input$valuationPeriod,
        isDiscounted = F
      ),
      digits = 0
    )
  })
  
  # Tab: Cohort Arrays --------------------------------------------------
  
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
    X = cacMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "retCost",
    X = retCostMatrix,
    n = n
  )
  
  cohortChartsServer(
    id = "varCost",
    X = varCostMatrix,
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
    id = "revMatrix",
    X = arpuMatrix,
    rowColSumsCP = c(T, T),
    rowColSumsCA = c(T, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
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

  cohortTableServer(
    id = "cacMatrix",
    X = cacMatrix,
    rowColSumsCP = c(F, T),
    rowColSumsCA = c(F, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
  )

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
    id = "varCostMatrix",
    X = varCostMatrix,
    rowColSumsCP = c(T, T),
    rowColSumsCA = c(T, F),
    n = n,
    isPercentage = F,
    isCurrency = T,
    digits = 0
  )
  
# Tab: Dashboard --------------------------------------------------------
  
# ... Dashboard ------------------------------------------------
  
  
  plotPeriodCohortViewServer(
    id = "dashboard_nCustomers",
    X = custMatrix,
    fromPeriod = 1,
    toPeriod = n,
    geomType = "geom_col",
    title = NULL,
    subtitle = NULL,
    xlab = "Period",
    ylab = "Number of Customers",
    digits = 0,
    isDollars = F
  )
  
  plotPeriodCohortViewServer(
    id = "dashboard_revenues",
    X = revMatrix,
    fromPeriod = 1,
    toPeriod = n,
    geomType = "geom_col",
    title = NULL,
    subtitle = NULL,
    xlab = "Period",
    ylab = "Revenues",
    digits = 0,
    isDollars = T
  )
  
  plotPeriodCohortViewServer(
    id = "dashboard_ARPU",
    X = reactive(revMatrix()/custMatrix()),
    fromPeriod = 1,
    toPeriod = n,
    geomType = "geom_line",
    title = NULL,
    subtitle = NULL,
    xlab = "Period",
    ylab = "Average Revenue per Customer",
    digits = 0,
    isDollars = T,
    colAggFunc = colMeans
  )
  
  plotPeriodCohortViewServer(
    id = "dashboard_marketingCosts",
    X = reactive(retCostMatrix()+cacMatrix()),
    fromPeriod = 1,
    toPeriod = n,
    geomType = "geom_col",
    title = NULL,
    subtitle = NULL,
    xlab = "Period",
    ylab = "Cost",
    digits = 0,
    isDollars = T
  )
  
  output$dashboard_operatingMargin <- renderPlot({
    if (input$dashboard_operatingMarginSwitch == "Period View"){
      PlotOperatingMarginPerPeriod(
        fromPeriod = 1,
        toPeriod = input$valuationPeriod,
        revMatrix = revMatrix(),
        profitMatrix = profitMatrix()
      )
    } else {
      PlotOperatingMarginByCohort(
        fromPeriod = 1,
        toPeriod = input$valuationPeriod,
        profitMatrix = profitMatrix(),
        revMatrix = revMatrix()
      )
    }
  })
  
  output$dashboard_profitsBeforeFixedCosts <- renderPlot({
    if (input$dashboard_profitsBeforeFixedCostsSwitch == "Period View"){
      PlotProfitsPerPeriod(
        fromPeriod = 1,
        toPeriod = input$valuationPeriod,
        profitMatrix = profitMatrix(),
        fixedCosts = input$fixedCost,
        afterFixedCosts = F
      )
    } else {
      PlotProfitsByCohort(
        fromPeriod = 1,
        toPeriod = input$valuationPeriod,
        profitMatrix = profitMatrix()
      )
    }
  })
  
  output$dashboard_costs <- renderPlot({
    PlotCostsPerPeriod(
      fromPeriod = 1,
      toPeriod = input$valuationPeriod,
      varCostMatrix = varCostMatrix(),
      retCostMatrix = retCostMatrix(),
      cacMatrix = cacMatrix(),
      fixedCost = input$fixedCost
    )
  })
  
  output$dashboard_profitsAfterFixedCosts <- renderPlot({
    PlotProfitsPerPeriod(
      fromPeriod = 1,
      toPeriod = input$valuationPeriod,
      profitMatrix = profitMatrix(),
      fixedCosts = input$fixedCost,
      afterFixedCosts = T
    )
  })

# ... SaaS Dashboard -------------------------------------------------------

  output$dashboard_overallNRR <- renderPlot({
    PlotNetRevenueRetentionRate(
      NetRevenueRetentionRate(
        revMatrix(),
        n = input$valuationPeriod)
    )
  })
  
  output$dashboard_overallCRR <- renderPlot({
    PlotCustomerRetentionRate(
      CustomerRetentionRate(
        custMatrix = custMatrix(),
        n = input$valuationPeriod
      )
    )
  })
  
  output$dashboard_newExistLostCustomers <- renderPlot({
    PlotNewExistLostCustomers(
      NewExistLostCustomers(
        custMatrix = custMatrix(),
        retRate = input$retRate,
        n = input$valuationPeriod
      )
    )
  })
  
  output$dashboard_newExistLostRevenues <- renderPlot({
    PlotNewExistLostRevenue(
      NewExistLostRevenue(
        revMatrix = revMatrix(),
        retRate = input$retRate,
        n = input$valuationPeriod
      )
    )
  })
  
  output$dashboard_shareOfExistVsNewRevenue <- renderPlot({
    PlotShareOfRevenueFromExistVsNew(
      dt = ShareOfRevenueFromExistVsNew(
        revMatrix = revMatrix(),
        n = input$valuationPeriod
      )
    )
  })
  
  output$dashboard_quickRatio <- renderPlot({
    PlotQuickRatio(
      fromPeriod = 1,
      toPeriod = input$valuationPeriod,
      newExistLostRevenue = NewExistLostRevenue(
        revMatrix = revMatrix(),
        retRate = input$retRate,
        n = input$valuationPeriod
      )
    )
  })
  
  output$dashboard_cohortRevenueLTVCurves <- renderPlot({
    PlotCumulativeCohortLTVs(
      CumulativeCohortLTVs(
        revMatrix(),
        n = input$valuationPeriod)
    )
  })
  
  output$dashboard_cohortProfitContributionAfterProductCost <- renderPlot({
    PlotCumulativeProfitContributionCurves(
      adjProfitMatrix = CumulativeProfitContribution(
        profitMatrix = profitMatrix(),
        cacMatrix = cacMatrix(),
        retCostMatrix = retCostMatrix(),
        varProdCostMatrix = varProdCostMatrix(),
        beforeAcqCost = T,
        beforeRetCost = T,
        beforeVarProdCost = F,
        n = input$valuationPeriod
      ),
      title = NULL,
      subtitle = NULL
    )
  })
  
  output$dashboard_cohortProfitContributionAfterVariableCost <- renderPlot({
    PlotCumulativeProfitContributionCurves(
      adjProfitMatrix = CumulativeProfitContribution(
        profitMatrix = profitMatrix(),
        cacMatrix = cacMatrix(),
        retCostMatrix = retCostMatrix(),
        varProdCostMatrix = varProdCostMatrix(),
        beforeAcqCost = T,
        beforeRetCost = F,
        beforeVarProdCost = F,
        n = input$valuationPeriod
      ),
      title = NULL,
      subtitle = NULL
    )
  })
  
  output$dashboard_cohortProfitContributionAfterAllCost <- renderPlot({
    PlotCumulativeProfitContributionCurves(
      adjProfitMatrix = CumulativeProfitContribution(
        profitMatrix = profitMatrix(),
        cacMatrix = cacMatrix(),
        retCostMatrix = retCostMatrix(),
        varProdCostMatrix = varProdCostMatrix(),
        beforeAcqCost = F,
        beforeRetCost = F,
        beforeVarProdCost = F,
        n = input$valuationPeriod
      ),
      title = NULL,
      subtitle = NULL
    )
  })
  
  output$dashboard_cohortLTVCACCurves <- renderPlot({
    PlotCohortedContributionLTVCACCurves(
      CohortedContributionLTVCACCurves(
        profitMatrix = profitMatrix(),
        cacMatrix = cacMatrix(),
        n = input$valuationPeriod
      )
    )
  })
  
  # output$dashboard_growthRatesRevenues <- renderPlot({
  #   PlotGrowthRates(
  #     dtGrowthRates = GrowthRates(X = revMatrix(), n = input$valuationPeriod),
  #     title = "Growth Rate of Revenues"
  #   )
  # })
  # 
  # output$dashboard_growthRatesCustomers <- renderPlot({
  #   PlotGrowthRates(
  #     dtGrowthRates = GrowthRates(X = custMatrix(), n = input$valuationPeriod),
  #     title = "Growth Rate of Number of Customers"
  #   )
  # })
  
  
}

