library(tidyverse)
library(data.table)
library(htmlTable)
library(DT)
library(RColorBrewer)

# Tab: Valuation ----------------------------------------------------------

CustomerEquity <- function(t, profitMatrix, discRate){
  if (t==1){
    ceCurrent <- profitMatrix[1, ]
    profitsFutureCustomers <- profitMatrix[2:nrow(profitMatrix), ]
    ceFuture <- colSums(profitsFutureCustomers, na.rm = T)
  } else {
    profitsCurrentCustomers <- profitMatrix[1:t, t:ncol(profitMatrix)]
    profitsFutureCustomers <- profitMatrix[(t+1):nrow(profitMatrix), t:ncol(profitMatrix)]
    
    ceCurrent <- colSums(profitsCurrentCustomers, na.rm = T)
    ceFuture <- colSums(profitsFutureCustomers, na.rm = T)
  }
  
  discFactors <- GenDiscountFactors(discRate = discRate,  
                                    lengthOut = ncol(profitMatrix)-t+1)
  
  ceCurrent <- sum(ceCurrent * discFactors, na.rm = T)
  ceFuture <- sum(ceFuture * discFactors, na.rm = T)
  
  return(list(
    ceCurrent = ceCurrent,
    ceFuture = ceFuture,
    ce = ceCurrent + ceFuture
  ))
}


IllustrativeValuationTable <- function(t, profitMatrix, discMatrix, isDiscounted,
                                       fixedCost, discRate, ce, noa, debt){
  if (isDiscounted == T){
    X <- profitMatrix * discMatrix
  } else if (isDiscounted == F){
    X <- profitMatrix
  }
  
  if (t == 1){
    existCohorts <- X[1, 1:3]
    existCohortLTV <- sum(X[1, ])
  } else {
    existCohorts <- colSums(X[1:t, t:(t+2)], na.rm = T)
    existCohortLTV <- sum(X[1:t, t:ncol(X)], na.rm = T)
  }
  
  # rows 2-4 show the future cohorts (t+1), (t+2), (t+3)
  # and their profits in t, t+1, t+2
  futCohorts <- X[(t+1):(t+1+2), t:(t+2)]
  futCohortsLTV <- rowSums(X[(t+1):(t+2), t:ncol(X)], na.rm = T)
  
  tbl <- rbind(existCohorts, futCohorts)
  rownames(tbl) <- sapply(t:(t+3), function(c){
    paste0("Cohort ", c)
  })
  
  if (t > 1) {
    newRownames <- rownames(tbl)
    newRownames <- newRownames[2: length(newRownames)]
    rownames(tbl) <- c(paste0("Cohorts 1-", t), newRownames)
  }
  
  # add period-by-period profits
  tbl <- rbind(tbl, colSums(tbl, na.rm = T))
  
  # add a column with cohort lifetime values 
  if (isDiscounted == T){
    cohortLTVs <- c(existCohortLTV, futCohortsLTV, NA, ce)
  } else if (isDiscounted == F){
    cohortLTVs <- c(existCohortLTV, futCohortsLTV, NA, NA)
  }
  
  tbl <- cbind(tbl, cohortLTVs)
  
  # Add fixed cost
  fixedCosts <- rep(fixedCost, 3)
  if (isDiscounted == T){
    fixedCosts <- fixedCost * sapply(0:150, function(t) 1/(1+discRate)^t)
    fixedCostsRow <- c(fixedCosts[1:3], sum(fixedCosts))
  } else {
    fixedCostsRow <- c(fixedCosts, NA)
  }
  tbl <- rbind(tbl, fixedCostsRow)
  
  # Present value of profit (after fixed costs)
  profitsAfterFixedCosts <- tbl[(nrow(tbl)-1), ] - tbl[nrow(tbl), ]
  tbl <- rbind(tbl, profitsAfterFixedCosts)
  
  # Non operating assets
  noaRow <- c(rep(NA, ncol(tbl)-1), noa)
  debtRow <- c(rep(NA, ncol(tbl)-1), debt)
  
  tbl <- rbind(tbl, noaRow)
  tbl <- rbind(tbl, debtRow)
  
  shvRow <- c(rep(NA, ncol(tbl)-1), 
              profitsAfterFixedCosts[length(profitsAfterFixedCosts)] + noa - debt)
  tbl <- rbind(tbl, shvRow)
  
  
  rowNames <- rownames(tbl)[1:4]
  rowNames <- c(
    rownames(tbl)[1:4], # cohort rows
    "Profits before Fixed Costs",
    "Fixed Costs",
    "Profits after Fixed Costs",
    "Non-Operating Assets",
    "Debt",
    "Enterprise Value"
  )
  rownames(tbl) <- rowNames
  
  colNames <- c(
    sapply(t:(t+2), function(i) paste0("t=", i)),
    "Profits of Cohort"
  )
  colnames(tbl) <- colNames
  
  dt <- data.table(tbl, keep.rownames = T)
  setnames(dt, old = "rn", new = "---")
  
  dt[, `...` := "..."]
  dt$`...`[(nrow(dt)-2):nrow(dt)] <- rep(" ", 3)
  setcolorder(dt, neworder = c(1, 2, 3, 4, 6, 5))
  return(dt)
}


PlotIllustrativeValuationTable <- function(dt){
  DT::datatable(
    data = dt,
    options = list(
      info = F, # removes the "Show 1 of 11 entries"-text at the bottom of a DT
      pageLength = 10,
      searching = F, # disables search bar in the top-right corner
      paging = F,    # disables "Show X entries" option
      ordering = F,
      columnDefs = list(
        list(visible = F, targets = 0),            # hide rownames
        list(className = "dt-right", targets = 1), # right-align first column
        list(className = "dt-center", targets = 2:5), # right-align first column
        list(className = "dt-left", targets = 6)   # left-align last column
      )
    )
  ) %>% 
    DT::formatCurrency(
      columns = c(2,3,4,6),
      digits = 0
    ) %>%
    DT::formatStyle(
      columns = 1,
      fontWeight = "bold"
    ) %>% 
    DT::formatStyle(
      columns = 5,
      color = "black"
    )
}

PlotStepByStepCBCVBarplot <- function(ceCurr = 15000, ceFuture = 60000, tvFixedCost = 55000, noa = 0, debt = 10000) {
  ceBeforeFixedCost <- ceCurr + ceFuture
  ceAfterFixedCost <- ceBeforeFixedCost - tvFixedCost
  firmValue <- ceAfterFixedCost + noa
  shv <- firmValue - debt
  
  y <- c(ceCurr, ceFuture, ceBeforeFixedCost, tvFixedCost, ceAfterFixedCost, noa, firmValue, debt, shv)
  dataPlt <- data.table(x = 1:length(y), y = y)
  
  
  xAxisLabels <- c(
    "Customer \n Equity \n of current \n customers",
    "Customer \n Equity \n of future \n customers",
    "Customer \n Equity \n before \n fixed \n cost",
    "Fixed \n Cost \n (terminal \n value)",
    "Customer \n Equity \n after \n fixed \n cost",
    "Non- \n operating \n assets",
    "Firm \n value",
    "Debt",
    "Shareholder \n value"
  )
  
  colRed <- "#e57373"
  lightPurple <- "#a7a7ff"
  darkPurple <- "#6464c8"
  
  p <- ggplot(dataPlt, aes(x = x, y = y)) + 
    geom_col(fill = c(lightPurple, lightPurple, darkPurple, colRed, darkPurple, lightPurple, darkPurple, colRed, darkPurple)) +
    scale_x_continuous(breaks = 1:9, labels = xAxisLabels) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  if (nrow(dataPlt[y < 0]) == 0){
    p <- p + 
      geom_text(data = dataPlt[y >= 0],
                mapping = aes(x = x, y = y, label = paste0("$ ", y)),
                vjust = -0.8)
  } else {
    p <- p +
      geom_text(data = dataPlt[y >= 0], mapping = aes(x = x, y = y, label = paste0("$ ", y)), vjust = -0.8) +
      geom_text(data = dataPlt[y < 0], mapping = aes(x = x, y = y, label = paste0("$ ", y)), vjust = 1.2)
  }
  return(p)
}

PlotStepByStepCBCVTable <- function(ceCurr = 15000, ceFuture = 60000, tvFixedCost = 55000, noa = 0, debt = 10000) {
  varNames <- c(
    "+ CE of Current Customers",
    "+ CE of Future Customers",
    "= CE before Fixed Cost",
    "- Fixed Cost (Terminal Value)",
    "= CE after Fixed Cost",
    "+ Non-Operating Assets",
    "- Debt",
    "= Enterprise Value"
  )
  
  enterpriseValue <- ceCurr + ceFuture - tvFixedCost + noa - debt
  
  values <- c(
    round(ceCurr),
    round(ceFuture),
    round(ceCurr + ceFuture),
    round(tvFixedCost),
    round(ceCurr + ceFuture - tvFixedCost),
    round(noa),
    round(debt),
    round(enterpriseValue)
  )
  
  values <- sapply(values, FormatDollars)
  
  dt <- data.table(Variable = varNames, Value = values)
  
  dt %>% 
    addHtmlTableStyle(
      css.cell = "padding-left: 1em; padding-right: 1em;"
    ) %>% 
    htmlTable(
      align = "lr",
      rgroup = c("Customer Equity Reporting", "Financial Structure", "Valuation"),
      n.rgroup = c(5, 2, 1),
      ctable = c("solid", "double"),
      rnames = F)
}

PlotStepByStepDCFTable <- function(valuationPeriod, profitMatrix, discMatrix, discRate, fixCost, noa, debt){
  t <- valuationPeriod
  discProfitMatrix <- profitMatrix * discMatrix
  discProfitsBeforeFixedCosts <- colSums(discProfitMatrix, na.rm = T)
  discProfitsBeforeFixedCosts <- discProfitsBeforeFixedCosts[t:length(discProfitsBeforeFixedCosts)]
  discFixCosts <- sapply(1:length(discProfitsBeforeFixedCosts), function(t){
    fixCost * (1/(1+discRate)^(t-1))
  })
  discProfitsAfterFixedCosts <- discProfitsBeforeFixedCosts - discFixCosts
  presentValueOfProfits <- sum(discProfitsAfterFixedCosts)
  enterpriseValue <- presentValueOfProfits + noa - debt
  
  varNames <- c(
    paste0("+ PV of Profit after Fixed Cost (t=", t, ")"),
    paste0("+ PV of Profit after Fixed Cost (t=", t+1, ")"),
    paste0("+ PV of Profit after Fixed Cost (t=", t+2, ")"),
    "+ PV of Profit after Fixed Cost (t=...)",
    "= Sum of PV of Profit after Fixed Cost",
    "+ Non-Operating Assets",
    "- Debt",
    "= Enterprise Value"
  )
  
  
  values <- c(
    round(discProfitsAfterFixedCosts[1]),
    round(discProfitsAfterFixedCosts[2]),
    round(discProfitsAfterFixedCosts[3]),
    NA,
    round(presentValueOfProfits),
    round(noa),
    round(debt),
    round(enterpriseValue)
  )
  
  values <- FormatDollars(xs = values, digits = 0)
  values[4] <- "..."
  
  dt <- data.table(Variable = varNames, Value = values)
  
  dt %>% 
    addHtmlTableStyle(
      css.cell = "padding-left: 1em; padding-right: 1em"
    ) %>% 
    htmlTable(
      align = "lr",
      rgroup = c("Free Cash Flow", "Financial Structure", "Valuation"),
      n.rgroup = c(5, 2, 1),
      ctable = c("solid", "double"),
      rnames = F
    )
}

DiscountedProfitContributionPerPeriod <- function(valuationPeriod, n, profitMatrix, discMatrix){
  discProfitMatrix <- profitMatrix * discMatrix
  discProfitMatrix <- discProfitMatrix[1:(valuationPeriod+n), valuationPeriod:(valuationPeriod+n)]
  discProfitsPerPeriod <- colSums(discProfitMatrix, na.rm = T)
  result <- data.table(
    t = valuationPeriod:(valuationPeriod + n),
    PresentValueOfProfit = discProfitsPerPeriod
  )
  return(result)
}

PlotDiscountedProfitContributionPerPeriod <- function(discProfitsPerPeriod){
  ggplot(discProfitsPerPeriod, aes(x = t, y = PresentValueOfProfit)) +
    geom_col(alpha = 0.8, fill = kDarkestBlue) +
    geom_text(aes(label = FormatDollars(round(PresentValueOfProfit))),
              vjust = -0.4) +
    theme_gg() +
    scale_x_continuous(breaks = 1:max(discProfitsPerPeriod$t)) +
    scale_y_continuous(labels = FormatDollars) +
    ggtitle("Present Value of Profits (before Fixed Cost) per Period") +
    xlab("Period") +
    ylab("Present Value of Profits")
}

DiscountedProfitContributionPerCohort <- function(valuationPeriod, n, profitMatrix, discMatrix){
  discProfitMatrix <- profitMatrix * discMatrix
  discProfitsPerCohort <- rowSums(discProfitMatrix, na.rm = T)
  discProfitsPerCohort <- discProfitsPerCohort[1:(valuationPeriod+n)]
  return(data.table(
    Cohort = 1:(valuationPeriod+n),
    PresentValueOfProfit = discProfitsPerCohort
  ))
}

PlotDiscountedProfitContributionPerCohort <- function(discProfitsPerCohort){
  ggplot(discProfitsPerCohort, aes(x = Cohort, y = PresentValueOfProfit)) +
    geom_col(alpha = 0.8, fill = kDarkestBlue) +
    geom_text(aes(label = FormatDollars(round(PresentValueOfProfit))),
              hjust = -0.1) +
    scale_y_continuous(labels = FormatDollars) +
    scale_x_reverse(breaks = 1:max(discProfitsPerCohort$Cohort)) +
    coord_flip() +
    theme_gg() +
    ggtitle("Present Value of (remaining) Profits per Cohort") +
    ylab("Present Value of a cohort's cumulative profits") +
    xlab("Cohort")
}

# Tab: Cohort View -------------------------------------------------------------

CohortViewTable <- function(revMatrix, costMatrix, cacMatrix, retCostMatrix, varCostMatrix, profitMatrix, discMatrix, n, isDiscounted){
  if (isDiscounted){
    revMatrix <- revMatrix * discMatrix
    costMatrix <- costMatrix * discMatrix
    profitMatrix <- profitMatrix * discMatrix
  }
  cohortViewTable <- data.table(
    Cohort = sapply(1:n, function(c) paste0("Cohort ", c, " (acquired in t=", c, ")")),
    Revenues        = rowSums(revMatrix[1:n, 1:n], na.rm = T),
    Costs           = (-1)*rowSums(costMatrix[1:n, 1:n], na.rm = T),
    AcquisitionCosts     = (-1)*rowSums(cacMatrix[1:n, 1:n], na.rm = T),
    RetentionCosts       = (-1)*rowSums(retCostMatrix[1:n, 1:n], na.rm = T),
    ProductCosts = (-1)*rowSums(varCostMatrix[1:n, 1:n], na.rm = T),
    Profits         = rowSums(profitMatrix[1:n, 1:n], na.rm = T)
  )
  names(cohortViewTable) <- sapply(names(cohortViewTable), SplitStringAtUppercase)
  return(cohortViewTable)
}

PlotCohortViewTable <- function(cohortViewTable, digits){
  DT::datatable(
    data = cohortViewTable,
    options = list(
      ordering = F,
      info = F, # removes the "Show 1 of 11 entries"-text at the bottom of a DT
      searching = F, # disables search bar in the top-right corner
      paging = F, # disables "Show X entries" option
      columnDefs = list(
        list(visible = F, targets = 0),
        list(className = "dt-center", targets = 2:ncol(cohortViewTable))
      )
    )
  ) %>% 
    formatCurrency(
      columns = 2:ncol(cohortViewTable),
      currency = "$",
      digits = digits
    )
}

# Tab: Period View --------------------------------------------------------

#' Calculates a profit loss statement
#'
#' @param tEnd int: how many periods the table should cover
#' @param revMatrix matrix
#' @param cogsMatrix matrix
#' @param cacMatrix matrix
#' @param retCostMatrix matrix
#' @param fixedCost numeric
#'
#' @return data.table
#'
#' @examples
ComputeProfitLossStatement <- function(showNPeriods, revMatrix, varCostMatrix, cacMatrix, retCostMatrix, fixedCost) {
  revenues <- colSums(revMatrix[, 1:showNPeriods], na.rm = T) #tEnd must be >= 2!
  revenuesOfNewCustomers <- diag(revMatrix[, 1:showNPeriods])
  revenuesOfExistingCustomers <- revenues - diag(revMatrix[, 1:showNPeriods]) 
  
  cogs <- colSums(varCostMatrix[, 1:showNPeriods], na.rm = T)
  profitContributionBeforeMarketingCost <- revenues - cogs
  
  acquisitionCost <- colSums(cacMatrix[, 1:showNPeriods], na.rm = T)
  retentionCost <- colSums(retCostMatrix[, 1:showNPeriods], na.rm = T)
  marketingCost <- acquisitionCost + retentionCost
  
  profitContributionAfterMarketingCost <- 
    profitContributionBeforeMarketingCost -
    acquisitionCost -
    retentionCost
  
  fixedCost <- rep(fixedCost, length(revenues)) # currently constant for each period
  profit <- profitContributionAfterMarketingCost - fixedCost
  
  return(data.table(
    t = sapply(1:showNPeriods, function(t) paste0("t=", t)),
    `Revenues, thereof` = revenues,
    `--- Revenues from existing customers` = revenuesOfExistingCustomers,
    `--- Revenues from new customers` = revenuesOfNewCustomers,
    `Costs, thereof` = cogs + acquisitionCost + retentionCost,
    `--- Product Costs` = cogs,
    `--- Acquisition Costs` = acquisitionCost,
    `--- Retention Costs` = retentionCost,
    `--- Fixed Costs` = fixedCost,
    `Profits before Fixed Costs` = profitContributionAfterMarketingCost,
    `Profits after Fixed Costs` = profit
  ))
}

#' Plots a Profit Loss Statement as a DT::datatable
#'
#' @param profitLoss data.table: output of ProfitLossStatement()
#'
#' @return DT::datatable
#'
#' @examples
PlotProfitLossStatement <- function(profitLoss){
  dt <- data.table::transpose(profitLoss, keep.names = "col", make.names = "t")
  setnames(dt, old = "col", new = " ")
  
  plt <- DT::datatable(
    dt,
    extensions = 'FixedColumns',
    options = list(
      pageLength = 11,
      info = F, # removes the "Show 1 of 11 entries"-text at the bottom of a DT
      # scrollX='600px',
      searching = F, # disables search bar in the top-right corner
      paging = F, # disables "Show X entries" option
      ordering = F,
      columnDefs = list(list(visible = F, targets = 0))# hide rownames
      # fixedColumns = list(leftColumns = 2)
    )
  ) %>% 
    # Make first column bold
    formatStyle(
      columns = 1,
      target = "row",
      fontWeight = styleEqual(
        c("Revenues, thereof",
          "Costs, thereof",
          "Profits before Fixed Costs",
          "Profits after Fixed Costs"
        ),
        rep("bold", 4)
      )  
    ) %>%
    formatCurrency(
      columns = 2:ncol(dt),
      currency = "$", 
      digits = 0
    )
  return(plt)
}

# Tab: Cohort Array -------------------------------------------------------------------

FormatCohortTableDT <- function(X, to, addRowSums, addColSums, view){
  Xs <- X[1:to, 1:to]
  
  if (view == "cohort-age"){
    Xs <- TransformView(Xs)
    X <- TransformView(X)
  }
  
  if (view == "cohort-period"){
    xLabel <- "t"
    xFrom <- 1
    xTo <- to
  } else if (view == "cohort-age"){
    xLabel <- "age"
    xFrom <- 0
    xTo <- (to-1)
  } 
  
  Xs <- AddRowColSums(Xs, X, rowSums = addRowSums, colSums = addColSums)
  Xs <- Replace(Xs, oldValue = 0, newValue = NA)
  
  # Add proper names to rows and columns
  if (addColSums == T){
    rownames(Xs) <- c(sapply(1:to, function(c) paste0("Cohort ", c)), "Sum")
  } else {
    rownames(Xs) <- sapply(1:to, function(c) paste0("Cohort ", c))
  }
  
  if (addRowSums == T){
    colnames(Xs) <- c(sapply(xFrom:xTo, function(t) paste0(xLabel, "=", t)), "Infinite Sum")
  } else {
    colnames(Xs) <- sapply(xFrom:xTo, function(t) paste0(xLabel, "=", t))
  }
  return(Xs)
}

PlotCohortTableDT <- function(X, digits, isPercentage, isCurrency){
  plt <- DT::datatable(
    data = X,
    options = list(
      ordering = F,
      pageLength = nrow(X), # number of rows to display
      searching = F, # disables search bar in the top-right corner
      paging = F, # disables "Show X entries" option
      columnDefs = list(
        list(className = "dt-center", targets = 1:ncol(X)) 
      )
    )
  ) %>% 
    formatStyle(
      columns = 0,
      fontWeight = "bold"
    ) %>% 
    formatCurrency(
      columns = 1:ncol(X),
      currency = ifelse(isCurrency, "$", ""),
      digits = digits
    )
  if (isPercentage){
    plt <- plt %>% formatPercentage(
      columns = 1:ncol(X),
      digits = digits
    )
  }
  return(plt)
}

PlotC3 <- function(X){
  data <- MatrixToDT(X)
  data <- data[value != 0]
  
  cohorts <- unique(data$cohort)
  
  tempRows <- data.table(
    cohort = cohorts[2:length(cohorts)],
    period = cohorts[2:length(cohorts)] - 1,
    value = 0
  )
  
  data <- rbind(data[, c("cohort", "period", "value")], tempRows)
  
  data <- data %>% 
    mutate(cohort = as.factor(cohort)) %>% 
    mutate(cohort = fct_reorder(cohort, desc(cohort)))
  
  nCohorts <- length(unique(data$cohort))
  
  suppressWarnings({
    plt <- ggplot(data, aes(x = period, y = value, fill = cohort)) +
      geom_area(position = "stack") +
      geom_line(position = "stack", alpha=0.2) +
      scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
      theme_gg() +
      xlab("Period") +
      ylab("") +
      labs(fill = "Cohort")
      
  })
  
  return(plt)
}

#' Plots a cohort table as a heatmap
#'
#' @param X matrix: cohort matrix
#' @param x string: variable on the x-axis; one of c("age", "period", "cohort")
#' @param y string: variable on the x-axis; one of c("age", "period", "cohort")
#'
#' @return ggplot
#'
#' @examples PlotHeatmap(X = GenDummyCustMatrix(), x = "age", y = "cohort")
PlotHeatmap <- function(X, x, y){
  data <- MatrixToDT(X)
  data <- data %>% 
    mutate(
      cohort = as.factor(cohort),
      age    = as.factor(age),
      period = as.factor(period)
    ) %>% 
    mutate(
      cohort = fct_reorder(cohort, desc(cohort))
    )
  
  ggplot(data, aes_string(x = x, y = y, fill = "value")) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(value, 2)), size = 4) +
    scale_fill_gradient(low = kLightestBlue, high = kDarkestBlue) +
    theme_gg() +
    theme(legend.position = "none") +
    ylab(FirstCharacterUppercase(y)) +
    xlab(FirstCharacterUppercase(x))
}

#' Plots a cohort matrix as a linechart, where each line corresponds to a cohort
#'
#' @param X matrix: cohort matrix, e.g. a cohort matrix of customer retention
#' @param x string: variable on the x-axis; one of c("age", or "period")
#'
#' @return ggplot
#'
#' @examples PlotLinechart(GenExpGrowthOverTime(0.8, 10), x = "period")
PlotLinechart <- function(X, x){
  data <- MatrixToDT(X)
  data <- data %>% 
    mutate(cohort = as.factor(cohort)) %>% 
    mutate(cohort = fct_reorder(cohort, desc(cohort)))
  
  nCohorts <- length(unique(data$cohort))
  
  ggplot(data, aes_string(x = x, y = "value", color = "cohort")) +
    geom_line() +
    theme_gg() +
    scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
    xlab(FirstCharacterUppercase(x)) +
    ylab("Value") +
    labs(color = "Cohort")
}


# Tab Dashboard > Dashboard ---------------------------------------------------------------

PlotPeriodView <- function(X, fromPeriod, toPeriod, geomType, title, subtitle, xlab, ylab, digits, isDollars=F, colAggFunc=colSums){
  X <- X[1:toPeriod, fromPeriod:toPeriod]
  x <- colAggFunc(X, na.rm = T)
  
  dtPlt <- data.table(
    period = fromPeriod:toPeriod,
    value = round(x, digits)
  )
  
  plt <- ggplot(dtPlt, aes(x = period, y = value))
  
  if (geomType == "geom_col"){
    plt <- plt + geom_col(alpha = 0.8, fill = kDarkestBlue)
  } else if (geomType == "geom_line"){
    plt <- plt + geom_line(alpha = 0.5, color = kDarkestBlue) +
      geom_point(size = 2, color = kDarkestBlue)
  }
  
  plt <- plt + 
    scale_x_continuous(breaks = fromPeriod:toPeriod) +
    theme_gg() +
    ggtitle(label = title, subtitle = subtitle) +
    xlab(xlab) +
    ylab(ylab) +
    theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))
  
  if (isDollars){
    plt <- plt +
      geom_text(aes(label = FormatDollars(value, digits = digits)), vjust = -0.4) +
      scale_y_continuous(labels = FormatDollars)
  } else {
    plt <- plt + geom_text(aes(label = value), vjust = -0.4)
  }
  return(plt)
}

PlotCohortView <- function(X, fromPeriod, toPeriod, geomType, title, subtitle, xlab, ylab, digits, isDollars){
  X <- X[1:toPeriod, 1:toPeriod]
  dt <- MatrixToDT(X)
  dt <- dt[period >= fromPeriod, ]
  dt <- dt[, cohort := as.factor(cohort)]
  nCohorts <- length(unique(dt$cohort))
  
  # dataset with for geom_text labels
  dtText <- copy(dt)
  
  if (geomType == "geom_col"){
    dtText <- dtText %>% 
      group_by(period) %>% 
      summarise(y = sum(value))
  } 
  
  if (geomType == "geom_col"){
    plt <- ggplot(dt, aes(x = period, y = value, fill = fct_rev(cohort))) +
      geom_col(color = "gray20") +
      scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
      labs(fill = "Cohort")
  } else if (geomType == "geom_line"){
    plt <- ggplot(dt, aes(x = period, y = value, color = fct_rev(cohort))) +
      geom_line(alpha = 0.5) +
      geom_point(size = 2) +
      scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
      labs(color = "Cohort")
  }
  
  plt <- plt +
    scale_x_continuous(breaks = fromPeriod:toPeriod) +
    theme_gg() +
    ggtitle(label = title, subtitle = subtitle) +
    xlab(xlab) +
    ylab(ylab)
  
  if (isDollars & geomType == "geom_col"){
    plt <- plt + geom_text(
        data = dtText,
        mapping = aes(x = period,
                      y = y,
                      label = FormatDollars(y, digits = digits),
                      fill = NULL,
                      color = NULL),
        vjust = -0.4
      ) +
      scale_y_continuous(labels = FormatDollars)
  }  else if (isDollars == F){
    plt <- plt + geom_text(
      data = dtText,
      mapping = aes(x = period,
                    y = y,
                    label = round(y, digits = digits),
                    fill = NULL,
                    color = NULL),
      vjust = -0.4
    )
  }
  return(plt)
}

# PlotNumberOfCustomersPerPeriod <- function(fromPeriod, toPeriod, custMatrix){
#   X <- custMatrix[1:toPeriod, fromPeriod:toPeriod]
#   nCustomers <- colSums(X, na.rm = T)
#   
#   dtPlt <- data.table(
#     Period = fromPeriod:toPeriod,
#     NumberOfCustomers = round(nCustomers, 2)
#   )
#   
#   ggplot(dtPlt, aes(x = Period, y = NumberOfCustomers)) +
#     geom_col(alpha = 0.8, fill = kDarkestBlue) +
#     geom_text(aes(label = NumberOfCustomers), vjust = -0.4) +
#     scale_x_continuous(breaks = fromPeriod:toPeriod) +
#     theme_gg() +
#     ggtitle("Number of Customers per Period") +
#     xlab("Period") +
#     ylab("Number of Customers")
# }
# 
# PlotRevenuesPerPeriod <- function(fromPeriod, toPeriod, revMatrix){
#   X <- revMatrix[1:toPeriod, fromPeriod:toPeriod]
#   revenues <- colSums(X, na.rm = T)
#   
#   dtPlt <- data.table(
#     Period = fromPeriod:toPeriod,
#     Revenues = round(revenues)
#   )
#   
#   ggplot(dtPlt, aes(x = Period, y = Revenues)) +
#     geom_col(alpha = 0.8, fill = kDarkestBlue) +
#     geom_text(aes(label = FormatDollars(Revenues)), vjust = -0.4) +
#     theme_gg() +
#     scale_x_continuous(breaks = fromPeriod:toPeriod) +
#     scale_y_continuous(labels = FormatDollars) +
#     ggtitle("Revenue per Period") +
#     xlab("Period") +
#     ylab("Revenue")
# }
# 
# PlotARPUperPeriod <- function(fromPeriod, toPeriod, revMatrix, custMatrix){
#   R <- colSums(revMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
#   N <- colSums(custMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
#   ARPU <- round(R/N, 2)
#   
#   dtPlt <- data.table(
#     Period = fromPeriod:toPeriod,
#     ARPU = ARPU
#   )
#   
#   ggplot(dtPlt, aes(x = Period, y = ARPU)) +
#     geom_line(alpha = 0.5) + 
#     geom_point(size = 2) +
#     geom_text(aes(label = FormatDollars(ARPU)), vjust = -0.4) +
#     theme_gg() +
#     scale_x_continuous(breaks = fromPeriod:toPeriod) +
#     scale_y_continuous(labels = FormatDollars) +
#     ggtitle("Average Revenue Per Customer per Period") +
#     xlab("Period") +
#     ylab("Avg. Revenue per Customer")
# }
# 
# PlotMarketingCostsPerPeriod <- function(fromPeriod, toPeriod, retCostMatrix,
#                                         cacMatrix){
#   retCost <- colSums(retCostMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
#   acqCost <- colSums(cacMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
#   marketingCost <- retCost + acqCost
#   
#   dtPlt <- data.table(
#     period = fromPeriod:toPeriod,
#     marketingCost = round(marketingCost)
#   )
#   
#   ggplot(dtPlt, aes(x = period, y = marketingCost)) +
#     geom_col(alpha = 0.8, fill = kDarkestBlue) +
#     geom_text(aes(label = FormatDollars(marketingCost)), vjust = -0.4) +
#     theme_gg() +
#     scale_x_continuous(breaks = fromPeriod:toPeriod) +
#     scale_y_continuous(labels = FormatDollars) +
#     ggtitle("Marketing Cost per Period") +
#     xlab("Period") +
#     ylab("Cost")
# }

PlotCostsPerPeriod <- function(fromPeriod, toPeriod, varCostMatrix,
                               retCostMatrix, cacMatrix, fixedCost){
  varCost <- colSums(varCostMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
  retCost <- colSums(retCostMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
  acqCost <- colSums(cacMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
  fixCost <- rep(fixedCost, (toPeriod - fromPeriod + 1))
  
  dtPlt <- data.table(
    Period = fromPeriod:toPeriod,
    VariableCosts = varCost,
    RetentionCosts = retCost,
    AcquisitionCosts = acqCost,
    FixedCosts = fixCost
  )
  
  dtPlt <- data.table::melt(
    data = dtPlt,
    id.vars = "Period",
    value.name = "Cost",
    variable.name = "Category"
  )
  
  dtPlt[, Category := SplitStringAtUppercase(Category)]
  dtPlt[, Category := factor(Category, levels = c("Variable Costs",
                                                  "Retention Costs",
                                                  "Acquisition Costs",
                                                  "Fixed Costs"))]
  
  ggplot(dtPlt, aes(x = Period, y = Cost, fill = Category)) +
    geom_col(color = "gray20") +
    theme_gg() +
    scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Reds")) +
    scale_x_continuous(breaks = fromPeriod:toPeriod) +
    scale_y_continuous(labels = FormatDollars) +
    xlab("Period") +
    ylab("Cost")
  
}

PlotOperatingMarginPerPeriod <- function(fromPeriod, toPeriod, revMatrix, profitMatrix){
  revenues <- colSums(revMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
  profits <- colSums(profitMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
  
  dtPlt <- data.table(
    period = fromPeriod:toPeriod,
    operatingMargin = profits/revenues
  )
  
  ggplot(dtPlt, aes(x = period, y = operatingMargin)) +
    geom_line() +
    geom_point(size = 2) + 
    theme_gg() +
    scale_x_continuous(breaks = fromPeriod:toPeriod) +
    scale_y_continuous(labels = FormatPercent) +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed") +
    # ggtitle("Operating Margin per Period",
            # subtitle = "The operating margin measures how much profit the firm makes on $1 of revenue after paying for variable costs.") +
    xlab("Period") +
    ylab("Operating Margin") +
    theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))
}

PlotProfitsPerPeriod <- function(fromPeriod, toPeriod, profitMatrix, fixedCosts, afterFixedCosts = F){
  profits <- colSums(profitMatrix[1:toPeriod, fromPeriod:toPeriod], na.rm = T)
  
  if (afterFixedCosts == T){
    fixedCosts <- rep(fixedCosts, length(profits))
    profits <- profits - fixedCosts
  }
  
  dtPlt <- data.table(
    period = fromPeriod:toPeriod,
    profits = profits
  )
  
  negativeProfits <- dtPlt[profits < 0, ]
  positiveProfits <- dtPlt[profits >= 0, ]
  
  plt <- ggplot(dtPlt, aes(x = period, y = profits)) +
    geom_col(alpha = 0.8, fill = kDarkestBlue) +
    theme_gg() +
    scale_x_continuous(breaks = fromPeriod:toPeriod) +
    scale_y_continuous(labels = FormatDollars) +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed") +
    xlab("Period") +
    ylab("Profits")
  
  if (nrow(negativeProfits) > 0){
    plt <- plt + 
      geom_text(
        data = negativeProfits,
        mapping = aes(x = period, y = profits, label = FormatDollars(round(profits))),
        vjust = 1
      )
  }
  
  if (nrow(positiveProfits) > 0){
    plt <- plt +
      geom_text(
        data = positiveProfits,
        mapping = aes(x = period, y = profits, label = FormatDollars(round(profits))),
        vjust = -0.4
      )
  }
  
  # if (afterFixedCosts == T){
  #   plt <- plt + ggtitle("Profits after Fixed Costs")
  # } else {
  #   plt <- plt + ggtitle("Profits before Fixed Costs")
  # }
  return(plt)
}

PlotCostsByCohort <- function(fromPeriod, toPeriod, costMatrix){
  X <- costMatrix[1:toPeriod, 1:toPeriod]
  dt <- MatrixToDT(X)
  dt <- dt[period >= fromPeriod, ]
  dt[, cohort := as.factor(cohort)]
  nCohorts <- length(unique(dt$cohort))
  
  suppressWarnings({
    ggplot(dt, aes(x = period, y = value, fill = fct_rev(cohort))) +
      geom_col(color = "gray20") +
      scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
      scale_x_continuous(breaks = fromPeriod:toPeriod) +
      scale_y_continuous(labels = FormatDollars) +
      theme_gg() +
      ggtitle("Variable Costs broken down by Cohorts",
              subtitle = "Variable costs associated with a cohort include (1) acquisition costs, (2) retention costs, and (3) product costs.") +
      xlab("Period") +
      ylab("Costs") +
      labs(fill = "Cohort")
  })
}

PlotOperatingMarginByCohort <- function(fromPeriod, toPeriod, revMatrix, profitMatrix){
  revMatrix <- revMatrix[1:toPeriod, 1:toPeriod]
  profitMatrix <- profitMatrix[1:toPeriod, 1:toPeriod]
  
  opMarginMatrix <- profitMatrix / revMatrix
  
  dt <- MatrixToDT(opMarginMatrix)
  dt <- dt[period >= fromPeriod, ]
  dt[, cohort := as.factor(cohort)]
  
  nCohorts <- length(unique(dt$cohort))
  
  suppressWarnings({
    ggplot(dt, aes(x = period, y = value, color = fct_rev(cohort))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed") +
      scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
      scale_x_continuous(breaks = fromPeriod:toPeriod) +
      scale_y_continuous(labels = FormatPercent) +
      theme_gg() +
      xlab("Period") +
      ylab("Operating Margin") +
      labs(color = "Cohort")
  })
}

PlotProfitsByCohort <- function(fromPeriod, toPeriod, profitMatrix){
  X <- profitMatrix[1:toPeriod, 1:toPeriod]
  
  dt <- MatrixToDT(X)
  dt <- dt[period >= fromPeriod, ]
  dt[, cohort := as.factor(cohort)]
  
  nCohorts <- length(unique(dt$cohort))
  
  suppressWarnings({
    ggplot(dt, aes(x = period, y = value, color = fct_rev(cohort))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed") +
      scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(nCohorts, "Blues"))(nCohorts)) +
      scale_x_continuous(breaks = fromPeriod:toPeriod) +
      scale_y_continuous(labels = FormatDollars) +
      theme_gg() +
      xlab("Period") +
      ylab("Profit") +
      labs(color = "Cohort")
  })
}


# Tab Dashboard > SaaS Dashboard ------------------------------------------

PlotGrowthRates <- function(dtGrowthRates, title){
  ggplot(dtGrowthRates, aes(x = t, y = growthRate)) +
    geom_line() +
    geom_point() +
    theme_gg() +
    scale_x_continuous(breaks = 1:max(dtGrowthRates$t)) +
    scale_y_continuous(labels = FormatPercent) +
    ggtitle(title) +
    xlab("Period") +
    ylab("Growth Rate")
}

NetRevenueRetentionRate <- function(revMatrix, n){
  revMatrix <- revMatrix[1:n, 1:n]
  ndr <- sapply(2:ncol(revMatrix), function(t){
    revenueBefore <- sum(revMatrix[1:(t-1), (t-1)], na.rm=T)
    revenueAfter  <- sum(revMatrix[1:(t-1), t], na.rm=T)
    netDollarRetention <- revenueAfter / revenueBefore
    return(netDollarRetention)
  })
  ndr <- c(NA, ndr)
  return(ndr)
}

PlotNetRevenueRetentionRate <- function(nrr){
  dtPlt <- data.table(
    NetRevenueRetention = nrr,
    t = 1:length(nrr)
  )
  ggplot(dtPlt, aes(x = t, y = NetRevenueRetention)) +
    geom_col(fill = kDarkestBlue) +
    geom_text(aes(label = paste0(round(NetRevenueRetention*100,2), "%")), vjust = -0.4) +
    xlab("Period") +
    ylab("Net Revenue Retention Rate") +
    # ggtitle("Net Revenue Retention Retention Rate") +
    theme_gg() +
    scale_x_continuous(breaks = 1:length(nrr)) +
    scale_y_continuous(labels = FormatPercent)
}

CustomerRetentionRate <- function(custMatrix, n){
  X <- custMatrix[1:n, 1:n]
  total <- colSums(X, na.rm = T)
  new <- diag(X)
  exist <- total - new
  totalPrevPeriod <- c(NA, total[1:(length(total)-1)])
  customerRetentionRates <- exist/totalPrevPeriod
  return(customerRetentionRates)
}

PlotCustomerRetentionRate <- function(customerRetentionRates){
  dtPlt <- data.table(
    t = 1:length(customerRetentionRates),
    crr = customerRetentionRates
  )
  ggplot(dtPlt, aes(x = t, y = crr)) +
    geom_col(fill = kDarkestBlue) +
    geom_text(aes(label = paste0(crr*100, "%")),
              nudge_y = 0.02) +
    xlab("Period") +
    ylab("Customer Retention Rate") +
    # ggtitle("Customer Retention Rate") +
    theme_gg() +
    scale_x_continuous(breaks = 1:length(customerRetentionRates)) +
    scale_y_continuous(labels = FormatPercent)
}

NewExistLostCustomers <- function(custMatrix, retRate, n){
  X <- custMatrix[1:n, 1:n]
  new <- diag(X)
  exist <- colSums(X, na.rm = T) - new
  lost <- exist * (1 - retRate)
  dt <- data.table(
    t = 1:n,
    new = new,
    exist = exist,
    lost = -lost
  )
  dt <- melt(dt,
             measure.vars = c("new", "exist", "lost"),
             variable.name = "Category", 
             value.name = "nCustomers"
  )
  return(dt)
}

PlotNewExistLostCustomers <- function(dt){
  
  colorLost <- "tomato3"
  colorExist <- kDarkestBlue
  colorNew <- kMidBlue
  
  ggplot(dt, aes(x = t, y = nCustomers, fill = Category)) +
    geom_col(color = "gray20") +
    scale_fill_manual(
      values = c(colorNew, colorExist, colorLost)
    ) +
    theme_gg() +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed") +
    # ggtitle("Number of New, Existing, and Lost Customers") +
    xlab("Period") +
    ylab("Number of Customers") +
    scale_x_continuous(breaks = 1:max(dt$t))
}

NewExistLostRevenue <- function(revMatrix, retRate, n){
  X <- revMatrix[1:n, 1:n]
  new <- diag(X)
  exist <- colSums(X, na.rm = T) - new
  lost <- exist * (1 - retRate)
  dt <- data.table(
    t = 1:n,
    new = new,
    exist = exist,
    lost = -lost
  )
  dt <- melt(dt,
             measure.vars = c("new", "exist", "lost"),
             variable.name = "Category", 
             value.name = "Revenue"
  )
}

PlotNewExistLostRevenue <- function(dt){
  
  colorLost <- "tomato3"
  colorExist <- kDarkestBlue
  colorNew <- kMidBlue
  
  ggplot(dt, aes(x = t, y = Revenue, fill = Category)) +
    geom_col(color = "gray20") +
    scale_fill_manual(
      values = c(colorNew, colorExist, colorLost)
    ) +
    theme_gg() +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    # ggtitle("Amount of New, Existing, and Lost Revenue") +
    xlab("Period") +
    ylab("Revenue") +
    scale_x_continuous(breaks = 1:max(dt$t)) +
    scale_y_continuous(labels = FormatDollars)
}

ShareOfRevenueFromExistVsNew <- function(revMatrix, n){
  X <- revMatrix[1:n, 1:n]
  new <- diag(X)
  exist <- colSums(X, na.rm = T) - new
  total <- new + exist
  shareNew <- new / total
  shareExist <- exist / total
  dt <- data.table(
    t = 1:n,
    new = shareNew,
    exist = shareExist
  )
  dt <- melt(dt,
             measure.vars = c("new", "exist"),
             variable.name = "Category",
             value.name = "percentageShare"
  )
  return(dt)
}

PlotShareOfRevenueFromExistVsNew <- function(dt){
  ggplot(dt, aes(x = t, y = percentageShare, fill = Category)) +
    geom_col(position = "fill", color = "gray20") +
    geom_text(
      data = dt %>% filter(Category == "exist"),
      mapping = aes(x = t, y = percentageShare, label = paste0(round(percentageShare*100), "%")),
      vjust = -0.4,
      size = 5,
      color = "black"
    ) +
    scale_fill_manual(values = c(kMidBlue, kDarkestBlue)) +
    scale_x_continuous(breaks = 1:max(dt$t)) +
    scale_y_continuous(labels = FormatPercent) +
    theme_gg() +
    # ggtitle("Share of Revenues from Existing versus New Customers") +
    xlab("Period") +
    ylab("Percentage Share")
}

PlotQuickRatio <- function(fromPeriod, toPeriod, newExistLostRevenue){
  # https://baremetrics.com/academy/saas-quick-ratio
  dtLong <- newExistLostRevenue
  dtWide <- data.table::dcast(dtLong, t ~ Category, value.var = "Revenue")
  dtWide[, quickRatio := ifelse(lost == 0, NA, (-1)*new/lost)]
  dtPlt <- dtWide[(t >= fromPeriod) & (t <= toPeriod)]
    
  ggplot(dtPlt, aes(x = t, y = quickRatio)) +
    geom_line(size = 1, color = kDarkestBlue) +
    geom_point(size = 3, color = kDarkestBlue) +
    scale_x_continuous(breaks = fromPeriod:toPeriod) +
    theme_gg() +
    ggtitle("Quick Ratio = (Added Revenue) / (Lost Revenue)") +
    xlab("Period") +
    ylab("Quick Ratio")
    
}


# Tab Dashboard > Cohort Economics --------------------------------------------


CohortedContributionLTVCACCurves <- function(profitMatrix, cacMatrix, n){
  # Profit matrix where acquisition costs are not substracted
  X <- profitMatrix + cacMatrix
  X <- CumulativeCohortLTVs(X, n)
  acqCost <- cacMatrix[1, 1]
  X / acqCost
}

PlotCohortedContributionLTVCACCurves <- function(X){
  dt <- MatrixToDT(X) %>% 
    mutate(cohort = as.factor(cohort),
           age = age + 1)
  
  ggplot(dt, aes(x = age, y = value, color = cohort)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = min(dt$age):max(dt$age)) +
    theme_gg() +
    xlab("Cohort Age") +
    ylab("LTV/CAC") 
}

CumulativeCohortLTVs <- function(revMatrix, n){
  X <- revMatrix[1:n, 1:n]
  for (i in 1:nrow(X)){
    X[i, i:ncol(X)] <- cumsum(X[i, i:ncol(X)])
  }
  return(X)
}

PlotCumulativeCohortLTVs <- function(X){
  dt <- MatrixToDT(X) %>% 
    mutate(cohort = as.factor(cohort),
           age = age + 1)
  
  ggplot(dt, aes(x = age, y = value, color = cohort)) +
    geom_line() +
    theme_gg() +
    scale_x_continuous(breaks = min(dt$age):max(dt$age)) +
    scale_y_continuous(labels = FormatDollars) +
    xlab("Cohort Age") +
    ylab("Cumulative Revenue")
    # ggtitle("Cumulative Revenue Curves")
}

CumulativeProfitContribution <- function(profitMatrix, 
                                        cacMatrix, 
                                        retCostMatrix, 
                                        varProdCostMatrix, 
                                        beforeAcqCost,
                                        beforeRetCost,
                                        beforeVarProdCost,
                                        n){
  
  X <- profitMatrix[1:n, 1:n]
  
  if (beforeAcqCost){X <- X + cacMatrix[1:n, 1:n]}
  if (beforeRetCost){X <- X + retCostMatrix[1:n, 1:n]}
  if (beforeVarProdCost){X <- X + varProdCostMatrix[1:n, 1:n]}
  
  for (i in 1:nrow(X)){
    X[i, i:ncol(X)] <- cumsum(X[i, i:ncol(X)])
  }
  return(X)
}

PlotCumulativeProfitContributionCurves <- function(adjProfitMatrix, title, subtitle){
  dt <- MatrixToDT(adjProfitMatrix) %>% 
    mutate(cohort = as.factor(cohort),
           age = age + 1)
  
  ggplot(dt, aes(x = age, y = value, color = cohort)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed") +
    theme_gg() +
    scale_x_continuous(breaks = min(dt$age):max(dt$age)) +
    scale_y_continuous(labels = FormatDollars) +
    xlab("Cohort Age") +
    ylab("Cumulative Profits") +
    ggtitle(title, subtitle)
}


# Misc --------------------------------------------------------------------



#' Uppercase the first letter of a string
#'
#' @param x A string e.g. "hello world"
#' @return A string e.g. "Hello world"
#' @examples FirstCharacterUppercase(x = "hello")
FirstCharacterUppercase <- function(x){
  firstChar <- substr(x, start = 1, stop = 1)
  lastChars <- substring(x, first = 2)
  return(paste0(toupper(firstChar), lastChars))
}

#' Adds spaces before every uppercase letter in a string.
#'
#' @param x A string e.g. "HelloWorld"
#' @return A string e.g. "Hello World"
#' @examples SplitStringAtUppercase(x = "HelloWorld")
SplitStringAtUppercase <- function(x){
  result <- gsub("([[:upper:]])", " \\1", x)
  result <- trimws(result)
  return(result)
}


#' Splits a string at dots "." and returns a vector of all pieces.
#'
#' @param x A string e.g. "hello.world"
#' @return A vector of string e.g. c("hello", "world")
#' @examples SplitStringAtDot("hello.world")
SplitStringAtDot <- function(x){
  return(str_split(x, "\\.")[[1]])
}



#' Turns a cohort revenue matrix into a cohort net revenue retention rate matrix.
#'
#' @param revMatrix A cohort-period matrix with revenues in the cells
#' @return matrix
#' @examples 
# RevenueToYearByYearRRR <- function(revMatrix){
NetRevenueRetentionRateMatrix <- function(revMatrix, relativeToPreviousYear){
  if (relativeToPreviousYear == T){
    revMatrix <- Replace(revMatrix, oldValue = 0, newValue = NA)
    Y <- revMatrix[, 1:(ncol(revMatrix)-1)]
    Y <- cbind(NA, Y)
    nrrMatrix <- (revMatrix/Y)
  }
  else {
    revenuesInAcqPeriod <- TransformView(revMatrix)[, 1]
    nrrMatrix <- t(t(revMatrix) / revenuesInAcqPeriod)
  }
  return(nrrMatrix)
}

#' Reverses the characters of a string.
#'
#' @param x A string
#' @return A string
#' @examples ReverseString("!dlrow olleh") # returns "hello world!"
ReverseString <- function(x){
  intToUtf8(rev(utf8ToInt(x)))
}

#' Transforms a number into a dollar-formatted string
#'
#' @param x a number, e.g. -3150.5
#' @param digits number of digits to show after decimal point
#' @return a string
#' @examples FormatNumberToDollars(-3150.5, digits = 3)
FormatNumberToDollars <- function(x, digits = NULL){
  if (is.na(x)){
    return("NA")
  }
  
  isNegative <- x < 0
  
  x <- as.character(abs(x))
  splits <- SplitStringAtDot(x)
  
  l <- splits[1]
  r <- splits[2]
  
  # format the left side of the decimal point
  l <- ReverseString(l)
  l <- strsplit(l, "(?<=.{3})", perl = T)[[1]]
  l <- rev(sapply(l, ReverseString, USE.NAMES = F))
  l <- paste(l, collapse = ",")
  l <- paste0("$", l)
  
  if (isNegative){
    l <- paste0("-", l)
  }
  
  if (is.na(r)){
    return(l)
  }
  
  if (is.null(digits)){
    out <- paste0(l, ".", r)
  } else {
    if (digits > nchar(r)){
      nDigitsLeft <- digits - nchar(r)
      r <- paste0(r, paste(rep("0", nDigitsLeft), collapse = ""))
    } 
    out <- paste0(l, ".", substr(x = r, start = 1, stop = digits))
  }
  return(out)
}

#' #' Formats each element of a vector of numbers into a dollar-formatted string
#'
#' @param xs a vector of numbers, e.g. c(100.5, -130)
#' @param digits number of digits to show after decimal point
#' @return a vector of strings
#' @examples 
#' FormatDollars(10.5)
#' FormatDollars(c(-1035.51313, 510.1), digits = 2)
FormatDollars <- function(xs, digits = NULL){
  if (length(xs) > 1){
    out <- sapply(xs, function(x) FormatNumberToDollars(x, digits = digits))
  } else {
    out <- FormatNumberToDollars(xs)
  }
  return(out)
}

#' Format each element of a vector of numbers into a percentage-formatted string
#'
#' @param x A vector of numbers, e.g. c(0.05, 1.05)
#' @return A vector of strings, e.g. c("5%", "105%")
#' @examples FormatPercent(x = 0.05)
FormatPercent <- function(x, digits = NULL){
  if (!is.null(digits)){
    return(paste0(round(x*100, digits), "%"))
  } else {
    return(paste0(x*100, "%"))
  }
}


#' Returns the number of periods t it takes until the probability of a customer
#' surviving until period t is approx. zero.
#'
#' @param retRate the period-by-period constant customer retention rate
#' @param precision how close we want the survival rate to get to zero
#' @return a number 
NPeriodsToZero <- function(retRate, precision = 1e-6) {
  for (t in 0:10000){
    if (retRate^t <= precision) {
      return(t)
    }
  }
}


# Matrix Utils ------------------------------------------------------------

SurvivalRates <- function(retMatrix){
  diags <- diag(retMatrix)
  retMatrix[is.na(retMatrix)] <- 1
  survMatrix <- t(apply(retMatrix, 1, cumprod))
  survMatrix[survMatrix == 1] <- NA
  diag(survMatrix) <- diags
  return(survMatrix)
}


#' Calculates period-by-period growth rates given a cohort matrix X
#'
#' @param X matrix: cohort array
#' 
#' @return data.table with columns "t" and "growthRate"
GrowthRates <- function(X, n){
  values <- colSums(X[1:n, 1:n], na.rm = T)
  valuesPrevPeriod <- c(NA, values[1:(length(values)-1)])
  growthRates <- (values / valuesPrevPeriod) - 1
  return(data.table(
    t = 1:length(growthRates),
    growthRate = growthRates
  ))
}

AddRowColSums <- function(Xs, X, rowSums, colSums) {
  rowSum <- rowSums(X, na.rm = T)  # infinite time horizon
  colSum <- colSums(Xs, na.rm = T)
  to <- ncol(Xs)
  
  if (rowSums == T & colSums == T){
    Xs <- cbind(Xs, rowSum[1:to])
    Xs <- rbind(Xs, c(colSum[1:to], NA))
  }
  
  if (rowSums == T & colSums == F){
    Xs <- cbind(Xs, rowSum[1:to])
  }
  
  if (rowSums == F & colSums == T){
    Xs <- rbind(Xs, colSums(Xs, na.rm = T))
  }
  return(Xs)
}

NonZeroEntries <- function(matrix){
  matrixOut <- matrix / matrix
  matrixOut[is.na(matrixOut)] <- 0
  matrixOut
}


MatrixToDT <- function(X, na.rm = T){
  dt <- as.data.table(reshape2::melt(X, varnames = c("cohort", "period")))
  dt[, age := period - cohort]
  if (na.rm == T) dt <- dt[!is.na(value)]
  return(dt)
}
  
TransformView <- function(X){
  XTransf <- sapply(1:nrow(X), function(rowIdx){
    row <- X[rowIdx, ]
    rowTransf <- row[rowIdx:ncol(X)]
    rowTransf <- FillUpZeros(rowTransf, ncol(X))
  })
  return(t(XTransf))
}

FillUpZeros <- function(x, length){
  if (length == length(x)) return(x)
  return(c(x, rep(0, length - length(x))))
}

GenDiscountFactors <- function(discRate, lengthOut) {
  discFactors <- sapply(1:lengthOut, function(t) {
    1 / ((1+discRate)^(t-1))
  })
  return(discFactors)
}

Replace <- function(X, oldValue, newValue){
  if (!is.na(oldValue)){
    X[X == oldValue] <- newValue
  } else {
    X[is.na(oldValue)] <- newValue
  }
  return(X)
}

Subset <- function(X, n){
  X <- X[1:n, 1:n]
  return(X)
}

LoremIpsum <- function(textLength, approx = T){
  # Function stolen from https://www.r-bloggers.com/2020/11/little-useless-useful-r-functions-r-lorem-ipsum/
  
  lw <- builtins() #(internal = FALSE)
  lw2 <- help(package="base") 
  lw3 <- ls("package:base")
  lwA <- c(lw,lw2,lw3)
  lwA <- unique(lwA)
  lwA <- trimws(gsub("[[:punct:]]", " ", lwA))
  lw <- lwA
  
  LorIps <- ''
  while (nchar(LorIps) < textLength) {
    lw <- gsub("^ *|(?<= ) | *$", "", lw, perl = TRUE)
    new_w <-  sample(lw,1, replace=TRUE)
    LorIps <- paste(LorIps, new_w, sep = " ")
    if (approx==FALSE){
      LorIps <- substr(LorIps, 1, textLength)
    }
  }
  
  last_word <- tail(strsplit(LorIps ,split=" ")[[1]],1)
  if ((nchar(last_word) == 1) == TRUE) {
    LorIps <- substr(LorIps, 1, nchar(LorIps)-1) # replace last char with blank space
  }
  return(LorIps)
}


# UI  -----------------------------------------------------------

SelectRevenueType <- function(revMatrix, custMatrix, revType){
  if (revType == "revenue"){
    return(revMatrix)
  } else if (revType == "arpu1"){
    arpuMatrix <- revMatrix / custMatrix
    return(arpuMatrix)
  } else if (revType == "arpu2"){
    nCustAtAcquisition <- custMatrix %>% TransformView() %>% .[, 1]
    arpuMatrix <- revMatrix / nCustAtAcquisition
    return(arpuMatrix)
  }
}

# Generate Cohort Matrices -------------------------------------------------------

GenConstantValues <- function(value, dim){
  matrix <- t(sapply(1:dim, function(t) {
    zerosBefore <- rep(NA, t-1)
    values <- rep(value, dim)
    row <- c(zerosBefore, values)
    rowTrimmed <- row[1:dim]
    rowTrimmed
  }))
  return(matrix)
}

GenDiagonalValues <- function(value, dim) {
  return(diag(value, ncol = dim, nrow = dim))
}

GenLinearAgeEffectMatrix <- function(slope, dim){
  values <- sapply(1:dim, function(t) slope*(t-1))
  matrix <- t(sapply(1:dim, function(t){
    row <- c(rep(NA, t-1), values)
    rowTrimmed <- row[1:dim]
  }))
  return(matrix)
}

GenExpGrowthOverTime <- function(factor, dim) {
  values <- sapply(1:dim, function(t) factor^(t-1))
  matrix <- t(sapply(1:dim, function(t){
    row <- c(rep(NA, t-1), values)
    rowTrimmed <- row[1:dim]
  }))
  return(matrix)
}

GenExpGrowthOverCohorts <- function(factor, dim) {
  matrixEntries <- unlist(sapply(1:dim, function(t) {
    row <- c(factor^(t-1), rep(0, dim))
  }))
  matrix <- t(matrix(
    data = matrixEntries,
    ncol = dim,
    nrow = dim
  ))
  return(matrix)
}

GenDiscountMatrix <- function(discRate, dim, startFromPeriod = 1) {
  row <- sapply(1:(dim - startFromPeriod + 1), function(t) {
    1 / ((1+discRate)^(t-1))
  })
  row <- c(rep(0, startFromPeriod-1), row)
  matrix <- matrix(
    data = rep(row, dim), 
    ncol = dim,
    byrow = T
  )
  return(matrix)

}


# App Styling -------------------------------------------------------------

kLightestBlue <- "#F7FBFF"
kMidBlue <- "#bdd7e7"
kDarkestBlue <- "#3182BD"

theme_gg <- function(){
  theme_bw() +
    theme(
      panel.background = element_rect(fill = "#ecf0f1", color = "#ecf0f1"),
      panel.grid.major = element_line(size = 0.5, colour = "white"), 
      panel.grid.minor = element_line(size = 0.5, colour = "white"),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14),
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "plain"),
      axis.title.y = element_text(angle = 90),
      axis.line = element_line(color = "grey50"),
      legend.position = "right",
      legend.text = element_text(color = "grey50")
    )
}

Heading <- function(text){
  style <- "font-weight: bold; margin-top: 1em;"
  return(
    shiny::h3(text, style = style)
  )
}

Space <- function(size=1){
  if (size == 1){
    return(br(style = "margin: 1em 0"))
  } else if (size == 2) {
    return(br(style = "margin: 2em 0"))
  }
}

# Dummies for Testing ---------------------------------------------------------

GenDummyCustMatrix <- function(){
  X <- GenConstantValues(value = 100, dim = 10)
  Y <- GenExpGrowthOverTime(factor = 0.8, dim = 10)
  X*Y
}

ggdummy <- function(){
  data <- data.frame(x = rnorm(15),
                     y = rnorm(15))
  ggplot(data, aes(x = x, y = y)) +
    geom_point()
}



