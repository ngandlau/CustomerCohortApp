library(shiny)
library(DT)

source("moduleCohortTable.R")
source("moduleCohortChart.R")
source("modulePlotPeriodCohortView.R")
source("utils.R")


# Tab: Introduction -------------------------------------------------------
tabIntroduction <- list(
  # includeMarkdown("documentation.rmd")
  Heading("Introduction to the R Shiny App"),
  p(LoremIpsum(textLength = 500)),
  img(src="figure-customer-equity.png", align = "center", style = "width: 100%; max-width: 700px; margin: 1em 0"),
  Heading("Another Header"),
  p(LoremIpsum(textLength = 500)),
  Heading("Final Header"),
  p(LoremIpsum(textLength = 500)),
  Heading("More Information"),
  p(LoremIpsum(textLength = 250))
)


# Tab: Valuation ---------------------------------------------------------------
tabValuation  <- list(
  Heading("Firm Valuation by Periods versus by Cohorts"),
  radioButtons("isDiscounted", label = " ", choices = list("Discounted Values" = T, "Undiscounted Values" = F)),
  DTOutput("illustrativeValuationTable"),
  Space(),
  fluidRow(
    column(
      6,
      Heading("Valuation by Periods"),
      htmlOutput("stepByStepDCFTable"),
      Space(size = 2),
      plotOutput("valuation_discProfitsPerPeriod")
    ),
    column(
      6,
      Heading("Valuation by Cohorts"),
      htmlOutput("stepByStepCBCVTable"),
      Space(size = 2),
      plotOutput("valuation_discProfitsPerCohort")
    )
  )
)

# Tab: Period View ---------------------------------------------------------------

tabPeriodView <- list(
  Heading("(Vertical) Profit & Loss Statement"),
  DTOutput("periodView_profitLossStatement") 
)

# Tab: Cohort View ---------------------------------------------------------------

tabCohortView <- list(
  Heading("(Horizontal) Profit & Loss Statement"),
  DTOutput("cohortView_cohortViewTable")
)

# Tab: Cohorts ---------------------------------------------------------------

tabCohorts <- list(
  tabsetPanel(
    tabPanel(
      title = "Number of Customers",
      Heading("Cohort Array"),
      cohortTableUI(id = "custMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "nCustomers",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)",
                       "Customer Cohort Chart"
                      ))
    ),
    tabPanel(
      title = "Customer Retention Rate",
      Heading("Type of Customer Retention"),
      radioButtons(inputId = "retentionType", 
                   label = "",
                   choiceNames = c("Customer Retention Rate", "Customer Survival Rate"),
                   choiceValues = c("retentionRate", "survivalRate")),
      Heading("Cohort Array"),
      cohortTableUI(id = "custRetMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "customerRetention",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)"
                     ))
    ),
    tabPanel(
      title = "Revenue",
      radioButtons(inputId = "revenueType", 
                   label = "Metric",
                   choiceNames = c("Revenue per Cohort", "Avg. Revenue per Customer := Revenue(t) / Customers(t)", "Avg. Revenue per Customer := Revenue(t) / Customers at Acquisition"),
                   choiceValues = c("revenue", "arpu1", "arpu2")),
      Heading("Cohort Array"),
      cohortTableUI(id = "revMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "revenue",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=age)",
                       "Linechart (x=period)",
                       "Customer Cohort Chart"
                     ))
    ),
    tabPanel(
      title = "Net Revenue Retention Rate",
      Heading("Calculation of Net Revenue Retention (NRR)"),
      radioButtons(inputId = "revRetType",
                   label = "",
                   choiceNames = c("NRR relative to previous period", "NRR relative to acquisition period"),
                   choiceValues = c("prev", "acq")),
      Heading("Cohort Array"),
      cohortTableUI(id = "revRetMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "revRet",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)"
                     ))
    ),
    tabPanel(
      title = "Profit",
      Heading("Cohort Array"),
      cohortTableUI(id = "profitMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "profits",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)",
                       "Customer Cohort Chart"
                     ))
    ),
    tabPanel(
      title = "Acquisition Costs",
      Heading("Cohort Array"),
      cohortTableUI(id = "cacMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "cac",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)"
                     ))
    ),
    tabPanel(
      title = "Retention Costs",
      Heading("Cohort Array"),
      cohortTableUI(id = "retCostMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "retCost",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)",
                       "Customer Cohort Chart"
                     ))
    ),
    tabPanel(
      title = "Variable Costs",
      Heading("Cohort Array"),
      cohortTableUI(id = "varCostMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "varCost",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)",
                       "Customer Cohort Chart"
                     ))
    ),
    tabPanel(
      title = "Total Costs",
      Heading("Cohort Array"),
      cohortTableUI(id = "costMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "cost",
                     choices = c(
                       "Cohort-Period Heatmap",
                       "Cohort-Age Heatmap",
                       "Linechart (x=period)",
                       "Linechart (x=age)",
                       "Customer Cohort Chart"
                     ))
    )
  )
)


# Tab: Dashboard ----------------------------------------------------------


tabDashboard  <- list(
  tabsetPanel(
    tabPanel(
      title = "Dashboard",
      Heading("Number of Customers"),
      plotPeriodCohortViewUI(id = "dashboard_nCustomers"),
      Heading("Revenues"),
      plotPeriodCohortViewUI(id = "dashboard_revenues"),
      Heading("Average Revenue per Customer"),
      plotPeriodCohortViewUI(id = "dashboard_ARPU"),
      Heading("Marketing Costs (Retention Cost + Acquisition Cost)"),
      plotPeriodCohortViewUI(id = "dashboard_marketingCosts"),
      Heading("Costs broken down by Category"),
      plotOutput("dashboard_costs"),
      Heading("Operating Margin"),
      p("Operating Margin = (Profit before Fixed Cost)/Revenue"),
      radioButtons("dashboard_operatingMarginSwitch", label = "", choices = c("Period View", "Cohort View")),
      plotOutput("dashboard_operatingMargin"),
      Heading("Profits before Fixed Costs"),
      radioButtons("dashboard_profitsBeforeFixedCostsSwitch", label = "", choices = c("Period View", "Cohort View")),
      plotOutput("dashboard_profitsBeforeFixedCosts"),
      Heading("Profits after Fixed Costs"),
      plotOutput("dashboard_profitsAfterFixedCosts")
    ),
    tabPanel(
      title = "SaaS Dashboard",
      Heading("Number of New, Existing, and Lost Customers"),
      plotOutput("dashboard_newExistLostCustomers"),
      Heading("Amount of New, Existing, and Lost Revenue"),
      plotOutput("dashboard_newExistLostRevenues"),
      Heading("Share of Revenues from New vs. Existing Customers"),
      plotOutput("dashboard_shareOfExistVsNewRevenue"),
      Heading("Customer Retention Rate"),
      plotOutput("dashboard_overallCRR"),
      Heading("Net Revenue Retention Rate"),
      plotOutput("dashboard_overallNRR"),
      Heading("Quick Ratio"),
      plotOutput("dashboard_quickRatio"),
      Heading("Cumulative Revenue"),
      plotOutput("dashboard_cohortRevenueLTVCurves"),
      Heading("Cumulative Profit Contribution after Product Cost"),
      plotOutput("dashboard_cohortProfitContributionAfterProductCost"),
      Heading("Cumulative Profit Contribution after Variable Cost"),
      plotOutput("dashboard_cohortProfitContributionAfterVariableCost"),
      Heading("Cumulative Profit Contribution after Product and Marketing Cost"),
      plotOutput("dashboard_cohortProfitContributionAfterAllCost"),
      Heading("(Cumulative Profit Contribution after after Product and Marketing Cost) / Acquisition Costs"),
      plotOutput("dashboard_cohortLTVCACCurves")
    )
  )
)



# UI ----------------------------------------------------------------------
ui <- shiny::navbarPage(
  title = "Firm Simulator",
  theme = "flatly-customized.min.css",
  tabPanel(
    title = "Introduction",
    style = "max-width: 800px",
    tabIntroduction,
  ),
  tabPanel(
    title = "App",
    shiny::sidebarLayout(
      
      # SIDEBAR PANEL -----------------------------------------------------------
      sidebarPanel = shiny::sidebarPanel(
        width = 2,
        Heading("Settings"),
        sliderInput(
          "valuationPeriod",
          label = "Valuation at the end of Period t=?", 
          min = 1, max = 20, step = 1, value = 5
        ),
        Heading("Data-generating Process"),
        numericInput("nNew", "Acquired Customers per Period", value = 20, width = "200px"),
        numericInput("cac", "Customer Acquisition Cost", min = 0, value = 250, width = "200px"),
        numericInput("rev", "Revenue per Customer", value = 200, width = "200px"),
        numericInput("contributionMargin", "Contribution Margin", min = 0, max = 1, value = 0.8, width = "200px"),
        numericInput("retCost", "Retention Cost per Customer", min = 0, value = 20, width = "200px"),
        numericInput("retRate", "Customer Retention Rate", min = 0, max = 1, value = 0.8, width = "200px"),
        numericInput("discRate","Discount Rate", min = 0, max = 0.3, value = 0.1, width = "200px"),
        numericInput("fixedCost", "Fixed Costs per Period", min = 0, value = 5000, width = "200px"),
        numericInput("noa", "Non-Operating Assets", min = 0, value = 0, width = "200px"),
        numericInput("debt", "Debt", value = 0, width = "200px"),
        
        ## ADVANCED SETTINGS
        Heading("Advanced Settings"),
        checkboxInput("advancedSettings", label = "Enable Advanced Settings", value = F),
        conditionalPanel(
          condition = "input.advancedSettings == true",
          numericInput("nNewIncrease", label = "Increase in Acquired Customers per Period", value = 0),
          numericInput("nNewMax", label = "Max. Acquired Customers per Period", value = -1),
          numericInput("cacIncrease", label = "Increase in Customer Acquisition Cost", value = 0),
          numericInput("cacMax", label = "Max. Customer Acquisition Cost", value = -1),
          numericInput("revIncrease", label = "Increase in Revenue per Customer over the Customer's Lifecycle", value = 0),
          numericInput("revMax", label = "Max. Revenue per Customer", value = -1),
          numericInput("retRateIncrease", label = "Increase in Customer Retention Rate over the Customer's Lifecycle", value = 0),
          numericInput("retRateMax", label = "Max. Customer Retention Rate", value = -1)
        )
      ),
      # MAIN PANEL --------------------------------------------------------
      mainPanel = shiny::mainPanel(
        shiny::tabsetPanel(
            tabPanel(title = "Valuation", tabValuation),
            tabPanel(title = "Period View", tabPeriodView),
            tabPanel(title = "Cohort View", tabCohortView),
            tabPanel(title = "Cohort Arrays", tabCohorts),
            tabPanel(title = "Dashboard", tabDashboard)
        )
      )
    )
  )
)
