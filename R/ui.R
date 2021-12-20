library(shiny)
library(DT)

source("moduleCohortTable.R")
source("moduleCohortChart.R")
source("modulePlotPeriodCohortView.R")
source("utils.R")
source("tabIntroduction.R")

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


# Tab: Customer Equity ----------------------------------------------------

tabCustomerEquity <- list(
  Heading("Customer Equity Reporting"),
  DTOutput("ce_customerEquityReporting")
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

# Tab: Cohort Tables ---------------------------------------------------------------

tabCohorts <- list(
  tabsetPanel(
    tabPanel(
      title = "Number of Customers",
      Heading("Cohort Table"),
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
      Heading("Cohort Table"),
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
      radioButtons(inputId = "arrays_revenue_metric", 
                   label = "",
                   choiceNames = c("Revenue per Cohort", "Avg. Revenue per Customer := Revenue(t) / Customers(t)", "Avg. Revenue per Customer := Revenue(t) / Customers at Acquisition"),
                   choiceValues = c("revenue", "arpu1", "arpu2")),
      Heading("Cohort Table"),
      radioButtons("arrays_revenue_arrayType",
                   label = "",
                   choiceNames = c("Cohort-Period Array", "Cohort-Age Array"),
                   choiceValues = c("cohort-period", "cohort-age")
                   ),
      DTOutput("arrays_revenue"),
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
      Heading("Cohort Table"),
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
      Heading("Cohort Table"),
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
      Heading("Calculation of Metric"),
      radioButtons(inputId = "acqCostType",
                   label = "",
                   choiceNames = c("Acquisition Cost per Cohort", "Acquisition Cost per Customer"),
                   choiceValues = c("acqCostPerCohort", "acqCostPerCustomer")),
      Heading("Cohort Table"),
      radioButtons("arrays_acqCost_arrayType",
                   label = "",
                   choiceNames = c("Cohort-Period Array", "Cohort-Age Array"),
                   choiceValues = c("cohort-period", "cohort-age")
      ),
      DTOutput("arrays_acqCost"),
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
      Heading("Cohort Table"),
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
      Heading("Cohort Table"),
      cohortTableUI(id = "prodCostMatrix"),
      Heading("Visualizations"),
      cohortChartsUI(id = "prodCost",
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
      Heading("Cohort Table"),
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
      # ... Number of Customers -----------------------------------------------------
      Heading("Number of Customers"),
      radioButtons(
        "dashboard_nCustomersSwitch",
        label = "",
        choiceNames = c(
          "Number of Customers by Periods",
          "Number of Customers by Cohorts",
          "Number of Lost, Existing, and New Customers",
          "Customer Retention Rate"
        ),
        choiceValues = c(
          "period_view",
          "cohort_view",
          "lost_exist_new",
          "customer_retention_rate"
        ),
        width = "100%"
      ),
      plotOutput("dashboard_nCustomers"),

      # ... Revenues -----------------------------------------------------
      Heading("Revenues"),
      radioButtons(
        "dashboard_revSwitch",
        label = "",
        choiceNames = c(
          "Revenues by Periods",
          "Revenues by Cohorts",
          "Lost, New, and Existing Revenues",
          "Quick Ratio (= New Revenue / Lost Revenue)",
          "Share of Revenues from Existing vs New Customers",
          "Net Revenue Retention Rate"
        ),
        choiceValues = c(
          "period_view",
          "cohort_view",
          "lost_exist_new",
          "quick_ratio",
          "share_exist_new",
          "net_revenue_retention"
        ),
        width = "100%"
      ),
      plotOutput("dashboard_revenues"),
      
      # ... ARPU -----------------------------------------------------
      Heading("Average Revenue per Customer"),
      radioButtons(
        "dashboard_arpuSwitch",
        label = "",
        choiceNames = c(
          "Avg. Revenue per Customer by Periods",
          "Avg. Revenue per Customer by Cohorts"
        ),
        choiceValues = c(
          "period_view",
          "cohort_view"
        )
      ),
      plotOutput("dashboard_ARPU"),
      
      # ... Costs -----------------------------------------------------
      Heading("Costs"),
      radioButtons(
        "dashboard_costSwitch",
        label = "",
        choiceNames = c(
          "Total Cost by Categories",
          "Marketing Cost by Periods",
          "Marketing Cost by Cohorts"
        ),
        choiceValues = c(
          "cost_by_categories",
          "marketing_cost_periods",
          "marketing_cost_cohorts"
        )
      ),
      plotOutput("dashboard_costs"),
      
            
      # ... Profits -----------------------------------------------------
      Heading("Profits"),
      radioButtons(
        inputId = "dashboard_profitsSwitch",
        label = "",
        choiceNames = c(
          "Profits after Fixed Cost by Periods",
          "Profits before Fixed Cost by Periods",
          "Profits before Fixed Cost by Cohorts",
          "Operating Margin before Fixed Cost by Periods",
          "Operating Margin before Fixed Cost by Cohorts"
        ),
        choiceValues = c(
          "profits_after_fixcost_by_periods",
          "profits_by_periods",
          "profits_by_cohorts",
          "opmargin_by_periods",
          "opmargin_by_cohorts"
        ),
        width = "100%"
      ),
      plotOutput("dashboard_profits"),
      
      # ... Cumulative Cohort Lifetime Curves ----------------------------------
      Heading("Cumulative Cohort Lifetime Curves"),
      radioButtons(
        inputId = "dashboard_cohortLifetimeCurvesSwitch",
        label = "",
        choiceNames = c(
          "Cumulative Cohort Revenues",
          "Cumulative Cohort Revenues / Cohort Acquisition Cost",
          "Cumulative Cohort Profits after Product Cost",
          "Cumulative Cohort Profits after Variable Cost",
          "Cumulative Cohort Profits after Variable Cost and Acquisition Cost",
          "Cumulative Cohort Profits after Variable Cost and Acquisition Cost / Cohort Acquisition Costs"
        ),
        choiceValues = c(
          "cum_revenue",
          "cum_revenue/cac",
          "cum_profit_after_prodcost",
          "cum_profit_after_varcost",
          "cum_profit_after_prodcost_and_marcost",
          "cum_profit_after_prodcost_and_marcost/cac"
        ), 
        width = "100%"
      ),
      plotOutput("dashboard_cohortLifetimeCurves")
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
        numericInput("revIntercept", "Revenue per Customer", value = 200, width = "200px"),
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
          Heading2("Number of Acquired Customers"),
          numericInput("nNewSlope", label = "Slope across Periods", value = 0),
          fluidRow(
            column(6, numericInput("nNewMin", label = "Min", value = -1)),
            column(6, numericInput("nNewMax", label = "Max", value = -1))
          ),
          Heading2("Customer Acquisition Cost"),
          numericInput("cacSlope", label = "Slope across Periods", value = 0),
          fluidRow(
            column(6, numericInput("cacMin", label = "Min", value = -1)),
            column(6, numericInput("cacMax", label = "Max", value = -1))
          ),
          Heading2("Revenue per Customer"),
          numericInput("revAgeSlope", label = "Slope across the Lifecycle", value = 0),
          numericInput("revCohortSlope", label = "Slope across Cohorts", value = 0),
          fluidRow(
            column(6, numericInput("revMin", label = "Min", value = -1)),
            column(6, numericInput("revMax", label = "Max", value = -1))
          ),
          Heading2("Customer Retention Rate"),
          numericInput("retRateAgeSlope", label = "Slope across the Lifecycle", value = 0),
          numericInput("retRateCohortSlope", label = "Slope across Cohorts", value = 0),
          fluidRow(
            column(6, numericInput("retRateMin", label = "Min", value = 0)),
            column(6, numericInput("retRateMax", label = "Max", value = 0))
          )
        )
      ),
      # MAIN PANEL --------------------------------------------------------
      mainPanel = shiny::mainPanel(
        shiny::tabsetPanel(
            tabPanel(title = "Valuation", tabValuation),
            tabPanel(title = "Customer Equity Reporting", tabCustomerEquity),
            tabPanel(title = "Period View", tabPeriodView),
            tabPanel(title = "Cohort View", tabCohortView),
            tabPanel(title = "Cohort Tables", tabCohorts),
            tabPanel(title = "Dashboard", tabDashboard)
        )
      )
    )
  )
)
