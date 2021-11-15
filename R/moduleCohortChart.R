cohortChartsUI <- function(id, choices){
    ns <- NS(id)
    tagList(
        radioButtons(ns("chartType"), label = "", choices = choices),
        plotOutput(ns("chart"))
    )
}

cohortChartsServer <- function(id, X, n){
    moduleServer(
        id,
        function(input, output, session){
            output$chart <- renderPlot({
                X <- as.matrix(X()[1:n(), 1:n()])
                if (input$chartType == "Cohort-Period Heatmap"){
                    PlotHeatmap(X = X, x = "period", y = "cohort")
                } else if (input$chartType == "Cohort-Age Heatmap") {
                    PlotHeatmap(X = X, x = "age", y = "cohort")
                } else if (input$chartType == "Customer Cohort Chart") {
                    PlotC3(X)
                } else if (input$chartType == "Linechart (x=age)"){
                    PlotLinechart(X, x = "age")
                } else if (input$chartType == "Linechart (x=period)")
                    PlotLinechart(X, x = "period")
            })
        }
    )
}