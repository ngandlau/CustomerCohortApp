plotPeriodCohortViewUI <- function(id){
    ns <- NS(id)
    tagList(
        radioButtons(ns("view"), label = "", choices = c("Period View", "Cohort View")),
        plotOutput(ns("chart"))
    )
}

plotPeriodCohortViewServer <- function(id, 
                               X, 
                               fromPeriod,
                               toPeriod, 
                               geomType,
                               title,
                               subtitle,
                               xlab,
                               ylab,
                               digits,
                               isDollars,
                               colAggFunc=colSums
                               ){
    moduleServer(
        id,
        function(input, output, session){
            output$chart <- renderPlot({
                X <- X()
                toPeriod = toPeriod()
                if (input$view == "Period View"){
                    PlotPeriodView(
                        X = X,
                        fromPeriod = fromPeriod,
                        toPeriod = toPeriod,
                        geomType = geomType,
                        title = title,
                        subtitle = subtitle,
                        xlab = xlab,
                        ylab = ylab,
                        digits = digits,
                        isDollars = isDollars,
                        colAggFunc = colAggFunc
                    )
                } else if (input$view == "Cohort View"){
                    PlotCohortView(
                        X = X,
                        fromPeriod = fromPeriod,
                        toPeriod = toPeriod,
                        geomType = geomType,
                        title = title,
                        subtitle = subtitle,
                        xlab = xlab,
                        ylab = ylab,
                        digits = digits,
                        isDollars = isDollars
                    )
                } 
            })
        }
    )
}