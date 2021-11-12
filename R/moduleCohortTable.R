cohortTableUI <- function(id){
    ns <- NS(id)
    tagList(
        radioButtons(ns("arrayType"),
                     label = "",
                     choiceNames = c("Cohort-Period Array", "Cohort-Age Array"), 
                     choiceValues = c(TRUE, FALSE)),
        DTOutput(ns("array"))
    )
}

cohortTableServer <- function(id, X, rowColSumsCP, rowColSumsCA, n, isPercentage, isCurrency, digits){
    moduleServer(
        id,
        function(input, output, session){
            output$array <- renderDT({
                if (input$arrayType == T){
                    Xf <- FormatCohortTableDT(
                        X = X(),
                        to = n(),
                        addRowSums = rowColSumsCP[1],
                        addColSums = rowColSumsCP[2],
                        view = "cohort-period"
                    )
                    plt <- PlotCohortTableDT(
                        X = Xf,
                        isPercentage = isPercentage,
                        isCurrency = isCurrency,
                        digits = digits
                    )
                } else if (input$arrayType == F){
                    Xf <- FormatCohortTableDT(
                        X = X(),
                        to = n(),
                        addRowSums = rowColSumsCA[1],
                        addColSums = rowColSumsCA[2],
                        view = "cohort-age"
                    )
                    plt <- PlotCohortTableDT(
                        X = Xf,
                        isPercentage = isPercentage,
                        isCurrency = isCurrency,
                        digits = digits
                    )
                }
                return(plt)
            })
        }
    )
}
