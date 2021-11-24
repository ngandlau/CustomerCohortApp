tabIntroduction <- list(
    Heading("Introduction to the R Shiny App"),
    tags$p("#todo"),
    # img(src="figure-customer-equity.png", align = "center", style = "width: 100%; max-width: 700px; margin: 1em 0"),
    Heading("Assumptions made for the data-generating process"),
    tags$p("The data-generating process resembles a subscription-firm like Dropbox. Customers are acquired, pay a fee for the upcoming period, and decide at the end of the period whether they want to cancel or renew."),
    tags$ul(
        tags$li("Customers are acquired at the beginning of a period."),
        tags$li("Customers pay in the beginning of a period."),
        tags$li("All revenues and costs occur in the beginning of the period."),
        tags$li("Customers can churn at the end of a period."),
        tags$li("The firm is valued in the beginning of the specified period. 
                 Because all revenues and costs occur also in the beginning of a period, revenues
                 and costs generated in the specified valuation period do not need to be discounted.")
    ),
    Heading("Authors"),
    HTML("<p>App developed by Prof. Dr. Bernd Skiera & <a href='https://www.nilsgandlau.de'>Nils Gandlau</a>.</p>")
)
