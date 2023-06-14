# Load the required library
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Mortgage Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("value", "Value of the home (€):", value = 450000, min = 1, step = 1000),
      sliderInput("LTV", "Loan to Value ratio (%):", min = 50, max = 100, value = 80),
      sliderInput("term_years", "Term in years:", min = 5, max = 40, value = 25, step = 5),
      sliderInput("interest_rate", "Annual interest rate (%):", min = 1, max = 10, value = 3.5, step = 0.5)
    ),
    
    mainPanel(
      textOutput("monthly_payment_output"),
      textOutput("pr_loan_output"),
      textOutput("pr_paid_output"),
      textOutput("pr_interest_output"),
      textOutput("monthly_interest_output")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Define reactive expressions for inputs
  value <- reactive({input$value})
  LTV <- reactive({input$LTV / 100}) 
  term_years <- reactive({input$term_years})
  annual_interest_rate <- reactive({input$interest_rate / 100})
  
  # Calculate the loan amount
  loan_amount <- reactive({
    LTV() * value()
  })
  
  # Total number of payments
  n_payments <- reactive({
    term_years() * 12
  })
  
  # Monthly interest rate
  monthly_interest_rate <- reactive({
    annual_interest_rate() / 12
  })
  
  # Monthly payment
  monthly_payment <- reactive({
    loan_amount() * (monthly_interest_rate() * (1 + monthly_interest_rate())^n_payments()) / 
      ((1 + monthly_interest_rate())^n_payments() - 1)
  })
  
  # Total amount paid over the life of the loan
  total_paid <- reactive({
    monthly_payment() * n_payments()
  })
  
  # Total interest paid
  total_interest_paid <- reactive({
    total_paid() - loan_amount()
  })
  
  # Monthly interest paid
  monthly_interest_paid <- reactive({
    total_interest_paid() / n_payments()
  })
  
  # Output the results
  output$monthly_payment_output <- renderText({
    paste("Monthly payment is €", format(round(monthly_payment(),digits=0), big.mark = ",", scientific = FALSE))
  })
  output$pr_loan_output <- renderText({
    paste("Total loan amount of €", format(round(loan_amount(),digits=0), big.mark = ",", scientific = FALSE))
  })
  output$pr_paid_output <- renderText({
    paste("Total paid over lifetime of mortgage €", format(round(total_paid(),digits=0), big.mark = ",", scientific = FALSE))
  })
  output$pr_interest_output <- renderText({
    paste("Total interest paid over the term of the loan is €", format(round(total_interest_paid(),digits=0), big.mark = ",", scientific = FALSE))
  })
  output$monthly_interest_output <- renderText({
    paste("Monthly interest paid is €", format(round(monthly_interest_paid(),digits=0), big.mark = ",", scientific = FALSE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
