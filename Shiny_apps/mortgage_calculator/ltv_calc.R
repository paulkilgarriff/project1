# Load the required library
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Mortgage Loan Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("term_years", "Term in years:", min = 5, max = 40, value = 25, step = 5),
      sliderInput("interest_rate", "Annual interest rate (%):", min = 1, max = 10, value = 3.5, step = 0.5),
      sliderInput("LTV", "Loan to Value ratio (%):", min = 50, max = 100, value = 80),
      numericInput("monthly_payment", "Monthly payment (€):", value = 2000, min = 1, step = 100)
    ),
    
    mainPanel(
      textOutput("loan_value_output"),
      textOutput("total_interest_output"),
      textOutput("monthly_interest_output")
      
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Define reactive expressions for inputs
  term_years <- reactive({input$term_years})
  annual_interest_rate <- reactive({input$interest_rate / 100})
  LTV <- reactive({input$LTV / 100}) 
  monthly_payment <- reactive({input$monthly_payment})
  
  # Total number of payments
  n_payments <- reactive({
    term_years() * 12
  })
  
  # Monthly interest rate
  monthly_interest_rate <- reactive({
    annual_interest_rate() / 12
  })
  
  # Loan amount based on monthly payment, term and interest rate
  loan_amount <- reactive({
    monthly_payment() / (monthly_interest_rate() * (1 + monthly_interest_rate())^n_payments()) * 
      ((1 + monthly_interest_rate())^n_payments() - 1)
  })
  
  # Total value of the home
  home_value <- reactive({
    loan_amount() / LTV()
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
  output$loan_value_output <- renderText({
    paste("The loan value is €", format(round(loan_amount(),digits=0), big.mark = ",", scientific = FALSE),
          "and the home value is €", format(round(home_value(),digits=0), big.mark = ",", scientific = FALSE))
  })
  output$total_interest_output <- renderText({
    paste("Total interest paid over the term of the loan is €", format(round(total_interest_paid(),digits=0), big.mark = ",", scientific = FALSE))
  })
  output$monthly_interest_output <- renderText({
    paste("Monthly interest paid is €", format(round(monthly_interest_paid(),digits=0), big.mark = ",", scientific = FALSE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
