library(shiny)

ui <- fluidPage(
  titlePanel("Housing Price Evaluation Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("size", "Size (m2):", min = 1, max = 10000, value = 100),
      numericInput("bedrooms", "Number of bedrooms:", min = 1, max = 10, value = 1),
      numericInput("bathrooms", "Number of bathrooms:", min = 1, max = 10, value = 1),
      selectInput("ber_rating", "BER rating:",
                  choices = c("A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","E1","E2","F","G"), 
                  selected = "A1"
      ),
      checkboxInput("property_type", "Property is a house (tick if yes)"),
      conditionalPanel(
        condition = "input.property_type == true",
        selectInput("house_type", "House type:",
                    choices = c("Detached", "Semi-detached", "Terrace", "End-terrace"),
                    selected = "Detached"
        )
      ),
      numericInput("plot_size", "Plot size (m2):", min = 1, max = 10000, value = 100),
      selectInput("county", "County:",
                  choices = c("Dublin", "Galway"),
                  selected = "Dublin"
      ),
      conditionalPanel(
        condition = "input.county == 'Dublin'",
        selectInput("area_dublin", "Area:", choices = c("Blackrock", "Lucan"), selected = "Blackrock")
      ),
      conditionalPanel(
        condition = "input.county == 'Galway'",
        selectInput("area_galway", "Area:", choices = c("Oranmore", "Athenry"), selected = "Oranmore")
      )
    ),
    
    mainPanel(
      h3("Estimated price:"),
      textOutput("estimated_price"),
      h3("Attribute contributions:"),
      textOutput("contributions")
    )
  )
)

server <- function(input, output) {
  estimated_price <- reactive({
    size_value = ifelse(input$size > 180, 180 * 2200 + (input$size - 180) * 2200 * 0.9, input$size * 2200)
    bedrooms_value = input$bedrooms * 14000
    bathrooms_value = input$bathrooms * 10000
    
    ber_values = c('A1' = 40000, 'A2' = 35000, 'A3' = 30000, 'B1' = 25000, 'B2' = 20000, 'B3' = 15000, 
                   'C1' = 10000, 'C2' = 5000, 'C3' = 0, 'D1' = -5000, 'D2' = -10000, 'E1' = -15000, 
                   'E2' = -20000, 'F' = -25000, 'G' = -50000)
    ber_value = ber_values[input$ber_rating]
    
    house_values = c('Detached' = 50000, 'Semi-detached' = 30000, 'Terrace' = 15000, 'End-terrace' = 25000)
    house_value = ifelse(input$property_type, house_values[input$house_type], 0)
    
    total_value = size_value + bedrooms_value + bathrooms_value + ber_value + house_value
    
    list(total = total_value, size = size_value, bedrooms = bedrooms_value, bathrooms = bathrooms_value, 
         ber = ber_value, house = house_value)
  })
  
  output$estimated_price <- renderText({
    paste0("€", round(estimated_price()$total, 2))
  })
  
  output$contributions <- renderText({
    total_value = estimated_price()$total
    size_contribution = estimated_price()$size / total_value * 100
    bedrooms_contribution = estimated_price()$bedrooms / total_value * 100
    bathrooms_contribution = estimated_price()$bathrooms / total_value * 100
    ber_contribution = estimated_price()$ber / total_value * 100
    house_contribution = estimated_price()$house / total_value * 100
    
    paste("Size: ", round(size_contribution, 2), "%",
          "Bedrooms: ", round(bedrooms_contribution, 2), "%",
          "Bathrooms: ", round(bathrooms_contribution, 2), "%",
          "BER: ", round(ber_contribution, 2), "%",
          "Type: ", round(house_contribution, 2), "%")
  })
}

shinyApp(ui = ui, server = server)

