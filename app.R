# Load libraries
library(shiny)
library(shinydashboard)


#' Create Input Unit
#' @description This app needs to query different type of information to create its tools such as income, expenses, disposable income, debts etc they are all different data but all need a header title and a certain number of input fields of certain types. This function generalizes those "units". See wireframe for visual example
#'
#' Required Parameters:
#' @param header General title for all the information needed in this unit
#' @param button_label label on the submission button. Note this should be the second to last parameter
#' @param button_id ID to call  the submitted information in this input field/s. Note this should be the last parameter
#' @param list(label,id,type)) Specifies individual info for each input field desired in this unit. One required but can have multiple. Types posible "numeric" and "text"
#'
#' @return Final stuff
# Function to create input fields that contain a header, paragraph, and variable number of input elements.
createInputUnit <- function(header, ..., button_label, button_id) {
  fluidRow(h3(header),
            p(paste("Enter your", tolower(header), "details:")),
            lapply(list(...), function(input_info) {
             switch(input_info$type,
                    numeric = numericInput(input_info$id, label = input_info$label, value = 0),
                    text = textInput(input_info$id, label = input_info$label, value = "")
              )
            }),
         actionButton(button_id, button_label)
  )
}


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Finances Visualized"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Budget", tabName = "budget", icon = icon("usd")),
      menuItem("Timeline", tabName = "timeline", icon = icon("line-chart")),
      menuItem("Dates", tabName = "dates", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Budget tab
      tabItem(
        tabName = "budget",
        fluidPage(
          fluidRow(
            column(6,
                   createInputUnit("Income", #Input Unit Income
                                    list(label = "Income Amount", id = "income_amount", type = "numeric"),
                                   button_label = "Submit Income", button_id = "income_submit")
                   ,
                   createInputUnit("Expenses", #Input Unit: Expenses
                         list(label = "Expense Title", id = "expense_title", type = "text"),
                         list(label = "Expense Price", id = "expense_price", type = "numeric"),
                         button_label = "Submit Expense", button_id = "expense_submit")
            ),
            column(6, h3("Column 2"))
          )
        )
      ),

      # Timeline tab (placeholder content)
      tabItem(
        tabName = "timeline",
        h2("Timeline Content Goes Here")
      ),

      # Dates tab (placeholder content)
      tabItem(
        tabName = "dates",
        h2("Dates Content Goes Here")
      )
    )
  )
)

# Define server logic (not required for this example)
server <- function(input, output) { }

# Run the application
shinyApp(ui, server)
