# Load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)
library(dplyr)
#' Create Input Unit
#' @description This app needs to query different type of information to create its tools such as income, expenses, disposable income, debts etc they are all different data but all need a header title and a certain number of input fields of certain types. This function generalizes those "units". See wireframe for visual example
#'
#' Required Parameters:
#' @param header General title for all the information needed in this unit
#' @param button_label label on the submission button. Note this should be the second to last parameter
#' @param button_id ID to call  the submitted information in this input field/s. Note this should be the last parameter
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

sumMinimums <- function(debt_df, index){
  if(index == 0){return(0)}
  # index the rows correctly given index (indexing breaks in case 1:1)
  if(index == 1){tosum <- debt_df[1,]}
  else{tosum <- debt_df[1:index,]}
  #sum column
  x <- sum(tosum$minimum)
  return(x)
}

findPreviousBalance <- function(month_index, title, result_df, column_retrival){
  #subset the row from the previous month with a certain title
  subset_previous_entry <- result_df[result_df$month == month_index-1 & result_df$title == title , ]
  # assuming id record returns just 1 bc there will always be only 1 entry for each debt for each month
  # balance will be the new balance of the located record
  balance <- subset_previous_entry[ ,column_retrival]
  return(balance)
}

calculatePaidBalance <- function(tackle, original_balance, disposable_income, minimum) {
  #if we will pay more than minimum
  if(tackle == TRUE){
    paidDown <- original_balance - minimum

    # if the current balance(after minimum) can be fully paid off with disposable income available
    if(paidDown <= disposable_income){
      return(data.frame(balance = 0, residual = disposable_income - paidDown, debtWiped = TRUE))
    }
    else{
      # case: disposable income is not enough to pay the whole debt
      return(data.frame(balance = paidDown - disposable_income, residual = 0, debtWiped = FALSE))
    }

  }
  else{
    paidDown <- original_balance - minimum
    if(paidDown < 0){
      return(data.frame(balance = 0, residual = 0, debtWiped = TRUE))
    }else{
      return(data.frame(balance = paidDown, residual = 0, debtWiped = FALSE))
    }

  }
}

calculateInterest <- function(current_balance, APR){
  monthly_apr <- (APR/100.0 ) /12.0
  return(current_balance*monthly_apr)
}

calculateTotalBalance <- function(current_month, result_df){
  #subset all the rows where month equals current month
  month_subset <- result_df[result_df$month == current_month, ]
  #return the sum of the month's new balance column
  return(sum(month_subset$new_balance))
}

simulateProgress <- function(debt_df, disposable_df) {

  result_df <- data.frame(
    month = numeric(),          # Month in the progress
    title = character(),        # Debt ID title
    original_balance = numeric(),  # Balance at the start of the month
    added_interest = numeric(),    # Interest added at the end of month balance
    new_balance = numeric(),       # End of month balance with interest added
    no_change = logical()          # What the balance would be if only paying minimum
  )

  md_df <- data.frame(
    month = numeric(),
    disposable = numeric()
  )

  total_balance <- sum(debt_df$balance)
  disposable <- tail(disposable_df, n = 1)$amount
  min_clear <- 0
  month_index <- 0
  #  highest index of a debt with a balance of zero
  debt_index <- 0

  while(total_balance > 0) {
    #Update month, total disposable money, and activate the disposable indicator.
    month_index <- month_index + 1
    total_disposable <- disposable + min_clear
    disposable_available <- TRUE
    md_df <- rbind(md_df,data.frame(month = month_index, disposable = total_disposable))

    #create a row for each debt
    for(debt in 1:nrow(debt_df)){
      #get correct current balance
      if(month_index == 1){current_balance <- debt_df[debt,]$balance}
      else{current_balance <- findPreviousBalance(month_index, debt_df[debt,]$title, result_df, "new_balance")}

      #this debt has already been wiped fill in the row with zeroes and next
      if(current_balance == 0){
        temp_row <- data.frame(
          month = month_index,
          title = debt_df[debt,]$title,
          original_balance = current_balance,
          added_interest = 0,
          new_balance = 0)
        result_df <- rbind(result_df, temp_row)
        next
      }

      #calculate decreasing this debt original balance for this month depending if there is disposable available this iteration
      decreasedBalance <- calculatePaidBalance(tackle = disposable_available, current_balance, total_disposable, debt_df[debt,]$minimum)

      #calculate interest using balance after og balance paid down
      dec_balance <- decreasedBalance$balance
      interest <- calculateInterest(dec_balance, debt_df[debt,]$APR)

      temp_row <- data.frame(
        month = month_index,
        title = debt_df[debt,]$title,
        original_balance = current_balance,
        added_interest = interest,
        new_balance = dec_balance + interest)

      result_df <- rbind(result_df, temp_row)

      total_disposable <- decreasedBalance$residual

      #If the disposible income residual is 0, there is no more disposable income available this month for the next debts
      if(total_disposable == 0){disposable_available <- FALSE}
      #if a new debt wipe, add it to the counter (debtWiped can only be TRUE once per debt since og balance if statement wipe debt catch that will never let it get this far)
      if(decreasedBalance$debtWiped == TRUE){debt_index <- debt_index + 1}

    }
    #sum the minimum payments of debts that have been zeroed to help disposable income
    min_clear <- sumMinimums(debt_df, debt_index)
    #calculate the total debt balance left after this month
    total_balance <- calculateTotalBalance(month_index, result_df)
  }
  return(list(timeline = result_df, monthly_disposable = md_df))
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
            column(4, align = "center",
                   createInputUnit("Income", #Input Unit Income
                                    list(label = "Income Amount", id = "income_amount", type = "numeric"),
                                   button_label = "Submit Income", button_id = "income_submit")
                   ,
                   createInputUnit("Expenses", #Input Unit: Expenses
                         list(label = "Expense Title", id = "expense_title", type = "text"),
                         list(label = "Expense Price", id = "expense_price", type = "numeric"),
                         button_label = "Submit Expense", button_id = "expense_submit")
            ),
            column(6, fluidRow(plotlyOutput("pie_chart"), style = "padding-bottom: 20px; padding-top: 30px"),   # Adjust the padding as needed
                   fluidRow(DTOutput("expenseTable")))
          )
        )
      ),
      # Timeline tab (placeholder content)
      tabItem(
        tabName = "timeline",align = "center",
        fluidPage(
          fluidRow(
            column(
              width = 4,
              createInputUnit("Disposable", #Input Unit Income
                              list(label = "Disposable Income", id = "disposable_income", type = "numeric"),
                              button_label = "Submit Disposable Income", button_id = "disposable_submit")
              ,
              createInputUnit("Debts", #Input Unit: Expenses
                              list(label = "Debt Name", id = "debt_title", type = "text"),
                              list(label = "Total Remaining Balance", id = "remaining_balance", type = "numeric"),
                              list(label = "APR", id = "apr", type = "numeric"),
                              list(label = "Monthly Minimum", id = "monthly_min", type = "numeric"),
                              button_label = "Submit Debt Data", button_id = "debt_submit"),
              actionButton("process_debts", "Get Debt Timeline"),
              sliderInput("Timeline", "Move Through The Months", min = 1, max = 50, value = 1)
            ),
            column(
              width = 8,
              plotlyOutput("lineChart")

            )
          )
        )
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
server <- function(input, output) {

  ####budget tab

  # Reactive data frames to store income and expenses submitted #default income to 0 for pie chart error messaging
  income <- reactiveVal(data.frame(monthly_amount = 0))
  expenses <- reactiveVal(data.frame(title = character(), price = numeric()))

  # update income data frame reactive when updated through income_submit button
  observeEvent(input$income_submit, {
    #format observed event result into our income format and bind it
    new_income <- data.frame(monthly_amount = input$income_amount)
    income(rbind(income(), new_income))
  })

  # update expense table reactive when updated through expense_submit button
  observeEvent(input$expense_submit, {
    #format observed event result into our expense format and bind it
    new_expense <- data.frame(title = input$expense_title, price = input$expense_price)
    expenses(rbind(expenses(), new_expense))
  })

  output$pie_chart <- renderPlotly({
    #data
    expenses_df <- expenses()

    #calculate disposable income and create a budget
    last_income <- tail(income(), n = 1)$monthly_amount
    budget_spending <- sum(expenses()$price)
    remaining <- last_income - budget_spending
    budget <- rbind(expenses_df, data.frame(title = "Money Remaining", price = remaining))
    print("this is budget")
    print(budget)

    #create pie chart
    if (nrow(budget) > 0) {
      #avoid error being displayed. Might not be working but leaving for extra security
      tryCatch({
        pie_chart <- plot_ly(budget, labels = ~title, values = ~price, type = "pie")
        pie_chart <- pie_chart %>% layout(title = "Budget of Needs")
        pie_chart
      }, error = function(e) {
        cat("Please input income and expenses information to create a pie chart", conditionMessage(e), "\n")
        #makes the plot not display?
        return(NULL)
      })
    }
  })

  output$expenseTable <- renderDT({
    indexes <-order(expenses()$price, decreasing = TRUE)
    sortedExpenses <- expenses()[indexes, ]
    datatable(sortedExpenses, options = list(pageLength = 10), class = 'cell-border stripe')
  })

### next tab

  disposable <- reactiveVal(data.frame(amount =0))
  debts <- reactiveVal(data.frame(title= character(), balance = numeric(), APR = numeric(), minimum = numeric()))
  timeline <- reactiveVal(data.frame(month = numeric() ,title = character(), original_balance = numeric(), added_interest = numeric(),new_balance = numeric()))

  observeEvent(input$disposable_submit, {
    #format observed event result into our income format and bind it
    new_disposable <- data.frame(amount = input$disposable_income)
    disposable(rbind(disposable(), new_disposable))
    print("disposable submit")
    print(disposable())
  })

  observeEvent(input$debt_submit, {
    #format observed event result into our expense format and bind it
    new_debt <- data.frame(title= input$debt_title, balance = input$remaining_balance, APR = input$apr, minimum = input$monthly_min)
    debts(rbind(debts(), new_debt))
    print("debt submit")
    print(debts())
  })

  observeEvent(input$process_debts, {
    dis_df <- disposable()
    debt_info <- debts()

    print("This is disposable")
    print(dis_df)

    print("This is debt")
    print(debt_info)

    simulation <- simulateProgress(debt_info,dis_df)
    timeline(rbind(timeline(),simulation$timeline))

    sorted_df <- timeline() %>% arrange(title)
    print(sorted_df)
  })

  # Render the plot
  output$lineChart <- renderPlotly({
    df <- timeline()

    df  <- df %>%
      group_by(month) %>%
      summarise(new_balance = sum(new_balance))

    plot_ly(df, x = ~month, y = ~new_balance, type = "bar") %>%
      layout(title = "Bar Plot of new_balance by Month",
             xaxis = list(title = "Month"),
             yaxis = list(title = "new_balance"))
    # Create the line chart using plot_ly
  #   plot_ly(df, x = ~month, y = ~new_balance, type = "scatter", mode = "lines") %>%
  #     layout(title = "Line Chart Faceted by Title",
  #            xaxis = list(title = "Month"),
  #            yaxis = list(title = "New Balance"))
  })
#   plot_ly(df, x = ~month, y = ~new_balance, color = ~title, type = "scatter", mode = "lines") %>%
#     layout(title = "Line Chart Faceted by Title",
#            xaxis = list(title = "Month"),
#            yaxis = list(title = "New Balance"),
#            facet_col = ~title)
# })

  }

# Run the application
shinyApp(ui, server)
