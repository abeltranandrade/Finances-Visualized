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

findOriginalBalance <- function(month_index, title, result_df){
  #Find index where the result_df matches the title we give it and its also the month previous to this
  id_record <- match(title, result_df$title,) & result_df$month == month_index -1
  # balance will be the new balance of the located record
  balance <- ifelse(!is.na(id_record), result_df$new_balance[id_record], NA)
  return(balance)
}

calculateInterest <- function(current_balance, APR){
  monthly_apr <- (APR/100.0 ) /12.0
  return(current_balance*monthly_apr)
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

  total_balance <- sum(debt_df$balance)
  print(total_balance)
  disposable <- tail(disposable_df, n = 1)$amount
  print(paste("disposable ", disposable))
  min_clear <- 0
  month_index <- 1
  #  highest index of a debt with a balance of zero
  debt_index <- 0

  while(total_balance > 0) {

    tackle_money <- disposable + min_clear
    print(paste("tackle money is ", tackle_money))

    while(tackle_money > 0){
      print("result_df ubside while")

      for(debt in 1:nrow(debt_df)){

        if(month_index == 1){
          new_rows <- data.frame(
            month = month_index,
            title = debt_df[debt,]$title,
            original_balance = debt_df[debt,]$balance,
            added_interest = NA,
            new_balance = NA
          )
        temp_balance <- new_rows$original_balance
        print(paste("I get to temp balance ", temp_balance))

        result_df <- dplyr::bind_rows(result_df, new_rows)

        }
        else{
          new_rows <- data.frame(
            month = month_index,
            title = debt$title,
            original_balance = findOriginalBalance(month_index, debt_title, result_df),
            added_interest = NA,
            new_balance = NA
          )

          if(new_rows$original_balance == 0){
            new_rows$added_interest <- 0
            new_rows$new_balance <- 0
          }

          result_df <- dplyr::bind_rows(result_df, new_rows)
        }
      }

      tackle_money <- 0
      # break if you have disposable income after going through each debt new balance(DO I NEED THIS?)ANSWER: YES OR YOU"LL STAY STUCK ON WHILE
      #if(debt == nrow(debt_df) && tackle_money > 0){break}
    }

    #iterate to next month for next row
    month_index <- month_index +1
    #where minimums of debt cleared up to this month have been sum
    min_clear <- sumMinimums(debt_accounts, debt_index)

    total_balance <- 0
    print(paste("total balance is ", total_balance))
  }

  return(result_df)
}



old_simulateProgress <- function(debt_df, disposable_df) {

  result_df <- data.frame(
    month = numeric(),          # Month in the progress
    title = character(),        # Debt ID title
    original_balance = numeric(),  # Balance at the start of the month
    added_interest = numeric(),    # Interest added at the end of month balance
    new_balance = numeric(),       # End of month balance with interest added
    no_change = logical()          # What the balance would be if only paying minimum
  )

  total_balance <- sum(debt_df$balance)
  print(total_balance)
  disposable <- tail(disposable_df, n = 1)$amount
  mean_clear <- 0
  month_index <- 1
  debt_index <- 0

  while(total_balance < 0) {

    tackle_money <- disposable + min_clear

    while(tackle_money > 0){
      print("result_df ubside while")

      for(debt in 1:nrow(debt_df)){
        result_df[month_index, "month"] <- month_index
        result_df[month_index, "title"] <- debt_df[debt, ]$title
        result_df[month_index, "disposable"] <- month_index

        if(month_index == 1){
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_df, debt,
                                                  result_df, tackle = TRUE, firstRow = 1)
        }else{
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_df, debt,
                                                  result_df, tackle = TRUE, firstRow = 0)
        }

        result_df <- new_calculations$dataframe
        # residual from this debt goes to tackle the next debt
        tackle_money <- new_calculations$residual
        #function updates new_debt_index if newly calculated DebtWiped is not NA
        new_debt_index <- checkForWipedDebt(new_calculations$DebtWiped, new_debt_index)

      }
      # break if you have disposable income after going through each debt new balance(DO I NEED THIS?)ANSWER: YES OR YOU"LL STAY STUCK ON WHILE
      if(debt == nrow(debt_df) && tackle_money > 0){break}
    }

    # If there are more debts to update, simulate paying minimums
    if(debt < nrow(debt_df)){
      # : is inclusive so add an index
      untouched = debt + 1
      for(y in untouched:nrow(debt_df)){
        #for all other loans, do only minimums
        if(month_index == 1){
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_df, y,
                                                  result_df, tackle = FALSE, firstRow = "1")
        }else{
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_df, y,
                                                  result_df, tackle = FALSE, firstRow = "0")
        }
        #function updates new_debt_index if newly calculated DebtWiped is not NA
        new_debt_index <- checkForWipedDebt(new_calculations$DebtWiped, new_debt_index)
        #updated dataframe with new calculations simulating paying mins
        result_df <- new_calculations$dataframe
      }
    }

    #iterate to next month for next row
    month_index <- month_index +1
    #update what is the lowest zero'ed debt and sum mins we can use
    debt_index <- new_debt_index
    #where minimums of debt cleared up to this month have been sum
    min_clear <- sumMinimums(debt_accounts, debt_index)

    total_balance <- 0
    print(paste("total balance is ", total_balance))
  }

  return(result_df)
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
              width = 8

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

    print(simulateProgress(debt_info,dis_df))
  })

  }

# Run the application
shinyApp(ui, server)
