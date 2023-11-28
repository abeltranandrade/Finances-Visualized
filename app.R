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
#' @return creates the desire amount of UI input blocks for the unit
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

new_createTimeline <-  function(debt_accounts, disposable_income) {
  createCorrespondingColumns <- function(debt_title){
    lapply(debt_title, function (debt){
    debt_columns <- paste0(debt, c("_Original_Balance", "_Interest_Added", "_New_Balance"))
    })
  }
  #data looks different for both cases
  calculateOriginalBalance <- function(month ,data, results, firstRow) {
    if(firstRow == TRUE){
      for( debt in 1:nrow(data)){
        current <- data[debt,]
        results[month, paste0(current$title,"_Original_Balance")] <- current$balance
      }
    }else{
      # Put the past month's same title new balance onto the next month original balance
      results[month, paste0(data$title,"_Original_Balance")] <- results[month-1, paste0(data$title,"_New_Balance")]
    }
    return(results)
  }

  calculateNewBalance <- function(month, disposable, debt_df, debt_index, result_df, tackle, firstRow) {

      # select the debt we are creating a new balance for
      current_debt <- debt_df[debt_index,]

      #calculate monthly APR
      monthly_apr <- (current_debt$APR/100.0 ) /12.0

      # What is the most recient balance being held on a certain debt
      # if first row, balance is in the debt dataset while other rows need their previous month new balance
      #if we want to get rid of this current statement and just do the original balance with the same title but I dont know if that will make that big of a difference. We will have to create a test error pulling case if the original balances for a certain debt is still NA
      current_balance <- switch(firstRow,
                                 "1" = current_debt$balance,
                                 "0" = result_df[ month-1, paste0(current_debt$title,"_New_Balance")])


      # take away the minimum payment which we assume we have included in our needs budget until out of debt
      current_balance <- current_balance - current_debt$minimum

      # if we can wipe out the debt this month (less debt balance than income)
      if(disposable > current_balance || current_balance < 0 ){
        # covering case that current balance goes negative from the minimum subtraction
        if(current_balance < 0){current_balance <- 0}

        #update the new values to the rows
        result_df[month, paste0(current_debt$title,"_Interest_Added")] <- 0
        result_df[month, paste0(current_debt$title,"_New_Balance")] <- 0


        # calculate amount of income remaining after clearing this debt
        residual <-  disposable - current_balance
        print(paste("INSIDE FUNC NEWBALANCE: Disposable is ", disposable, " and current balance is", current_balance, " Then residual is ", residual))

        # return updated dataframe and the amount of income left to tackle debt this month
        return(list(dataframe = result_df, residual = residual, DebtWiped = debt_index))
      }
      else{ # Not enough disposable income to tackle this debt fully

      # If you have to tackle with disposable income or not
      if(tackle == TRUE){
        temp_balance <- current_balance - disposable
      }else{
        # we already subtracted the minimum amount from current balance so temp is same as current
        temp_balance <- current_balance
      }

      #calculating this months interest added
      interest <- temp_balance * monthly_apr
      new_balance <- temp_balance + interest

      #update the new values to the rows
      result_df[month, paste0(current_debt$title,"_Interest_Added")] <- interest
      result_df[month, paste0(current_debt$title,"_New_Balance")] <- new_balance

      #If we do not tackle the debt in this one, we will have to return the dataframe and say that there is no more disposable income this month
      return(list(dataframe = result_df, residual = 0, DebtWiped = NA))
      }
  }

  findNewBalance <- function(row) {
    # identify the new balance column inded
    new_balance_columns <- grep("New_Balance$", names(row), value = TRUE)
    # Sum the values in the selected columns for the current row
    sum_values <- sum(row[new_balance_columns])
    return(sum_values)
  }

  updateZeros <- function(month, debt_title, timeline_df){
      timeline_df[month,paste(title, "_Original_Balance")] <- 0
      timeline_df[month,paste(title, "_New_Balance")] <- 0
      timeline_df[month, paste(title, "Interest_Added")] <- 0
    # Return the updated dataframe
    return(timeline_df)
  }

  sumMinimums <- function(debt_df, index){
    # index the rows correctly given index (indexing breaks in case 1:1)
    if(index == 1){tosum <- debt_df[1,]}
    else{tosum <- debt_df[1:index,]}
    #sum column
    x <- sum(tosum$minimum)
    return(x)
  }

  sumMinimums2 <- function(debt_df, index){
    # index the rows correctly given index (indexing breaks in case 1:1)
    row <- debt_df[1,]$minimum

    #sum column
    x <- sum(tosum$minimum)
    return(x)
  }

  checkForWipedDebt <- function(new_calc_return,current_debt_index){
    if(!is.na(new_calc_return)){return(new_calc_return)}
    else{return(current_debt_index)}
  }

  createNewMonth <- function(month_index, disposable, debt_df, timeline_results2, firstRow, ClearDebt_index){
    timeline_results2[month_index, 'Month'] <- month_index
    tackle_money <- disposable

  }

  #initiating "tracking" functions
  min_clear <- 0
  month_index <- 1
  total_balance <- 1
  first_row <- TRUE
  #the lowest index of a debt that still has a balance (not paid off)
  debt_index <- 0

  #default variables
  disposable <- tail(disposable_income, n = 1)$amount
  default_columns <- c("Month", "Disposable")

  # creates vector with the modifiable columns for each debt
  Debt_columns <- createCorrespondingColumns(debt_accounts$title)
  varColumnTitles <- unlist(Debt_columns)
  columnTitles <- c(default_columns, varColumnTitles)
  # create result dataframe
  timeline_results <- data.frame(setNames(replicate(length(columnTitles), numeric()), columnTitles))

  # First row is an edge case
  if(first_row == TRUE){
    #fill default columns
    timeline_results[month_index, 'Month'] <- month_index
    timeline_results[month_index, 'Disposable'] <- disposable

    tackle_money <- disposable

    #calculate original balances
    timeline_results2 <- calculateOriginalBalance(month_index, debt_accounts, timeline_results, firstRow = TRUE )

    #while we have disposable money this month (this will finish the first row)
    while(tackle_money > 0){
    # Go through debts and update their balances while we still have disposable income
      for(i in 1:nrow(debt_accounts)){
      # while there is disposable money to use, use tackle = TRUE
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, i,
                                             timeline_results2, tackle = TRUE, firstRow = 1)
          #updated dataframe with new calculations
          timeline_results2 <- new_calculations$dataframe
          # residual from this debt goes to tackle the next debt
          tackle_money <- new_calculations$residual
          #function updates new_debt_index if newly calculated DebtWiped is not NA
          new_debt_index <- checkForWipedDebt(new_calculations$DebtWiped, new_debt_index)
      }
    }
    # if there are still un-updated debts to calculate after disposable is gone
    if(i < nrow(debt_accounts)){
      # : is inclusive so add an index
      untouched = i + 1
      for(j in untouched:nrow(debt_accounts)){
        # For all the other j debts, calculate new balance only through minimum
        new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, j,
                                                timeline_results2, tackle = FALSE, firstRow = 1)
        #function updates new_debt_index if newly calculated DebtWiped is not NA
        new_debt_index <- checkForWipedDebt(new_calculations$DebtWiped, new_debt_index)
        #update the dataframe with new calculations
        timeline_results2 <- new_calculations$dataframe
      }
    }

    #I would need a new iteration of the month to happen after this
    month_index <- month_index +1
    # Update the lowest debt's index that has been zero'ed and calculate the minimum payments to snowball
    debt_index <- new_debt_index
    min_clear <- sumMinimums(debt_accounts, debt_index)

    # since we updated month(finished a row), we have to refresh the tackle_money
    tackle_money <- disposable + min_clear

    #Finished with the first row
    first_row <- FALSE
  }


  # while there are still balances on any debt (each iteration represents a month)
  while(total_balance > 0){

    # create default columns
    timeline_results2[month_index, 'Month'] <- month_index
    timeline_results2[month_index, 'Disposable'] <- tackle_money
    #generate next row of original balances
    timeline_results2 <- calculateOriginalBalance(month_index, debt_accounts, timeline_results2, firstRow = FALSE )

    #while there is still disposable income, update debt balance with it
    while(tackle_money > 0){
      # go through each loan and calculate its new balance this month
      for(x in 1:nrow(debt_accounts)){
          # update new balance columns for x debt using disposible income
            new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, x,
                                                    timeline_results2, tackle = TRUE, firstRow = "0")
            #update dataframe with new calculations
            timeline_results2 <- new_calculations$dataframe
            # residual from this debt goes to tackle the next debt
            tackle_money <- new_calculations$residual
            #function updates new_debt_index if newly calculated DebtWiped is not NA
            new_debt_index <- checkForWipedDebt(new_calculations$DebtWiped, new_debt_index)

      }
      # break if you have disposable income after going through each debt new balance(DO I NEED THIS?)ANSWER: YES OR YOU"LL STAY STUCK ON WHILE
      if(x == nrow(debt_accounts) && tackle_money > 0){break}
    }

    # If there are more debts to update, simulate paying minimums
    if(x < nrow(debt_accounts)){
      # : is inclusive so add an index
      untouched = x + 1
      for(y in untouched:nrow(debt_accounts)){
        #for all other loans, do only minimums
        new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, y,
                                                timeline_results2, tackle = FALSE, firstRow = "0")
        #function updates new_debt_index if newly calculated DebtWiped is not NA
        new_debt_index <- checkForWipedDebt(new_calculations$DebtWiped, new_debt_index)
        #updated dataframe with new calculations simulating paying mins
        timeline_results2 <- new_calculations$dataframe
      }
    }

    #iterate to next month for next row
    month_index <- month_index +1
    #update what is the lowest zero'ed debt and sum mins we can use
    debt_index <- new_debt_index
    min_clear <- sumMinimums(debt_accounts, debt_index)
    # since we updated month(finished a row), we have to refresh the tackle_money
    tackle_money <- disposable + min_clear



    print("end of the month results are")
    print(timeline_results2)

    #calculate total debt balance after this month's updated values
    latest_results <- tail(timeline_results2 , n = 1)
    total_balance <- findNewBalance(latest_results)
    print(paste("Total Balance at the end of month ", month_index))
    print(total_balance)
    print("**************************************************************")
  }

  # return once all debts have been zero'ed
  return(timeline_results2)

}

# Define UI
#####
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
      #####
      tabItem(
        tabName = "budget",
        fluidPage(
          fluidRow(
            column(4,align = "center",
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
      #####
      # Timeline tab
      #####
      tabItem(
        tabName = "timeline",align = "center",
        fluidPage(
          fluidRow(
            column(
              width = 4,
              createInputUnit("Disposable", #Input Unit Income
                              list(label = "Disposable Income", id = "disposable_income", type = "numeric"),
                              button_label = "Submit Disposable Income", button_id = "Disposable_submit")
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
              fluidRow(DTOutput("TimelineTable"))
            )
          )
        )
      ),
      #####
      # dates Tab
      #####
        tabItem(
          tabName = "dates",
          h2("Dates Content Goes Here"),
          tabsetPanel(
            tabPanel("Tab 1", value = "tab1"),
            tabPanel("Tab 2", value = "tab2"),
            uiOutput("dynamic_tab")
          )
        )
      #####
    )
  )
)
#####

# Define server logic
server <- function(input, output) {

  #budget tab
  #####

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

#####

  #timeline Tab Start
  ########

  disposable <- reactiveVal(data.frame(amount =0))
  debts <- reactiveVal(data.frame(title= character(), balance = numeric(), APR = numeric(), minimum = numeric()))
  timeline <- reactiveVal()
  observeEvent(input$Disposable_submit, {
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


    print("This is the function Timeline")
    #print(createTimeline(debt_info, dis_df))
    #print(new_createTimeline(debt_info, dis_df))
    timeline(new_createTimeline(debt_info, dis_df))


  })

  output$custom_value_box <- renderValueBox({
    valueBox(
      value = 123,
      subtitle = "Subtitle",
      icon = icon("star"),
      color = "purple",
      href = "#",  # Add a hyperlink if needed
      width = "100%"
    )
  })


  output$mean_sqft = renderValueBox({
    valueBox(
      paste0("$" ),
      "Mean property size (in sq. ft.)"
    )
  })

  output$check <- renderDT({
    indexes <-order(expenses()$price, decreasing = TRUE)
    sortedExpenses <- expenses()[indexes, ]
    datatable(sortedExpenses, options = list(pageLength = 10), class = 'cell-border stripe')
  })

  output$TimelineTable <- renderDT({
    datatable(timeline(), options = list(pageLength = 10), class = 'cell-border stripe')
  })

########

  #other tab
  #####
  tab_content <- list(
    tab1 = textInput("data_tab1", "Enter data for Tab 1"),
    tab2 = textInput("data_tab2", "Enter data for Tab 2")
    # Add more tabs and corresponding widgets as needed
  )

  # Render dynamic tab content
  output$dynamic_tab <- renderUI({
    selected_tab <- input$dynamic_tab
    widget <- tab_content[[selected_tab]]
    widget
  })
#####

  }

# Run the application
shinyApp(ui, server)

