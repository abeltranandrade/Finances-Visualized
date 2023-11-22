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

  calculateNewBalance <- function(month, disposable, debt_df, debt_index, result_df, tackle, firstRow = FALSE) {

      # select the debt we are creating a new balance for
      current_debt <- debt_df[debt_index,]

      #calculate monthly APR
      monthly_apr <- (current_debt$APR/100) /12.0
      # maximum we have to pay down the loan this month
      disposable <- disposable + current_debt$minimum

      # What is the most recient balance being held on a certain debt
      # if first row, previous balance is the same row's original balance, while other rows is the previous row new balance
      current_balance <- switch(firstRow,
                                 "1" = current_debt$balance,
                                 "0" = result_df[ month-1, paste0(current_debt$title,"_New_Balance")])

      # TODO: THis is wrong! bc it uses the current balance of the OG debt not the one changing in each row. Gotta do the division earlier
      # if we can wipe out the debt this month (less balance on card than money)
      if(disposable > current_balance){
        #update the new values to the rows
        result_df[month, paste0(current_debt$title,"_Interest_Added")] <- 0
        result_df[month, paste0(current_debt$title,"_New_Balance")] <- 0
        residual <-  disposable - current_debt$balance
        # When we make the for loop in the outside to get to choose which ones to pay down and which not to we will have to return here
        return(list(dataframe = result_df, residual = residual))
      }
      else{
      # previous_balance <- switch(firstRow,
      #                            "1" = result_df[ month, paste0(current_debt$title,"_Original_Balance")],
      #                            "0" = result_df[ month-1, paste0(current_debt$title,"_New_Balance")])

      # If you have to tackle with disposable income or not
      if(tackle == TRUE){
        temp_balance <- current_balance - disposable
      }else{
        temp_balance <- current_balance - current_debt$minimum
      }
      print("Temp Balance is")
      print(temp_balance)
      #calculating this months interest added
      interest <- temp_balance * monthly_apr
      new_balance <- temp_balance + interest

      #update the new values to the rows
      result_df[month, paste0(current_debt$title,"_Interest_Added")] <- interest
      result_df[month, paste0(current_debt$title,"_New_Balance")] <- new_balance

      print("result_df inside new balance is ")
      print(result_df)
      #If we do not tackle the debt in this one, we will have to return the dataframe and say that there is no more disposable income this month
      return(list(dataframe = result_df, residual = 0))
      }
    }

  min_clear <- 0
  disposable <- tail(disposable_income, n = 1)$amount
  month_index <- 1
  total_balance <- 1
  first_row <- TRUE
  #the lowest index of a debt that still has a balance # Get the lowest non paid off debt current_debt <- debt_accounts[debt_index,]
  debt_index <- 1

  default_columns <- c("Month", "Disposable")
  # creates vector with the modifiable columns for each debt
  Debt_columns <- createCorrespondingColumns(debt_accounts$title)
  varColumnTitles <- unlist(Debt_columns)
  columnTitles <- c(default_columns, varColumnTitles)
  # create result dataframe
  timeline_results <- data.frame(setNames(replicate(length(columnTitles), numeric()), columnTitles))

  if(first_row == TRUE){
    timeline_results[month_index, 'Month'] <- month_index
    timeline_results[month_index, 'Disposable'] <- disposable
    tackle_money <- disposable

    #calculate original balances once
    timeline_results2 <- calculateOriginalBalance(month_index, debt_accounts, timeline_results, firstRow = TRUE )

    #while we still have disposible money this month
    while(tackle_money > 0){
    # Go through each debt
      for(i in 1:nrow(debt_accounts)){
      # while there is still extra money to tackle, use tackle = TRUE
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, i,
                                             timeline_results2, tackle = TRUE, firstRow = 1)
          print("I passed the new balance")
          print(new_calculations)
          #updated dataframe with new calculations
          timeline_results2 <- new_calculations$dataframe
          # residual from this debt goes to tackle the next debt
          tackle_money <- new_calculations$residual
          print(paste("timeline result2  for sequence while we still have money ", i))
          print(timeline_results2)
          print("tackle money is ")
          print(tackle_money)
      }
    }
    # if there are still un-updated, untackleable debts to calculate in first month
    if(i < nrow(debt_accounts)){
      # : is inclusive so add an index
      untouched = i + 1
      for(j in untouched:nrow(debt_accounts)){
        print(paste("I am outside the while loop on index ", j))
        # For all the other i debts, calculate new balance only through minimum
        new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, j,
                                                timeline_results2, tackle = FALSE, firstRow = 1)
        #updated dataframe with new calculations
        timeline_results2 <- new_calculations$dataframe
        print(paste("timeline result2 outside we still have money ", j))
        print(timeline_results2)
        print(paste(" I get to the end of the end of the for loop in loop ", j))
      }
    }
    #I would need a new iteration of the month to happen after this
    month_index <- month_index +1
    # since we updated month(finished a row), we have to refresh the tackle_money
    tackle_money <- disposable
    #Finished with the first row
    first_row <- FALSE

  }
  # while there are still balances on a loan (each iteration is a month)
  while(total_balance > 0){
    # create default columns
    timeline_results2[month_index, 'Month'] <- month_index
    timeline_results2[month_index, 'Disposable'] <- disposable
    #generate next row of original balances
    timeline_results2 <- calculateOriginalBalance(month_index, debt_accounts, timeline_results2, firstRow = FALSE )

    print("This is timeline result 2 after adding next month balances are !!!")
    print(timeline_results2)
    # Create function that if original balance is 0, everything should be zero automatically and could be skipped. Would need to maybe return the debt index as well

    # go through each loan and calculate its new balance this month
    for(x in 1:nrow(debt_accounts)){
      #while there is still disposable income
      while(tackle_money > 0){
          new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, x,
                                                  timeline_results2, tackle = TRUE, firstRow = 0)
          #updated dataframe with new calculations
          timeline_results2 <- new_calculations[1]
          # residual from this debt goes to tackle the next debt
          tackle_money <- new_calculations[2]
      }
      #for all other loans, do only minimums
      new_calculations <- calculateNewBalance(month_index, tackle_money, debt_accounts, x,
                                              timeline_results2, tackle = FALSE, firstRow = 0)
      #updated dataframe with new calculations
      timeline_results2 <- new_calculations[1]
    }
    #I would need a new iteration of the month to happen after this
    month_index <- month_index +1
    # since we updated month(finished a row), we have to refresh the tackle_money
    tackle_money <- disposable

    # TODO: calculate the total balance from timeline results tail and the new balance columns
    total_balance <- 0
    print(paste("I will end here on the first row for everything changing balance to ", total_balance))
  }

}

createTimeline <- function(debt_accounts, disposable_income) {

          createSuffixColumns <- function(titles, suffix){
            # In this one I am storing all of them in the vector column_name so the whole result is on one vector not a vector with multiple vectors inside (if you were to return)
            column_name <- paste(titles, suffix, sep = "_")
          }

          createCorrespondingColumns <- function(debt_title){
            debt_columns <- paste0(debt_title, c("Original_Balance", "Interest_Added", "New_Balance"), sep = "_")

          }

          update_cell <- function(input, result_data, month, new_balance) {
            switch(TRUE,
                   grepl("New_Balance$", input) ~ {
                     result_data[month, input] <- new_balance
                     # Additional apple processing code here
                   },
                   grepl("Added_Interest$", input) ~ {
                     result_data[month, input] <- new_balance
                     # Additional apple processing code here
                   },
                   TRUE ~ {
                     print("Unknown input")
                     # Code to handle unknown input
                   }
            )
          }
          #extremely similar but one takes one more variable disposable
          payingMinimum <- function(month, result_data, column,previous_balance, loan_info){
            monthly_apr <- (loan_info$APR/100) /12.0
            temp_balance <- previous_balance - loan_info$minimum
            interest <- temp_balance * monthly_apr
            new_balance <- temp_balance + interest
            update_cell(column, result_data, month, new_balance)

            # switch (column,
            #         endsWith("New_Balance"), result_data[month, column] <- new_balance,
            #         endsWith("Added_Interest"), result_data[month, column] <- interest
            #
            # )
          }

          #Replaces the added interest and new balance column for a certain month (row) for a certain loan info
          payingDownBalance <- function(month, result_data, column, previous_balance, disposable, minimum, apr) {
            monthly_apr <- (apr/100) /12.0
            # before monthly interest added
            temp_balance <- previous_balance - minimum - disposable
            #calculating this months interest added
            interest <- temp_balance * monthly_apr
            new_balance <- temp_balance + interest
            print("column is")
            print(column)
            switch (column,
              endsWith("New_Balance"), result_data[month, column] <- new_balance,
              endsWith("Added_Interest"),  result_data[month, column] <- interest

            )

            #returns if there is residual money the amount (0 if all used up)

          }

          min_clear <- 0
          disposable <- tail(disposable_income, n = 1)$amount
          month_index <- 1
          debt_index <- 1
          total_balance <- 1
          first_row <- TRUE

          default_columns <- c("Month", "Disposable")

          interest_columns <- createSuffixColumns(debt_accounts$title, "Added_Interest")
          og_balance_columns <- createSuffixColumns(debt_accounts$title, "Original_Balance")
          new_balance_columns <- createSuffixColumns(debt_accounts$title, "New_Balance")


          Debt_columns <- createCorrespondingColumns(debt_accounts$title)

          print("This is debt_columns")
          print(Debt_columns)
          print(unlist(Debt_columns))
          columnTitles <- c(default_columns, interest_columns, og_balance_columns, new_balance_columns)
          timeline_results <- data.frame(setNames(replicate(length(columnTitles), numeric()), columnTitles))

          # Do first row of months as a default to be able to use the previous_month from timeline_results


          # OMG I could do it by month and disposable income being used up per row. Do like while total balance is not 0
          ############# Whole different idea #2
          # while(total_balance != 0){
          #     money <- disposable + min_clear
          #     current_debt <- debt_accounts[debt_index, ]
          #
          #     #create the row of the current month
          #     timeline_results[month_index, "month"] <- month_index
          #
          #     if(month_index == 1){
          #       payingDownBalance(month_index, timeline_results, subset(columnTitles, select = starts_with(interest_columns, current_debt$title)),
          #                         timeline_results[month_index-1,], money, current_debt$minimum, current_debt$APR)
          #       print("The one paying down balance does")
          #       print(timeline_results)
          #
          #     }
          #     total_balance <- total_balance - 1
          #
              # if you can pay multiple balances this month (This below was already commented)
              # if(current_debt$balance < money ){
              #   payingdownbalance(month_index, timeline_results, startWith(interest_columns, current_debt$title),
              #                     timeline_results[month_index-1,], money, current_debt$minimum, current_debt$APR)
              # }else{# if this money only helps you put down the total principle
              #   payingdownbalance(month_index, timeline_results, startWith(interest_columns, current_debt$title),
              #                     timeline_results[month_index-1,], money, current_debt$minimum, current_debt$APR)
              # }

              # maybe instead of this if statement I ould have paying down return the residual and if there exists a residual keep going down on the debts
          #}

          ################################# whole different ideas #1
          # go through tackling every debt
          # for (debt in seq_len(nrow(debt_accounts))) { #looking at all the debts
          #
          #
          #   # if we wiped the debt already with the last iteration disposible income, skip to next debt
          #   if(debt_accounts[debt, ]$balance == 0){
          #     print("I get here sometime")
          #     next
          #   }
          #
          #   # Decide which debts could use the disposable income and which will only be paid minimum
          #   if(debt_accounts[debt, ]$balance < disposable_income){
          #     # starting values that will be wiped out this month
          #     largest <- debt_accounts[debt, ]$balance
          #     last_num <- debt
          #     # Find the index of the debt we cannot knock out with the current disposable income, make the og dataset get 0 balances (actually I think that changing the balance is bad)
          #     range <- lapply(debt_accounts[debt:nrow(debt_accounts)], function(index){
          #       largest <- largest + index$balance
          #       if(largest > disposable_income){
          #         return(last_num)
          #       }else{
          #         #change the value in the dataset to show that this index of debt has been wiped out
          #         debt_accounts[debt, ]$balance <- 0
          #         debt_accounts[last_num, ]$balance <- 0
          #         last_num <- last_num +1
          #       }
          #     })
          #
          #     debt_tackling <- debt_accounts[debt:range]
          #     debt_growing <- debt_accounts[range+1:nrow(debt_accounts),]
          #   }else{
          #     # separating vectors that we are tacking vs that being paid with minimums and growing interest
          #     debt_tackling <- debt_accounts[debt, ]
          #     debt_growing <- debt_accounts[debt+1:nrow(debt_accounts),]
          #   }
          #
          #   month_counter <- month_counter +1
          #   print(paste("This is debt tackling during month ", month_counter))
          #   print(debt_tackling)
          #   print(paste("This is debt growing during month ", month_counter))
          #   print(debt_growing)
          #   # While we are still dealing with the same debt
          #   # while(debt_accounts[debt, ]$balance > disposable_income){
          #   # }
          #   # If we are starting at the first mont
          #
          # }

}

decreaseBalance <- function(debt_title, remaining_balance, disposable_income, minimum, APR ){
  monthly_apr <- APR/12
  og_balance_title <- paste0(debt_title, "Original Balance")
  #og_data <- seq(from = remaining_balance, to = 0, by = -(disposable_income + minimum))

  #create dataframe
  constant <- c("Month", "Disposable")
  #created
  lapply()

  new_balance_title <- paste0(debt_title, "New Balance")
  new_paid_down <-  remaining_balance - (minimum + disposable_income)
  interest_added <-
  new_data <- seq(from = remaining_balance - (disposable_income + minimum) , to = 0, by = )

  title_col <- paste0(debt_title, each = nrow(column_data))

  #debt_title_df <-

}

create_timeline_df <- function(debt_accounts, disposable_income) {
  # Ensure debt_accounts is a data frame with the required columns
  if (!is.data.frame(debt_accounts) ||
      !all(c("title", "balance", "APR", "minimum") %in% colnames(debt_accounts))) {
    stop("debt_accounts should be a data frame with columns: title, balance, APR, minimum")
  }

  # Ensure disposable_income is a data frame with a column named "income"
  if (!is.data.frame(disposable_income) || !("amount" %in% colnames(disposable_income))) {
    stop("disposable_income should be a data frame with a column named 'income'")
  }

  # Get unique titles from debt_accounts
  titles <- unique(debt_accounts$title)

  # Create a sequence of months (assuming 12 months)
  months <- seq(from = Sys.Date(), by = "months", length.out = 12)

  # Function to calculate new balance based on the minimum payment
  calculate_new_balance <- function(balance, minimum) {
    balance - minimum
  }

  # Use lapply to iterate over unique titles
  timeline_list <- lapply(titles, function(title) {
    # Filter debt_accounts for the current title
    debt_subset <- subset(debt_accounts, title == title)

    # Iterate over each month
    new_balances <- Reduce(calculate_new_balance, debt_subset$balance, debt_subset$minimum, accumulate = TRUE)

    # Create a data frame for the current title
    data.frame(
      title = rep(title, length(months)),
      old_balance = rep(debt_subset$balance, each = length(months)),
      new_balance = new_balances
    )
  })

  # Combine the data frames from lapply into a single data frame
  timeline_df <- do.call(rbind, timeline_list)

  return(timeline_df)
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
              width = 8

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
    print(new_createTimeline(debt_info, dis_df))

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





# # Should be the same as the OG but with a cleaner switch have not tested
# calculateOriginalBalance2 <- function(month ,data, debt_index, results, firstRow) {
#   for( debt in debt_index:nrow(data)){
#     current <- data[debt,]
#     switch (firstRow,
#             1 = results[month, paste0(data[debt,]$title,"_Original_Balance")] <- current$balance,
#             0 = results[month, paste0(data[debt,]$title,"_Original_Balance")] <- results[month-1, paste0(data$title,"_New_Balance")]
#     )
#   }
#   return(results)
# }
