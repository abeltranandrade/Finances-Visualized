# Load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)
library(dplyr)
library(tidyr)
library(profr)
library(profvis)
#' Create Input Unit
#' @description This app needs to query different type of information to create its tools such as income, expenses, disposable income, debts etc they are all different data but all need a header title and a certain number of input fields of certain types. This function generalizes those "units". See wireframe for visual example.  Function to create input fields that contain a header, paragraph, and variable number of input elements.
#'
#' Required Parameters:
#' @param header General title for all the information needed in this unit
#' @param button_label label on the submission button. Note this should be the second to last parameter
#' @param button_id ID to call  the submitted information in this input field/s. Note this should be the last parameter
#'
#' @return creates and displays the input units components wanted in the UI of shiny
createInputUnit <- function(header, ..., button_label, button_id) {
  fluidRow(h3(header),
            p(paste("Enter your", tolower(header), "details:")),
            lapply(list(...), function(input_info) {
             switch(input_info$type,
                    numeric = numericInput(input_info$id, label = input_info$label, value = 0, min = 0),
                    text = textInput(input_info$id, label = input_info$label, value = "")
              )
            }),
         actionButton(button_id, button_label)
  )
}

updateInputUnit <- function(session, ...) {
           lapply(list(...), function(input_info) {
             switch(input_info$type,
                    numeric = updateNumericInput(session,input_info$id, value = 0),
                    text = updateTextInput(session,input_info$id, value = "")
             )
           })
}

#' Title
#' @description Where you create how the value boxes look and where they display information. It can be expanded later to become more general and have different templates.
#'
#' @param title information detailing what the number above represents
#' @param value The variable value we want to display to the user about their debt data throughout the months
#'
#' @return
#' @export
#'
#' @examples
createValueBox <- function(title, value ) {
  #valueBox(value, subtitle = title, color = "light-blue")
  valueBox(
    paste0("$", value),
    title
  )
}

#' Title
#'
#' @param id_titles a vector containing the chosen names for each value box unit you want to create in the ui
#'
#' @return create x amount of value boxes to the ui
#' @export
#'
#' @examples
multipleValueBoxes <- function(id_titles){
  fluidRow(
    lapply(id_titles, function(box_title){
      valueBoxOutput(box_title, width = 3)
    })
  )
}

createTitleSection <- function(heading_text, background_color, box_content, font_color, padding, box_height, margin_bottom = "20px") {
       fluidRow(
         div(style = paste0("background-color: ", background_color, "; color: ", font_color, "; padding: ", padding, "; height: ", box_height, "; margin-bottom: ", margin_bottom, ";"),
                    h3(heading_text),
                    h4(box_content)
                )
       )

}

checkRequiredColumns <- function(functionName, dataset, required_names) {
  if(!all(required_names %in% names(dataset))){
    functionId <- paste("In the function ", functionName)
    stop(paste(functionId, ", the dataset passed does not have one of the columns required"))
  }
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

reset_values <- function() {
  disposable(data.frame(amount = 0))
  debts(data.frame(title = character(), balance = numeric(), APR = numeric(), minimum = numeric()))
  timeline(data.frame(month = numeric(), title = character(), original_balance = numeric(), added_interest = numeric(), new_balance = numeric(), extra = numeric(), minimum = numeric(), added_interest_min = numeric(), new_balance_min = numeric()))
  timeline_disposable(data.frame(month = numeric(), disposable = numeric()))
  min_simulation_saved(data.frame(title = character(), saved_interest = numeric(), saved_months = numeric()))
}

#' Title Find Previous Balance
#' @description Used to retreive a certain debt's previous month value. Typically used for the balance but made allow any column retrival
#'
#' @param month_index Current or consequent month for which we want to get the previous balance for
#' @param title The title of the debt we want to retrieve information for
#' @param result_df Data frame where we can find the data with all previous month information stored
#' @param column_retrival Name of column we want to retrieve from
#'
#' @return Generally it is the previous value of the requested debt from the requested column. This will be usually the previous month balance.
#' @export
#'
#' @examples
findPreviousBalance <- function(month_index, title, result_df, column_retrival){
  checkRequiredColumns("findPreviousBalance", result_df, list("month", "title", column_retrival))
  #subset the row from the previous month with a certain title
  subset_previous_entry <- result_df[result_df$month == month_index-1 & result_df$title == title , ]
  # assuming ID record returns just 1 bc there will always be only 1 entry for each debt for each month
  # balance will be the new balance of the located record
  balance <- subset_previous_entry[ ,column_retrival]
  return(balance)
}

#' Title Calculate Paid Balance
#'
#' @description Calculates the updated new balance for a certain debt after monthly payment depending if we will use extra money to tackle the debt or if we will just pay a minimum payment
#'
#' @param tackle boolean value that signals if this debt will use disposible income to tackle the debt faster
#' @param original_balance starting balance for the debt in this month
#' @param disposable_income amount of disposible income available to this debt
#' @param minimum The minimum payment that must be paid to this card
#'
#' @return a data frame with columns: balance, the updated debt balance / residual: amount of disposable income left over after this debt / debtWiped: boolean value to know if debt has been completely zeroed
#' @export
#'
#' @examples
calculatePaidBalance <- function(tackle, original_balance, disposable_income, minimum) {
  #if we will pay more than minimum
  if(tackle == TRUE){
    paidDown <- original_balance - minimum
    #If you payoff the current balance with the minimum payment, you did not use the disposible income so return it as residual (In this case, we would not consider the remainder of the minimum payment we freed into money we can use to pay more debts, I feel its going to confuse the user even more seeing inconsistencies like that. We will use it the following month anyway and its not exactly perfect)
    if(paidDown <= 0){
      return(data.frame(balance = 0, residual = disposable_income, debtWiped = TRUE))
    }
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
    if(paidDown <= 0){
      return(data.frame(balance = 0, residual = 0, debtWiped = TRUE))
    }else{
      return(data.frame(balance = paidDown, residual = 0, debtWiped = FALSE))
    }

  }
}

#' Title Calculate Interest
#'@description Calculates the interest added to the balance given the debt ending balance and its calculated monthly rate.
#'
#' @param current_balance current ending balance for the specific debt for the month
#' @param APR Yearly rate of interest
#'
#' @return amount of interest that would be added to the total amount of debt for this month
#' @export
#'
#' @examples
calculateInterest <- function(current_balance, APR){
  monthly_apr <- (APR/100.0 ) /12.0
  return(current_balance*monthly_apr)
}

#' Title Calculate the Total Balance
#'
#' @description Calculates the total balance owed from all different debts
#'
#' @param current_month month we want to calculate the total debt balance
#' @param result_df Data frame containing all month's debt balance informations
#'
#' @return the amount of total debt balance owed throughout all debt accounts
#' @export
#'
#' @examples
calculateTotalBalance <- function(current_month, result_df){
  checkRequiredColumns("calculateTotalBalance", result_df, list("month", "new_balance"))
  #subset all the rows where month equals current month
  month_subset <- result_df[result_df$month == current_month, ]
  #return the sum of the month's new balance column
  return(sum(month_subset$new_balance))
}

#' Title Simulate Progress
#'@description Simulates the payoff journey of all the debts submitted through the UI if they were to use their disposable money each month to implement the snowball method
#'
#' @param debt_df Data frame containing all initial debt information such as debt title, balance, APR and minimum
#' @param disposable_df Data frame that contains the disposible income information the user will have available after their budgetted needs
#'
#' @return A data frame with a row for each debt's progress per month. With columns ....
#' @export
#'
#' @examples
simulateProgress <- function(debt_df, disposable_df) {
  #validate the columns we need are in the parametered data frames
  checkRequiredColumns("SimulateProgress-Debt_df", debt_df, list( "title", "balance","APR", "minimum"))
  checkRequiredColumns("SimulateProgress-Disposable_df", disposable_df, list( "amount"))

  result_df <- data.frame(
    month = numeric(),          # Month in the progress
    title = character(),        # Debt ID title
    original_balance = numeric(),  # Balance at the start of the month
    added_interest = numeric(),    # Interest added at the end of month balance
    new_balance = numeric(),       # End of month balance with interest added
    extra = numeric()
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
          new_balance = 0,
          extra = 0)
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
        added_interest = round(interest, 2),
        new_balance = round(dec_balance + interest,2),
        extra = round(total_disposable - decreasedBalance$residual,2))

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


#' No Change Simulation
#'
#' @description Creates a simulation of just paying minimum payments for the debt information given in the amount of month's they could be debt free using disposible income
#'
#' @param debt_df data frame containing all basic debt information collected from the user. (title, balance, APR, minimum payment)
#'
#' @return a data frame with a row for each debt's progress per month for total_months amount of months.
#' @export
#'
#' @examples
noChangeSimulation <- function(debt_df){
  result_df <- data.frame(
    month = numeric(),          # Month in the progress
    title = character(),        # Debt ID title
    added_interest_min = numeric(),    # Interest added at the end of month balance
    new_balance = numeric()       # End of month balance with interest added
  )

  total_balance <- sum(debt_df$balance)
  month_index <- 0

  while(total_balance > 0) {
    #start a new month
    month_index <- month_index + 1
    #create a row for each debt
    for(debt in 1:nrow(debt_df)){
      # Edge case to find the initial month's debt balance
      if(month_index == 1){current_balance <- debt_df[debt,]$balance}
      else{current_balance <- findPreviousBalance(month_index, debt_df[debt,]$title, result_df, "new_balance")}

      #this debt has already been wiped fill in the row with zeroes and next
      if(current_balance == 0){
        temp_row <- data.frame(
          month = month_index,
          title = debt_df[debt,]$title,
          added_interest_min = 0,
          new_balance = 0)
        result_df <- rbind(result_df, temp_row)
        next
      }

      #calculate decreasing this debt original balance for this month without disposable income
      decreasedBalance <- calculatePaidBalance(tackle = FALSE, current_balance, 0, debt_df[debt,]$minimum)

      #calculate interest using balance after og balance paid down
      dec_balance <- decreasedBalance$balance
      interest <- calculateInterest(dec_balance, debt_df[debt,]$APR)

      #create new row and bind to results
      temp_row <- data.frame(
        month = month_index,
        title = debt_df[debt,]$title,
        added_interest_min = round(interest,2),
        new_balance = round(dec_balance + interest,2))
      result_df <- rbind(result_df, temp_row)
    }
    #calculate the total debt balance left after this month
    total_balance <- calculateTotalBalance(month_index, result_df)
  }
  #change the name of the column to be able to join it with other simulation. Cannot do it to start because it breaks calculateTotalBalance(), this is easier
  colnames(result_df)[colnames(result_df) == "new_balance"] <- "new_balance_min"
  return(result_df)
}

# Function to generate colored rows based on month value (FOR LATER)
color_rows <- function(x) {
  ifelse(x %% 2 == 0, "background-color: white", "background-color: blue")
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
              conditionalPanel(
                condition = "input.process_debts > 0",
                sliderInput("Timeline", "Move Through The Months", min = 0, max = 20, value = 0),
                actionButton("reset_button", "Reset The Plan")
              ),
              conditionalPanel(#(FOR LATER)
                condition = "input.process_debts > 0",
                downloadButton("downloadPDF", "Download PDF")
              )
            ),
            column(
              width = 8,
              multipleValueBoxes(c("DispoBox", "TotalBox", "IntSavedBox", "MinimumFreedBox")),
              fluidRow(plotlyOutput("total_balance_bar")),
              createTitleSection("Debts Being Focused", "#FFFF33", "These debts should be paid off rapidly using their original minimum payment and disposible income ", "black", box_height = "100px", padding = "3px", margin_bottom = "20px"),
              fluidRow(DTOutput("focus_debt_df")),
              createTitleSection("Debts on Minimum Payments ", "#DC143C", "Pay the minimum payments on these debts and focus all your extra money on the ones above this month ", "white", box_height = "100px", padding = "3px", margin_bottom = "20px"),
              fluidRow(DTOutput("minimum_debt_df")),
              createTitleSection("Paid Off Debts! ", "#238823", "Congratulations! Now see how much money you saved on interest :) ", "white", box_height = "100px", padding = "3px", margin_bottom = "20px"),
              fluidRow(DTOutput("paid_debt_df")),
              fluidRow(plotlyOutput("cummulative_interest_bar"))
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
server <- function(input, output, session) {

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
  timeline <- reactiveVal(data.frame(month = numeric() ,title = character(), original_balance = numeric(), added_interest = numeric(),new_balance = numeric(), extra = numeric(), minimum = numeric(),added_interest_min = numeric(), new_balance_min = numeric()))
  timeline_disposable <-reactiveVal(data.frame(month = numeric(),disposable = numeric()))
  min_simulation_saved <- reactiveVal(data.frame(title = character(), saved_interest = numeric(), saved_months = numeric()))
  #storing given disposible income into disposable() reactive
  observeEvent(input$disposable_submit, {
    #format observed event result into our income format and bind it
    new_disposable <- data.frame(amount = input$disposable_income)
    disposable(rbind(disposable(), new_disposable))
  })

  # storing individual debt information submitted by user into debts() reactive
  observeEvent(input$debt_submit, {
    #verify each debt name is unique and minimum payment is not 0
    debt_exists <- any(debts()$title == input$debt_title)
    if (debt_exists) {showNotification("Please enter a unique name for this debt. Please resubmit the infomation", type = "error")}
    else if(input$monthly_min <= 0){showNotification("Please enter a minimum payment larger than zero. Please resubmit the information", type = "error")}
    else{
    #format observed event result into our expense format and bind it, then clear the input boxes
    new_debt <- data.frame(title= input$debt_title, balance = input$remaining_balance, APR = input$apr, minimum = input$monthly_min)
    debts(rbind(debts(), new_debt))
    updateInputUnit(session,
                    list(label = "Debt Name", id = "debt_title", type = "text"),
                    list(label = "Total Remaining Balance", id = "remaining_balance", type = "numeric"),
                    list(label = "APR", id = "apr", type = "numeric"),
                    list(label = "Monthly Minimum", id = "monthly_min", type = "numeric"))
    }
  })

  #actives the simulations given the information collected from the other above events
  observeEvent(input$process_debts, {
    dis_df <- disposable()
    debt_info <- debts()

    #run payoff simulation
     #simulation_prof <- profvis({
      simulation <- simulateProgress(debt_info,dis_df)
    #})
     #print(simulation_prof)
    #simulating paying minimum only
    min_only_all_months <- noChangeSimulation(debt_info)

    #populate reactive with monthly_disposable info given from simulation
    timeline_disposable(rbind(timeline_disposable(), simulation$monthly_disposable))

    #Creating a merged data frame that has both the disposible income simulation data and the minimum payment simulation for x amount of months and store in timeline
    simulation$timeline <- simulation$timeline %>%
      left_join(debts() %>% select(title, minimum), by = "title")

    min_only_process_months <- min_only_all_months[min_only_all_months$month <= max(simulation$timeline$month), ]
    all_together <- merge(simulation$timeline, min_only_process_months, by = c("month", "title"))
    timeline(rbind(timeline(),all_together))

    # subset of the minimum only simulation that goes beyond our projected disposable income simulation to find interest we saved and projected month each debt will be paid with only minimum
    subset_data <- min_only_all_months[min_only_all_months$month > max(timeline()$month), ]
    new_data <- subset_data %>%
      group_by(title) %>%
      summarise(interest_saved = sum(added_interest_min), #use mutate rather than summarize if you still want the other columns but you won't given the UML now
             month_ended = min(month[new_balance_min == 0])) # finds the minimum value in the month column where the row has new_balance_min =0
    min_simulation_saved(rbind(min_simulation_saved(), tibble(new_data)))

    #slider will max out at the maximum amount of months of our simulation
    updateSliderInput(session, "Timeline", max = max(timeline()$month))
  })

  descriptions <- c("Disposable Income", "Total Debt Balance", "Interest Saved", "Minimum Freed")
  box_titles <- c("DispoBox", "TotalBox", "IntSavedBox", "MinimumFreedBox")

  observeEvent(input$Timeline,{

    #value boxes section start
    # transformation to get disposable value box for a certain month. Find the disposable for this month
    disposable_value <- timeline_disposable() %>%
      filter(month == input$Timeline)

    current_month_debt_info <- timeline() %>%
      filter(month == input$Timeline)

    #(valueBox) Find total debt balance for x month on the slider
    total_debt_balance <- sum(current_month_debt_info$new_balance)

    #Find rows of debts this month where full balance has been paid
    paid_debts_rows <- current_month_debt_info %>%
      filter(new_balance == 0)

    #(valueBox) Sum all the minimum payments of debts that have a balance of 0 this month
    minimum_wiped_sum <- sum(paid_debts_rows$minimum)

    # filter all months up to the x slider value and sum its interest while tackling simulation and the interest while paying minimum payments(interest_saved_progress)
    months_progressed <- timeline() %>%
      filter(month <= input$Timeline)

    remaining_interest_rows <- paid_debts_rows %>%
      mutate(interest_saved = ifelse(title %in% min_simulation_saved()$title,     #did 2 separate mutates to avoid the amount of times I would do this if statement/matching
                                     min_simulation_saved()$interest_saved[match(title, min_simulation_saved()$title)],
                                     NA_real_))

    #Calculate the interest saved, both while progressing and without all the extra months of interest
    interest_saved_progress <- sum(months_progressed$added_interest_min) - sum(months_progressed$added_interest)
    interest_saved_after_payoff <- sum(remaining_interest_rows$interest_saved)
    total_interest_saved <- interest_saved_progress + interest_saved_after_payoff

    updated_values <- c(disposable_value$disposable, total_debt_balance, total_interest_saved , minimum_wiped_sum )
    # Updates ValueBoxes given month information just calculated
      lapply(1:length(box_titles), function(i) {
        output[[box_titles[i]]] <- renderValueBox({createValueBox(descriptions[i], updated_values[i])})})
    #value boxes section end

    # Create a dataframe for the paid table displayed
    focus_df <- timeline() %>%
      filter(month  == input$Timeline & extra > 0) %>%
      select(title, original_balance, extra, minimum, new_balance)

    # Create a dataframe for the paid table displayed
      minimum_df <- timeline() %>%
        filter(month  == input$Timeline & extra <= 0 & new_balance != 0) %>%
        select(title, original_balance, minimum, added_interest , new_balance)

      # Create a dataframe for the paid table displayed
      paid_df <- timeline() %>%
        filter(new_balance == 0 ) %>% #cannot filter by current month because I want to find the month it was paid off
        group_by(title) %>%
        mutate(month_paid = min(month)) %>%
        ungroup() %>%
        filter(month == input$Timeline) %>%
        group_by(title) %>%
        mutate(total_interest_saved = ifelse(title %in% min_simulation_saved()$title,     #did 2 separate mutates to avoid the amount of times I would do this if statement/matching
                                       min_simulation_saved()$interest_saved[match(title, min_simulation_saved()$title)],
                                       NA_real_),
               months_saved = ifelse(title %in% min_simulation_saved()$title,
                                    min_simulation_saved()$month_ended[match(title, min_simulation_saved()$title)],
                                    NA_real_) - month_paid) %>%
        ungroup() %>%
        select(title, month_paid, months_saved, total_interest_saved)

    output[["focus_debt_df"]] <- renderDT({datatable(focus_df, options = list(pageLength = 5),
                                                     colnames = c("Debt Title", "Month Starting Balance", "Disposable Income Towards Debt","Minimum Payment", "Month New Balance")) %>%
                                                    formatCurrency(columns = c(2:5), currency = "$", interval = 3) })

    output[["minimum_debt_df"]] <- renderDT({datatable(minimum_df, options = list(pageLength = 5),
                                          colnames = c("Debt Title", "Month Starting Balance","Minimum Payment","Interest Added This Month", "Month New Balance")) %>%
                                          formatCurrency(columns = c(2:5), currency = "$", interval = 3)})

    output[["paid_debt_df"]] <- renderDT({datatable(paid_df, options = list(pageLength = 5),
                                          colnames = c("Debt Title","Month Debt was Paid Off","Month's Saved By Paying Off Quicker", "Interest Saved By Not Carrying a Balance Anymore")) %>%
                                          formatCurrency(columns = c(4), currency = "$", interval = 3) })

    # Bar Graph of how much interest you'd pay a month
    output[["interest_bar"]] <- renderPlotly({
      plot_ly(months_progressed, x = ~month) %>%
        add_trace(y = ~added_interest, name = "Tackling Debt Added Interest", type = "bar") %>%
        add_trace(y = ~added_interest_min, name = "Paying Minimum Added Interest", type = "bar") %>%
        layout(barmode = "group",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Added Interest"),
               title = "Added Interest by Month")
    })

    total_added_interest <-  months_progressed %>%
      arrange(month) %>%      #ensure month's are in a correct order for cumulative sum
      mutate(cumulative_added_interest = cumsum(added_interest), #calculate the commulative sum by month
             cumulative_minimum_added_interest = cumsum(added_interest_min)) %>%
      group_by(month) %>% # adding this because if not, it stacks bars for each debt and creates different tooltips. This way Im getting the max cumulative value for the month
      summarise(max_cumulative_added_interest = max(round(cumulative_added_interest,2)),
                max_cumulative_minimum_added_interest = max(round(cumulative_minimum_added_interest,2)))

    interest_hover_text <- paste("Month: ", total_added_interest$month, "<br>",
                                 "Total Added Interest: $",format(total_added_interest$max_cumulative_added_interest, big.mark = ",", nsmall = 2),"<br>",
                                 "Difference: $", format(round(total_added_interest$max_cumulative_added_interest - total_added_interest$max_cumulative_minimum_added_interest, 2), big.mark = ",", nsmall = 2),"<br>")

    min_interest_hover_text <- paste("Month: ", total_added_interest$month, "<br>",
                                 "Total Added Interest: $",format(total_added_interest$max_cumulative_minimum_added_interest, big.mark = ",", nsmall = 2),"<br>",
                                 "Difference: $",format(round(total_added_interest$max_cumulative_minimum_added_interest - total_added_interest$max_cumulative_added_interest, 2), big.mark = ",", nsmall = 2),"<br>")

    #bar graph of total interest that will acrrue throughout the payment journey faceted by if you paid extra or just minimum payments
    output[["cummulative_interest_bar"]] <- renderPlotly({
      plot_ly(total_added_interest, x = ~month) %>%
        add_trace(y = ~max_cumulative_added_interest, name = "Paying Extra", type = "bar", marker = list(color = "#8fc44f", line = list(color = 'black', width = 1)),
                  text = interest_hover_text, hoverinfo = "text", hoverlabel = list(font = list(size = 12))) %>%
        add_trace(y = ~max_cumulative_minimum_added_interest, name = "Paying Minimum", type = "bar", marker = list(color = "#C44F8F", line = list(color = 'black', width = 1)), # , yellow : ffff5c ,  lime green: #8fc44f, green: 4fc484 pinkish #C44F8F
                  text = min_interest_hover_text, hoverinfo = "text", hoverlabel = list(font = list(size = 12))) %>%
        layout(barmode = "group",
               xaxis = list(title = "Month", rangemode = "nonnegative"),
               yaxis = list(title = "Total Cumulative Added Interest", rangemode = "nonnegative"),
               title = "Total Interest Accumulation: Extra vs Minimum Payments")
    })

    balance_data <- months_progressed %>%
      group_by(month) %>%
      summarise(total_balance = sum(new_balance),
                total_balance_min = sum(new_balance_min))

    balance_hover_text <- paste("Month: ", balance_data$month, "<br>",
                                 "Total Balance: $", format(balance_data$total_balance,big.mark = ",",nsmall = 2),"<br>",
                                 "Difference: $", format(round(balance_data$total_balance - balance_data$total_balance_min,2), big.mark = ",", nsmall = 2) ,"<br>")

    min_balance_hover_text <- paste("Month: ", balance_data$month, "<br>",
                                "Total Balance Paying Minimum: $", format(balance_data$total_balance_min,big.mark = ",",nsmall = 2),"<br>",
                                "Difference: $", format(round(balance_data$total_balance_min - balance_data$total_balance, 2),big.mark = ",", nsmall = 2),"<br>")

    #bar graph of total interest that will acrrue throughout the payment journey faceted by if you paid extra or just minimum payments
    output[["total_balance_bar"]] <- renderPlotly({
      plot_ly(balance_data, x = ~month) %>%
        add_trace(y = ~total_balance, name = "Paying Extra", type = "bar", marker = list(color = "#8fc44f", line = list(color = 'black', width = 1)),
                  text = balance_hover_text, hoverinfo = "text", hoverlabel = list(font = list(size = 12))) %>%
        add_trace(y = ~total_balance_min, name = "Paying Minimum", type = "bar", marker = list(color = "#C44F8F", line = list(color = 'black', width = 1)),   #red d62451
                  text = min_balance_hover_text, hoverinfo = "text", hoverlabel = list(font = list(size = 12))) %>%
        layout(barmode = "group",
               xaxis = list(title = "Month",rangemode = "nonnegative"),
               yaxis = list(title = "Total Debt Balance", rangemode = "nonnegative"),
               title = "Total Debt Balance: Extra vs Minimum Payments")
    })
  })

  observeEvent(input$reset_button, {
    reset_values()
  })


}
# Run the application
shinyApp(ui, server)
