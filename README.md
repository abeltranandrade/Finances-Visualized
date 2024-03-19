# Finances Visualized

## About

[Finances Visualized]() is a Shiny web application designed for effortless visualization of finances and debt impact. The app features two tabs: one for budget creation and expense visualization, illustrating their impact on disposable income. In the second tab, a user can input their disposable income and each of their debt information to generate a comprehensive month-to-month plan outlining their contribution to each debt. The dashboard illustrates the benefits of prioritizing debt repayment over minimum payments, showcasing the long-term advantages of making sacrifices now. This is achieved by emphasizing key facts about debt that resonate universally: the potential for long-term interest savings, increased monthly disposable income for budgeting and savings goals, and the minimal impact on total balances when only making minimum payments. The month-to-month plan, accompanied by essential statistics, will be downloadable upon providing the necessary information. 

## Why Create This

Creating this tool was crucial to me because many people find numbers intimidating, and despite understanding the negative implications of debt, it's challenging to grasp the true extent of losses incurred by not taking action immediately.My tool achieves this by contrasting the outcomes of aggressive debt repayment with only making minimum payments, while also offering a specific plan detailing the monthly target for each debt repayment. You can utilize this tool to determine the additional monthly income required to achieve debt payoff by a specific target month by changing the disposible income. 

## Definitions

*disposable income* - Disposable income is the money you have left over after paying for all your necessary expenses, like housing, food, and bills. It's what you can spend or save as you please.

*avalanche technique* -  a strategy where you prioritize paying off debts with the highest interest rates first, while making minimum payments on all other debts. This approach aims to minimize the total interest paid over time, potentially helping you become debt-free faster and save money in the long run.

*snowball technique* - The snowball technique for debt is a strategy where you prioritize paying off debts with the smallest balances first, while making minimum payments on all other debts. As each debt is paid off, the freed-up funds are then directed towards tackling the next smallest debt. This approach aims to build momentum and motivation by achieving smaller victories early on, potentially helping you gain confidence and momentum to tackle larger debts later.

## How to Access

Finances Visualized dashboard can be accessed in 2 ways. You can use the hosted dashboard online, or downloading the code and run it through your terminal or in RStudio. 

The link for the hosted dashboard is: 

### How to Install Using RStudio:

1. Clone the Repository and open as a project in RStudio
  - Click green "<>code" button at the top of this repository and copy the HTTP link
  - Open RStudio
  - Go to File > New Project > Version Control > Git 
  - Paste the HTTP link and store in file path of choice on your machine
2. Open the app.R file in RStudio and Run the App 
  - Open app.R on the bottom right hand side of Rstudio on the tab Files
  - ensure all dependencies are installed(see dependencies below)
  - Click on "Run App" Button at top right side of the app.R window
  - A new pop up window will appear, use dashboard there or click on the "Open on Browser" button on top left of the pop-up
3. Use the dashboard :)

### How to Install Using the Command Line:

1. Have a version of R on your computer
  - if you do not, go __________ to download
2. Clone this github repository onto your desired file path
3. Navigate to the directory where your Shiny app's files are located using the cd command on a terminal
4. Once you're in the correct directory, start an R session by typing R and pressing Enter.
5. Install the necessary shiny libraries 
  - use command install.packages("package") for each package dependency listed in the dependency list
  - This command will use CRAN the Comprehensive R Archive Network (CRAN). Follow any prompts or confirmations if necessary.
6. Run the Shiny app
  - Execute the shiny app by using this code with your machine's specific path to the app.R directory shiny::runApp("path") 
  - Press Enter, and the Shiny app will start running. You should see output in the terminal indicating that the app is running, along with a URL       you can visit in your web browser to view the app.
  - if you want to quit the app, use Ctrl + C to interrupt the processes
  

## Dependencies

``` r
to install packages
install.packages(c("shiny", "shinydashboard", "plotly", "DT", "dplyr"))

#to load packages (this should be done automatically by app.R, but if given error try to rerun on terminal)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
```

The dependencies used for this app are shiny and shinydashboard, for web app functionality. Packages plotly and DT are to make interactive visualizations and nicely formatted tables. The package dplyr is a helpful and efficient data wrangling package. We also use profr and profviz for data profiling and optimization but you will not need to run those to use the app.

## Dashboard Functionality

| Feature        | Location           | Function |
| ------- |:-------:| -------------:|
| Disposable Income Input Unit | Timeline | The amount the app will use to simulate the user paying off their debts. It will always use the last number inputed|
| Debt Input Unit   | Timeline  | Input fields where users can enter information for each debt they owe |
| Process Debts button |  Timeline  | Will trigger both paying off and minimum payment simulations and populate all reactives that fuel the dashboard. See UML diagram for more details on the reactives |
| Month Slider |  Timeline after Debt Submissions | An interactive widget enabling users to navigate month-by-month, accessing all dashboard information and visualizations up to the selected month. |
| Total Debt visualization | Timeline     |  Interactive bar graph displaying the comparison between total debt owed when making minimum payments versus paying off debts more aggressively, up to the value set by the month slider |
| Total Interest Accrued visualization | Timeline | An interactive bar graph compares the total interest paid when making minimum payments versus paying off debts more aggressively. The comparison is shown up to the value set by the month slider. |
| Debt payment category tables | Timeline    | Each month, the user's debts will be categorized into three tables. These tables indicate whether additional payments should be made, minimum payments suffice, or if the debt has been completely paid off by that month |
| Download PDF | Timeline after Debt Submissions |  User can download a pdf containing interest saved information and their month-to-month plan for each debt |
| Reset Button | Timeline after Debt Submissions | resets the dashboard so the user can readd a new scenario |
| Statistic value boxes | Timeline  | Value boxes providing users with key summarized information per month, including their disposable income, total debt, saved interest, and eliminated minimum payments |


## Data Sources and Assumptions

All data comes from the user itself, from their disposible income to the debt information they submit. 

Given the uniqueness of ways each credit card or loan recalculates its minimum payment each month, I assumed paying the same minimum payment each month. As long as users do not make more purchases on those cards, as they pay the debt the minimum payments will be lower and lower, making the simulation a slight over estimation of the time it could take. 

In the current code, we also assume the user inputs the debts in a intentional order, deciding if they want to do the avalanche or snowball techniques. 

## Repo Architecture

Where can you find the code/functions, documentation, profiling efficiency tests, put in here any UML diagrams I can complete. 

## License

[MIT License](https://www.tldrlegal.com/license/mit-license?ref=fossa.com#fulltext). Copyright (c) 2023 CitationProfileR authors.

## How to Provide Feedback

Questions, bug reports, and feature requests can be submitted to this repo's [issue queue](https://github.com/LukasWallrich/citationProfileR).


## Have Questions?

Contact us at l.wallrich@bbk.ac.uk or lmikhelashvili@smith.edu.
