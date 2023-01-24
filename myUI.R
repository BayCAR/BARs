#' The postP.Bfun function
#'
#' @import stats
#'
#' @import utils
#'
#' @import shiny
#'
#' @import DT
#
#' @import redcapAPI
#'
#' @import REDCapR
#'
#' @examples
#'
#' myUI()
#'
#' @export

myUI= function()
{  fluidPage(
    titlePanel("Bayesian Covariate-Adaptive Randomization (BayCAR)!"),
    sidebarLayout(
      # Sidebar with a slider input
      sidebarPanel
      ( column(6, fileInput("file2",  ("New subject file"), accept = ".csv"),
               checkboxInput("header2", "Header", TRUE)),
        column(6, fileInput("excludedid",  ("Excluded subject file"), accept = ".csv"),
               checkboxInput("header3", "Header", TRUE)),
        column(6, selectInput("arms", "Treatment Arms",
                              c("2 arms", "3 arms", "4 arms", "5 arms", "6 arms", "7 arms", "8 arms", "9 arms"),selected ="2 arms")),
        column(6, selectInput("trial", "Trial type",
                              c("multi-site", "single-site"),selected ="multi-site")),
        column(6, numericInput("planned.sample.size", "planned.sample.size", 20)),
        column(12, selectInput("Continuous", "Continuous covariates",
                               choices = c("age", "NA", "sex"),selected = "NA", selectize = TRUE, multiple = TRUE))
      ),
      # main panel
      mainPanel(
        column(12,
               h4("Click the 'Show' button to confirm the input values for the new data.
                  \n Do not click the 'Action' button unless all the values have been confirmed."),
               actionButton("show", "Show")),
        column(12, dataTableOutput("tablenew")),
        column(12, h3(uiOutput("noID"))),
        column(12, h3(uiOutput("wrong.ran"))),
        column(12, h3(uiOutput("missing.ran"))),
        column(12, selectInput("display", "Display diagnostic tables? (Please always try to do so.)", c("No", "Yes"), "Yes")),
        column(12, h4("Click on the 'Action' button to run! The group assignments for the new subject(s) will be shown in a table with a red background (underneath the optional blue diagnosis tables) below. Othersise, there will be a warning message in red. Also, make sure to click on the download icon to backup."),
               actionButton("action", "Action")),
        br(),
        column(6, align="center", dataTableOutput('table1')),
        column(6, align="center", dataTableOutput('table2')),
        column(6, align="center", dataTableOutput('table3')),
        br(),
        column(6, align="center", dataTableOutput('table4')),
        br(),
        column(6, align="center", dataTableOutput('table5')),
        column(6, align="center", dataTableOutput('table6')),
        column(6, align="center", dataTableOutput('table7')),
        column(6, align="center", dataTableOutput('table8')),
        column(6, align="center", dataTableOutput('table9')),
        column(6, align="center", dataTableOutput('table10')),
        column(6, align="center", dataTableOutput('table11')),
        column(6, align="center", dataTableOutput('table12')),
        column(6, align="center", dataTableOutput('table13')),
        column(6, align="center", dataTableOutput('table14')),
        column(6, align="center", dataTableOutput('table15')),
        column(6, align="center", dataTableOutput('table16')),
        column(6, align="center", dataTableOutput('table17')),
        column(6, align="center", dataTableOutput('table18')),
        column(6, align="center", dataTableOutput('table19')),
        column(6, align="center", dataTableOutput('table20')),
        br(),
        column(12, align="center", dataTableOutput('table99')),
        column(12, textOutput('display' )),
        column(12, h3(textOutput(' '))),
        column(12, h3(uiOutput('WNOnewlyRandomized'))),
        br(),
        column(12, align="center", dataTableOutput('table98')),
        downloadButton("downloadData", "Download Data")

        )
      )
  )
}
