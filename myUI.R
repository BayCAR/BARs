#' The postP.Bfun function
#'
#' @param id  The name space of a project.
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
#' #myUI("ACTION")
#'
#' @export

myUI=function (id) {
  ns=NS(id)
  fluidPage(
  titlePanel("Bayesian Covariate-Adaptive Randomization (BayCAR)!"),
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel
    ( column(6, fileInput(ns("file2"),  ("New subject file"), accept = ".csv"),
             checkboxInput(ns("header2"), "Header", TRUE)),
      column(6, fileInput(ns("excludedid"),  ("Excluded subject file"), accept = ".csv"),
             checkboxInput(ns("header3"), "Header", TRUE)),
      column(6, selectInput(ns("arms"), "Treatment Arms",
                            c("2 arms", "3 arms", "4 arms", "5 arms", "6 arms", "7 arms", "8 arms", "9 arms"),selected ="2 arms")),
      column(6, selectInput(ns("trial"), "Trial type",
                            c("multi-site"), "single-site"),selected ="multi-site"),
      column(6, numericInput(ns("planned.sample.size"), "planned.sample.size", 20)),
      column(12, selectInput(ns("Continuous"), "Continuous covariates",
                             choices = c("age", "NA", "sex"),selected = "NA", selectize = TRUE, multiple = TRUE))
    ),
    # main panel
    mainPanel(
      column(12,
             h4("Click the 'Show' button to confirm the input values for the new data.
          \n Do not click the 'Action' button unless all the values have been confirmed."),
             actionButton(ns("show"), "Show")),
      column(12, dataTableOutput(ns("tablenew"))),
      column(12, h3(uiOutput(ns("noID")))),
      column(12, selectInput(ns("display"), "Display diagnostic tables? (Please always try to do so.)", c("No", "Yes"), "Yes")),
      column(12, h4("Click on the 'Action' button to run! The group assignments for the new subject(s) will be shown in a table with a red background (underneath the optional blue diagnosis tables) below. Othersise, there will be a warning message in red."),
             actionButton(ns("action"), "Action")),
      br(),
      column(6, align="center", dataTableOutput(ns('table1'))),
      column(6, align="center", dataTableOutput(ns('table2'))),
      column(6, align="center", dataTableOutput(ns('table3'))),
      br(),
      column(6, align="center", dataTableOutput(ns('table4'))),
      br(),
      column(6, align="center", dataTableOutput(ns('table5'))),
      column(6, align="center", dataTableOutput(ns('table6'))),
      column(6, align="center", dataTableOutput(ns('table7'))),
      column(6, align="center", dataTableOutput(ns('table8'))),
      column(6, align="center", dataTableOutput(ns('table9'))),
      column(6, align="center", dataTableOutput(ns('table10'))),
      column(6, align="center", dataTableOutput(ns('table11'))),
      column(6, align="center", dataTableOutput(ns('table12'))),
      column(6, align="center", dataTableOutput(ns('table13'))),
      column(6, align="center", dataTableOutput(ns('table14'))),
      column(6, align="center", dataTableOutput(ns('table15'))),
      column(6, align="center", dataTableOutput(ns('table16'))),
      column(6, align="center", dataTableOutput(ns('table17'))),
      column(6, align="center", dataTableOutput(ns('table18'))),
      column(6, align="center", dataTableOutput(ns('table19'))),
      column(6, align="center", dataTableOutput(ns('table20'))),
      br(),
      column(12, align="center", dataTableOutput(ns('table99'))),
      column(12, textOutput(ns('display' ))),
      column(12, h3(textOutput(ns(' ')))),
      column(12, h3(uiOutput(ns('WNOnewlyRandomized')))),
      br(),
      column(12, align="center", dataTableOutput(ns('table98')))

    )
  )
)
}
