library(shiny)
library(shinythemes)
library(shinydashboard)
library(bslib)
library(DT)

# Define UI for application
ui <- navbarPage(
  
  # Application title
  title = "Patent Analytics",
  
  # Add custom CSS styling
  tags$head(
    tags$script(src = "message-handler.js"),
    tags$style(HTML("
        /* Forest green color for active tab */
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:hover, 
    .nav-tabs > li.active > a:focus {
      background-color: #228B22 !important;
      color: #fff !important;
      border-color: #228B22 !important;
    }
    /* Forest green color for inactive tabs */
    .nav-tabs > li > a {
      color: #fff;
      background-color: #333 !important;
      border-color: #228B22 !important;
    }
    /* White text for tab labels */
    .nav-tabs > li > a {
      color: #fff !important;
    }
    /* White text for active tab labels */
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:hover, 
    .nav-tabs > li.active > a:focus {
      color: #fff !important;
    }
      /* Navbar styling */
      .navbar-inverse {
        background-color: #333 !important;
        border-color: #444 !important;
      }
      .navbar-inverse .navbar-nav > li > a {
        color: #fff !important;
      }
      .navbar-inverse .navbar-nav > li > a:hover, 
      .navbar-inverse .navbar-nav > li > a:focus {
        color: #fff !important;
        background-color: #006633 !important;
      }
      .navbar-inverse .navbar-brand {
        color: #fff !important;
      }
      
      /* Tabs styling */
      .nav-tabs > li > a {
        color: #fff;
        background-color: #333;
        border-color: #444;
      }
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:hover, 
      .nav-tabs > li.active > a:focus {
        color: #333;
        background-color: #006633;
        border-color: #444;
      }
      .nav-tabs > li > a:hover, 
      .nav-tabs > li > a:focus {
        color: #fff;
        background-color: #444;
      }
      
      /* Form input styling */
      .form-group input[type='text'] {
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 6px 12px;
        font-size: 14px;
        color: #fff;
        background-color: #444;
      }
      .form-group .btn-primary {
        background-color: #006633;
        border-color: #006633;
      }
      .form-group .btn-primary:hover {
        background-color: #005329;
        border-color: #005329;
      }
      .form-group label {
        color: #fff;
      }
      
      /* Page content styling */
      body {
        background-color: #222;
        color: #fff;
      }
      h2 {
        font-size: 28px;
        font-weight: bold;
        margin-top: 30px;
        margin-bottom: 20px;
      }
      p {
        font-size: 16px;
        line-height: 1.5;
        margin-bottom: 30px;
      }
      
      /* Highlight color */
      .highlight {
        color: #007A4D;
      }
      .nav-tabs > li.active > a.highlight, 
      .nav-tabs > li.active > a:hover.highlight, 
      .nav-tabs > li.active > a:focus.highlight {
        color: #fff;
        background-color: #007A4D;
        border-color: #444;
      }
      .form-group input[type='text'].highlight {
        border-color: #007A4D;
      }
      .form-group .btn-primary{
      background-color:#228B22;
      }
      .form-group .btn-primary.highlight {
        background-color: #007A4D;
        border-color: #007A4D;
      }
      .form-group .btn-primary.highlight:hover {
        background-color: #005329;
        border-color: #005329;
      }
    "))
  ),
  # Home page
  tabPanel(
    "Home",
    h2("Welcome to the wonderful world of patent analytics!"),
    p("Please select an analysis from the tabs above.")
  ),
  
  # Competition analysis page
  tabPanel(
    "Competition",
    fluidPage(
      tags$label("CPC Codes"),
      textInput("cpc_codes", "", placeholder = "Enter codes..."),
      tags$label("Sub CPC Codes"),
      textInput("trends_input2", "", placeholder = "Enter codes..."),
      tags$label("CPC Labels"),
      textInput("trends_input3", "", placeholder = "Enter labels..."),
      textInput("n", "n", 50),
      actionButton("go", "Go", class='btn-primary'),
      DTOutput(outputId = 'competition_dt')
    )
  ),
  
  # Trends analysis page
  tabPanel(
    "Trends",
    fluidPage(
      fluidRow(
        column(
          3,
          tags$label("CPC Codes"),
          textInput("trends_input1", "", placeholder = "Enter codes..."),
          tags$label("Sub CPC Codes"),
          textInput("trends_input2", "", placeholder = "Enter codes..."),
          tags$label("CPC Labels"),
          textInput("trends_input3", "", placeholder = "Enter labels..."),
          actionButton("trends_button", "Run Analysis", class = "btn-primary")
        ),
        column(
          9,
          h2("Trends Analysis Results"),
          textOutput("trends_output1"),
          textOutput("trends_output2"),
          textOutput("trends_output3")
        )))))

        
        
