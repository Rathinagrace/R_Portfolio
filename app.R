library(shiny)
library(shinythemes)
library(dplyr)

# --- 1. SETUP RESOURCE PATH ---
# This allows Shiny to access your HTML files for the iframes.
addResourcePath("reports", getwd())

# --- 4.5 UTILITY FUNCTION: DYNAMIC IFRAME GENERATOR ---
create_project_tab <- function(title, file_name) {
  tabPanel(title,
           fluidPage(
             tags$iframe(src = paste0("reports/", file_name), 
                         style = "width:100%; height:800px; border:none;")
           )
  )
}

# --- UI DESIGN ---
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "SG Smart Transport AI Portfolio",
  
  # --- 1. PROJECT OVERVIEW (README CONTENT) ---
  tabPanel("Home",
           fluidPage(
             column(10, offset = 1,
                    wellPanel(
                      h1("SG Smart Transport AI Portfolio", style = "color: #2c3e50;"),
                      p("A comprehensive data science portfolio demonstrating advanced analytics and machine learning applied to Singapore’s urban mobility ecosystem.", 
                        style = "font-size: 18px; font-weight: 500; color: #34495e;")
                    ), # <--- COMMA ADDED
                    h3("1. Project Overview"),
                    p("This interactive R Shiny application showcases the integration of deep learning and computer vision to optimize urban transport logistics."),
                    
                    h3("2. Live Deployment"),
                    p(strong("URL: "), tags$a(href="https://7zz3zw-rathina0grace-monica.shinyapps.io/R_Portfolio/", "https://7zz3zw-rathina0grace-monica.shinyapps.io/R_Portfolio/")),
                    
                    h3("3. Features and Technical Design"),
                    tags$ul(
                      tags$li(strong("Dynamic Showcase:"), "Integrated R Markdown reports using a custom utility function and secure iframe rendering."),
                      tags$li(strong("Deep Learning:"), "Implemented demand forecasting using the H2O framework."),
                      tags$li(strong("Computer Vision:"), "Developed a Convolutional Neural Network (CNN) for real-time vehicle classification."),
                      tags$li(strong("Interactive UI:"), "Designed with a responsive 'flatly' theme for professional presentation.")
                    ),
                    
                    h3("4. Portfolio Contents"),
                    tags$ul(
                      tags$li(code("app.R"), ": Master Shiny application source code."),
                      tags$li(code("www/"), ": Directory containing knitted HTML reports."),
                      tags$li(code(".Rmd Files"), ": Original source code for EDA, Deep Learning, and Computer Vision phases.")
                    ),
                    
                    h3("5. Technical Challenges Resolved"),
                    tags$ul(
                      tags$li(strong("Conflict Resolution:"), "Transitioned from ", code("includeHTML()"), " to an iframe bridge to fix document warning conflicts."),
                      tags$li(strong("Code Efficiency:"), "Modularized UI components with R functions to maintain a clean codebase.")
                    ),
                    
                    hr(),
                    h3("6. Contact"),
                    p(strong("Author: "), "Rathina Grace Monica"),
                    p(strong("Role: "), "Adjunct Lecturer & Data Scientist")
             )
           )
  ),
  
  # --- 2. PROJECT SHOWCASE ---
  navbarMenu("Projects",
             create_project_tab("Phase 1: EDA", "eda_report.html"),
             create_project_tab("Phase 2: Deep Learning", "dnn_report.html"),
             create_project_tab("Phase 3: Computer Vision", "cnn_report.html")
  ),
  
  # --- 3. PROJECT LEAD PROFILE ---
  tabPanel("Project Lead Profile",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h2("Rathina Grace Monica"),
                   h4(tags$i("Adjunct Lecturer & Data Scientist")),
                   hr(),
                   h5(strong("Academic Credentials:")),
                   tags$ul(
                     tags$li(strong("Master of Engineering"), " - First Class Distinction"),
                     tags$li(strong("Bachelor of Engineering"), " - First Class Distinction"),
                     tags$li(strong("WSQ ACLP 2.0 Certified"), " - Adult Educator")
                   )
                 ),
                 h4("Technical Skills"),
                 div(
                   span(class="label label-primary", "Deep Learning (H2O/Keras)"),
                   span(class="label label-info", "R / Shiny Programming"),
                   span(class="label label-success", "Python (Pandas/NumPy)"),
                   span(class="label label-warning", "Computer Vision (CNN)")
                 )
               ),
               mainPanel(
                 h3("Project Spotlight: SG Smart Transport AI"),
                 p("Utilizing the LTA DataMall ecosystem to solve 'Last Mile' connectivity challenges."),
                 wellPanel(
                   h4(strong("Deep Dive into the AI Engine")),
                   tags$ul(
                     tags$li(strong("Data Synthesis:"), "Pipeline for real-time mobility feeds."),
                     tags$li(strong("Deep Learning:"), "DNN optimization for non-linear demand forecasting."),
                     tags$li(strong("Computer Vision:"), "CNN architecture for vehicle recognition.")
                   )
                 )
               )
             )
           )),
  
  # --- 4. CONTACT TAB ---
  tabPanel("Contact",
           fluidPage(
             wellPanel(
               h3("Contact Information"),
               p(strong("Email:"), "rathina.grace.monica@example.com"),
               p(strong("Location:"), "Singapore")
             )
           ))
) 

# --- SERVER LOGIC ---
server <- function(input, output) { }

# --- RUN APPLICATION ---
shinyApp(ui = ui, server = server)