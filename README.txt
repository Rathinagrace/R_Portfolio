README: SG Smart Transport AI Portfolio
1. Project Overview
This interactive R Shiny application serves as a comprehensive data science portfolio, demonstrating advanced analytics and machine learning applied to Singapore’s urban mobility ecosystem.

2. Live Deployment Link
URL: https://7zz3zw-rathina0grace-monica.shinyapps.io/R_Portfolio/

3. Features and Technical Design

Dynamic Showcase: Integrated three R Markdown reports (EDA, DNN, CNN) using a custom utility function and iframe rendering.

Deep Learning: Implemented demand forecasting using the H2O framework.

Computer Vision: Developed a Convolutional Neural Network (CNN) for real-time vehicle classification from traffic feeds.

Interactive UI: Designed with the "flatly" theme for a professional, responsive user experience.

4. Portfolio Contents

app.R: Master Shiny application source code.

www/: Directory containing knitted HTML reports for all three project phases.

.Rmd Files: Original source code for Exploratory Data Analysis, Deep Learning, and Computer Vision phases.

5. Technical Challenges Resolved

Conflict Resolution: Addressed the "Complete HTML Document" warning by transitioning from includeHTML() to a secure iframe bridge for report integration.

Code Efficiency: Developed reusable R functions to manage project tabs dynamically, reducing code duplication.

6. Contact
Author: Rathina Grace Monica
Role: Adjunct Lecturer & Data Scientist