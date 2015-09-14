library(shiny)
library(DT)

shinyUI(navbarPage("myRecipeFacts",theme="theme.css",
                   tabPanel("About",
                            fluidRow(
                              column(12,
                                     strong("About me and the site"), helpText("I made this site with one goal: to give people an easier way to track their nutrition.
                                                                               Typical nutrition sites allow you to enter single items and select a match from a dropdown menu, which would be tedious if you wanted to enter an entire recipe.
                                                                               This site makes the process simple. You can type in the recipe line by line, or even copy-paste recipes you found on the web if it fits the format.
                                                                               The site currently has the core functionalities, but additional features are planned.
                                                                               Thank you and enjoy!"),
              strong("Contact"), helpText("If you find any problems, please email me.
                                          If the problem occurred due to a recipe input, please include the exact text you used so that I may replicate and solve the problem."),
                helpText("nutriTrackWeb@gmail.com"),#support@myrecipefacts.com"),
              strong("Other Sites"), helpText("If you would like to support my other projects, please take a look at the following links:"),
              helpText(a("Math/pun T-shirts", href="http://www.cafepress.com/LameShirtsAbound", target="_blank")),
              strong("Disclaimer"), helpText("There may be discrepencies between actual nutrients and the ones in the Nutritionix database.
                              So take these values with a grain of salt. For example, the carrots registered in the database may be different from what you use, leading to natural differences.
                                             The database is in no way complete and universal. There may be errors in the data or even how it is reported. It may not even contain the item you want."),
              helpText('From Nutritionix: "Our mission is to provide the most accurate nutrition information possible, but errors in our data can occur. If you have specific food allergies or diet restrictions, please consult with a physician or registered dietitian before changing your diet habits. This data is provided for informational purposes only, and Nutritionix provides no guarantees in respect to the accuracy of the data"'),
              strong("Limitations"), helpText("The data comes from the Nutritionix API which has restrictions in place. One of these is that there is a limit to how many database searches are allowed through this site.
                                              Thus, if the site stops working due to this restriction, please try again the next day.")
                              ),
              helpText("Coded in R/Rstudio and ", a("shinyapps", href="http://shiny.rstudio.com/", target="_blank")),
              helpText("Data is provided by and thanks to", a("Nutritionix API", href="http://www.nutritionix.com/api", target="_blank")),
              helpText("Copyright 2015; Longphi Nguyen")
                            )
                   ),
  
  navbarMenu("Nutrition Calculator",
  tabPanel("How To Use",
           h3("[!] Note: Some browsers may not show the app correctly such as Google Chrome."),
           wellPanel(
           h3("How to use the nutrition calculator:"),
           h6('Looking to calculate how much calories, protein, carbs, and more are in your meal?'),
           h6('Enter a recipe and press the Submit button. If any items did not match the database, then errors will appear in red; you may continue normally, however the items in red will be excluded from the calculations.'),
           h6("Example input: 1 2/3 tbs cinnamon"),
           h6("               2 eggs"))),
           
  tabPanel("Nutrition Calculator",
           fluidRow(
             tabsetPanel("Your Recipe", id="recipeTab",
                         tabPanel("Step 1: Recipe",
                                  fluidRow(column(2,uiOutput("submitButton")),
                                          column(2, actionButton("clearButton", label = "Clear Box"))),
                                  textOutput("characterlimit"),
                                  uiOutput("recipeBox"),#tags$textarea(id="recipeInput", rows=10, cols=110, "Test", maxlength=750),
                                  verbatimTextOutput("errorMessages"),
                                  tags$head(tags$style("#errorMessages{color: red;
                                 font-style: italic;
                                 }"
                                  ))
                         ),
                         tabPanel("Step 2: Database",
                                  fluidRow(
                                  column(2,actionButton("seeNutrition", label = "Go To Step 3")),
                                  column(2,uiOutput("previousButton")),
                                  column(8,uiOutput("nextButton"))
                                  ),
                                  textOutput("pageNumber"),
                                  tags$head(tags$style("#pageNumber{font-size: 10px;}")),
                                  fluidRow(column(3,strong("Your Items")), column(9, strong("Database match (measurements are not yet converted to match yours)"))), 
                                  fluidRow(column(2,textOutput("select1_info")),
                                           #column(1,uiOutput("check1")),# output$check1=checkboxInput("check1", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_1"))
                                  ),
                                  fluidRow(column(2,textOutput("select2_info")),
                                           #column(1,uiOutput("check2")),#column(1, checkboxInput("check2", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_2"))
                                  ),
                                  fluidRow(column(2,textOutput("select3_info")),
                                           #column(1,uiOutput("check3")),#column(1, checkboxInput("check3", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_3"))
                                  ),
                                  fluidRow(column(2,textOutput("select4_info")),
                                           #column(1,uiOutput("check4")),#column(1, checkboxInput("check4", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_4"))
                                  ),
                                  fluidRow(column(2,textOutput("select5_info")),
                                           #column(1,uiOutput("check5")),#column(1, checkboxInput("check5", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_5"))
                                  ),
                                  fluidRow(column(2,textOutput("select6_info")),
                                           #column(1,uiOutput("check6")),#column(1, checkboxInput("check6", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_6"))
                                  ),
                                  fluidRow(column(2,textOutput("select7_info")),
                                           #column(1,uiOutput("check7")),#column(1, checkboxInput("check7", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_7"))
                                  ),
                                  fluidRow(column(2,textOutput("select8_info")),
                                           #column(1,uiOutput("check8")),#column(1, checkboxInput("check8", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_8"))
                                  ),
                                  fluidRow(column(2,textOutput("select9_info")),
                                           #column(1,uiOutput("check9")),#column(1, checkboxInput("check9", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_9"))
                                  ),
                                  fluidRow(column(2,textOutput("select10_info")),
                                           #column(1,uiOutput("check10")),#column(1, checkboxInput("check10", label = "", value = FALSE)),
                                           column(9,htmlOutput("selectUI_10"))
                                  )
                         ),
                         tabPanel("Step 3: Nutrition",
                                  fluidRow(column(4, textInput("servingInput", label="Divide into servings:", value=1))),
                                  fluidRow(
                           column(12,htmlOutput("sortColumns")),
                           column(12,DT::dataTableOutput("nutritionTable"))
                         ))
                                  
             ) # tabsetPanel - "Your Recipe"
           ) # fluidRow
  )), # tabPanel - "Nutrition"
                      
  navbarMenu("Measurement Conversion",
  tabPanel("How To Use",
             wellPanel(
               h3("How to use the measurement converter:"),
               h6("Looking to scale up or down a recipe? Enter a recipe and type in a multiplier."),
               h6("You can rescale the recipe, keeping the original recipe's measurements or automatically choose the smallest necessary measurement."),
               h6('The custom unit option allows you to specify what the units are converted to. Enter the desired units into the "custom units" box for each line. These should be aligned with the recipe. Blank lines will keep the original unit for those lines.')
             )),
  tabPanel("Measurement Converter",
           fluidRow(
             column(3, textInput("mc_scale", label="Multiply recipe by:", value=1))),
           fluidRow(
             column(2, strong("Rescale using:")),
             column(2, actionButton("mc_submit", label = "original units")),
             column(2, actionButton("mc_submit3", label = "custom units")),
             column(2, actionButton("mc_submit2", label = "optimized units"))
           ),
           br(),
           fluidRow(
             column(4, strong("Your recipe")),
             column(2, strong("Custom units (optional)")),
             column(4, strong("Rescaled recipe"))
           ),
           fluidRow(
             column(4, tags$textarea(id="mc_input", rows=10, cols=35, "", maxlength=1500)),
             column(2, tags$textarea(id="mc_units", rows=10, cols=15, "", maxlength=1500)),
             column(4, verbatimTextOutput("mc_rescaledText"))
             )
           )) # tabPanel - "Measurement Conversion"
))
