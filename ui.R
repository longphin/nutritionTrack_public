library(shiny)
library(DT)

shinyUI(navbarPage("NutritionTrack",
  tabPanel("How to use this site", wellPanel(
    h6('Enter a recipe and press the Submit button. If any items did not match the database, then errors will appear in red. You may continue normally, however the items in red will be excluded from the results.'),
    h6('The "Database Matches" tab contains dropdown lists for the items that were matched. You can use these to fix any matches that the system made.'),
    h6("Example input: 1 2/3 tbs cinnamon"),
    h6("               2 eggs")
    )),
  
  tabPanel("Nutrition",
           fluidRow(
             tabsetPanel("Your Recipe", id="recipeTab",
                         tabPanel("Your Recipe",
                                  #actionButton("submit", label = "Submit"),
                                  uiOutput("submitButton"),
                                  textOutput("characterlimit"),
                                  #column(12, actionButton("submit", label = "Submit"), textOutput("characterlimit")),
                                  #column(4, tags$textarea(id="recipeInput", rows=10, cols=40, "",maxlength=500))
                                  tags$textarea(id="recipeInput", rows=10, cols=110, "", maxlength=750),
                                  verbatimTextOutput("errorMessages"),
                                  tags$head(tags$style("#errorMessages{color: red;
                                 font-style: italic;
                                 }"
                                  ))
                         ),
                         tabPanel("Database Matches",
                                  fluidRow(
                                  column(2,actionButton("seeNutrition", label = "see Nutrition")),
                                  column(2,uiOutput("previousButton")),
                                  column(8,uiOutput("nextButton"))
                                  ),
                                  textOutput("pageNumber"),
                                  tags$head(tags$style("#pageNumber{font-size: 10px;}")),
                                  fluidRow(column(3,strong("Your Items")), column(9, strong("Database match (measurements are not yet converted to match yours)"))), 
                                  fluidRow(column(3,textOutput("select1_info")),
                                           column(9,htmlOutput("selectUI_1"))
                                  ),
                                  fluidRow(column(3,textOutput("select2_info")),
                                           column(9,htmlOutput("selectUI_2"))
                                  ),
                                  fluidRow(column(3,textOutput("select3_info")),
                                           column(9,htmlOutput("selectUI_3"))
                                  ),
                                  fluidRow(column(3,textOutput("select4_info")),
                                           column(9,htmlOutput("selectUI_4"))
                                  ),
                                  fluidRow(column(3,textOutput("select5_info")),
                                           column(9,htmlOutput("selectUI_5"))
                                  ),
                                  fluidRow(column(3,textOutput("select6_info")),
                                           column(9,htmlOutput("selectUI_6"))
                                  ),
                                  fluidRow(column(3,textOutput("select7_info")),
                                           column(9,htmlOutput("selectUI_7"))
                                  ),
                                  fluidRow(column(3,textOutput("select8_info")),
                                           column(9,htmlOutput("selectUI_8"))
                                  ),
                                  fluidRow(column(3,textOutput("select9_info")),
                                           column(9,htmlOutput("selectUI_9"))
                                  ),
                                  fluidRow(column(3,textOutput("select10_info")),
                                           column(9,htmlOutput("selectUI_10"))
                                  )
                         ),
                         tabPanel("The Facts",
                                  fluidRow(column(4, textInput("servingInput", label="Divide into servings:", value=1))),
                                  fluidRow(
                           column(12,htmlOutput("sortColumns")),
                           column(12,DT::dataTableOutput("nutritionTable"))
                         ))
                                  
             ) # tabsetPanel - "Your Recipe"
           ) # fluidRow
  ), # tabPanel - "Nutrition"
                      
 # tabPanel("Measurement Conversion"),
  
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
                helpText("NutriTrackWeb@gmail.com"),
              strong("Other Sites"), helpText("If you would like to support my other projects, please take a look at the following links:"),
                helpText(a("Math/pun T-shirts", href="http://www.cafepress.com/LameShirtsAbound", target="_blank")),
              strong("Disclaimer"), helpText("There may be discrepencies between actual nutrients and the ones in the Nutritionix database.
                              So take these values with a grain of salt. For example, the carrots registered in the database may be different from what you use, leading to natural differences.
                                             The database is in no way complete and universal. There may be errors in the data or even how it is reported. It may not even contain the item you want."),
                                   helpText('From Nutritionix: "Our mission is to provide the most accurate nutrition information possible, but errors in our data can occur. If you have specific food allergies or diet restrictions, please consult with a physician or registered dietitian before changing your diet habits. This data is provided for informational purposes only, and Nutritionix provides no guarantees in respect to the accuracy of the data"'),
              strong("Limitations"), helpText("The data comes from the Nutritionix API which has restrictions in place. One of these is that there is a limit to how many database searches are allowed through this site.
                                              Thus, if the site stops working due to this restriction, please try again the next day.")
             ),
             helpText("Data is provided by and thanks to", a("Nutritionix API", href="http://www.nutritionix.com/api", target="_blank"))
           )
  )
))
