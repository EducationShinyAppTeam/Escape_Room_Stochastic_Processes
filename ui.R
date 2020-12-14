library(shiny)
library(shinydashboard)
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
library(shinyWidgets)
library(boastUtils)
library(grid)
library(ggplot2)
#library(shinyalert)



# MISSING APP META DATA, ADD

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Escape Room Template",
    titleWidth = 220,
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/', icon("home")))
    ),
  #adding prereq pages
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id='tabs',
      menuItem("Overview", tabName = "instruction", icon = icon("tachometer-alt")),
      menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
      menuItem("Game", tabName = "game", icon = icon("gamepad"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"
      )
    ),
    tabItems(
      # Overview Tab ----
      tabItem(
        tabName = "instruction",
        h1("Escape Room Game Prototype"),
        p("This is a template of Escape Room game."),
        h2("Instructions"),
        tags$ol(
          tags$li("Answer questions to earn Action points."),
          tags$li("Use Action points to interact with objects in the scene and gain items which will be stored in your backpack."),
          tags$li("Items in the backpack may need to be combined to be useful. Such as using a key or password to open a box in your backpack.")
        ),
        #p(),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "go",
            label = "Go",
            icon = icon("bolt"),
            style = "danger",
            size = "large"
          )),
        h2("Acknowledgements"),
        p("Zeyuan(Primo) Wang, Xigang Zhang",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 12/27/2020 by ZW.")
        )
      ),
      # Prereq Tab ----
      #Adding pre-requisites page to remove background from instructions page
      tabItem(
        tabName="prereq",
        h2("Background"),
        p("The escape room game is a type of game that could be applied in apps
          for most chapters."),
        p("To create your own room, you have to figure out the scene, items,
          relations, and the picture you need."),
        p("More information would go here."),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "start",
            label = "Go to the overview",
            icon = icon("bolt"),
            style = "danger",
            size = "large"
          )
        )
      ),
      # Game Tab ----
      tabItem(
        tabName ="game",
        withMathJax(),
        h2("Escape from a Coastal Living Room"),
        fluidRow(
          # I would have the activeChance object return both the leading text and
          # the value and be set up in a column with width = 3
          column(
            width = 1,
            #p("Action Point: ")
          ),
          column(
            width = 1,
            #uiOutput("activeChance")
          ),
        ),
        fluidRow(
          column(
            width = 7,
            #offset = 1,
            plotOutput(
              outputId = "target",
              click = 'Click' ,
              dblclick = "Click12", 
              width = "90%",
              height = "90%"
            )
            #, style = "height: '100%';" # Forces the row to have a certain height
          ),
          # column(
          # Not sure why this was commented out and still around
          # This is a preparation for two rooms escaperoom
          #   width = 4,
          #   plotOutput(
          #     outputId = "target2",
          #     click = 'Click2',
          #     dblclick = "Click22"
          #   ),
          #   style = "height: 320px;"
          # ),
          # column(
          #   width = 2,
          #   #uiOutput("backpack")
          #   # If this going to be a text list, you might want more than 3 columns
          # ),
          column(
            width = 5,
            h3("Action points: "),
            p("Action points you can use: "),
            uiOutput("activeChance"),
            p("Out of Action Points? "),
            p("Answer Questions below to gain Action Points to interact with scenes!"),
            h3("Backpack: "),
            uiOutput("backpack", class = "largerFont"),
            h3("Used Items: "),
            uiOutput("usedItems", class = "largerFont"),
            
            h3('Combine items:'),
            p('Select items to combine and gain new items.'),
            
            selectInput(
              inputId = "backpackList",
              label = "Backpack",
              choices = list(
                "Nothing"),
              multiple = TRUE
            ),
            
            p("Selelct Items to Combine"),
            bsButton(
              inputId = "combine",
              label = "Combine Item",
              style = "danger",
              size = "default",
              disabled = FALSE
            ),
  
            p(uiOutput("answer")),
            uiOutput("feedback4")
          )
          # column(width = 2,
          #        p("Selelct Items to Combine"),
          #        bsButton(
          #          inputId = "combine",
          #          label = "Combine Item",
          #          style = "danger",
          #          size = "default",
          #          disabled = FALSE
          #        )
          #        )
          # column(
          #   width = 1,
          #   conditionalPanel(
          #     #condition = 'input.interact != 0',
          #     #fluidRow(
          #     uiOutput("condition")
          #     # I don't know what this is supposed to be
          #     #
          #     #)
          #   )
          # )
        ),
        fluidRow(
          column(
            width = 4,
            offset = 4,
            bsButton(
              inputId = "interact",
              label = "Interact",
              type = "action", # You can omit this as this is default
              size = "large",
              value = FALSE, # You can omit these last two as they are default
              disabled = FALSE
            ),
            #useShinyalert(),
            bsButton(
              inputId = 'clear',
              label = "Restart",
              style = 'danger',
              size = 'large'
            )
          ),
          #############################################################################
          column(
            width = 3,
            offset = 1,
            # conditionalPanel(
            #   condition = "input.interact != 0",
            #   wellPanel(
            #     div(
            #       style = "position: relative; top:0",
            #       #print("Feedback")
            #     ),
            #     img(src = "arrow.gif", width = 40),
            #   )
            # )
          )
        ),
        hr(), 
        #############################################################################
        
        # If you're going to use a horizontal rule, do so here, not
        # in a fluidRow



        # fluidRow( # Why are you using a fluidRow here?
        #   conditionalPanel(
        #     condition = "input.interact != 0",
        #     p(uiOutput("answer")), # Why was this in a header tag?
        #     # You only get one (1) h1 in the entire app.
        #     # What is the point of the above prompt?
        #     wellPanel( # We generally reserve wellPanels for controls
        #       # feedback# isn't an Informative Name (see Style Guide)
        #       uiOutput("feedback1"), # how about "coordinates"
        #       uiOutput("feedback2"), # how about "backpackContents"
        #       uiOutput("feedback3"), # unknown, saving for debugging
        #       uiOutput("feedback4"), # currentGoal?
        #       uiOutput("feedback5") # unknown, for debugging
        #     )
        #   )
        # ),



        h3("Answer Questions below to gain Action Points, then use them to interact with objects in the room!"),
        wellPanel( # I'm removing the fluidRows
          h4("Scenario"),
          uiOutput("scenario"),
          h4("Question 1"),
          uiOutput("question1"),
          radioGroupButtons(
            inputId = "answersQ1",
            label = "Choose your answer",
            choices = list(
              "--Select Your Answer--",
              "A",
              "B",
              "C",
              "D"
            ),
            selected = character(0),
            status = "game",
            size = "lg",
            direction = "vertical",
            individual = FALSE,
            checkIcon = list(yes = icon("check-square"),
                             no = icon("square-o"))
          ),
          bsButton(
            inputId = "submitAnswer1",
            label = "Submit Answer",
            style = 'danger',
            size = 'medium'
          ),
          uiOutput("questionFeedback1"),
          br(),
          h4("Question 2"),
          uiOutput("question2"),
          radioGroupButtons(
            inputId = "answersQ2",
            label = "Choose your answer",
            choices = list(
              "--Select Your Answer--",
              "A",
              "B",
              "C",
              "D"
            ),
            selected = character(0),
            status = "game",
            size = "lg",
            direction = "vertical",
            individual = FALSE,
            checkIcon = list(yes = icon("check-square"),
                             no = icon("square-o"))
          ),
          bsButton(
            inputId = "submitAnswer2",
            label = "Submit Answer",
            style = 'danger',
            size = 'medium'
          ),
          uiOutput("questionFeedback2"),
          br(),
          h4("Question 3"),
          uiOutput("question3"),
          radioGroupButtons(
            inputId = "answersQ3",
            label = "Choose your answer",
            choices = list(
              "--Select Your Answer--",
              "A",
              "B",
              "C",
              "D"
            ),
            selected = character(0),
            status = "game",
            size = "lg",
            direction = "vertical",
            individual = FALSE,
            checkIcon = list(yes = icon("check-square"),
                             no = icon("square-o"))
          ),
          bsButton(
            inputId = "submitAnswer3",
            label = "Submit Answer",
            style = 'danger',
            size = 'medium'
          ),
          uiOutput("questionFeedback3")
        ),
        bsButton(
          inputId = 'nextQuestion',
          label = "Next Question",
          style = 'danger',
          size = 'large'
        ),
        hr(),
        uiOutput("trigger1"),
        uiOutput("trigger2")
      )
    )
  )
)
