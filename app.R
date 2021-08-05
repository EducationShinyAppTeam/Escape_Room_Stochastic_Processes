# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Load additional dependencies and setup functions ----
source('coastal-room.R')

# Define constants ----
arbitraryChoices <- list("A", "B", "C", "D")

# Load Data ----
questionBank <- read.csv(file = "questionbank.csv", header = TRUE)
roomItems <- read.csv(file = "roomItems.csv", header = TRUE )

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Stochastic Escape Room",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Escape_Room_Stochastic_Processes")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Escape Room", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Coastal Escape Room for Stochastic Processes"),
          p("We need to build some overview text."),
          h2("Instructions"),
          tags$ol(
            tags$li("The app works best in a maximized window."),
            tags$li("Answer questions to earn action points."),
            tags$li("Use action points to interact with objects in the scene
                    and gain items which will be stored in your backpack."),
            tags$li("Items in the backpack may need to be combined to be useful.
                    Such as using a key or password to open a box in your backpack.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "Go",
              icon = icon("bolt"),
              size = "large"
            )),
          h2("Acknowledgements"),
          p("This app was originally created by Zeyuan (Primo) Wang with Xigang
            Zhang supplying original artwork.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 8/5/2021 by NJH.")
          )
        ),
        #### Prerequisites Page ----
        ##### Needs Completion ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("The escape room game is a type of game that could be applied in apps
          for most chapters."),
          p("To create your own room, you have to figure out the scene, items,
          relations, and the picture you need."),
          p("More information would go here.")
        ),
        #### Escape Room Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Escape from a Coastal Living Room"),
          p("Answer questions (below) to earn action points to interact with
            elements of the scene. Click on different parts of scene and then
            press the Interact button."),
          bsButton(
            inputId = "debug",
            label = "debug",
            icon = icon("bug"),
            size = "large"
          ),
          fluidRow(
            column(
              width = 7,
              offset = 0,
              coastalRoom,
              tags$script(HTML(
                "document.getElementById('coastalRoom').focus();
                $('#objects').on('click', '.scene-object', (ev) => {
                  Shiny.setInputValue('object', ev.target.id);
                })"
              ))
            ),
            ##### Scene Info Column ----
            column(
              width = 5,
              uiOutput("clickedObject", class = "largerFont"),
              bsButton(
                inputId = "interactObject",
                label = "Interact with object",
                icon = icon("hand-point-up"),
                size = "large",
                style = "success",
                disabled = TRUE
              ),
              br(),
              br(),
              uiOutput("actionPointReport", class = "largerFont"),
              p("Out of action points? Answer questions below to gain action
                points to interact with the scene and objects."),
              hr(),
              h3("Collected Items"),
              p("Your backpack contains:"),
              uiOutput("backpackContents"),
              h4("Combine Items"),
              p("Select two items from your backpack to use an action point to
                combine."),
              selectInput(
                inputId = "selectedItems",
                label = "Items to combine",
                choices = "nothing",
                selected = NULL,
                multiple = TRUE
              ),
              bsButton(
                inputId = "combineItems",
                label = "Combine items",
                icon = icon("object-group"),
                style = "warning",
                size = "large"
              ),
              hr(),
              bsButton(
                inputId = "resetGame",
                label = "Reset Game",
                style = "danger",
                size = "large"
              )
            )
          ),
          hr(),
          h3("Earn Action Points"),
          p("Use the context to answer questions to earn more action points."),
          ##### Questions ----
          h4("Scenario"),
          uiOutput("scenario"),
          h4("Question 1"),
          uiOutput("question1"),
          radioGroupButtons(
            inputId = "answersQ1",
            label = "Choose your answer",
            choices = arbitraryChoices,
            selected = character(0),
            size = "lg",
            direction = "vertical",
            individual = FALSE,
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            ),
            status = "game"
          ),
          fluidRow(
            column(
              width = 2,
              offset = 0,
              bsButton(
                inputId = "submitAnswer1",
                label = "Submit Answer",
                style = "success",
                size = "large"
              )
            ),
            column(
              width = 10,
              offset = 0,
              uiOutput("questionFeedback1")
            )
          ),
          br(),
          h4("Question 2"),
          uiOutput("question2"),
          radioGroupButtons(
            inputId = "answersQ2",
            label = "Choose your answer",
            choices = arbitraryChoices,
            selected = character(0),
            size = "lg",
            direction = "vertical",
            individual = FALSE,
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            )
          ),
          fluidRow(
            column(
              width = 2,
              offset = 0,
              bsButton(
                inputId = "submitAnswer2",
                label = "Submit Answer",
                style = "success",
                size = "large"
              )
            ),
            column(
              width = 10,
              offset = 0,
              uiOutput("questionFeedback2")
            )
          ),
          br(),
          h4("Question 3"),
          uiOutput("question3"),
          radioGroupButtons(
            inputId = "answersQ3",
            label = "Choose your answer",
            choices = arbitraryChoices,
            selected = character(0),
            size = "lg",
            direction = "vertical",
            individual = FALSE,
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            )
          ),
          fluidRow(
            column(
              width = 2,
              offset = 0,
              bsButton(
                inputId = "submitAnswer3",
                label = "Submit Answer",
                style = "success",
                size = "large"
              )
            ),
            column(
              width = 10,
              offset = 0,
              uiOutput("questionFeedback3")
            )
          ),
          br(),
          bsButton(
            inputId = 'nextQuestion',
            label = "New Context and Questions",
            style = "warning",
            size = "large"
          ),
          uiOutput("trigger1"),
          uiOutput("trigger2")
        ),
        #### Set up the References Page ----
        ##### Needs Completing ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## Debugging ----
  observeEvent(
    eventExpr = input$debug,
    handlerExpr = {
      print(mapping())
      sendSweetAlert(
        session = session,
        title = "Winner!",
        type = "success",
        html = TRUE,
        text = tags$div(
          tags$p("Congrats! You have escaped from the room; enjoy the beach."),
          tags$img(src = "beach.png", alt = "a beach scene", width = "100%")
        )
      )
    }
  )

  ## User Specific Elements ----
  interactedList <- reactiveVal("start")
  actionPoints <- reactiveVal(10)
  backpackNew <- reactiveVal(NULL)
  index <- reactiveVal(sample(x = nrow(questionBank) / 3, size = 1) * 3 - 2)

  ### Hide items behind objects ----
  mapping <- reactiveVal({
    places <- objects$name[which(objects$assignable != "no")]
    places <- sample(places, length(places), replace = FALSE)
    mappings <- roomItems
    for (i in 1:nrow(mappings)) {
      if (mappings$location[i] == "") {
        mappings$location[i] <- places[i]
      }
    }
    mappings
  })


  ## Watch and report selected scene object ----
  observeEvent(
    eventExpr = input$object,
    handlerExpr = {
      if (is.null(input$object)) {
        message <- "Select an object in the scene."
      } else {
        cleanObject <- gsub(
          pattern = "_",
          replacement = " ",
          x = input$object
        )
        message <- paste0(
          "You've clicked on the ",
          cleanObject,
          "."
        )
        updateButton(
          session = session,
          inputId = "interactObject",
          label = paste(" Interact with the", cleanObject),
          disabled = FALSE
        )
      }
      output$clickedObject <- renderUI({
        p(message)
      })
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  ## Interact with selected object ----
  observeEvent(
    eventExpr = input$interactObject,
    handlerExpr = {
      if (input$object == "exit" && "exitKey" %in% backpackNew()) {
        sendSweetAlert(
          session = session,
          title = "Winner!",
          type = "success",
          html = TRUE,
          text = tags$div(
            tags$p("Congrats! You have escaped from the room; enjoy the beach."),
            tags$img(src = "beach.png", alt = "a beach scene", width = "100%")
          )
        )
        updateButton(
          session = session,
          inputId = "interactObject",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "combineItems",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
      } else if ((input$object == "exit" && !("exitKey" %in% backpackNew())) ||
                 (input$object == "closet" && !("closetKey" %in% backpackNew()))) {
        sendSweetAlert(
          session = session,
          title = "Key Needed",
          type = "warning",
          text = "A key is needed to interact further with this object."
        )
      } else if (actionPoints() < 1) {
        sendSweetAlert(
          session = session,
          title = "Out of Action Points",
          type = "warning",
          text = "You're out of action points. Correctly answer questions
            below to earn more points to spend on actions."
        )
      } else if (input$object %in% interactedList()) {
        sendSweetAlert(
          session = session,
          title = "Already Interacted",
          type = "info",
          text = "You've already interacted with this object; there is nothing
          more for you to do with it."
        )
      } else {
        actionPoints(actionPoints() - 1)
        interactedList(c(interactedList(), input$object))
        if (!(input$object %in% mapping()$location)) {
          sendSweetAlert(
            session = session,
            title = "Nothing Found",
            type = "info",
            text = "There is nothing here."
          )
        } else {
          foundItem <- mapping()$itemName[which(mapping()$location == input$object)]
          foundItemDesp <- mapping()$description[which(mapping()$location == input$object)]
          sendSweetAlert(
            session = session,
            title = "Found Item!",
            type = "info",
            text = paste0("You have found ", foundItemDesp,". It's been added to
                          your backpack.")
          )
          backpackNew(c(backpackNew(), foundItem))
          updateSelectInput(
            session = session,
            inputId = "selectedItems",
            choices = backpackNew()
          )
        }
      }
    }
  )

  ## Combine items ----
  observeEvent(
    eventExpr = input$combineItems,
    handlerExpr = {
      if (length(input$selectedItems) < 2 || is.null(input$selectedItems)) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Too few items selected; please select two items to combine.",
          type = "error"
        )
      } else if (length(input$selectedItems) > 2) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Too many items selected; please select two items to combine.",
          type = "error"
        )
      } else if (actionPoints() <= 0) {
        sendSweetAlert(
          session = session,
          title = "Out of Action Points",
          type = "warning",
          text = "You're out of action points. Correctly answer questions
            below to earn more points to spend on actions."
        )
      } else {
        box <- input$selectedItems[grepl("Box", input$selectedItems)]
        key <- input$selectedItems[grepl("Key", input$selectedItems)]

        if (length(box) != 0 & length(key) != 0) {
          newItem <- roomItems$itemName[which(roomItems$location == box &
                                                roomItems$required == key)]
          newDescp <- roomItems$description[which(roomItems$location == box &
                                                    roomItems$required == key)]
        } else {
          newItem <- "none"
        }

        if (grepl("Key", newItem)) {
          sendSweetAlert(
            session = session,
            title = "New Item!",
            type = "info",
            text = paste0(
              "You successfully combined the items and have found ",
              newDescp,
              ". This has been added to your backpack."
            )
          )
          backpackNew(c(backpackNew(), newItem))
          actionPoints(actionPoints() - 1)
          updateSelectInput(
            session = session,
            inputId = "selectedItems",
            choices = backpackNew()
          )
        } else {
          sendSweetAlert(
            session = session,
            title = "Nothing Happened",
            type = "info",
            text = "Nothing happened when you combined the objects."
          )
        }
      }
    }
  )



  ## Initial Game Settings ----
  gameInProgress <- FALSE

  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "Click on different parts of the scene to interact. Answer
        questions to earn more action points.",
        type = "info"
      )
    })

  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game"
    )
  })

  ## Initial Loading of Context and Questions ----
  observeEvent(
    eventExpr = input$pages,
    handlerExpr = {
      if (input$pages == "game" && gameInProgress == FALSE) {
        output$scenario <- renderUI({p(questionBank[index(), "scenario"])})

        output$question1 <- renderUI({
          withMathJax(questionBank[index(), "question"])
        })
        updateRadioGroupButtons(
          session = session,
          inputId = "answersQ1",
          choices = list(
            questionBank[index(), "A"],
            questionBank[index(), "B"],
            questionBank[index(), "C"],
            questionBank[index(), "D"]
          ),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "game"
        )

        output$question2 <- renderUI({
          withMathJax(questionBank[index() + 1, "question"])
        })
        updateRadioGroupButtons(
          session = session,
          inputId = "answersQ2",
          choices = list(
            questionBank[index() + 1, "A"],
            questionBank[index() + 1, "B"],
            questionBank[index() + 1, "C"],
            questionBank[index() + 1, "D"]
          ),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "game"
        )

        output$question3 <- renderUI({
          withMathJax(questionBank[index() + 2, "question"])
        })
        updateRadioGroupButtons(
          session = session,
          inputId = "answersQ3",
          choices = list(
            questionBank[index() + 2, "A"],
            questionBank[index() + 2, "B"],
            questionBank[index() + 2, "C"],
            questionBank[index() + 2, "D"]
          ),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "game"
        )

        gameInProgress <- TRUE
      }
    })

  ## Update Context and Questions ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      ### Need to rethink the randomization of indices
      index(sample(nrow(questionBank) / 3, 1) * 3 - 2)

      output$scenario <- renderUI({p(questionBank[index(), "scenario"])})

      output$question1 <- renderUI({
        withMathJax(questionBank[index(), "question"])
      })
      updateRadioGroupButtons(
        session = session,
        inputId = "answersQ1",
        choices = list(
          questionBank[index(), "A"],
          questionBank[index(), "B"],
          questionBank[index(), "C"],
          questionBank[index(), "D"]
        ),
        selected = character(0),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = "game"
      )

      output$question2 <- renderUI({
        withMathJax(questionBank[index() + 1, "question"])
      })
      updateRadioGroupButtons(
        session = session,
        inputId = "answersQ2",
        choices = list(
          questionBank[index() + 1, "A"],
          questionBank[index() + 1, "B"],
          questionBank[index() + 1, "C"],
          questionBank[index() + 1, "D"]
        ),
        selected = character(0),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = "game"
      )

      output$question3 <- renderUI({
        withMathJax(questionBank[index() + 2, "question"])
      })
      updateRadioGroupButtons(
        session = session,
        inputId = "answersQ3",
        choices = list(
          questionBank[index() + 2, "A"],
          questionBank[index() + 2, "B"],
          questionBank[index() + 2, "C"],
          questionBank[index() + 2, "D"]
        ),
        selected = character(0),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = "game"
      )

      updateButton(
        session = session,
        inputId = "submitAnswer1",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "submitAnswer2",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "submitAnswer3",
        disabled = FALSE
      )

      output$questionFeedback1 <- renderIcon()
      output$questionFeedback2 <- renderIcon()
      output$questionFeedback3 <- renderIcon()
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Answer Checking ----
  observeEvent(
    eventExpr = input$submitAnswer1,
    handlerExpr = {
      if (is.null(input$answersQ1)) {
        output$questionFeedback1 <- renderUI({p("Please select an answer.")})
      } else {
        ans1 <- questionBank[index(), "answer"]
        output$questionFeedback1 <- renderIcon(
          icon = ifelse(
            test = input$answersQ1 == ans1,
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        if (input$answersQ1 == ans1) {
          actionPoints(actionPoints() + 1)
        }
        updateButton(
          session = session,
          inputId = "submitAnswer1",
          disabled = TRUE
        )
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$submitAnswer2,
    handlerExpr = {
      if (is.null(input$answersQ2)) {
        output$questionFeedback2 <- renderUI({p("Please select an answer.")})
      } else {
        ans2 <- questionBank[index() + 1, "answer"]
        output$questionFeedback2 <- renderIcon(
          icon = ifelse(
            test = input$answersQ2 == ans2,
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        if (input$answersQ2 == ans2) {
          actionPoints(actionPoints() + 1)
        }
        updateButton(
          session = session,
          inputId = "submitAnswer2",
          disabled = TRUE
        )
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    eventExpr = input$submitAnswer3,
    handlerExpr = {
      if (is.null(input$answersQ3)) {
        output$questionFeedback3 <- renderUI({p("Please select an answer.")})
      } else {
        ans3 <- questionBank[index(), "answer"]
        output$questionFeedback3 <- renderIcon(
          icon = ifelse(
            test = input$answersQ3 == ans3,
            yes = "correct",
            no = "incorrect"
          ),
          width = 48
        )
        if (input$answersQ3 == ans3) {
          actionPoints(actionPoints() + 1)
        }
        updateButton(
          session = session,
          inputId = "submitAnswer3",
          disabled = TRUE
        )
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  ## Reset Button ----
  observeEvent(
    eventExpr = input$resetGame,
    handlerExpr = {
      interactedList("start")
      actionPoints(1)
      backpackNew(NULL)
      index(sample(x = nrow(questionBank) / 3, size = 1) * 3 - 2)
      places <- objects$name[which(objects$assignable != "no")]
      places <- sample(places, length(places), replace = FALSE)
      mappings <- roomItems
      for (i in 1:nrow(mappings)) {
        if (mappings$location[i] == "") {
          mappings$location[i] <- places[i]
        }
      }
      mapping(mappings)
      updateButton(
        session = session,
        inputId = "interactObject",
        label = "Interact with object",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "combineItems",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = FALSE
      )
      updateSelectInput(
        session = session,
        inputId = "selectedItems",
        choices = "Collect items first"
      )
    }
  )

  ## Display Elements ----
  ### Display remaining action points ----
  output$actionPointReport <- renderUI({
    paste("You have", actionPoints(), "action point(s) remaining.")
  })

  ### Display backpack contents ----
  output$backpackContents <- renderUI({
    paste(backpackNew(), collapse = ", ")
  })

  ### Code for Re-rendering mathematics ----
  output$trigger1 <- renderUI({withMathJax()})
  output$trigger2 <- renderUI({withMathJax()})

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
