# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(grid)
library(ggplot2)
library(V8)
library(shinyjs)
library(png)

# Load additional dependencies and setup functions ----
source("escapeRoomHelpers.R")
source('coastal-room.R')

# Define constants ----
arbitraryChoices <- list("A", "B", "C", "D")
# background <- png::readPNG('www/oceansideRoom.png')
beachScene <- png::readPNG("www/beach.png")

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
            div(class = "updated", "Last Update: 8/2/2021 by NJH.")
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
              ),
              hr(),
              h3("Old-Ignore"),
              uiOutput("answer", class = "largerFont"),
              bsButton(
                inputId = "interact",
                label = "Interact with object",
                icon = icon("hand-point-up"),
                size = "large",
                style = "danger"
              ),
              hr(),
              uiOutput("activeChance", class = "largerFont"),
              hr(),
              h3("Items in your backpack"),
              uiOutput("backpack", class = "largerFont"),
              br(),
              h4("Combine Items"),
              p('Select items to combine and gain new items.'),
              selectInput(
                inputId = "backpackList",
                label = "Backpack",
                choices = list(
                  "Nothing"),
                multiple = TRUE
              ),
              bsButton(
                inputId = "combine",
                label = "Combine items",
                style = "warning",
                size = "large",
                disabled = FALSE
              ),
              br(),
              h4("Items you've used"),
              uiOutput("usedItems", class = "largerFont"),
              uiOutput("feedback4"),
              hr(),
              bsButton(
                inputId = 'clear',
                label = "Restart",
                style = 'danger',
                size = 'large'
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

  ## Display remaining action points ----
  output$actionPointReport <- renderUI({
    paste("You have", actionPoints(), "action point(s) remaining.")
  })

  ## Display backpack contents ----
  output$backpackContents <- renderUI({
    paste(backpackNew(), collapse = ", ")
  })

# OLD ----
  ## Creation of Items for the Room ----
  ### Draws on the escapeRoomHelpers.R file
  None <- createItem(name = "nothing", description = "found nothing")
  goldenPassword <- createItem(
    name = "goldenPassword",
    description = "password for goldenBox"
  )
  fakePassword <- createItem(
    name = "fakePassword",
    description = "password for nothing"
    )
  copperKey <- createItem(name = "copperKey", description = "key for copperBox")
  fakeKey <- createItem(name = "fakeKey", description = "key for nothing")
  goldenBox <- createItem(name = "goldenBox", description = "a locked box")
  silverBox <- createItem(name = "silverBox", description = "a locked box")
  copperBox <- createItem(name = "copperBox", description = "a locked box")
  mirror <- createItem(name = "mirror", description = "a regular mirror")
  screwdriver <- createItem(
    name = "screwdriver",
    description = "a regular screwdriver"
  )

  ### Combined Items
  EntranceKey <- createItem(
    name = "EntranceKey",
    description = "key for main entrance",
    keys = c("copperBox", "copperKey")
  )
  ElevatorKey <- createItem(
    name = "ElevatorKey",
    description = "key for elevator",
    keys = c("goldenBox", "goldenPassword")
  )

  out <- createItem(
    name = "out",
    description = "Having this item means you successfully escaped from the room."
  )

  ## Creation of Interaction Points in the Scene ----
  ### Draws on the escapeRoomHelpers.R file
  ### Will potentially be replaced with the SVG interaction objects
  sceneNothing <- createScene(
    type = "nothing",
    description = "Nothing is there.",
    name = "nothing",
    item = None
  )
  sceneStart <- createScene(
    type = "nothing",
    description = "You may start.",
    name = "start",
    item = None
  )
  sceneNoChance <- createScene(
    type = "nothing",
    description = "You have no action points left. Answer questions to ear action points.",
    name = "noChance",
    item = None
  )
  sceneElevator <- createScene(
    type = "exit",
    description = "an locked elevator, need a key to use.",
    name = "elebator",
    item = out,
    keys = c("ElevatorKey")
    # exit1
  )
  sceneDoor <- createScene(
    type = "exit",
    description = "main entrance",
    name = "door",
    item = out,
    keys = c("EntranceKey")
  )
  scenePainting <- createScene(
    type = "decoration",
    description = "painting on the wall",
    name = "painting",
    item = fakePassword
    # password for nothing
  )
  sceneDroplight <- createScene(
    type = "light",
    description = "a droplight on the ceiling",
    name = "droplight",
    item = fakeKey
    # key for nothing
  )
  sceneCarpet <- createScene(
    type = "decoration",
    description = "carpet on the floor",
    name = "carpet",
    item = copperKey
    # key for box 3
  )
  sceneTable1 <- createScene(
    type = "table",
    description = "table on the carpet",
    name = "table1",
    item = goldenBox
    # contains ElevatorKey, the key for elevator, need password 1 to open
  )
  sceneTable2 <- createScene(
    type = "table",
    description = "table next to the chair",
    name = "table2",
    item = silverBox
    # useless box
  )
  sceneChair <- createScene(
    type = "chair",
    description = "a chair",
    name = "chair",
    item = copperBox
    # contains EntranceKey, the key for elevator, need copperKey to open
  )
  sceneLamp <- createScene(
    type = "light",
    description = "lamp on the table",
    name = "lamp",
    item = screwdriver
    # useless
  )
  scenePlant <- createScene(
    type = "plant",
    description = "plant in the room",
    name = "plant",
    item = goldenPassword
    # password for goldenBox
  )

  ## Core Lists ----
  ### Draws on the escapeRoomHelpers.R file
  sceneList <- createList(
    sceneElevator, sceneDoor, scenePainting, sceneDroplight, sceneCarpet,
    sceneTable1, sceneTable2, sceneChair, sceneLamp, scenePlant
  )

  itemList <- createList(
    None, goldenPassword, fakePassword, copperKey, fakeKey,
    goldenBox, silverBox, copperBox, mirror, screwdriver,
    EntranceKey, ElevatorKey, out
  )

  backpackNames <- c(
    "None", "goldenPassword", "fakePassword", "copperKey",
    "fakeKey", "goldenBox", "silverBox", "copperBox", "mirror",
    "screwdriver", "EntranceKey", "ElevatorKey", "out"
  ) # Could this be a global constant?

  combineList <- createList(ElevatorKey, EntranceKey)

  combineListNames <- c(
    "copperBox", "copperKey", "EntranceKey", "copperKey",
    "copperBox", "EntranceKey", "goldenBox", "goldenPassword",
    "ElevatorKey", "goldenPassword", "goldenBox", "ElevatorKey"
  )

  ## Scene Mapping ----
  ### Can't be moved to escapeRoomHelpers.R due to the hard coding of scene*
  recognize <- function(x, y){
    if (x > -322 & x < -184 & y > -49 & y < 203) {
      return(sceneElevator)
    } else if (x > 261 & x < 322 & y > -11 & y < 147) {
      return(scenePainting)
    } else if (x > -106.7 & x < 31.5 & y > 199.3 & y < 253.3) {
      return(sceneDroplight)
    } else if (x > -162.6 & x < 64.5 & y > -226.2 & y < -131.9) {
      return(sceneCarpet)
    } else if (x > -97.9 & x < -0.2 & y > -163.8 & y < -93.1) {
      return(sceneTable1)
    } else if (x > 48 & x < 81 & y > 24.67 & y < 52.4) {
      return(sceneTable2)
    } else if (x > 92.4 & x < 128 & y > 23.3 & y < 88.4) {
      return(sceneChair)
    } else if (x > 140.6 & x < 240.88 & y > -133.3 & y < 304.65) {
      return(sceneDoor)
    } else if (x > -321.2 & x < -251.4 & y > -194.3 & y < -120.86) {
      return(sceneLamp)
    } else if (x > -182.9 & x < -161.3 & y > 67.6 & y < 99.5) {
      return(scenePlant)
    } else if (x == 10 & y == 10) {
      return(sceneStart)
    } else if (x == 11 & y == 11) {
      return(sceneNoChance)
    } else {
      return(sceneNothing)
    }
  }

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

  ## Code for Re-rendering mathematics ----
  output$trigger1 <- renderUI({withMathJax()})
  output$trigger2 <- renderUI({withMathJax()})

  ## Creating Reactive Values ----
  ### Needs better naming
  var <- reactiveValues(x = NULL, y = NULL, x2 = NULL, y2 = NULL)
  ### Needs better naming
  index <- reactiveVal(sample(x = nrow(questionBank) / 3, size = 1) * 3 - 2)

  ### What is this for?
  indexLlist <- reactiveValues(listc = 1:nrow(questionBank))

  ## Initialize Player Attributes ----
  player <- reactiveValues(actionPoint = 1)
  backpack <- reactiveValues(
    None = 0,
    goldenPassword = 0,
    fakePassword = 0,
    copperKey = 0,
    fakeKey = 0,
    goldenBox = 0,
    silverBox = 0,
    copperBox = 0,
    mirror = 0,
    screwdriver = 0,
    EntranceKey = 0,
    ElevatorKey = 0,
    out = 0
  )

  ## Initialize Used Items ----
  usedItems <- reactiveValues(
    None = 0,
    goldenPassword = 0,
    fakePassword = 0,
    copperKey = 0,
    fakeKey = 0,
    goldenBox = 0,
    silverBox = 0,
    copperBox = 0,
    mirror = 0,
    screwdriver = 0,
    EntranceKey = 0,
    ElevatorKey = 0,
    out = 0
  )

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
          player$actionPoint <- player$actionPoint + 1
          actionPoint(actionPoint() + 1)
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
          player$actionPoint <- player$actionPoint + 1
          actionPoint(actionPoint() + 1)
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
          player$actionPoint <- player$actionPoint + 1
          actionPoint(actionPoint() + 1)
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

  ## Creating the Scene ----
  ### Create function for plotting the target
  plotTarget <- function(x, y) {
    coordinates <- data.frame(x = x, y = y)
    rg <- grid::rasterGrob(
      image = background,
      width = unit(1,"npc"),
      height = unit(1,"npc")
    )

    plot <- ggplot(data = coordinates, mapping = aes(x = x, y = y)) +
      annotation_custom(rg) +
      geom_point(color = "red", shape = 4, size = 7) +
      geom_point(color = "red", shape = 19, size = 5) +
      scale_x_continuous(
        expand = expansion(mult = 0, add = 0),
        limits = c(-300,300)
        ) +
      scale_y_continuous(
        expand = expansion(mult = 0, add = 0),
        limits = c(-300,300)
      ) +
      theme_void() +
      theme(aspect.ratio = nrow(background)/ncol(background))

    return(plot)
  }

  ## Read the clicked points of the room ----
  observe({
    if (is.null(input$Click)) {
      return()
    } else {
      #Save the coordinates of clicked points as two vectors
      #commented lines are saved for multi-room escape room game
      isolate({
        var$x <- input$Click$x
        var$y <- input$Click$y
      })
    }
  })

  observe({
    if (length(var$x) >= 1) {
      updateButton(
        session = session,
        inputId = "interact",
        value = FALSE,
        disabled = FALSE
      )
    }
  })

  ## I'm unclear as to what these chunks do ----
  #Reset the button to FALSE so that all the conditionalPanel will disappear
  observeEvent(
    eventExpr = input$interact,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "interact",
        value = FALSE,
        disabled = TRUE
      )
    }
  )

  #Reset(clear) the clicked points
  observe({
    if (input$interact == FALSE) {
      var$x <- 10
      var$y <- 10
    }
  })
  observeEvent(
    eventExpr = input$clear,
    handlerExpr = {
      if (input$interact == FALSE) {
        var$x <- 10
        var$y <- 10
      }
    })

  #Set the related relationship between buttons: "interact" "combine", not sure if "clear" is needed
  #"clear" is just in case that when the user wants to restart the game and clear all the data.
  observe({
    if (length(var$x) == 1){
      updateButton(session,"interact", value = FALSE, disabled = FALSE)
    }
  })
  observeEvent(input$interact,{
    updateButton(session,"clear", disabled = FALSE)
  })
  observeEvent(input$combine,{
    updateButton(session,"clear", disabled = FALSE)
  })

  observeEvent(input$combine,{
    updateButton(session, "interact", value = FALSE, disabled = FALSE)
  })
  observeEvent(input$clear,{
    updateButton(session, "interact", value = FALSE, disabled = FALSE)
  })
  observeEvent(input$clear,{
    updateButton(session, "combine",disabled = FALSE)
  })

  ## Scene Interaction Code Chunk ----
  observeEvent(input$interact,{
    current = recognize(var$x, var$y)
    tempName = current$item$name

    if(is.null(player$actionPoint)){
      player = reactiveValues(actionPoint = 10)
    }

    if(is.null(backpack)){
      backpack = list(tempName = 0)
    }

    if(is.null(backpack[[tempName]])){
      backpack[[tempName]] = 0
    }

    if(tempName != "None"){

      if (player$actionPoint < 1){
        var$x = 11
        var$y = 11

      } else if (backpack[[tempName]] == 0 & player$actionPoint > 0 & usedItems[[tempName]] == 0){
        if (is.null(current$keys)){

          backpack[[tempName]] <- 1

          backpack$None <- 0
          player$actionPoint <- player$actionPoint - 1

          activeItems <- c()
          n <- length(backpackNames)

          for (i in 1:n){
            if(is.null(backpack[[backpackNames[i]]])){
              backpack[[backpackNames[i]]] = 0
            } else if(backpack[[backpackNames[i]]] == 1){
              activeItems = append(activeItems, backpackNames[i])
            } else {
            }
          }

          tempString <- paste(tempName, "is added to your backpack!")
          #shinyalert("Item Gained!", tempString, type = "info")

          sendSweetAlert(
            session = session,
            title = "Item Gained!",
            text = tempString,
            type = "info"
          )

          updateSelectInput(session, inputId = "backpackList",
                            label = "Backpack",
                            choices = activeItems)

        } else if (backpack[[current$keys]] == 1){
          backpack[[tempName]] <- 1

          backpack$None <- 0
          player$actionPoint <- player$actionPoint - 1

          activeItems <- c()
          n <- length(backpackNames)

          for (i in 1:n){
            if(is.null(backpack[[backpackNames[i]]])){
              backpack[[backpackNames[i]]] = 0
            } else if(backpack[[backpackNames[i]]] == 1){
              activeItems = append(activeItems, backpackNames[i])
            } else {
            }
          }


          updateSelectInput(session, inputId = "backpackList",
                            label = "Backpack",
                            choices = activeItems)
        } else {
          output$answer <- renderUI({
            p("You need a key or password to interact with this scene.")
          })
        }
      } else {
        backpack[[tempName]] = 1
        backpack$None = 0
        output$answer <- renderUI({
          #h2("You have already activated with this scene.")
          tempString <- "You have already activated with this object."
          sendSweetAlert(
            session = session,
            title = "Whoops!",
            text = tempString,
            type = "error"
          )

        })

      }
    } else {
      output$answer <- renderUI({
        p("Nothing to interact.")
      })
    }
  })


  ## Scene Plot Output ----
  output$target <- renderPlot(
    expr = {
      plotTarget(var$x, var$y)
    },
    width = 600,
    height = 600
  )

  ## Display what's in the backpack ----
  observe({
    # display items in backpack
    activeItems <- c()
    n <- length(backpackNames)
    for (i in 1:n) {
      if (is.null(backpack[[backpackNames[i]]])) {
        backpack[[backpackNames[i]]] <- 0
      } else if (backpack[[backpackNames[i]]] == 1) {
        activeItems <- append(activeItems, backpackNames[i])
      }
    }
    output$backpack <- renderUI({
      paste(activeItems, collapse = ", ")
    })

    # Display Used Items ----
    damagedItems <- c()
    n <- length(backpackNames)
    for (i in 1:n) {
      if (is.null(usedItems[[backpackNames[i]]])) {
        usedItems[[backpackNames[i]]] <- 0
      } else if (usedItems[[backpackNames[i]]] == 1) {
        damagedItems <- append(damagedItems, backpackNames[i])
      }
    }
    output$usedItems <- renderUI({
      paste(damagedItems, collapse = ", ")
    })
  })

  ## Display what the user has clicked on ----
  observe({
    current <- recognize(var$x, var$y)
    output$answer <- renderUI({
      p(current$description)
    })

  })

  ## Display action points ----
  observe({
    output$activeChance <- renderUI({
      p(paste("You have", player$actionPoint, "action point(s) remaining."))
    })
  })

  ## Print feedback ? ----
  observe({
    if (backpack$out == 0) {
      output$feedback4 <- renderUI({
        # Why is there a sweet alert inside of an output?
        # sendSweetAlert(
        #   session = session,
        #   title = "Hint!",
        #   text = "Find Clues to escape the room!\nMaximize the window for perfect Experience~",
        #   type = "info"
        # )

      })
    } else {
      output$feedback4 <- renderUI({
        paste("Successfully Escaped!")
      })

      sendSweetAlert(
        session = session,
        title = "Congratulations!",
        text = "You successfully escaped the Room! Enjoy the beach!",
        type = "success"
      )

      output$target <- renderPlot(
        expr = {
          plotFreedom(beachScene)
      },
      alt = "You've escaped the room; enjoy the beach!",
      height = 600,
      width = 600
      )
    }
  })

  ## Combining items ----
  observeEvent(input$combine, {
    items <- input$backpackList
    n <- floor(length(combineListNames)/3)
    if (length(items) == 2) {
      for (i in 1:n) {
        if (combineListNames[3 * n - 2] == items[1] & combineListNames[3 * n - 1] == items[2]) {
          backpack[[combineListNames[3 * n]]] <- 1
          backpack[[combineListNames[3 * n - 2]]] <- 0
          backpack[[combineListNames[3 * n - 1]]] <- 0
          usedItems[[items[1]]] <- 1
          usedItems[[items[2]]] <- 1
          tempName = combineListNames[3 * n]

          tempString <- paste(tempName, "is added to your backpack!")
          #shinyalert("Item Gained!", tempString, type = "info")
          sendSweetAlert(
            session = session,
            title = "Item Gained!",
            text = tempString,
            type = "info"
          )
        }
        else {
          tempString <- "This two items cannot be combined."
          sendSweetAlert(
            session = session,
            title = "Whoops",
            text = tempString,
            type = "error"
          )
        }
      }

      activeItems <- c()
      n <- length(backpackNames)

      for (i in 1:n) {
        if (is.null(backpack[[backpackNames[i]]])) {
          backpack[[backpackNames[i]]] <- 0
        } else if (backpack[[backpackNames[i]]] == 1) {
          activeItems <- append(activeItems, backpackNames[i])
        } else {
          # What's supposed to go here?
        }
      }

      updateSelectInput(
        session = session,
        inputId = "backpackList",
        label = "Backpack",
        choices = activeItems
      )

    } else {
      sendSweetAlert(
        session = session,
        title = "Whoops!",
        text = "You have to combine exactly two items that
          aren't able to be combined.",
        type = "error"
      )
    }
  })

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
