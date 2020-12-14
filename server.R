# library(shiny)
# library(png)
# library(shinyBS)
# library(shinyjs)
# library(V8)
# library(shinydashboard)
# library(shinyWidgets)
# library(grid)
# library(ggplot2)
# library(shinyalert)






shinyServer(function(input, output,session) {
  
  # Read in Question Bank ----
  bank <- read.csv(file = "questionbank.csv", stringsAsFactors = FALSE)
  
  ###########Logic part########################################################################
  
  # IMPORTANT NOTES ABOUT SCOPE
  # for now the scope of the following functions and objects are too wide, 
  # It supposed to be shrinked to this game only, which means I will try to put them into the ShinyServer function
  # Not tested yet, don't know if any error will caused by this, but the current version runs well for single user/locally
  
  
  # Define functions for escape room structures
  
  createItem <- function(name, description = "", keys = c()){
    temp <- list(name, description, keys)
    names(temp) <- c("name", "description", "keys")
    return(temp)
  }
  
  makeItem <- function(item, backpack = backpack){
    m = length(item$keys)
    count = m
    n = length(backpack)
    coordinate = c()
    for (j in 1:m){
      for (i in 1:n){
        if (item$keys[j] == backpack[i]){
          count = count - 1
          coordinate = append(coordinate, i)
        }
      }
    }
    if (count == 0){
      n = length(backpack)
      temp = backpack
      paste("Successfully created item!")
      temp[n + 1] = item
      names(temp)[n + 1] = item$name
      temp = temp[-coordinate]
      return(temp)
    } else {
      paste("Not enough materials in backpack.")
      return(backpack)
    }
  }
  
  createList <- function(...){
    argument = list(...)
    temp = argument
    n = length(argument)
    name = c()
    for (i in 1:n){
      name = append(name, temp[[i]]$name)
    }
    names(temp) = name
    return(temp)
  }
  
  createListName <- function(...){
    argument = list(...)
    temp = argument
    n = length(argument)
    name = c()
    for (i in 1:n){
      name = append(name, temp[[i]]$name)
    }
    names(temp) = name
    return(name)
  }
  
  
  
  
  createScene <- function(type, description, name, item = None, keys = c(), state = 0){
    temp <- list(name, type, description, item, keys, state)
    names(temp) <- c("name", "type", "description", "item", "keys", "state")
    return(temp)
  }
  
  
  createPlayer <- function(name, backpack, activeChance = 0){
    temp = list(name, backpack, activeChance)
    names(temp) = c("name", "backpack", "activeChance")
  }
  
  
  listNames <- function(list){
    l = c()
    n = length(list)
    for (i in 1:n){
      l = append(l, list[[i]]$name)
    }
    return(l)
  }
  
  
  # Create items
  None <- createItem(name = "nothing", description = "found nothing")
  
  goldenPassword <- createItem(name = "goldenPassword", description = "password for goldenBox")
  fakePassword <- createItem(name = "fakePassword", description = "password for nothing")
  copperKey <- createItem(name = "copperKey", description = "key for copperBox")
  fakeKey <- createItem(name = "fakeKey", description = "key for nothing")
  goldenBox <- createItem(name = "goldenBox", description = "a locked box")
  silverBox <- createItem(name = "silverBox", description = "a locked box")
  copperBox <- createItem(name = "copperBox", description = "a locked box")
  
  mirror <- createItem(name = "mirror", description = "a regular mirror")
  screwdriver <- createItem(name = "screwdriver", description = "a regular screwdriver")
  
  
  # Combined Itmes
  EntranceKey <- createItem(name = "EntranceKey", description = "key for main entrance", keys = c("copperBox", "copperKey"))
  ElevatorKey <- createItem(name = "ElevatorKey", description = "key for elevator", keys = c("goldenBox", "goldenPassword"))
  out <- createItem(name = "out", description = "having this item means you successfully escaped from the room")
  
  
  # Create scenes
  
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
    description = "You have no Action Point left. Answer questions to gain Action Point",
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
  
  
  # Create lists that contains elements needed for the escape room
  sceneList <- createList(sceneElevator, sceneDoor, scenePainting, sceneDroplight, sceneCarpet,
                          sceneTable1, sceneTable2, sceneChair, sceneLamp, scenePlant)
  
  itemList <- createList(None, goldenPassword, fakePassword, copperKey, fakeKey, goldenBox, silverBox, copperBox, mirror, screwdriver, EntranceKey, ElevatorKey, out)
  
  backpackNames <- c("None", "goldenPassword", "fakePassword", "copperKey", "fakeKey", "goldenBox", "silverBox", "copperBox", "mirror", "screwdriver", "EntranceKey", "ElevatorKey", "out")
  
  combineList <- createList(ElevatorKey, EntranceKey)
  
  combineListNames <- c("copperBox", "copperKey", "EntranceKey", "copperKey" ,"copperBox", "EntranceKey", "goldenBox", "goldenPassword", "ElevatorKey", "goldenPassword", "goldenBox", "ElevatorKey")
  
  recognize <- function(x, y){
    if (x > -322 & x < -184 & y > -49 & y < 203){
      return(sceneElevator)
    } else if(x > 261 & x < 322 & y > -11 & y < 147){
      return(scenePainting)
    } else if(x > -106.7 & x < 31.5 & y > 199.3 & y < 253.3){
      return(sceneDroplight)
    } else if(x > -162.6 & x < 64.5 & y > -226.2 & y < -131.9){
      return(sceneCarpet)
    } else if(x > -97.9 & x < -0.2 & y > -163.8 & y < -93.1){
      return(sceneTable1)
    } else if(x > 48 & x < 81 & y > 24.67 & y < 52.4){
      return(sceneTable2)
    } else if(x > 92.4 & x < 128 & y > 23.3 & y < 88.4){
      return(sceneChair)
    } else if(x > 140.6 & x < 240.88 & y > -133.3 & y < 304.65){
      return(sceneDoor)
    } else if(x > -321.2 & x < -251.4& y > -194.3 & y < -120.86){
      return(sceneLamp)
    } else if(x > -182.9 & x < -161.3 & y > 67.6 & y < 99.5){
      return(scenePlant)
    } else if (x == 10 & y == 10){
      return(sceneStart)
    } else if (x == 11 & y == 11){
      return(sceneNoChance)
    } else{
      return(sceneNothing)
    }
  }
  
  # function that prepared for adding items to backpack, but I found a replace solution for that.
  # addItem <- function(item, backpack. = backpack){
  #   backpack[[item$name]] = item
  #   return(backpack)
  # }
  
  ###########Logic part######################################################################
  
  
  
  
  
  
  
  
  
  
  # Initializing a game progress object
  gameInProgress <- FALSE

  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click the door and hit interactive.",
      type = "info"
    )
  })

  ###UPDATE: adding the go button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "game")
  })

  observeEvent(input$start, {
    updateTabItems(session, "tabs", "instruction")
  })

  # Retrigger MathJax processing
  output$trigger1 <- renderUI({
    withMathJax()
  })
  output$trigger2 <- renderUI({
    withMathJax()
  })


  # Create reactiveValues to restore the coordinates of clicked points and the
  # calculated bias and reliability # Ummm, please update comments
  var <- reactiveValues(x = NULL, y = NULL, x2 = NULL, y2 = NULL)

  # Please see prior comments about commentd out code
  # backpackNames <- c("None", "goldenPassword", "fakePassword", "copperKey", "fakeKey", "goldenBox",
  # "silverBox", "copperBox", "mirror", "screwdriver", "EntranceKey", "ElevatorKey", "out")

  # Randomly generate one question every time when "Next Question" button is clicked.
  # When I look at the question bank, there is one context (alexis, james,...)
  # which only has TWO rows, which will throw off what you're attempting to do
  index <- reactiveValues(
    index = sample(dim(bank)[1]/3, 1)*3 - 2 # dim is not appropriate here
    ## why not add an index column to the bank?
    ## you could then use sample(length(unique(bank$index)), 1) and you wouldn't
    ## need the mathematical transformation.
    #listc = 1:dim(bank)[1]
    # restind = listc[listc != index]
    )
  # Generally speaking, it isn't a good idea to 1) use the same name for different
  # objects, and 2) to nest the same names.

  # You could create a single reactiveValues object and add many of these objects
  # to this single one.

  # The code that is 2 lines below is a backup plan if the next line doesn't work, where the next line is located around line 465 or earlier.
  ###### qIndexList <- sample(length(unique(bank$ids)), n = length(unique(bank$)), replace = FALSE)
  indexLlist <- reactiveValues(listc=1:dim(bank)[1])

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
  
  observeEvent(input$clear,{
    
    # sendSweetAlert(
    #   session = session,
    #   title = "Congratulations!",
    #   text = "You Successfully Escaped the Room! Enjoy the beach~",
    #   type = "success"
    # )
    
    #shinyalert("Congratulations!", "You Successfully Escaped the Room! Enjoy the beach~", type = "success")
    
    # This part is saved for 'clear' button, which is suppose to restart the game, 
    # But I am still thinking if this is necessary
  })

  
  ## Neil helped me to fix some issues that I couldn't handle, and here is his comment:
  # Neil's Test Area for Updating Scenarios and Question Text ----
  # The idea here is to use the gameInProgress object in addition to whether
  # the person has moved to the game tab to update the questions/choices the
  # very first time. The gameInProgress object will prevent the questions from
  # changing any time the person switches out of and then back into the game tab.

  ## Initialize Scenario, Questions, and Choices First Time
  observeEvent(input$tabs, {
    if (input$tabs == "game"){
      if (gameInProgress == FALSE){
        ## Display Scenario
        output$scenario <- renderUI({
          p(bank[index$index, "scenario"]) # Use column names rather than current position
        })
        ## Question 1
        output$question1 <- renderUI({
          withMathJax(bank[index$index, "question"])
        })
        ### Update Choices 1
        updateRadioGroupButtons(
          session = session,
          inputId = "answersQ1",
          choices = list(
            bank[index$index, "A"],
            bank[index$index, "B"],
            bank[index$index, "C"],
            bank[index$index, "D"]
          ),
          selected = character(0),
          status = "game",
          checkIcon = list(yes = icon("check-square"),
                           no = icon("square-o"))
        )
        ## Question 2
        output$question2 <- renderUI({
          withMathJax(bank[index$index + 1, "question"])
        })
        ### Update Choices 2
        updateRadioGroupButtons(
          session = session,
          inputId = "answersQ2",
          choices = list(
            bank[index$index + 1, "A"],
            bank[index$index + 1, "B"],
            bank[index$index + 1, "C"],
            bank[index$index + 1, "D"]
          ),
          selected = character(0),
          status = "game",
          checkIcon = list(yes = icon("check-square"),
                           no = icon("square-o"))
        )
        ## Question 3
        output$question3 <- renderUI({
          withMathJax(bank[index$index + 2, "question"])
        })
        ### Update Choices 3
        updateRadioGroupButtons(
          session = session,
          inputId = "answersQ3",
          choices = list(
            bank[index$index + 2, "A"],
            bank[index$index + 2, "B"],
            bank[index$index + 2, "C"],
            bank[index$index + 2, "D"]
          ),
          selected = character(0),
          status = "game",
          checkIcon = list(yes = icon("check-square"),
                           no = icon("square-o"))
        )
        gameInProgress <- TRUE
      }}
  })

  ## Update Scenario, Questions, and Choices with Next Question Button
  observeEvent(input$nextQuestion, {
  
    ## the next line is supposed to be added to make sure no repeated questions will be shown. but I didn't test it yet.
    # qIndexList <- sample(length(unique(bank$ids)), n = length(unique(bank$)), replace = FALSE)

    index$index <- sample(dim(bank)[1]/3, 1)*3 - 2

    output$scenario <- renderUI({
      p(bank[index$index, "scenario"]) # Use column names rather than current position
    })
    ## Question 1
    output$question1 <- renderUI({
      withMathJax(bank[index$index, "question"])
    })
    ### Update Choices 1
    updateRadioGroupButtons(
      session = session,
      inputId = "answersQ1",
      choices = list(
        bank[index$index, "A"],
        bank[index$index, "B"],
        bank[index$index, "C"],
        bank[index$index, "D"]
      ),
      selected = character(0),
      status = "game",
      checkIcon = list(yes = icon("check-square"),
                       no = icon("square-o"))
    )
    ## Question 2
    output$question2 <- renderUI({
      withMathJax(bank[index$index + 1, "question"])
    })
    ### Update Choices 2
    updateRadioGroupButtons(
      session = session,
      inputId = "answersQ2",
      choices = list(
        bank[index$index + 1, "A"],
        bank[index$index + 1, "B"],
        bank[index$index + 1, "C"],
        bank[index$index + 1, "D"]
      ),
      selected = character(0),
      status = "game",
      checkIcon = list(yes = icon("check-square"),
                       no = icon("square-o"))
    )
    ## Question 3
    output$question3 <- renderUI({
      withMathJax(bank[index$index + 2, "question"])
    })
    ### Update Choices 3
    updateRadioGroupButtons(
      session = session,
      inputId = "answersQ3",
      choices = list(
        bank[index$index + 2, "A"],
        bank[index$index + 2, "B"],
        bank[index$index + 2, "C"],
        bank[index$index + 2, "D"]
      ),
      selected = character(0),
      status = "game",
      checkIcon = list(yes = icon("check-square"),
                       no = icon("square-o"))
    )
  })


  # End of Neil's Test Area ----

  observeEvent(input$submitAnswer1, {
    ans1 = bank[index$index, "answer"] # See prior comments
    if (input$answersQ1 == ans1){
      player$actionPoint = player$actionPoint + 1 # Use <- not = here
      updateButton(
        session = session,
        inputId = "submitAnswer1",
        label = "Submit Answer",
        style = "danger",
        size = "small",
        disabled = TRUE
      ) # You only need to include the elements you want to change beyond
      ## session and inputId.
      output$questionFeedback1 <- renderUI({
        img(src = "check.png", width = 30, alt = "Correct") # Don't forget alt text
        #p("Correct, reactice chance +1!")
      })
    } else if (input$answersQ1 == "--Select Your Answer--"){
      output$questionFeedback1 <- renderUI({
        p("Make your choice.")
      })
    } else {
      output$questionFeedback1 <- renderUI({
        img(src = "cross.png", width = 30, alt = "Incorrect") # Don't forget alt text
        #p("Incorrect, try again?")
      })
    }
 })

## codes after this line might be improved by using methods include but not limited to:
  # use groupradiobutton, follow style guide, etc.
#####################################################
  observeEvent(input$submitAnswer2,{
    ans2 = bank[index$index+1, 7]
    if (input$answersQ2 == ans2){
      player$actionPoint = player$actionPoint + 1
      updateButton(session,"submitAnswer2",label = "Submit Answer",style = "danger", size = "small", disabled = TRUE)
      output$questionFeedback2 <- renderUI({
        img(src = "check.png",width = 30)
        #p("Correct, reactice chance +1!")
      })
    } else if (input$answersQ2 == "--Select Your Answer--"){
      output$questionFeedback2 <- renderUI({
        p("Make your choice.")
      })
    } else {
      output$questionFeedback2 <- renderUI({
        img(src = "cross.png",width = 30)
        #p("Incorrect, try again?")
      })
    }
  })
#####################################################
  observeEvent(input$submitAnswer3,{
    ans3 = bank[index$index+2, 7]
    if (input$answersQ3 == ans3){
      player$actionPoint = player$actionPoint + 1
      updateButton(session,"submitAnswer3",label = "Submit Answer",style = "danger", size = "small", disabled = TRUE)
      output$questionFeedback3 <- renderUI({
        img(src = "check.png",width = 30)
      })
    } else if (input$answersQ3 == "--Select Your Answer--"){
      output$questionFeedback3 <- renderUI({
        p("Make your choice.")
      })
    } else {
      output$questionFeedback3 <- renderUI({
        img(src = "cross.png",width = 30)
      })
    }
  })

## Testing Turning off this code ----
## Actually it works well when this part is turned off, but I kept this in case 
  
  # output$question1 <- renderUI({
  #   print(input$tabs)
  #   updateRadioButtons(
  #     session = session,
  #     inputId = "choice1",
  #     inline = FALSE,
  #     choices = list(
  #       bank[index$index, "A"],
  #       bank[index$index, "B"],
  #       bank[index$index, "C"],
  #       bank[index$index, "D"],
  #       "test"
  #     ), selected = character(0)
  #   ) # By putting the update here with the question render, this should
  #   # guarantee that the choices and questions render together
  #   withMathJax(bank[index$index, "question"])
  #   print("test")
  # })

  # output$question2 <- renderUI({
  #   updateRadioButtons(
  #     session = session,
  #     inputId = "choice2",
  #     inline = FALSE,
  #     choices = list(
  #       bank[index$index + 1, "A"],
  #       bank[index$index + 1, "B"],
  #       bank[index$index + 1, "C"],
  #       bank[index$index + 1, "D"]
  #     ), selected = character(0)
  #   )
  #   withMathJax(bank[index$index + 1, "question"])
  # })

  # output$question3 <- renderUI({
  #   updateRadioButtons(
  #     #updateSelectInput(
  #     session = session,
  #     inputId = "choice3",
  #     choices = list(
  #       bank[index$index + 2, "A"],
  #       bank[index$index + 2, "B"],
  #       bank[index$index + 2, "C"],
  #       bank[index$index + 2, "D"]
  #     ), selected = character(0)
  #   )
  #   withMathJax(bank[index$index + 2, "question"])
  # })

  # observeEvent(input$nextQuestion,{
  #   index$index <- sample(dim(bank)[1]/3, 1)*3 - 2
  #
  #   ### Updating Questions ----
  #   output$question1 <- renderUI({
  #     question1 = bank[index$index,2]
  #     withMathJax(question1)
  #   })
  #
  #   output$question2 <- renderUI({
  #     question2 = bank[index$index+1,2]
  #     withMathJax(question2)
  #   })
  #
  #   output$question3 <- renderUI({
  #     question3 = bank[index$index+2,2]
  #     withMathJax(question3)
  #   })
  #
  #
  #   updateButton(session,"submitAnswer1",label = "Submit Answer",style = "danger", size = "small", disabled = FALSE)
  #   output$questionFeedback1 <- renderUI({})
  #   updateButton(session,"submitAnswer2",label = "Submit Answer",style = "danger", size = "small", disabled = FALSE)
  #   output$questionFeedback2 <- renderUI({})
  #   updateButton(session,"submitAnswer3",label = "Submit Answer",style = "danger", size = "small", disabled = FALSE)
  #   output$questionFeedback3 <- renderUI({})
  #
  #  })


# Testing Turning off This Code ----
  # observe({
  #   currentChoices1 <- list(
  #                      bank[index$index, 3],
  #                      bank[index$index, 4],
  #                      bank[index$index, 5],
  #                      bank[index$index, 6]
  #                     )
  #   updateRadioButtons(session = session, inputId = "choice1", choices = currentChoices1, selected = character(0), inline = FALSE)
  # })
  #
  # observe({
  #   currentChoices2 <- list(
  #                       bank[index$index+1, 3],
  #                       bank[index$index+1, 4],
  #                       bank[index$index+1, 5],
  #                       bank[index$index+1, 6]
  #                     )
  #   updateRadioButtons(session = session, inputId = "choice2", choices = currentChoices2, selected = character(0), inline = FALSE)
  # })
  #
  # observe({
  #   currentChoices3 <- list(
  #                       bank[index$index+2, 3],
  #                       bank[index$index+2, 4],
  #                       bank[index$index+2, 5],
  #                       bank[index$index+2, 6]
  #                     )
  #   updateRadioButtons(
  #   #updateSelectInput(
  #     session = session, inputId = "choice3", choices = currentChoices3, selected = character(0))
  #
  # })



#############################################################################################################
  # Original COde
  #Create function for ploting the target
  # plotTarget = function(x,y){
  #   #Get image
  #   isolate(ima <- readPNG("Room6.png"))
  # 
  #   isolate(plot(
  #     x=-300:300,ylim=c(-300,300),xlim=c(-300,300),type='p',xlab = '',ylab = ''))
  # 
  #   #Get the plot information so the image will fill the plot box, and draw it
  #   isolate(lim <- par())
  #   isolate(rasterImage(ima
  #                       , lim$usr[1]
  #                       , lim$usr[3]
  #                       , lim$usr[2]
  #                       , lim$usr[4]
  #                       ))
  #   lines(x,y, xlim = c(-300,300), ylim = c(-300,300),type = 'p', pch = 13, cex = 4, col = "red")
  # }

  ##############################################################################################################
  # Testing area on using ggplot to plot
  #Create function for ploting the target
  plotTarget <- function(x,y){
    coordinate <- data.frame(x = x, y = y)
    background <- png::readPNG('Room6.png')
    rg <- grid::rasterGrob(background, width=unit(1,"npc"), height=unit(1,"npc"))
    
    ggplot(coordinate, aes(x,y)) + 
      annotation_custom(rg) +
      geom_point(colour="red", type = 'p', pch = 13, cex = 4,) +
      scale_x_continuous(expand=c(0,0), lim=c(-300,300)) +
      scale_y_continuous(expand=c(0,0), lim=c(-300,300)) +
      theme_void() +
      theme(aspect.ratio = nrow(background)/ncol(background))
  }
  
  
  
  ############################################################################################################
  # The following codes are saved for multi-room escape room game. 
  # to be specific, they are functions that is going to display the other views of the room or other rooms.
  
  plotTarget2 = function(){
    #Get image
    isolate(ima <- readPNG("beach.png"))

    #Set up the plot area
    #isolate(plot(x=-15:15,ylim=c(-12,12),xlim=c(-15,15),type='p',xlab = '',ylab = ''))
    isolate(plot(x=-300:300,ylim=c(-300,300),xlim=c(-300,300),type='p',xlab = '',ylab = ''))


    #Get the plot information so the image will fill the plot box, and draw it
    isolate(lim <- par())
    isolate(rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]))
    #lines(x,y, xlim = c(-15,15), ylim = c(-12,12),type = 'p', pch = 20)
  }

  # plotTarget3 = function(x,y){
  #   #Get image
  #   isolate(ima <- readPNG("Room6.png"))
  #
  #   #Set up the plot area
  #   #isolate(plot(x=-15:15,ylim=c(-12,12),xlim=c(-15,15),type='p',xlab = '',ylab = ''))
  #   isolate(plot(x=-300:300,ylim=c(-300,300),xlim=c(-300,300),type='p',xlab = '',ylab = ''))
  #
  #
  #   #Get the plot information so the image will fill the plot box, and draw it
  #   isolate(lim <- par())
  #   isolate(rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]))
  #   lines(x,y, xlim = c(-15,15), ylim = c(-12,12),type = 'p', pch = 20)
  # }

  #Save the clicked points or clear all.
  observe({
    # Initially will be empty
    if (is.null(input$Click)
        #& is.null(input$Click2)
        ){
      return()
    }
    #Save the coordinates of clicked points as two vectors
    #commented lines are saved for multi-room escape room game
    isolate({
      #var$x <- input$Click2$x
      #var$y <- input$Click2$y
      var$x <- input$Click$x
      var$y <- input$Click$y
      #var$x2 <- input$Click2$x
      #var$y2 <- input$Click2$y
    })
  })

  observe({
    if (length(var$x) >= 1){
      updateButton(session,"interact",label = "Interact",style = "danger", size = "large", value = FALSE, disabled = FALSE)
    }
  })

  #Reset the button to FALSE so that all the conditionalPanel will disappear
  observeEvent(input$interact,{{
    updateButton(session,"interact",label = "Interact",style = "danger",size = "large", value = FALSE, disabled = T)
  }})
  #Reset(clear) the clicked points
  observe({
    if (input$interact == FALSE){
      var$x <- 10
      var$y <- 10
    }
  })
  observeEvent(input$clear,{
    if (input$interact == FALSE){
      var$x <- 10
      var$y <- 10
    }
  })

  #Set the related relationship between buttons: "interact" "combine", not sure if "clear" is needed 
  #"clear" is just in case that when the user wants to restart the game and clear all the data. 
  observe({
    if (length(var$x) == 1){
      updateButton(session,"interact", label = "Interact", value = FALSE, disabled = FALSE)
    }
  })
  observeEvent(input$interact,{
    updateButton(session,"clear", disabled = FALSE)
  })
  observeEvent(input$combine,{
    updateButton(session,"clear", disabled = FALSE)
  })

  observeEvent(input$combine,{
    updateButton(session, "interact", label = "Interact",value = FALSE, disabled = FALSE)
  })
  observeEvent(input$clear,{
    updateButton(session, "interact", label = "Interact",value = FALSE, disabled = FALSE)
  })
  observeEvent(input$clear,{
    updateButton(session, "combine",disabled = FALSE)
  })


############################################################################################
  # The following codes are prepared for multi-room escaperoom
  #The "next" button will be enabled once the user hits interact.
  #observe(priority = 1,{
  #  if (input$interact== FALSE){
  #    updateButton(session,"combine", label="Next>>", disabled = TRUE)
  #  }
  #  else{updateButton(session,"combine", label="Next>>", disabled = FALSE)}
  #})

  #observe(priority = 2,{
  #  if (length(indexLlist$listc)==1){
  #    updateButton(session,"combine", label="Next>>", disabled = TRUE)
  #  }
  #})

  #output$plota <- renderPlot({
  #  plotA(var$x,var$y)
  #},height = 320, width = 320)

  #output$plotb <- renderPlot({
  #  plotB(var$x,var$y)
  #},height = 320, width = 320)


###########################################################################################################
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
              h2("You need a key or password to interact with this scene.")
            })
        }
      } else {
        backpack[[tempName]] = 1
        backpack$None = 0
        output$answer <- renderUI({
          #h2("You have already activated with this scene.")
          tempString <- "You have already activated with this object."
          
          #shinyalert("Woops!", tempString, type = "error")
          
          sendSweetAlert(
            session = session,
            title = "Woops!",
            text = tempString,
            type = "error"
          )
          
        })

      }
    } else {
      output$answer <- renderUI({
        h2("Nothing to interact.")
      })
    }
  })

############################################################################################################
  # The following codes are prepared for multi-room escaperoom
  ##Plot three outputs using the functions defined before
 # output$target <- renderPlot({
#    n = length(var$x)
#    xx = var$x[n]
#    yy = var$y[n]
#    plotTarget(xx,yy)
#  },height = 320, width = 320)
#
  # output$answer <- renderUI({
  #  n = length(var$x)
  #  xx = var$x[n]
  #  yy = var$y[n]
  #  #if (xx > 0.8 & xx < 1.5 & yy > -1.4 & yy < 0.4){
  #  if (xx > 4 & xx < 7.5 & yy > -7 & yy < 2){
  #    print("Successfully Escaped!")
  #  } else if (xx > -4 & xx < 0 & yy > -1 & yy < 4){
  #    print("It is just a window.")
  #  } else {
  #    print("It is not the door!")
  #  }
  # })

  # output$target2 <- renderPlot({
  #   plotTarget3(var$x2, var$y2)
  # }, height = 320, width = 320)
############################################################################################################
  


  output$target <- renderPlot({
    plotTarget(var$x, var$y)
  }
  , height = 900, width = 900
  )
  

  observe({
    # display items in backpack
    activeItems = c()
    n = length(backpackNames)
    for (i in 1:n){
      if(is.null(backpack[[backpackNames[i]]])){
        backpack[[backpackNames[i]]] = 0
      } else if(backpack[[backpackNames[i]]] == 1){
        activeItems = append(activeItems, backpackNames[i])
      }
    }
    output$backpack <- renderUI({
      paste(activeItems, collapse = "\n")
    })
    
    # display usedItems
    damagedItems = c()
    n = length(backpackNames)
    for (i in 1:n){
      if(is.null(usedItems[[backpackNames[i]]])){
        usedItems[[backpackNames[i]]] = 0
      } else if(usedItems[[backpackNames[i]]] == 1){
        damagedItems = append(damagedItems, backpackNames[i])
      }
    }
    output$usedItems <- renderUI({
      paste(damagedItems, collapse = "\n")
    })
  })

  
  
  observe({
    current = recognize(var$x, var$y)
    output$answer <- renderUI({
      h2(current$description)
    })

  })

  observe({
    output$activeChance <- renderUI({
      p(player$actionPoint)
    })
  })

  ##Print feedbacks.
  observe({
    if(backpack$out == 0){
      output$feedback4 <- renderUI({
        #paste("Find Clues to escape the room!")
        #shinyalert("Hint", "Find Clues to escape the room!", type = "info")
        
        sendSweetAlert(
          session = session,
          title = "Hint!",
          text = "Find Clues to escape the room!",
          type = "info"
        )
        
      })
    } else {
      output$feedback4 <- renderUI({
        paste("Successfully Escaped!")
      })
      
      sendSweetAlert(
        session = session,
        title = "Congratulations!",
        text = "You Successfully Escaped the Room! Enjoy the beach~",
        type = "success"
      )
      
      #shinyalert("Congratulations!", "You Successfully Escaped the Room! Enjoy the beach~", type = "success")
      
      output$target <- renderPlot({
        plotTarget2()
      }
      , height = 900, width = 900
      )
    }
  })


  observeEvent(input$combine, {
    items = input$backpackList
    n = floor(length(combineListNames)/3)
    if (length(items) == 2){
      for (i in 1:n){
        if (combineListNames[3*n-2] == items[1] & combineListNames[3*n-1] == items[2]){
          backpack[[combineListNames[3*n]]] = 1
          backpack[[combineListNames[3*n-2]]] = 0 
          backpack[[combineListNames[3*n-1]]] = 0
          usedItems[[items[1]]] = 1
          usedItems[[items[2]]] = 1
          tempName = combineListNames[3*n]
          
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
            title = "Woops",
            text = tempString,
            type = "error"
          )
        }
      }

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
        #h2("You have to combine exactly two items that are able to be combined.")
        tempString <- "You have to combine exactly two items that are able to be combined."
        #shinyalert("Woops!", tempString, type = "error")
        
        sendSweetAlert(
          session = session,
          title = "Woops!",
          text = tempString,
          type = "error"
        )
        
      })
    }
  })

})



