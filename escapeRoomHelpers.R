# Helper functions for the Escape Room

## Item and "Scene" Creation and Management ----
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
  for (j in 1:m) {
    for (i in 1:n) {
      if (item$keys[j] == backpack[i]) {
        count = count - 1
        coordinate = append(coordinate, i)
      }
    }
  }
  if (count == 0) {
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
  for (i in 1:n) {
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
  for (i in 1:n) {
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
  for (i in 1:n) {
    l = append(l, list[[i]]$name)
  }
  return(l)
}

## Display the Freedom Scene ----
plotFreedom <- function(freeImg) {
  # freeImg should be the image associated with escaping the room
  par(mar = c(1,1,1,1))
  plot(
    x = c(-300, 300),
    y = c(-300, 300),
    type = "n",
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  rasterImage(
    image = freeImg,
    xleft = -300,
    xright = 300,
    ybottom = -300,
    ytop = 300
  )
}
