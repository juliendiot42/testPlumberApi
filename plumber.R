# Copyright 2019 The University of Tokyo
# Author: Julien Diot juliendiot@ut-biomet.org
#
# This file the source code of an example of a Plumber API.

##### Packages #####

library(plumber)
library(plotly)
library(DT)
library(XML)
library(rmarkdown)
require(caret)


##### Options #####

options(stringsAsFactors = FALSE)



##### API description #####

#* @apiTitle Plumber Example API
#* @apiDescription API example for the Plumber tutorial
#* @apiVersion 1.0.42
#* @apiTag default_example Endpoints of the default plumber.R example
#* @apiTag outputs Endpoints to present differents outputs
#* @apiTag message_Board Endpoints for the "message board" API
#* @apiTag Rpower Embedded the power of R in your API
#* @apiContact @email juliendiot@ut-biomet.org


############################ Default plumber example ###########################

#* Echo back the input
#* @tag default_example
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  # list(msg = paste0("The message is: '", msg, "'"))
  paste0("The message is: '", msg, "'")
}

#* Plot a histogram
#* @tag default_example
#* @png
#* @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @tag default_example
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b){
  as.numeric(a) + as.numeric(b)
}



############################# Our custom functions #############################


#* Return the time of the API's server
#* @get /time
function(){
  list(time = Sys.time(),
       timezone = Sys.timezone())
}



#* Return square root of x
#* @param x a number
#* @get /sqrt
function(x, res){
  if (missing(x)) {
    res$status <- 400 # Bad Request
    return(list(error = "x must be specified."))
  }

  x <- as.numeric(x)
  if (is.na(x) | x < 0) {
    res$status <- 400 # Bad Request
    return(list(error = "x must be positive number."))
  }

  sqrt(x)
}


#* Return subset of Iris data
#* @get /iris/<spec>
function(spec) {
  subset(iris, Species == spec)
}

############################## Differents outputs ##############################

#* JSON output
#* @tag outputs
#* @get /iris
#* @json
function(spec) {
  iris
}

#* HTML output
#* @tag outputs
#* @get /html
#* @html
function(){
  "<html><h1>Some HTML code:</h1><p>Hello world!</p></html>"
}

#* jpeg output
#* @tag outputs
#* @get /jpeg
#* @jpeg (width = 500, height = 500)
function(){
  curve(dnorm,-5,5)
}

#* png output
#* @tag outputs
#* @get /png
#* @png (width = 300, height = 300)
function(){
  curve(dnorm,-5,5)
}

#* plotly (htmlwidget) output
#* @tag outputs
#* @get /plotly
#* @serializer htmlwidget
function(){
  x <- seq(-5, 5, length.out = 500)
  d <- dnorm(x)

  plot_ly(
    type = 'scatter',
    mode = 'lines',
    name = paste("N(0;1)"),
    x = x,
    y = d,
    text = paste("f(X) =", round(d, 3),
                 "\nX =", round(x, 3)),
    hoverinfo = 'text'
  )
}

#* datatable (htmlwidget) output
#* @tag outputs
#* @get /datatable
#* @serializer htmlwidget
function(){
  datatable(iris)
}

#* pdf output
#* @tag outputs
#* @get /pdf
#* @serializer contentType list(type="application/pdf")
function(){
  tmp <- tempfile()
  pdf(tmp)
  plot(1:10, type = "b")
  text(4, 8, "PDF from plumber!")
  text(6, 2, paste("The time is", Sys.time()))
  dev.off()

  readBin(tmp, "raw", n = file.info(tmp)$size)
}



#* Endpoint that bypasses serialization
#* @tag outputs
#* @get /no_serialization
function(res){
  res$body <- "the content of the body"
  res
}



#* xml output
#* @tag outputs
#* @get /xml
#* @serializer contentType list(type="application/xml")
function(){
  data <- iris

  xml <- xmlTree()
  xml$addTag("document", close = FALSE)
  for (i in 1:nrow(data)) {
    xml$addTag("row", close = FALSE)
    for (j in names(data)) {
      xml$addTag(j, data[i, j])
    }
    xml$closeTag()
  }
  xml$closeTag()
  saveXML(xml)
}






############################# Public Messages Board ############################

#* Add a message to the "Public Messages Board"
#* @tag message_Board
#* @param from The sender
#* @param subject The message subject
#* @param content The message content
#* @post /messages
function(from, content, subject="no subject"){

  newMessage = data.frame(id = NA,
                          from = from,
                          subject = subject,
                          content = content,
                          time = as.character(Sys.time()))

  file <- "./data/messages.txt"

  if (file.exists(file)) {
    messages <- read.table(file,
                           header = T,
                           sep = '\t')

    newMessage$id <- max(messages$id) + 1
    write.table(newMessage, file, append = TRUE,
                sep = "\t",
                row.names = F,
                col.names = F)
    out <- "messages added !"
  } else {
    newMessage$id <- 1
    write.table(newMessage,
                file,
                sep = "\t",
                row.names = F,
                col.names = T)
    out <- "new file created, messages added !"
  }
  return(list(out = out,
              id = newMessage$id))
}



#* Return all messages of the message board
#* @tag message_Board
#* @get /messages
function() {

  file <- "./data/messages.txt"
  if (file.exists(file)) {
    messages <- read.table(file,
                           header = T,
                           sep = '\t')
    return(messages)
  }
}

#* Return a subset of messages according to param
#* @tag message_Board
#* @get /messages/<param>
function(param) {

  file <- "./data/messages.txt"
  if (file.exists(file)) {
    messages <- read.table(file,
                           header = T,
                           sep = '\t')

    if (param %in% unique(messages$from)) {

      return(subset(messages, from == param))

    } else if (param %in% unique(messages$subject)) {

      return(subset(messages, subject == param))

    } else if (as.numeric(param) %in% messages$id) {

      return(messages[messages$id ==  as.numeric(param),])

    }
  }
}





#* Edit a message
#* @tag message_Board
#* @param id id of the message
#* @param from The sender
#* @param subject The message subject
#* @param content The message content
#* @put /messages
function(id, from=NULL, content=NULL, subject=NULL){
  id <- as.numeric(id)
  file <- "./data/messages.txt"

  if (file.exists(file)) {

    messages <- read.table(file,
                           header = T,
                           sep = '\t')

    if (id %in% messages$id) {

      if (!is.null(from)) {
        messages[messages$id == id,]$from <- from
      }
      if (!is.null(content)) {
        messages[messages$id == id,]$content <- content
      }
      if (!is.null(subject)) {
        messages[messages$id == id,]$subject <- subject
      }
      messages[messages$id == id,]$time <- as.character(Sys.time())

      write.table(messages,
                  file,
                  sep = "\t",
                  row.names = F,
                  col.names = T)
      return("Messages edited ! ")

    }
  }
}




#* Delete a message
#* @tag message_Board
#* @delete /messages/<id>
function(id){
  id <- as.numeric(id)
  file <- "./data/messages.txt"

  if (file.exists(file)) {

    messages <- read.table(file,
                           header = T,
                           sep = '\t')

    if (id %in% messages$id) {
      messages <- messages[messages$id != id,]

      write.table(messages,
                  file,
                  sep = "\t",
                  row.names = F,
                  col.names = T)
      return("Messages deleted ! ")

    }
  }
}

################################### R power ####################################
#* Predict species of Iris
#* @tag Rpower
#* @param SepalLength
#* @param SepalWidth
#* @param PetalLength
#* @param PetalWidth
#* @get /predictIris
function(SepalLength, SepalWidth, PetalLength, PetalWidth){

  irisKNNmodel <- readRDS("data/irisKNNmodel.rds")

  newdata <- data.frame(
    Sepal.Length = as.numeric(SepalLength),
    Sepal.Width = as.numeric(SepalWidth),
    Petal.Length = as.numeric(PetalLength),
    Petal.Width = as.numeric(PetalWidth)
  )
  as.character(predict(irisKNNmodel, newdata = newdata))
}


#* Run Rmarkdown to generate a plumber tutorial
#* @tag Rpower
#* @get /plumbertuto
#* @html
function(){
  file <- tempfile("PlumberTuto", fileext = ".html")
  render("PlumberTuto.Rmd",
         output_file = file,
         envir = new.env(parent = globalenv()),
         encoding = "UTF-8")
  HTML <- paste(readLines(file), collapse = "\n")
  HTML
}

##################################### Filter ###################################

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}
