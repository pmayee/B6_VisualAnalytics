#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(igraph)
library(network)
library(sna)
library(RColorBrewer)
library(networkD3)
library(wordcloud)
library(tm)
library(SnowballC)
#library(colourpicker)
library(dplyr)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("#Tag-@Mention Visualizations",fluid = T,theme = "bootstrap.css",collapsible = T,
                         colorSet <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
                                       "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                         tabPanel("WordCloud",
                                  
                                  #THIS IS THE SIDEBAR INPUT PANEL
                                  column(2,wellPanel(
                                    
                                    checkboxInput("checkbox3", label = "Document stemming", value = TRUE),
                                    checkboxInput("checkbox2", label = "Repeatable", value = TRUE),
                                    
                                    # Slider input for minimum frequency
                                    sliderInput("slider4", "Minimum Frequency:",
                                                min = 0.0, max = 500, value = 50),
                                    
                                    # Slider input for rotation change
                                    sliderInput("slider3", "Rotation:",
                                                min = 0.0, max = 1.0, value = 0.35),
                                    
                                    # Slider input for number of words change
                                    sliderInput("slider2", "Maximum Words:",
                                                min = 10, max = 1000, value = 100),
                                    
                                    # Text file uploader
                                    fileInput("file", "Text file", accept=c("text/plain", ".txt"))
                                    
                                  )),
                                  
                                  #THIS IS THE MAIN CONTENT
                                  column(10, 
                                         # Image download button
                                         downloadButton("wordcloud_img", "Download Image"),
                                         # CSV download button
                                         downloadButton("freq_csv", "Download Frequency Table"),
                                         # Wordcloud image rendered
                                         imageOutput("wordcloud")

                                        )         #end of main body columns 
                         ) #end tab panel
                         
                         ,
                         tabPanel("Network Graph - 1",
                                  
                                  column(2,wellPanel(
                                    
                                  sliderInput("node_repulsion", "Node Repulsion:", 
                                              min = 10, max = 500, 
                                              value = 10),
                                  
                                  sliderInput('link_distance', "Link Distance", 
                                              min=5, max=300,
                                              value=30, 
                                              step=5),
                                  
                                  sliderInput('font_size', "Font Size", 
                                              min=5, max=50,
                                              value=10, 
                                              step=2),
                                  
                                  selectInput('node_color', "Node Color", 
                                              colorSet,
                                              selected="YlGnBu"),
                                  
                                  sliderInput("label_opacity", "Adjust Label Color opacity", 
                                              min = 0, max = 1, 
                                              value = 0,
                                              step=0.1)
                                  
                                  # selectInput('selected_hash', "Select #Tag", ""),
                                  # # colorSet,
                                  # # selected="YlGnBu")
                                  # 
                                  # selectInput('node_color2', "Node Color", 
                                  #             colorSet,
                                  #             selected="YlGnBu")
                                  
                                  
                                  )),
                                  column(10,
                                         
                                         forceNetworkOutput("forceNet1")
                                        
                                  )),
                      
                                  tabPanel("Network Graph - #Hash",
                                           
                                           column(2,wellPanel(
                                             
                                      
                                             selectInput('selected_hash', "Select #Tag", "",multiple = T),
                                             # colorSet,
                                             # selected="YlGnBu")
                                             
                                             
                                             sliderInput("node_repulsion2", "Node Repulsion:", 
                                                         min = -100, max = 500, 
                                                         value = 150),
                                             
                                             sliderInput('link_distance2', "Link Distance", 
                                                         min=5, max=300,
                                                         value=75, 
                                                         step=5),
                                             
                                             sliderInput('font_size2', "Font Size", 
                                                         min=5, max=50,
                                                         value=10, 
                                                         step=2),
                                             
                                             selectInput('node_color2', "Node Color", 
                                                         colorSet,
                                                         selected="Reds"),#YlGnBu
                                             
                                             sliderInput("label_opacity2", "Adjust Label Color opacity", 
                                                         min = 0, max = 1, 
                                                         value = 0.6,
                                                         step=0.1)
                                             
                                             # , colourInput("colourInput2", NULL, "orange",
                                             #   allowTransparent = TRUE)
                                             
                                           )),
                                  
                                  column(10,
                                         
                                         forceNetworkOutput("forceNet2")
                                  ))
                                ,

                                tabPanel("Network Graph - @Mention",

                                  column(2,wellPanel(


                                    selectInput('selected_mention', "Select @Mention", "",multiple = T),
                                    # colorSet,
                                    # selected="YlGnBu")


                                    sliderInput("node_repulsion3", "Node Repulsion:",
                                                min = 10, max = 500,
                                                value = 150),

                                    sliderInput('link_distance3', "Link Distance",
                                                min=5, max=300,
                                                value=75,
                                                step=5),

                                    sliderInput('font_size3', "Font Size",
                                                min=5, max=50,
                                                value=10,
                                                step=2),

                                    selectInput('node_color3', "Node Color",
                                                colorSet,
                                                selected="Reds"),#YlGnBu

                                    sliderInput("label_opacity3", "Adjust Label Color opacity",
                                                min = 0, max = 1,
                                                value = 0.6,
                                                step=0.1)

                                    # , colourInput("colourInput2", NULL, "orange",
                                    #   allowTransparent = TRUE)

                                  )),

                                  column(10,

                                         forceNetworkOutput("forceNet3")
                                  ))
                         
                                  

                         
                         # ,
                         # navbarMenu("More Info",
                         #            tabPanel("Test1",
                         #                     dataTableOutput("table")
                         #            )
                         # )
  ))
  

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  linksFileLocation <-  paste0(getwd(),"/Twitter_Hash_UserMention.csv")
  
  # linksFileLocation <-  paste0(getwd(),"/Twitter_Eng_Hash_UserMention_Unique_6.csv")
  # nodesFileLocation <- paste0(getwd(),"/Unique mention-mention_6.csv")
  
  links <- read.csv(linksFileLocation, header = T, as.is = T)
  #nodes <- read.csv(nodesFileLocation, header = T, as.is = T)
  
  links <- mutate_each(links, funs(tolower))
  links$Weight <- as.numeric(as.character(links$Weight))
  
  
  observe({
    updateSelectInput(session, "selected_hash", choices = unique(links$FROM) )
    updateSelectInput(session, "selected_mention", choices = unique(links$TO) )
    
  })

  
  output$forceNet1 <- renderForceNetwork({
    
    # data.frame(lapply(links, function(v) {
    #   if (is.character(v)) return(tolower(v))
    #   else return(v)
    # }))
    
    #class(links$Weight)
    #names(links)<-c("FROM","TO","Weight")
    #sapply(links,class)
    #nrow(unique(l[,c("FROM", "TO")]))
    #nrow(aggregate(links[,3], links[,-3], sum))
    links <- aggregate(links[,3], links[,-3], sum)
    links <- links[order(links$FROM, links$TO),]
    colnames(links)[3] <- "Weight"
  
    n2<-1
    nodes<-data.frame(c(links$FROM,links$TO),n2,stringsAsFactors=F)
    names(nodes)<-c("id","TYPE")
    nodes$TYPE[substr(nodes$id,1,1)=="@"]<-2
    
    vertices<-data.frame(name = unique(nodes)$id, group = unique(nodes)$TYPE,stringsAsFactors=F  ) 
    
    links$value <-(links$Weight/sum(links$Weight))*500
    links$FROM.index = match(links$FROM, vertices$name)-1
    links$TO.index = match(links$TO, vertices$name)-1

    scalecolors <- function(nodes, palette) {
      n <- max(unique(vertices$group))
      cols <- rev(RColorBrewer::brewer.pal(n, palette))
      cols <- paste0("'", paste(cols, collapse = "', '"), "'")
      networkD3::JS(paste0('d3.scale.ordinal().domain([0,', n, ']).range([', cols, '])'))
    }
        
    forceNetwork(Links = links, Nodes = vertices,
                 Source = 'FROM.index', 
                 Target = 'TO.index',
                 NodeID = 'name', 
                 Value = "value",
                 Group = 'group',
                 charge = -input$node_repulsion,
                 colourScale = scalecolors(vertices, input$node_color),
                 linkColour = "#000000",
                 linkDistance = input$link_distance,
                 zoom = T, 
                 opacity = 2,
                 opacityNoHover = input$label_opacity,
                 fontSize=input$font_size
                 #,height = 5800
                 )     
  })
  
  output$forceNet2 <- renderForceNetwork({
    
    if(is.null(input$selected_hash)){
      hash <- "#climate"
    }else{
      hash <- input$selected_hash
     }

    l <- data.frame(FROM=character(),TO=character(),Weight=integer(),stringsAsFactors=F)
    l1<- data.frame(FROM=character(),TO=character(),Weight=integer(),stringsAsFactors=F)
    
    for(i in 1:length(hash)){
      l1<-links[links$FROM==hash[i],]
      l<-rbind(l1,l,stringsAsFactors=F)
    }
    
        
    # hash <- c("#climate","#uselection")
    # class(hash)
    # hash[2]
    #colnames(links)
    #class(links$Weight)
    #n2<-3
    #names(x2)<-c("id","TYPE")
    #x2$TYPE[substr(x2$id,1,1)=="@"]<-4
    # l <- mutate_each(l, funs(tolower))
    # l$Weight <- as.numeric(as.character(l$Weight))

    l <- aggregate(l[,3], l[,-3], sum)
    l <- l[order(l$FROM, l$TO),]
    colnames(l)[3] <- "Weight"
    
    n2<-1
    nodes<-data.frame(c(l$FROM,l$TO),n2,stringsAsFactors=F)
    names(nodes)<-c("id","TYPE")
    nodes$TYPE[substr(nodes$id,1,1)=="@"]<-2
    
   # x2<-data.frame(c(l$FROM,l$TO),stringsAsFactors=F)
    
    vertices<-data.frame(name = unique(nodes)$id, group = unique(nodes)$TYPE,stringsAsFactors=F  ) 
    
    # vertices<-data.frame(name = unique(x2)$id, group = unique(x2)$TYPE,stringsAsFactors=F ) 
    
    l$value <-(l$Weight/sum(l$Weight))*500
    l$FROM.index = match(l$FROM, vertices$name)-1
    l$TO.index = match(l$TO, vertices$name)-1
    
    scalecolors2 <- function(nodes, palette) {
      n <- max(unique(vertices$group))
      cols <- rev(RColorBrewer::brewer.pal(n, palette))
      cols <- paste0("'", paste(cols, collapse = "', '"), "'")
      networkD3::JS(paste0('d3.scale.ordinal().domain([0,', n, ']).range([', cols, '])'))
    }
    
    forceNetwork(Links = l, Nodes = vertices,
                 Source = 'FROM.index', 
                 Target = 'TO.index',
                 NodeID = 'name', 
                 Value = "value",
                 Group = 'group',
                 charge = -input$node_repulsion2,
                 colourScale = scalecolors2(vertices, input$node_color2), #input$colourInput2), 
                 linkColour = "#000000",
                 linkDistance = input$link_distance2,
                 zoom = T, 
                 opacity = 2,
                 opacityNoHover = input$label_opacity2,   #0.8,
                 #, node
                 fontSize=input$font_size2
    )     
  })
  
  
  output$forceNet3 <- renderForceNetwork({
    
    if(is.null(input$selected_mention)){
      mention <- "@realdonaldtrump"
    }else{
      mention <- input$selected_mention
    }
    
    l <- data.frame(FROM=character(),TO=character(),Weight=integer(),stringsAsFactors=F)
    l1<- data.frame(FROM=character(),TO=character(),Weight=integer(),stringsAsFactors=F)
    
    for(i in 1:length(mention)){
      l1<-links[links$TO==mention[i],]
      l<-rbind(l1,l,stringsAsFactors=F)
    }

    l <- aggregate(l[,3], l[,-3], sum)
    l <- l[order(l$FROM, l$TO),]
    colnames(l)[3] <- "Weight"
    
    n2<-1
    nodes<-data.frame(c(l$FROM,l$TO),n2,stringsAsFactors=F)
    names(nodes)<-c("id","TYPE")
    nodes$TYPE[substr(nodes$id,1,1)=="@"]<-2
    
    vertices<-data.frame(name = unique(nodes)$id, group = unique(nodes)$TYPE,stringsAsFactors=F) 
    
    l$value <-(l$Weight/sum(l$Weight))*500
    l$FROM.index = match(l$FROM, vertices$name)-1
    l$TO.index = match(l$TO, vertices$name)-1
    
    scalecolors3 <- function(nodes, palette) {
      n <- max(unique(vertices$group))
      cols <- rev(RColorBrewer::brewer.pal(n, palette))
      cols <- paste0("'", paste(cols, collapse = "', '"), "'")
      networkD3::JS(paste0('d3.scale.ordinal().domain([0,', n, ']).range([', cols, '])'))
    }
    
    forceNetwork(Links = l, Nodes = vertices,
                 Source = 'FROM.index', 
                 Target = 'TO.index',
                 NodeID = 'name', 
                 Value = "value",
                 Group = 'group',
                 charge = -input$node_repulsion3,
                 colourScale = scalecolors3(vertices, input$node_color3), #input$colourInput2), 
                 linkColour = "#000000",
                 linkDistance = input$link_distance3,
                 zoom = T, 
                 opacity = 2,
                 opacityNoHover = input$label_opacity3,   #0.8,
                 #, node
                 fontSize=input$font_size3
    )     
  })
  
  
 ##################################################### 
  
  
  datainput <- reactive({
    
    # Outputs a helpful message when no text file is uploaded
    validate(
      need((input$text != "") || (!is.null(input$file)),
           "Please upload a text file."
      )
    )
    
    # Load text from text file uploaded
    if (!is.null(input$file)){
      a <- input$file$datapath
      a <- substr(a, 1, nchar(a) - 1)
      words <- Corpus(DirSource(a))
    }
    
    # Remove all punctuations except hashtag
    removeMostPunctuation<-
      function (x, preserve_intra_word_dashes = FALSE)
      {
        rmpunct <- function(x) {
          #x <- gsub("#","\002", x)
          x <- gsub("[]\\?!\"\'$%&(){}+*/:;,._`|~\\[<=>\\^-]", "", x)
          #gsub("\002", "#", x, fixed = TRUE)
        }
        if (preserve_intra_word_dashes) {
          x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
          x <- rmpunct(x)
          gsub("\001", "-", x, fixed = TRUE)
        } else {
          rmpunct(x)
        }
      }
    
    
    # Cleaning & normalizing the corpus.
    words <- tm_map(words, content_transformer(removeMostPunctuation),
                    preserve_intra_word_dashes = TRUE)
    words <- tm_map(words, removeNumbers)
    words <- tm_map(words, removeWords, c(stopwords("en"),"the","amp","httpstcoxzuewsek"))
    words <- tm_map(words, stripWhitespace)
    
  })
  
  # Reactive element to transform the data on the basis of
  # (de)selection of checkbox3 in ui.R
  finalinput <- reactive({
    if (input$checkbox3) datainput <- tm_map(datainput(), stemDocument)
    datainput()
  })
  
  # Reactive element to transform the data on the basis of
  # (de)selection of checkbox2 in ui.R
  asdas <- reactive({
    if (input$checkbox2) wordcloud_rep <- repeatable(wordcloud)
    else wordcloud_rep <- wordcloud
  })
  
  # Reactive element to generate the wordcloud and save it as a png
  # and return the filename.
  make_cloud <- reactive ({
    wordcloud_rep <- asdas()
    
    png("wordcloud.png", width=10, height=8, units="in", res=350)
    w <- wordcloud_rep(finalinput(),
                       scale=c(5, 0.5),
                       min.freq=input$slider4,
                       max.words=input$slider2,
                       rot.per=input$slider3,
                       use.r.layout=FALSE,
                       colors=brewer.pal(8, "Dark2"))
    dev.off()
    
    filename <- "wordcloud.png"
  })
  
  # Download handler for the image.
  output$wordcloud_img <- downloadHandler(
    filename = "wordcloud.png",
    content = function(cloud) {
      file.copy(make_cloud(), cloud)
    })
  
  # Download handler for the csv.
  output$freq_csv <- downloadHandler(
    filename = "freq.csv",
    content = function(freq) {
      a <- DocumentTermMatrix(finalinput())
      b <- sort(colSums(as.matrix(a)), decreasing=TRUE)
      write.csv(b, freq)
    })
  
  # Sending the wordcloud image to be rendered.
  output$wordcloud <- renderImage({
    list(src=make_cloud(), alt="Image being generated!", height=600)
  },
  deleteFile = FALSE)
#########################################################################################################
  

})

# Run the application 
shinyApp(ui = ui, server = server)

