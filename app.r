library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(sf)
library(wordcloud)
#library(shinyjs)
#library(rapportools)
#library(colourpicker)
#library(DT)


scenes <- read.csv("www/data/scenes.csv")
episodes <- read.csv("www/data/episodes.csv")
characters <- read.csv("www/data/characters.csv")
appearances <- read.csv("www/data/appearances.csv")

locations <- st_read("www/data/GoTRelease/Locations.shp", crs=4326, quiet=TRUE)
lakes <- st_read("www/data/GoTRelease/Lakes.shp", crs=4326, quiet=TRUE)
conts <- st_read("www/data/GoTRelease/Continents.shp", crs=4326, quiet=TRUE)
land <- st_read("www/data/GoTRelease/Land.shp", crs=4326, quiet=TRUE)
wall <- st_read("www/data/GoTRelease/Wall.shp", crs=4326, quiet=TRUE)
islands <- st_read("www/data/GoTRelease/Islands.shp", crs=4326, quiet=TRUE)
kingdoms <- st_read("www/data/GoTRelease/Political.shp", crs=4326, quiet=TRUE)
landscapes <- st_read("www/data/GoTRelease/Landscape.shp", crs=4326, quiet=TRUE)
roads <- st_read("www/data/GoTRelease/Roads.shp", crs=4326, quiet=TRUE)
rivers <- st_read("www/data/GoTRelease/Rivers.shp", crs=4326, quiet=TRUE)
scenes_locations <- st_read("www/data/GoTRelease/ScenesLocations.shp",crs=4326, quiet=TRUE)


colforest="#c0d7c2"
colriver="#7ec9dc"
colriver="#87cdde"
colland="ivory"
borderland = "ivory3"

battles <- data.frame(
    c("Assault on Dreadfort", 4, 6),
    c("Daenerys Sacks Astapor", 3, 4),
    c("The Battle of Winterfell", 5, 10),
    c("Siege of Meereen", 6, 9),
    c("Uprising at Daznak's Pit", 5, 9),
    c("Greyjoy Naval Battle", 7, 2),
    c("The Raid at the Weirwood",  6,  5),
    c("Battle Beyond the Wall", 7, 6),
    c("Loot Train Attack", 7, 4),
    c("Battle at Hardhome", 5, 8),
    c("Battle of Winterfell", 8, 3),
    c("Battle of the Blackwater", 2, 9),
    c("Battle of Castle Black", 4, 9),
    c("Battle of the Bastards", 6, 9),
    row.names = c("Battle", "Season", "Episode")
)
colnames(battles) <- 1:14
battles <- data.frame(t(battles)) %>% arrange(Season, Episode)

landpol = st_union(st_geometry(land)) 
islandpol = st_union(st_geometry(islands))
backpol=st_union(landpol,islandpol)

the_map <- ggplot() + geom_sf(data=backpol,color=borderland,fill=colland)

ui <- dashboardPage(
  dashboardHeader(title="GoT dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Information", tabName="general-info", newtab=FALSE),
      menuItem("Characters", tabName="characters", newtab=FALSE),
      menuItem("Battles", tabName="battles", newtab=FALSE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="general-info", 
        tabsetPanel(
          tabPanel("Scenes",
            fluidRow(
              column(
                3,
                tags$strong("Choose scene")
              ),
              column(
                3,
                textInput(inputId="sceneId", label=NULL, placeholder="from 1 to 3840")
              ),
              column(
                1,
                actionButton(inputId="confirmSceneId", label="Ok")
              )
            ),
            fluidRow(
                tableOutput("sceneInfo")
            ),
            fluidRow(
              column(
                8,
                tableOutput("scenePersos")
              ),
              column(
                4,
                imageOutput("persoImg"),
              )
            )
          ),
          tabPanel("Episodes",
            fluidRow(
             column(
               3,
               tags$strong("Choose episode")
             ),
             column(
               3,
               textInput(inputId="episodeId", label=NULL, placeholder="from 1 to 73")
             ),
             column(
               1,
               actionButton(inputId="confirmEpisodeId", label="Ok")
             )
            ),
            fluidRow(
             tableOutput("episodeInfo")
            ),
            fluidRow(
             column(
               4,
               plotOutput("episodeSceneDur")
             ),
             column(
               4,
               plotOutput("episodePersoDur"),
             ),
             column(
               4,
               plotOutput("episodeMap"),
             )
            )
          ),
          tabPanel("Seasons",
            fluidRow(
             column(
               3,
               tags$strong("Choose season")
             ),
             column(
               3,
               textInput(inputId="seasonId", label=NULL, placeholder="from 1 to 8")
             ),
             column(
               1,
               actionButton(inputId="confirmSeasonId", label="Ok")
             )
            ),
            fluidRow(
             tableOutput("seasonInfo")
            ),
            fluidRow(
             column(
               4,
               plotOutput("seasonSceneDur")
             ),
             column(
               4,
               plotOutput("seasonPersoDur"),
             ),
             column(
               4,
               plotOutput("seasonMap"),
             )
            )
          )
        )
      ),
      tabItem(tabName="characters",
        tabsetPanel(
          fluidRow(
              
              # Input: Slider for the number of bins ----
              selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"), selected = "Blue",multiple = F),
              selectInput(inputId="channel1",label="Choose characters",choices = as.vector(characters[1]), selected = "Jon Snow",multiple = F),
              selectInput(inputId="channel2",label="Select an other character",choices = as.vector(rbind(characters[1],"None")), selected = "None",multiple = F),
              sliderInput(inputId = "nbrpersonne", label = "Maximum knowledge of other characters:", min = 15, max = 50, value = 5),
            ),
            
            # Main panel for displaying outputs ----
            fluidRow(
              tabsetPanel(type='tabs',
                tabPanel(
                
                  titlePanel(h5("Spatial repartition")),
                  h2("1-Spacial repartition of characters per scenes",style='background-color:coral;
                       padding-left: 15px'),
                  plotOutput(outputId = "distPlot")),
                  
                  tabPanel(titlePanel(h5("Appearance time")),h2("2-Appearance time per episode",style='background-color:coral; padding-left: 15px'),
                           plotOutput(outputId = "distPlot1")),
                  tabPanel(titlePanel(h5("Wordcloud")),
                           h2("3-Wordcloud of characters and their interaction with other characters",style='background-color:coral; padding-left: 15px'),
                           plotOutput(outputId = "distPlot2"))
              )
            )
        )
      ),
      tabItem(tabName="battles",
        fluidRow(
          selectInput(inputId='battleId', 'Options', battles$Battle, selectize=TRUE)
        ),
        fluidRow(
          column(
            4,
            tableOutput("battleChars")
          ),
          column(
            4,
            plotOutput("battleDeaths"),
          ),
          column(
            4,
            plotOutput("battleMap"),
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  sceneData <- eventReactive(input$confirmSceneId, {
    scenes %>% filter(sceneId == input$sceneId) %>%
      left_join(episodes) %>%
      select(
        c("sceneId", "episodeId", "seasonNum", 
          "location", "subLocation", "duration", 
          "nbdeath")
      ) %>%
      rename(
        "Scene" = sceneId, 
        "Episode" = episodeId, 
        "Season" = seasonNum, 
        "Location" = location, 
        "Sublocation" = subLocation, 
        "Duration (s)" = duration, 
        `Number of deaths` = nbdeath
      )
  })
  scenePerso <- eventReactive(input$confirmSceneId, {
    scenes %>% filter(sceneId == input$sceneId) %>%
      left_join(appearances) %>%
      left_join(characters) %>%
      select(c("name", "sex", "house")) %>%
      rename(
        "Character" = name,
        "Sex" = sex,
        "House" = house
      )
  })
  
  episodeData <- eventReactive(input$confirmEpisodeId, {
    episodes %>% filter(episodeId == input$episodeId) %>%
      left_join(scenes) %>%
      mutate(minSceneId = min(sceneId), maxSceneId = max(sceneId), total_duration = total_duration/60) %>%
      select(
        c("episodeNum", "episodeTitle", "seasonNum", 
          "total_duration", "minSceneId", "maxSceneId")
      ) %>% 
      rename(
       "Number" = episodeNum,
       "Title" = episodeTitle,
       "Season" = seasonNum,
       `Duration (mn)` = total_duration,
       `First Scene` = minSceneId,
       `Last Scene` = maxSceneId
      ) %>%
      unique()
      
  })
  episodeDuration <- eventReactive(input$confirmEpisodeId, {
    ggplot(scenes %>% left_join(episodes) %>%  filter(episodeId == input$episodeId)) +
      geom_line(aes(x=sceneId, y=duration/60)) +
      xlab("Scene") +
      ylab("Scene duration (mn)") +
      ggtitle("Scenes' duration")
  })
  episodeTimePerso <- eventReactive(input$confirmEpisodeId, {
    ggplot(
      appearances %>% left_join(scenes) %>% 
      left_join(episodes) %>% 
      filter(episodeId==input$episodeId) %>%
      group_by(name) %>% 
      summarise(screenTime=sum(duration)) %>%
      arrange(desc(screenTime)) %>%
      mutate(name=factor(name, levels=unique(name))) %>%
      top_n(30, screenTime)
    ) +
      geom_bar(aes(x=reorder(name, screenTime), y=screenTime/60), stat="identity") +
      coord_flip() + xlab("") + ylab("Screen time") +
      ggtitle("Screen time per character")
  })
  episodeLocs <- eventReactive(input$confirmEpisodeId, {
    scenes %>% left_join(episodes) %>%
      filter(!is.na(location), episodeId==input$episodeId) %>%
      select(c("location", "subLocation")) %>%
      group_by(location, subLocation) %>% 
      summarise(freq=n()) %>%
      ungroup() %>%
      left_join(scenes_locations) %>%
      st_as_sf()
  })
  episodeMap <- eventReactive(input$confirmEpisodeId, {
    the_map +  
      geom_sf(data=episodeLocs(), aes(size=freq), colour="orange", show.legend = 'point') +
      geom_sf_text(
        data=episodeLocs() %>% 
          mutate(f=freq/sum(freq)) %>%
          filter(f > 0.1),
        aes(label=location),color="#000000",
        vjust="bottom",family="Palatino", fontface="italic")+
      scale_size_area(name="Number of scenes", max_size=10)+
      coord_sf(expand = 0,ndiscr = 0) +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = colriver,color=NA)
      ) +
      labs(title = "Geographic repartition of scenes",x="",y="")
  })
  
  seasonData <- eventReactive(input$confirmSeasonId, {
    episodes %>% left_join(scenes) %>%
      group_by(seasonNum) %>%
      summarise(
        "Episodes"=n_distinct(episodeTitle),
        "Duration (h)"= sum(duration)/3600,
        `First Scene` = min(sceneId),
        `Last Scene` = max(sceneId),
        "Number of deaths"=sum(nbdeath)
      ) %>% 
      filter(seasonNum==input$seasonId) %>%
      rename("Number"=seasonNum)
  })
  seasonDuration <- eventReactive(input$confirmSeasonId, {
    ggplot(scenes %>% left_join(episodes) %>%  filter(seasonNum == input$seasonId)) +
      geom_line(aes(x=sceneId, y=duration/60)) +
      xlab("Scene") +
      ylab("Scene duration (mn)") +
      ggtitle("Scenes' duration")
  })
  seasonTimePerso <- eventReactive(input$confirmSeasonId, {
    ggplot(
      appearances %>% left_join(scenes) %>% 
        left_join(episodes) %>% 
        filter(seasonNum==input$seasonId) %>%
        group_by(name) %>% 
        summarise(screenTime=sum(duration)) %>%
        arrange(desc(screenTime)) %>%
        mutate(name=factor(name, levels=unique(name))) %>%
        top_n(30, screenTime)
    ) +
      geom_bar(aes(x=reorder(name, screenTime), y=screenTime/60), stat="identity") +
      coord_flip() + xlab("") + ylab("Screen time") +
      ggtitle("Screen time per character")
  })
  seasonLocs <- eventReactive(input$confirmSeasonId, {
    scenes %>% left_join(episodes) %>%
      filter(!is.na(location), seasonNum==input$seasonId) %>%
      select(c("location", "subLocation")) %>%
      group_by(location, subLocation) %>% 
      summarise(freq=n()) %>%
      ungroup() %>%
      left_join(scenes_locations) %>%
      st_as_sf()
  })
  seasonMap <- eventReactive(input$confirmSeasonId, {
    the_map +  
      geom_sf(data=seasonLocs(), aes(size=freq), colour="orange", show.legend = 'point') +
      geom_sf_text(
        data=seasonLocs(), aes(label=location),
        check_overlap=TRUE, color="#000000", 
        vjust="bottom", family="Palatino", fontface="italic")+
      scale_size_area(name="Number of scenes", max_size=10)+
      coord_sf(expand = 0,ndiscr = 0) +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = colriver,color=NA)
      ) +
      labs(title = "Geographic repartition of scenes",x="",y="")
  })
  
  battleChars <- eventReactive(input$battleId, {
    episodes %>% left_join(scenes) %>%
      left_join(appearances) %>%
      left_join(characters) %>%
      filter(
        seasonNum==battles[battles["Battle"]==input$battleId][2],
        episodeNum==battles[battles["Battle"]==input$battleId][3]
      ) %>%
      select(c("name", "sex", "house")) %>%
      unique  %>%
      rename(
        "Character" = name,
        "Sex" = sex,
        "House" = house
      ) %>%
      arrange(House) %>%
      head(15)
  })
  battleDeaths <- eventReactive(input$battleId, {
    ggplot(
      episodes %>% left_join(scenes) %>%
        filter(
          seasonNum==battles[battles["Battle"]==input$battleId][2],
          episodeNum==battles[battles["Battle"]==input$battleId][3]
        )
    ) +
      geom_line(aes(x=sceneId, y=nbdeath)) +
      xlab("Scene") +
      ylab("Number of deaths") +
      ggtitle("Number of deaths in the battle episode")
  })
  battleLocs <- eventReactive(input$battleId, {
    scenes %>% left_join(episodes) %>%
      filter(
        !is.na(location),
        seasonNum==battles[battles["Battle"]==input$battleId][2],
        episodeNum==battles[battles["Battle"]==input$battleId][3]
      ) %>%
      select(c("location", "subLocation")) %>% unique %>%
      left_join(scenes_locations) %>%
      st_as_sf()
  })
  battleMap <- eventReactive(input$battleId, {
    the_map + 
      geom_sf(data=battleLocs(), aes(geometry=geometry)) +
      geom_sf_text(
        data=battleLocs(), aes(label=subLocation),
        check_overlap=TRUE, color="#000000", 
        vjust="bottom", family="Palatino", fontface="italic")+
      coord_sf(expand = 0,ndiscr = 0) +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = colriver,color=NA)
      ) +
      labs(title = "Location of the Battle and surrounding events",x="",y="")
  })
  
  output$sceneInfo <- renderTable({
    sceneData()
  }, na="-")
  output$scenePersos <- renderTable({
    scenePerso()
  }, na="-")
  
  output$episodeInfo <- renderTable({
    episodeData()
  }, na="-")
  output$episodeSceneDur <- renderPlot({
    episodeDuration()
  })
  output$episodePersoDur <- renderPlot({
    episodeTimePerso()
  })
  output$episodeMap <- renderPlot({
    episodeMap()
  })
  
  output$seasonInfo <- renderTable({
    seasonData()
  }, na="-")
  output$seasonSceneDur <- renderPlot({
    seasonDuration()
  })
  output$seasonPersoDur <- renderPlot({
    seasonTimePerso()
  })
  output$seasonMap <- renderPlot({
    seasonMap()
  })
  
  output$battleChars <- renderTable({
    battleChars()
  }, na="-")
  output$battleDeaths <- renderPlot({
    battleDeaths()
  })
  output$battleMap <- renderPlot({
    battleMap()
  })
  
  output$distPlot <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    
    
    
    if(input$channel2=="None"){
      
      
      
      
      loc_time=appearances %>% filter(name==input$channel1) %>% left_join(scenes) %>% group_by(location) %>% summarize(duration=sum(duration,na.rm=TRUE))
      loc_time_js = scenes_locations %>% left_join(loc_time)
      
      
      colforest="#c0d7c2"
      colriver="#7ec9dc"
      colriver="#d7eef4"
      colland="ivory"
      borderland = "ivory3"
      ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
        geom_sf(data=islands,fill=colland,col="ivory3")+
        geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
        geom_sf(data=rivers,col=colriver)+
        geom_sf(data=lakes,col=colriver,fill=colriver)+
        geom_sf(data=wall,col="black",size=1)+
        geom_sf(data=loc_time_js,aes(size=duration/60),color=sColor)+theme_minimal()+
        scale_size_area("Duration (min) :",max_size = 15,breaks=c(30,60,120,240))+
        theme(panel.background = element_rect(fill = colriver,color=NA),legend.position = "bottom")+
        theme_economist()
      
    }
    else{
      
      
      
      
      
      
      loc_time1=appearances %>% filter(name == input$channel1) %>% left_join(appearances,by=c("sceneId"="sceneId"))
      loc_time2=loc_time1 %>% filter(name.y == input$channel2)%>%left_join(scenes) 
      loc_time_js1=scenes_locations %>% left_join(loc_time2)%>% drop_na()
      
      
      if(is.empty(loc_time_js1$sceneId)==FALSE){
        
        
        
        colforest="#c0d7c2"
        colriver="#7ec9dc"
        colriver="#d7eef4"
        colland="ivory"
        borderland = "ivory3"
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data=loc_time_js1,aes(size=duration/60),color=sColor)+theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          scale_size_area("Scenes duration (min) :",max_size = 15,breaks=c(30,60,120,240))+
          theme(panel.background = element_rect(fill = colriver,color=NA),legend.position = "bottom")+
          theme_economist()
        
      }
      else{
        colforest="#c0d7c2"
        colriver="#7ec9dc"
        colriver="#d7eef4"
        colland="ivory"
        borderland = "ivory3"
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA))+
          theme_economist()
        
        
      }
      
    }
    
    
  })
  output$distPlot1 <- renderPlot({
    
    if(input$channel2=="None"){
      
      jstime <-  appearances %>% filter(name==input$channel1) %>% left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))
      jstime_1 <- jstime %>%left_join(episodes)%>%group_by(seasonNum)
      Seasons <- factor(jstime_1$seasonNum)
      ggplot(jstime_1) + 
        geom_point(aes(x=episodeId,y=time,colour = Seasons)) +
        theme_bw()+
        xlab("Episodes")+ylab("temps")+
        ggtitle(paste("Appearance Time per episode", input$channel1))+
        theme_economist()
      
    }
    else{
      js_time1 <- appearances %>% filter(name == input$channel1) %>% left_join(appearances,by=c("sceneId"="sceneId"))
      js_time2<- js_time1 %>% filter(name.y == input$channel2)%>%left_join(scenes)%>%group_by(episodeId) %>% summarise(time=sum(duration))
      jstime_1<- js_time2 %>%left_join(episodes)%>%group_by(seasonNum)
      Seasons <- factor(jstime_1$seasonNum)
      ggplot(jstime_1) + 
        geom_point(aes(x=episodeId,y=time,colour = Seasons)) +
        theme_bw()+
        xlab("Episodes")+ylab("temps")+
        ggtitle(paste("Character appearance time per episode", input$channel1,"avec",input$channel2))+
        theme_economist()
    }
    
    
  })
  output$distPlot2 <- renderPlot({
    
    js_data=appearances %>% filter(name == input$channel1)
    
    Withperson=js_data %>% left_join(appearances,by=c("sceneId"="sceneId")) %>%  filter(name.x!=name.y) %>%  group_by(name.x,name.y) %>% count(name.y, sort = TRUE)
    
    
    wordcloud(words = Withperson$name.y, freq = Withperson$n,scale=c(3,0.25),min.freq = 1, max.words=input$nbrpersonne,random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2") )
    
    
  })
}

shinyApp(ui, server)