#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")


#Data

locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)
lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326)
conts=st_read("./data/GoTRelease/Continents.shp",crs=4326)
land=st_read("./data/GoTRelease/Land.shp",crs=4326)
wall=st_read("./data/GoTRelease/Wall.shp",crs=4326)
islands=st_read("./data/GoTRelease/Islands.shp",crs=4326)
kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326)
landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326)
rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326)
scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp", crs=4326)

colforest="#c0d7c2"
colriver="#7ec9dc"
colriver="#87cdde"
colland="ivory"
borderland = "ivory3"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title

  
  
  # Sidebar with a slider input for number of bins 
  navbarPage(theme = shinytheme("slate"), "GAME OF THRONES", tabPanel("Accueil",
                                   sidebarPanel(
                                     tags$img(height=400, width=400, 
                                              src="https://cap.img.pmdstatic.net/fit/http.3A.2F.2Fprd2-bone-image.2Es3-website-eu-west-1.2Eamazonaws.2Ecom.2Fcap.2F2017.2F05.2F10.2F6b7b11a5-b337-4ba6-a65b-71697e7f5d92.2Ejpeg/1200x630/background-color/ffffff/quality/70/cr/wqkgVG91cyBkcm9pdHMgcsOpc2VydsOpcyAvIENBUElUQUw%3D/game-of-thrones-ce-que-cette-serie-culte-nous-apprend-sur-le-pouvoir-en-entreprise-960913.jpg"),
                                     tags$br(), tags$h3("Veuillez cliquer sur les onglets ci-dessus pour avoir differentes informations sur GOT"), tags$br(),
                                       style='width: 1000px; height: 1000px')), tabPanel("Information préliminaire",  sidebarPanel( h3("Information préliminaire"), selectInput("InpID", "Veuillez choisir une option", choices = c("nombre de personnages morts dans l’ensemble de la série","Nombre de personnages morts lors de la première saison", "la durée de la scène la plus longue", "le nombre de personnages qui passent plus de 30 minutes à l’écran sur l’ensemble des saisons")),
                                                                                                                                        tags$br(), h1( column(5, textOutput(outputId = "Graphe"))),style='width: 1000px; height: 1000px' )), tabPanel(("Information générale"), sidebarPanel(selectInput("InpID1", h4("Veuillez choisir une option"), choices = c("Map de GOT", "Répartition des durées des scènes par épisodes","Evolution du nombre de mort au cours du temps", "Temps de présence de John Snow par épisode", "Clustering des personnages principaux suivant leur lieux de présence", "Personnages qui apparaissent plus d'une heure sur l'ensemble des saisons")),plotOutput(outputId = "distPlot"),style='width: 1000px; height: 1000px')),
                                                                                                                                        tabPanel("Localisation individuelle", sidebarPanel(  selectInput("select",h4("Veuillez choisir une personnage"),
                                                                                                                                                                                         
                                                                                                                                                                                         choices = list( "Jon Snow", "Tyrion Lannister" , "Daenerys Targaryen"  , "Sansa Stark","Cersei Lannister","Arya Stark" ), 
                                                                                                                                                                                         selected = "Jon Snow"),plotOutput(outputId = "distPlott"),style='width: 1000px; height: 1000px')))                                                                                                   
                                                                                                                                
                                   
                                  

)



# Define server logic required to draw a histogram
server=function(input, output){
  
  output$Graphe = renderText({
    c = input$InpID
    if (c == "nombre de personnages morts dans l’ensemble de la série"){
      sum(scenes$nbdeath)
      
    }
    else if (c == "la durée de la scène la plus longue"){
      as.character(scenes[which.max(scenes$duration),][6][1])
    }
    else if (c == "le nombre de personnages qui passent plus de 30 minutes à l’écran sur l’ensemble des saisons"){
      appearances %>% left_join(scenes)  %>% 
        group_by(name) %>% 
        summarise(screenTime=sum(duration)) %>% 
        filter(screenTime>30*60) %>% 
        nrow()
    }
    else {
      sum(scenes$nbdeath[scenes$episodeId<=10])
    }
  })
  output$distPlot=renderPlot({
    b = input$InpID1
    if (b == "Map de GOT"){
      
      map = ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
        geom_sf(data=islands,fill=colland,col="ivory3")+
        geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
        geom_sf(data=rivers,col=colriver)+
        geom_sf(data=lakes,col=colriver,fill=colriver)+
        geom_sf(data=wall,col="black",size=1)+
        geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
        theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
        theme(panel.background = element_rect(fill = colriver,color=NA)) +
        labs(title = "GoT",caption = "Etiennne Côme, 2020",x="",y="")
      map
      
    }
    
    else if (b == "Répartition des durées des scènes par épisodes"){
      labels = scenes %>% filter(duration>400)
      map1 = ggplot(scenes %>% left_join(episodes))+
        geom_boxplot(aes(x=factor(episodeId),y=duration,fill=factor(seasonNum)))+
        geom_text(data=labels ,aes(x=factor(episodeId),y=duration,label=subLocation),hjust = "right",vjust="top")+
        scale_x_discrete("N° épisode",as.character(seq(1,73, by=5)))+
        scale_fill_brewer(palette="Spectral",guide="none")+
        ylab("Durée des scènes (min)")+
        ggtitle("Répartition des durées des scènes par épisodes")+
        theme_bw()
      map1
      
    }
    else if (b == "Evolution du nombre de mort au cours du temps"){
      
      deaths = scenes %>% select(nbdeath,duration,location,episodeId) %>% 
        mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
      
      # instant de changement de saison
      # ? lag
      season_t = episodes %>% mutate(ld=lag(total_duration)) %>% 
        mutate(ld=if_else(is.na(ld),0,ld), td = cumsum(ld)) %>% 
        filter(episodeNum==1) %>% pull(td)
      
      # geom_line + labels personalisés
      map2 = ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
        scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                           labels =   paste("Saison",1:8),)+
        scale_y_continuous("Nombre de morts cumulés", expand=c(0,0))+
        theme_bw()+
        theme(axis.text.x=element_text(angle=90))+
        ggtitle("Evolution du nombre de mort au cours du temps")
      map2
      
    }
    else if (b=="Temps de présence de John Snow par épisode"){
      jstime = appearances %>% filter(name=="Jon Snow") %>% 
        left_join(scenes) %>% 
        group_by(episodeId) %>% 
        summarise(time=sum(duration))
      map3 = ggplot(jstime) + 
        geom_line(aes(x=episodeId,y=time))+
        theme_bw()+
        xlab("épisode")+ylab("temps")+
        ggtitle("Temps de présence par épisode de John Snow")
      map3
    }
    else if (b=="Clustering des personnages principaux suivant leur lieux de présence"){
      duration_location_character = scenes %>% left_join(appearances) %>% 
        group_by(name,location) %>% 
        summarize(duration=sum(duration))
      
      duration_large = duration_location_character %>% 
        pivot_wider(values_from = duration,names_from = location,values_fill = c("duration"=0))
      
      
      X=as.matrix(duration_large[,-1])
      Xs=X[rowSums(X)>60*60,]
      Xns=Xs/rowSums(Xs)
      rownames(Xns)=duration_large$name[rowSums(X)>60*60]
      
      hc=hclust(dist(Xns,method="manhattan"))
      plot(hc,main = "Clustering des personnages principaux suivant leur lieux de présences",sub ="@comeetie, 2020",xlab = "")
    }
    else if (b=="Personnages qui apparaissent plus d'une heure sur l'ensemble des saisons"){
      screenTimePerSeasons = appearances %>% left_join(scenes) %>% 
        left_join(episodes) %>% 
        group_by(name,seasonNum) %>% 
        summarise(screenTime=sum(duration)) %>% 
        arrange(desc(screenTime)) 
      screenTimeTotal = screenTimePerSeasons %>% 
        group_by(name) %>% 
        summarise(screenTimeTotal=sum(screenTime))
      mainCharacters = screenTimeTotal %>% 
        filter(screenTimeTotal>60*60) %>% 
        arrange(screenTimeTotal) %>% 
        mutate(nameF=factor(name,levels = name))
      data = screenTimePerSeasons %>% left_join(mainCharacters) %>% filter(!is.na(nameF))
      ggplot(data)+
        geom_bar(aes(y=nameF,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
        scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
        geom_text(data=mainCharacters,aes(y=nameF,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
        scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
        ylab("")+ggtitle("Temps d'apparition cumulé par personnage et saison")
    }

  })
  
  output$distPlott=renderPlot({
    cc = input$select
    library(sf)
    library(tidyr)
    library(ggplot2)
    library(dplyr)
    library(readr)
    library(leaflet)
    characters = read_csv("data/characters.csv")
    episodes = read_csv("data/episodes.csv")
    scenes = read_csv("data/scenes.csv")
    appearances = read_csv("data/appearances.csv")
    
    locations=st_read("./data/GoTRelease/Locations.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    locations=st_transform(locations,4326)
    #locations=leaflet(locations)
    scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    conts=st_read("./data/GoTRelease/Continents.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    land=st_read("./data/GoTRelease/Land.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    wall=st_read("./data/GoTRelease/Wall.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    islands=st_read("./data/GoTRelease/Islands.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326, stringsAsFactors = T, quiet=TRUE)
    landscapes = st_transform(landscapes,4326)
    roads=st_read("./data/GoTRelease/Roads.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326, stringsAsFactors = F, quiet=TRUE)
    
    colforest="#c0d7c2"
    colriver="#7ec9dc"
    colriver="#87cdde"
    colland="ivory"
    borderland = "ivory3"
    
    main_char= cc
    landpol = st_union(st_geometry(land)) 
    islandpol = st_union(st_geometry(islands))
    backpol=st_union(landpol,islandpol)
    background = st_as_sf(data.frame(name=main_char,geometry=rep(backpol,6)))
    
    loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
    loc_time_mc = scenes_locations %>% left_join(loc_time)
    
    
    
    
    
    mapp=ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
      geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
      geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
      coord_sf(expand = 0,ndiscr = 0)+
      scale_color_discrete(guide="none")+
      scale_size_area("Durée (min) :",max_size = 12,breaks=c(30,60,120,240))+
      
      theme(panel.background = element_rect(fill = colriver,color=NA),
            text = element_text(family="Palatino",face = "bold",size = 14),
            legend.key = element_rect(fill="#ffffff"),
      ) +
      labs(title = glue("Temps de présence de {input$select}"),caption = "@comeetie, 2020",x="",y="")
    mapp
  })
  
}

#Run the application 
shinyApp(ui = ui, server = server)
library(rsconnect)
rsconnect::deployApp('C:/Users/aboubakiri.diaw/Desktop/test')

