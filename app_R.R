library(shiny)
library(dplyr)
Acteur <- read.csv("Acteur.csv", encoding="UTF-8",sep=";")
Jouer <- read.csv("Jouer.csv", encoding="UTF-8",sep=";")
Titre <- read.csv("Titre.csv",encoding="UTF-8",sep=";")
Realiser <- read.csv("Realiser.csv",encoding="UTF-8",sep=";")
Realisateur <- read.csv("Realisateur.csv",encoding="UTF-8",sep=";")

shinyApp(

  ui = fluidPage(
    #shinythemes::themeSelector(),"shinythemes",
      titlePanel("Films par annee par acteur"),
      
      
      #filtres
      sidebarPanel(
        selectInput("choix_acteur", "Acteur : ", 
                    choices=Acteur$Acteur),
        selectInput("choix_film", "Film : ", 
                    choices=Titre$Titre),
        selectInput("choix_rea", "Realisateur : ", 
                    choices=Realisateur$Realisateur),
        hr(),
        helpText("Source : Kaggle, Netflix")
      ),
      
      
      tabPanel("1",
               mainPanel(
                 tabsetPanel(
                   #Onglet Acteur
                   tabPanel("Acteur",
                            #Affiche barplot "filmplot" déclaré dans la partie server
                            plotOutput("filmplot")
                   ),
                   #Onglet Film
                   tabPanel("Film", "Liste des acteurs ayant joué dans ce film :",
                            #Affiche la liste "liste_acteurs" déclarés dans la partie server
                            textOutput("liste_acteurs")
                            ),
                   #Onglet Realisateur
                   tabPanel("Realisateur", "Liste des films du réalisateur",
                            #Affiche la liste "liste_films" déclarés dans la partie server
                            textOutput("liste_films"),
                            #Affiche barplot "films_annee" déclaré dans la partie server
                            plotOutput("films_annee")
                            )
                 )
               )
      ),
  ),
  
#
#Partie Serveur : 
#
  server = function(input, output) {
    
    #Onglet Acteur
    output$filmplot <- renderPlot({
      #filtre les films de l'acteur sélectionné
      choix_acteur= input$choix_acteur
      Acteur_filtre= filter(Acteur, Acteur == choix_acteur)
      Jouer_filtre = left_join(x = Acteur_filtre, y = Jouer, 
                               by = c("ID_Acteur"="ID_acteur"))
      Jouer_filtre$ID_titre = as.integer(Jouer_filtre$ID_titre)
      Film_filtre = left_join(x=Jouer_filtre, y= Titre, by=c("ID_titre"="ID_Titre"))
      # Barplot du nombre de films par année
      barplot(height = table(Film_filtre$Annee_Sortie),
              main=input$choix_acteur,
              ylab="Nombre de film",
              xlab="Annees", col= "lightseagreen")
      
      
    })
    
    #Onglet Film
    output$liste_acteurs <- renderText({
      choix_film= input$choix_film
      #filtre les acteurs du film selectionné
      film_filtre= filter(Titre, Titre == choix_film)
      
      Jouer$ID_titre = as.integer(Jouer$ID_titre)
      
      Jouer_filtre = left_join(x = film_filtre, y = Jouer, 
                               by = c("ID_Titre"="ID_titre"))
      Acteur_filtre = left_join(x=Jouer_filtre, y= Acteur, by=c("ID_acteur"="ID_Acteur"))
      
      paste(Acteur_filtre$Acteur)
    },sep=", \n")
    
    
    #Onglet Réa
    output$liste_films <- renderText({
      #filtre les films du réalisateur selectionné
      choix_rea= input$choix_rea
      
      Realisateur_filtre= filter(Realisateur, Realisateur == choix_rea)
      
      Realiser_filtre = left_join(x = Realisateur_filtre, y = Realiser, 
                                  by = c("ID_Real"="ID_realisateur"))
      Film_filtre = left_join(x=Realiser_filtre, y= Titre, 
                              by=c("ID_titre"="ID_Titre"))
      # Affiche les acteurs ayant joué dans ce film
      print(Film_filtre$Titre)
    },sep=", \n")
    
    
    #Onglet Rea
    output$films_annee <- renderPlot({
      #filtre les films du réalisateur selectionné
      choix_rea= input$choix_rea
      
      Realisateur_filtre= filter(Realisateur, Realisateur == choix_rea)
      
      Realiser_filtre = left_join(x = Realisateur_filtre, y = Realiser, 
                                  by = c("ID_Real"="ID_realisateur"))
      Film_filtre = left_join(x=Realiser_filtre, y= Titre, 
                              by=c("ID_titre"="ID_Titre"))
      #Affiche barplot du nombre de films par année
      barplot(height = table(Film_filtre$Annee_Sortie),
              main=input$choix_rea,
              ylab="Nombre de film",
              xlab="Annees", col= "darkred")
    })
})

