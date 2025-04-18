#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage(
    theme = shinytheme("flatly"), #flatly, cerulean, journal, united
    title = div(img(src = "images_covid.png", height = "28px",width = "32px"," COVID-19")),
    collapsible = TRUE,
    tabPanel(title =tagList(icon("home"),"Accueil"),
             hr(width="950px"),
             div(
               h1(
                 "Bienvenu(e) sur l'application COVID-19"
                 , style = "color : #0099ff; text-align:center")
             ),
             hr(width="800px"),
             div(
               p(
                 align="center",
                 img(src="images_covid2.png", 
                     title="logo de COVID-19", width = "30%", height = "40%"
                 ),
               )
             ),
             hr(width="950px"),
             div(
               box( title = "Introduction",
                    p(
                      "Bienvenue sur notre application COVID-19. Cette application a été conçue pour fournir
                 des informations à jour et des analyses sur la pandémie de COVID-19 qui a touché le monde 
                 entier. Notre objectif est de fournir un outil utile pour comprendre la propagation du virus,
                 suivre les tendances et les statistiques,et prendre des décisions informées pour la santé 
                 publique."
                    )
               ),
               box(title = "Informations générales sur la pandémie",
                   p(
                     "La pandémie de COVID-19, causée par le coronavirus SARS-CoV-2, a émergé pour la première
                   fois à Wuhan, en Chine, en décembre 2019. Depuis lors, elle s'est propagée dans le monde 
                   entier, entraînant des perturbations massives dans la vie quotidienne, des décès tragiques 
                   et des défis sans précédent pour les systèmes de santé et les économies mondiales."
                   )
               ),
               box( title = "Objectif de l'application",
                    p(
                      "Notre application vise à fournir plusieurs fonctionnalités clés :",
                      tags$ul(
                        tags$li("Suivi des cas confirmés, des décès et des guérisons dans le monde entier."),
                        tags$li("Visualisation des données sur les cartes et les graphiques pour mieux comprendre
                             la propagation du virus."),
                        tags$li("Analyse des tendances et des modèles pour aider à prévoir les futures évolutions 
                             de la pandémie.")
                      )
                    )
               ),
               box(title = "Comment utiliser l'application ?",
                   p(
                     "Très simple et intuitif. Utilisez les onglets et les menus pour accéder aux différentes 
                sections, telles que les cartes interactives, les graphiques de tendance et les analyses 
                détaillées. Vous pouvez également utiliser les filtres et les options pour affiner les données
                affichées selon vos besoins.
                Nous vous encourageons à explorer les différentes fonctionnalités de l'application et à utiliser
                les informations fournies de manière responsable pour vous protéger et protéger les autres.
                Et aussi de nous laisser vos avis en commentaire."
                   )
               ),
             ),
    ),
    nav_menu(
      title=tagList(icon("database"),"Base de Données"),
      nav_panel(
        "Bases de données",
        sidebarLayout(
          sidebarPanel(
            br(),
            br(),
            checkboxGroupInput("columns", "Choix des Colonnes à afficher :",
                               choices = colnames(world_covid_19),
                               selected = colnames(world_covid_19)),
            br(),
            fluidRow(
              column(2),
              column(8,
                     actionButton("show_data", "Afficher"),),
              column(2),
            ),
            
            br(),
            fluidRow(
              column(3, downloadButton("download_csv", "CSV")),
              column(3, downloadButton("download_excel", "Excel")),
              column(3, downloadButton("download_pdf", "pdf"))
            ),
          ),
          
          mainPanel(
            h1("DATABASE",style = "color : #0099ff; text-align:center"),
            h4(textOutput("show_text_data"),style = "text-align:center"),
            br(),
            dataTableOutput("data_table")
          )
        )
      ),
      nav_panel(
        "Résumé Statistique",
        sidebarPanel(
          br(),
          selectInput("variable_selection", "Sélectionnez les variables à comparer :",
                      choices = colnames(world_covid_19), multiple = TRUE),
          br(),
          fluidRow(
            column(1),
            column(6),
            column(5,),
          ),
          actionButton("show_summary", "Afficher résumé"),
          actionButton("clean_summary", "Réinitialiser")
        ),
        
        mainPanel(
          br(),
          br(),
          h4(textOutput("show_text"),style = "color : #0099ff; text-align:center"),
          verbatimTextOutput("summary_output"),
          
        )
      )
                #verbatimTextOutput("summary_output"))
    ),
    nav_menu(
      title = tagList(icon("search"),"Rechercher"),
      nav_panel(
        "Monde",
        fluidRow(
          column(
            1,
          ),
          column(1),
          column(
            8,
            h1("Le nombre total de cas confirmés, de décès , de guérisons et d'actifs dans 
               le monde",style = "color : #0099ff;text-align:center"),
            br(),
            br(),
            box(uiOutput("box_world0")),
            box(uiOutput("box_world1")),
            br(),
            br(),
            br(),
            br(),
            box(uiOutput("box_world2")),
            box(uiOutput("box_world3")),
          ),
          column(2)
        )
        ),
      nav_panel(
        "Par Continent",
        nav_panel(
          "Monde",
          sidebarLayout(
            sidebarPanel(
              br(),
              selectInput("selecteur1",h4("Choisir un continent:"), choices = c(data_region$`WHO Region`)),
              br(),
              actionButton("sum_continent", "RECHERCHER")
            ),
            mainPanel(
              h1("Le nombre total de cas confirmés, de décès , de guérisons et d'actifs du continent:",
                 textOutput("name_continent"),
                 style = "color : #0099ff; text-align:center"),
              br(),
              br(),
              br(),
              box(uiOutput("box_continent0")),
              box(uiOutput("box_continent1")),
              br(),
              br(),
              br(),
              br(),
              box(uiOutput("box_continent2")),
              box(uiOutput("box_continent3")),
            ))),
        ),
      nav_panel(
        "Par Pays",
        sidebarLayout(
          sidebarPanel(
            selectInput("selecteur2","Choisir un pays :", choices = c(unique(data_covid_19$`Country/Region`))),
            br(),
            actionButton("sum_country", "RECHERCHER")),
          mainPanel(
            h1("Le nombre total de cas confirmés, de décès , de guérisons et d'actifs du pays:",
               textOutput("name_countrie"),style = "color : #0099ff; text-align:center"),
            br(),
            br(),
            box(uiOutput("box_country0")),
            box(uiOutput("box_country1")),
            br(),
            br(),
            br(),
            br(),
            box(uiOutput("box_country2")),
            box(uiOutput("box_country3"))),
          )
          ),
     ),
    nav_menu(
      title = tagList(icon("map-marker-alt"),"Carte"),
      # Carte des cas confirmés pour le monde entier
      nav_panel(
        "Du Monde",
        fluidRow(
          column(1),
          column(10,
                 h1("La carte du monde des cas confirmés", style = " text-align:center"),
                 br(),
                 card( card_body(leafletOutput("mapworld")))
                 ),
          column(1),
        )
      ),
      # Carte des cas confirmés pour un continent précis
      nav_panel(
        " D'un Continent/Region",
        sidebarLayout(
          sidebarPanel(
            br(),
            selectInput("selecteur", "Choisir un continent:", choices = c("",data_region$`WHO Region`)),
            br(),
            actionButton("go_map", "RECHERCHER")),
          mainPanel(
            card(
              card_header(h1("Les différents continents affectés",style = " text-align:center")),
              card_body(leafletOutput("map_region"))
            ))
          )),
      # Carte des cas confirmés pour un pays précis
      nav_panel(
        "D'Un Pays ",
        sidebarLayout(
          sidebarPanel(
            br(),
            selectInput("country","Choisir un pays:", choices =c("",unique(data_covid_19$`Country/Region`))),
            br(),
            actionButton("go_country","RECHERCHER")
          ),
          mainPanel(
            card(
              card_header(h1("La carte" ,textOutput("country"),style = " text-align:center")),
              br(),
              card_body(leafletOutput("map_country")))
          )
          )
      ),
    ),
    tabPanel(
      title=tagList(icon("chart-bar"), " Graphiques"),
      navset_card_pill(
        nav_panel(
          "World",
          br(),
         column(
           12,
           column(
             2,
             div(
               id="radioContainer",
               sidebar(
               position = "left",
               useShinyjs(),
                 id="radioContainer",
                 radioButtons(inputId = "variable", label = "Choisir une Variable : ",
                              choices = c("Voir Tout","Cas confirmés","Décès","Guéris","Actifs")),
                 #actionButton("go_graphs", "Mise à jour"),
               ),
             ),
             div(
               id="side",
               sidebar(
                 "",
                 position = "right",
                 useShinyjs(),
                 checkboxGroupInput(
                   "checkGroup",
                   "Selectionner les variables à visionner:",
                   choices = list("Confirmed", "Deaths", "Recovered", "Active"),
                   selected = "Confirmed"
                 )
               )
             ),
             div(
               id="side_01",
               position = "left",
               useShinyjs(),
               radioButtons(inputId = "variable_circle", label = "Choisir une Variable : ",
                            choices = c("Cas confirmés","Décès","Guéris","Actifs")),
             )
           ),
           column(
             10,
             mainPanel(
               navset_card_pill(
                 id="graph_world",
                 nav_panel("Histogramme", 
                           br(),
                           plotlyOutput("band_chart_world")),
                 nav_panel("Boîte à Moustaches",
                           br(),
                           plotlyOutput("boxplot_world")),
                 nav_panel("Nuage de points",
                           br(),
                           plotlyOutput("scatterplot")
                           ),
                 nav_panel("Courbe d'évolution",
                           br(),
                           plotlyOutput("covid_plot"),
                           ),
                 nav_panel("Diagramme circulaire",
                           br(),
                           plotlyOutput("circle_chart")
                           )
               )
             )
           ),
           ),
         ),
        nav_panel(
          "Time",
          br(),
          column(
            12,
          column(
            3,
            div(
              id="container0",
              sidebar(
                position = "left",
                useShinyjs(),
                id="radioContainer",
                radioButtons(inputId = "variables", label = "Choisir une Variable : ",
                             choices = c("Cas confirmés","Décès","Guéris","Actifs")),
                #actionButton("go_graphs", "Mise à jour"),
              ),
            ),
            div(
              id="container1",
              wellPanel(
                "",
                position = "right",
                br(),
                checkboxGroupInput(
                  "checkGroups",
                  "Selectionner les variables à visionner:",
                  choices = list("Confirmed", "Deaths", "Recovered", "Active"),
                  selected = "Confirmed"
                )
              )
            ),
            div(
              id="container2",
              br(),
              sidebar(
                position = "left",
                useShinyjs(),
                id="radioContainer",
                radioButtons(inputId = "var", label = "Choisir une Variable : ",
                             choices = c("Tout","Cas confirmés","Décès","Guéris","Actifs")),
                #actionButton("go_graphs", "Mise à jour"),
              ),
            )
          ),
          column(
            9,
            mainPanel(
              navset_card_pill(
                id="graph_date",
                nav_panel("Histogramme",
                          br(),
                          plotlyOutput("band_chart_date")
                          ),
                nav_panel("Boîte à Moustaches",
                          br(),
                          plotlyOutput("box_date")),
                nav_panel("Courbe d'évolution",
                          br(),
                          plotlyOutput("covid_plot1")),
                nav_panel("Diagramme circulaire",
                          br(),
                          plotlyOutput("circle_date"))
              )
            )
          ))
          ),
        #nav_panel("Country")
      )
    ),
    nav_menu(
      title=tagList(icon("chart-line"), " Prévisions"),
      nav_panel(
        "Par continent",
        sidebarLayout(
          sidebarPanel(
            selectInput("continent", "Sélectionner un continent :",
                        choices = sort(unique(data_covid_19$`WHO Region`))),
            dateInput("dates", "Sélectionner une Date:", value = Sys.Date(), 
                      min = Sys.Date() , max = "20100-12-31" ),
            actionButton("predicts_continent", "Prédire")
          ),
          mainPanel(
            plotlyOutput("predictionPlot_continent"),
            verbatimTextOutput("predictionResults_continent")
          )
        )
        ),
      nav_panel(
        "Par Pays",
        sidebarLayout(
          sidebarPanel(
            # la sélection d'un pays
             selectInput("countrys", "Sélectionner un pays:", 
                         choices = sort(unique(data_covid_19$`Country/Region`))), 
             dateInput("date", "Sélectionner une Date:", value = Sys.Date(), 
                       min = Sys.Date() , max = "20100-12-31" ),
             #colourInput(inputId="color", "Choose Color for Plot:", value = "blue"),
              actionButton("predict", "Prédire")
                  ),
                  
                  mainPanel(
                    plotlyOutput("predictionPlot"),
                    h3(verbatimTextOutput("predictionResult"), style = "text-align:center")
                  )
                )),
    ),
    tabPanel(
      title=tagList(icon("comments"), " Commentaires"),
      sidebarLayout(
        sidebarPanel(
          width = 5,
          h2("Laissez-nous un commentaire"),
          textInput("first_name", "Prénom :"),
          textInput("last_name", "Nom :"),
          textAreaInput("comment_input", "Entrez votre commentaire :"),
          actionButton("submit_comment", "Soumettre")
        ),
        mainPanel(
          width=7,
          h4("Nombre de commentaires :"),
          verbatimTextOutput("comment_count"),
          h3("Les Commentaires :"),
          uiOutput("comment_output"),
          verbatimTextOutput("status_message")
        )
      )
    ),
    nav_spacer(),
    nav_menu(
      title = tagList(icon("info-circle"),"À Propos"),
      nav_panel(
        "Application",
        fluidRow(
          column(1),
          column(10,
                 h3("À propos de cette application"),
                 p("l'application Les COVID-19 est un projet de visualisation réalisé dans le cadre d'un projet de fin
                 d'étude du cours R Avancés en Master 1 de Data Science.
                 Elle fournit des informations sur les différents cas de COVID-19 dans le monde ainsi que les décès 
                 suite à ce virus, les guérisons et les personnes actives par pays et par continent.
                 Elle permet également aux utilisateurs de soumettre et afficher des commentaires."),
                 p("L'application utilise les données les plus récentes pour fournir des prédictions 
                   sur les cas confirmés."),
                 h4("Fonctionnalités :"),
                 tags$ul(
                   tags$li("Affichage des données COVID-19 par pays et par continent 
                           avec une carte leaflet()"),
                   tags$li("Visualisation de la Base de données du COVID-19 et le resumé statistique"),
                   tags$li("Visualisation des graphiques à savoir"),
                   tags$li("Prédiction des cas confirmés"),
                   tags$li("Soumission et affichage de commentaires")
                 ),
                 p("Cette application a été développée pour démontrer les capacités de Shiny 
                   en matière de visualisation de données et d'interaction utilisateur."),
                 h4("Les liens de formation"),
                 tags$ol(
                   tags$li(h5("Liens de pages HTML")),
                 tags$ul(
                   tags$li(tags$a(href ="https://shiny.posit.co/r/getstarted/build-an-app/customizing-ui/tabs.html",
                                  "https://shiny.posit.co/r/getstarted/build-an-app/customizing-ui/tabs.html")),
                   tags$li(tags$a(href ="https://thinkr.fr/cartographie-interactive-comment-visualiser-mes-donnees-spatiales-de-maniere-dynamique-avec-leaflet/",
                                  "https://thinkr.fr/cartographie-interactive-comment-visualiser-mes-donnees-spatiales-de-maniere-dynamique-avec-leaflet/")),
                   tags$li(tags$a(href ="https://epirhandbook.com/fr/shiny.html", 
                                  "https://epirhandbook.com/fr/shiny.html", target = "_blank")),
                   tags$li(tags$a(href="https://lrouviere.github.io/TUTO_VISU_R/",
                                  "https://lrouviere.github.io/TUTO_VISU_R/"))
                 ),
                 tags$li(
                   h5("Liens de vidéos YouTube"),
                  tags$ul(
                    tags$li(a(href="https://www.youtube.com/watch?v=So-AL21HnWU&ab_channel=YouTubeViewers",
                              "https://www.youtube.com/watch?v=So-AL21HnWU&ab_channel=YouTubeViewers")),
                    tags$li(a(href="https://www.youtube.com/watch?v=G_n2svA1-eY&t=3147s&ab_channel=J.ADATATECHCONSULTING",
                              "https://www.youtube.com/watch?v=G_n2svA1-eY&t=3147s&ab_channel=J.ADATATECHCONSULTING"))
                  )),
                 
                 )
          ),
          column(1)
        )
      ),
      nav_panel(
        "Établissement UFHB",
        column(1),
        column(
          10,
          h3(
            "Université Félix Houphouet Boigny de Cocody"
            , style = "color : #0099ff; text-align:center"),
          p(
            align="center",
            img(src="logo_UFHB.png", 
                title="logo de UFHB", width = "18%",
            ),
            br(),
            a("site de l'université UFHB",
              href="https://univ-cocody.ci/",
              target="_blank")
          )
          ,   
          h4("À propos de l'établissement UFHB :"),
          p("Créée en juillet 1959, sous la dénomination de Centre d’Enseignement 
          Supérieur d’Abidjan, et officialisée par l’arrêté du 11 septembre 1959, 
            l’Université Félix Houphouët-Boigny est une université ivoirienne, établie 
            dans un campus de 200 hectares situé au cœur de la commune de Cocody, à Abidjan.
            L’université fut, dans les années 70 et 80, très réputée en Afrique de l'Ouest 
            francophone pour ses nombreuses facultés."),
          h4("Oraganisation:"),
          p("L'université est dirigée par un président, actuellement le professeur Ballo Zié 
            (succédant à Abou Karamoko), assisté de deux vice-présidents et d'un secrétaire général17.
            Acteur primordial du développement socio-économique et culturel en Côte d'Ivoire, l'université
            Félix-Houphouët-Boigny assure des missions d'enseignement et de recherche au sein de treize unités 
            de formation et de recherche (UFR), deux centres de recherche autonomes, une école de formation 
            continue et des instituts."),
          h4("Localisation de l'université UFHB :"),
          p("Il s'agit de la carte de localisation du département Mathématique et Informatique (MI) de UFHB"),
          card(
            card_header(h5("La Carte de UFHB", style = "text-align:center")),
            br(),
            leafletOutput("school_map", width = "100%", height = "450px")),
            br(),
               ),
        column(1),
        
        p(),
      ),
      nav_panel(
        "De nous",
        h4("À propos des développeurs :"),
        p(
          "Cette application a été développée par deux jeunes étudiants de l'Université Félix Houphouet 
          Boigny de Cocody en classe de Master 1 de Data Science."
        ),
        p("Il s'agit de Ditchaba YEO Et Adama SANOGO.")
      ),
    ),
    
  ),
)
