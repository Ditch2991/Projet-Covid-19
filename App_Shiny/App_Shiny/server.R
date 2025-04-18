#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #############################################################################
  commentaires <- reactiveValues(liste = character(0))
  ##############################################################
  #### Extraction du continent précis dans BD
  filtered_data <- reactive({
    #req(input$rechercher)
    filter(data_covid_19 ,`WHO Region`==input$selecteur)
  })

  ##############################################################
  filtered_data_country <-reactive({
    data_covid_19 |>
    filter(`Country/Region`==input$country)|>
    summarise(
      latitude = mean(Lat, na.rm = TRUE),
      longitude = mean(Long, na.rm = TRUE))
  })
  ###############################################################
  filtered_continent <- reactive({
    data_region |> 
      filter(`WHO Region`==input$selecteur1)
  })
  ######################################################################
  filtered_country <- reactive({
    data_01 |>
      filter(`Country/Region`==input$selecteur2)
  })
    ################################################################
    # summary
    output$summary <- renderPrint({
      summary(data_covid_19)
    })
    ################################################################
    # table
    output$table <- renderDataTable({
      data_covid_19
    })
    #################################################################
    # la carte du monde des cas confirmés
    output$mapworld <- renderLeaflet({
      ma_carte_01
    })
    ###############################################################
    # la carte simple
    output$map <- renderLeaflet({
      ma_carte_00 
    })
    
    output$sum <- renderPrint({
      input$ma_carte_base_layers
    })
    #################################################################"
    # la carte d'un continent
    output$map_region <- renderLeaflet({
      # Mise à jour de la carte grâce à l'action bouton rechercher
      input$go_map
      isolate({
        #la longitude de setview
        longitude <-0
        #la latitude de setview
        latitude <- 0
        filtered <- 
          filtered_data() |>
          group_by(`Country/Region`) |>
          summarise(
            Lat =mean(Lat),
            Long=mean(Long),
            Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active= sum(Active)
          )
        # la sélection de la longitude et la latitude en fonction des continents
        if (input$selecteur=="Africa") {
          longitude <-15.712238
          latitude <- 0.5238691
        }else if(input$selecteur=="Americas") {
          longitude <- -77.517460	
          latitude <- 18.6646825
        }else if(input$selecteur=="Eastern Mediterranean"){
          longitude <- 41.049556
          latitude <- 26.1617314
        }else if(input$selecteur=="Europe"){
          longitude <- 6.865596
          latitude <- 37.7650589
        }else if(input$selecteur=="South-East Asia"){
          longitude <- 93.459266
          latitude <- 13.9158153
        }else if(input$selecteur=="Western Pacific"){
          longitude <- 119.373725
          latitude <- 17.7193016
        }
        if(input$selecteur==""){
          ma_carte_00 |>
            setView(0, 0, zoom = 2)
        }else {
          ma_carte_00 |>
            #addProviderTiles("CartoDB.DarkMatter") |>
            addCircleMarkers(
              data = filtered,
              lng = ~Long,
              lat = ~Lat,
              color = "red",
              popup = ~paste(
                `Country/Region`, "<br>",
                "Cas confirmés:", Confirmed,"<br>",
                "Décès:", Deaths, "<br>",
                "Guéris:", Recovered,"<br>",
                "Cas Actifs:", Active),
              labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
              clusterOptions = markerClusterOptions(),
              radius = 3
            ) |>
            setView(lng =  longitude, lat = latitude, zoom = 3)
          }
      })
    })
    ###################################################################################################
    # Le nom du pays selectionné
    output$country <- renderText({
      input$go_country
      isolate({
        input$country
      })
    })
    ####################################################################################################
    # La carte d'un pays selectionné
    output$map_country <- renderLeaflet({
      input$go_country
      isolate({
        if (input$country==""){
          ma_carte_00
        } else{
          ma_carte_00 |>
            setView(lng = filtered_data_country()$longitude, lat = filtered_data_country()$latitude, zoom = 7)
        }
      })
    })
    #######################################################################
    # Confirmed, Deaths, Recovered and Active of world
    output$box_world0 <- renderUI({
      value_box(
        title = "Confirmed",
        value = world_data$Confirmed,
        showcase = bsicons::bs_icon("check-circle"),
        theme = "danger")
    })
    output$box_world1 <- renderUI({
      value_box(
        title = "Deaths",
        value = world_data$Deaths,
        showcase = icon("skull"),
        theme = "primary"
        )
    })
    output$box_world2 <- renderUI({
      value_box(
        title = "Recovered",
        value = world_data$Recovered,
        showcase = bsicons::bs_icon("heart"),
        theme = "success"
        )
    })
    output$box_world3 <- renderUI({
      value_box(
        title = "Active",
        value = world_data$Active,
        showcase = icon("refresh"),
        theme = "warning"
        )
    })
    #######################################################################
    # Confirmed, Deaths, Recovered and Active by continent
    output$box_continent0 <- renderUI({
      input$sum_continent
      isolate({
        value_box(
          title = "Confirmed",
          value = filtered_continent()$Confirmed_total,
          showcase = bsicons::bs_icon("check-circle"),
          theme = "danger"
        )
      })
      })
    output$box_continent1 <- renderUI({
      input$sum_continent
      isolate({
        value_box(
          title = "Deaths",
          value = filtered_continent()$Deaths_total,
          showcase = icon("skull"),
          theme = "primary"
        )
      })
    })
    output$box_continent2 <- renderUI({
      input$sum_continent
      isolate({
        value_box(
          title = "Recovered",
          value = filtered_continent()$Recovered_total,
          showcase = bsicons::bs_icon("heart"),
          theme = "success"
        )
      })
    })
    output$box_continent3 <- renderUI({
      input$sum_continent
      isolate({
        value_box(
          title = "Active",
          value = filtered_continent()$Active_total,
          showcase = icon("refresh"),
          theme = "warning"
        )
      })
    })
    #############################################################################################
    output$name_continent <- renderText({
      input$sum_continent
      isolate({
        input$selecteur1
      })
    })
    ###########################################################################################
    # Confirmed, Deaths, Recovered and Active by country
    output$box_country0 <- renderUI({
      input$sum_country
      isolate({
        value_box(
          title = "Confirmed",
          value = filtered_country()$confirmed_total,
          showcase = bsicons::bs_icon("check-circle"),
          theme = "danger"
        )
      })
    })
    output$box_country1 <- renderUI({
      input$sum_country
      isolate({
        value_box(
          title = "Deaths",
          value = filtered_country()$Deaths_total,
          showcase = icon("skull"),
          theme = "primary"
        )
      })
    })
    output$box_country2 <- renderUI({
      input$sum_country
      isolate({
        value_box(
          title = "Recovered",
          value = filtered_country()$Recovered_total,
          showcase = bsicons::bs_icon("heart"),
          theme = "success"
        )
      })
    })
    output$box_country3 <- renderUI({
      input$sum_country
      isolate({
        value_box(
          title = "Active",
          value = filtered_country()$Active_total,
          showcase = icon("refresh"),
          theme = "warning"
        )
      })
    })
    ###############################################################################
    output$name_countrie <- renderText({
      input$sum_country
      isolate({
        input$selecteur2
      })
    })
######################################################################################
    output$plot2 <- renderPlot({
      barre <-
        ggplot(data_region) + 
        aes(x= reorder(`WHO Region`,-Confirmed_total),
            y= Confirmed_total, fill = `WHO Region`) +
        geom_bar(stat = "identity") +
        labs(title = "Nombre total de cas confirmés par pays",
             x = "Continent",
             y = "Nombre total de cas confirmés") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        guides(fill = guide_legend(title = "Continent"))
      ggplotly(barre)
      
    })
#######################################################################################
    
######################## SECTION COMMENTAIRE #######################################
    # Initialiser le vecteur de commentaires
    comments <- reactiveVal(list())
    
    # Observer pour l'action de soumettre un commentaire
    observeEvent(input$submit_comment, {
      # Vérifier si les champs de nom, prénom et commentaire ne sont pas vides
      if (input$comment_input != "") {
        # Ajouter le nouveau commentaire à la liste
        comment_text <- paste(input$first_name, input$last_name, ":", input$comment_input)
        comments(c(comments(), list(comment_text)))
        
        # Réinitialiser les champs après soumission
        updateTextInput(session, "first_name", value = "")
        updateTextInput(session, "last_name", value = "")
        updateTextAreaInput(session, "comment_input", value = "")
        
        # Afficher un message de succès
        output$status_message <- renderText({
          "Commentaire soumis avec succès !"
        })
      } else {
        # Afficher un message d'erreur si l'un des champs est vide
        output$status_message <- renderText({
          "Veuillez remplir tous les champs avant de soumettre le commentaire."
        })
      }
    })
    
    # Afficher les commentaires
    output$comment_output <- renderUI({
      if (length(comments()) > 0) {
        comment_list <- lapply(comments(), function(comment) {
          tagList(
            p(comment),
            hr()  # Ajouter une ligne horizontale entre les commentaires
          )
        })
        do.call(tagList, comment_list)
      } else {
        tags$div("Aucun commentaire pour le moment.")
      }
    })
    
    # Afficher le nombre de commentaires
    output$comment_count <- renderText({
      paste("Nombre total de commentaires :", length(comments()))
    })
    
  ###############################################################################
  ######################## LES GRAPHIQUES #######################################
    ################## GRAPHIQUE EN BANDE ################################
  output$band_chart_world <- renderPlotly({
    # Création le graphique en bande avec ggplot2
    titre <- ""
    var_y <- ""
    nom_graph<-"" 
    if(input$variable=="Voir Tout"){
      pivot <-
      data_region |>
        pivot_longer(
          cols = c(Confirmed_total,Deaths_total, Recovered_total,Active_total),
          names_to = "status",
          values_to ="nombre_status" ,
        )
      p <- ggplot(pivot, aes(x = `WHO Region`, y = nombre_status, fill = status)) +
        geom_bar(stat = "identity") +
        labs(title = "Les différents états par région de l'OMS", 
             '<br>',
             x = "Région de l'OMS", y = "états") +
        theme_minimal()
    } else{
      if (input$variable=="Décès"){
        titre <-"Décès par région de l'OMS"
        var_y <- data_region$Deaths_total
        nom_graph<- "Nombre de décès"
      } else if(input$variable=="Cas confirmés"){
        titre <- "Cas confirmés par région de l'OMS"
        var_y <- data_region$Confirmed_total
        nom_graph<- "Nombre de cas confirmés"
      } else if(input$variable=="Guéris") {
        titre <- "Guérisons par région de l'OMS"
        var_y <- data_region$Recovered_total
        nom_graph<- "Nombre de guérisons"
      } else if(input$variable=="Actifs"){
        titre <- "Actifs par région de l'OMS"
        var_y <- data_region$Active_total
        nom_graph<- "Nombre d'actifs"
      }
      p <- ggplot(data_region, aes(x = `WHO Region`, y = var_y, fill = `WHO Region`)) +
        geom_bar(stat = "identity") +
        labs(title = titre, 
             '<br>',
             x = "Région de l'OMS", y = nom_graph) +
        theme_minimal()
    }
    # Convertir le graphique ggplot en graphique interactif avec ggplotly
    ggplotly(p)
  })
###########################################################################################################
  ########################################### BOXPLOT DU MONDE #############################################
  output$boxplot_world <- renderPlotly({
    # Créaction du graphique en boîte à moustaches avec ggplot2
    titre <- ""
    var_y <- ""
    nom_graph<-""
    name <- ""
    couleur <- ""
    if(input$variable=="Voir Tout"){
      plot_ly(data_region, type = "box") |>
        add_boxplot(y = ~Deaths_total,
              name = "Décès",
              boxmean = TRUE) |>
        add_boxplot(y = ~Recovered_total,
                    name= "Guérisons",
                    boxmean = TRUE) |> 
        add_boxplot(y = ~Confirmed_total,
                    name= "Cas confirmés",
                    boxmean = TRUE) |>
        add_boxplot(y = ~Active_total,
                    name= "Actifs",
                    boxmean = TRUE) |>
        layout(title = "Boîte à moustaches des cas COVID-19",
               yaxis = list(title = "Nombre de différents cas"))
    } else{
      if (input$variable=="Décès"){
        titre <-"Décès par région de l'OMS"
        var_y <- data_region$Deaths_total
        nom_graph<- "Nombre de décès"
        name <- "Décès"
        couleur <- "grey"
      } else if(input$variable=="Cas confirmés"){
        titre <- "Cas confirmés par région de l'OMS"
        var_y <- data_region$Confirmed_total
        nom_graph<- "Nombre de cas confirmés"
        name <- "Cas confirmés"
        couleur <- "red"
      } else if(input$variable=="Guéris") {
        titre <- "Guérisons par région de l'OMS"
        var_y <- data_region$Recovered_total
        nom_graph<- "Nombre de guérisons"
        name <- "Guéris"
        couleur <- "green"
      } else if(input$variable=="Actifs"){
        titre <- "Actifs par région de l'OMS"
        var_y <- data_region$Active_total
        nom_graph<- "Nombre d'actifs"
        name <- "Actifs"
        couleur <- "orange"
      }
      plot_ly(data_region, type = "box") |>
        add_boxplot(y = ~var_y,
                    line = list(color = couleur),
                    name = name,
                    boxmean = TRUE) |>
        layout(title = titre,
               yaxis = list(title = nom_graph),
               size=12,
               showlegend = FALSE
               )
    }
    # Convertir le graphique ggplot en graphique interactif avec ggplotly
    #ggplotly(p)
    
  })
  ##################################################################################
  #############################Nuage de points#####################################
  output$scatterplot <- renderPlotly({
    # Créer le nuage de points avec ggplot2
    p <- ggplot(data_region, aes(x = `WHO Region`, y = Deaths_total, 
                             color = `WHO Region`, size = Active_total)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Nuage de points des décès par région de l'OMS",
           x = "Cas confirmés", y = "Nombre de décès") +
      theme_minimal()
    
    # Convertir le graphique ggplot en graphique interactif avec ggplotly
    ggplotly(p)
  })
  ########################################################################################################
  #########################CHANGEMENT DE COMPORTEMENT EN FONCTION DU GRAPHE##############################
  observeEvent(
    input$graph_world,
    {
      if(input$graph_world=="Nuage de points" || input$graph_world=="Courbe d'évolution"){
        if (input$graph_world=="Nuage de points"){
          #Cacher les 3 sidebar
          runjs('document.getElementById("radioContainer").style.display = "none";')
          runjs('document.getElementById("side").style.display = "none";')
          runjs('document.getElementById("side_01").style.display = "none";')
          
        } else if(input$graph_world=="Courbe d'évolution"){
          runjs('document.getElementById("radioContainer").style.display = "none";')
          runjs('document.getElementById("side").style.display = "block";')
          runjs('document.getElementById("side_01").style.display = "none";')
        }
        
      } else if(input$graph_world=="Diagramme circulaire"){
        runjs('document.getElementById("side_01").style.display = "block";')
        runjs('document.getElementById("radioContainer").style.display = "none";')
        runjs('document.getElementById("side").style.display = "none";')
      }else{
        #Cacher le sidebar de l'id "side" seulement
        runjs('document.getElementById("radioContainer").style.display = "block";')
        runjs('document.getElementById("side").style.display = "none";')
        runjs('document.getElementById("side_01").style.display = "none";')
      }
     
      #updateRadioButtons(session,inputId="variable",class="disabled-radio")
      #shinyjs::disable("variable")
    }
  )
  ######################################################################################################
  ############################# LES COURBES D'EVOLUTION ###############################################
  # Créer le graphique de la courbe d'évolution des décès COVID-19
  output$covid_plot <- renderPlotly({
    
    select_data <- data_region_01[,input$checkGroup]
    # Vérifier si au moins une case à cocher est sélectionnée
    if (length(input$checkGroup) == 0) {
      # Si aucune case à cocher n'est cochée, afficher un message indiquant 
      #de sélectionner au moins une valeur
      p<-plot_ly(x = NULL, y = NULL, type = 'scatter', mode = 'markers', marker = list(color = 'transparent'))
      return(p)
    } else{
      plot <- plot_ly(data = data_region_01, x=~data_region_01$`WHO Region`)
      if("Deaths" %in% input$checkGroup){
        plot <- add_lines(plot, y = ~Deaths, type = 'scatter', mode = 'lines', 
                          name = 'Deaths', color = I("grey"), colors = colors)
      }
      if("Confirmed" %in% input$checkGroup){
        plot <- add_lines(plot, y = ~Confirmed, , color = I("red"), type = 'scatter', mode = 'lines',
                          name = 'Confirmed', colors = colors)
      }
      if("Recovered"%in% input$checkGroup){
        plot <- add_lines(plot, y = ~Recovered, color = I("green"), type = 'scatter', mode = 'lines', 
                          name = 'Recovered', colors = colors)
      }
      if("Active" %in% input$checkGroup){
        plot <- add_lines(plot, y = ~Active, color = I("orange"), type = 'scatter', mode = 'lines', 
                          name = 'Active', colors = colors)
      }
      plot <- plot |> 
        layout(
          br(),
          title = "Évolution des cas COVID-19",
          xaxis = list(title = "Region"),
          yaxis = list(title = "Nombre de cas en millions"))
      plot
      
    }
  })
  ###########################################################################################################
  
  ####################################### DIAGRAMME CIRCULAIRE POUR LES CONTINENTS ##########################
  output$circle_chart <- renderPlotly({
    var <-""
    titre <-""
      if(input$variable_circle=="Cas confirmés"){
        var <- data_region$Confirmed_total
        titre <-"Répartition des cas confirmés COVID-19 par région de l'OMS"
      }else if(input$variable_circle=="Décès"){
        var <- data_region$Deaths_total
        titre <-"Répartition des Décès COVID-19 par région de l'OMS"
      }else if(input$variable_circle=="Guéris") {
        var <- data_region$Recovered_total
        titre <-"Répartition des Guérisons COVID-19 par région de l'OMS"
      } else if(input$variable_circle=="Actifs"){
        var <- data_region$Active_total
        titre <-"Répartition des Actifs COVID-19 par région de l'OMS"
      }
      plot_ly(data_region, labels = ~`WHO Region`, values = ~var, type = 'pie',
              textinfo = 'label+percent', insidetextorientation = 'radial') |>
        layout(title =titre )
  })
  
  #########################################################################################################
  observeEvent(
    input$graph_date,
    {
      if(input$graph_date=="Histogramme" || input$graph_date=="Diagramme circulaire"){
        runjs('document.getElementById("container0").style.display = "block";')
        runjs('document.getElementById("container1").style.display = "none";')
        runjs('document.getElementById("container2").style.display = "none";')
      }else if(input$graph_date=="Courbe d'évolution"){
        runjs('document.getElementById("container0").style.display = "none";')
        runjs('document.getElementById("container1").style.display = "block";')
        runjs('document.getElementById("container2").style.display = "none";')
      }
      else if (input$graph_date=="Boîte à Moustaches"){
        runjs('document.getElementById("container0").style.display = "none";')
        runjs('document.getElementById("container1").style.display = "none";')
        runjs('document.getElementById("container2").style.display = "block";')
      }
    }
  )
  ###########################################################################################################
  
  ################################ GRAPHIQUE AVEC LA DATE ###################################################
  output$band_chart_date <- renderPlotly({
    months$Month <- paste0("01/",months$Month)
    months$Month <- as.Date(months$Month, format = "%d/%m/%Y")
    months$Month <- format(months$Month, "%B")
    var_y <- ""
    titre <-""
    titre01 <- ""
    if(input$variables=="Cas confirmés"){
      var_y <- months$Confirmed
      titre <- "Cas confirmés"
      titre01 <- "Cas Confirmés COVID-19 par région de l'OMS et par date"
    } else if(input$variables=="Décès"){
      var_y <- months$Deaths
      titre <- "Décès"
      titre01 <- "Décès COVID-19 par région de l'OMS et par date"
    }
    else if(input$variables=="Guéris"){
      var_y <- months$Recovered
      titre <- "Guéris"
      titre01 <- "Guérisons COVID-19 par région de l'OMS et par date"
    }
    else if(input$variables=="Actifs"){
      var_y <- months$Active
      titre <- "Cas Actifs"
      titre01 <- "Cas Actifs COVID-19 par région de l'OMS et par date"
    }
    plot_ly(months, x = ~Month, y = ~var_y, color = ~`WHO Region`, type = 'bar') |>
      layout(title = titre01,
             xaxis = list(title = "Date"),
             yaxis = list(title = titre),
             barmode = 'group')
  })
  
  ############################## COURBE D'EVOLUTION EN FONCTION DU TEMPS #####################################
  output$covid_plot1 <- renderPlotly({
    #select_data <- data_region_01[,input$checkGroups]
    # Vérifier si au moins une case à cocher est sélectionnée
    if (length(input$checkGroups) == 0) {
      # Si aucune case à cocher n'est cochée, afficher un message indiquant 
      #de sélectionner au moins une valeur
      p<-plot_ly(x = NULL, y = NULL, type = 'scatter', mode = 'markers', marker = list(color = 'transparent'))
      return(p)
    } else{
      plot <- plot_ly(data = month, x=~Month)
      if("Deaths" %in% input$checkGroups){
        plot <- add_lines(plot, y = ~Deaths, type = 'scatter', mode = 'lines', 
                          name = 'Deaths', color = I("grey"), colors = colors)
      }
      if("Confirmed" %in% input$checkGroups){
        plot <- add_lines(plot, y = ~Confirmed, , color = I("red"), type = 'scatter', mode = 'lines',
                          name = 'Confirmed', colors = colors)
      }
      if("Recovered"%in% input$checkGroups){
        plot <- add_lines(plot, y = ~Recovered, color = I("green"), type = 'scatter', mode = 'lines', 
                          name = 'Recovered', colors = colors)
      }
      if("Active" %in% input$checkGroups){
        plot <- add_lines(plot, y = ~Active, color = I("orange"), type = 'scatter', mode = 'lines', 
                          name = 'Active', colors = colors)
      }
      plot <- plot |> 
        layout(
          br(),
          title = "Évolution des cas COVID-19",
          xaxis = list(title = "Date (Mois)"),
          yaxis = list(title = "Nombre de cas en millions"))
      plot
      
    }
  })
  ##################################### BOÎTES A MOUSTACHES POUR LA DATE ###################################
  output$box_date <- renderPlotly({
    titre <- ""
    var_y <- ""
    nom_graph<-""
    name <- ""
    couleur <- ""
    if(input$var=="Tout"){
      plot_ly(month, type = "box") |>
        add_boxplot(y = ~Deaths,
                    name = "Décès",
                    boxmean = TRUE) |>
        add_boxplot(y = ~Recovered,
                    name= "Guérisons",
                    boxmean = TRUE) |> 
        add_boxplot(y = ~Confirmed,
                    name= "Cas confirmés",
                    boxmean = TRUE) |>
        add_boxplot(y = ~Active,
                    name= "Actifs",
                    boxmean = TRUE) |>
        layout(title = "Boîte à moustaches des cas COVID-19",
               yaxis = list(title = "Nombre de différents cas"))
    } else{
      if (input$var=="Décès"){
        titre <-"Décès par date"
        var_y <- month$Deaths
        nom_graph<- "Nombre de décès"
        name <- "Décès"
        couleur <- "grey"
      } else if(input$var=="Cas confirmés"){
        titre <- "Cas confirmés par date"
        var_y <- month$Confirmed
        nom_graph<- "Nombre de cas confirmés"
        name <- "Cas confirmés"
        couleur <- "red"
      } else if(input$var=="Guéris") {
        titre <- "Guérisons par date"
        var_y <- month$Recovered
        nom_graph<- "Nombre de guérisons"
        name <- "Guéris"
        couleur <- "green"
      } else if(input$var=="Actifs"){
        titre <- "Actifs par date"
        var_y <- month$Active
        nom_graph<- "Nombre d'actifs"
        name <- "Actifs"
        couleur <- "orange"
      }
      plot_ly(data_region, type = "box") |>
        add_boxplot(y = ~var_y,
                    line = list(color = couleur),
                    name = name,
                    boxmean = TRUE) |>
        layout(title = titre,
               yaxis = list(title = nom_graph),
               size=12,
               showlegend = FALSE
        )
    }
  })
  
############################### DIAGRAMME CIRCULAIRE EN FONCTION DE LA DATE (MOIS) ###########################
  output$circle_date <- renderPlotly({
    #TRANSFOMER DES NUMEROS DE MOIS EN NOM DE MOIS
    month$Month <- format(month$Month, "%B")
    var <-""
    titre <-""
    if (input$variables=="Cas confirmés"){
        var <- month$Confirmed
        titre <-"Répartition des cas confirmés COVID-19 en fonction du temps"
      }else if(input$variables == "Décès"){
        var <- month$Deaths
        titre <-"Répartition des Décès COVID-19 en fonction du temps"
      }else if(input$variables == "Guéris") {
        var <- month$Recovered
        titre <-"Répartition des Guérisons COVID-19 en fonction du temps"
      } else if(input$variables == "Actifs"){
        var <- month$Active
        titre <-"Répartition des Actifs COVID-19 en fonction du temps"
      }
      plot_ly(month, labels = ~Month, values = ~var, type = 'pie',
              textinfo = 'label+percent', insidetextorientation = 'radial') |>
        layout(title =titre )
  })
##############################################################################################################
  
##################################SECTION PREDICTION ######################################################
  
  ########################### CONTINENT #########################################
  # Fonction pour effectuer la prédiction pour un continent spécifique
  perform_continent_prediction <- function(data, continent, date) {
    # Filtrer les données pour le continent spécifié et la date spécifiée
    filtered_data <- data %>%
      filter(`WHO Region` == continent, Date <= date)
    
    # Vérifier s'il y a suffisamment de données pour effectuer la prédiction
    if (nrow(filtered_data) < 2) {
      return("Insuffisamment de données pour prédire")
    }
    
    # Construire un modèle de régression linéaire multiple
    model <- lm(Confirmed ~ Recovered + Deaths + Active, data = filtered_data)
    
    # Créer les données futures pour la prédiction
    future_data <- data.frame(
      Recovered = mean(filtered_data$Recovered),
      Deaths = mean(filtered_data$Deaths),
      Active = mean(filtered_data$Active)
    )
    
    # Effectuer la prédiction pour la date spécifiée
    prediction <- predict(model, newdata = future_data)
    
    return(prediction)
  }
  
  # Observer pour les actions du bouton "Predicts" pour les continents
  observeEvent(input$predicts_continent,{
    # Créer un dataframe pour le traçage des données réelles
    filtered_real_data <- data_covid_19 |>
      filter(`WHO Region` == input$continent, Date <= input$date & !is.na(Confirmed))
    # Effectuer la prédiction pour le continent spécifié
    prediction <- perform_continent_prediction(filtered_real_data, input$continent, input$dates)
    # Mettre à jour l'interface utilisateur avec les résultats de la prédiction
    output$predictionResults_continent <- renderPrint({
      if (is.numeric(prediction)) {
        paste("Nombre de Cas confirmés prévu en", input$dates, "pour le continent", 
              input$continent, ":", round(prediction))
      } else {
        prediction  # Message d'erreur
      }
    })
    # Créer un dataframe pour le traçage des données réelles pour le continent
    plot_data_continent <- filtered_real_data |>
      arrange(Date) |>
      mutate(Type = "Réel") |>
      select(Date, Confirmed, Type)
    
    # Créer un dataframe pour les prédictions avec la même structure que les données réelles
    # Créer un dataframe pour les prédictions
    prediction_data <- data.frame(
      Date = input$dates,
      Confirmed = round(prediction),
      Type = "Prédit"
    )
    
    # Combiner les données réelles avec les prédictions
    combined_data <- bind_rows(plot_data_continent, prediction_data)
    
    # Tracer le graphique interactif
    output$predictionPlot_continent <- renderPlotly({
      p <- plot_ly(combined_data, x = ~Date, y = ~Confirmed, color = ~Type,
                   type = "scatter", mode = "lines+markers") |>
        layout(title = paste("COVID-19, Cas confirmés pour le continent", input$continent),
               xaxis = list(title = "Date"),
               yaxis = list(title = "Cas confirmés"))
      
      p
    })
    
  })
  
  ################################### PAYS #####################################
  
  # Fonction pour effectuer la prédiction pour le pays
  perform_prediction <- function(data, date) {
    model <- lm(Confirmed ~ Recovered + Deaths + Active, data = data)
    future_data <- data.frame(
      Recovered = mean(data$Recovered),
      Deaths = mean(data$Deaths),
      Active = mean(data$Active)
    )
    prediction <- predict(model, newdata = future_data)
    return(prediction)
  }
  # Observer pour les actions du bouton "Predict"
  observeEvent(input$predict, {
    # Filtrer les données par pays et date
    filtered_data <- data_covid_19 |>
      filter(`Country/Region` == input$countrys, Date <= input$date)
    
    # Effectuer la prédiction sur les données filtrées
    prediction <- perform_prediction(filtered_data, input$date)
    
    # Mettre à jour l'interface utilisateur avec les résultats de la prédiction
    output$predictionResult <- renderPrint({
      paste("Nombre de Cas confirmés prévu en", input$date, ":", round(prediction))
    })
    
    # Graphique des données réelles et de la prédiction
    output$predictionPlot <- renderPlotly({
      plot_data <- filtered_data |>
        arrange(Date) |>
        mutate(Type = "Réel") |>
        select(Date, Confirmed, Type)
      
      prediction_data <- data.frame(
        Date = input$date,
        Confirmed = round(prediction),
        Type = "Prédit"
      )
      
      combined_data <- rbind(plot_data, prediction_data)
      
      p <- ggplot(combined_data, aes(x = Date, y = Confirmed, color = Type)) +
        geom_line() +
        geom_point() +
        labs(title = paste("COVID-19, Cas confirmés en", input$countrys),
             x = "Date", y = "Cas confirmés") +
        theme_minimal()
      
      ggplotly(p)
    })
  })
  
  ###########################################################################################################
  
  ############################### LA CARTE DE UFHB ##########################################################
  # Afficher la carte de l'école
  output$school_map <- renderLeaflet({
    ABJ<- tibble(V="Universite Abidjan") |> tidygeocoder::geocode(V)
    content <- paste(sep = "<br/>",
                     "<b><a href='https://univ-cocody.ci/'>Université Félix Houphjouet</a></b>",
                     "UFR MI")
    leaflet(ABJ) |> addTiles() |>
      addPopups(~long,~lat, content,
                options = popupOptions(closeButton = TRUE)
      )
  })
  
  ############################################################################################################
  ###########################DATABASE ######################################################################
 
    # Filtrer les colonnes à afficher en fonction de la sélection de l'utilisateur
    output$data_table <- renderDataTable({
      input$show_data
      isolate({
        filtered_data <- world_covid_19[, input$columns, drop = FALSE]
        filtered_data
      })
      
    })
  # Exporter les données au format CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("world_covid_19", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(new_, file, row.names = FALSE)
    }
  )
  
  # Exporter les données au format PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("world_covid_19", ".pdf", sep = "")
    },
    content = function(file) {
      pdf_report <- rmarkdown::render(
        input = "report.Rmd",
        output_format = "pdf_document",
        output_file = file,
        params = list(data = data)
      )
    })
  
  # Exporter les données au format Excel
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("world_covid_19", ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(data, file)
    }
  )
  # Afficher le texte concernant les variables affichées
  observeEvent(input$show_data,{
    selected_vars <- input$columns
    text_to_swhow <- ""
    if (length(selected_vars) == 0) {
      text_to_swhow <- paste("Pas de variables sélectionnées")
    } else {
      text_to_swhow <- paste("Les variables affichées sont dans la BD:", collapse = "\n")
      for (var in selected_vars) {
        text_to_swhow <- paste(text_to_swhow , "\n", var, ",", sep = "")
      }
    }
    output$show_text_data <- renderText({
      text_to_swhow
    })
  })
  ################################ RESUME STATISTIQUE #################################
  # Observer les changements dans la sélection des variables et mettre à jour le résumé statistique
  observeEvent(input$show_summary, {
    selected_vars <- input$variable_selection
    selected_summary <- summary(world_covid_19[selected_vars])
    output$summary_output <- renderPrint({
      selected_summary
    })
  })
  # Observer les changements dans la sélection des variables et comparer les résumés statistiques
  observeEvent(input$show_summary, {
    selected_vars <- input$variable_selection
    text_to_swhow <- ""
    if (length(selected_vars) == 1) {
      text_to_swhow <- paste("Résumé statistique de la variable :", selected_vars)
    } else {
      text_to_swhow <- paste("Résumé statistique des variables:", collapse = "\n")
      for (var in selected_vars) {
        text_to_swhow <- paste(text_to_swhow , "\n", var, ",", sep = "")
      }
    }
    output$show_text <- renderText({
      text_to_swhow
    })
  })
  
  observeEvent(input$clean_summary,{
    updateTextInput(session,"variable_selection",value = "")
    output$summary_output <- renderPrint({
      "Aucune variable sélectionnée."
    })
    output$show_text <- renderText({
      ""
    })
  })
}

