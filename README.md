📊 Application Covid-19
🏷️ Auteurs
Adama SANOGO & Ditchaba YEO

📅 Date de création
21 mai 2024

📌 Description du projet
Cette application vise à fournir un outil interactif et intuitif pour analyser et visualiser l’évolution de la pandémie de Covid-19 à différents niveaux : mondial, continental, régional et national.

L'application permet aux utilisateurs de :
✅ Suivre les cas confirmés, décès, guérisons et cas actifs. 
✅ Explorer des graphiques et cartes interactives pour visualiser la propagation du virus. 
✅ Analyser les tendances et modéliser l'évolution de la pandémie pour prendre des décisions informées en santé publique.

📂 Description des données
Le jeu de données utilisé contient les informations suivantes : 
🔹 Province/State : États fédéraux ou territoires extérieurs. 
🔹 Country/Region : Pays du monde. 
🔹 Lat & Long : Coordonnées géographiques. 
🔹 Date : Dates d’enregistrement des données. 
🔹 Confirmed : Nombre de cas confirmés. 
🔹 Deaths : Nombre de décès. 
🔹 Recovered : Nombre de guérisons. 
🔹 Active : Nombre de cas encore actifs. 
🔹 WHO Region : Classification régionale de l’OMS.

⚙️ Traitement des données
Quelques étapes importantes réalisées pour nettoyer et analyser les données :

Repositionnement de la variable Date avec relocate()

Suppression des valeurs manquantes pour Province/State

Regroupement des données par continent et pays pour une meilleure lisibilité

Création de visualisations interactives avec Leaflet et ggplot2


🎨 Visualisation des données
L'application propose une interface utilisateur intuitive avec : 
✅ Menus horizontaux pour naviguer entre les sections. 
✅ Filtres et options avancées pour affiner l'affichage des données. 
✅ Graphiques et cartes interactives facilitant l'exploration des statistiques.

🔗 Lien vers l'application : https://yditchaba.shinyapps.io/App_Shiny/ 
📂 Source des données : https://www.kaggle.com/datasets/imdevskp/corona-virus-report
