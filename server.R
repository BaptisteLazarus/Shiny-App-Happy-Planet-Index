library(shiny)

server <- function(input, output) {
  
  output$tablee <- renderDataTable({
    dataCluster = read_xlsx(file_path, sheet = "Feuil1")
  })
  
  output$gg_americas <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_asia <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_europe <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_eastafrica <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_post <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_subsah <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$sum_region <- renderPrint({
    table(data$Region)
  })
  
  output$sum_avglife <- renderPrint({
    summary(data$AverageLifeExpectancy)
  })
  
  output$sum_avgwlb <- renderPrint({
    summary(data$`AverageWellbeing
            `)
  })
  
  output$sum_happy <- renderPrint({
    summary(data$HappyLifeYears)
  })
  
  output$sum_foot <- renderPrint({
    summary(data$`Footprint(gha/capita)`)
  })
  
  output$sum_ineq <- renderPrint({
    summary(data$InequalityofOutcomes)
  })
  
  output$sum_happyplanet<- renderPrint({
    summary(data$HappyPlanetIndex)
  })
  
  output$sum_gdp<- renderPrint({
    summary(data$`GDP/capita
            ($PPP)`)
  })
  
  output$sum_pop<- renderPrint({
    summary(data$Population)
  })
  
  output$capture <- renderPrint({
    groupes.kmeans <- kmeans(data.cr,centers=2,nstart=5)
    print(groupes.kmeans)
    print(table(groupes.cah,groupes.kmeans$cluster))
  })
  
  output$plotRegion <- renderPlot({
    plotRegion = barplot(plotRegion, main = "Répartition géographique des pays de l'étude", xlab = "Région du globe", ylab = "Nombre de pays")
  })
  output$plotLife <- renderPlot({
    plotLife = boxplot(data$AverageLifeExpectancy, main = "Distribution de l'espérance de vie", ylab = "Années")
  })
  output$plotWell <- renderPlot({
    plotWell = boxplot(data$AverageWellbeing, main = "Distribution du bien ètre", ylab = "Note sur 10")
  })
  output$plotHappy <- renderPlot({
    plotHappy = boxplot(data$HappyLifeYears, main = "Distribution de l'espérance de vie heureuse", ylab = "Années")
  })
  output$plotFoot <- renderPlot({
    plotFoot = boxplot(data$Footprint, main = "Distribution de l'empreinte écologique", ylab = "hectar global par habitant")
  })
  output$plotInegal <- renderPlot({
    plotInegal = boxplot(data$InequalityofOutcomes, main = "Distribution des inégalités en espérance de vie et bien ètre", ylab = "% d'inégalités")
  })
  
  output$plotPlanet <- renderPlot({
    plotPlanet = boxplot(data$HappyPlanetIndex, main = "Distribution de l'indice de bonheur", ylab = "Indice de bonheur")
  })
  output$plotPIB <- renderPlot({
    plotPIB = boxplot(data$GDPpercapita, main = "Distribution du PIB par habitant", ylab = "PIB par habitant")
  })
  output$plotPopulation <- renderPlot({
    plotPopulation = boxplot(data$Population, main = "Distribution des populations", ylab = "Nombre d'habitants")
  })
  output$plotCompareLife <- renderPlot({
    plotCompareLife = boxplot(dataPlot$AverageLifeExpectancy~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"),xlab = "Région", ylab="Espérance de vie")
  })
  output$plotCompareWell <- renderPlot({
    plotCompareWell = boxplot(dataPlot$AverageWellbeing~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"), xlab = "Région", ylab = "Bien-être (note sur 10)")
  })
  output$plotCompareFoot <- renderPlot({
    plotCompareFoot = boxplot(dataPlot$Footprint~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"), xlab = "Région", ylab = "Empreinte écologique")
  })
  output$plotComparePlanet <- renderPlot({
    plotComparePlanet = boxplot(dataPlot$HappyPlanetIndex~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"), xlab = "Région", ylab = "Indice de bonheur")
  })
  
  # image1 creates a new PNG file each time Radius changes
  #output$capture <- renderImage({
  # Get width and height of image1
  # width  <- 919
  #height <- 160
  
  # A temp file to save the output.
  # This file will be automatically removed later by
  # renderImage, because of the deleteFile=TRUE argument.
  #outfile <- tempfile(fileext = "Capture.PNG")
  
  # Generate the image and write it to file
  #x <- matrix(rep((0:(width-1))/(width-1), height), height,
  #           byrow = TRUE)
  #y <- matrix(rep((0:(height-1))/(height-1), width), height)
  #gauss2d <- function(x, y, r = 0.15) {
  # exp(
  #  -((x - 0.5)^2 + (y - 0.5)^2) /
  #   (2 * r^2)
  #  )
  #}
  #pic <- gauss2d(x, y)
  #writePNG(pic, target = outfile)
  
  # Return a list containing information about the image
  #list(src = "Capture.PNG",
  #    contentType = "image/png",
  #   width = width,
  #  height = height,
  # alt = "capture")
  
  #}, deleteFile = F)
  
  output$ACP <- renderPlot({
    plotACP = plot(res, choix="var")
  })
  output$dendogramme <- renderPlot({
    plotCluser = plot(cah.ward)
  })
  output$solutions <- renderPlot({
    plotsolution = plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")
  })
  
  output$causes <- renderPlot({
    ggplot(data2, aes(x=data2$Footprint, y=data2$`HPI Rank`, fill = data2$Region, size = data2$Population)) +
      geom_point(shape=21) +
      labs(x = "Emprunte écologique nationale",
           y = "HPI Rank",
           title = "Emprunte écologique dans le monde") +
      theme_light()
  })
  
  output$map2 <- renderLeaflet({
    carte2
  })  
  output$map <- renderLeaflet({
    carte
  })
}

