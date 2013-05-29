

library(googleVis.Mango)
library(shiny)
googleVis.Mango:::.initializeShinyFolder()

runApp(system.file("examples", "shiny", "gvisMotionChart", package = "googleVis.Mango"), port = 8100, launch.browser = TRUE)

runApp(system.file("examples", "shiny", "MotionChart", package = "googleVis.Mango"), port = 8100, launch.browser = TRUE)

runApp(system.file("examples", "shiny", "StackChart", package = "googleVis.Mango"), port = 8100, launch.browser = TRUE)

runApp(system.file("examples", "shiny", "Corrplot", package = "googleVis.Mango"), port = 8100, launch.browser = TRUE)

runApp(system.file("examples", "shiny", "ClusterChart", package = "googleVis.Mango"), port = 8100, launch.browser = TRUE)

runApp(system.file("examples", "shiny", "h7n9_Vis", package = "googleVis.Mango"), port = 8100, launch.browser = TRUE)


