Guide to this repository
===

This repository contains the code, files and presentation from a talk I gave at [Google DevFest Twin Cities](http://m.devfest.mn/), presenting an overview to the `shiny` package for R. The presentation goes over some basics, but the majority of the content lies in the three example `shiny` apps contained here:

- Transportation exploration: using `shiny` as a "visualization prototyping environment." Use user input to set variables used for axis, color, and point sizes.

- Interactive contour plot: shows how you could share the results of modeling/analysis work with co-workers via a `shiny` app.

- Insurance visualizer: my attempt to take the benefit data provided to me at work, and translate it into a visualization to help myself (and colleagues) make informed benefit plan choices. The data has been anonymized for sharing.


Reproducing an app
===

After you clone the repository, you'll need to install [RStudio](http://www.rstudio.com/) and any packages listed in the first lines of either the `ui.R` or `server.R` files in the app folder. At the very least, you'll need to run `install.packages("shiny")`. From there, just change your working directory and run the app!

```{r}
# using transportation as an example
setwd("/path/to/devFest-shiny/transpo-exploration")
library(shiny)
runApp()
```

That's it! A browser tab will open up, pointing to a port on http://localhost which will run the app. Fiddle around with the `ui.R` or `server.R` files, save, and refresh the browser session to see your changes.

For reference, please see any of the apps on my public RStudio server account:

- Transpo exploration on [spark.rstudio](http://spark.rstudio.com/jwhendy/transpo-exploration) or [shinyapps.io](http://jwhendy.shinyapps.io/transpo-exploration/)

- [Interactive contour](http://spark.rstudio.com/jwhendy/interactive-contour/)

- [Insurance vizualizer](http://spark.rstudio.com/jwhendy/insurance-visualizer/)

Please see the last few pages of the presentation (.pdf) for helpful references/places to get help.