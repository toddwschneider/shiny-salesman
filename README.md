A Shiny app to solve the traveling salesman problem with simulated annealing. Check out the full post here: https://toddwschneider.com/posts/traveling-salesman-with-simulated-annealing-r-and-shiny/

To run on your local machine, paste the following into your R console:

```R
install.packages(c("shiny", "maps", "geosphere"), repos="http://cran.rstudio.com/")
library(shiny)
runGitHub("shiny-salesman", "toddwschneider")
```

![](http://images.rapgenius.com/0e1ca854cbc30f33abc46108f2ba38f2.640x640x42.gif)
