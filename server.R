library(shiny)
library(maps)
library(geosphere)
source("helpers.R")

shinyServer(function(input, output, session) {
  vals = reactiveValues()
  
  map_name = reactive({
    tolower(input$map_name)
  })
  
  set_random_cities = reactive({
    input$set_random_cities + input$set_random_cities_2
  })
  
  city_choices = reactive({
    if (map_name() == "world") {
      return(all_cities)
    } else if (map_name() == "usa") {
      return(usa_cities)
    }
  })
  
  update_allowed_cities = observe({
    if (isolate(input$go_button) == 0 & isolate(set_random_cities()) == 0 & map_name() == "world") return()
    
    updateSelectizeInput(session, "cities", choices=city_choices()$full.name)
  }, priority=500)
  
  one_time_initialization = observe({
    isolate({
      cty = subset(city_choices(), full.name %in% seed_cities)
      cty$n = 1:nrow(cty)
      updateSelectizeInput(session, "cities", selected=cty$full.name)

      vals$cities = cty
      vals$distance_matrix = readRDS("data/distance_matrix.rds")
      vals$great_circles = readRDS("data/great_circles.rds")
    })
  }, priority=1000)
  
  set_cities_randomly = observe({
    if (set_random_cities() == 0 & map_name() == "world") return()
    run_annealing_process$suspend()
    
    isolate({
      if (map_name() == "world") {
        cty = generate_random_cities(n=20, min_dist=500)
      } else if (map_name() == "usa") {
        cty = generate_random_cities(n=20, min_dist=50, usa_only=TRUE)
      }
      
      cty$n = 1:nrow(cty)
      updateSelectizeInput(session, "cities", selected=cty$full.name)
      
      vals$cities = cty
    })
  }, priority=100)
  
  set_cities_from_selected = observe({
    if (input$go_button == 0) return()
    run_annealing_process$suspend()
    
    isolate({
      cty = subset(city_choices(), full.name %in% input$cities)
      if (nrow(cty) == 0 | identical(sort(cty$full.name), sort(vals$cities$full.name))) return()
      cty$n = 1:nrow(cty)
      vals$cities = cty
    })
  }, priority=50)
  
  set_dist_matrix_and_great_circles = observe({
    if (input$go_button == 0 & set_random_cities() == 0 & map_name() == "world") return()
    
    isolate({
      if (nrow(vals$cities) < 2) return()
      if (identical(sort(vals$cities$name), sort(colnames(vals$distance_matrix)))) return()
      
      dist_mat = distm(vals$cities[,c("long", "lat")]) * miles_per_meter
      dimnames(dist_mat) = list(vals$cities$name, vals$cities$name)
      
      vals$distance_matrix = dist_mat
      vals$great_circles = calculate_great_circles(vals$cities)
    })
  }, priority=40)
  
  setup_to_run_annealing_process = observe({
    input$go_button
    set_random_cities()
    map_name()
    
    isolate({
      vals$tour = sample(nrow(vals$cities))
      vals$tour_distance = calculate_tour_distance(vals$tour, vals$distance_matrix)
      vals$best_tour = c()
      vals$best_distance = Inf

      vals$s_curve_amplitude = ensure_between(input$s_curve_amplitude, 0, 1000000)
      vals$s_curve_center = ensure_between(input$s_curve_center, -1000000, 1000000)
      vals$s_curve_width = ensure_between(input$s_curve_width, 1, 1000000)
      vals$total_iterations = ensure_between(input$total_iterations, 1, 1000000)
      vals$plot_every_iterations = ensure_between(input$plot_every_iterations, 1, 1000000)
      
      vals$number_of_loops = ceiling(vals$total_iterations / vals$plot_every_iterations)
      vals$distances = rep(NA, vals$number_of_loops)
      
      vals$iter = 0
    })
    
    run_annealing_process$resume()
  }, priority=20)
  
  run_annealing_process = observe({
    qry = parseQueryString(session$clientData$url_search)
    if (input$go_button == 0 & is.null(qry$auto)) return()
    
    if (nrow(isolate(vals$cities)) < 2) return()
    
    isolate({
      intermediate_results = run_intermediate_annealing_process(
                               cities = vals$cities,
                               distance_matrix = vals$distance_matrix,
                               tour = vals$tour,
                               tour_distance = vals$tour_distance,
                               best_tour = vals$best_tour,
                               best_distance = vals$best_distance,
                               starting_iteration = vals$iter,
                               number_of_iterations = vals$plot_every_iterations,
                               s_curve_amplitude = vals$s_curve_amplitude,
                               s_curve_center = vals$s_curve_center,
                               s_curve_width = vals$s_curve_width
                             )
      
      vals$tour = intermediate_results$tour
      vals$tour_distance = intermediate_results$tour_distance
      vals$best_tour = intermediate_results$best_tour
      vals$best_distance = intermediate_results$best_distance

      vals$iter = vals$iter + vals$plot_every_iterations
      
      vals$distances[ceiling(vals$iter / vals$plot_every_iterations)] = intermediate_results$tour_distance
    })
    
    if (isolate(vals$iter) < isolate(vals$total_iterations)) {
      invalidateLater(0, session)
    } else {
      isolate({
        vals$tour = vals$best_tour
        vals$tour_distance = vals$best_distance
      })
    }
  }, priority=10)
  
  output$map = renderPlot({
    plot_tour(vals$cities, vals$tour, vals$great_circles, map_name=tolower(input$map_name), label_cities=input$label_cities)
    
    if (length(vals$tour) > 1) {
      pretty_dist = prettyNum(vals$tour_distance, big.mark=",", digits=0, scientific=FALSE)
      pretty_iter = prettyNum(vals$iter, big.mark=",", digits=0, scientific=FALSE)
      pretty_temp = prettyNum(current_temperature(vals$iter, vals$s_curve_amplitude, vals$s_curve_center, vals$s_curve_width),
                              big.mark=",", digits=0, scientific=FALSE)
      
      plot_title = paste0("Distance: ", pretty_dist, " miles\n",
                          "Iterations: ", pretty_iter, "\n",
                          "Temperature: ", pretty_temp)
                          
      title(plot_title)
    }
  }, height=550)
  
  output$annealing_schedule = renderPlot({
    xvals = seq(from=0, to=vals$total_iterations, length.out=100)
    yvals = current_temperature(xvals, vals$s_curve_amplitude, vals$s_curve_center, vals$s_curve_width)
    plot(xvals, yvals, type='l', xlab="iterations", ylab="temperature", main="Annealing Schedule")
    points(vals$iter, current_temperature(vals$iter, vals$s_curve_amplitude, vals$s_curve_center, vals$s_curve_width), pch=19, col='red')
  }, height=260)
  
  output$distance_results = renderPlot({
    if (all(is.na(vals$distances))) return()
    
    xvals = vals$plot_every_iterations * (1:vals$number_of_loops)
    plot(xvals, vals$distances, type='o', pch=19, cex=0.7, 
         ylim=c(0, max(vals$distances, na.rm=TRUE)), xlab="iterations", ylab="current tour distance",
         main="Evolution of Current Tour Distance")
  }, height=260)
  
  session$onSessionEnded(function() {
    run_annealing_process$suspend()
    set_cities_randomly$suspend()
  })
})
