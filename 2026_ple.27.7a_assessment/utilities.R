# utilities.R

# From functions_TE.R

### ------------------------------------------------------------------------ ###
### functions for SAM
### ------------------------------------------------------------------------ ###

### function for transforming SAM summary into a format for ggplot
### V2, including catch
SAM_to_ggplot2 <- function(input, ### SAM object
                           include_catch = FALSE, ### add catch?
                           rec_factor = 1, ### recruits unit
                           long_format = TRUE ### long format
){
  #cat("SAM_to_ggplot2\n",file="./model/log.txt", append=TRUE)
  ### create summary
  res <- summary(input)
  
  ### use absolute numbers for recruits
  res[, 1:3] <- res[, 1:3] * rec_factor
  
  ### separate Recruitment/SSB/F
  res <- rbind(res[, 1:3], res[, 4:6], res[, 7:9])
  
  ### get year
  res <- cbind(res, as.numeric(row.names(res)))
  row.names(res) <- NULL
  
  ### convert to data frame
  res <- as.data.frame(res)
  
  ### set names
  colnames(res) <- c("estimate", "low", "high", "year")
  res$quant <- rep(c("Recruits", "SSB", "Fbar"),
                   each = nrow(res)/3)
  
  ### extract calculate estimated catch
  ### find positions of catch data
  idx <- which(names(input$sdrep$value) == "logCatch")
  ### extract catch data and upper/lower bounds (+-1.96SE)
  catch_estimated <- data.frame(estimate = exp(input$sdrep$value[idx]),
                                low = exp(input$sdrep$value[idx] -
                                            1.96 * input$sdrep$sd[idx]),
                                high = exp(input$sdrep$value[idx] +
                                             1.96 * input$sdrep$sd[idx]),
                                year = input$data$years,
                                quant = "Catch")
  
  ### add to data frame
  res <- rbind(res, catch_estimated)
  
  ### get catch input data
  ### get log observations
  logobs <- cbind(as.data.frame(input$data$aux), logobs = input$data$logobs)
  ### remove log scale
  logobs$obs <- exp(logobs$logobs)
  ### remove columns not required anymore
  catch <- subset(logobs,fleet==1)[, c("year", "age", "obs")]
  ### format
  catch <- cast(catch, year ~ age, value = "obs", fun.aggregate = sum)
  ### remove SSB column (age -1), if available
  #catch <- catch[, setdiff(names(catch), "-1")]
  ### remove year column and set year as row name
  row.names(catch) <- catch$year
  catch <- catch[, setdiff(names(catch), "year")]
  ### replace NAs with 0
  catch[is.na(catch)] <- 0
  
  ### get catch weights
  catch_weights <- as.data.frame(input$data$catchMeanWeight)
  
  ### calculate catch biomass @ age
  catch <- catch * catch_weights
  
  ### annual catch
  catch <- rowSums(catch)
  
  ### set up object for merging with res
  catch <- data.frame(year = names(catch),
                      quant = "Catch",
                      input = catch)
  
  ### merge
  res <- merge(res, catch, all = TRUE)
  
  if(identical(long_format, TRUE)){
    
    res <- melt(res, c("year", "quant", "input"))
    
    ### sort factor levels
    res$variable <- as.factor(res$variable)
    res$variable <- factor(res$variable,
                           levels = levels(res$variable)[c(3, 1, 2)])
    
  }
  
  ### sort factor levels
  res$quant <- as.factor(res$quant)
  res$quant <- factor(res$quant,
                      levels = levels(res$quant)[c(4, 2, 3, 1)])
  
  ### return
  return(res)
  
}

### function for plotting results of list of SAM objects
### V2, including catch
plot_SAM_list2 <- function(input_list, ### list of fitted SAM objects
                           plot_catch = FALSE, ### plot catch?
                           plot_catch_data = FALSE, ### plot dots for catch data
                           ### TRUE/FALSE/1=only 1st group
                           group = " ", ### description of groups
                           SAM_to_ggplot2, ### required function
                           rec_factor = 1000, ### units
                           plot_zoom = "" ### plot only range of years
){
  #cat("plot_SAM_list2\n",file="./model/log.txt", append=TRUE)
  ### format for plotting
  res_summary <- lapply(seq_along(input_list), function(x){
    ### transform
    res <- SAM_to_ggplot2(input = input_list[[x]], rec_factor = rec_factor,
                          long = TRUE)
    ### add retro year
    res$group <- names(input_list)[x]
    res
  })
  names(res_summary) <- names(input_list)
  
  ### combine to single data frame
  res_summary <- do.call(rbind, res_summary)
  
  ### subset to year range if desired
  if(is.numeric(plot_zoom)){
    
    res_summary <- res_summary[res_summary$year %in% plot_zoom, ]
    
  }
  
  ### exclude catch if desired
  if(!isTRUE(plot_catch)){
    
    res_summary <- res_summary[!res_summary$quant == "Catch", ]
    
  }
  
  
  ### plot
  p <- ggplot(data = res_summary) +
    geom_line(aes(x = year, y = value,
                  colour = as.factor(group),
                  linetype = as.factor(variable),
                  alpha = as.factor(variable)
    )) +
    scale_linetype_manual("", values = c("dotted", "solid", "dotted")) +
    scale_colour_discrete(group) +
    scale_alpha_manual("", values = c(0.5, 1, 0.5)) +
    theme_bw() +
    labs(x = "year", y = "") +
    ylim(0, NA)
  
  ### black only, if just one group
  if(length(unique(res_summary$group)) == 1){
    p <- p + scale_colour_manual(group, values = "black")
  }
  
  ### plot catch
  if(isTRUE(plot_catch)){
    
    p <- p + facet_wrap(~ quant, scales = "free_y", nrow = 2)
    
    ### plot dots for catch data and all groups
    if(isTRUE(plot_catch_data)){
      
      p <- p + geom_point(data = res_summary[res_summary$variable ==
                                               "estimate", ],
                          aes(x = year, y = input, colour = as.factor(group),
                              shape = as.factor(group))) +
        scale_shape_discrete(group)
      
      ### plot dots for catch, but only first group
    } else if(plot_catch_data == 1){
      
      longest_assessment <- which.max(unlist(lapply(input_list, function(x) {

            tail(x$data$year, 1)

      })))
      
      p <- p + geom_point(data = res_summary[res_summary$variable ==
                                               "estimate" &
                                               res_summary$group ==
                                               unique(res_summary$group)[longest_assessment], ],
                          aes(x = year, y = input, colour = as.factor(group)),
                          colour = "black")
      
    }
    
  } else {
    
    p <- p + facet_wrap(~ quant, scales = "free_y", nrow = 3)
    
  }
  
  ### return plot
  p
  
}

### function for plotting F pattern/selectivity from list of SAM objects
plot_selectivity <- function(input_list, ### list of fitted SAM objects
                             group = "", ### name of grouping
                             standardize = 0, ### standardize selectivity
                             ### 0 = raw F values
                             ### 1 = standardized per year
                             ### standardized to maximum per year
                             plot_along_years = FALSE,
                             sep_ld = FALSE ### plot landings/discards separate
){
  #cat("plot_selectivity\n",file="./model/log.txt", append=TRUE)
  ### go through list
  res <- lapply(seq_along(input_list), function(x){
    
    ### get ages available in F@age estimations
    ### (neccessairy because of possible linked F patterns at age)
    ages <- input_list[[x]]$conf$keyLogFsta[1, ] + 1
    
    ### extract F
    F_values <- exp(input_list[[x]]$pl$logF)[ages, ]
    F_values <- as.data.frame(F_values)
    
    ### set names
    names(F_values) <- input_list[[x]]$data$years
    
    ### standardize per year
    if(standardize > 0){
      
      ### calculate catchability
      F_values <- apply(F_values, 2, function(x){x / sum(x)})
      
    }
    if(standardize == 2){
      
      ### standardise to maximum
      F_values <- apply(F_values, 2, function(x){x / max(x)})
      
    }
    
    F_values <- as.data.frame(F_values)
    
    ### separate F into landings and discards, if required
    if(sep_ld == TRUE){
      
      ### get landings fraction
      land_frac <- input_list[[x]]$data$landFrac[,,1]
      land_frac <- as.data.frame(t(land_frac))
      
      ### separate F by landings and discards
      F_landings <- F_values * land_frac
      F_discards <- F_values * (1 - land_frac)
      
      ### combine data frames
      F_values_combined <- rbind(cbind(F_landings, catch = "landings.n"),
                                 cbind(F_discards, catch = "discards.n"))
      
    } else {
      
      F_values_combined <- F_values
      F_values_combined$catch <- "catch"
      
    }
    
    ### get ages
    F_values_combined$age <- 
      as.numeric(seq(input_list[[x]]$data$minAgePerFleet[1],
                     input_list[[x]]$data$maxAgePerFleet[1]))
    
    ### format
    F_values <- melt(F_values_combined, c("age", "catch"))
    names(F_values)[2:4] <- c("catch", "year", "F")
    
    ### add group name
    F_values$group <- names(input_list)[[x]]
    
    ### return
    F_values
    
  })
  
  ### combine list
  res <- do.call(rbind, res)
  
  ### y-axis label
  y_label <- switch(standardize+1, "fishing mortality", "selectivity",
                    "selectivity (standardized)")
  
  ### plot F along years, grouped by age
  if(plot_along_years){
    
    p <- ggplot(data = res, aes(x = as.numeric(as.character(year)), y = F,
                                colour = as.factor(group),
                                group = interaction(as.factor(group), catch), 
                                linetype = catch
    )) +
      geom_line() +
      facet_wrap(~ paste0("age = ", age)) +
      theme_bw() +
      labs(y = y_label, x = "year")
    
    
    ### plot F/selectivity curve per year
  } else {
    
    ### plot
    p <- ggplot(data = res, 
                aes(x = age, y = F, colour = group, linetype = catch)) +
      geom_line() +
      facet_wrap(~ year, scales = "free_y") +
      theme_bw() +
      labs(y = y_label)
    
  }
  
  ### black only, if just one group
  if(length(unique(res$group)) == 1){
    p <- p + scale_colour_manual(group, values = "black")
  } else{
    p <- p + scale_colour_discrete(group)
  }
  
  p
  
}

### function for plotting estimated catchabilities
plot_catchability <- function(fit_list, ### list of fitted SAM objects
                              fleet_SSB, ### which fleets are SSB
                              fleet_names, ### names of fleets
                              group ### name of group
){
  #cat("plot_catchability\n",file="./model/log.txt", append=TRUE)
  ### "loop" through models
  res <- lapply(seq_along(fit_list), function(x){
    
    ### get model results
    model <- fit_list[[x]]
    ### extract configuration of catchability coupling
    key <- model$conf$keyLogFpar
    ### get order of values in matrix
    o <- order(key[key>-1])
    ### find corresponding fleet for values
    fleet <- row(key)[key>-1][o]
    ### find corresponding ages for values
    age <- col(key)[key>-1][o]
    ### extract/calculate values
    estimate <- exp(model$pl$logFpar)[key[key > -1][o]+1]
    ### 95% of the values lie within +- 1.96SE
    lower    <- exp(model$pl$logFpar - 1.96 *
                      model$plsd$logFpar)[key[key > -1][o]+1]
    upper   <- exp(model$pl$logFpar + 1.96 *
                     model$plsd$logFpar)[key[key > -1][o]+1]
    ### create data frame with values
    catchabilities <- data.frame(fleet = fleet,
                                 age = age,
                                 estimate = estimate,
                                 lower = lower,
                                 upper = upper)
    
    ### add model name
    catchabilities$model <- names(fit_list)[x]
    
    ### return
    catchabilities
    
  })
  
  ### combine list elements
  catchabilities <- do.call(rbind, res)
  
  ### long format
  catchabilities <- melt(catchabilities, c("fleet", "age", "model"))
  
  ### set age to "SSB" for SSB indices
  catchabilities$age[catchabilities$fleet %in% fleet_SSB] <- "SSB"
  
  ### sort variable
  catchabilities$variable <- as.factor(catchabilities$variable)
  catchabilities$variable <- factor(catchabilities$variable,
                                    levels = levels(catchabilities$variable)[c(3, 1, 2)])
  
  ### sort indices and give names
  catchabilities$fleet <- factor(catchabilities$fleet,
                                 labels = fleet_names[unique(catchabilities$fleet)])
  
  p <- ggplot(data = catchabilities,
              aes(x = age, y = value, colour = model, alpha = variable,
                  linetype = variable, shape = variable,
                  group = interaction(variable, model)), ) +
    geom_line() + geom_point() +
    scale_linetype_manual("", values = c("dotted", "solid", "dotted")) +
    scale_shape_manual("", values = c(3, 16, 3)) +
    scale_colour_discrete(group) +
    scale_alpha_manual("", values = c(0.5, 1, 0.5)) +
    facet_wrap(~ fleet, nrow = 1, scales = "free") +
    theme_bw() +
    labs(x = "age", y = "catchability") +
    ylim(0, NA)
  p
  
}

### function for plotting residuals
plot_residuals  <- function(fit_list, ### list of fitted SAM objects
                            fleet_names, ### names of fleets
                            n_cores = 1, ### number of cores used
                            lib_path = paste0(.libPaths(), "/../library2")
                            ### path to library with mack SAM version
){
  #cat("plot_residuals\n",file="./model/log.txt", append=TRUE)
  ### use parallel computing?
  if(n_cores == 1){
    
    ### sequential processing
    residual_list <- lapply(fit_list, residuals)
    
  } else {
    
    ### start paralell cluster
    cl <- makeCluster(n_cores)
    clusterExport(cl, list("lib_path"))
    
    ### calculate residuals in parallel
    ### WARNING: terminal output including warnings is suppressed!
    residual_list <- parLapply(cl, fit_list, function(x){
      
      ### use not-exported method from SAM package
      ### sadly not working reliably...
      #stockassessment:::residuals.sam(x)
      
      ### load the proper SAM version...
      withr::with_libpaths(new = lib_path, library("stockassessment"))
      
      ### calculate residuals
      residuals(x)
      
    })
    
    ### stop parallel cluster
    stopCluster(cl)
    
  }
  
  ### formatting
  residual_list <- lapply(seq_along(residual_list), function(x){
    
    ### force into data frame
    res <- as.data.frame(do.call(cbind, residual_list[[x]]))
    
    ### add model name
    res$model <- names(residual_list)[x]
    
    res
    
  })
  
  ### combine elements
  res <- do.call(rbind, residual_list)
  
  ### set fleet names
  res$fleet_name <- as.factor(res$fleet)
  res$fleet_name <- factor(res$fleet_name,
                           labels = fleet_names)
  
  ### sort out ages
  res$age <- as.character(res$age)
  res$age[res$age == "-1"] <- "SSB"
  res$age <- as.factor(res$age)
  
  p <- ggplot(data = res,
              aes(x = year, y = age, size = abs(residual),
                  fill = residual > 0)) +
    geom_point(shape = 21, colour = "black", alpha = 0.8) +
    scale_fill_manual(name = "", values = c("white","black"),
                      labels = c("negative", "positive")) +
    scale_size_continuous("residuals",# breaks = c(0.1,1,2,3,4),
                          guide = guide_legend(order = 1),
                          range = c(0.1,4)) +
    #facet_wrap(facets = ~ model + fleet_name, scales = "free_y",
    #           ncol = length(fleet_names)) +
    facet_grid(model ~ fleet_name, scales = "free") +
    labs(x = "", y = "age") +
    theme_bw(base_size = 10) +
    theme(legend.key = element_blank(),
          panel.grid = element_blank()
    )
  p
  
}
#'##############################################################################
#'                          Functions for tables                               #
#'##############################################################################

style_table1 <- function(tab) {
  #cat("style_table1\n",file="./model/log.txt", append=TRUE)
  # Capitalize first letter of column, make header, last column and second
  # last row in boldface and make last row italic
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  #emphasize.strong.rows(nrow(tab))
  set.alignment("right")
  
  return(tab)
}

style_table2 <- function(tab) {
  #cat("style_table2\n",file="./model/log.txt", append=TRUE)
  # Capitalize first letter of column, make header, last column and second
  # last row in boldface and make last row italic
  names(tab) <- pandoc.strong.return(names(tab))
  set.alignment("right")
  
  return(tab)
}
