get_marfx_logistic <- function(model, model_dataset, type = "likelihood",
                               cat_variables = NULL) {
  library(MASS)
  library(nnet)
  library(dplyr)
  
  # - pulling key coefficients and data
  pe <- coef(model) # point estimates
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  
  # - simulate Betas
  sims <- 1000
  simbetas <- MASS::mvrnorm(sims, pe, vc) 
  
  # - initializing the results dataset
  marf_res <- NULL
  
  # - pulling the outcome and explanatory variables from the model formula. 
  #   Detecting any interactions and saving the names as well
  yname <- strsplit(as.character(model$formula), split = "~")[[2]]
  predictors <- unique(strsplit(
    strsplit(as.character(model$formula), split = "~")[[3]], 
    split = " \\+ | \\* "
  )[[1]])
  interactions_pre <- strsplit(
    strsplit(as.character(model$formula), split = "~")[[3]], 
    split = " \\+ "
  )[[1]]
  interactions <- interactions_pre[which(grepl(" * ", interactions_pre))]
  inter_variables <- strsplit(interactions, split = " \\* ")
  
  # - pulling the right side of the model formula to create model matrices 
  #   in the future (know which categorical variables need to be transformed to
  #   dummies)
  right_side <- paste0("~",
                       strsplit(as.character(model$formula), split = "~")[[3]])
  
  # - a version of the datset only with the variables of interest (and removing
  #   any rows that have an NA for any of those variables). Pulling the actual
  #   outcome values as well.
  full_data <- na.omit(model_dataset[,c(yname, predictors)])
  full_data <- full_data[!is.infinite(rowSums(full_data)),]
  X <- full_data[,predictors]
  Y <- full_data[,yname]
  
  # - create a generic scenario (all variables at their average value)
  # - create a generic scenario (all variables at their minimum --dummies-- or
  #   average --numeric-- value)
  gen_scen <- NULL
  for (j in 1:ncol(X)) {
    if (colnames(X[j]) %in% cat_variables) {
      # - if it's a factor: giving it an NA for now
      gen_scen <- c(gen_scen, NA)
    } else {
      # - if dummy variable, calculate the minimum (so 1)
      if (length(table(X[j])) > 2) {
        gen_scen <- c(gen_scen, as.matrix(mean(X[,j])))
      } else {
        # - if numeric, calculate average value
        gen_scen <- c(gen_scen, as.matrix(min(X[,j])))
      }
    }
  }
  gen_scen <- t(as.data.frame(gen_scen))
  gen_scen <- data.frame(gen_scen)
  colnames(gen_scen) <- colnames(X)
  rownames(gen_scen) <- NULL
  
  # - a list of variables for which to calculate the marginal effects in a first
  #   step: excluding factor variables and interactions
  variables <- predictors[which(!(predictors %in% 
                                    c(cat_variables)))]
  
  # - iterating through the selected predictors and calculating the marginal 
  #   effect on the outcome of going from minimum to maximum value
  #   -- NUMERIC VARIABLES --
  for (v in variables) {
    # - check if dummy variable or numeric
    if (length(table(X[,v])) > 2) {
      dummy <- FALSE
    } else {
      dummy <-TRUE
    }
    # - creating a 0 and 1 scenario for dummies, a mean and mean+1sd for numeric
    if (dummy) {
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen1[,v] <- max(X[,v])
    } else {
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen1[,v] <- mean(X[,v]) + sd(X[,v])
    }
    
    # - transforming the scenario to model.matrix format (transforming 
    #   categorical variables into dummies)
    for (cat_v in cat_variables) {
      scen0[,cat_v] <- factor(X[,cat_v][1],
                              levels = levels(factor(X[,cat_v])))
      scen1[,cat_v] <- scen0[,cat_v]
    }
    
    for (var in variables) {
      if (!(var %in% cat_variables)) {
        scen0[,var] <- as.numeric(as.character(scen0[,var]))
        scen1[,var] <- as.numeric(as.character(scen1[,var]))
      }
    }
    
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    mmatrix1 <- model.matrix(formula(right_side), scen1)
    
    # - keep the value for the interaction constant
    if (length(inter_variables) > 0) {
      for (inter_var in inter_variables) {
        var01 <- inter_var[1]
        var02 <- inter_var[2]
        mmatrix1[,paste0(paste0(var01, ":", var02))] <- mmatrix0[
          ,paste0(paste0(var01, ":", var02))]
      }
    }
    
    # - N predicted values for each scenario, and then taking the average 
    #   likelihood or the average differendce, and 95% CIs.
    #yhats0 <- exp(mmatrix0 %*% t(as.matrix(simbetas)))
    #yhats1 <- exp(mmatrix1 %*% t(as.matrix(simbetas)))
    yhats0 <- 1 / (1 + (exp(-mmatrix0 %*% t(as.matrix(simbetas)))))
    yhats1 <- 1 / (1 + (exp(-mmatrix1 %*% t(as.matrix(simbetas)))))
    
    if (type == "likelihood") {
      pred_values <- yhats1 / yhats0
    } else if (type == "difference") {
      pred_values <- yhats1 - yhats0
    }
    
    pe <- round(mean(pred_values), 5)
    lwr <- round(quantile(pred_values, probs = 0.03, na.rm = TRUE), 5)
    upr <- round(quantile(pred_values, probs = 0.97, na.rm = TRUE), 5)
    
    # - adding the marginal effect for this variable to the results dataset
    new_row <- data.frame(v, pe, lwr, upr)
    marf_res <- rbind(marf_res, new_row)
  }
  
  #   -- CATEGORICAL VARIABLES --
  for (v in cat_variables) {
    # - pull the reference class for this cat var
    full_data[,v] <- as.factor(full_data[,v])
    ref_class <- factor(levels(full_data[,v])[1], 
                        levels = levels(full_data[,v]))
    scen0 <- gen_scen
    
    # - set up 'random' values for the other categorical variables
    for (cat_v in cat_variables) {
      if (cat_v != v) {
        scen0[,cat_v] <- factor(X[,cat_v][1], 
                                levels = as.character(unique(X[,cat_v])))
      }
    }
    scen0[,v] <- ref_class
    
    # - calculate Pr baseline ref class
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    #yhats0 <- exp(mmatrix0 %*% t(as.matrix(simbetas)))
    yhats0 <- 1 / (1 + (exp(-mmatrix0 %*% t(as.matrix(simbetas)))))
    
    # - calculate now Pr and marginal effect for the other levels
    other_levels <- levels(full_data[,v])[2:length(levels(full_data[,v]))]
    for (lev in other_levels) {
      scen1 <- scen0
      scen1[,v] <- factor(lev, levels = levels(scen0[,v]))
      mmatrix1 <- model.matrix(formula(right_side), scen1)
      #yhats1 <- exp(mmatrix1 %*% t(as.matrix(simbetas)))
      yhats1 <- 1 / (1 + (exp(-mmatrix1 %*% t(as.matrix(simbetas)))))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.03), 4)
      upr <- round(quantile(pred_values, probs = 0.97), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      new_row <- data.frame(v = paste0(v, ":", lev), pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
  }
  
  #   -- INTERACTIONS  --
  # - calculating the marginal effect of interactions
  if (length(inter_variables) > 0) {
    for (inter_var in inter_variables) {
      # - calculating the marginal effect of a +1sd change for the first
      #   variable in the interaction, when the second variable is LOW, and then
      #   also when the second variable is HIGH (-1sd & +1sd)
      var01 <- inter_var[1]
      var02 <- inter_var[2]
      # - calculate LOW and HIGH value for second variable
      var02_low <- min(X[,var02])#mean(X[,var02]) - sd(X[,var02])
      var02_high <- max(X[,var02])#mean(X[,var02]) + sd(X[,var02])
      # - now, calculate marginal effect for each of these two scenarios
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen0[,var02] <- var02_low
      scen1[,var02] <- var02_low
      scen0[,var01] <- mean(X[,var01])
      scen1[,var01] <- mean(X[,var01]) + sd(X[,var02])
      
      mmatrix0 <- model.matrix(formula(right_side), scen0)
      mmatrix1 <- model.matrix(formula(right_side), scen1)
      
      # - N predicted values for each scenario, and then taking the average 
      #   likelihood or the average differendce, and 95% CIs.
      yhats0 <- 1 / (1 + (exp(-mmatrix0 %*% t(as.matrix(simbetas)))))
      yhats1 <- 1 / (1 + (exp(-mmatrix1 %*% t(as.matrix(simbetas)))))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.03, na.rm = TRUE), 4)
      upr <- round(quantile(pred_values, probs = 0.97, na.rm = TRUE), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      v <- paste0(var01, ":", var02, "LOW")
      new_row <- data.frame(v, pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
      
      # - now, calculate marginal effect for HIGH values of the second variable
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen0[,var02] <- var02_high
      scen1[,var02] <- var02_high
      scen0[,var01] <- mean(X[,var01])
      scen1[,var01] <- mean(X[,var01]) + sd(X[,var02])
      
      mmatrix0 <- model.matrix(formula(right_side), scen0)
      mmatrix1 <- model.matrix(formula(right_side), scen1)
      
      # - N predicted values for each scenario, and then taking the average 
      #   likelihood or the average differendce, and 95% CIs.
      yhats0 <- 1 / (1 + (exp(-mmatrix0 %*% t(as.matrix(simbetas)))))
      yhats1 <- 1 / (1 + (exp(-mmatrix1 %*% t(as.matrix(simbetas)))))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.03, na.rm = TRUE), 4)
      upr <- round(quantile(pred_values, probs = 0.97, na.rm = TRUE), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      v <- paste0(var01, ":", var02, "HIGH")
      new_row <- data.frame(v, pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
  }
  
  return(marf_res)
}
