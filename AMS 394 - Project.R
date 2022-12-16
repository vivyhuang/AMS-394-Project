testing1 <- function(sample1, sample2, interval_choice, mu_choice, alternative_choice, method_choice) {
  dataset = cbind(sample1, sample2)
  n1 <- sum(!is.na(sample1) & !is.na(sample2))
  n2 <- sum(!is.na(sample1) & is.na(sample2))
  n3 <- sum(is.na(sample1) & !is.na(sample2))
  n4 <- sum(is.na(sample1) & is.na(sample2))
  
  if((n1 > 0 & n2 > 0 & n3 == 0) | (n1 > 0 & n3 > 0 & n2 == 0)) {
    dataset <- na.omit(dataset)
    interval_choice = as.double(interval_choice)
    mu_choice = as.double(mu_choice)
    alternative_choice = as.integer(alternative_choice)
    if (alternative_choice == 1) {
      alt = "two.sided"
    }else if (alternative_choice == 2) {
      alt = "less"
    }else {
      alt = "greater"
    }
    diff <- dataset[,c(1)] - dataset[,c(2)]
    normality_test <- shapiro.test(diff)
    # normally distributed
    if (normality_test$p.value >= (1 - interval_choice)) {
      print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, paired = TRUE, conf.level = interval_choice))
    } else {
      print(wilcox.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, paired = TRUE, conf.level = interval_choice))
    }
    
    #treating as two independent samples
    sample1 <- na.omit(sample1)
    sample2 <- na.omit(sample2)
    interval_choice = as.double(interval_choice)
    mu_choice = as.double(mu_choice)
    alternative_choice = as.integer(alternative_choice)
    if (alternative_choice == 1) {
      alt = "two.sided"
    }else if (alternative_choice == 2) {
      alt = "less"
    }else {
      alt = "greater"
    }
    normal1 <- shapiro.test(sample1[,c(1)])
    normal2 <- shapiro.test(sample2[,c(1)])
    #normally distributed
    if ((normal1$p.value >= (1 - interval_choice)) & (normal2$p.value >= (1 - interval_choice))) {
      var_test <- var.test(dataset[,c(1)], dataset[,c(2)])
      #if p-value is greater than 0.05, then variances is equal
      if (var_test$p.value >= (1 - interval_choice)) {
        print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, var.equal = TRUE, conf.level = interval_choice))
      } else {
        print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, var.equal = FALSE, conf.level = interval_choice))
      }
      #not normally distributed
    } else {
      print(wilcox.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, conf.level = interval_choice))
    }
    
  }else if ((n1 > 0 & n2 == 0 & n3 == 0)) {
    dataset <- na.omit(dataset)
    interval_choice = as.double(interval_choice)
    mu_choice = as.double(mu_choice)
    alternative_choice = as.integer(alternative_choice)
    if (alternative_choice == 1) {
      alt = "two.sided"
    }else if (alternative_choice == 2) {
      alt = "less"
    }else {
      alt = "greater"
    }
    diff <- dataset[,c(1)] - dataset[,c(2)]
    normality_test <- shapiro.test(diff)
    # normally distributed
    if (normality_test$p.value >= (1 - interval_choice)) {
      print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, paired = TRUE, conf.level = interval_choice))
    } else {
      print(wilcox.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, paired = TRUE, conf.level = interval_choice))
    }
    
  }else if ((n2 > 0 & n3 > 0 & n1 == 0)) {
    sample1 <- na.omit(sample1)
    sample2 <- na.omit(sample2)
    interval_choice = as.double(interval_choice)
    mu_choice = as.double(mu_choice)
    alternative_choice = as.integer(alternative_choice)
    if (alternative_choice == 1) {
      alt = "two.sided"
    }else if (alternative_choice == 2) {
      alt = "less"
    }else {
      alt = "greater"
    }
    normal1 <- shapiro.test(sample1[,c(1)])
    normal2 <- shapiro.test(sample2[,c(1)])
    #normally distributed
    if ((normal1$p.value >= (1 - interval_choice)) & (normal2$p.value >= (1 - interval_choice))) {
      var_test <- var.test(dataset[,c(1)], dataset[,c(2)])
      #if p-value is greater than 0.05, then variances is equal
      if (var_test$p.value >= (1 - interval_choice)) {
        print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, var.equal = TRUE, conf.level = interval_choice))
      } else {
        print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, var.equal = FALSE, conf.level = interval_choice))
      }
      #not normally distributed
    } else {
      print(wilcox.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, conf.level = interval_choice))
    }
    
  }else if ((n1 > 0 & n2 > 0 & n3 > 0)) {
    data1 <- subset(dataset, (!is.na(dataset[,c(1)]) & !is.na(dataset[,c(2)])))
    data2 <- subset(dataset, (!is.na(dataset[,c(1)]) & is.na(dataset[,c(2)])))
    data3 <- subset(dataset, (is.na(dataset[,c(1)]) & !is.na(dataset[,c(2)])))
    sample1 <- na.omit(sample1)
    sample2 <- na.omit(sample2)
    interval_choice = as.double(interval_choice)
    normal1 <- shapiro.test(sample1[,c(1)])
    normal2 <- shapiro.test(sample2[,c(1)])
    #normally distributed
    if ((normal1$p.value >= (1 - interval_choice)) & (normal2$p.value >= (1 - interval_choice))) {
      #"Type 1 for Kim et al.'s Method or 2 for Looney and Jones's Method"
      method_choice = as.integer(method_choice)
      
      if (method_choice == 1) {
        mu_choice = as.double(mu_choice)
        D_bar = mean(data1[,c(1)] - data1[,c(2)])
        SD = sd(data1[,c(1)] - data1[,c(2)])
        T_bar = mean(data2[,c(1)])
        ST = sd(data2[,c(1)])
        N_bar = mean(data3[,c(2)])
        SN = sd(data3[,c(2)])
        nH = 2 / sum((1/n2),(1/n3))
        t3_top = (n1*D_bar) + (nH*(T_bar - N_bar)) - mu_choice
        t3_bottom = sqrt((n1*(SD**2)) + ((nH**2)*(((ST**2)/n2) + ((SN**2)/n3))))
        t3 = t3_top / t3_bottom
        alternative_choice = as.integer(alternative_choice)
        if (alternative_choice == 1) {
          p_value = 2*(1-pnorm(abs(t3)))
          z_value = abs(qnorm(interval_choice/2))
          lower_bound = t3_top - (z_value*t3_bottom)
          upper_bound = t3_top + (z_value*t3_bottom)
          results <- list("p-value" = p_value, "CI: lower bound" = lower_bound, "CI: upper bound" = upper_bound)
          return(results)
        }else if (alternative_choice == 2) {
          p_value = pnorm(t3)
          z_value = abs(qnorm(interval_choice))
          lower_bound = -Inf
          upper_bound = t3_top + (z_value*t3_bottom)
          results <- list("p-value" = p_value, "CI: lower bound" = lower_bound, "CI: upper bound" = upper_bound)
          return(results)
        }else {
          p_value = 1 - pnorm(t3)
          z_value = abs(qnorm(interval_choice))
          lower_bound = t3_top - (z_value*t3_bottom)
          upper_bound = Inf
          results <- list("p-value" = p_value, "CI: lower bound" = lower_bound, "CI: upper bound" = upper_bound)
          return(results)
        }
        
      } else if (method_choice == 2) {
        data12 <- rbind(data1, data2)
        data13 <- rbind(data1, data3)
        T_bar_star = mean(data12)
        ST_star = sd(data12)
        N_bar_star = mean(data13)
        SN_star = sd(data13)
        STN1 = cov(data1[,c(1)], data1[,c(2)])
        Zcorr_top = (T_bar_star - N_bar_star - mu_choice)
        Zcorr_bottom = sqrt((ST_star**2/(n1 + n2)) + (SN_star**2/(n1 + n3)) - ((2*n1*STN1)/((n1 + n2)(n1 + n3))))
        Zcorr = Zcorr_top / Zcorr_bottom
        alternative_choice = as.integer(alternative_choice)
        if (alternative_choice == 1) {
          p_value = 2*(1-pnorm(abs(Zcorr)))
          z_value = qnorm(1 - (1 - interval_choice/2))
          lower_bound = Zcorr_top - (z_value*Zcorr_bottom)
          upper_bound = Zcorr_top + (z_value*Zcorr_bottom)
        }else if (alternative_choice == 2) {
          p_value = pnorm(t3)
          z_value = abs(qnorm(interval_choice))
          lower_bound = -Inf
          upper_bound = Zcorr_top + (z_value*Zcorr_bottom)
        }else {
          p_value = 1 - pnorm(Zcorr)
          z_value = abs(qnorm(interval_choice))
          lower_bound = Zcorr_top - (z_value*Zcorr_bottom)
          upper_bound = Inf
        }
      } else {
        return("Error: incorrect input")
      }
    } else {
      dataset <- na.omit(dataset)
      interval_choice = as.double(interval_choice)
      mu_choice = as.double(mu_choice)
      alternative_choice = as.integer(alternative_choice)
      if (alternative_choice == 1) {
        alt = "two.sided"
      }else if (alternative_choice == 2) {
        alt = "less"
      }else {
        alt = "greater"
      }
      diff <- dataset[,c(1)] - dataset[,c(2)]
      normality_test <- shapiro.test(diff)
      # normally distributed
      if (normality_test$p.value >= (1 - interval_choice)) {
        print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, paired = TRUE, conf.level = interval_choice))
      } else {
        print(wilcox.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, paired = TRUE, conf.level = interval_choice))
      }
      sample1 <- na.omit(sample1)
      sample2 <- na.omit(sample2)
      interval_choice = as.double(interval_choice)
      mu_choice = as.double(mu_choice)
      alternative_choice = as.integer(alternative_choice)
      if (alternative_choice == 1) {
        alt = "two.sided"
      }else if (alternative_choice == 2) {
        alt = "less"
      }else {
        alt = "greater"
      }
      normal1 <- shapiro.test(sample1[,c(1)])
      normal2 <- shapiro.test(sample2[,c(1)])
      #normally distributed
      if ((normal1$p.value >= (1 - interval_choice)) & (normal2$p.value >= (1 - interval_choice))) {
        var_test <- var.test(dataset[,c(1)], dataset[,c(2)])
        #if p-value is greater than 0.05, then variances is equal
        if (var_test$p.value >= (1 - interval_choice)) {
          print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, var.equal = TRUE, conf.level = interval_choice))
        } else {
          print(t.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, var.equal = FALSE, conf.level = interval_choice))
        }
        #not normally distributed
      } else {
        print(wilcox.test(dataset[,c(1)], dataset[,c(2)], alternative = alt, mu = mu_choice, conf.level = interval_choice))
      }
    }
    
  }else if ((n1 == 0 & n2 > 0 & n3 == 0) | (n1 == 0 & n2 == 0 & n3 > 0)) {
    interval_choice = as.double(interval_choice)
    mu_choice = as.double(mu_choice)
    alternative_choice = as.integer(alternative_choice)
    if (alternative_choice == 1) {
      alt = "two.sided"
    }else if (alternative_choice == 2) {
      alt = "less"
    }else {
      alt = "greater"
    }
    if (n2 > 0) {
      normality_test <- shapiro.test(sample1)
      # normally distributed
      if (normality_test$p.value >= (1 - interval_choice)) {
        print(t.test(sample1, mu = mu_choice, alternative = alt, conf.level = interval_choice))
      } else {
        print(wilcox.test(sample1, mu = mu_choice, alternative = alt, conf.level = interval_choice))
      }
    }else {
      normality_test <- shapiro.test(sample2)
      # normally distributed
      if (normality_test$p.value >= (1 - interval_choice)) {
        print(t.test(sample2, mu = mu_choice, alternative = alt, conf.level = interval_choice))
      } else {
        print(wilcox.test(sample2, mu = mu_choice, alternative = alt, conf.level = interval_choice))
      }
    }
    
  }else {
    return("Error: cannot be tested")
  }
}
