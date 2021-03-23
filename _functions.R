#SIMULATED COHORT
library(tableone)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(nlme)



FitFlextableToPage <- function(ft, pgwidth = 6){
   
   ft_out <- ft %>% autofit()
   
   ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
   return(ft_out)
}

###### Simulated data ######
   var_rand <- function(n, no_groups){ # <- for grouping
      return(c(rep("A",n/no_groups),rep("B",n/no_groups))[sample(1:n, n, replace=F)])
   }
   
   var_categories <- function(n, categories){ # <- for grouping
      return(sample(categories, n, replace=T))
   }
   
   var_binom <- function(n, proportions){ #  <- for binomial variables
      return(rbinom(n, 1, proportions))
   }

   var_beta <- function(n, min, max, side,digs=0){ #  <- for skewed variables
      if(side == "right") return(round(min+(max-min)*rbeta(n,5,2),digits=digs))
      if(side == "left") return(round(min+(max-min)*rbeta(n,2,5),digits=digs))
   }

   var_norm <- function(n, mean, sd, digs=0){ #  <- for normaldistributed data
      return(round(rnorm(n, mean, sd),digits=digs))
   }

   var_sample <- function(n, min, max){ #  <- for normaldistributed data
      return(as.numeric(as.character(sample(min:max, n, replace=T))))
   }
   
   
###### Table 1 ###### 
   
   tbl1_func <- function(df, cont_vars, cat_vars, strat_var){
      
      quiet <- function(x) { 
         sink(tempfile()) 
         on.exit(sink()) 
         invisible(force(x)) 
      } 
      
      tbl1 <- CreateTableOne(vars = c(cont_vars,cat_vars), strata = strat_var, 
                             data = df, factorVars = cat_vars, addOverall = TRUE,
                             test = FALSE, )
      temp <- quiet(print(tbl1))
      temp <- temp[,c(2,3,1)]
      
      
      tbl1$table <- temp
      tbl1$table <- data.frame(cbind(rownames(tbl1$table),tbl1$table), check.names = F)
      colnames(tbl1$table)[1] <- " "
      tbl1$missingdata <- round(tbl1$MetaData$percentMissing[tbl1$MetaData$percentMissing > 0],digits=1)
      tbl1$missingdata <- paste(names(tbl1$missingdata),paste0(tbl1$missingdata,sep=" %"),sep=": ",collapse="; " )
      md <- paste0("Missing data: ", tbl1$missingdata)
      
      tbl1$table <- merge_at(add_footer(merge_h(add_header(flextable(tbl1$table),` `="Baseline characteristics", A="Baseline characteristics", B="Baseline characteristics", Overall="Baseline characteristics", top=TRUE),part="header"), ` `=paste0("Missing data: ", tbl1$missingdata)),j=1:4,part="footer")
      
      return(FitFlextableToPage(tbl1$table))
      
      #return(kable(tbl1$table,format = "latex", booktabs = T, linesep = "",
       #            caption="Baseline characteristics") %>% kable_styling(latex_options = "hold_position") %>% add_footnote(paste0("Missing data: ", tbl1$missingdata), notation="none"))
   }
   
###### Table ###### 
   
   tbl_cont_func <- function(df, cont_var, strat_var = "group"){
      
      cont_vars <- colnames(df)[grepl(cont_var,colnames(df))]
      
      quiet <- function(x) { 
         sink(tempfile()) 
         on.exit(sink()) 
         invisible(force(x)) 
      } 
      
      tbl1 <- CreateTableOne(vars = c(cont_vars), strata = strat_var, 
                             data = df, addOverall = TRUE,
                             test = FALSE, )
      temp <- quiet(print(tbl1))
      temp <- temp[,c(2,3,1)]
      
      
      tbl1$table <- temp
      tbl1$table <- data.frame(cbind(rownames(tbl1$table),tbl1$table), check.names = F)
      colnames(tbl1$table)[1] <- " "
      tbl1$missingdata <- round(tbl1$MetaData$percentMissing[tbl1$MetaData$percentMissing > 0],digits=1)
      tbl1$missingdata <- paste(names(tbl1$missingdata),paste0(tbl1$missingdata,sep=" %"),sep=": ",collapse="; " )
      
      tbl1$table <- merge_at(add_footer(merge_h(add_header(flextable(tbl1$table),` `="Summarised results", A="Summarised results" , B="Summarised results", Overall="Summarised results", top=TRUE),part="header"), ` `=paste0("Missing data: ", tbl1$missingdata)),j=1:4,part="footer")
      
      return(FitFlextableToPage(tbl1$table))
      
   }
   
   tbl_cat_func <- function(df, cat_vars, strat_var = "group"){
      
      cat_vars <- colnames(df)[grepl(cat_vars,colnames(df))]
      
      quiet <- function(x) { 
         sink(tempfile()) 
         on.exit(sink()) 
         invisible(force(x)) 
      } 
      
      tbl1 <- CreateTableOne(vars = c(cat_vars), strata = strat_var, 
                             data = df, factorVars = cat_vars, addOverall = TRUE,
                             test = FALSE, )
      temp <- quiet(print(tbl1))
      temp <- temp[,c(2,3,1)]
      
      
      tbl1$table <- temp
      tbl1$table <- data.frame(cbind(rownames(tbl1$table),tbl1$table), check.names = F)
      colnames(tbl1$table)[1] <- " "
      tbl1$missingdata <- round(tbl1$MetaData$percentMissing[tbl1$MetaData$percentMissing > 0],digits=1)
      tbl1$missingdata <- paste(names(tbl1$missingdata),paste0(tbl1$missingdata,sep=" %"),sep=": ",collapse="; " )
      
      tbl1$table <- merge_at(add_footer(merge_h(add_header(flextable(tbl1$table),` `="Summarised results", A="Summarised results" , B="Summarised results", Overall="Summarised results", top=TRUE),part="header"), ` `=paste0("Missing data: ", tbl1$missingdata)),j=1:4,part="footer")
      
      return(FitFlextableToPage(tbl1$table))
      
   }

###### DF to table #####
   df_to_linreg <- function(df){
      
      return(FitFlextableToPage(merge_h(add_header(flextable(df),` `="Linear regression", `p-value`="Linear regression", top=TRUE),part="header")))
      
   }
   
   df_to_mwu <- function(df){
      
      return(FitFlextableToPage(merge_h(add_header(flextable(df),` `="Mann-Whitney U test",`Estimate (95% Confidence interval)`= "Mann-Whitney U test", `p-value`="Mann-Whitney U test", top=TRUE),part="header")))
      
   }
   
   
###### Feasibility outcomes ###### 
   
   feasibility_outcome <- function(numerator, demonitator, name, requirement){
   
      prop_result <- NULL
      prop_result$mean <- numerator/demonitator
      SE <- sqrt(prop_result$mean*(1-prop_result$mean)/demonitator)
      E <- qnorm(.975)*SE
      prop_result$lcl <- prop_result$mean-E
      prop_result$ucl <- prop_result$mean+E
      prop_result$ucl[prop_result$ucl>1] <- 1
      
      prop_result$fig <- ggplot() + geom_point(aes(x=1,y=prop_result$mean)) + 
         geom_errorbar(aes(x=1,ymin=prop_result$lcl,ymax=prop_result$ucl), width=0.1) +
         ggtitle(name) + xlim(0.8,1.2) +
         annotate("rect", xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = requirement-0.01, 
                  fill = "red", color=NA, alpha = 0.1) +
         annotate("rect", xmin = -Inf,xmax = Inf,ymin = requirement-0.01, ymax = Inf,
                  fill = "green",color=NA,  alpha = 0.1) +
         scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0,1)) +
         theme_classic() + theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
                                 axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                                 axis.line = element_blank(), plot.title.position = "panel",
                                 plot.margin = unit(c(0,0,0,0),"cm")) + coord_flip() 
      
      prop_result$name <- name
      prop_result$requirement <- requirement
      prop_result$perc_mean <- paste0(round(prop_result$mean*100,digits=0),"%")
      prop_result$perc_lcl <- paste0(round(prop_result$lcl*100,digits=0),"%")
      prop_result$perc_ucl <- paste0(round(prop_result$ucl*100,digits=0),"%")
      prop_result$perc_requirement <- paste0(round(prop_result$requirement*100,digits=0),"%")
      
      return(prop_result)
   }
   
      
###### MWU ######
   
   MWU_test <- function(df,var,digs=2){
      
      var <- gsub("\\(","\\\\(",var)
      var <- gsub("\\)","\\\\)",var)
      
      test <- wilcox.test(df[df$group == "A",grepl(var,colnames(df))],df[df$group == "B",grepl(var,colnames(df))], correct=F,conf.int=T)
      
      output <- NULL
      output$test <- test
      output$estimate <- round(test$estimate[[1]],digits=digs)
      output$p <- round(test$p.value,digits=digs*2)
      output$lcl <- round(test$conf.int[1],digits=digs)
      output$ucl <- round(test$conf.int[2],digits=digs)
      
      output$est_ci <- paste0(output$estimate, " (", output$lcl, "-", output$ucl,")")
      
      return(output)
   }
   
###### Linear regression ######
   
   lin_reg <- function(df,var,time=c("baseline","follow-up")){
      
      temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      temp1 <- temp[,grepl(paste0(paste0(time, collapse="|"),"|group|pt_id"),colnames(temp))]
      colnames(temp1) <- c("pt_id","group","baseline","followup")
      temp1$baseline <- as.numeric(temp1$baseline)
      temp1$followup <- as.numeric(temp1$followup)
      lin_reg <- lm(followup~group+baseline, data = temp1)
      
      output <- NULL
      lin_p <- summary(lin_reg)$coefficients
      output$lin_reg <- lin_reg
      output$lin_p <- round(lin_p[grepl("group",row.names(lin_p)),ncol(lin_p)],digits=5)
      
      if(min(temp1$baseline,na.rm=T) > 0 & min(temp1$followup,na.rm=T) > 0){
         temp1$log_baseline <- log10(temp1$baseline)
         temp1$log_followup <- log(temp1$followup)
         lin_reg_log <- lm(log_followup~group+log_baseline, data = temp1)
         lin_log_p <- summary(lin_reg_log)$coefficients
         output$lin_reg_log <- lin_reg_log
         output$lin_log_p <- round(lin_log_p[grepl("group",row.names(lin_log_p)),ncol(lin_log_p)],digits=5)
      }else{
         output$lin_reg_log <- NA
         output$lin_log_p <- NA
      }
      
      output$df <- temp1
      
      return(output)
   }
   
###### Logistic regression ######
   
   log_reg <- function(df,var){
      
      OR_table <- function(x, d = 3, intercept = F){
         
         temp1 <- as.data.frame(exp(coef(x)))
         temp2 <- as.data.frame(exp(confint(x)))
         temp3 <- as.data.frame(summary(x)$coef[,4])
         rownames(temp2) <- rownames(temp1)
         results <- as.data.frame(cbind(temp1,cbind(temp2,temp3)))
         colnames(results) <- c("or","lcl","ucl","p")
         results <- round(results,digits=3)
         if(intercept == FALSE){
            results <- results[c(2:nrow(results)),]
         }
         return(results)
      }
      
      temp1 <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      colnames(temp1) <- c("pt_id","group","outcome")
      
      #temp1 <- read.csv2("log_to_RR.csv")
      
      temp1$outcome <- as.factor(temp1$outcome)
      temp1$group <- as.factor(temp1$group)
      log_reg <- glm(outcome~group, data = temp1, family="binomial")
      
      output <- NULL
      output$or_table <- OR_table(log_reg)
      output$or_p <- output$or_table[ncol(output$or_table)]
      
      
      p <- predict(object = log_reg,
                   newdata = temp1,
                   type = "response",
                   se.fit = TRUE)
      mult <- qnorm(0.5*(1-0.95))
      out <- cbind(p$fit,
                   p$se.fit,
                   p$fit+p$se.fit*mult,
                   p$fit-p$se.fit*mult)
     
      colnames(out) <- c("rr", "Std.Err", "lcl", "ucl")
      
      out <- data.frame(unique(out))
      out <- out[,c("rr","lcl","ucl")]
      out <- out[1,]/out[2,]
      out[,c("lcl","ucl")] <- 0
      out$p <- 0
       output$rr_table <- out
      
      rr <- data.frame(t(c(epitools::riskratio(temp1$group,temp1$outcome)$measure[2,],NA)))
      colnames(rr) <- c("rr", "lcl", "ucl","p")
      output$rr_table <- rr
      
      # if(min(temp1$baseline,na.rm=T) > 0 & min(temp1$followup,na.rm=T) > 0){
      #    temp1$log_baseline <- log10(temp1$baseline)
      #    temp1$log_followup <- log(temp1$followup)
      #    lin_reg_log <- lm(log_followup~group+log_baseline, data = temp1)
      #    lin_log_p <- summary(lin_reg_log)$coefficients
      #    output$lin_reg_log <- lin_reg_log
      #    output$lin_log_p <- round(lin_log_p[grepl("group",row.names(lin_log_p)),ncol(lin_log_p)],digits=5)
      # }else{
      #    output$lin_reg_log <- NA
      #    output$lin_log_p <- NA
      # }
      # 
      # output$df <- temp1
      
      return(output)
   }   
   
   
###### Assumption-plot ###### 
   fig_lin_assumptions <- function(input, name){
      
      #Residuals QQ
      gg_resQQ <- function(LM) { # argument: a linear model  
         y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
         x <- qnorm(c(0.25, 0.75))
         slope <- diff(y)/diff(x)
         int <- y[1L] - slope * x[1L]
         p <- ggplot(LM, aes(sample=.resid)) +
            stat_qq(alpha = 0.5) +
            geom_abline(slope = slope, intercept = int, color="blue")
         
         return(p)
      }
      
      var <- input$df
      a1 <- gg_resQQ(input$lin_reg) + theme_minimal() + ggtitle("Residual QQ-plot") + 
               theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                     plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
      homogein <- data.frame(fitted=fitted(input$lin_reg), residuals=residuals(input$lin_reg))
      a2 <- ggplot(data=homogein, aes(x=fitted, y=residuals)) +
            geom_point() +
            geom_smooth(method="lm",se=F) + theme_minimal() + ggtitle("Homogeneity") +
            theme( plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
      if(!is.na(input$lin_reg_log)){
         b1 <- gg_resQQ(input$lin_reg_log) + theme_minimal() + ggtitle("Residual QQ-plot (log)") + 
                  theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                     plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
         homogein <- data.frame(fitted=fitted(input$lin_reg_log), residuals=residuals(input$lin_reg_log))
         b2 <- ggplot(data=homogein, aes(x=fitted, y=residuals)) +
            geom_point() +
            geom_smooth(method="lm",se=F) + theme_minimal() + ggtitle("Homogeneity (log)") +
            theme( plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                   plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      }else{
         b1 <- ggplot() + theme_minimal() + ggtitle("Residual QQ-plot (log)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
         b2 <- ggplot() + theme_minimal() + ggtitle("Homogeneity (log)") + 
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      }
      
     return(grid.arrange(a1,a2,b1,b2,ncol=4, top=paste0("Assumptions for linear regression | ",name)))
   }
   
   #LINEAR REGRESSION
   
   
   


###### Continuous figure ###### 
   fig_num_cont <- function(df, var, x_axis = " ", y_axis,type){
      
      temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      temp1 <- NULL
      for(i in c(3:ncol(temp))){
         time <- strsplit(colnames(temp)[i],"_")[[1]][length(strsplit(colnames(temp)[i],"_")[[1]])]
         temp1 <- data.frame(rbind(temp1,cbind(temp$pt_id,temp$group,time,temp[[i]])))
      }
      colnames(temp1) <- c("pt_id","group","time","result")
      temp1$result <- as.numeric(temp1$result)
      
      median_IQR <- function(x) {
         data.frame(y = median(x), # Median
                    ymin = quantile(x)[2], # 1st quartile
                    ymax = quantile(x)[4])  # 3rd quartile
      }
      
      mean_CI <- function(x) {
         data.frame(y = mean(x), # Median
                    ymin = t.test(x)$conf.int[1], # 1st quartile
                    ymax = t.test(x)$conf.int[2])  # 3rd quartile
      }
      
      if(type == "median"){
         g1 <- ggplot(data=temp1, aes(x=time, y=result, color=group, group=group)) + 
            stat_summary(fun=median, geom="point", position=position_dodge(width=0.25)) +
            stat_summary(fun=median, geom="line", position=position_dodge(width=0.25)) +
            stat_summary(fun.data=median_IQR, geom="errorbar", width=0.0, position=position_dodge(width=0.25)) +
            theme_minimal() + labs(x=x_axis,y=y_axis,title="Median (IQR)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  legend.title = element_blank(), plot.margin = unit(c(0,0,0,0),"cm"),
                  legend.margin = unit(c(0,0,0,0),"cm"))
      }else{
         g1 <- ggplot(data=temp1, aes(x=time, y=result, color=group, group=group)) + 
            stat_summary(fun=mean, geom="point", position=position_dodge(width=0.25)) +
            stat_summary(fun=mean, geom="line", position=position_dodge(width=0.25)) +
            stat_summary(fun.data=mean_CI, geom="errorbar", width=0.2, position=position_dodge(width=0.25)) +
            theme_minimal() + labs(x=x_axis,y=y_axis, title="Mean (95%CI)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  legend.title = element_blank(), plot.margin = unit(c(0,0,0,0),"cm"),
                  legend.margin = unit(c(0,0,0,0),"cm"), legend.spacing = unit(0, 'cm'))
      }
      return(g1)
   }

###### Continuous ###### 

   