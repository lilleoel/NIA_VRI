#SIMULATED COHORT
library(tableone)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(nlme)

###### HELPER FUNCTIONS ######

FitFlextableToPage <- function(ft, pgwidth = 6){
   
   ft_out <- ft %>% autofit()
   
   ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
   return(ft_out)
}
quiet <- function(x) { 
   sink(tempfile()) 
   on.exit(sink()) 
   invisible(force(x)) 
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

tbl_func <- function(df, strat_var, vars = NULL, Grepl=FALSE){
   require(Publish)
   
   if(Grepl) vars <- colnames(df)[grepl(paste0(vars,collapse="|"),colnames(df))]
   vars <- paste0("`",vars,"`")
   
   temp <- univariateTable(formula(paste0(strat_var, "~",paste0(vars,collapse="+"))),data=df, compare.groups = F)
   temp <- quiet(print(temp))
   
   for(i in unique(temp$Variable)){
      if(length(levels(as.factor(df[[i]]))) == 2){
         temp$Variable[which(temp$Variable %in% i)+1] <- temp$Variable[which(temp$Variable %in% i)]
         temp <- temp[!(temp$Variable == i & temp$Level == levels(as.factor(df[[i]]))[1]),] 
      }
   }
   
   colnames(temp)[1] <- "  "
   colnames(temp)[2] <- " "
   colnames(temp) <- gsub("\\(","\n\\(",colnames(temp))
   colnames(temp) <- gsub(paste0(strat_var," = "),"",colnames(temp))
   
   temp[] <- lapply(temp,FUN=function(x) gsub("NaN \\(NA\\)","",x))
   
   temp <- data.frame(temp, check.names = F)
   temp[[1]] <- gsub("\\."," ",temp[[1]])
   
   return(FitFlextableToPage(align(flextable(data.frame(temp, check.names = F)),align="center",part="header")))
}




###### DF to table #####
   df_to_linreg <- function(df){
      
      return(FitFlextableToPage(merge_h(add_header(flextable(df),` `="Linear regression", `p-value`="Linear regression", top=TRUE),part="header")))
      
   }
   
   df_to_mwu <- function(df){
      
      return(FitFlextableToPage(merge_h(add_header(flextable(df),` `="Mann-Whitney U test",`Estimate (95% Confidence interval)`= "Mann-Whitney U test", `p-value`="Mann-Whitney U test", top=TRUE),part="header")))
      
   }
   
   
###### Logistic regression ######

   log_reg <- function(df,vars,outcome){
      
      OR_table <- function(x, d = 3, intercept = F, simplify = T){
         
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
         if(simplify == TRUE){
            results$or <- paste0(results$or, " [", results$lcl,";", results$ucl,"]")
            results$lcl <- NULL; results$ucl <- NULL
            colnames(results) <- c("or_ci","p")
         }
         return(results)
      }
      
      table_uni <- NULL
      for(i in 1:length(vars)){
         mod <- as.formula(sprintf(paste0("factor(",outcome, ") ~ ", vars[i])))
         
         table_uni <- rbind(table_uni,OR_table(glm(mod,data=df, family=binomial)))
      }
      
      vars_multi <- rownames(table_uni)[table_uni$p < 0.05]
      
      mod <- as.formula(sprintf(paste0("factor(",outcome, ") ~ ", paste0(vars_multi,collapse=" + "))))
      
      table_multi <- OR_table(glm(mod,data=df, family=binomial))
      
      
      table_combined <- merge(table_uni,table_multi, by=0, all.x=T)
      
      return(table_combined)
   }   
   
   
###### Continuous figure ###### 
   # df <- df_samples[df_samples$sample_group == "Negative" | df_samples$sample_group == "Positive",]
   # var <- "CSV.WBC.RBC.ratio"
   # group <- "sample_group"
   # title <- "CSF-WBC / CSF-RBC"
   # 
   # 
   fig_boxplot <- function(df, var, group, title = " "){
      
      temp <- aggregate(df[[group]][!is.na(df[[var]])],by=list(df[[group]][!is.na(df[[var]])]),length)
      colnames(temp) <- c("sample_group","n")
      temp$new_name <- paste0(temp$sample_group, "\n(n = ", temp$n, ")")
      df <- merge(df,temp[,c("sample_group","new_name")],by="sample_group",all.x=T)
      
      my_comparisons <- list( unique(df$new_name))
      
      g1 <- ggplot(data=df, aes(x=df$new_name, y=df[[var]], color=df$new_name)) + 
         geom_boxplot() + geom_jitter(width=0.2) +
         theme_minimal() + 
         theme(axis.ticks.x=element_blank(), axis.text.y = element_text(angle=90, hjust=0.5)) +
         labs(title=title) +
         stat_compare_means(comparisons = my_comparisons,label = "p.adj",hjust=0) + 
         theme(plot.subtitle = element_text(hjust=0.5, face="bold")) +
         scale_color_manual(values=c("#666666", "#000000")) +
         theme(axis.title = element_blank(), axis.text.y = element_text(angle=0)) +
         theme(legend.position = "none") + coord_flip() + theme(plot.margin=unit(c(0,0,0,0), "mm"))
      
      return(g1)
   }

###### PPV ###### 

  pred_table <- function(df, var, group_criteria,test_criteria, cols){

      df <- df[,cols][complete.cases(df[,cols]),]
      temp <- NULL
      temp$outcome <- eval(parse(text = test_criteria))
      temp$group <- eval(parse(text = group_criteria))
      temp <- data.frame(temp)[complete.cases(temp),]
      temp <- table(temp)[2:1,2:1]
      res_temp <- epiR::epi.tests(temp)
      temp <- round(summary(res_temp),digits=2)
      temp$est <- paste0(temp$est, " (", temp$lower,"-", temp$upper,")")
      temp$lower <- NULL
      temp$upper <- NULL
      output <- data.frame(cbind(paste0(var, " (n=",res_temp$tab[3,3],")"),temp["se",],temp["sp",],temp["ppv",],temp["npv",]))
      colnames(output) <- c("Variables","Sensitivity","Specificity","PPV","NPV")
      
      
      return(output)  
   }
   
   
      
   