#' 
#'  Linreg function.
#' 
#' @param formula An object of class formula..
#'  @param data A data_set.
#'  @return A model estimation.
#' @export linreg
#' @export

# export
# exportClass 

linreg <- setRefClass("linreg", fields = list(formula="formula",
                                              data="data.frame",
                                              data_name="character",
                                              X="matrix",
                                              y="numeric",
                                              beta_hat="matrix",
                                              y_hat="matrix",
                                              e_hat="matrix",
                                              df="integer",
                                              sigma_2_hat="numeric",
                                              var_hat_beta_hat="matrix",
                                              t_beta="matrix",
                                              pvalue="matrix"),
                      methods = list(
                        initialize = function(formula,data){
                          data_name <<- deparse(substitute(data))
                          data<<-data
                          formula<<-formula
                          X<<-model.matrix(formula,data)
                          y_col<-all.vars(formula)[1]
                          y<<-data[,colnames(data)==y_col]
                          beta_hat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          y_hat<<-X%*%beta_hat
                          e_hat<<-y-y_hat
                          df<<-nrow(X)-(ncol(X))
                          sigma_2_hat<<-as.numeric((t(e_hat)%*%e_hat)/df)
                          var_hat_beta_hat<<-sigma_2_hat*solve(t(X)%*%X)
                          t_beta<<-beta_hat/as.numeric(sqrt(diag(var_hat_beta_hat)))
                          pvalue<<- 2*pt(-abs(t_beta), df)
                        },
                        print = function(){
                          "Prints a formula and the coefficients."
                          coef <- numeric()
                          
                          cat("Call:\nlinreg(formula = ",
                              Reduce(paste,deparse(formula)),
                              ", data = ",
                              data_name,
                              ")\n\nCoefficients:\n ",sep="")
                          
                          for (i in 1:length(beta_hat)){
                            coef[i]<-beta_hat[i]
                          }
                          names_coef<-paste(" ",rownames(beta_hat)[1]," ",sep="")
                          for (i in 2:length(coef)){
                            names_coef<-paste(names_coef,"  ",rownames(beta_hat)[i],sep="")
                          }
                          cat(names_coef)
                          cat("\n")
                          cat(coef,sep="      ")
T                        },
                        plot = function(){
                          "Plots the residuals against fitted values and Scale-Location."
                          require("ggplot2")
                          std_res<-sqrt(abs(scale(e_hat)))
                          aaa <-data.frame(y_hat,e_hat,std_res)
                          aa2 <- aaa[c(which.max(e_hat),which.min(e_hat),which.max(e_hat[-max(e_hat)])),]
                          
                          p1 <- ggplot(aaa) +
                            labs(title="Residuals vs Fitted", x = "Fitted values", y ="Residuals")+
                            aes(x = y_hat, y = e_hat) + 
                            geom_point(shape=21, colour="black", fill="white")+ # Definierar punktdiagram
                            stat_summary(fun.y=mean, colour="red", geom="line")+
                            #geom_smooth(method = "lm", se = FALSE)
                            geom_text(data=aa2, aes(x = y_hat, y = std_res, label=rownames(aa2)),hjust=1.5)+
                            theme_bw()+
                            theme(plot.title = element_text(hjust = 0.5),axis.title.y = element_text(angle=0,vjust = 0.5))
                          
                          
                          p2 <- ggplot(aaa) +
                            labs(title="scale_location", x = "Fitted values", y ="Residuals")+
                            aes(x = y_hat, y = std_res) + 
                            geom_point(shape=21, colour="black", fill="white")+ # Definierar punktdiagram
                            stat_summary(fun.y=mean, colour="red", geom="line")+
                            #geom_smooth(method = "lm", se = FALSE)
                            geom_text(data=aa2, aes(x = y_hat, y = std_res, label=rownames(aa2)),hjust=1.5)+
                            theme_bw()+
                            theme(plot.title = element_text(hjust = 0.5),axis.title.y = element_text(angle=0,vjust = 0.5))
                          
                          list(p1,p2)
                        },
                        resid = function(){
                          "Returns a vector of the residuals"
                          return(e_hat)
                        },
                        pred = function(){
                          "Returns a vector of predicted values"
                          return(y_hat)
                          
                        },
                        coef = function(){
                          "Returns the coefficients."
                          coef <- as.vector(beta_hat)
                          names(coef) <- rownames(beta_hat)
                          return(coef)
                          
                        },
                        summary = function(){
                          "Returns the coefficients, standard error, t-values and p-values"
                          cat("Call:\nlinreg(formula = ",Reduce(paste,deparse(formula)),", data = ",data_name,
                              ")\n\nCoefficients:\n ",sep="")
                          
                          Significance<-rep(NA,nrow(pvalue))
                          for (i in 1:length(pvalue)){
                            if (pvalue[i]<0.001) Significance[i]<-"***"
                            else if (pvalue[i]<0.01) Significance[i]<-"**"
                            else if (pvalue[i]<0.05) Significance[i]<-"*"
                            else if (pvalue[i]<0.1) Significance[i]<-"."
                            else Significance[i]<-" "
                          }
                          cat(paste(rownames(beta_hat),beta_hat,sqrt(diag(var_hat_beta_hat)),t_beta,pvalue,Significance),"\n",
                              sep="\n")
                          cat("Residual standard error:",sqrt(sigma_2_hat),"on" ,df, "degrees of freedom!",sep=" ")
                        }
                      )
)
