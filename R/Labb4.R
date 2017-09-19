#' 
#' #' Linreg function.
#' #' 
#' #' @param formula An object of class formula..
#' #' @param data A data_set.
#' #' @return A model estimation.


linreg <- setRefClass("linreg", fields = list(formula="formula", data="data.frame",
                                              data_name="character",
                                              X="matrix", y="numeric", beta_hat="matrix",
                                              y_hat="matrix", e_hat="matrix", df="integer",
                                              sigma_2_hat="numeric",var_hat_beta_hat="matrix",
                                              t_beta="matrix",pvalue="matrix"),
                      methods = list(
                        
                        initialize = function(formula,data){
                          data_name <<- deparse(substitute(data))
                          data<<-data
                          formula<<-formula
                          #Creating x and y matrices
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
                          pvalue<<-1-pt(t_beta,df)
                          
                          
                          
                          
                        },
                        print = function(){
                          coef <- as.vector(beta_hat)
                          names(coef) <- rownames(beta_hat)
                          
                          a<-cat("Call:\nlinreg(formula = ", Reduce(paste,deparse(formula)),
                                 ", data = ", data_name,")\n\nCoefficients:\n",sep="")
                          a
                          coef
                          # a<-paste("linreg(formula = ", Reduce(paste,deparse(formula)),
                          #     ", data = ", data_name,")",sep="")
                          # 
                          # coef
                          # a<-list(a,coef)
                          # print(a)
                          # 
                        },
                        plot = function(){
                          requireNamespace(ggplot2)
                          std_res<-sqrt(abs(scale(e_hat)))
                          aaa <-data.frame(y_hat,e_hat,std_res)
                          p1 <- ggplot(aaa) +
                            aes(x = y_hat, y = e_hat) +
                            geom_point()+ # Definierar punktdiagram
                            geom_smooth(method = "loess", se = FALSE)
                          
                          
                          p2 <- ggplot(aaa) +
                            aes(x = y_hat, y = std_res) +
                            geom_point()+ # Definierar punktdiagram
                            geom_smooth(method = "loess", se = FALSE)
                          
                          print(p1)
                          print(p2)
                        },
                        resid = function(){
                          return(e_hat)
                        },
                        pred = function(){
                          return(y_hat)
                          
                        },
                        coef = function(){
                          coef <- as.vector(beta_hat)
                          names(coef) <- rownames(beta_hat)
                          return(coef)
                          
                        },
                        summary = function(){
                          cat("Call:\n Formula=", Reduce(paste,deparse(formula)),
                              "Data=", data_name)
                          
                          residuals<-c(min(e_hat), quantile(e_hat,0.25), median(e_hat),
                                       quantile(e_hat,0.75),max(e_hat))
                          names(residuals)<-c("Min", "1Q", "Median", "3Q", "Max")
                          cat("\n\nResiduals:\n")
                          print(residuals)
                          
                          
                          
                          cat("\n\nCoefficients:\n")
                          coef<-data.frame(beta_hat,sqrt(diag(var_hat_beta_hat)),t_beta,pvalue)
                          names(coef)<-c("Estimate","Std. Error","t-value","P-value")
                          print(coef)
                          cat("\n\nDegrees of freedom:",df)
                          
                        }
                        
                      )
)

#hej<-linreg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris)

#hej$print()
# datasets::iris
# 
# 
# 
# 
# 
# args = commandArgs(T)
# print(args)
# a<-assign(data)

#   linreg <- function (formula,data){
#   data_name <<- deparse(substitute(data))
#   #Creating x and y matrices
#   X<-model.matrix(formula,data)
#   y_col<-all.vars(formula)[1]
#   y<-data[,colnames(data)==y_col]
#   #equations
#   beta_hat<-solve(t(X)%*%X)%*%t(X)%*%y
#   y_hat<-X%*%beta_hat
#   e_hat<-y-y_hat
#   df<-nrow(X)-(ncol(X))
#   sigma_2_hat<-as.numeric((t(e_hat)%*%e_hat)/df)
#   var_hat_beta_hat<-sigma_2_hat*solve(t(X)%*%X)
#   t_beta<-beta_hat/as.numeric(sqrt(diag(var_hat_beta_hat)))
#   pvalue<-1-pt(t_beta,df)
#   #class
#   linreg2 <- setRefClass("linreg", fields = list(beta_hat = "matrix",  y_hat = "matrix",
#                                                  e_hat = "matrix",
#                                                  df = "integer",
#                                                  sigma_2_hat = "numeric",
#                                                  var_hat_beta_hat = "matrix",
#                                                  t_beta = "matrix",
#                                                  pvalue = "matrix"),
#                          methods = list(
# 
#                            print.linreg    <<- function(x){
# 
# 
#                              coef <- as.vector(beta_hat)
#                              names(coef) <- rownames(beta_hat)
# 
#                              cat("Call:\n Formula:", Reduce(paste,deparse(formula)),
#                                  "Data:", data_name,
#                                  "\n \n \n",
#                                  "Coefficients:\n")
#                              coef
#                            },
#                            plot.linreg     <<- function(x){
#                              requireNamespace(ggplot2)
#                              std_res<-sqrt(abs(scale(e_hat)))
#                              aaa <-data.frame(y_hat,e_hat,std_res)
#                              p1 <- ggplot(aaa) +
#                                aes(x = y_hat, y = e_hat) +
#                                geom_point()+ # Definierar punktdiagram
#                                geom_smooth(method = "loess", se = FALSE)
# 
# 
#                              p2 <- ggplot(aaa) +
#                                aes(x = y_hat, y = std_res) +
#                                geom_point()+ # Definierar punktdiagram
#                                geom_smooth(method = "loess", se = FALSE)
# 
#                              print(p1)
#                              print(p2)
#                            },
#                            resid.linreg    <<- function(x){
# 
#                              return(e_hat)
# 
#                            },
#                            pred.linreg     <<- function(x){
#                              return(y_hat)
# 
#                            },
#                            coef.linreg     <<- function(x){
# 
#                              coef <- as.vector(beta_hat)
#                              names(coef) <- rownames(beta_hat)
#                              return(coef)
# 
#                            },
#                            summary.linreg  <<- function(x){
#                              cat("Call:\n Formula=", Reduce(paste,deparse(formula)),
#                                  "Data=", data_name)
# 
#                              residuals<-c(min(e_hat), quantile(e_hat,0.25), median(e_hat),
#                                           quantile(e_hat,0.75),max(e_hat))
#                              names(residuals)<-c("Min", "1Q", "Median", "3Q", "Max")
#                              cat("\n\nResiduals:\n")
#                              print(residuals)
# 
# 
# 
#                              cat("\n\nCoefficients:\n")
#                              coef<-data.frame(beta_hat,sqrt(diag(var_hat_beta_hat)),t_beta,pvalue)
#                              names(coef)<-c("Estimate","Std. Error","t-value","P-value")
#                              print(coef)
#                              cat("\n\nDegrees of freedom:",df)
# 
#                            }
# 
#                          )
#   )
#   resultat <- linreg2(beta_hat = beta_hat,
#                       y_hat = y_hat,
#                       e_hat = e_hat,
#                       df = df,
#                       sigma_2_hat = sigma_2_hat,
#                       var_hat_beta_hat = var_hat_beta_hat,
#                       t_beta = t_beta,
#                       pvalue = pvalue)
#   return(resultat)
# }
