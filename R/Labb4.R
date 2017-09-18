
#' Linreg function.
#' 
#' @param formula An object of class formula..
#' @param data A data_set.
#' @return A model estimation.











linreg <- function (formula,data){
  data_<-data
  formula_<-formula
  
  data_name <<- deparse(substitute(data_))
  #Creating x and y matrices
  X<-model.matrix(formula_,data_)
  y_col<-all.vars(formula_)[1]
  y<-data_[,colnames(data_)==y_col]
 
  
  #equations
  beta_hat<-solve(t(X)%*%X)%*%t(X)%*%y
  y_hat<-X%*%beta_hat
  e_hat<-y-y_hat
  df<-nrow(X)-(ncol(X))
  sigma_2_hat<-as.numeric((t(e_hat)%*%e_hat)/df)
  var_hat_beta_hat<-sigma_2_hat*solve(t(X)%*%X)
  t_beta<-beta_hat/as.numeric(sqrt(diag(var_hat_beta_hat)))
  pvalue<-1-pt(t_beta,df)
  
  #class
  linreg2 <- setRefClass("linreg", fields = list(beta_hat = "matrix",  y_hat = "matrix",
                                               e_hat = "matrix",
                                               df = "integer",
                                               sigma_2_hat = "numeric",
                                               var_hat_beta_hat = "matrix",
                                               t_beta = "matrix",
                                               pvalue = "matrix"),
                      methods = list(
                        print.linreg <<- function(x){
                        
                          
                          coef <- as.vector(beta_hat)
                          names(coef) <- rownames(beta_hat)
                        
                          cat("Call:\n Formula:", Reduce(paste,deparse(formula_)),
                              "Data:", data_name, 
                              "\n \n \n",
                              "Coefficients:\n")
                              coef
                        },
                        plot.linreg <<- function(x){
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
                        resid.linreg <<- function(){
                          
                          return(e_hat)
                          
                        },
                        pred.linreg <<- function(){
                          return(y_hat)
                          
                        },
                        coef.linreg <<- function(x){
                          
                          coef <- as.vector(beta_hat)
                          names(coef) <- rownames(beta_hat)
                          return(coef)
                        
                        }
                      
                         

  )
  )

  resultat <- linreg2(beta_hat = beta_hat,  
                      y_hat = y_hat,
                      e_hat = e_hat,
                      df = df,
                      sigma_2_hat = sigma_2_hat,
                      var_hat_beta_hat = var_hat_beta_hat,
                      t_beta = t_beta,
                      pvalue = pvalue)
                     
                       
  
 return(resultat) 
}

