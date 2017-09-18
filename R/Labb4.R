
#' Linreg function.
#' 
#' @param formula An object of class formula..
#' @param data A data_set.
#' @return A model estimation.











linreg <- function (formula,data){
  
  data_name <<- deparse(substitute(data))
  #Creating x and y matrices
  X<-model.matrix(formula,data)
  y_col<-all.vars(formula)[1]
  y<-data[,colnames(data)==y_col]
 
  
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
                        
                          cat("Call:\n Formula:", Reduce(paste,deparse(formula)),
                              "Data:", data_name, 
                              "\n \n \n",
                              "Coefficients:\n")
                              coef
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
                        
                        },
                        plot.linreg <<- function(x){
                          hej
                          require(ggplot2)
                          require(grid)
                          std_res<-sqrt(abs(scale(e_hat)))
                          aaa <-data.frame(hej$y_hat,hej$e_hat,std_res)
                          p1 <- ggplot(aaa) + 
                            aes(x = hej.y_hat, y = hej.e_hat) + 
                            geom_point()+ # Definierar punktdiagram
                            geom_smooth(method = "loess", se = FALSE)
                          
                          
                          p2 <- ggplot(aaa) +
                            aes(x = hej.y_hat, y = std_res) + 
                            geom_point()+ # Definierar punktdiagram
                            geom_smooth(method = "loess", se = FALSE)
                          
                          print(p1)
                          print(p2)
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



hej <- linreg(Petal.Length~Species, data = iris)

plot.linreg(hej)
pred.linreg(hej)



beta_hat_Q <- qr.Q(qr(hej$beta_hat))
beta_hat_R <- qr.R(qr(hej$beta_hat))

var_hat_beta_hat_Q <- qr.Q(qr(hej$var_hat_beta_hat))
var_hat_beta_hat_R <- qr.R(qr(hej$var_hat_beta_hat))


                            
                                      
#Lite exempel
data<-iris
formula<-Petal.Length~Species


qr.Q(qr(beta_hat))
qr.R(qr(beta_hat))
qr.Q(qr(var(beta_hat)))
qr.R(qr(var(beta_hat)))












