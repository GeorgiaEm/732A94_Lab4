#' 
#'  ridgereg function.
#' 
#' @param formula An object of class formula..
#' @param data A data_set.
#' @param lambda A choosen penilty 
#' @return A model estimation.
#' @export ridgereg
#' @export
#' 


#devtools::document()
# formula <- Petal.Length~Species
# 
# data <- iris
# 
# X<-model.matrix(formula,data)
# lambda <- 0

ridgereg <- setRefClass("ridgereg", fields = list(formula="formula",
                                              data="data.frame",
                                              data_name="character",
                                              X="matrix",
                                              Xnorm ="matrix",
                                              I = "matrix",
                                              y="numeric",
                                              lambda="numeric",
                                              beta_hat="matrix",
                                              y_hat="matrix"),
                                           
                      methods = list(
                        initialize = function(formula,data,lambda=0){
                          data_name <<- deparse(substitute(data))
                          data<<-data
                          formula<<-formula
                          X<<-model.matrix(formula,data)
                          Xnorm <<- apply(X,2,norm<-function(x){return (x-mean(x))/var(x)})
                          y_col<-all.vars(formula)[1]
                          y<<-data[,colnames(data)==y_col]
                          I <<- diag(ncol(X))
                          beta_hat<<-solve(t(X)%*%X+lambda*I)%*%t(X)%*%y
                          y_hat<<-X%*%beta_hat
                       
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
                                                 },
                        pred = function(newdata=NULL){
                          "Returns a vector of predicted values"
                          if(is.null(newdata)){
                          return(y_hat)
                          } else{
                          X2<-model.matrix(formula,newdata)%*%beta_hat
                          y_hat2 <- apply(X2,2,norm<-function(x2){return (x2-mean(x2))/var(x2)})
                          return(y_hat2)
                          }
                        }
                        ,
                        coef = function(){
                          "Returns the coefficients."
                          coef <- as.vector(beta_hat)
                          names(coef) <- rownames(beta_hat)
                          return(coef)
                          
                        }
                        
                      )

)

