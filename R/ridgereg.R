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


# Acknowledgement: Sara Jesperson, Sara Johansson and Simon Jonsson

ridgereg <- setRefClass("ridgereg", fields = list(formula="formula",
                                                  data="data.frame",
                                                  data_name="character",
                                                  lambda="numeric",
                                                  beta_hat="matrix",
                                                  y_hat="matrix"),
                        
                        methods = list(
                          initialize = function(formula,data,lambda=0){
                            data_name <<- deparse(substitute(data))
                            data<<-data
                            formula<<-formula
                            X<-model.matrix(formula,data)
                            X[,-1] <- scale(X[,-1]) 
                            y<-as.matrix(data[,colnames(data)==all.vars(formula)[1]])
                            colnames(y)<-all.vars(formula)[1]
                            QR<-qr(X)
                            Q <- qr.Q(QR)
                            R <- qr.R(QR)
                            beta_hat<<-solve(t(R) %*% R + lambda * diag(dim(t(R) %*% R)[1])) %*% t(X) %*% y
                            y_hat <<- X %*% beta_hat
                            data <<- data.frame(y,X[,-1])
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
                          predict = function(newdata=NULL){
                            "Returns a vector of predicted values"
                            if(is.null(newdata)){
                              return(y_hat)
                              
                            } else{
                              newdata <- as.matrix(data.frame(Intercept=1,newdata))
                              y_hat_new <- (as.matrix(newdata)%*% as.matrix(beta_hat))[,1]
                              
                            }
                            return(y_hat_new)
                          },
                          coef = function(){
                            "Returns the coefficients."
                            coef <- as.vector(beta_hat)
                            names(coef) <- rownames(beta_hat)
                            return(coef)
                            
                          }
                          )
                        )
