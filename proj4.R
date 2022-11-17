newt<-function(theta,func,grad,k,hess=NULL,tol=1e-8,fscale=1,maxit=100,max.half=20,eps=1e-6){
  ## This function is to find the minimum value by using newton's method.
  ## It takes several arguments as input.'theta' is a vector of initial values 
  ## for the optimization parameters. 'func' is objective function.'grad' is the
  ## gradient function.'hess' is the hessian function. Note that 'func' and 'grad'
  ## must be provided, while 'hess' need to compute approximation of hessian matrix
  ## if not supplied. 'tol' is the convergence tolerance.'fscale' is a rough estimate 
  ## of the magnitude of func near the optimum - used in convergence testing.'maxit'
  ## is the maximum number of Newton iterations to try before giving up.'max.half'
  ## is the maximum number of times a step should be halved before concluding that 
  ## the step has failed to improve the objective.'eps' is the finite difference 
  ## intervals to use when a Hessian function is not provided.
  ## It returns a list containing 5 items.'f' is the value of the objective 
  ## function at the minimum.'theta' is the value of the parameters at the minimum.
  ## 'iter' is the number of iterations taken to reach the minimum.'g' is the 
  ## gradient vector at the minimum.'Hi' is the inverse of the Hessian matrix at
  ## the minimum.
  
  ## If the objective or derivatives are not finite at the initial theta
  if(any(is.infinite(func(theta))) | any(is.infinite(grad(theta)))){
    ## issue error and interrupt process
    stop('The objective or derivatives are not finite at the initial theta')
  }
  
  k=0 ## used to record the number of iterations,initialize to 0
  while(k<=maxit){ ## keep loop if the number of iterations is smaller than threshold
    ## if meet stopping criteria: ||grad(theta)|| < tol*(||func(theta)||+fscale)
    if(abs(norm(grad(theta),type='2')) < tol*(abs(norm(func(theta),type='2'))+fscale)){
      break ## interrupt
    }
    
    if (is.null(hess)){ ## if 'hess' not provided, compute Hessian matrix approximately
      row1<-(grad(theta+c(eps,0))-grad(theta-c(eps,0)))/(2*eps) ## get fxx and fxy
      row2<-(grad(theta+c(0,eps))-grad(theta-c(0,eps)))/(2*eps) ## get fxy and fyy
      hess_mat<-matrix(cbind(row1,row2),nrow=2,ncol=2) ## Combination into Hessian matrix
    } else {
      hess_mat<-hess(theta) ## compute Hessian matrix by formula directly
    }
    
    ## If Hessian matrix is not positive definite, add a multiple beta of the
    ## identity matrix to it.
    beta<-1e-6 ## initialize to a very small number
    ea<-eigen(hess_mat,symmetric=TRUE) ## eigen-decomposition of Hessian matrix
    ## If the Hessian is not positive definite at convergence
    if(any(ea$values<=0)){ ## if not all eigen values greater than 0
      warning('the Hessian is not positive definite. Already corrected')
      while(TRUE){ ## keep loop if modified Hessian is still not positive definite
        y<-tryCatch(length(chol(hess_mat+beta*diag(length(theta)))),error=function(e) {return(1)})
        if(y==1){ ## if modified Hessian is not positive definite
          beta=beta*10 ## multiply the multiplier by 10
        }else{ ## if modified Hessian is positive definite
          break
        }
      }
    }
    
    ## get newton direction: -(inverse of modified Hessian matrix)*gradient
    direction<-(-1)*chol2inv(chol(hess_mat+beta*diag(length(theta))))%*%grad(theta)
    
    ## Far from the optimum, the newton direction Δ might overshoot and increase objective. 
    ## If so, repeatedly halve the newton direction until func(θ + Δ) < func(θ).
    k_half<-0 ## record the number of times Δ should be halved
    alpha<-1 ## repeatedly halve the newton direction, start at 1
    while(k_half<=max.half){ ## keep loop if the number of halving times < threshold
      if(func(theta+alpha%*%t(direction))<func(theta)){ ## if meet stopping criteria
        break ## choose this alpha and interrupt
      }
      k_half=k_half+1 ## update the number of halving times
      alpha=alpha/2 ## update alpha
    }
    
    #If the step fails to reduce the objective despite trying max.half step halvings
    if(k_half>max.half){
      ## issue error and interrupt
      stop(paste('fails to reduce the objective despite trying',max.half,'step halvings'))
    }
    
    theta=theta+alpha%*%t(direction) ## update theta
    k=k+1 ##update the number of iterations
  }
  
  #If maxit is reached without convergence
  if(k>maxit){ ## if the number of iteration larger than threshold
    ## issue error and interrupt
    stop(paste('After',maxit,'steps, the objective still does not converge'))
  }
  
  obj<-list()## create a list to return
  ## below are all elements containing in obj
  obj$f<-func(theta) ## value of the objective
  obj$theta<-theta ## value of the parameters
  obj$iter<-k ## number of iterations
  obj$g<-grad(theta) ## gradient vector
  obj$hi<-chol2inv(chol(hess_mat)) ## inverse of the Hessian matrix
  return(obj)
}