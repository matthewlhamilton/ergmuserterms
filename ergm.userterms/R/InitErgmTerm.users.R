



# new 09 09 2018 ----



InitErgmTerm.nsp2attr <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","p1keep","p2keep"),
                       vartypes = c("character","numeric", "numeric"),
                       defaultvalues = list(NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  p1u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  nodecov <- match(nodecov,p1u,nomatch=length(p1u)+1)
  
  u <- c(a$p1keep,a$p2keep)
  
  coef.names <- paste0("nsp2attr.", a$attrname, ".", p1u[a$p1keep], "_", p1u[a$p2keep])
  list(name = "nsp2attr", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}



InitErgmTerm.nsp2attr3 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","pexcl"),
                       vartypes = c("character","numeric"),
                       defaultvalues = list(NULL, NULL),
                       required = c(TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  exclu<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ exclu<-c(exclu,NA) }
  nodecov <- match(nodecov,exclu,nomatch=length(exclu)+1)
  
  u <- c(a$pexcl)
  
  coef.names <- paste0("nsp2attr3.", a$attrname, ".excl:", exclu[a$pexcl])
  list(name = "nsp2attr3", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}



InitErgmTerm.nsp2attr2 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("pattrname","p1keep","p2keep","battrname","bkeep"),
                       vartypes = c("character","numeric","numeric","character","numeric"),
                       defaultvalues = list(NULL,NULL,NULL,NULL,NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE,TRUE))  
  n <- network.size(nw)
  
  pnodecov <- get.node.attr(nw,a$pattrname)
  bnodecov <- get.node.attr(nw,a$battrname)
  
  p1u<-sort(unique(pnodecov))
  if(any(is.na(pnodecov))){ p1u<-c(p1u,NA) }
  pnodecov <- match(pnodecov,p1u,nomatch=length(p1u)+1)
  
  bu<-sort(unique(bnodecov))
  if(any(is.na(bnodecov))){ bu<-c(bu,NA) }
  bnodecov <- match(bnodecov,bu,nomatch=length(bu)+1)
  
  u <- c(a$p1keep,a$p2keep,a$bkeep)
  
  coef.names <- paste0("nsp2attr2.", a$pattrname, ".", p1u[a$p1keep], ">",a$battrname, ".", bu[a$bkeep], ">", a$pattrname, ".", p1u[a$p2keep])
  list(name = "nsp2attr2", coef.names = coef.names, #name and coef.names: required
       inputs = c(pnodecov,bnodecov,u), minval=0)
}



# new 09 06 2018 ----



InitErgmTerm.dgwespattr3 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","pexcl","alpha"),
                       vartypes = c("character","numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  exclu<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ exclu<-c(exclu,NA) }
  nodecov <- match(nodecov,exclu,nomatch=length(exclu)+1)
  
  u <- c(a$pexcl, a$alpha)
  
  coef.names <- paste0("dgwespattr3.", a$attrname, ".excl:", exclu[a$pexcl], ".a:",a$alpha)
  list(name = "dgwespattr3", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}




InitErgmTerm.dgwespattr2 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("pattrname","p1keep","p2keep","battrname","bkeep","alpha"),
                       vartypes = c("character","numeric","numeric","character","numeric","numeric"),
                       defaultvalues = list(NULL,NULL,NULL,NULL,NULL,NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE,TRUE, TRUE))  
  n <- network.size(nw)
  
  pnodecov <- get.node.attr(nw,a$pattrname)
  bnodecov <- get.node.attr(nw,a$battrname)
  
  p1u<-sort(unique(pnodecov))
  if(any(is.na(pnodecov))){ p1u<-c(p1u,NA) }
  pnodecov <- match(pnodecov,p1u,nomatch=length(p1u)+1)
  
  bu<-sort(unique(bnodecov))
  if(any(is.na(bnodecov))){ bu<-c(bu,NA) }
  bnodecov <- match(bnodecov,bu,nomatch=length(bu)+1)
  
  u <- c(a$p1keep,a$p2keep,a$bkeep,a$alpha)
  
  coef.names <- paste0("dgwespattr2.", a$pattrname, ".", p1u[a$p1keep], ">",a$battrname, ".", bu[a$bkeep], ">", a$pattrname, ".", p1u[a$p2keep],".a:",a$alpha)
  list(name = "dgwespattr2", coef.names = coef.names, #name and coef.names: required
       inputs = c(pnodecov,bnodecov,u), minval=0)
}


# this one measures esp brokered by nodes with a certain attr, e.g., for vertical brokerage
InitErgmTerm.dgwespattr <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","p1keep","p2keep","alpha"),
                       vartypes = c("character","numeric", "numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  p1u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  nodecov <- match(nodecov,p1u,nomatch=length(p1u)+1)
  
  u <- c(a$p1keep,a$p2keep, a$alpha)
  
  coef.names <- paste0("dgwespattr.", a$attrname, ".", p1u[a$p1keep], "_", p1u[a$p2keep],".a:",a$alpha)
  list(name = "dgwespattr", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}

# ----


InitErgmTerm.edgecovgwnspattr<-function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist,
                      varnames = c("decay","mat","attrname","brcat"),
                      vartypes = c("numeric","matrix,network,character","character","numeric"),
                      defaultvalues = list(NULL,NULL,NULL,NULL),
                      required = c(TRUE, TRUE, TRUE, TRUE))
  decay<-a$decay
  
  xm<-as.matrix(a$mat)
  
  nodecov <- get.node.attr(nw, a$attrname)
  brattr<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ brattr<-c(brattr,NA) }
  nodecov <- match(nodecov,brattr,nomatch=length(brattr)+1)
  
  dname <- "edgecovgwnspattr"
  coef.names <- paste0("edgecovgwnspattr.",decay,".",a$attrname,".",brattr[a$brcat])
  u <- c(decay,a$brcat, NCOL(xm),nodecov,as.double(xm))
  
  list(name=dname, coef.names=coef.names, inputs=u)    
}

InitErgmTerm.edgecovgwnsp<-function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist,
                      varnames = c("decay","mat"),
                      vartypes = c("numeric","matrix,network,character"),
                      defaultvalues = list(NULL,NULL),
                      required = c(TRUE, TRUE))

  decay<-a$decay

  xm<-as.matrix(a$mat)
  coef.names <- paste("edgecovgwnsp.", decay,sep="")
  dname <- "edgecovgwnsp"
  list(name=dname, coef.names=coef.names, inputs=c(decay,NCOL(xm), as.double(xm)))    
}



InitErgmTerm.dgwnspattr3 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","pexcl","alpha"),
                       vartypes = c("character","numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  exclu<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ exclu<-c(exclu,NA) }
  nodecov <- match(nodecov,exclu,nomatch=length(exclu)+1)
  
  u <- c(a$pexcl, a$alpha)
  
  coef.names <- paste0("dgwnspattr3.", a$attrname, ".excl:", exclu[a$pexcl], ".a:",a$alpha)
  list(name = "dgwnspattr3", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}



InitErgmTerm.dgwnspattr2 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("pattrname","p1keep","p2keep","battrname","bkeep","alpha"),
                       vartypes = c("character","numeric","numeric","character","numeric","numeric"),
                       defaultvalues = list(NULL,NULL,NULL,NULL,NULL,NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE,TRUE, TRUE))  
  n <- network.size(nw)
  
  pnodecov <- get.node.attr(nw,a$pattrname)
  bnodecov <- get.node.attr(nw,a$battrname)
  
  p1u<-sort(unique(pnodecov))
  if(any(is.na(pnodecov))){ p1u<-c(p1u,NA) }
  pnodecov <- match(pnodecov,p1u,nomatch=length(p1u)+1)
  
  bu<-sort(unique(bnodecov))
  if(any(is.na(bnodecov))){ bu<-c(bu,NA) }
  bnodecov <- match(bnodecov,bu,nomatch=length(bu)+1)
  
  u <- c(a$p1keep,a$p2keep,a$bkeep,a$alpha)
  
  coef.names <- paste0("dgwnspattr2.", a$pattrname, ".", p1u[a$p1keep], ">",a$battrname, ".", bu[a$bkeep], ">", a$pattrname, ".", p1u[a$p2keep],".a:",a$alpha)
  list(name = "dgwnspattr2", coef.names = coef.names, #name and coef.names: required
       inputs = c(pnodecov,bnodecov,u), minval=0)
}



InitErgmTerm.dgwnspattr <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","p1keep","p2keep","alpha"),
                       vartypes = c("character","numeric", "numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  p1u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  nodecov <- match(nodecov,p1u,nomatch=length(p1u)+1)
  
  u <- c(a$p1keep,a$p2keep, a$alpha)
  
  coef.names <- paste0("dgwnspattr.", a$attrname, ".", p1u[a$p1keep], "_", p1u[a$p2keep],".a:",a$alpha)
  list(name = "dgwnspattr", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}















InitErgmTerm.path2attr <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE,
                       varnames = c("a1","a2","a3","v2","a1keep","a2keep","a3keep","v2keep"), # a1 > a2 > a3
                       vartypes = c("character","character","character","character","numeric", "numeric","numeric", "numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL, NULL, NULL,NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))  
  # n <- network.size(nw)
  
  a1nodecov <- get.node.attr(nw, a$a1)
  a2nodecov <- get.node.attr(nw, a$a2)
  a3nodecov <- get.node.attr(nw, a$a3)
  v2nodecov <- get.node.attr(nw, a$v2)
  
  a1u<-sort(unique(a1nodecov))
  a2u<-sort(unique(a2nodecov))
  a3u<-sort(unique(a3nodecov))
  v2u<-sort(unique(v2nodecov))
  
  # if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  a1nodecov <- match(a1nodecov,a1u,nomatch=length(a1u)+1)
  a2nodecov <- match(a2nodecov,a2u,nomatch=length(a2u)+1)
  a3nodecov <- match(a3nodecov,a3u,nomatch=length(a3u)+1)
  v2nodecov <- match(v2nodecov,v2u,nomatch=length(v2u)+1)
  
  u <- c(a$a1keep,a$a2keep, a$a3keep, a$v2keep)
  
  coef.names <- paste0("path2attr.", a$a1, ":", a1u[a$a1keep], ">", a$a2, ":", a2u[a$a2keep], ">", a$a3, ":", a3u[a$a3keep], "...var:", a$v2, ":", v2u[a$v2keep])
  list(name = "path2attr", coef.names = coef.names, #name and coef.names: required
       inputs = c(a1nodecov,a2nodecov,a3nodecov,v2nodecov, u), minval=0)
}



InitErgmTerm.path2attr2 <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE,
                       varnames = c("a1","a2","a3","v2","a1keep","a2keep","a3keep","v2keep"), # a1 > a2 > a3
                       vartypes = c("character","character","character","character","numeric", "numeric","numeric", "numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL, NULL, NULL,NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))  
  # n <- network.size(nw)
  
  a1nodecov <- get.node.attr(nw, a$a1)
  a2nodecov <- get.node.attr(nw, a$a2)
  a3nodecov <- get.node.attr(nw, a$a3)
  v2nodecov <- get.node.attr(nw, a$v2)
  
  a1u<-sort(unique(a1nodecov))
  a2u<-sort(unique(a2nodecov))
  a3u<-sort(unique(a3nodecov))
  v2u<-sort(unique(v2nodecov))
  
  # if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  a1nodecov <- match(a1nodecov,a1u,nomatch=length(a1u)+1)
  a2nodecov <- match(a2nodecov,a2u,nomatch=length(a2u)+1)
  a3nodecov <- match(a3nodecov,a3u,nomatch=length(a3u)+1)
  v2nodecov <- match(v2nodecov,v2u,nomatch=length(v2u)+1)
  
  u <- c(a$a1keep,a$a2keep, a$a3keep, a$v2keep)
  
  coef.names <- paste0("path2attr2.", a$a1, ":", a1u[a$a1keep], ">", a$a2, ":", a2u[a$a2keep], ">", a$a3, ":", a3u[a$a3keep], "...var:", a$v2, ":", v2u[a$v2keep])
  list(name = "path2attr2", coef.names = coef.names, #name and coef.names: required
       inputs = c(a1nodecov,a2nodecov,a3nodecov,v2nodecov, u), minval=0)
}


# nw <- bnet
# arglist <- list("geolevel2","geolevel",1:10)
# 
# InitErgmTerm.b1twostar <- function(nw, arglist, ...) {
#   ### Check the network and arguments to make sure they are appropriate.
#   a <- check.ErgmTerm (nw, arglist, directed=FALSE, bipartite=TRUE,
#                        varnames = c("b1attrname", "b2attrname", "base"),
#                        vartypes = c("character", "character", "numeric"),
#                        defaultvalues = list(NULL, NULL, NULL),
#                        required = c(TRUE, FALSE, FALSE))
#   ### Process the arguments
#   nb1 <- get.network.attribute(nw, "bipartite")
#   n <- network.size(nw)
#   b1nodecov <- get.node.attr(nw, a$b1attrname)[1:nb1]
#   b1u<-sort(unique(b1nodecov))
#   if(any(is.na(b1nodecov))){ b1u<-c(b1u,NA) }
#   if(is.null(a$b2attrname)) { a$b2attrname <- a$b1attrname }
#   b2nodecov <- get.node.attr(nw, a$b2attrname)[(1+nb1):n]
#   b2u<-sort(unique(b2nodecov))
#   if(any(is.na(b2nodecov))){b2u<-c(b2u,NA)}
#   # Recode to numeric
#   b1nodecov <- match(b1nodecov,b1u,nomatch=length(b1u)+1)
#   b2nodecov <- match(b2nodecov,b2u,nomatch=length(b2u)+1)
#   nr <- length(b1u)
#   nc <- length(b2u)
#   u <- cbind(rep(1:nr, nc*nc), rep(rep(1:nc, each=nr), nc), rep(1:nc, each=nc*nr))
#   u <- u[u[,2] <= u[,3],]  
#   if (any(NVL(a$base,0)!=0)) { u <- u[-a$base,] }
#   coef.names <- paste("b1twostar", a$b1attrname, b1u[u[,1]],  a$b2attrname,
#                       apply(matrix(b2u[u[,2:3]],ncol=2), 1, paste, collapse="."),
#                       sep=".")
#   list(name = "b1twostar", coef.names = coef.names, #name and coef.names: required
#        inputs = c(b1nodecov, b2nodecov, u[,1], u[,2], u[,3]), minval = 0)
# }




InitErgmTerm.gwb1nspnew <- function(nw, arglist, initialfit = FALSE, ...) {
  a <- check.ErgmTerm(nw, arglist, directed = FALSE, bipartite = TRUE, varnames = c("alpha", "fixed", "cutoff"), vartypes = c("numeric", "logical", "numeric"), defaultvalues = list(0, FALSE, 30), required = c(TRUE, TRUE, FALSE))
  alpha <- a$alpha
  fixed <- a$fixed
  cutoff <- a$cutoff
  alpha = alpha[1]
  nb1 <- get.network.attribute(nw, "bipartite")
  #   maxesp <- min(cutoff,nb1) # CHECK THIS ...
  maxesp <- min(cutoff, network.size(nw) - nb1) # CHECK THIS...
  d <- 1:maxesp
  if (!initialfit && !fixed) {
    stop("The gwb1nspnew term is not yet able to handle a ", 
         "non-fixed decay term.", call. = FALSE)
    ld <- length(d)
    if (ld == 0) {
      return(NULL)
    }
    map <- function(x, n, ...) {
      i <- 1:n
      x[1] * exp(x[2]) * (1 - (1 - exp(-x[2]))^i)
    }
    gradient <- function(x, n, ...) {
      i <- 1:n
      a <- 1 - exp(-x[2])
      exp(x[2]) * rbind(1 - a^i, x[1] * (1 - a^i - i * a^(i - 1)))
    }
    if (is.directed(nw)) {
      dname <- "tnsp"
    }
    else {
      dname <- "nsp"
    }
    list(name = dname, coef.names = paste("nsp#", d, sep = ""), 
         inputs = c(d), params = list(gwb1nspnew = NULL, gwb1nspnew.alpha = alpha), 
         map = map, gradient = gradient)
  }
  else {
    if (initialfit && !fixed) 
      coef.names <- "gwb1nspnew"
    else coef.names <- paste("gwb1nspnew.fixed.", alpha, sep = "")
    if (is.directed(nw)) {
      dname <- "gwtnsp"
    }
    else {
      dname <- "gwb1nspnew" # Always alpha and fix
    }
    list(name = dname, coef.names = coef.names, inputs = c(alpha))
  }
}

InitErgmTerm.gwb2nspnew <- function(nw, arglist, initialfit = FALSE, ...) {
  a <- check.ErgmTerm(nw, arglist, directed = FALSE, bipartite = TRUE, varnames = c("alpha", "fixed", "cutoff"), vartypes = c("numeric", "logical", "numeric"), defaultvalues = list(0, FALSE, 30), required = c(TRUE, TRUE, FALSE))
  alpha <- a$alpha
  fixed <- a$fixed
  cutoff <- a$cutoff
  alpha = alpha[1]
  nb1 <- get.network.attribute(nw, "bipartite")
  maxesp <- min(cutoff,nb1)
  #   maxesp <- min(cutoff, network.size(nw) - nb1) # CHECK THIS...
  d <- 1:maxesp
  if (!initialfit && !fixed) {
    stop("The gwb2nspnew term is not yet able to handle a ", 
         "non-fixed decay term.", call. = FALSE)
    ld <- length(d)
    if (ld == 0) {
      return(NULL)
    }
    map <- function(x, n, ...) {
      i <- 1:n
      x[1] * exp(x[2]) * (1 - (1 - exp(-x[2]))^i)
    }
    gradient <- function(x, n, ...) {
      i <- 1:n
      a <- 1 - exp(-x[2])
      exp(x[2]) * rbind(1 - a^i, x[1] * (1 - a^i - i * a^(i - 1)))
    }
    if (is.directed(nw)) {
      dname <- "tnsp"
    }
    else {
      dname <- "nsp"
    }
    list(name = dname, coef.names = paste("nsp#", d, sep = ""), 
         inputs = c(d), params = list(gwb2nspnew = NULL, gwb2nspnew.alpha = alpha), 
         map = map, gradient = gradient)
  }
  else {
    if (initialfit && !fixed) 
      coef.names <- "gwb2nspnew"
    else coef.names <- paste("gwb2nspnew.fixed.", alpha, sep = "")
    if (is.directed(nw)) {
      dname <- "gwtnsp"
    }
    else {
      dname <- "gwb2nspnew" # Always alpha and fix
    }
    list(name = dname, coef.names = coef.names, inputs = c(alpha))
  }
}



InitErgmTerm.dgwdspattr <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=TRUE, 
                       varnames = c("attrname","p1keep","p2keep","alpha"),
                       vartypes = c("character","numeric", "numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  p1u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  nodecov <- match(nodecov,p1u,nomatch=length(p1u)+1)
  
  u <- c(a$p1keep,a$p2keep, a$alpha)
  
  coef.names <- paste0("dgwdspattr.", a$attrname, ".", p1u[a$p1keep], "_", p1u[a$p2keep],".a:",a$alpha)
  list(name = "dgwdspattr", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}

InitErgmTerm.gwdspattr <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, directed=FALSE, 
                       varnames = c("attrname","p1keep","p2keep","alpha"),
                       vartypes = c("character","numeric", "numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  p1u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  nodecov <- match(nodecov,p1u,nomatch=length(p1u)+1)
  
  u <- c(a$p1keep,a$p2keep, a$alpha)
  
  coef.names <- paste0("gwdspattr.", a$attrname, ".", p1u[a$p1keep], "_", p1u[a$p2keep],".a:",a$alpha)
  list(name = "gwdspattr", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}


InitErgmTerm.cycle2pendants <- function(nw, arglist, ...) {
  ### Check the network and arguments to make sure they are appropriate.
  a <- check.ErgmTerm (nw, arglist, #directed=FALSE,
                       varnames = c("attrname","p1keep","p2keep","alpha"),
                       vartypes = c("character","numeric", "numeric","numeric"),
                       defaultvalues = list(NULL, NULL, NULL, NULL),
                       required = c(TRUE, TRUE, TRUE, TRUE))  
  n <- network.size(nw)
  
  nodecov <- get.node.attr(nw, a$attrname)
  p1u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){ p1u<-c(p1u,NA) }
  nodecov <- match(nodecov,p1u,nomatch=length(p1u)+1)
  
  u <- c(a$p1keep,a$p2keep, a$alpha)
  
  coef.names <- paste0("cycle2pendants.", a$attrname, ".", p1u[a$p1keep], "_", p1u[a$p2keep],".a:",a$alpha)
  list(name = "cycle2pendants", coef.names = coef.names, #name and coef.names: required
       inputs = c(nodecov, u), minval=0)
}


InitErgmTerm.mindegree <- function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=FALSE, bipartite=FALSE,
                      varnames = c("mindeg", "by"),
                      vartypes = c("numeric", "character"),
                      required = c(TRUE, FALSE),
                      defaultvalues = list(NULL, NULL))
  if(length(a$mindeg) > 1)
    stop("The argument mindeg to mindegree expected a vector of length ",
         "1, but received a vector of length ",length(a$mindeg))
  if (is.null(a$by)) {
    attrflag <- 0
    nodecov <- NULL
    coef.names <- paste("mindegree", a$mindeg, sep="")
  } else {
    attrflag <- 1
    nodecov <- get.node.attr(nw, a$by)
    coef.names <- paste("mindegree.", a$by, a$mindeg, sep="")
    u <- sort(unique(nodecov))
    nodecov <- match(nodecov,u)
  }
  list(name = "mindegree",
       coef.names = coef.names,
       pkgname = "ergm.userterms",
       inputs = c(attrflag, a$mindeg, nodecov),
       dependence = TRUE,
       emptynwstats = (a$mindeg == 0) * network.size(nw)
  )
}
