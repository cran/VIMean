# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# hello <- function() {
#   print("Hello, world!")
# }

VIM <- function(data,id,repeat.vars){

  #compute row average
  avg <- apply(X=as.matrix(data[,repeat.vars]),MARGIN=1,FUN=function(x){mean(x,na.rm=T)})

  #compute row standard deviation
  sntd <- apply(X=as.matrix(data[,repeat.vars]),MARGIN=1,FUN=function(x){sd(x,na.rm=T)})

  #tune parameters for VIM
  nls.vim <- nls(sntd ~ k*avg^p, start=c(k=1,p=1))
  nls.vim.s <- summary(nls.vim)
  coef <- nls.vim.s$coefficients

  # VIM = k*(sntd/avg)^p
  vim <- coef["k","Estimate"]*(sntd/avg)^coef["p","Estimate"]

  r2.fit <- lm(predict(nls.vim)~sntd)
  rf.fit.s <- summary(r2.fit)
  r.squared <- rf.fit.s$r.squared
  adj.r.squared <- rf.fit.s$adj.r.squared

  return(list(vim=vim,coef=coef,r.squared=r.squared,adj.r.squared=adj.r.squared))
}

