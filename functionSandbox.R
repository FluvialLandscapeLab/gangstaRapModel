reset.lp = function(){
  m <<- new.env(parent = parent.env(globalenv()))
  assign("lpModel", readGangsta.lp("C:\\Users\\mathe\\Documents\\R Projects\\gangsta\\lpFiles\\CONSH_Ox.Hx._CodeSupplementForManuscript.lp"), envir = m)
  source('~/R Projects/gangsta/sandbox.R')
  m$getSolvedValues = getSolvedValues
}
