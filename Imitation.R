im = function(n,d =0.1,iter = 250,b=1.9)
{
  #generate initial matrix and simulation history storage
  cur_mat = matrix(0,n,n)
  cur_mat[] = 3*rbinom(n^2,1,d)
  n = dim(cur_mat)[1]
  nbd_mat = matrix(1,3,3)
  storage = array(0,c(n,n,(iter+1)))
  props = rep(0,(iter+1))
  storage[,,1] = cur_mat
  
  #generate filters
  for(i in 1:8)
  {
    nam = paste("fil2",i,sep="")
    temp_mat=matrix(0,3,3)
    assign(nam,temp_mat)
  }
  fil21[1,1]=1
  fil22[1,2]=1
  fil23[1,3]=1
  fil24[2,1]=1
  fil25[2,3]=1
  fil26[3,1]=1
  fil27[3,2]=1
  fil28[3,3]=1
  
  for (r in 1:iter) #rounds
  {
    #generate score_mat
    score_mat = matrix(0,n,n)
    sum_mat = round(filter2((1*(cur_mat>1)),nbd_mat))
    score_mat = ((((1*(cur_mat>1)))*(9-sum_mat)*b) + ((1-(1*(cur_mat>1)))*(9-sum_mat)))
    
    #find highest score
    for (i in 1:8)
    {
      assign(paste("score",i,sep=""),round(filter2(score_mat,get(paste("fil2",i,sep=""))),5))
    }
    highscore_mat = pmax(score_mat,score1,score2,score3,score4,score5,score6,score7,score8)
    
    #find highest scoring neighbour
    nbr_mat = matrix(0,n,n)
    for(i in 1:8)
    {
      nbr_mat = nbr_mat + (nbr_mat==0)*i*(highscore_mat == get(paste("score",i,sep=""))) 
    }
    
    #generate neighbours move matrix
    for(i in 1:8)
    {
      assign(paste("move",i,sep=""),round(filter2((1*(cur_mat > 1)),get(paste("fil2",i,sep="")))))
    }
    nbr_move_mat = matrix(0,n,n)
    for(i in 1:8)
    {
      nbr_move_mat = nbr_move_mat + (nbr_mat == i)*(get(paste("move",i,sep="")))
    }
    nbr_move_mat = nbr_move_mat + (nbr_mat == 0)*(cur_mat > 1)*1
    
    cur_mat = 3*(cur_mat > 1)
    
    #generate new current matrix
    cur_mat = cur_mat + 2*(cur_mat == 0)*nbr_move_mat - 2*(cur_mat == 3)*(1 - nbr_move_mat)
    
    storage[,,(r+1)] = cur_mat
    print(r)
  }
  for(i in 1:(iter+1)) #find proportion of cooperation
  {
    props[i] = sum(1*(storage[,,i]<2))/(n^2)
  }
  
  return(props)
}