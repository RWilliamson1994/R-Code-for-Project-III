mcc = function(n=149,mcc=0.8,mcccoop=0.9, coop = 0.5, p = 0.01)
{
  #generate simulation history storage 
  iter = 250
  ini_mat = matrix(0,n,n)
  c = (1 - mcc)*coop
  d = (1 - mcc)*(1 - coop)
  nbhd_mat = matrix(1,3,3)
  nbhd_mat[2,2] = 0
  storage = array(0,c(n,n,iter+1))
  props = rep(0,(iter+1))
  
  #generate the initial matrix
  unif_mat = matrix(0,n,n)
  unif2_mat = matrix(0,n,n)
  unif_mat[] = runif(n^2,0,1)
  unif2_mat[] = runif(n^2,0,1)
  mcc_mat = unif_mat > (c+d)
  cur_mat = ini_mat + 3*(unif_mat <= d) + 3*(unif2_mat > mcccoop)*mcc_mat
  storage[,,1] = cur_mat
  
  for(r in 1:iter) #rounds
  {
    #generate matrix of number of cooperative neighbours
    cur_mat = 3*(cur_mat > 1)
    coop_mat = round(filter2(1*(cur_mat==0),nbhd_mat))
    
    #generate probability of changing strategy
    prob_mat = (0.6 - coop_mat*(0.55/8))*(cur_mat==0) + (0.3 - coop_mat*(.25/8))*(cur_mat==3)
    
    #generate matrix of mcc players who are changing strategy
    unif3_mat = matrix(0,n,n)
    unif3_mat[] = runif(n^2,0,1)
    change_mat = (prob_mat > unif3_mat)*mcc_mat
    
    #update mcc players' strategy
    cur_mat = cur_mat + 2*change_mat*(cur_mat==0) - 2*change_mat*(cur_mat==3)
    
    #introduce noise
    cur_mat = 3*(cur_mat > 1)
    coop_prop = sum((1-(cur_mat/3))*mcc_mat)/sum(1*mcc_mat)
    unif4_mat = matrix(0,n,n)
    unif4_mat[] = runif(n^2,0,1)
    prob2_mat = matrix(1,n,n)
    prob2_mat = prob2_mat - (unif4_mat*mcc_mat)
    change2_mat = prob2_mat < p
    unif5_mat = matrix(0,n,n)
    unif5_mat[] = runif(n^2,0,1)
    new_coop_mat = coop_prop > unif5_mat 
    new_def_mat = 1 - new_coop_mat
    cur_mat = cur_mat + 2*change2_mat*new_def_mat*(cur_mat==0) - 2*change2_mat*new_coop_mat*(cur_mat==3)
    print(r)
    storage[,,r+1] = cur_mat
  }
  
  for(i in 1:(iter+1)) #find proportion of cooperation
  {
    props[i] = sum(1*(storage[,,i]<2))/(n^2)
  }
  
  return(props)
}