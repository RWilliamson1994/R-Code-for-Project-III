fermi = function(n,d=0.1,iter=250,b=1.2,k=.75,p=.01)
{
  #generate initial matrix and simulation history storage
  cur_mat = matrix(0,n,n)
  cur_mat[] = 3*rbinom(n^2,1,d)
  nbd_mat = matrix(1,3,3)
  storage = array(0,c(n,n,(iter+1)))
  props = rep(0,(iter+1))
  storage[,,1] = cur_mat
  
  #generate filters
  for(i in 1:8)
  {
    nam = paste("fil",i,sep="")
    temp_mat=matrix(0,3,3)
    temp_mat[2,2]=1
    assign(nam,temp_mat)
  }
  fil1[1,1]=-1 
  fil2[1,2]=-1 
  fil3[1,3]=-1 
  fil4[2,1]=-1
  fil5[2,3]=-1
  fil6[3,1]=-1
  fil7[3,2]=-1
  fil8[3,3]=-1
  
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
    
    new_mat=3*(cur_mat>1)
    
    #generate random matrix
    rdm_mat = matrix(0,n,n)
    rdm_mat[] = sample(1:8,n^2,TRUE)
    
    #use random matrix to generate differences, and then probabilities
    dif_mat = matrix(0,n,n)
    for(i in 1:8)
    {
      assign(paste("dif",i,sep=""),round(filter2(score_mat,get(paste("fil",i,sep=""))),4))
      dif_mat = dif_mat + ((rdm_mat==i)*get(paste("dif",i,sep="")))
    }
    prob_mat = 1/(1+exp(dif_mat/k))
    
    #use random matrix to generate enemy move matrix
    en_move_mat = matrix(0,n,n)
    for(i in 1:8)
    {
      assign(paste("enemy",i,sep=""),round(filter2(new_mat,get(paste("fil2",i,sep="")))))
      en_move_mat = en_move_mat + ((rdm_mat==i)*get(paste("enemy",i,sep="")))
    }
    
    #generate matrix of who will copy their enemy
    unif_mat = matrix(0,n,n)
    unif_mat[]=runif(n^2,0,1)
    change_mat = prob_mat > unif_mat
    
    #generate new move matrix
    new_mat = new_mat + change_mat*(new_mat==0)*(en_move_mat==3)*2 + change_mat*(new_mat==3)*(en_move_mat==0)*(-2)
    
    #introduce noise
    unif2_mat = matrix(0,n,n)
    unif2_mat[] = runif(n^2,0,1)
    p_mat = matrix(p,n,n)
    change2_mat = p_mat > unif2_mat
    prop = sum(1*cur_mat<2)/(n^2)
    unif3_mat = matrix(0,n,n)
    unif3_mat[] = runif(n^2,0,1)
    coop_mat = change2_mat*(unif3_mat < prop)
    def_mat = change2_mat*(unif3_mat > prop)
    new_mat = new_mat + coop_mat*(new_mat>1)*(-2) + def_mat*(new_mat<2)*2
    
    cur_mat = new_mat
    
    storage[,,r+1] = cur_mat
    print(r)
  }
  
  for(i in 1:(iter+1)) #find proportion of cooperation
  {
    props[i] = sum(1*(storage[,,i]<2))/(n^2)
  }
  
  return(props)
}