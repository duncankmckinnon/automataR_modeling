generation <- function(ancestors, size, generation.rule)
{
  generation.entity <- vector(length = length(ancestors))
  generation.entity_ind <- 1
  ancestor_start_ind <- 0
  ancestor_end_ind <- ancestor_start_ind
  ancestor_loop_ind <- 1 
  while(ancestor_start_ind < length(ancestors))
  {
    if(ancestor_start_ind < floor(size/2))
    {
      generation.vals <- c(ancestors[(length(ancestors) - floor(size/2) + (ancestor_start_ind)):length(ancestors)], ancestors[1:(floor(size/2) + ancestor_start_ind)])
    }
    else
    {
      ancestor_end_ind <- ancestor_start_ind + size - 1
      if(ancestor_end_ind > length(ancestors))
      {
        generation.vals <- c(ancestors[ancestor_start_ind:length(ancestors)], ancestors[1:ancestor_loop_ind])
        ancestor_loop_ind <- ancestor_loop_ind + 1
      }
      else if(ancestor_start_ind < ceiling(length(ancestors)))
      {
        generation.vals <- ancestors[ancestor_start_ind:ancestor_end_ind]
      }
      else
      {
        return(generation.entity)
      }
    }
    x <- as.integer(generation.rule(generation.vals))
    generation.entity[generation.entity_ind] <- x
    ancestor_start_ind <- ancestor_start_ind + 1
    generation.entity_ind <- generation.entity_ind + 1
  }
  return(generation.entity)
}



cellular_automata <- function(rule_num = 1, CA.cross_size = 11, CA.num_gen = floor(CA.cross_size/2), CA.e_size = 3, show = F)
{
  CA.gen <- vector(mode = "integer", length = CA.cross_size)
  if(CA.cross_size %% 2 == 1)
  {
    CA.gen[floor(CA.cross_size/2) + 1] <- 1
  }
  else
  {
    CA.gen[CA.cross_size/2] <- 1
  }
  CA.store <- c(CA.gen)
  CA.rule <- automata_rule(rule_num, size = CA.e_size)
  for(CA.ind in 1:CA.num_gen)
  {
    CA.gen <- generation(CA.gen, size = CA.e_size, generation.rule = CA.rule)
    CA.store <- c(CA.gen, CA.store)
  }
  ca <- t(matrix(CA.store, ncol = (CA.num_gen+1), nrow = (CA.cross_size)))
  if(show)
  {
    image(1 - t(ca), col = grey(seq(0, 1, length = 256)))
  }
  return(ca)
}

automata_rule <- function(automata_rule.rule_num, size = 3) {
  n <- 2^size
  automata_rule.num_vec <- 0:(n-1)
  automata_rule.binary <- vector(mode = "integer", length = n)
  automata_rule.I2B <- int2binary(automata_rule.rule_num)
  automata_rule.binary[1:length(automata_rule.I2B)] <- automata_rule.I2B
  automata_rule.rule <- function(val)
  {
    return(automata_rule.binary[(automata_rule.num_vec[binary2int(val)+1]+1)])
  }
  return(automata_rule.rule)
}

binary2int <- function(binaryRay)
{
  B2I.int <- 0
  for(B2I.ind in 1:length(binaryRay))
  {
    B2I.int <- B2I.int + binaryRay[B2I.ind] * 2^(B2I.ind-1)
  }
  return(B2I.int)
}

int2binary <- function(int)
{
  I2B.binary <- c()
  if(int < 1)
  {
    return(0)
  }
  while(int >= 2)
  {
    I2B.binary <- c(I2B.binary, int %% 2)
    int <- floor(int/2)
  }
  return(c(I2B.binary, int))
}
