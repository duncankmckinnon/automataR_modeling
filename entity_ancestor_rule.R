
entity <- function(ancestors, rule, size = 1)
{
  return(rule(ancestors, size))
}

run_gen <- function(ancestors, size, run_gen.rule)
{
  run_gen.entity <- vector(length = length(ancestors))
  run_gen.entity_ind <- 1
  ancestor_start_ind <- 0
  ancestor_end_ind <- ancestor_start_ind
  ancestor_loop_ind <- 1 
  while(ancestor_start_ind < length(ancestors))
  {
    if(ancestor_start_ind < floor(size/2))
    {
      run_gen.vals <- c(ancestors[(length(ancestors) - floor(size/2) + (ancestor_start_ind)):length(ancestors)], ancestors[1:(floor(size/2) + ancestor_start_ind)])
    }
    else
    {
      ancestor_end_ind <- ancestor_start_ind + size - 1
      if(ancestor_end_ind > length(ancestors))
      {
        run_gen.vals <- c(ancestors[ancestor_start_ind:length(ancestors)], ancestors[1:ancestor_loop_ind])
        ancestor_loop_ind <- ancestor_loop_ind + 1
      }
      else if(ancestor_start_ind < ceiling(length(ancestors)))
      {
        run_gen.vals <- ancestors[ancestor_start_ind:ancestor_end_ind]
      }
      else
      {
        return(run_gen.entity)
      }
    }
    x <- as.integer(run_gen.rule(run_gen.vals))
    run_gen.entity[run_gen.entity_ind] <- x
    ancestor_start_ind <- ancestor_start_ind + 1
    run_gen.entity_ind <- run_gen.entity_ind + 1
  }
  return(run_gen.entity)
}

cellular_automata <- function(rule_num = 1, CA.cross_size = 11, CA.num_gen = 20)
{
  CA.gen <- vector(mode = "integer", length = CA.cross_size)
  if(CA.cross_size %% 2 == 1)
  {
    CA.gen[CA.cross_size/2 + 1] <- 1
  }
  else
  {
    CA.gen[CA.cross_size/2] <- 1
  }
  CA.store <- c(CA.gen)
  CA.rule <- automata_rule(rule_num)
  for(CA.ind in 1:CA.num_gen)
  {
    CA.gen <- run_gen(CA.gen, size = 3, run_gen.rule = CA.rule)
    CA.store <- c(CA.store, CA.gen)
  }
  return(t(matrix(CA.store, ncol = (CA.num_gen+1), nrow = (CA.cross_size))))
}

automata_rule <- function(automata_rule.rule_num) {
  automata_rule.num_vec <- 0:7
  automata_rule.binary <- vector(mode = "integer", length = 8)
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
