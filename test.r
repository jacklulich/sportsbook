#Library of Betting Functions

#Converts American Odds to Decimals Odds
to_decimal_odds = function(american_odds){
  decimal_odds = 1
  if(american_odds >= 0){
    decimal_odds = 1 + (american_odds/100)
  }
  else{
    decimal_odds = 1 - (100/american_odds) 
    return(decimal_odds)
  }
  return(decimal_odds)
}

#Converts Decimals Odds to American Odds
to_american_odds = function(decimal_odds){
  american_odds = -100
  if(decimal_odds >= 2){
    american_odds = (decimal_odds-1)*100
  }
  else{
    american_odds = -100/(decimal_odds-1)
  }
  return (american_odds)
}

#Gives payout on bet given initial stake and odds
payout = function(stake, odds){
  converted_odds = odds
  if(abs(odds) >= 50){
    converted_odds = to_decimal_odds(odds) 
  }
  return (converted_odds*stake)
}

#Gives breakeven win percentage, assumes line is decimal
implied_probability = function(line){
  return (1/line)
}

#Calculates expected holds on a series of lines
expected_hold = function(lines){
  sum_bep = 0
  for (value in lines){
      sum_bep = sum_bep + implied_probability(value)
  }
  return((1-(1/sum_bep))*100)
}

#Given a line, returns odds w/out vig. i/o decimal
remove_vig = function(lines){
  breakeven = implied_probability(lines)
  total_vig = sum(breakeven)
  true_percentages = breakeven/total_vig
  vig_free = 1/true_percentages
  return (vig_free)
}