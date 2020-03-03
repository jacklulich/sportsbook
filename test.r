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
break_wp = function(line){
  return (1/line)
}

#Calculates expected holds on a series of lines
expected_hold = function(lines){
  sum_bep = 0
  for (value in lines){
      sum_bep = sum_bep + break_wp(value)
  }
  return((1-(1/sum_bep))*100)
}

s = c(to_decimal_odds(205),to_decimal_odds(-250))
p = expected_hold(s)
p
