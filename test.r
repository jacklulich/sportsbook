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

payout = function(stake, odds){
  converted_odds = odds
  if(abs(r) >= 50){
    converted_odds = to_decimal_odds(odds) 
  }
  return (converted_odds*stake)
}