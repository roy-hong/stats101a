# paste all of your code for this problem here

display <- function (state) { 
  # displays the current state of the board. [5pts]
  
  display_state <- character(9)
  for (i in 1:length(state)) { # deep copying to replace NAs in state vector
    display_state[i] <- state[i]
    if ( is.na(display_state[i]) ) display_state[i] <- i
  }
  
  
  
  cat("```c\n ",display_state[1],"|",display_state[2],"|",display_state[3],"\n ---+---+--- \n ",display_state[4],"|",display_state[5],"|",display_state[6],"\n ---+---+--- \n ",display_state[7],"|",display_state[8],"|",display_state[9],"\n```\n")
  
}





update <- function (state, who, pos) { 
  # updates the state of the board by putting an x or o (who) in the designated position (pos) [10 pts]
  if (is.na(state[pos]) == TRUE ) {
    state[pos] <- who
    state
  }
  
  else {
    print("Please enter a valid move: " )
    state
  }
  
}






check_winner <- function (state) { 
  game_over <- FALSE
  
  for (i in 1:length(triples)) {
    if ( all ( is.na(state[ triples[[i]] ]) == FALSE ) ) { # don't consider any row or column with NAs 
      if ( all(state[triples[[i]]] == "x",na.rm = TRUE) ) game_over <- TRUE 
      if ( all(state[triples[[i]]] == "o",na.rm = TRUE) ) game_over <- TRUE
    }
  }
  game_over
}












computer_turn <- function (state) {
  
  critical_point <- NA # default spot
  x_count <- 0
  o_count <- 0
  
  
  for(i in 1:9) { # count the number of x's and o's in order to determine the team.
    if( is.na(state[i]) == FALSE && state[i] == "x") x_count <- x_count + 1
    if( is.na(state[i]) == FALSE && state[i] == "o") o_count <- o_count + 1
  }
  
  
  state_copy <- character(9)
  for(i in 1:9) { #deep copy for trials 
    state_copy[i] <- state[i]
  }
  
  
  
  
  if(x_count == o_count) { # x's turn
    
    for(i in 1:9) {
      if( is.na(state_copy[i]) ) { # if the spot is empty, try with state_copy 
        
        state_copy <- update(state_copy,"x",i)
        if (check_winner(state_copy) == TRUE) { # if find the spot to finish the game, return the spot
          critical_point = i
          break;
        }
        state_copy[i] <- NA # restore the copy if taking the spot is not able to terminate the game
      }
    }
    
    if(is.na(critical_point)) {
      for(i in 1:9) { # BLOCKING 
        if( is.na(state_copy[i]) ) { # if the spot is empty, try with state_copy 
          
          state_copy <- update(state_copy,"o",i)
          if (check_winner(state_copy) == TRUE) {
            critical_point = i
            break;
          }
          state_copy[i] <- NA 
        }
      }
      
    }
    
    
  }
  
  
  if(x_count > o_count) { # o's turn
    
    for(i in 1:9) {
      if( is.na(state_copy[i]) ) { # if the spot is empty, try with state_copy 
        
        state_copy <- update(state_copy,"o",i)
        if (check_winner(state_copy) == TRUE) { # if find the spot to finish the game, return the spot
          critical_point = i
          break;
        }
        state_copy[i] <- NA # restore the copy if taking the spot is not able to terminate the game
      }
    }
    
    
    if(is.na(critical_point)) {
      for(i in 1:9) { # BLOCKING 
        if( is.na(state_copy[i]) ) { # if the spot is empty, try with state_copy 
          
          state_copy <- update(state_copy,"x",i)
          if (check_winner(state_copy) == TRUE) {
            critical_point = i
            break;
          }
          state_copy[i] <- NA 
        }
      }
      
    }
  }
  
  
  
  if (is.na(state[5]) && is.na(critical_point)) critical_point <- 5 # 5 is the most preferred spot at the early of game
  while (is.na(critical_point)) { # Alternative case if failed to find a spot to immediately terminate the game. 
    
    random_number <- sample(1:9, size = 1)
    if (is.na(state[random_number])) critical_point <- random_number
    
  } 
  
  critical_point
  
}











play <- function(){
  # determine game conditons: 1 or 2 players. If computer plays, is it player 1 or 2.
  dummy_keyboard_input <- readline("\n Welcome to Tic Tac Toe game! press any key to start the game \n\n")
  players <- c("player1", "player2") # by default, two players play tic-tac-toe
  playmode <- 2
  
  num_of_players <- readline("Enter the number of human players (1 or 2 ): ")
  playmode <- as.integer(num_of_players)
  cat("number of players : ",playmode,"\n")
  
  
  if(playmode == 1) { # gameplay against computer
    computer_play <- readline(prompt="Should the computer play first or second? (1 or 2) ")
    players[as.integer(computer_play)] <- "computer"
    cat("computer's turn : ",computer_play,"\n")
  }
  
  
  
  # initialize game board
  state <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
  
  while( !check_winner(state)  ){
    # x's turn
    # display board
    display(state)
    
    # x chooses where to play. prompt user or computer_turn()
    x_input <- NA
    if(players[1] == "player1") {
      
      n <- readline("Where should x play (1 through 9 ): ")
      x_input <- as.integer(n)
      if(is.na(state[x_input]) == FALSE ) { # IF YOU MAKE MORE THAN ONE MISTAKE, YOUR TURN WILL BE TAKEN AWAY
        print("try again with valid input! \n")
        n <- readline("Where should x play (1 through 9 ): ")
        x_input <- as.integer(n)
      }
      
    } else { # Computer automatically decide where to place 
      x_input <- computer_turn(state)
    }
    
    
    state <- update(state, "x", x_input) # update board
    
    if(check_winner(state)) {
      print("player x has won this game !")
      break; # if x wins - quit loop
    }
    
    # draw condition
    if( all(is.na(state) == FALSE ) ) {
      print("DRAW !")
      break;
    }
    
    # o's turn
    # display board
    display(state)
    
    # o chooses where to play. prompt user or computer_turn()
    o_input <- NA
    if(players[2] == "player2") {
      
      n <- readline("Where should o play (1 through 9 ): ")
      o_input <- as.integer(n)
      if(is.na(state[o_input]) == FALSE ) { # IF YOU MAKE MORE THAN ONE MISTAKE, YOUR TURN WILL BE TAKEN AWAY
        print("try again with valid input! \n")
        n <- readline("Where should o play (1 through 9 ): ")
        o_input <- as.integer(n)
      }
      
    } else { # Computer automatically decide where to place 
      o_input <- computer_turn(state)
    }
    
    
    state <- update(state, "o", o_input) # update board
    
    if(check_winner(state)) {
      print("player o has won this game !")
      break; # if o wins - quit loop
    }
    
  }    
  
  display(state)  
}   # display final board state and who the winner is



