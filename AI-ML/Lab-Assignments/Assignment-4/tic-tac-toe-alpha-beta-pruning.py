#This function is used to draw the board's current state every time the user turn arrives. 
def ConstBoard(board):
    print("Current State Of Board : \n\n");
    for i in range (0,9):
        if((i>0) and (i%3)==0):
            print("\n");
        if(board[i]==0):
            print("- ",end=" ");
        if (board[i]==1):
            print("O ",end=" ");
        if(board[i]==-1):    
            print("X ",end=" ");
    print("\n\n");

#This function takes the user move as input and make the required changes on the board.
def User1Turn(board):
    pos=input("Enter X's position from [1...9]: ");
    pos=int(pos);
    if(board[pos-1]!=0):
        print("Wrong Move!!!");
        exit(0) ;
    board[pos-1]=-1;

def minimax(board, player, alpha, beta):
    x = analyzeboard(board)
    if x != 0:
        return x * player

    pos = -1
    value = -2

    for i in range(0, 9):
        if board[i] == 0:
            board[i] = player
            score = -minimax(board, (player * -1), -beta, -alpha)
            board[i] = 0

            if score > value:
                value = score
                pos = i

            alpha = max(alpha, value)

            if alpha >= beta:
                break

    if pos == -1:
        return 0

    return value

# This function makes the computer's move using alpha-beta pruning
def CompTurn(board):
    pos = -1
    value = -2
    alpha = float('-inf')
    beta = float('inf')

    for i in range(0, 9):
        if board[i] == 0:
            board[i] = 1
            score = -minimax(board, -1, -beta, -alpha)
            board[i] = 0

            if score > value:
                value = score
                pos = i

            alpha = max(alpha, value)

            if alpha >= beta:
                break

    board[pos] = 1

def analyzeboard(board):
    cb=[[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]];

    for i in range(0,8):
        if(board[cb[i][0]] != 0 and
           board[cb[i][0]] == board[cb[i][1]] and
           board[cb[i][0]] == board[cb[i][2]]):
            return board[cb[i][2]];
    return 0;

#Main Function.
def main():
    board=[0,0,0,0,0,0,0,0,0];
    #The broad is considered in the form of a single dimentional array.
    #One player moves 1 and other move -1.
    print("Computer : O Vs. You : X");
    player= input("Enter to play 1(st) or 2(nd) :");
    player = int(player);
    for i in range (0,9):
        if(analyzeboard(board)!=0):
            break;
        if((i+player)%2==0):
            CompTurn(board);
        else:
            ConstBoard(board);
            User1Turn(board);
         

    x=analyzeboard(board);
    if(x==0):
         ConstBoard(board);
         print("Draw!!!")
    if(x==-1):
         ConstBoard(board);
         print("X Wins!!!")
    if(x==1):
         ConstBoard(board);
         print("O Wins !!!!")
       
#---------------#
main()
#---------------#