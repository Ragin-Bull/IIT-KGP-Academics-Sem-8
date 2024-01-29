# This example is for a simple hypothetical game with a 3x3 board.
# You can adapt it to different games by modifying the `evaluate_board` and `get_possible_moves` functions.

def print_board(board):
    for row in board:
        print(" ".join(row))
    print()

def evaluate_board(board):
    # Evaluate the board for the current player (assuming 'X' is the maximizing player)
    # This is a simple evaluation function for demonstration purposes.
    score = 0
    for row in board:
        for cell in row:
            if cell == 'X':
                score += 1
            elif cell == 'O':
                score -= 1
    return score

def is_game_over(board):
    # Check if the game is over (someone won or it's a tie)
    for row in board:
        if all(cell == 'X' for cell in row) or all(cell == 'O' for cell in row):
            return True

    for col in zip(*board):
        if all(cell == 'X' for cell in col) or all(cell == 'O' for cell in col):
            return True

    if all(board[i][i] == 'X' for i in range(3)) or all(board[i][i] == 'O' for i in range(3)):
        return True

    if all(board[i][2-i] == 'X' for i in range(3)) or all(board[i][2-i] == 'O' for i in range(3)):
        return True

    return all(cell != '.' for row in board for cell in row)

def get_possible_moves(board, player):
    # Get possible moves for the current player
    moves = []
    for i in range(3):
        for j in range(3):
            if board[i][j] == '.':
                new_board = [row[:] for row in board]
                new_board[i][j] = player
                moves.append(new_board)
    return moves

def minimax(board, depth, maximizing_player):
    if depth == 0 or is_game_over(board):
        return evaluate_board(board)

    if maximizing_player:
        max_eval = float('-inf')
        for move in get_possible_moves(board, 'X'):
            eval = minimax(move, depth - 1, False)
            max_eval = max(max_eval, eval)
        return max_eval
    else:
        min_eval = float('inf')
        for move in get_possible_moves(board, 'O'):
            eval = minimax(move, depth - 1, True)
            min_eval = min(min_eval, eval)
        return min_eval

def find_best_move(board):
    best_move = None
    best_eval = float('-inf')

    for move in get_possible_moves(board, 'X'):
        eval = minimax(move, 2, False)  # Adjust the depth as needed
        if eval > best_eval:
            best_eval = eval
            best_move = move

    return best_move

# Example usage:
initial_board = [['.', '.', '.'],
                 ['.', '.', '.'],
                 ['.', '.', '.']]

print("Initial Board:")
print_board(initial_board)

best_move = find_best_move(initial_board)

print("Best Move:")
print_board(best_move)
