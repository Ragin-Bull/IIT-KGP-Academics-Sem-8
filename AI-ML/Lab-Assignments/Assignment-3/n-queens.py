import random
import matplotlib.pyplot as plt

def initialize_board(n):
    return [random.randint(0, n-1) for _ in range(n)]

def get_neighbors(board):
    neighbors = []
    for i in range(len(board)):
        for j in range(len(board)):
            if j != board[i]:
                neighbor = list(board)
                neighbor[i] = j
                neighbors.append(tuple(neighbor))
    return neighbors

def evaluate(board):
    conflicts = 0
    for i in range(len(board)):
        for j in range(i + 1, len(board)):
            if board[i] == board[j] or abs(board[i] - board[j]) == abs(i - j):
                conflicts += 1
    return conflicts

def print_board(board):
    for i in range(len(board)):
        row = ["Q" if j == board[i] else "." for j in range(len(board))]
        print(" ".join(row))
    print()

def hill_climbing_nqueens(n, max_iterations=1000):
    current_board = initialize_board(n)
    evaluations = [evaluate(current_board)]

    for _ in range(max_iterations):
        neighbors = get_neighbors(current_board)
        if not neighbors:
            break

        neighbor_evaluations = [(neighbor, evaluate(neighbor)) for neighbor in neighbors]
        best_neighbor, best_evaluation = min(neighbor_evaluations, key=lambda x: x[1])

        if best_evaluation >= evaluate(current_board):
            break

        current_board = best_neighbor
        evaluations.append(best_evaluation)

    return current_board, evaluations

def plot_evaluations(evaluations):
    plt.plot(evaluations, marker='o')
    plt.title('Hill Climbing Algorithm - N-Queens Problem')
    plt.xlabel('Iteration')
    plt.ylabel('Number of Conflicts')
    plt.show()

# Example with N = 8
n = 8
solution, evaluations = hill_climbing_nqueens(n)

print("Final solution:")
print_board(solution)

plot_evaluations(evaluations)
