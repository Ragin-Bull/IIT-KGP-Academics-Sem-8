import random
import math

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

def simulated_annealing_nqueens(n, initial_temperature=1000, cooling_rate=0.95, max_iterations=1000):
    current_board = initialize_board(n)
    current_evaluation = evaluate(current_board)
    best_board = current_board
    best_evaluation = current_evaluation
    temperatures = [initial_temperature]

    for iteration in range(1, max_iterations + 1):
        temperature = initial_temperature * (cooling_rate ** iteration)
        temperatures.append(temperature)

        neighbors = get_neighbors(current_board)
        if not neighbors:
            break

        next_board = random.choice(neighbors)
        next_evaluation = evaluate(next_board)

        delta_evaluation = next_evaluation - current_evaluation

        if delta_evaluation < 0 or random.random() < math.exp(-delta_evaluation / temperature):
            current_board = next_board
            current_evaluation = next_evaluation

            if current_evaluation < best_evaluation:
                best_board = current_board
                best_evaluation = current_evaluation

    return best_board, best_evaluation, temperatures

# Example with N = 8
n = 8
solution, evaluations, temperatures = simulated_annealing_nqueens(n)

print("Final solution:")
print_board(solution)
print("Final evaluation:", evaluations)

# Plotting the temperature changes during the annealing process
import matplotlib.pyplot as plt

plt.plot(temperatures, marker='o')
plt.title('Simulated Annealing - Temperature Schedule')
plt.xlabel('Iteration')
plt.ylabel('Temperature')
plt.show()
