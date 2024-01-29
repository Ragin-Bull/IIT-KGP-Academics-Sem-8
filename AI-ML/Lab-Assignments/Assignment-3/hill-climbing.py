import random

def hill_climbing(initial_state, get_neighbors, evaluate):
    current_state = initial_state

    while True:
        neighbors = get_neighbors(current_state)
        if not neighbors:
            break  # If there are no neighbors, we've reached a local minimum

        neighbor_evaluations = [(neighbor, evaluate(neighbor)) for neighbor in neighbors]
        best_neighbor, best_evaluation = min(neighbor_evaluations, key=lambda x: x[1])

        if best_evaluation >= evaluate(current_state):
            break  # If the best neighbor is not better, we've reached a local minimum

        current_state = best_neighbor

    return current_state

# Example usage:
def get_neighbors(state):
    # Generate neighboring states by making small changes to the current state
    neighbors = []
    for i in range(len(state)):
        neighbor = list(state)
        neighbor[i] = random.choice([0, 1])  # Assuming binary state representation
        neighbors.append(tuple(neighbor))
    return neighbors

def evaluate(state):
    # Evaluate the fitness of the state
    # You need to define your own evaluation function based on the specific problem
    # Lower values are better for minimization problems
    return sum(state)

# Example with a random initial state
initial_state = tuple(random.choice([0, 1]) for _ in range(10))
result = hill_climbing(initial_state, get_neighbors, evaluate)

print("Initial state:", initial_state)
print("Final state:", result)
print("Final evaluation:", evaluate(result))
