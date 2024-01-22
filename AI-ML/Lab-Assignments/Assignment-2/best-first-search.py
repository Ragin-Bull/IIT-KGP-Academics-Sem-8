# Write the Best First Search ALgorithm
def Best_First_Search(src, goal):
    OPEN = []
    CLOSED = []
    OPEN.append(src)
    parent = {}
    parent[src] = None

    while len(OPEN) > 0:
        currentValue = heuristics[OPEN[0]]
        currentNode = OPEN[0]

        for i in range(1, len(OPEN)):
            if heuristics[OPEN[i]] < currentValue:
                currentNode = OPEN[i]
                currentValue = heuristics[OPEN[i]]

        OPEN.remove(currentNode)
        CLOSED.append(currentNode)

        if currentNode == goal:
            path = []
            curr = currentNode

            while curr is not None:
                path.append(curr)
                curr = parent[curr]

            path.reverse()
            return path

        for node in graph[currentNode]:
            for x in OPEN:
                if x == node:
                    continue

            for x in CLOSED:
                if x == node:
                    continue

            OPEN.append(node)
            parent[node] = currentNode

    return "Failed to generate a path!"


# Define the graph that we'll be using: static input
graph = [[1, 2, 3], [4, 5], [6], [7, 8], [], [], [], [9], [], []]
heuristics = [20, 22, 21, 10, 25, 24, 30, 5, 12, 0]

print(Best_First_Search(0, 9))
