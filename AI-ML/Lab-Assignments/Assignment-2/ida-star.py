import sys
inf = sys.maxsize

def ida_star(start, goal):
    threshold = heuristic(start, goal)

    while True:
        visited = set()
        res, currentCost = DLS(start, goal, threshold, visited)

        if res == "Success":
            return currentCost  # Goal has been reached

        if res == inf:
            return "Goal is not reachable"  # Goal is not reachable

        threshold = res


def DLS(current, goal, threshold, visited):
    currentCost = heuristic(current, goal)

    if currentCost > threshold:
        return currentCost, 0

    if current == goal:
        return "Success", 0

    minCost = inf

    for nhbr, weight in graph[current]:
        if nhbr not in visited:
            visited.add(nhbr)
            res, pathCost = DLS(nhbr, goal, threshold, visited)

            if res == "Success":
                return "Success", pathCost + weight

            if res != inf and pathCost + weight < minCost:
                minCost = pathCost + weight


def heuristic(current, goal):
    heuristics = [5, 15, 2, 8, 0]
    return heuristics[current]


graph = [[(1, 5), (2, 3)], [(3, 7)], [(4, 2)], [(4, 8)], []]

print(ida_star(0, 4))
