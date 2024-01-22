import heapq


def a_star_algorithm(start, goal):
    pq = [(0, start)]
    parent = {}
    visited = set()
    parent[start] = None
    cummulativeCost = {start: 0}

    while len(pq) > 0:
        currentCost, currentNode = heapq.heappop(pq)

        if currentNode == goal:
            path = []
            curr = goal
            while curr is not None:
                path.append(curr)
                curr = parent[curr]
            path.reverse()
            return path

        if currentNode in visited:
            continue

        visited.add(currentNode)

        for nhbr, weight in graph[currentNode]:
            updatedCost = cummulativeCost[currentNode] + weight

            if nhbr not in cummulativeCost or updatedCost < cummulativeCost[nhbr]:
                cummulativeCost[nhbr] = updatedCost
                totalCost = heuristic[nhbr] + updatedCost
                heapq.heappush(pq, (totalCost, nhbr))
                parent[nhbr] = currentNode

    return "Failed to generate path"


heuristic = [5, 15, 2, 8, 0]
graph = [[(1, 5), (2, 3)], [(3, 7)], [(4, 2)], [(4, 8)], []]

start = 0
goal = 4

print(a_star_algorithm(start, goal))
