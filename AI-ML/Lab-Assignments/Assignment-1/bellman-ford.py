import sys

def bellman_ford (n, edges, src):
    dist = [sys.maxsize for _ in range(n)]
    dist[src] = 0

    for i in range(n):
        for u, v, wt in edges:
            getDistance = dist[u] + wt
            if getDistance < dist[v]:
                dist[v] = getDistance

    for u, v, wt in edges:
        if dist[u] + wt < dist[v]:
            return "Negative cycle exists in the graph"
    
    return dist



 
edges = [(0,1,5), (1, 2, 1), (1, 4, 2), (2, 4, 1), (4,3,-1), (3, 5, 2), (5,4,-3)]
n = 6
src = 0

print(bellman_ford(n, edges, src))
