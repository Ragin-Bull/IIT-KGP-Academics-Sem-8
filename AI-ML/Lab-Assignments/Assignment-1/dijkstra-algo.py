import heapq
import sys

def dijkstra(graph, src):
    n = len(graph)
    dist = [sys.maxsize for x in range(n)]
    dist[src]=0
    visited = [False for x in range(n)]
    
    pq = [(0, src)]
    while len(pq) > 0:
        currD, u = heapq.heappop(pq)
        
        if visited[u]:
            continue
        
        visited[u]=True

        for v, wt in graph[u]:
            if dist[u] + wt < dist[v]:
                dist[v] = dist[u] + wt
                heapq.heappush(pq, (dist[v], v))

