def floyd_warshall(graph):
    n = len(graph)
    for k in range(n):
        for i in range(n):
            for j in range(n):
                graph[i][j] = min(graph[i][j], graph[i][k] + graph[k][j]) 

    return graph


graph = [[0,1,43], [1,0,6], [-1,-1,0]]
print(floyd_warshall(graph))
