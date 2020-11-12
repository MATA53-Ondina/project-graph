graph = ["1": {
            "p": 20,
            "v": -10000000,
            "l": -10000000,
            "adjacentes": [1, 3]
        },
        "2": {
            "p": 10,
            "v": -10000000,
            "l": -10000000,
            "adjacentes": [1, 3]
        },
        "3": {
            "p": 15,
            "v": -10000000,
            "l": -10000000,
            "adjacentes": [1, 2]
        }
]

def fco(graph):
    Q = []
    V = ["s": -10000000, "t": -10000000]
    tmp = 0
    l = 1
    for vertex in graph:
        vertex[p] = -1000000000
        V = vertex[p] - grauMinimo()
        Q.push(vertex)

    while (!Q):
        if (vertex.p - grauMinimo() == vMax()):
            s = Q.pop()
        if vertex.p is null:
            L = l
            l += 1
            vertex[v] = vertex.p

    for v in graph:
        for t in v[adjacentes]:
            if v[v] > t[v]:
                tmp = calculaTMP() # min{V(s),p(t)}
                if tmp > t[v]:
                    t[l] = v[l]
                    t[p] = v[p]
                    t[v] = tmp # atualizar a posição de t em Q


def main():
    fco(graph);

if __name__ == "__main__":
        main()
