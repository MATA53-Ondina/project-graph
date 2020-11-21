#!/usr/bin/python3
# imports
import sys
import math

graph = {1: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 0,
            "v": 0,
            "adj": [(2, 12), (3, 5), (4, 10)]
        },
        2: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": [(5, 4), (6, 5)]
        },
        3: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": [(9, 3), (10, 2)]
        },
        4: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": [(7, 4), (8, 5)]
        },
        5: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": []
        },
        6: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": []
        },
        7: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": []
        },
        8: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": []
        },
        9: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": []
        },
        10: {
            "d": [],
            "l": 0,
            "p": 0,
            "peso": 10,
            "v": -1,
            "adj": []
        },
    }

# mapa de valores de Caminhos ótimos do grafo 
V = []
P = {} #floresta de caminhos ótimos

# camputa menos pesso dos arcos.
def p(v):
    p = sys.maxsize
    for x in graph:
        if x is v:
            for (no, peso)in v["adj"]:
                if peso < p:
                    p = peso
    return p

# detecta caminhos mínimos
def caminhosMinimos(s):
    k = 0
    for v in graph:
        for (no, peso) in v["adj"]:
            if k == peso != s:
                k = peso - s
            else:
                if peso > s:
                    k = s
                elif peso < s:
                    k = peso
    return k


def caminhosOtimos():
    k = 0
    for s in graph:
        if s == 1 and s["v"] == -1:
            s["v"] = 0
        for (no, peso) in s["adj"]:
            graph[no]["v"] = s["p"] + peso
            graph[no]["d"].append((s,s["d"]+1))
    for s in graph:
        V.append((s["v"],s))
    V.sort(reverse=True)
    return k

# função para cálculo do peso desde a raiz até o nó
# calcula os menores caminhos baseado nos pesos das arestas
# Isso aqui da o retorno do vetor V.
def bfs():
    return True

# escolhe o maior valor de dentro do vetor V.
def vMax ():
    return V.pop(0)

# distância de s à t
def d(s, t):
    dist = graph[t]["d"]
    k = 0
    for (pai, distance) in dist:
        if pai == s:
            k = distance
    return distance

def DF():
    k = 0
    for v in graph:
        for (no, peso) in v["adj"]:
            if peso > k:
                k = peso
    return k

def ro (s, t):
    # Comprimento do maior arco
    df = DF()
    ps = (1 / (math.sqrt(2*math.pi*((df/3)**2)) * t["adj"].length)) * (-d(s,t)/2*((df/3)**2))
    return ps

# min{V(s),p(t)}
def calculaTMP(s, t):
    p = 0
    n = 0
    for (peso, no) in V:
        if no == s:
            n = no
            p = peso
    return min(ro(s,t),p)

# Floresta de Caminhos Ótimos
def fco(graph):
    Q = []
    tmp = 0
    l = 1
    gamma = 1.0
    for vertex in graph:
        vertex["p"] = -1
        V.append(vertex["p"] - gamma)
        Q.append(vertex)

    while (not Q):
        no = vMax()
        s = Q.pop(no[1])
        if s not in P:
            graph[s]["l"] = l
            l += 1
            for (k, w) in V:
                if w == s:
                    k = ro(s)

    for v in graph:
        for (t, peso) in v["adj"]:
            if v["v"] > graph[t]["v"]:
                tmp = calculaTMP(v, t) # min{V(s),p(t)}
                if tmp > graph[t]["v"]:
                    graph[t]["l"] = v["l"]
                    graph[t]["p"] = ro(v, t)
                    graph[t]["v"] = tmp # atualizar a posição de t em Q


def main():
    fco(graph)

if __name__ == "__main__":
        main()