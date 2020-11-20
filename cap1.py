#!/usr/bin/python3
# -*- coding: utf-8 -*-
# imports
import sys

graph = [{1: {
            "p": 3,
            "peso": 3,
            "v": -1,
            "adj": [(2, 12), (3, 5), (4, 10)]
        },
        2: {
            "p": 10,
            "peso": 10,
            "v": -1,
            "adj": [(5, 4), (6, 5)]
        },
        3: {
            "p": 10,
            "peso": 10,
            "v": -1,
            "adj": [(9, 3), (10, 2)]
        },
        5: {
            "p": 10,
            "peso": 10,
            "v": -1,
            "adj": [(7, 4), (8, 5)]
        },
}]

V = []

# camputa menos pesso dos arcos.
def p(v):
    p = sys.maxsize
    for x in graph:
        if x is v:
            for (no, peso)in v["adj"]:
                if peso < p:
                    p = peso
    return p

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

# função para cálculo do peso desde a raiz até o nó
# calcula os menores caminhos baseado nos pesos das arestas
# Isso aqui da o retorno do vetor V.
def bfs():
    return True

# escolhe o maior valor de dentro co vetor V.
def vMax ():
    return 0

# min{V(s),p(t)}
def calculaTMP():
    return 0

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
        if (vertex.p - gamma == vMax()):
            s = Q.pop()
        if vertex.p is null:
            L = l
            l += 1
            vertex[v] = vertex.p

    for v in graph:
        for t in v["adj"]:
            if v["v"] > t["v"]:
                tmp = calculaTMP() # min{V(s),p(t)}
                if tmp > t[v]:
                    t[l] = v[l]
                    t[p] = v[p]
                    t[v] = tmp # atualizar a posição de t em Q


def dijkstra(graph, distancia, predecessor):

    Q = []

    for v in grapf:
        distancia[v] = float('inf') #defini a variavel com um valor muito grande
        predecessor[v] = -1

    for vertex in graph: #Passando todos os vertices do grafo para Q
        Q.push(vertex)

    while len(Q) > 0:
        u = extraiMinimo(distancia,Q)

        for t in v["adj"]:
            if (distancia[t] > distancia[u] + graph['peso']):
                distancia[t] = distancia[u] + graph['peso']
                predecessor[t] = u

    return distancia, predecessor

def main():
    fco(graph)

    distancia = []
    predecessor = []
    distancia, predecessor = dijkstra(graph, distancia, predecessor)

if __name__ == "__main__":
        main()
