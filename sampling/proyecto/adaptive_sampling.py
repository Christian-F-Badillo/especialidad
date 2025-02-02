import networkx as nx
import matplotlib.pyplot as plt
import random
import numpy as np
from matplotlib.colors import Normalize
from matplotlib.cm import ScalarMappable
import matplotlib
import pandas as pd
matplotlib.rcParams['figure.figsize'] = 8, 10

def generar_vecindades(familia, max_vecinos, seed=1):
    np.random.seed(seed)
    random.seed(seed)  # Configurar la semilla para random
    for id_mun in familia:
        num_vecinos = np.random.binomial(n=max_vecinos, p = 0.65, size=1).item()
        
        vecinos = random.sample([m for m in familia if m != id_mun], num_vecinos)
        familia[id_mun]["vecinos"] = vecinos

        # Aseguramos que las conexiones sean bidireccionales
        for vecino in vecinos:
            if id_mun not in familia[vecino]["vecinos"]:
                familia[vecino]["vecinos"].append(id_mun)

    return familia


def simular_datos(num_familias, max_miembros, max_vecinos, seed=1):
    familia = {}
    np.random.seed(seed)
    random.seed(seed)  # Configurar la semilla para random
    for i in range(1, num_familias + 1):
        cases = np.random.poisson(0.35)
        familia[i] = {
            "miembros": np.random.choice(range(2, max_miembros), 1).item(),
            "vecinos": []
        }

        familia[i]["casos"] = cases if cases <= familia[i]["miembros"] else familia[i]["miembros"]

    new_familia = generar_vecindades(familia, max_vecinos, seed)
    return new_familia


def muestreo_adaptativo(familia, tamano_inicial, C_min, seed=1):
    np.random.seed(seed)
    random.seed(seed)  # Configurar la semilla para random
    mas = np.random.choice(list(familia.keys()), tamano_inicial, replace=False)
    muestra = mas.tolist()
    evaluados = set()
    recorrido = []  # Para guardar el orden del recorrido

    while True:
        nuevos_familia = []
        for mun_id in muestra:
            if mun_id not in evaluados:
                evaluados.add(mun_id)
                recorrido.append(mun_id)
                if familia[mun_id]["casos"] >= C_min:
                    vecinos = familia[mun_id]["vecinos"]
                    for vecino_id in vecinos:
                        if vecino_id not in muestra and vecino_id not in nuevos_familia:
                            nuevos_familia.append(vecino_id)

        muestra.extend(nuevos_familia)

        if not nuevos_familia:
            break

    return muestra, recorrido

# 3. Graficar con contornos y aristas personalizadas
def graficar_muestra(familia, muestra, recorrido):

    G = nx.Graph()

    # Crear nodos y aristas
    for mun_id, data in familia.items():
        G.add_node(mun_id, casos=data["casos"])
        for vecino in data["vecinos"]:
            G.add_edge(mun_id, vecino)

    # Obtener la intensidad del color basada en los casos
    casos_maximos = max([familia[n]["casos"] for n in G.nodes()])
    normalizador = Normalize(vmin=0, vmax=casos_maximos)  # Normalizar valores de casos

    # Crear lista de colores para nodos
    colores = [normalizador(familia[n]["casos"]) for n in G.nodes()]
    bordes = ['black' if n not in muestra else 'red' for n in G.nodes()]  # Contorno azul para seleccionados
    node_width = [1 if n not in muestra else 3 for n in G.nodes()]

    # Identificar aristas conectadas a nodos seleccionados
    edge_colors = []
    widths = []
    for u, v in G.edges():
        if u in muestra and v in muestra:
            edge_colors.append('green')  # Aristas entre seleccionados
            widths.append(3)
        else:
            edge_colors.append('gray')  # Aristas no seleccionadas
            widths.append(1)

    # Graficar el grafo
    pos = nx.spring_layout(G, seed=42)  # Layout del grafo
    plt.figure(figsize=(10, 8))
    plt.set_cmap("Purples")
    nx.draw_networkx_nodes(G, pos, node_color=colores, edgecolors=bordes, node_size=500, alpha=0.9, linewidths=node_width)
    nx.draw_networkx_edges(G, pos, edge_color=edge_colors, alpha=0.6, width=widths)
    nx.draw_networkx_labels(G, pos, font_size=8, font_color="black")

    # Crear el mappable para la barra de color
    sm = ScalarMappable(norm=normalizador)
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=plt.gca(), shrink=0.8)
    cbar.set_label("Número de casos", fontsize=12)

    # Agregar título
    plt.title("", fontsize=1)
    plt.axis('off')
    plt.tight_layout()
    plt.savefig("img/ejem4_disease.jpg", bbox_inches='tight', dpi=400)
    plt.show()

def get_conections(nodos, id):
    # Crear el grafo
    G = nx.Graph()

    for mun_id, data in nodos.items():
        G.add_node(mun_id)
        for vecino in data["vecinos"]:
            G.add_edge(mun_id, vecino)

    # Identificar componentes conexas
    vecinos_distancia_1_todos = {nodo: list(G.neighbors(nodo)) for nodo in G.nodes()}
    vecinos_distancia_1_todos = {k:v for k,v in vecinos_distancia_1_todos.items() if k in id}


    return vecinos_distancia_1_todos

def bfs(nodo, visitados, diccionario):
    subred = []
    cola = [nodo]
    while cola:
        actual = cola.pop(0)
        if actual not in visitados:
            visitados.add(actual)
            subred.append(actual)
            cola.extend(diccionario.get(actual, []))
    return subred

def generar_dataframe_subredes(diccionario):

    visitados = set()
    subredes = []
    nodos_llave_por_subred = []

    for nodo in diccionario:
        if nodo not in visitados:
            subred = bfs(nodo, visitados, diccionario)
            subredes.append(subred)
            # Nodos que son llaves dentro de esta subred
            nodos_llave = [n for n in subred if n in diccionario]
            nodos_llave_por_subred.append(nodos_llave)

    # Crear el DataFrame
    data = {
        "SubRed": list(range(0, len(subredes))),
        "#Nodos": [len(subred) for subred in subredes],
        "Nodos": [", ".join(map(str, sorted(subred))) for subred in subredes],
        "Nodos en Muestra": [", ".join(map(str, sorted(llaves))) for llaves in nodos_llave_por_subred],  # Nodos que son llaves
    }
    df = pd.DataFrame(data)
    return df

# Ejecución
Nfamilia = 100
SEMILLA = 423204
NINICIAL = 10
CRITERIO = 1
MAXPOP = 12
MAXVECINOS = 4

datos_familia = simular_datos(Nfamilia, MAXPOP, MAXVECINOS, SEMILLA)
muestra, recorrido = muestreo_adaptativo(datos_familia, NINICIAL, CRITERIO, SEMILLA)

print(f"Muestra: {muestra},\nn = {len(muestra)}")

muestra_nodos = {mun:datos_familia[mun] for mun in muestra}

connections = get_conections(datos_familia, muestra)

# Subredes
df_subredes = generar_dataframe_subredes(connections)
print(df_subredes.head())

#graficar_muestra(datos_familia, muestra, recorrido)

def get_data_muestra(df:pd.DataFrame= None, data:dict=None)-> pd.DataFrame:

    fam_id = df["Nodos"]

    disease_cases = []

    for i in fam_id:

        nodos = [int(j) for j in i.split(",")]

        data_cases = [data[j]["casos"] for j in nodos]

        disease_cases.append(data_cases)

    valid_nodes = []
    for i in disease_cases:
        valids = 0
        for j in i:
            if j > 0:
                valids += 1
        valid_nodes.append(valids if valids != 0 else 1)

    w_i = [sum(i)/j for i, j in zip(disease_cases, valid_nodes)]

    df.drop(["Nodos", "#Nodos", "Nodos en Muestra", "Nodos"], axis=1, inplace=True)
    df["#Nodos Validos"] = valid_nodes
    df["Casos"] = [np.sum(i) for i in disease_cases]
    df["$w_i$"] = w_i

    return df

df = get_data_muestra(df_subredes, datos_familia)
print(df)

df.to_csv("DatosEjemplo4.csv", index=False)

casos = 0
for i in datos_familia.keys():
    casos += datos_familia[i]["casos"]

print(casos)