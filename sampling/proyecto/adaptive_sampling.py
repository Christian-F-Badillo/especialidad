import networkx as nx
import matplotlib.pyplot as plt
import random
import numpy as np
from matplotlib.colors import Normalize
from matplotlib.cm import ScalarMappable
import matplotlib.cm as cm
import pandas as pd


# 1. Simulación de datos
def generar_vecindades(municipios, max_vecinos, seed = 1):
    np.random.seed(seed)
    for id_mun in municipios:
        num_vecinos = np.random.randint(1, max_vecinos)
        municipios[id_mun]["vecinos"] = random.sample(
            [m for m in municipios if m != id_mun], 
            num_vecinos
        )
    return municipios

def simular_datos(num_municipios, max_poblacion, max_vecinos, seed = 1):
    municipios = {}
    np.random.seed(seed)
    for i in range(1, num_municipios + 1):
        municipios[i] = {
            "poblacion": np.random.choice(range(4000, max_poblacion), 1),
            "casos": np.random.binomial(n = max_poblacion/1000, p = 0.05) * 25,
            "vecinos": []
        }
    new_municipios = generar_vecindades(municipios, max_vecinos, seed)
    return new_municipios


# 2. Muestreo adaptativo
def muestreo_adaptativo(municipios, tamano_inicial, C_min, seed = 1):

    np.random.seed(seed)
    mas = np.random.choice(list(municipios.keys()), tamano_inicial, replace=False)
    muestra = mas.tolist()
    evaluados = set()
    recorrido = []  # Para guardar el orden del recorrido

    while True:
        nuevos_municipios = []
        for mun_id in muestra:
            if mun_id not in evaluados:
                evaluados.add(mun_id)
                recorrido.append(mun_id)
                if municipios[mun_id]["casos"] >= C_min:
                    vecinos = municipios[mun_id]["vecinos"]
                    for vecino_id in vecinos:
                        if vecino_id not in muestra and vecino_id not in nuevos_municipios:
                            nuevos_municipios.append(vecino_id)
                            
        muestra.extend(nuevos_municipios)

        if not nuevos_municipios:
            break

    return muestra, recorrido

# 3. Graficar con contornos y aristas personalizadas
def graficar_muestra(municipios, muestra, recorrido):
    G = nx.Graph()

    # Crear nodos y aristas
    for mun_id, data in municipios.items():
        G.add_node(mun_id, casos=data["casos"])
        for vecino in data["vecinos"]:
            G.add_edge(mun_id, vecino)

    # Obtener la intensidad del color basada en los casos
    casos_maximos = max([municipios[n]["casos"] for n in G.nodes()])
    normalizador = Normalize(vmin=0, vmax=casos_maximos)  # Normalizar valores de casos
    cmap = cm.get_cmap('Purples')  # Colormap para intensidad del color

    # Crear lista de colores para nodos
    colores = [cmap(normalizador(municipios[n]["casos"])) for n in G.nodes()]
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
    nx.draw_networkx_nodes(G, pos, node_color=colores, edgecolors=bordes, node_size=500, alpha=0.9, linewidths=node_width)
    nx.draw_networkx_edges(G, pos, edge_color=edge_colors, alpha=0.6, width=widths)
    nx.draw_networkx_labels(G, pos, font_size=8, font_color="black")

    # Crear el mappable para la barra de color
    sm = ScalarMappable(norm=normalizador, cmap=cmap)
    sm.set_array([])
    cbar = plt.colorbar(sm, ax=plt.gca(), shrink=0.8)
    cbar.set_label("Número de casos", fontsize=12)

    # Agregar título
    plt.title("Muestra Seleccionada", fontsize=14)
    plt.axis('off')
    plt.show()

def crear_dataframe_subredes(nodos):
    """
    Crea un DataFrame que describe las subredes conectadas de los nodos.

    Parámetros:
        nodos (list[dict]): Lista de nodos donde cada nodo es un diccionario
                            con las llaves 'población', 'casos' y 'vecinos'.

    Retorna:
        pd.DataFrame: DataFrame con las columnas:
                      - 'Red': ID de la subred.
                      - 'Número de Nodos': Número total de nodos en la subred.
                      - 'Lista de Nodos': Lista de nodos en la subred.
                      - 'Total de Casos': Suma total de casos en la subred.
                      - 'Suma de Población': Suma de las poblaciones en la subred.
    """
    # Crear el grafo
    G = nx.Graph()
    
    # Agregar nodos y conexiones
    for i, nodo in enumerate(nodos):
        G.add_node(i, pop=nodo['poblacion'], cases=nodo['casos'])
    for vecino in nodo['vecinos']:
        G.add_edge(i, vecino)
    
    # Identificar componentes conexas
    componentes = list(nx.connected_components(G))
    
    # Crear una lista para almacenar la información de las subredes
    datos_redes = []
    for idx, componente in enumerate(componentes):
        # Obtener nodos de la subred
        lista_nodos = list(componente)
        
        # Calcular métricas
        suma_casos = sum([G.nodes[int(node)]['cases'] for node in lista_nodos])
        suma_poblacion = sum([G.nodes[node]['pop'] for node in lista_nodos])
        
        # Agregar información de la subred
        datos_redes.append({
            'Red': idx + 1,
            'Número de Nodos': len(lista_nodos),
            'Lista de Nodos': lista_nodos,
            'Total de Casos': suma_casos,
            'Suma de Población': suma_poblacion
        })
    
    # Crear el DataFrame
    df_redes = pd.DataFrame(datos_redes)
    
    return df_redes

# Ejecución
NMUNICIPIOS = 75
SEMILLA = 19111973
NINICIAL = 10
CRITERIO = 30
MAXPOP = 20000
MAXVECINOS = 4

datos_municipios = simular_datos(NMUNICIPIOS, MAXPOP, MAXVECINOS, SEMILLA)
muestra, recorrido = muestreo_adaptativo(datos_municipios, NINICIAL, CRITERIO, SEMILLA)

print(f"Muestra: {muestra},\nn = {len(muestra)}")
#graficar_muestra(datos_municipios, muestra, recorrido)

# Ejemplo de uso
nodos = [datos_municipios.get(i) for i in muestra]

df_resultado = crear_dataframe_subredes(nodos)
print(df_resultado)
