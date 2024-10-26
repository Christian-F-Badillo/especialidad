# Python 3.11
# author: Christian Badillo
# Date: 2024-25-10

# Importamos las librerias necesarias
import asyncio
from playwright.async_api import async_playwright, Playwright
import pandas as pd
import numpy as np
import sys
from typing import List, Dict

# Links
links = ["https://www.fifaindex.com/es-mx/players/"]
pattern_link = [f"https://www.fifaindex.com/es-mx/players/?page={i}" for i in range(2, 601)]
links.extend(pattern_link)

# Se define la función.
async def scraping(playwright: Playwright, links:List[str]) -> Dict[str, List[str]]:
    """
    Obtiene los datos de las posiciones de los jugadores.
    """
    # Se crean metadatos falsos para el Agente.
    user_agent_strings = [
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36", "Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko"]

    data_scraped = {
        "player": [],
        "positions": []
    }
    
    # Se hace el scraping de los datos de los jugadores.
    for link in links:
        # Abre el navegador
        browser = await playwright.chromium.launch(headless=True)

        # Se reemplazan los metadatos al azar en cada iteeración
        context = await browser.new_context(
            user_agent=np.random.choice(user_agent_strings)
        )

        # Creamos una nueva pestaña
        page = await context.new_page()
        # Establcemos el tiempo limite para que el sitio web se cargue.
        timeout = 3600000 # En milisegundos.
        
        # Se indica el link al cual se va a navegar.
        await page.goto(link, timeout=timeout)

        # Se localiza la clase "link-player" que contiene el nombre del jugador
        elementos = page.locator('a.link-player[href]')
        # Se extrae el texto.
        player_name = await elementos.all_text_contents()

        # Se exporta
        data_scraped["player"].extend(player_name)    
        
        # Selector para cada fila de la tabla
        filas = page.locator("table tr")

        # En esta clase del HTML se tiene el texto de las posiciones de los jugadores.
        clase_enlace = "link-position"

        positions = [] # Se inicia una lista vacia para almacenar las posiciones.
        count = await filas.count() # Se cuenta el número de filas de la tabla.

        # Se itera sobre las filas de la tabla HTML.
        for i in range(count):
            # Selecciona todos los href que contienen las posiciones de los jugadores en su inner text
            href_position = filas.nth(i).locator(f"a.{clase_enlace}")
            row_text = [] # Para almacenar los datos de las posiciones de la fila i-th.
            
            # Itera sobre cada link en la fila y extrae su texto
            link_count = await href_position.count() # Cuenta el número de posiciones.

            # Itera sobre el número de posiciones para obtener si texto.
            for j in range(link_count):
                text = await href_position.nth(j).inner_text() # Se obtiene la i-th posición del jugador j.
                row_text.append(text) #Se almacena la posición

            # Almacena todas las posibles posiciones del jugador j.
            positions.append(row_text)

        data_scraped["positions"].extend(positions)

        # Se cierra el navegador.
        await browser.close()
    
    return data_scraped


# Definimos una función para realizar el scraping por lotes
async def scraping_by_batch(playwright: Playwright, links:List[str], batch_num:int=5) -> None:

    """
    Realiza el scraping dividiendo las tareas en lotes.
    """
    size: int = len(links) # Número de Links
    batch_size: int = size // batch_num # Tamaño de los lotes
    lista_links = [links[i:i+batch_size] for i in range(0, size, batch_size)] # Dividimos los links en lotes

    # Guardamos los datos por batch
    for i, batch in enumerate(lista_links):

        sys.stdout.write(f"Starting batch {i} \n") # Imprimimos el progreso

        data = await scraping(playwright, batch) # Realizamos el scraping

        df = clean_data(data=data)
        
        df.to_csv(f"~/Escritorio/especialidad/multivariate/proyecto/data/player_positions_data_batch_{i}.csv") # Guardamos los datos

        sys.stdout.write(f"Batch {i} completed \n") # Imprimimos el progreso
        sys.stdout.write("-"*50)


def clean_data(data:Dict[str, List[str]]=None) -> pd.DataFrame:
    
    clean_data = {
        "player" : [],
        'positions': []
    }

    for key in data.keys():
        for value in data[key]:
            if value:
                clean_data[key].append(value)
            else:
                continue
    
    return pd.DataFrame(clean_data)


# Inicializamos el loop de asyncio
async def main() -> None:
    async with async_playwright() as playwright:
        batch_size = 100

        # Realizamos el scraping por lotes
        await scraping_by_batch(playwright, links, batch_num=batch_size)


# Ejecutamos el código
asyncio.run(main())
