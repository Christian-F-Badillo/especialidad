# Python 3.11
# author: Christian Badillo
# Date: 2024-25-10

# Importamos las librerias necesarias
import pandas as pd
import os
import re

def merge_data(data_path:str, outdir:str, name:str) -> None:
    """
    Une los datos de las noticias en un solo archivo.
    """

    # Obtenemos los archivos
    files = os.listdir(data_path)
    re_expression = r"player_positions_data_batch_\d+.csv" # Expresión regular para leer los archivos
    files_toread = re.compile(re_expression) # Buscamos los archivos que coincidan con la expresión regular

    # cambiamos el directorio
    os.chdir(data_path)

    # Leemos los archivos
    data = pd.concat([pd.read_csv(file) for file in files if files_toread.match(file)])
    data = data[["player", "positions"]]
    # Guardamos los datos
    data.to_csv(f"{outdir}/{name}.csv", index=False)


dir_data = "/home/quicho/Escritorio/especialidad/multivariate/proyecto/data"
out_dir = "/home/quicho/Escritorio/especialidad/multivariate/proyecto"
name = "data_player_position"

merge_data(data_path=dir_data, outdir=out_dir, name=name)