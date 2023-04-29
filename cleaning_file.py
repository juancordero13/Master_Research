import pandas as pd
import numpy as np
import csv

with open(r"/Users/juancordero/Desktop/My_GitHub/Master_Research/Input/caudal_ebro.txt") as file:
    text_file = file.read()
    text_file = text_file.split("----------------------------------------")[1]
    text_file = text_file.replace("     ", ",").replace("    ", ",").replace("   ", ",").replace("  ", ",").replace(" ", ",").replace(",9011,", "9011,")
    print(text_file)

with open('output.csv', 'w') as output_file:
    output_file.write(text_file)