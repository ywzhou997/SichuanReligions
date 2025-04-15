"""
projectMap_byType.py

This script processes an Excel file containing data on religious sites and splits the data 
into multiple Excel files based on the unique values in the 'type_faith_simp' column. 
Each resulting file is saved in the 'byType' subdirectory with an auto-generated FID column 
for ArcGIS use.
This script also Fills missing "End Year" values in the "Timeline_Data_ArcGIS" sheet with 
the current year and saves the result, and save it to a different file.

Author: Yuwei Zhou
Date: April 14 2025

Dependencies:
    - argparse
    - os
    - pandas
    - datetime

Usage:
    python projectMap_byType.py --file_name SichuanReligion_Dataset_Map&Timeline_July2024
"""


import argparse
import os
import pandas as pd
from datetime import datetime


# Process input argument
parser = argparse.ArgumentParser(description="Input a file name under /SichuanReligion/")
parser.add_argument("--file_name", required=True, help = "File name under /SichuanReligion/")
params = parser.parse_args()

# Input file
file_dir = '/Users/zhouyuwei/Desktop/SichuanReligion/'
file_path = os.path.join(file_dir, params.file_name)

# Output file
output_dir = os.path.join(file_dir, 'byType/')

# Open workbook
try:
    df = pd.read_excel(file_path, sheet_name='Map_Data')
    faith_type = df["type_faith_simp"].unique()
    now = datetime.now()
    now_year = now.strftime("%Y")+"-01-01"
    end_year = []

    for faith in faith_type:
        temp = df.loc[df["type_faith_simp"] == faith].copy()
        temp['FID(for ArcGIS)'] = range(1, len(temp)+1)
        output_name = faith.replace("/", "_")+".xlsx"
        output_path = os.path.join(output_dir, output_name)
        temp.to_excel(output_path, engine="openpyxl", index = False)
        print("successfully written into", output_name)

    # Save the timeline data for ArcGIS in a seperate file
    tl_df = pd.read_excel(file_path, sheet_name='Timeline_Data_ArcGIS')
    for index, row in tl_df.iterrows():
        if pd.isnull(row['End Year']):
            end_year.append(now_year)
        else:
            end_year.append(row['End Year']) 
    tl_df["End Year"] = end_year
    tl_output_file_name = params.file_name[:-5]+"_TimelineforArcGIS.xlsx"
    tl_output_path = os.path.join(file_dir, tl_output_file_name)
    tl_df.to_excel(tl_output_path, engine="openpyxl", index = False)
    print("successfully written into", tl_output_path)


except FileNotFoundError:
    print("Error: File not found. Please check the path and try again.")





