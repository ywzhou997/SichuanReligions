"""
nunnery_byType.py

This script processes an Excel file containing data on religious sites and splits the data 
into multiple Excel files based on the unique values in the 'Site_type' column. 
Each resulting file is saved in the 'byType' subdirectory with an auto-generated FID column 
for ArcGIS use.

Author: Yuwei Zhou
Date: April 14 2025
License: MIT

Dependencies:
    - argparse
    - os
    - pandas

Usage:
    python nunnery_byType.py --file_name Nuns_nunneries_202501.xlsx
"""


# need to pip install pandas, openpyxl
import argparse
import os
import pandas as pd


# Process input argument
parser = argparse.ArgumentParser(description="Input a file name under /SichuanReligion/Stefania")
parser.add_argument("--file_name", required=True, help = "File name under /SichuanReligion/Stefania")
params = parser.parse_args()

# Input file
file_dir = '/Users/zhouyuwei/Desktop/SichuanReligion/Stefania/'
file_path = os.path.join(file_dir, params.file_name)

# Output file
output_dir = os.path.join(file_dir, 'byType/')

# Open workbook
try:
    df = pd.read_excel(file_path)
    site_type = df["Site_type"].unique()

    for site in site_type:
        temp = df.loc[df["Site_type"] == site].copy()
        temp['FID(for ArcGIS)'] = range(1, len(temp)+1)
        output_name = site.replace(" ", "_")+".xlsx"
        output_path = os.path.join(output_dir, output_name)
        temp.to_excel(output_path, engine="openpyxl", index = False)
        print("successfully written into", output_name)

except FileNotFoundError:
    print("Error: File not found. Please check the path and try again.")





