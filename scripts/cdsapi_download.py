#!/usr/bin/env python3
# coding: utf-8
#
# -------------- Script Overview -------------------
#
# This script is for downloading ERA5 data from Copernicus
#
#   Author: Lauren Grimley, lauren.grimley@unc.edu
#   Last edited by: LEG 2/6/24
#
# Inputs:
#   1) required - the range of years to download the data for
#   2) required - bounding box
#   3) optional - variable names of interest (default is wind u-v, precip, temperature)
#   4) required - output directory
#
# Outputs:
#   netcdf files of era5 hourly data
#
# Dependencies:
#   Install the CDS API: https://cds.climate.copernicus.eu/api-how-to

import cdsapi
import os

# Edit: years, bounding box, working directory
year = list(range(2010, 2021))
bbox = [38, -86, 31, -72, ]
out_dir = r'Z:\users\lelise\data\meteo\era5'

''' Run the script '''
# Loop through the years and download era5 using cdsapi.Client
for i in range(len(year)):
    file = 'era5_' + str(year[i]) + '.nc'
    fileout = os.path.join(out_dir, file)
    c = cdsapi.Client()
    c.retrieve(
        'reanalysis-era5-land',
        {
            'format': 'netcdf',
            # Update variables that you want to pull
            'variable': [
                '10m_u_component_of_wind',
                '10m_v_component_of_wind',
                'total_precipitation',
                '2m_temperature',
                #'surface_runoff',
            ],
            'year': [str(year[i]),
                     ],
            'month': [
                '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12',
            ],
            'day': [
                '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12',
                '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24',
                '25', '26', '27', '28', '29', '30', '31',
            ],
            'time': ['00:00', '01:00', '02:00', '03:00', '04:00', '05:00',
                     '06:00', '07:00', '08:00', '09:00', '10:00', '11:00',
                     '12:00', '13:00', '14:00', '15:00', '16:00', '17:00',
                     '18:00', '19:00', '20:00', '21:00', '22:00', '23:00',
                     ],
            'area': bbox,
        },
        fileout)
    print('Completed writing', file)
