# -*- coding: utf-8 -*-
"""
Calculate Market Access

TODO: For pandanas snap, make sure in UTM so get meters.
TODO: Convert "cell to node" distance to appropriate travel time
"""

# Setup Project Filepath
import getpass
if getpass.getuser() == 'WB521633': project_file_path = 'C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE/'
if getpass.getuser() == 'robmarty': project_file_path = '/Users/robmarty/Dropbox/World Bank/IEs/Ethiopia IE/'

# Load Packages
import geopy
import geopandas as gpd
import os, sys, time
import pandas as pd
import importlib
import networkx as nx
import osmnx as ox
from shapely.ops import unary_union, linemerge, transform
from shapely.wkt import loads
from shapely.geometry import LineString, MultiLineString, Point
sys.path.append(r'' + project_file_path + 'Code/GOSTNets/GOSTNets')
import GOSTnet as gn
import LoadOSM as losm
import pyproj
from functools import partial
from scipy import spatial
import numpy as np
#import rasterio
#from rasterio.plot import show
import pyreadr
import functools

# Parameters ------------------------------------------------------------------
# CONSTANT_POPULATION: If True, then use 2000 population for all market access
#                      calculateions. If False, then use population of given year
CONSTANT_POPULATION = True

# MA_CALC_FUNCTION: If "by_cell" then loop through cells, if "by_node" then loop
#                   through nodes. (either: by_cell OR by_node)
MA_CALC_FUNCTION = "by_cell"

# WALKING_SPEED: Walking speed in km/hr (GOSTNets Uses 4.5km/hr by default)
WALKING_SPEED = 4.5

# Functions for Calculating Market Access -------------------------------------

# Method 1: Loop through cells
def calc_MA_per_cell(cell_id):

    points_gdp_Gsnap_i = points_gdp_Gsnap[points_gdp_Gsnap.cell_id == cell_id]

    traveltime_all_df_i = traveltime_all_df[traveltime_all_df.index == list(points_gdp_Gsnap_i.NN)[0]]
    
    # km/hr then convert to seconds
    traveltime_list = np.array(list(traveltime_all_df_i.iloc[0]), dtype=np.float) + (list(points_gdp_Gsnap_i.NN_dist)[0]/1000)/WALKING_SPEED*60*60
    MA = sum((market_population / np.array(list(traveltime_list), dtype=np.float)))

    if (cell_id % 1000) == 0:
        print(cell_id)

    return(MA)

# Method 2: Loop through Nodes
def calc_MA_per_node(points_nodes_i):

    # Grab all points that are snapped to that road
    points_gdp_Gsnap_nodei = points_gdp_Gsnap[points_gdp_Gsnap.NN == points_nodes_i]

    # Travel time from that node to all markets
    traveltime_all_df_i = np.array(list(traveltime_all_df[traveltime_all_df.index == points_nodes_i].iloc[0]), dtype=np.float)

    # Travel time from cell to node on graph
    traveltime_cell_node = np.array(list(points_gdp_Gsnap_nodei.NN_dist), dtype=np.float)*2

    def calc_MA_per_node_i(time_to_node, time_to_market, market_pop):
        theta = 1
        MA = sum((market_pop/(time_to_market + time_to_node))**theta)
        return(MA)

    MA_all = list(map(functools.partial(calc_MA_per_node_i, time_to_market=traveltime_all_df_i, market_pop=market_population), traveltime_cell_node))

    points_gdp_Gsnap_nodei['MA'] = MA_all

    return(points_gdp_Gsnap_nodei)

# Load Origin and Destinations ------------------------------------------------
# Origins
points_dict = pyreadr.read_r(os.path.join(project_file_path,'Data','IntermediateData','Outputs for Grid','DMSPOLS', 'points.Rds'))
points_df = points_dict[None]
points_df = pd.DataFrame(points_df)

geometry = [Point(xy) for xy in zip(points_df.long, points_df.lat)]
points_df = points_df.drop(['long', 'lat'], axis=1)
crs = {'init': 'epsg:4326'}
points_gdp = gpd.GeoDataFrame(points_df, crs=crs, geometry=geometry)

# Markets
markets = pd.read_csv(os.path.join(project_file_path,'Data','RawData','citypopulation','geocoded_interpolated', 'city_pop_geocoded.csv'))
geometry = [Point(xy) for xy in zip(markets.lon, markets.lat)]
markets = markets.drop(['lon', 'lat'], axis=1)
crs = {'init': 'epsg:4326'}
markets_gdp = gpd.GeoDataFrame(markets, crs=crs, geometry=geometry)

# Ethiopia Projection
UTM_eth_str = 'epsg:20138'
UTM_eth = {'init': 'epsg:20138'}

# Loop through year -----------------------------------------------------------
for YEAR in ['1996', '1998','2000','2002','2004', '2006', '2008', '2010', '2012', '2014', '2016']:

    # Load Graph
    G = nx.read_gpickle(os.path.join(project_file_path,'Data','RawData','RoadNetworkPanelDataV3_GraphObjects','roads_' + YEAR + '.pickle'))

    # Snap Origin/Destination to Network
    markets_gdp_Gsnap = gn.pandana_snap(G, markets_gdp, add_dist_to_node_col=True, target_crs = UTM_eth_str)
    points_gdp_Gsnap = gn.pandana_snap(G, points_gdp, add_dist_to_node_col=True, target_crs = UTM_eth_str)

    # Dataframe of Travel Time: Markets to all Nodes --------------------------
    nodes = list(markets_gdp_Gsnap.NN)

    # Initialize Dataframe
    node_i = nodes[0]
    traveltime_dict = nx.single_source_dijkstra_path_length(G, node_i, cutoff = None, weight = 'time')
    traveltime_all_df = pd.Series(traveltime_dict).to_frame(node_i)

    # Loop through nodes and merge to dataframe
    for node_i in nodes[1:]:
        print(node_i)

        traveltime_dict = nx.single_source_dijkstra_path_length(G, node_i, cutoff = None, weight = 'time')
        traveltime_df = pd.Series(traveltime_dict).to_frame(node_i)

        traveltime_all_df = traveltime_all_df.merge(traveltime_df, how='outer', left_index=True, right_index=True)

    # Calculate Market Access -------------------------------------------------
    #### Grab relevant variables
    if CONSTANT_POPULATION == True:
        market_population = np.array(list(markets_gdp_Gsnap['pop.2000'][0:]), dtype=np.float)
    else:
        market_population = np.array(list(markets_gdp_Gsnap['pop.'+YEAR][0:]), dtype=np.float)

    points_nodes = list(points_gdp_Gsnap.NN.unique())

    #### Loop through cells
    if MA_CALC_FUNCTION == 'by_cell':
        points_gdp_Gsnap['MA'] = list(map(calc_MA_per_cell, list(points_gdp_Gsnap.cell_id)))

        pyreadr.write_rds(os.path.join(project_file_path,'Data','IntermediateData','Outputs for Grid','DMSPOLS', 'MA_'+YEAR+'_constantpop'+str(CONSTANT_POPULATION)+'.Rds'), points_gdp_Gsnap)

    #### Loop through nodes
    if MA_CALC_FUNCTION == 'by_node':
        counter = 1
        points_MA_all = calc_MA_per_node(points_nodes[0])
        for points_nodes_i in points_nodes[1:]:
            points_MA_all_i = calc_MA_per_node(points_nodes_i)
            points_MA_all = points_MA_all.append(points_MA_all_i)
            counter = counter + 1

            if (counter % 10) == 0:
                print(counter)
                print(points_nodes_i)

        pyreadr.write_rds(os.path.join(project_file_path,'Data','IntermediateData','Outputs for Grid','DMSPOLS', 'MA_'+YEAR+'_constantpop'+str(CONSTANT_POPULATION)+'.Rds'), points_MA_all)
