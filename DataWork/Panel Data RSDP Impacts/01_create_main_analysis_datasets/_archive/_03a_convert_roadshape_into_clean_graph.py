# -*- coding: utf-8 -*-
"""
Prepare Road Shapefiles as Network

# 1. Load shapefile and convert to network
# 2. Clean network
# 3. Adds points along network
"""

# TODO Figure out what's happening with G_2006a
# TODO Time not appearing in all edges for some reason. NEED TO DO:
#      Graph to time AFTER everything is cleaned. That should be very last step

# Setup -----------------------------------------------------------------------
# Setup Project Filepath
import getpass
if getpass.getuser() == 'WB521633': project_file_path = 'C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE/'
if getpass.getuser() == 'robmarty': project_file_path = '/Users/robmarty/Dropbox/World Bank/IEs/Ethiopia IE/'

# Load Packages
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

# Define Functions ============================================================

# Shapefile to G --------------------------------------------------------------
def shp_to_G(shape1, speed_var, target_utm, temp_osm_network):
    print(speed_var)

    shape = shape1.copy()

    # Convert to OSM Type
    shape = shape.to_crs({'init':'epsg:4326'})
    shape['osm_id'] = shape.index
    shape[speed_var] = shape[speed_var].apply(int)
    shape['infra_type'] = 'speed_' + shape[speed_var].apply(str)
   # eth = losm.OSM_to_network(os.path.join(project_file_path,'Data','RawData','template_osmfile_for_gostnet', 'tajikistan-latest.osm.pbf'))
    temp_osm_network.roads_raw = shape
    temp_osm_network.generateRoadsGDF(verbose = False)
    temp_osm_network.initialReadIn()
    G = temp_osm_network.network

    # Add length in meters
    source = 'epsg:4326'

    project_WGS_UTM = partial(
                    pyproj.transform,
                    pyproj.Proj(init=source),
                    pyproj.Proj(init=target_utm))
    G2 = G.copy()

    for u, v, data in G2.edges(data = True):

        project_UTM_WGS = partial(
                    pyproj.transform,
                    pyproj.Proj(init=target_utm),
                    pyproj.Proj(init=source))

        UTM_geom = transform(project_WGS_UTM, data['Wkt'])

        data['length'] = UTM_geom.length

    return G2


# Shapefile to G --------------------------------------------------------------
def shp_to_G_time(shape1, speed_var, target_utm, temp_osm_network):
    print(speed_var)

    shape = shape1.copy()

    # Convert to OSM Type
    shape = shape.to_crs({'init':'epsg:4326'})
    shape['osm_id'] = shape.index
    shape[speed_var] = shape[speed_var].apply(int)
    shape['infra_type'] = 'speed_' + shape[speed_var].apply(str)
   # eth = losm.OSM_to_network(os.path.join(project_file_path,'Data','RawData','template_osmfile_for_gostnet', 'tajikistan-latest.osm.pbf'))
    temp_osm_network.roads_raw = shape
    temp_osm_network.generateRoadsGDF(verbose = False)
    temp_osm_network.initialReadIn()
    G = temp_osm_network.network

    # Add length in meters
    source = 'epsg:4326'

    project_WGS_UTM = partial(
                    pyproj.transform,
                    pyproj.Proj(init=source),
                    pyproj.Proj(init=target_utm))
    G2 = G.copy()

    for u, v, data in G2.edges(data = True):

        project_UTM_WGS = partial(
                    pyproj.transform,
                    pyproj.Proj(init=target_utm),
                    pyproj.Proj(init=source))

        UTM_geom = transform(project_WGS_UTM, data['Wkt'])

        data['length'] = UTM_geom.length

    # Create speed dictionary
    s_d = {'speed_10': 10,
           'speed_15': 15,
           'speed_20': 20,
           'speed_25': 25,
           'speed_30': 30,
           'speed_35': 35,
           'speed_45': 45,
           'speed_50': 50,
           'speed_70': 70,
           'speed_120': 120}

    G_time = gn.convert_network_to_time(G2,
                                       distance_tag = 'length',
                                       graph_type = 'drive',
                                       road_col = 'infra_type',
                                       speed_dict = s_d)

    return G_time

# Clean Network ---------------------------------------------------------------
def clean_network(G1, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, output_name):

    G = G1.copy()

    # Squeezes clusters of nodes down to a single node if they are within the snapping tolerance
    G = gn.simplify_junctions(G, UTM_eth, WGS, 50)

    # ensures all streets are two-way
    G = gn.add_missing_reflected_edges(G)

    # Finds and deletes interstital nodes based on node degree
    G = gn.custom_simplify(G)

    # rectify geometry
    for u, v, data in G.edges(data = True):
        if type(data['Wkt']) == list:
                data['Wkt'] = gn.unbundle_geometry(data['Wkt'])

    # For some reason CustomSimplify doesn't return a MultiDiGraph. Fix that here
    G = gn.convert_to_MultiDiGraph(G)

    # This is the most controversial function - removes duplicated edges. This takes care of two-lane but separate highways, BUT
    # destroys internal loops within roads. Can be run with or without this line
    G = gn.remove_duplicate_edges(G)

    # Run this again after removing duplicated edges
    G = gn.custom_simplify(G)

    # Ensure all remaining edges are duplicated (two-way streets)
    G = gn.add_missing_reflected_edges(G)

    # Pulls out largest strongly connected component
    # TODO: Maybe better way to deal with subgraphs?
    G = max(nx.strongly_connected_component_subgraphs(G), key=len)

    # Salt Long Lines
    G = gn.salt_long_lines(G, WGS_str, UTM_eth_str, thresh = 1000, factor = 1)

    # Add time variable
    # Create speed dictionary
    s_d = {'speed_10': 10,
           'speed_15': 15,
           'speed_20': 20,
           'speed_25': 25,
           'speed_30': 30,
           'speed_35': 35,
           'speed_45': 45,
           'speed_50': 50,
           'speed_70': 70,
           'speed_120': 120}

    G_time = gn.convert_network_to_time(G,
                                       distance_tag = 'length',
                                       graph_type = 'drive',
                                       road_col = 'infra_type',
                                       speed_dict = s_d)

    # save final
    gn.save(G_time, output_name, wpath)

    return G_time

# Load Data and Define Objects ------------------------------------------------
# Define UTMs
UTM_eth = {'init': 'epsg:20138'}
UTM_eth_str = 'epsg:20138'
WGS = {'init': 'epsg:4326'}
WGS_str = 'epsg:4326'

# Define folder where graph objects should go
wpath = os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_GraphObjects')

# Load template OSM Object
template_osm_network = losm.OSM_to_network(os.path.join(project_file_path,'Data','RawData','template_osmfile_for_gostnet', 'tajikistan-latest.osm.pbf'))

# Load Roads
roads_2016 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2016.shp'))

roads_1996 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_1996.shp'))
roads_1998 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_1998.shp'))
roads_2000 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2000.shp'))
roads_2002 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2002.shp'))
roads_2004 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2004.shp'))
roads_2006 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2006.shp'))
roads_2008 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2008.shp'))
roads_2010 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2010.shp'))
roads_2012 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2012.shp'))
roads_2014 = gpd.read_file(os.path.join(project_file_path, 'Data', 'RawData', 'RoadNetworkPanelDataV3_1996_2016_Revised', 'All_Network_2014.shp'))

G_1996 = shp_to_G(roads_1996, 'Speed1996', UTM_eth_str, template_osm_network)
G_1998 = shp_to_G(roads_1998, 'Speed1998', UTM_eth_str, template_osm_network)
G_2000 = shp_to_G(roads_2000, 'Speed2000', UTM_eth_str, template_osm_network)
G_2002 = shp_to_G(roads_2002, 'Speed2002', UTM_eth_str, template_osm_network)
G_2004 = shp_to_G(roads_2004, 'Speed2004', UTM_eth_str, template_osm_network)
G_2006 = shp_to_G(roads_2006, 'Speed2006a', UTM_eth_str, template_osm_network)
G_2008 = shp_to_G(roads_2008, 'Speed2008', UTM_eth_str, template_osm_network)
G_2010 = shp_to_G(roads_2010, 'Speed2010', UTM_eth_str, template_osm_network)
G_2012 = shp_to_G(roads_2012, 'Speed2012', UTM_eth_str, template_osm_network)
G_2014 = shp_to_G(roads_2014, 'Speed2014', UTM_eth_str, template_osm_network)
G_2016 = shp_to_G(roads_2016, 'Speed2016', UTM_eth_str, template_osm_network)

G_1996_clean = clean_network(G_1996, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_1996')
G_1998_clean = clean_network(G_1998, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_1998')
G_2000_clean = clean_network(G_2000, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2000')
G_2002_clean = clean_network(G_2002, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2002')
G_2004_clean = clean_network(G_2004, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2004')
G_2006_clean = clean_network(G_2006, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2006')
G_2008_clean = clean_network(G_2008, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2008')
G_2010_clean = clean_network(G_2010, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2010')
G_2012_clean = clean_network(G_2012, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2012')
G_2014_clean = clean_network(G_2014, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2014')
G_2016_clean = clean_network(G_2016, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2016')










# PURGATORY  ----------------------------------------------------------------------


G_1996 = shp_to_G_time(roads_1996, 'Speed1996', UTM_eth_str, template_osm_network)
G_1998 = shp_to_G_time(roads_1998, 'Speed1998', UTM_eth_str, template_osm_network)
G_2000 = shp_to_G_time(roads_2000, 'Speed2000', UTM_eth_str, template_osm_network)
G_2002 = shp_to_G_time(roads_2002, 'Speed2002', UTM_eth_str, template_osm_network)
G_2004 = shp_to_G_time(roads_2004, 'Speed2004', UTM_eth_str, template_osm_network)
G_2006 = shp_to_G_time(roads_2006, 'Speed2006a', UTM_eth_str, template_osm_network)
G_2008 = shp_to_G_time(roads_2008, 'Speed2008', UTM_eth_str, template_osm_network)
G_2010 = shp_to_G_time(roads_2010, 'Speed2010', UTM_eth_str, template_osm_network)
G_2012 = shp_to_G_time(roads_2012, 'Speed2012', UTM_eth_str, template_osm_network)
G_2014 = shp_to_G_time(roads_2014, 'Speed2014', UTM_eth_str, template_osm_network)
G_2016 = shp_to_G_time(roads_2016, 'Speed2016', UTM_eth_str, template_osm_network)

G_1996_clean = clean_network(G_1996, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_1996')
G_1998_clean = clean_network(G_1998, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_1998')
G_2000_clean = clean_network(G_2000, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2000')
G_2002_clean = clean_network(G_2002, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2002')
G_2004_clean = clean_network(G_2004, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2004')
G_2006_clean = clean_network(G_2006, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2006')
G_2008_clean = clean_network(G_2008, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2008')
G_2010_clean = clean_network(G_2010, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2010')
G_2012_clean = clean_network(G_2012, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2012')
G_2014_clean = clean_network(G_2014, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2014')
G_2016_clean = clean_network(G_2016, UTM_eth, UTM_eth_str, WGS, WGS_str, wpath, 'roads_2016')

# PURGATORY ===================================================================
# Checking things here
G_1996_clean.number_of_edges()
G_1996_clean.number_of_nodes()
a = [len(c) for c in sorted(nx.strongly_connected_components(G_1996_clean), key=len, reverse=True)]
b = nx.strongly_connected_components(G_1996_clean)
# To calculate travel time:
# 1. pandana_snap (markets -- should be close to road)
# 2. pandana_snap (destinations/markets -- should be close to road)
# 3. destinations come from raster object, where


results_dict = nx.single_source_dijkstra_path_length(G_1996_clean, 'new_obj_3230', cutoff = None, weight = 'time')

G_1996_clean.number_of_edges()
G_1996_clean.number_of_nodes()

Gc.number_of_edges()
Gc.number_of_nodes()
