#!/home/ldelisle/.conda/envs/omero/bin/python
import omero
from omero.gateway import BlitzGateway
from omero.cmd import Delete2
import argparse
import sys
import pandas as pd
from collections import OrderedDict
import copy
import numpy as np
import os

# These pieces are from https://raw.githubusercontent.com/mpievolbio-scicomp/obat/master/03-KeyVal_from_csv.py
"""
 MIF/Add_Key_Val_from_csv.py

 Adds key-value (kv) metadata to images in a dataset from a csv file
 The first column contains the filenames
 The first  row of the file contains the keys
 The rest is the values for each file/key

-----------------------------------------------------------------------------
  Copyright (C) 2018
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
------------------------------------------------------------------------------
@author Christian Evenhuis
<a href="mailto:christian.evenhuis@gmail.com">christian.evenhuis@gmail.com</a>
@version 5.3
@since 5.3

"""

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
def remove_MapAnnotations(conn, dtype, Id ):
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    image = conn.getObject(dtype,int(Id))
    namespace = omero.constants.metadata.NSCLIENTMAPANNOTATION

    filename = image.getName()

    anns = list( image.listAnnotations())
    mapann_ids = [ann.id for ann in anns
         if isinstance(ann, omero.gateway.MapAnnotationWrapper) ]

    try:
        delete = Delete2(targetObjects={'MapAnnotation': mapann_ids})
        handle = conn.c.sf.submit(delete)
        conn.c.waitOnCmd(handle, loops=10, ms=500, failonerror=True,
                     failontimeout=False, closehandle=False)

    except Exception as ex:
        print("Failed to delete links: {}".format(ex.message))
    return
# End

# I noticed a bug: when the delete fails
# AttributeError: 'CmdError' object has no attribute 'message'

def readCSV(file, sep):
    keyvalsdf = pd.read_csv(file, sep=sep, dtype=str)
    if not 'id' in keyvalsdf:
        raise Exception("id much be part of the colnames")
    return(keyvalsdf)


def set_all_key_values(df, username, password, host, port, replace=False):
    columns_to_ignore = ["id", "image.name", "dataset.name", "dataset.id", "project.name", "user.omename"]
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        new_keys = []
        deleted_keys = []
        for i in df.index:
            my_dict = df.loc[i].to_dict()
            my_image = conn.getObject("Image", int(my_dict['id']))
            existing_kv = {}
            for ann in my_image.listAnnotations():
                if( isinstance(ann, omero.gateway.MapAnnotationWrapper) ):
                    kvs = ann.getValue()
                    for k,v in kvs:
                        # OMERO allow to have multiple values for the same key.
                        # Here only the first value is kept.
                        if not (k in existing_kv):
                            existing_kv[k] = v
            if replace:
                kv = {}
                changed = True
            else:
                kv = existing_kv.copy()
                changed = False
            msg_adding = ""
            msg_changing = ""
            new_keys_image = []
            complement_kv = {}
            for key in my_dict:
                if key in columns_to_ignore:
                    continue
                if isinstance(my_dict[key], float) and np.isnan(my_dict[key]):
                    continue
                if key in kv and my_dict[key] != kv[key]:
                    msg_changing += f"Changing {kv[key]} to {my_dict[key]} for {key} for {my_image.getName()}"
                    msg_changing += "\n"
                    changed = True
                if key not in existing_kv:
                    if key not in new_keys:
                        msg_adding += f"Adding {key}."
                        msg_adding += "\n"
                        new_keys_image.append(key)
                    complement_kv[key] = my_dict[key]
                    changed = True
                kv[key] = my_dict[key]
            if changed:
                dict_to_add = None
                msgs_to_print = msg_adding
                try:
                    remove_MapAnnotations(conn, 'Image', int(my_dict['id']))
                except Exception as e:
                    if msg_changing != "":
                        print("Could not delete the current annotation.")
                        print("Desired changes that failed is:")
                        print(msg_changing)
                    dict_to_add = complement_kv
                else:
                    dict_to_add = kv
                    msgs_to_print += msg_changing
                    new_missing_keys = [k for k in existing_kv if (not k in kv and not k in deleted_keys)]
                    if len(new_missing_keys) > 0:
                        for key in new_missing_keys:
                            msgs_to_print += f"Deleting {key}."
                            msgs_to_print += "\n"
                            deleted_keys.append(key)
                if len(dict_to_add) > 0:
                    map_ann = omero.gateway.MapAnnotationWrapper(conn)
                    namespace = omero.constants.metadata.NSCLIENTMAPANNOTATION
                    map_ann.setNs(namespace)
                    # convert the dict to a list of lists
                    kv_list=[]
                    for k, v in dict_to_add.items():
                        kv_list.append( [k,v] )
                    map_ann.setValue(kv_list)
                    map_ann.save()
                    my_image.linkAnnotation(map_ann)
                    if msgs_to_print != "":
                        print(msgs_to_print)
                    for new_k in new_keys_image:
                        new_keys.append(new_k)

argp = argparse.ArgumentParser(
    description=("Update all key values from a csv file."))
argp.add_argument("-s", "--server", help="OMERO server hostname")
argp.add_argument("-p", "--port", help="OMERO server port")
argp.add_argument("-u", "--user", help="OMERO username")
argp.add_argument("-w", "--password", help="OMERO password or file with password in first line")
argp.add_argument("-f", "--file", help="CSV with key values with header."
                  " One row per image of the dataset."
                  " One row must be 'id'")
argp.add_argument("--sep", default  =',', help="Field separator in the file.")
argp.add_argument("--replace", action="store_true",
                  help="Will put the key values in the file but not keep existing key values.")

args = argp.parse_args()
if os.path.exists(args.password):
    with open(args.password, 'r') as f:
        password = f.readlines()[0].strip()
else:
    password = args.password

key_values_df = readCSV(args.file, args.sep)
set_all_key_values(key_values_df,
                   args.user, password,
                   args.server, args.port,
                   args.replace)
