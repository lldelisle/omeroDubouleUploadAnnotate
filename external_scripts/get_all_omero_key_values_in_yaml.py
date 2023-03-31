#!/home/ldelisle/.conda/envs/omero/bin/python
from yaml import dump
import omero
from omero.gateway import BlitzGateway
import argparse
import sys
import os

def get_all_key_values(username, password, host, port, exceptions):
    # Get existing key/values
    # Except the one in exceptions
    key_values = {}
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        # Naive version 6 times slower:
        # for ann in conn.getObjects("MapAnnotation"):
        #     if isinstance(ann._obj, omero.gateway.MapAnnotationI):
        #         for k, v in ann.getValue():
        #
        # Version with query quicker:
        # inspired by https://gist.github.com/will-moore/12d31c026bfe5adaea4b2341494069a0#file-metadata_queries-py
        params = omero.sys.ParametersI()
        query = """
            select mv.name, mv.value from 
            ImageAnnotationLink ial
            join ial.child a
            join a.mapValue mv"""
        if exceptions is not None:
            query += f"""
            where mv.name not in ('{"', '".join(exceptions)}')
            """
        result = conn.getQueryService().projection(query, params)
        for row in result:
            k, v = [x.val for x in row]
            if k not in key_values:
                key_values[k] = []
            if v not in key_values[k]:
                key_values[k].append(v)
    return(key_values)


argp = argparse.ArgumentParser(
    description=("Write all key/values used in the group into a yaml."))
argp.add_argument("-s", "--server", help="OMERO server hostname")
argp.add_argument("-p", "--port", help="OMERO server port")
argp.add_argument("-u", "--user", help="OMERO username")
argp.add_argument("-w", "--password", help="OMERO password or file with password in first line")
argp.add_argument("-e", "--exceptions", nargs="+", help="Keys separated by space to not include in yaml")
argp.add_argument("-o", "--output", default=sys.stdout,
                  type=argparse.FileType('w'),
                  help="Output yaml file.")

args = argp.parse_args()
if os.path.exists(args.password):
    with open(args.password, 'r') as f:
        password = f.readlines()[0].strip()
else:
    password = args.password
key_values = get_all_key_values(args.user, password,
                                args.server, args.port, args.exceptions)
args.output.write(dump(key_values, default_style="'"))
