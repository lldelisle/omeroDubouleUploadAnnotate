#!/home/ldelisle/.conda/envs/omero/bin/python
from yaml import dump
import omero
from omero.gateway import BlitzGateway
import argparse
import sys

def get_all_key_values(username, password, host, port):
    # Get existing key/values
    key_values = {}
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        for ann in conn.getObjects("MapAnnotation"):
            if isinstance(ann._obj, omero.gateway.MapAnnotationI):
                for k, v in ann.getValue():
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
argp.add_argument("-w", "--password", help="OMERO password")
argp.add_argument("-o", "--output", default=sys.stdout,
                  type=argparse.FileType('w'),
                  help="Output yaml file.")

args = argp.parse_args()

key_values = get_all_key_values(args.user, args.password,
                                args.server, args.port)
args.output.write(dump(key_values, default_style="'"))
