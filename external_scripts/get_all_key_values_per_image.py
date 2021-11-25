#!/home/ldelisle/.conda/envs/omero/bin/python
import omero
from omero.gateway import BlitzGateway
import argparse
import sys
import os
import pandas as pd

def get_images_df(username, password, host, port, user_only):
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        # inspired by https://gist.github.com/will-moore/12d31c026bfe5adaea4b2341494069a0#file-metadata_queries-py
        params = omero.sys.ParametersI()
        query = """select 
    image.id, image.name, dataset.name, project.name, mv.name, mv.value, project.details.owner.id
from 
    Image image
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
join image.annotationLinks ial
join ial.child a
join a.mapValue mv
"""
        if user_only:
            query += f"where project.details.owner.id = {conn.getEventContext().userId}\n"
        else:
            # We want to get the correspondance id name:
            group = conn.getGroupFromContext()
            # List the group owners and other members
            owners, members = group.groupSummary()
            ids_to_name = {}
            for o in owners:
                ids_to_name[o.getId()] = o.getOmeName()
            for m in members:
                ids_to_name[m.getId()] = m.getOmeName()
        result = conn.getQueryService().projection(query, params)
    result_dict = {}
    for row in result:
        image_id, image_name, dataset_name, project_name, mv_name, mv_value, owner_id = [x.val for x in row]
        if image_id not in result_dict:
            result_dict[image_id] = {'id': image_id, 'image.name': image_name,
                                    'dataset.name':dataset_name, 'project.name': project_name}
            if not user_only:
                result_dict[image_id]['user.omename'] = ids_to_name[owner_id]
        result_dict[image_id][mv_name] = mv_value
    return(pd.DataFrame([result_dict[id] for id in result_dict]))


argp = argparse.ArgumentParser(
    description=("Get a csv with all images matching the key values."))
argp.add_argument("-s", "--server", help="OMERO server hostname")
argp.add_argument("-p", "--port", help="OMERO server port")
argp.add_argument("-u", "--user", help="OMERO username")
argp.add_argument("-w", "--password", help="OMERO password or file with password in first line")
argp.add_argument("--onlyUserProjects",
                  help = "Restrict search to projects associated to the user.",
                  action="store_true")
argp.add_argument("-o", "--output", default=sys.stdout,
                  type=argparse.FileType('w'),
                  help="Output csv file.")

args = argp.parse_args()
if os.path.exists(args.password):
    with open(args.password, 'r') as f:
        password = f.readlines()[0].strip()
else:
    password = args.password
df = get_images_df(args.user, password, args.server, args.port,
                   args.onlyUserProjects)
df.to_csv(args.output, index=False)
