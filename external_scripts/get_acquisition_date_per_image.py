#!/home/ldelisle/.conda/envs/omero/bin/python
import omero
from omero.gateway import BlitzGateway
import argparse
import sys
import os
import pandas as pd
import datetime

def get_images_df(username, password, host, port, user_only, selected_project, selected_dataset):
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        # inspired by https://gist.github.com/will-moore/12d31c026bfe5adaea4b2341494069a0#file-metadata_queries-py
        params = omero.sys.ParametersI()
        # Get the acquisition date time
        query = """select 
    image.id, image.acquisitionDate
from 
    Image image
"""
        if user_only:
            query += f"""
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
where project.details.owner.id = {conn.getEventContext().userId}
"""
        if selected_dataset is not None:
            query += f"""
join image.datasetLinks d
join d.parent dataset
where dataset.id = {selected_dataset}
"""
        if selected_project is not None:
            query += f"""
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
where project.id = {selected_project}
"""
        result = conn.getQueryService().projection(query, params)
    result_list = []
    for row in result:
        try:
            image_id, acquisition_ms = [x.val for x in row]
        except AttributeError:
            # Some images have no acquisition date
            pass
        else:
            acquisition_date = datetime.datetime.fromtimestamp(acquisition_ms / 1000)
            adate, atime = str(acquisition_date).split()
            result_list.append({'id': image_id,
                                'acquisition.day': adate,
                                'acquisition.time': atime})
    return(pd.DataFrame(result_list))


argp = argparse.ArgumentParser(
    description=("Get a csv with all images matching the key values."))
argp.add_argument("-s", "--server", help="OMERO server hostname")
argp.add_argument("-p", "--port", help="OMERO server port")
argp.add_argument("-u", "--user", help="OMERO username")
argp.add_argument("-w", "--password", help="OMERO password or file with password in first line")
argp.add_argument("--onlyUserProjects",
                  help = "Restrict search to projects associated to the user.",
                  action="store_true")
argp.add_argument("--selectedProject", type=int, default=None,
                  help="ID of project to restrict the search to.")
argp.add_argument("--selectedDataset", type=int, default=None,
                  help="ID of dataset to restrict the search to.")
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
                   args.onlyUserProjects, args.selectedProject,
                   args.selectedDataset)
df.to_csv(args.output, index=False)
