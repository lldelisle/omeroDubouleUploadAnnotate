#!/home/ldelisle/.conda/envs/omero/bin/python
import omero
from omero.gateway import BlitzGateway
import argparse
import sys
import os
import pandas as pd
import string

def get_images_df(username, password, host, port, user_only, selected_project, selected_dataset, selected_screen, selected_plate):
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        # inspired by https://gist.github.com/will-moore/12d31c026bfe5adaea4b2341494069a0#file-metadata_queries-py
        params = omero.sys.ParametersI()
        # First get the structure (image - dataset - project)
        query1 = """select 
    image.id, image.name, dataset.name, dataset.id, project.name, project.details.owner.id
from 
    Image image
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
"""
        # First get the structure (image - well - plate - screen)
        query1bis = """select 
    image.id, image.name, w.row, w.column, plate.name, plate.id, screen.name, screen.details.owner.id
from 
    Image image
join image.wellSamples ws
join ws.well w
join w.plate plate
join plate.screenLinks s
join s.parent screen
"""
        # Then get the key values
        query2 = """select 
    image.id, mv.name, mv.value
from 
    Image image
join image.annotationLinks ial
join ial.child a
join a.mapValue mv
"""
        query2bis = ""
        if user_only:
            query1 += f"where project.details.owner.id = {conn.getEventContext().userId}"
            query1bis += f"where screen.details.owner.id = {conn.getEventContext().userId}"
            query2bis = query2 + f"""
join image.wellSamples ws
join ws.well w
join w.plate plate
join plate.screenLinks s
join s.parent screen
where screen.details.owner.id = {conn.getEventContext().userId}
"""
            query2 += f"""
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
where project.details.owner.id = {conn.getEventContext().userId}
"""
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
        if selected_dataset is not None:
            query1 += f"where dataset.id = {selected_dataset}"
            query2 += f"""
join image.datasetLinks d
join d.parent dataset
where dataset.id = {selected_dataset}
"""
            query1bis = ""
        if selected_project is not None:
            query1 += f"where project.id = {selected_project}"
            query2 += f"""
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
where project.id = {selected_project}
"""
            query1bis = ""
        if selected_plate:
            query1bis += f"where plate.id = {selected_plate}"
            query1 = ""
            query2 += f"""
join image.wellSamples ws
join ws.well w
join w.plate plate
where plate.id = {selected_plate}
"""
        if selected_screen:
            query1bis += f"where screen.id = {selected_screen}"
            query1 = ""
            query2 += f"""
join image.wellSamples ws
join ws.well w
join w.plate plate
join plate.screenLinks s
join s.parent screen
where screen.id = {selected_screen}
"""
        if (query1 != ""):
            result1 = conn.getQueryService().projection(query1, params)
        else:
            result1 = []
        if (query1bis != ""):
            result1bis = conn.getQueryService().projection(query1bis, params)
        else:
            result1bis = []
        result2 = conn.getQueryService().projection(query2, params)
        if (query2bis != ""):
            result2bis = conn.getQueryService().projection(query2bis, params)
        else:
            result2bis = []
    result_dict = {}
    if (len(result1) > 0):
        for row in result1:
            image_id, image_name, dataset_name, dataset_id, project_name, owner_id = [x.val for x in row]
            if image_id not in result_dict:
                result_dict[image_id] = {'id': image_id, 'image.name': image_name,
                                        'dataset.name':dataset_name, 'dataset.id':dataset_id,
                                        'project.name': project_name}
                if not user_only:
                    result_dict[image_id]['user.omename'] = ids_to_name[owner_id]
    if (len(result1bis) > 0):
        for row in result1bis:
            image_id, image_name, w_row, w_column, plate_name, plate_id, screen_name, owner_id = [x.val for x in row]
            if image_id not in result_dict:
                result_dict[image_id] = {'id': image_id, 'image.name': image_name,
                                        'well.name': f"{list(string.ascii_uppercase)[w_row]}{w_column + 1}" , 'plate.name': plate_name, 'plate.id':plate_id,
                                        'screen.name': screen_name}
                if not user_only:
                    result_dict[image_id]['user.omename'] = ids_to_name[owner_id]    
    for row in result2:
        image_id, mv_name, mv_value = [x.val for x in row]
        # We only add key values for images
        # Which are within datasets/projects:
        if image_id in result_dict:
            result_dict[image_id][mv_name] = mv_value
    return(pd.DataFrame([result_dict[id] for id in result_dict]))


argp = argparse.ArgumentParser(
    description=("Get a csv with all images matching the key values."))
argp.add_argument("-s", "--server", help="OMERO server hostname")
argp.add_argument("-p", "--port", help="OMERO server port")
argp.add_argument("-u", "--user", help="OMERO username")
argp.add_argument("-w", "--password", help="OMERO password or file with password in first line")
argp.add_argument("-o", "--output", default=sys.stdout,
                  type=argparse.FileType('w'),
                  help="Output csv file.")
group = argp.add_mutually_exclusive_group()
group.add_argument("--onlyUserProjects",
                  help = "Restrict search to projects associated to the user.",
                  action="store_true")
group.add_argument("--selectedProject", type=int, default=None,
                  help="ID of project to restrict the search to.")
group.add_argument("--selectedDataset", type=int, default=None,
                  help="ID of dataset to restrict the search to.")
group.add_argument("--selectedScreen", type=int, default=None,
                  help="ID of screen to restrict the search to.")
group.add_argument("--selectedPlate", type=int, default=None,
                  help="ID of plate to restrict the search to.")

args = argp.parse_args()
if os.path.exists(args.password):
    with open(args.password, 'r') as f:
        password = f.readlines()[0].strip()
else:
    password = args.password
df = get_images_df(args.user, password, args.server, args.port,
                   args.onlyUserProjects, args.selectedProject,
                   args.selectedDataset, args.selectedScreen,
                   args.selectedPlate)
df.to_csv(args.output, index=False)
