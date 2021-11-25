#!/home/ldelisle/.conda/envs/omero/bin/python
import omero
from omero.gateway import BlitzGateway
import argparse
import sys
import os
import pandas as pd

def get_images_df(username, password, host, port, keyvalues, user_only):
    # keyvalues are like key1=value1;key2=value2...
    # with no '=' in value
    kv_pairs = [['='.join(p.split('=')[:-1]), p.split('=')[-1]]
                for p in keyvalues.split(";")]
    my_col_names = ['id', 'image.name', 'dataset.name', 'project.name']
    with BlitzGateway(username, password, host=host, port=port, secure=True) as conn:
        # inspired by https://gist.github.com/will-moore/12d31c026bfe5adaea4b2341494069a0#file-metadata_queries-py
        params = omero.sys.ParametersI()
        n = len(kv_pairs)
        query = """select 
    image.id, image.name, dataset.name, project.name"""
        if not user_only:
            query += ", project.details.owner.id"
            my_col_names.append('user.omename')
        query +="""
from
    Image image
join image.datasetLinks d
join d.parent dataset
join dataset.projectLinks p
join p.parent project
where image.id in
    (
    select
        image.id
    from 
        Image image
    join image.annotationLinks ial
    join ial.child a
    join a.mapValue mv
    where
        """
        for i, (k,v) in enumerate(kv_pairs):
            if i > 0:
                query +=" or "
            query += f"(mv.name = :key{i} and mv.value = :value{i})"
            params.addString(f'key{i}', k)
            params.addString(f'value{i}', v)
        query += f"""
    group by
        image.id
    having
        count(distinct mv.name) = {n}
    )
"""
        if user_only:
            query += f"    and project.details.owner.id = {conn.getEventContext().userId}\n"
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
    result_list = [[x.val for x in row] for row in result]
    if not user_only:
        result_list = [v[:-1] + [ids_to_name[v[-1]]] for v in result_list]
    return(pd.DataFrame(result_list, columns=my_col_names))


argp = argparse.ArgumentParser(
    description=("Get a csv with all images matching the key values."))
argp.add_argument("-s", "--server", help="OMERO server hostname")
argp.add_argument("-p", "--port", help="OMERO server port")
argp.add_argument("-u", "--user", help="OMERO username")
argp.add_argument("-w", "--password", help="OMERO password or file with password in first line")
argp.add_argument("--keyvalues", help="all key values pairs that the image should match under the form key1=value1;key2=value2...")
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
                args.keyvalues, args.onlyUserProjects)
df.to_csv(args.output, index=False)
