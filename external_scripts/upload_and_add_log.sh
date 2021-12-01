#!/bin/bash
omero_path="$1"
omero_server="$2"
omero_user="$3"
omero_password=$(cat "$4")
depth=$5
project_name_or_id=$6
dataset_name_or_id=$7
omero_dir_or_file=$8
temp_file=$9
to_create=${10}
if [ "$to_create" = "both" ]; then
    # Create a project:
    project_name_or_id=$(${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new Project name="${project_name_or_id}" | awk -F ":" 'END{print $NF}')
    echo "Just created the new project ${project_name_or_id}"
fi
if [ "$to_create" = "both" ] || [ "$to_create" = "dataset" ]; then
    dataset_name_or_id=$(${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new Dataset name="${dataset_name_or_id}" | awk -F ":" 'END{print $NF}')
    echo "Just created the new dataset ${dataset_name_or_id}"
    ${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new ProjectDatasetLink parent=Project:${project_name_or_id} child=Dataset:${dataset_name_or_id}
fi
echo "Start upload"
${omero_path} import -s ${omero_server} -u ${omero_user} -w ${omero_password} --depth ${depth} -T Dataset:id:"${dataset_name_or_id}" ${omero_dir_or_file} &> ${temp_file}
echo "Upload finished"
echo "Uploading $temp_file"
# Warning: omero upload gives a deprecation warning...
oFile=$(${omero_path} upload -s ${omero_server} -u ${omero_user} -w ${omero_password} $temp_file | awk 'END{print $NF}')
echo "Create FileAnnotation for $oFile"
fAnn=$(${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new FileAnnotation file=${oFile})
echo "Create Link between dataset and $fAnn"
${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new DatasetAnnotationLink parent=Dataset:${dataset_name_or_id} child=${fAnn}
