#!/bin/bash
omero_path="$1"
omero_server="$2"
omero_user="$3"
omero_password=$(cat "$4")
depth=$5
project_name=$6
dataset_name=$7
omero_dir_or_file=$8
temp_file=$9
${omero_path} import -s ${omero_server} -u ${omero_user} -w ${omero_password} --depth ${depth} -T Project:name:"${project_name}"/Dataset:name:"${dataset_name}" ${omero_dir_or_file} &> ${temp_file} 
dId=$(${omero_path} search -s ${omero_server} -u ${omero_user} -w ${omero_password} --field "name" --ids-only --style plain "Dataset" "${dataset_name}" | awk -F ":" 'END{print $NF}')
oFile=$(${omero_path} upload -s ${omero_server} -u ${omero_user} -w ${omero_password} $temp_file | awk '{print $NF}')
fAnn=$(${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new FileAnnotation file=${oFile})
${omero_path} obj -s ${omero_server} -u ${omero_user} -w ${omero_password} new DatasetAnnotationLink parent=Dataset:${dId} child=${fAnn}