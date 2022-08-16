import sys
import os
from pathlib import Path
import json

DIRS = ["Sequences", "Sets", "Maps", "NonlinearArithmetic"]

def get_cls_from_compile_name(compile_name):
    """ Get fully-qualified class name from fully-qualified compile name of method"""
    rindex = compile_name.rindex('.')
    return compile_name[:rindex]

def read_stats(directory):
    """ Use stats to define dictionary from directories to set of classes defined within those directories """
    dir_dict = {}
    # cls_block_dict maintains mapping from compiled class names to a dict with block coverage info for that class
    cls_block_dict = {}
    for (dir_path, _, filenames) in os.walk(directory):
        main_dir = None
        # Since the directories we're grouping by aren't at same level, use DIR list; if none match, assign Misc.
        for dir in DIRS:
            if dir in dir_path:
                main_dir = dir
                break
        if main_dir is None:
            main_dir = "Misc."
        cls_set = set()
        for filename in filenames:
            full_path = os.path.join(dir_path, filename)
            data = " ".join(
                [line.strip("\n") for line in open(full_path).readlines()])
            methods = json.loads(data)
            for method in methods:
                cls_name = get_cls_from_compile_name(method["compiledName"])
                cls_set.add(cls_name)
                # add block coverage info to the cls_block_dict
                if cls_name not in cls_block_dict:
                    cls_block_dict[cls_name] = { "coveredblocks":0, "totalblocks":0}
                cls_block_dict[cls_name]["totalblocks"] += int(method["blocks"])
                cls_block_dict[cls_name]["coveredblocks"] += int(method["blocksCovered"])                
        # Add set of classes to directory in which they are defined
        if main_dir in dir_dict:
            dir_dict[main_dir].update(cls_set)
        else:
            dir_dict[main_dir] = cls_set
    return dir_dict, cls_block_dict

def group_coverage_by_dir(dir_dict, report_file, cls_block_dict):
    """ Group all class coverage json objects by the directory in which the class is defined """
    table_dict = {}
    # initialize table_dict with starting values for each dir in dir_dict
    for dir in dir_dict:
        table_dict[dir] = {"coveredlines":0, "coverablelines":0, "coveredbranches":0, 
            "totalbranches":0, "coveredblocks":0, "totalblocks":0}
    data = " ".join(
        [line.strip("\n") for line in open(report_file).readlines()])
    coverage_report = json.loads(data)
    cls_list = coverage_report["coverage"]["assemblies"][0]["classesinassembly"]
    for clazz in cls_list:
        name = clazz["name"].split("`")[0]
        for dir, cls_set in dir_dict.items():
            # if found dir where clazz is defined, then add metrics to corresponding table_dict key
            if name in cls_set:
                table_dict[dir]["coveredlines"] += int(clazz["coveredlines"])
                table_dict[dir]["coverablelines"] += int(clazz["coverablelines"])
                table_dict[dir]["coveredbranches"] += int(clazz["coveredbranches"])
                table_dict[dir]["totalbranches"] += int(clazz["totalbranches"])
                table_dict[dir]["coveredblocks"] += cls_block_dict[name]["coveredblocks"]
                table_dict[dir]["totalblocks"] += cls_block_dict[name]["totalblocks"]
    # To report overall coverage, iterate over all dir keys in table_dict
    overall_coverage = {"coveredlines":0, "coverablelines":0, "coveredbranches":0, 
        "totalbranches":0, "coveredblocks":0, "totalblocks":0}
    for _, coverage_obj in table_dict.items():
        overall_coverage["coveredlines"] += coverage_obj["coveredlines"]
        overall_coverage["coverablelines"] += coverage_obj["coverablelines"]
        overall_coverage["coveredbranches"] += coverage_obj["coveredbranches"]
        overall_coverage["totalbranches"] += coverage_obj["totalbranches"]
        overall_coverage["coveredblocks"] += coverage_obj["coveredblocks"]
        overall_coverage["totalblocks"] += coverage_obj["totalblocks"]
    assembly_name = coverage_report["coverage"]["assemblies"][0]["name"]
    return table_dict, overall_coverage, assembly_name



def format_and_print(dir, coverage_obj):
    line_coverage = str(round(coverage_obj["coveredlines"] / coverage_obj["coverablelines"], 2))
    branch_coverage = str(round(coverage_obj["coveredbranches"] / coverage_obj["totalbranches"], 2))
    block_coverage = str(round(coverage_obj["coveredblocks"] / coverage_obj["totalblocks"] , 2))
    coverable_lines = str(coverage_obj["coverablelines"])
    print(f'{dir} & {line_coverage} & {branch_coverage} & {block_coverage} & {coverable_lines} \\\\')

def print_table_dict(table_dict, overall_coverage, asm_name):
    """ Print dir and line/branch coverage in TeX table format """
    print('Project  &  Statement  &  Branch  &  Block  &  Lines \\\\')
    print('\hline\hline')
    format_and_print(asm_name, overall_coverage)
    print('\hline')
    for dir, coverage_obj in sorted(table_dict.items()):
        format_and_print(dir, coverage_obj)

def begin_format():
    dir_dict, cls_block_dict = read_stats(sys.argv[1])
    table_dict, overall_coverage, asm_name = group_coverage_by_dir(dir_dict, sys.argv[2], cls_block_dict)
    print_table_dict(table_dict, overall_coverage, asm_name)

if __name__ == "__main__":
    begin_format()