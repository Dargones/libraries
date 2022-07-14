import json
import sys
from collections import defaultdict
import re

from numpy import True_

def get_just_method_name(name):
    space = name.find(' ')
    paren = name.find('(')
    return name[space+1:paren]

def is_tested_method(name, method_set):
    for method in method_set:
        check_list = method.split('::', 1)
        if check_list[0] in name and check_list[1] in name:
            return True
    print("returning false on " + name)
    return False

"""
Generate a detailed coverage report based on the .json file produced by coverlet
First argument must be the name of the file
Subsequent arguments must be the names of the modules to include in the report
If no modules are specified, the report is generated for all modules
"""
if __name__ == "__main__":
    data = " ".join([line.strip("\n") for line in open(sys.argv[1]).readlines()])
    parsed_data = json.loads(data)

    tested_methods = {}
    tested_methods_file = open('TestedMethodLister/TestedMethods.txt', 'r')
    lines = tested_methods_file.readlines()
        
    tested_methods = {re.sub(r"[\n\t\s]*", "", line) for line in lines}

    modules = []
    if len(sys.argv) > 2:
        modules = sys.argv[2:]

    stats = defaultdict(dict)
    stats["overall"] = {"lines": 0, "branches": 0, "methods": 0,
                        "lines-covered": 0, "branches-covered": 0,
                        "methods-covered": 0}

    for dll in parsed_data.keys():
        for cs in parsed_data[dll].keys():
            for clazz in parsed_data[dll][cs].keys():
                curr_module = ""
                analyze_class = modules == []
                for module in modules:
                    if clazz.startswith(module + "_Compile."):
                        curr_module = module
                        if module not in stats:
                            stats[module] = {"lines": 0, "branches": 0,
                                             "methods": 0, "lines-covered": 0,
                                             "branches-covered": 0,
                        "methods-covered": 0}
                        analyze_class = True
                        break
                if not analyze_class:
                    continue
                curr_stats = {"lines": 0, "branches": 0, "methods": 0,
                              "lines-covered": 0, "branches-covered": 0,
                              "methods-covered": 0}

                for methodName in parsed_data[dll][cs][clazz].keys():
                    just_method_name = get_just_method_name(methodName)
                    if is_tested_method(just_method_name, tested_methods):
                        method = parsed_data[dll][cs][clazz][methodName]
                        curr_stats["methods"] += 1
                        curr_stats["branches"] += len(method["Branches"])
                        curr_stats["branches-covered"] += sum(b["Hits"] != 0
                                        for b in method["Branches"])
                        curr_stats["lines"] += len(method["Lines"])
                        lines_add = sum(method["Lines"][line] != 0
                                        for line in method["Lines"].keys())
                        curr_stats["lines-covered"] += lines_add
                        curr_stats["methods-covered"] += lines_add != 0

                for key in curr_stats:
                    stats["overall"][key] += curr_stats[key]
                    if curr_module != "":
                        stats[curr_module][key] += curr_stats[key]

    for key in stats:
        if stats[key]["lines"] == 0:
            line_coverage = 0
        else:
            line_coverage = stats[key]["lines-covered"]/stats[key]["lines"]
        if stats[key]["branches"] == 0:
            branch_coverage = 0
        else:
            branch_coverage = stats[key]["branches-covered"]/stats[key]["branches"]
        if stats[key]["methods"] == 0:
            method_coverage = 0
        else:
            method_coverage = stats[key]["methods-covered"]/stats[key]["methods"]
        print(f'{key}:\n'
              f'\tLines: {stats[key]["lines"]}\n'
              f'\tBranches: {stats[key]["branches"]}\n'
              f'\tMethods: {stats[key]["methods"]}\n'
              f'\tLine coverage: {line_coverage}\n'
              f'\tBranch coverage: {branch_coverage}\n'
              f'\tMethod coverage {method_coverage}\n\n')

