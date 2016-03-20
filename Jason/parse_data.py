import csv
import sys
import math
import statistics
import collections
import time
import random
import pickle

GOOD_A = []

with open('some_results.txt', 'r') as file:
    sharpe_ratios = []
    a_vars = []
    lines = file.readlines()
    for line in lines:
        splitt = line.split(' : ', 1)
        sharpe_ratios.append(float(splitt[0]))

        this_list = splitt[1].replace(' ', '').replace('[', '').replace(']', '').replace('\n', '')
##        this_list.replace(' ', '')
##        this_list.replace('[', '')
##        this_list.replace(']', '')
##        this_list.replace('\n', '')

        created_list = this_list.split(',')
        final_list = []
        for num in created_list:
            final_list.append(float(num))
            
        a_vars.append(final_list)


    ratios_map = {}

    lst = []
    for a in a_vars[0]:
        lst.append(int(a))
    key = str(lst)
    
    ratios_map[key] = sharpe_ratios[0]                        

    for i in range(len(sharpe_ratios)):

        lst = []
        for a in a_vars[i]:
            lst.append(int(a))
        key = str(lst)
        
        if key not in ratios_map:
            add_or_not = True
            for keys in ratios_map.keys():
                sts = keys.replace(' ', '').replace('[', '').replace(']', '').split(',')
                new_list = []
                for val in sts:
                    new_list.append(int(val))
                if new_list[5] == lst[5]:
                    add_or_not = False
            if add_or_not:
                ratios_map[key] = sharpe_ratios[i]
        else:
            if ratios_map[key] > sharpe_ratios[i]:
                ratios_map[key] = sharpe_ratios[i]

    
    for val in ratios_map.values():
        for i in range(len(sharpe_ratios)):
            if sharpe_ratios[i] == val:
                GOOD_A.append(a_vars[i])

##    print(good_a)
##    print(len(sharpe_ratios))
##    print(len(ratios_map.keys()))

print(GOOD_A)
f = open('good_results.blob', 'wb')
pickle.dump(GOOD_A, f)
f.close()
                
