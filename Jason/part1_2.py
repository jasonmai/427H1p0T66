import csv
import sys
import math

NUM_STOCKS = 100

#data[day][date/stock][stock_num][stock_val]
def parse_file(file_name):
    file = open(file_name, 'rt')
    raw_data = csv.reader(file)
    parsed_data = []
    for day_data in raw_data:
        day = int(day_data[0].strip())
        stocks = []
        base_idx = 1
        for i in range(NUM_STOCKS):
            stock = {}
            stock['so'] = float(day_data[base_idx + 0].strip())
            stock['sh'] = float(day_data[base_idx + 1].strip())
            stock['sl'] = float(day_data[base_idx + 2].strip())
            stock['sc'] = float(day_data[base_idx + 3].strip())
            stock['tvl'] = float(day_data[base_idx + 4].strip())
            stock['ind'] = float(day_data[base_idx + 5].strip())
            stocks.append(stock)
            base_idx += 6
        date_stocks = {}
        date_stocks['date'] = day
        date_stocks['stocks'] = stocks
        parsed_data.append(date_stocks)
    return parsed_data


def weight(day, stock_num, parsed_data):
    if (day < 2):
        return 99
    init_val = -(1/NUM_STOCKS)
    rcc_val = rcc((day - 1), stock_num, parsed_data)
    avg_rcc_val = avg_rcc(day - 1, parsed_data)
    return init_val * (rcc_val - avg_rcc_val)

def weight_2(day, stock_num, rcc_matrix):
    if (day < 2):
        return 99
    init_val = -(1/NUM_STOCKS)
    rcc_val = rcc_matrix[day - 1][stock_num]
    avg_rcc_val = avg_rcc(day - 1, rcc_matrix)
    return init_val * (rcc_val - avg_rcc_val)

def rcc(day, stock_num, parsed_data):
    if (day < 1):
        return 99
    sc_tj = parsed_data[day]['stocks'][stock_num]['sc']
    sc_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['sc']
    return (sc_tj/sc_t_minus1_j) - 1

def ave_rcc_2(day, rcc_matrix):
    sum_rcc  = 0
    for i in range(NUM_STOCKS):
        sum_rcc += rcc_matrix[day][i]
    return sum_rcc/NUM_STOCKS

def avg_rcc(day, parsed_data):
    sum_rcc = 0
    for i in range(NUM_STOCKS):
        sum_rcc += rcc(day, i, parsed_data)
    return sum_rcc/NUM_STOCKS

def rp(day, parsed_data):
    if (day < 2):
        return 99
    sum_numerator = 0
    sum_denom = 0
    for i in range(NUM_STOCKS):
        sum_numerator += weight(day, i, parsed_data) * rcc(day, i, parsed_data)
        sum_denom += abs(weight(day, i, parsed_data))
    return sum_numerator/sum_denom

def rp_2(day, weight_matrix, rcc_matrix):
    if (day < 2):
        return 99
    sum_numerator = 0
    sum_denom = 0
    for i in range(NUM_STOCKS):
        sum_numerator += weight_matrix[day][i] * rcc_matrix[day][i]
        sum_denom += abs(weight_matrix[day][i])
    return sum_numerator/sum_denom

def cumR(day, rp_values):
    if (day < 2):
        return 99
    cum = 0
    for day in range(2, day + 1):
        cum += math.log(1 + rp_values[day])
    return cum

def memoize_data(parsed_data):
    weight_data_tj = []
    rcc_data_tj = []
    for day in range(len(parsed_data)):
        stock_weights = []
        stock_rccs = []
        for stock in range(NUM_STOCKS):
            stock_weights.append(weight(day, stock, parsed_data))
            stock_rccs.append(rcc(day, stock, parsed_data))
        weight_data_tj.append(stock_weights)
        rcc_data_tj.append(stock_rccs)
    return [weight_data_tj, rcc_data_tj]

#PARSED_DATA = parse_file('p1data')


def output(file_name, parsed_data):
    
##    weight_and_rcc = memoize_data(parsed_data)
##    weight_data_tj = weight_and_rcc[0]
##    rcc_data_tj = weight_and_rcc[1]

    with open(file_name, 'w', newline='') as fp:
        file = csv.writer(fp, delimiter=',')
        data = []
        header = ['yyyymmdd', 'RP#(t)', 'CumR(t)', 'Time Series 1', 'Time Series 2', 'W#(tj)']

        data.append(header)
        rps = []
        for day in range(len(parsed_data)):
            row = []
            row.append(parsed_data[day]['date'])
            
            rp_val = rp(day, parsed_data)
            #rp_val = rp_2(day, weight_data_tj, rcc_data_tj)
            rps.append(rp_val)
            row.append(format(rp_val, '.7f'))
            
            row.append(format(cumR(day, rps), '.7f'))
            row.append('99')
            row.append('99')

            for stock in range(NUM_STOCKS):
                #row.append(format(weight_data_tj[day][stock], '.7f'))
                row.append(format(weight(day, stock, parsed_data), '.7f'))
            data.append(row)
            
        file.writerows(data)

output('test.csv', parse_file('p1data'))







