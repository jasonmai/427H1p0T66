import csv
import sys
import math
import pickle

NUM_STOCKS = 100
SAVED_EQ_FILE_NAME = 'saved_eq_vals'
CONSTS = [1,1,1,1,1,1,1,1,1,1,1,1]
#CONSTS = [1,0,0,0,0,0,0,0,0,0,0,0]
#CONSTS = [0,0,0,0,1,0,0,0,0,0,0,0]

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

def rcc(day, stock_num, parsed_data):
    if (day < 1):
        return 99
    sc_tj = parsed_data[day]['stocks'][stock_num]['sc']
    sc_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['sc']
    return (sc_tj/sc_t_minus1_j) - 1

def cumR(day, rp_values):
    if (day < 2):
        return 99
    cum = 0
    for day in range(2, day + 1):
        cum += math.log(1 + rp_values[day])
    return cum

def time_series_1(day, weight_matrix):
    if (day < 2):
        return 99
    cum = 0
    for weight in weight_matrix[day]:
        cum += abs(weight)
    return cum/NUM_STOCKS

def time_series_2(day, ts_1, weight_matrix):
    if (day < 2):
        return 99
    return sum(weight_matrix[day])/ts_1

def rco(day, stock_num, parsed_data):
    if (day < 1):
        return 99
    so_tj = parsed_data[day]['stocks'][stock_num]['so']
    sc_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['sc']
    return (so_tj/sc_t_minus1_j) - 1


def roc(day, stock_num, parsed_data):
    sc_tj = parsed_data[day]['stocks'][stock_num]['sc']
    so_tj = parsed_data[day]['stocks'][stock_num]['so']
    return (sc_tj/so_tj) - 1

def roo(day, stock_num, parsed_data):
    if (day < 1):
        return 99
    so_tj = parsed_data[day]['stocks'][stock_num]['so']
    so_t_minus_1_j = parsed_data[day-1]['stocks'][stock_num]['so']
    return (so_tj/so_t_minus_1_j) - 1

def rvp(day, stock_num, parsed_data):
    init_val = (1/(4*(math.log(2))))
    ln_sh = math.log(parsed_data[day]['stocks'][stock_num]['sh'])
    ln_sl = math.log(parsed_data[day]['stocks'][stock_num]['sl'])
    return init_val * ((ln_sh - ln_sl) ** 2)

def a1(day, stock_num, rcc_matrix):
    rcc_t_minu1_j = rcc_matrix[day - 1][stock_num]
    avg_rcc = rcc_matrix[day - 1][NUM_STOCKS]
    return ((rcc_t_minu1_j - avg_rcc)/NUM_STOCKS)

def a2(day, stock_num, roo_matrix):
    roo_tj = roo_matrix[day][stock_num]
    avg_roo = roo_matrix[day][NUM_STOCKS]
    return ((roo_tj - avg_roo)/NUM_STOCKS)

def a3(day, stock_num, roc_matrix):
    roc_t_minu1_j = roc_matrix[day - 1][stock_num]
    avg_roc = roc_matrix[day - 1][NUM_STOCKS]
    return ((roc_t_minu1_j - avg_roc)/NUM_STOCKS)

def a4(day, stock_num, rco_matrix):
    rco_tj = rco_matrix[day][stock_num]
    avg_rco = rco_matrix[day][NUM_STOCKS]
    return ((rco_tj - avg_rco)/NUM_STOCKS)

def a5(day, stock_num, rcc_matrix, parsed_data):
    tvl_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['tvl']
    avg_tvl_val = avg_tvl(day - 1, stock_num, parsed_data)
    rcc_t_minus1_j = rcc_matrix[day - 1][stock_num]
    avg_rcc = rcc_matrix[day - 1][NUM_STOCKS]
    return ((tvl_t_minus1_j / avg_tvl_val) *
            ((rcc_t_minus1_j - avg_rcc)/NUM_STOCKS))

def a6(day, stock_num, roo_matrix, parsed_data):
    tvl_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['tvl']
    avg_tvl_val = avg_tvl(day - 1, stock_num, parsed_data)
    roo_tj = roo_matrix[day][stock_num]
    avg_roo = roo_matrix[day][NUM_STOCKS]
    return ((tvl_t_minus1_j / avg_tvl_val) *
            ((roo_tj - avg_roo)/NUM_STOCKS))

def a7(day, stock_num, roc_matrix, parsed_data):
    tvl_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['tvl']
    avg_tvl_val = avg_tvl(day - 1, stock_num, parsed_data)
    roc_t_minus1_j = roc_matrix[day - 1][stock_num]
    avg_roc = roc_matrix[day - 1][NUM_STOCKS]
    return ((tvl_t_minus1_j / avg_tvl_val) *
            ((roc_t_minus1_j - avg_roc)/NUM_STOCKS))

def a8(day, stock_num, rco_matrix, parsed_data):
    tvl_t_minus1_j = parsed_data[day - 1]['stocks'][stock_num]['tvl']
    avg_tvl_val = avg_tvl(day - 1, stock_num, parsed_data)
    rco_tj = rco_matrix[day][stock_num]
    avg_rco = rco_matrix[day][NUM_STOCKS]
    return ((tvl_t_minus1_j / avg_tvl_val) *
            ((rco_tj - avg_rco)/NUM_STOCKS))

def a9(day, stock_num, rcc_matrix, parsed_data):
    rvp_t_minus1_j = rvp(day - 1, stock_num, parsed_data)
    avg_rvp_val = avg_rvp(day - 1, stock_num, parsed_data)
    rcc_t_minus1_j = rcc_matrix[day - 1][stock_num]
    avg_rcc = rcc_matrix[day - 1][NUM_STOCKS]
    return ((rvp_t_minus1_j / avg_rvp_val) *
            ((rcc_t_minus1_j - avg_rcc)/NUM_STOCKS))

def a10(day, stock_num, roo_matrix, parsed_data):
    rvp_t_minus1_j = rvp(day - 1, stock_num, parsed_data)
    avg_rvp_val = avg_rvp(day - 1, stock_num, parsed_data)
    roo_tj = roo_matrix[day][stock_num]
    avg_roo = roo_matrix[day][NUM_STOCKS]
    return ((rvp_t_minus1_j / avg_rvp_val) *
            ((roo_tj - avg_roo)/NUM_STOCKS))

def a11(day, stock_num, roc_matrix, parsed_data):
    rvp_t_minus1_j = rvp(day - 1, stock_num, parsed_data)
    avg_rvp_val = avg_rvp(day - 1, stock_num, parsed_data)
    roc_t_minus1_j = roc_matrix[day - 1][stock_num]
    avg_roc = roc_matrix[day - 1][NUM_STOCKS]
    return ((rvp_t_minus1_j / avg_rvp_val) *
            ((roc_t_minus1_j - avg_roc)/NUM_STOCKS))

def a12(day, stock_num, rco_matrix, parsed_data):
    rvp_t_minus1_j = rvp(day - 1, stock_num, parsed_data)
    avg_rvp_val = avg_rvp(day - 1, stock_num, parsed_data)
    rco_tj = rco_matrix[day][stock_num]
    avg_rco = rco_matrix[day][NUM_STOCKS]
    return ((rvp_t_minus1_j / avg_rvp_val) *
            ((rco_tj - avg_rco)/NUM_STOCKS))

def rp2(day, weight_matrix, roc_matrix):
    if (day < 2):
        return 99
    sum_numerator = 0
    sum_denom = 0
    for i in range(NUM_STOCKS):
        sum_numerator += weight_matrix[day][i] * roc_matrix[day][i]
        sum_denom += abs(weight_matrix[day][i])
    return sum_numerator/sum_denom


AVG_TVL_CACHE = {}
def avg_tvl(day, stock_num, parsed_data):
    cache_key = str(day) + ',' + str(stock_num)
    if (cache_key in AVG_TVL_CACHE):
        return AVG_TVL_CACHE[cache_key]
    avg_from = max(0, day - 199)
    avg_to = day
    num_vals = (avg_to + 1) - avg_from
    sum_tvls = 0
    for day in range(avg_from, (avg_to + 1)):
        sum_tvls += parsed_data[day]['stocks'][stock_num]['tvl']
    result = sum_tvls/num_vals
    AVG_TVL_CACHE[cache_key] = result
    return result

AVG_RVP_CACHE = {}
def avg_rvp(day, stock_num, parsed_data):
    cache_key = str(day) + ',' + str(stock_num)
    if (cache_key in AVG_RVP_CACHE):
        return AVG_RVP_CACHE[cache_key]
    avg_from = max(0, day - 199)
    avg_to = day
    num_vals = (avg_to + 1) - avg_from
    sum_rvps = 0
    for day in range(avg_from, (avg_to + 1)):
        sum_rvps += rvp(day, stock_num, parsed_data)
    result = sum_rvps/num_vals
    AVG_RVP_CACHE[cache_key] = result
    return result

def weight(day, stock_num, eq_comp_matrix):
    if (day < 2):
        return 99
    constants = CONSTS
    result = 0
    for i in range(12):
        result += constants[i] * eq_comp_matrix[i][day][stock_num]
    return result

def generate_weight_matrix(eq_comp_matrix, parsed_data):
    weight_matrix = []
    for day in range(len(parsed_data)):
        stock_weights = []
        for stock_num in range(NUM_STOCKS):
            stock_weights.append(weight(day, stock_num, eq_comp_matrix))
        weight_matrix.append(stock_weights)
    return weight_matrix

def memoize_r_data(parsed_data):
    rcc_matrix = []
    roo_matrix = []
    rco_matrix = []
    roc_matrix = []
    for day in range(len(parsed_data)):
        stock_rccs = []
        stock_roos = []
        stock_rcos = []
        stock_rocs = []
        for stock in range(NUM_STOCKS):
            stock_rccs.append(rcc(day, stock, parsed_data))
            stock_roos.append(roo(day, stock, parsed_data))
            stock_rcos.append(rco(day, stock, parsed_data))
            stock_rocs.append(roc(day, stock, parsed_data))
        stock_rccs.append(sum(stock_rccs)/NUM_STOCKS)
        stock_roos.append(sum(stock_roos)/NUM_STOCKS)
        stock_rcos.append(sum(stock_rcos)/NUM_STOCKS)
        stock_rocs.append(sum(stock_rocs)/NUM_STOCKS)
        rcc_matrix.append(stock_rccs)
        roo_matrix.append(stock_roos)
        rco_matrix.append(stock_rcos)
        roc_matrix.append(stock_rocs)
    return [rcc_matrix, roo_matrix, rco_matrix, roc_matrix]

def memoize_component_vals(parsed_data, r_data):
    rcc_matrix = r_data[0]
    roo_matrix = r_data[1]
    rco_matrix = r_data[2]
    roc_matrix = r_data[3]

    eq_comp_matrix = [[] for i in range(12)]

    for day in range(len(parsed_data)):
        cs_rows = [[] for i in range(12)]
        for stock_num in range(NUM_STOCKS):
            a_vals = []
            if (day < 2):
                for i in range(12):
                    a_vals.append(0)
            else: 
                a_vals.append(a1(day, stock_num, rcc_matrix))
                a_vals.append(a2(day, stock_num, roo_matrix))
                a_vals.append(a3(day, stock_num, roc_matrix))
                a_vals.append(a4(day, stock_num, rco_matrix))
                a_vals.append(a5(day, stock_num, rcc_matrix, parsed_data))
                a_vals.append(a6(day, stock_num, roo_matrix, parsed_data))
                a_vals.append(a7(day, stock_num, roc_matrix, parsed_data))
                a_vals.append(a8(day, stock_num, rco_matrix, parsed_data))
                a_vals.append(a9(day, stock_num, rcc_matrix, parsed_data))
                a_vals.append(a10(day, stock_num, roo_matrix, parsed_data))
                a_vals.append(a11(day, stock_num, roc_matrix, parsed_data))
                a_vals.append(a12(day, stock_num, rco_matrix, parsed_data))
            for i in range(12):
                cs_rows[i].append(a_vals[i])
        for i in range(12):
            eq_comp_matrix[i].append(cs_rows[i])
    return eq_comp_matrix

def generate_roc_matrix(parsed_data):
    roc_matrix = []
    for day in range(len(parsed_data)):
        stock_rocs = []
        for stock in range(NUM_STOCKS):
            stock_rocs.append(roc(day, stock, parsed_data))
        stock_rocs.append(sum(stock_rocs)/NUM_STOCKS)
        roc_matrix.append(stock_rocs)
    return roc_matrix

def write_comp_vals(file_name, data):
    file = open(file_name, 'wb')
    pickle.dump(data, file)
    
def read_saved_eq_comp_matrix_vals(file_name):
    return pickle.load(open(file_name, 'rb'))

def print_formatter(item):
    print_item = round(item, 7) + 0
    return format(print_item, '.7f')

def output(file_name, parsed_data, saved_data_exists):

    if (saved_data_exists):
        eq_comp_matrix = read_saved_eq_comp_matrix_vals(SAVED_EQ_FILE_NAME)
        weight_matrix = generate_weight_matrix(eq_comp_matrix, parsed_data)
        roc_matrix = generate_roc_matrix(parsed_data)
    else: 
        r_data = memoize_r_data(parsed_data)
        eq_comp_matrix = memoize_component_vals(parsed_data, r_data)
        write_comp_vals(SAVED_EQ_FILE_NAME, eq_comp_matrix)
        weight_matrix = generate_weight_matrix(eq_comp_matrix, parsed_data)
        roc_matrix = r_data[3]

    with open(file_name, 'w', newline='') as fp:
        file = csv.writer(fp, delimiter=',')
        data = []
        header = ['yyyymmdd', 'RP#(t)', 'CumR(t)', 'Time Series 1',
                  'Time Series 2', 'W#(tj)']

        data.append(header)
        rps = []
        for day in range(len(parsed_data)):
            row = []
            row.append(parsed_data[day]['date'])

            rp_val = rp2(day, weight_matrix, roc_matrix)
            rps.append(rp_val)
            row.append(print_formatter(rp_val))
            
            row.append(print_formatter(cumR(day, rps)))

            time_series_1_val = time_series_1(day, weight_matrix)
            row.append(print_formatter(time_series_1_val))
            
            row.append(print_formatter(time_series_2(day, time_series_1_val,
                                       weight_matrix)))

            for stock in range(NUM_STOCKS):
                row.append(print_formatter(weight_matrix[day][stock]))
            data.append(row)

        file.writerows(data)



output('jm_p2_results.csv', parse_file('p1data'), True)
