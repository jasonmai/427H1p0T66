import csv
import sys
import math
import statistics
import collections
import time
import random
import pickle

NUM_STOCKS = 100
SAMPLE_DATA_FILE_NAME = 'p1data'
SAVED_PARSED_DATA = 'saved_parsed.blob'
SAVED_EQ_FILE_NAME = 'saved_eq_vals'
CONSTS = [1,1,1,1,1,1,1,1,1,1,1,1]
#CONSTS = [1,0,0,0,0,0,0,0,0,0,0,0]
#CONSTS = [0,1,0,0,0,0,0,0,0,0,0,0]
#CONSTS = [0,0,0,0,1,0,0,0,0,0,0,0]
#CONSTS = [0,0,1,0,0,0,0,0,0,0,0,0]
#CONSTS = [0,0,0,0,0,0,1,0,0,0,0,0]
#CONSTS = [0,1,0,0,0,0,0,0,0,0,1,0]

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
    return sum(weight_matrix[day])/(ts_1 * NUM_STOCKS)

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

def weight(day, stock_num, eq_comp_matrix, constants):
    if (day < 2):
        return 99
    result = 0
    for i in range(12):
        result += constants[i] * eq_comp_matrix[i][day][stock_num]
    return result

def generate_weight_matrix(eq_comp_matrix, constants, parsed_data):
    weight_matrix = []
    for day in range(len(parsed_data)):
        stock_weights = []
        for stock_num in range(NUM_STOCKS):
            stock_weights.append(weight(day, stock_num, eq_comp_matrix,
					constants))
        weight_matrix.append(stock_weights)
    return weight_matrix

def memoize_r_data(parsed_data):
    rcc_matrix = []
    roo_matrix = []
    rco_matrix = []
    roc_matrix = []
    roc_matrix = generate_roc_matrix(parsed_data)
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


def sharpe_ratio(rps):
    actual_rps = rps[2:]
    avg_returns = statistics.mean(actual_rps)
    stdev_returns = statistics.stdev(actual_rps)
    return avg_returns/stdev_returns

def print_formatter(item):
    print_item = round(item, 7) + 0
    if (print_item == 0):
        return 0
    return print_item
    #return format(print_item, '.7f')


def output(file_name, parsed_data, constants, saved_data_exists):

    if (saved_data_exists):
        eq_comp_matrix = read_saved_eq_comp_matrix_vals(SAVED_EQ_FILE_NAME)
        weight_matrix = generate_weight_matrix(eq_comp_matrix, constants,
                                               parsed_data)
        roc_matrix = generate_roc_matrix(parsed_data)
    else: 
        r_data = memoize_r_data(parsed_data)
        eq_comp_matrix = memoize_component_vals(parsed_data, r_data)
        write_comp_vals(SAVED_EQ_FILE_NAME, eq_comp_matrix)
        weight_matrix = generate_weight_matrix(eq_comp_matrix, constants,
                                               parsed_data)
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



def generate_rps(constants, parsed_data):
    #t0 = time.perf_counter()
    eq_comp_matrix = read_saved_eq_comp_matrix_vals(SAVED_EQ_FILE_NAME)
    #print('time: ', time.perf_counter() - t0)

    #t0 = time.perf_counter()
    weight_matrix = generate_weight_matrix(eq_comp_matrix, constants,
					   parsed_data)
    #print('time: ', time.perf_counter() - t0)

    #t0 = time.perf_counter()
    roc_matrix = generate_roc_matrix(parsed_data)
    #print('time: ', time.perf_counter() - t0)

    #t0 = time.perf_counter()
    rps = []
    for day in range(len(parsed_data)):
        rps.append(rp2(day, weight_matrix, roc_matrix))
    #print('time: ', time.perf_counter() - t0)
    
    return rps

def get_sharpe(constants, parsed_data):
    t0 = time.perf_counter()
    rps = generate_rps(constants, parsed_data)
    sharpe = sharpe_ratio(rps)
    print('time: ', time.perf_counter() - t0)
    print(sharpe)

#output('jm_p2_results.csv', parse_file(SAMPLE_DATA_FILE_NAME), CONSTS, True)
#output('jm_p2_results.csv', parse_file(SAMPLE_DATA_FILE_NAME), CONSTS, False)

##rps = generate_rps(CONSTS, parse_file(SAMPLE_DATA_FILE_NAME))
##sharpe = sharpe_ratio(rps)











def save_file_data(data):
    file = open(SAVED_PARSED_DATA, 'wb')
    return pickle.dump(data, file)

def load_file_data():
    return pickle.load(open(SAVED_PARSED_DATA, 'rb'))  

#save_file_data(parse_file(SAMPLE_DATA_FILE_NAME))
LOADED_FILE = load_file_data()
LOADED_EQ = read_saved_eq_comp_matrix_vals(SAVED_EQ_FILE_NAME)
SAVED_ROC = generate_roc_matrix(LOADED_FILE)
def generate_rps_opt(constants, parsed_data):
    weight_matrix = generate_weight_matrix(LOADED_EQ, constants,
					   parsed_data)
    rps = []
    for day in range(len(parsed_data)):
        rps.append(rp2(day, weight_matrix, SAVED_ROC))
    return rps
def get_sharpe_opt(constants, parsed_data):
    rps = generate_rps_opt(constants, parsed_data)
    return sharpe_ratio(rps)
def gs(constants):
    return get_sharpe_opt(constants, LOADED_FILE)



def random_algo():
    for i in range(10000):
        constants = []
        for i in range(12):
            rand = random.randint(-2,2)
            rand += random.randint(-100000000000,100000000000)/100000000000
            constants.append(rand)
        sharpe = gs(constants)
        if (sharpe > 0.4):
            print(constants, ' : ', sharpe)
            break;
        else:
            print('.', end='')

def random_algo2():
    for i in range(10000):
        constants = [-0.14951564, -0.4744389, 0.8760662, -1.874715, 0.166849, -0.3826503, 0.2115474, 0.177571, -0.2179634, -0.03938351, 0.1606311, 0.302414]
        for i in range(12):
            constants[i] += random.randint(-100000000000,100000000000)/100000000000
        sharpe = gs(constants)
        if (sharpe > 0.437):
            print(constants, ' : ', sharpe)
            break;
        else:
            print('.', end='')


def gs_range(constants, from_day, to_day):
    rps = generate_rps_opt(constants, LOADED_FILE)
    actual_rps = rps[from_day:to_day]
    avg_returns = statistics.mean(actual_rps)
    stdev_returns = statistics.stdev(actual_rps)
    return avg_returns/stdev_returns


constrained_to = [2,3,6,7,10,11]
def hill_climbing(starting_constants):
    constants = starting_constants[:]
    current_best_sharpe = gs(starting_constants)
    sharpe = current_best_sharpe
    current_a_to_vary = constrained_to[random.randint(0,5)]
    amount_to_vary = random.randint(-100000000000,100000000000)/100000000000
    amount_to_vary += random.randint(-2,2)
    delta = collections.deque(maxlen=10)
    delta.append(1)
    while (True):
        temp_constants = constants[:]
        temp_constants[current_a_to_vary] += amount_to_vary

        prev_sharpe = sharpe
        sharpe = gs_range(temp_constants, 2, 602)

        if (sharpe > current_best_sharpe):
            print('sharpe: ', sharpe)
            valid_sharpe = gs_range(temp_constants, 603, 1003)
            print('valid sharpe: ', valid_sharpe)
            print(temp_constants)
            print('\n')
            
            current_best_sharpe = sharpe
            constants = temp_constants
            if (amount_to_vary > 0):
                amount_to_vary = random.randint(1,100000000000)/100000000000
            else:
                amount_to_vary = random.randint(-100000000000,0)/100000000000

            if sharpe > 0.4:
                with open("results.txt", "a") as file:
                    file.write(str(sharpe) + ' : ' + str(valid_sharpe) + ' : ' + str(temp_constants) + '\n')
            delta.append(sharpe - prev_sharpe)
            if (sum(delta) < 0.000001):
                delta = collections.deque(maxlen=10)
                delta.append(1)
                current_a_to_vary = constrained_to[random.randint(0,5)]
                amount_to_vary = random.randint(-100000000000,100000000000)/100000000000
                amount_to_vary += random.randint(-2,2)
        else:
            print('.')
            current_a_to_vary = constrained_to[random.randint(0,5)]
            amount_to_vary = random.randint(-100000000000,100000000000)/100000000000
            amount_to_vary += random.randint(-2,2)

#hill_climbing([0,0,1,0,0,0,1,0,0,0,1,0])   
#hill_climbing([-0.08911538287000001, -0.53621066426, 0.8760662, -1.94655274693, 0.166849, -0.34789106041999995, 0.2115474, 0.16615337498000002, -0.2179634, -0.03938351, 0.1606311, 0.302414])
#hill_climbing([-0.08911538287000001, -0.53621066426, 0.8760662, -1.87618043702, 0.166849, -0.34789106041999995, 0.2115474, 0.177571, -0.2179634, -0.03938351, 0.1606311, 0.302414])



##results = pickle.load(open('good_results.blob', 'rb'))
##for res in results:
##    
##    print(gs_range(res, 603, 1003), ' ', res)

##for res in results:
##    print(res)




