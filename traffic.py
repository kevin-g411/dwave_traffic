def callDwave(stamp):
    #量子（D-wave）を使用した渋滞緩和ナビ
    
    #量子ビットチェック(未実装)
    
    #ライブラリの呼び出し
    from dwave.system.samplers import DWaveSampler
    from dwave.system.composites import EmbeddingComposite
    import pandas as pd
    import numpy as np
    import time
    import datetime
    import csv
    import math

    #プログラムの開始時間
    start = time.time()
    dat = stamp
    with open(r"C:\Users\watanabe.kevin\Desktop\d-wave_traffic\params\prm"+dat+".csv") as f:
        reader = csv.reader(f)
        f.closed

        l = [row for row in reader]
        l2 = [[] for i in range(4)]
        
    for i in range(4):
        count = 0
        while count < len(l[i]):
            if l[i][count] != '':
                l2[i].append(l[i][count])
            count += 1
    
    #パラメータ設定
    #n:物品の数
    #kappa: 制限関数重み
    
    cars = int(l2[0][0])
    
    tate = int(l2[0][1])
    yoko = int(l2[0][2])
    segments = tate + yoko
    routes_d = math.factorial(segments) / (math.factorial(tate) * math.factorial(yoko))
    raw_routes = int(routes_d)
    
    kappa = float(l2[0][3]) * cars * segments
    lamb = float(l2[0][4]) * cars * segments
    
    raw_route_s = [[0 for j in range(segments)]for i in range(raw_routes)]
    
    for i in range(raw_routes):
        for j in range(segments):
            ind = i * segments + j
            raw_route_s[i][j] = int(l2[1][ind])
            #[[0,3,6,9],[0,3,8,11],[0,1,4,9],[2,7,10,11],[2,5,6,9],[2,5,8,11]]
    
    raw_route_p = [[0 for j in range(segments + 1)]for i in range(raw_routes)]
    
    for i in range(raw_routes):
        for j in range(segments + 1):
            ind = i * (segments + 1) + j
            raw_route_p[i][j] = l2[2][ind]
            #raw_route_p = [["A","B","E","F","I"],["A","B","E","H","I"],["A","B","C","F","I"],
                   #["A","D","G","H","I"],["A","D","E","F","I"],["A","D","E","H","I"]]
    
    route_s = [[] for i in range(cars)]
    routes = [0 for i in range(cars)]
    max = 0
    
    passby = [[0]for i in range(cars)]

    for i in range(cars):
        #for j in range(segments + 1):
        #ind = i * (segments + 1) + j
        passby[i][0] = l2[3][i]
    #passby = [["C"],["G"],["E"],["E"],["E"]]

    #経由地点決定
    for current in range(cars):
        for current2 in range(raw_routes):
            if len(list(set(passby[current]) & set(raw_route_p[current2]))) != 0:
                route_s[current].append(raw_route_s[current2])
                routes[current] = routes[current] + 1
            if max < routes[current]:
                max = routes[current]
    
    #単位三角行列の形成
    elements = int (max * (max - 1) / 2)
    coefficient_tr = [[0 for i in range(elements)] for j in range(cars)]
    for s in range(cars):
            ind = 0
            for i in range(routes[s]):
                for j in range(i + 1, routes[s]):
                    count = 0
                    for k in range(segments):
                        if route_s[s][i][k] == route_s[s][j][k]:
                           count = count + 1
                    coefficient_tr[s][ind] = count
                    ind = ind + 1
    
    #単位()行列の形成
    elements = max * max
    elements2 = int (cars * (cars - 1) / 2)
    ind2 = 0
    coefficient_sq = [[0 for i in range(elements)] for j in range(elements2)]
    for s in range(cars):
        for t in range(s + 1, cars):
            ind = 0
            for i in range(routes[s]):
                for j in range(routes[t]):
                    count = 0
                    for k in range(segments):
                        if route_s[s][i][k] == route_s[t][j][k]:
                           count = count + 1
                    coefficient_sq[ind2][ind] = count
                    ind = ind + 1
            ind2 = ind2 + 1
      
    #モデルの設定
    leap_k = 0
    ind_a = [0 for i in range(ind2)]
    Q = dict()
    for k in range(cars):
        ind1 = 0
        for i in range(routes[k]):
            eff_i = i + leap_k
            leap_l = 0
            for l in range(cars):
                for j in range(routes[l]):
                    eff_j = j + leap_l
    
                    if eff_i == eff_j:
                        Q.update({("x"+str(eff_i), "x"+str(eff_j)):segments - kappa - (2 * cars - 1) * lamb})
    
                    elif eff_i < eff_j:
                        if l == k:
                            Q.update({("x"+str(eff_i), "x"+str(eff_j)):2 * coefficient_tr[k][ind1] + 2 * kappa + 2 * lamb})
                            ind1 = ind1 + 1
    
                        else:
                            area =int(k * cars - (k * (k + 1)) / 2 + l - (k + 1)) 
                            Q.update({("x"+str(eff_i), "x"+str(eff_j)):2 * coefficient_sq[area][ind_a[area]] + 2 * lamb})
                            ind_a[area] = ind_a[area] + 1         
                    else:
                       continue
                leap_l = leap_l + routes[l]
        leap_k = leap_k + routes[k]
    
    #print(Q)
    #アニーリング時間の測定開始
    start_time = time.time() - start
    print("Start anealing", start_time)
    # D-waveへの接続
    # 使い方
    # ここでは社内環境で接続するためにproxyを設定していますが、
    # 基本的にはqubo型のモデルとtokenと試行回数（num_reads）を設定するだけでDwaveに計算が投げられます。
    # ※API Tokenは https://www.dwavesys.com/take-leap から取得してください
    owntoken = "DEV-a1fd90e8ee0819f69f5274b38edc6aedbc2d1837"
    dwaveSampler = DWaveSampler(token = owntoken, proxy = "https://172.24.1.34:3128")
    response = EmbeddingComposite(dwaveSampler).sample_qubo(Q, num_reads = 5000)    
    #アニーリング時間の測定終了
    stop_time = time.time() - start
    print("End annealing", stop_time)
    print ("End annealing...\n" )
    
    #データ処理
    #ここではcsvで出力している
    df_result = pd.DataFrame()
    
    k = 0
    
    #with open(r'C:\Users\watanabe.kevin\Desktop\d-wave_traffic\Dwave' + dat + "_" + str(cars) + "_" + str(max) + "_" + str(kappa) + "_" + str(lamb) + '.csv', 'w',newline='') as f:
    with open(r'C:\Users\watanabe.kevin\Desktop\d-wave_traffic\Dwave' + dat + "_" + str(cars) + "_" + str(max) + "_" + "{0:.1f}".format(kappa) + "_" + "{0:.1f}".format(lamb) + '.csv', 'w',newline='') as f:
        writer = csv.writer(f)
        for sample, energy, num_occurrences, chain_break_fraction in list(response.data()):
            #print(sample, "Energy: ", energy, "Occurrences: ", num_occurrences)
            df_tmp = pd.DataFrame(dict(sample), index=[k])
            df_tmp['Energy'] = energy
            df_tmp['Occurrences'] = num_occurrences
            df_result = df_result.append(df_tmp)
            writer.writerow([sample, 'Energy: ', energy, 'Occurrences: ', num_occurrences])
            k+=1
    result = df_result.pivot_table(index=df_result.columns[:j].tolist()+['Energy'], values=['Occurrences'], aggfunc='sum').sort_values('Energy', ascending=False)
    #結果の画面表示
    #print(df_result)
    
    #処理時間表示（計算時間含む）
    pr_time = time.time() - start - stop_time
    print("processing time: " + str(pr_time))
    #計算時間表示
    ann_time = stop_time - start_time 
    print ("annealing time: " + str(ann_time))
