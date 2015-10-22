'''
  File:   learn_and_test.py 
  Course: Intellegent Systems (471 group, term 7)
  Module: Recommender Systems
  Author: Aleksei Semin (alllex.semin@gmail.com)
  Date:   10.2015 (c)
  Misc:   Need to have original songlog.db in the same directory 
'''
import os
import sys
import time
import math
import random
import datetime
import sqlite3 as db
from multiprocessing import Pool

def dt(start,finish):
    return str(datetime.timedelta(seconds=finish-start))

def get(d, k):
    return d[k] if k in d else 0

t1 = time.time()

db_path = 'songlog.db'
conn = db.connect(db_path)

# parameters
ulimit = 0
K = 10

print "Collaborative filtering with", K, "nearest neighbors"

if ulimit <= 0: 
    print "Learning on the full user-set"
else:
    print "Users count", ulimit

qlimit = "limit {}".format(ulimit) if ulimit > 0 else ""
user_ids = map(lambda x: x[0], conn.execute("select id from users {}".format(qlimit)).fetchall())
userv_dict = dict()
global_maxrate = 0

for user_id in user_ids:
    q = "select song_id, amount from learnlog where user_id={}".format(user_id)
    listened = conn.execute(q).fetchall() 
    if len(listened) == 0: # in case all records are in test set
        continue
    usongs, urates = zip(*listened)
    # user's biggest rating
    maxrate = max(urates)
    # normalize user's ratings
    urates_norm = [r / maxrate for r in urates] 
    # length of normalized user vector 
    userv_len = math.sqrt(sum([a*a for a in urates_norm])) 
    # crate map : song -> rating
    userv = dict(zip(usongs, urates_norm))
    userv_dict[user_id] = (userv_len, maxrate, userv)
    if maxrate > global_maxrate:
        global_maxrate = maxrate

# keep only the users with calculated parameters
user_id_set = set(userv_dict.keys())
# update set of ids 
user_ids = list(user_id_set)

def get_veclen(u):
    return userv_dict[u][0]

def get_maxrate(u):
    return userv_dict[u][1]

def get_rates(u):
    return userv_dict[u][2]

t2 = time.time()
print "Global maximal listen-count", global_maxrate
print "User fav-vectors created...", dt(t1, t2)

def sim(user_n, user_m): # cosine
    (userv_len_n, maxrate_n, userv_n) = userv_dict[user_n]
    (userv_len_m, maxrate_m, userv_m) = userv_dict[user_m]
    # calculating scalar product
    s = sum([userv_n[k] * get(userv_m, k) for k in userv_n.keys()])
    # return cosine simularity
    return s / (userv_len_n * userv_len_m)

def knn(u, k=K):
    sims = [(sim(u, v), v) for v in user_ids if v != u]
    top = sorted(sims, reverse=True)[:k]    
    return (u, top)

task_pool = Pool()
# find kNN for each user
user_knn_pairs = task_pool.map(knn, user_ids)
# create map : user -> kNN
user_knn = dict(user_knn_pairs)
    
t2 = time.time()
print "User neigbours found...", dt(t1, t2)

def predict(u, s):
    uknn = user_knn[u]
    (ws, ns) = zip(*uknn)
    # obtain normalized ratings of neighbors for the song
    rs = map(lambda n: get(get_rates(n), s), ns)
    # prediction as ratings weighted with simularity factors 
    p = sum([x * y for x, y in zip(ws, rs)]) / len(ns)
    # convert into number of plays
    return get_maxrate(u) * p

curs = conn.cursor()
curs.execute("select user_id, song_id, amount from testlog")

test_count = 0
mae_sum = 0
mse_sum = 0
while True:
    fetched = curs.fetchone()
    if fetched is None:
        break
    # next test sample
    (u, s, c) = fetched 
    if u not in user_id_set: # skip users with no data
        continue
    r = predict(u, s)
    d = abs(r - c)
    mae_sum += d
    mse_sum += d * d
    test_count += 1

nmae = (mae_sum / test_count) / global_maxrate
nrmse = math.sqrt(mse_sum / test_count) / global_maxrate

print test_count, "tests done"
print "NMAE =", nmae
print "NRMSE =", nrmse

conn.close()

t2 = time.time()
print "Done...", dt(t1, t2)