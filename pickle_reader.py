import pandas as pd

def read_pickle_file(file):
    pickle_data = pd.read_pickle(file)
    return pickle_data

def myprint(d):
  for k, v in d.items():
    if isinstance(v, dict):
      myprint(v)
    else:
      print("{0} - {1} : {2}".format(k,type(v), v ))   

if __name__ == "__main__":
    data = read_pickle_file('jtcmprep.pkl')
    myprint(data)