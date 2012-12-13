module DecisionTreeTest where 

import DataSet
import DecisionTreeUtils

obs1 = Observation [1, -1] True
obs2 = Observation [2, -3] True
obs3 = Observation [3, -2] False
obs4 = Observation [4, -4] False
obs5 = Observation [5, -5] False
obs6 = Observation [6, -6] True
obs7 = Observation [7, -7] True
obs8 = Observation [8, -8] False
obs9 = Observation [9, -9] True
obs10 = Observation [10, -10] False

training = [obs6, obs7, obs8, obs9, obs10, obs1, obs2, obs3, obs4, obs5]


