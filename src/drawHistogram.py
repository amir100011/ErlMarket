import matplotlib.pyplot as plt
from erlport.erlterms import List
from collections import defaultdict
from multiprocessing import Process, Queue
QueueErlang = Queue()  # communication channel between erlang and the plot process
Products = {'dairy': ['cheese', 'milk', 'yogurt'],
            'meat': ['steak', 'chicken'],
            'bakery': ['bread', 'buns'],
            'unified': ['cheese', 'milk', 'yogurt', 'steak', 'chicken', 'bread', 'buns']}


def processDepartmentDataFromErlang(title: List, productList: List):
    departmentName = title.to_string()
    defaultProductsList = Products[departmentName]
    dictAmountPerProduct = defaultdict(int)
    for item in defaultProductsList:
        dictAmountPerProduct[item] = 0
    for item in productList:
        nameOfProduct = item[2]
        amount = item[-1]
        nameOfProduct = nameOfProduct.to_string()
        dictAmountPerProduct[nameOfProduct] += amount
    messageToPlotProcess = ["histogram", departmentName, dictAmountPerProduct]
    QueueErlang.put(messageToPlotProcess)


def drawHistogramHelper(QueueData:Queue):
    title = QueueData.get()
    data = QueueData.get()
    plt.figure(num=1)
    ProductNames = data.keys()
    amountPerProduct = []
    for item in ProductNames:
        amountPerProduct.append(data[item])
    ax = plt.subplot(111)
    width = 0.3
    bins = list(map(lambda x: x, range(1, len(ProductNames) + 1)))
    plt.bar(bins, amountPerProduct, width=width)
    plt.title(title)
    ax.set_xticks(bins)
    ax.set_xticklabels(ProductNames)
    plt.show()  # this blocks the process , that is why we did all this mess


def plotProcessStart():
    plotProcess = Process(target=plotProcessLoop, args=())
    plotProcess.start()


def plotProcessStop():
    QueueErlang.put(["terminate"])


def plotProcessLoop(drawHistogramProcess=None):
    item = QueueErlang.get()
    if item[0] == "histogram":
            [_, title, dataDict] = item
            if drawHistogramProcess is not None:
                drawHistogramProcess.terminate()  # it can only be a living process from a previous iteration
            QueueData = Queue()
            drawHistogramProcess = Process(target=drawHistogramHelper, args=(QueueData,))
            drawHistogramProcess.start()
            QueueData.put(title)
            QueueData.put(dataDict)
            plotProcessLoop(drawHistogramProcess)  # recursive call for further instructions
    if item[0] == "terminate":
        print("I got to terminate")
        if drawHistogramProcess is not None:
                drawHistogramProcess.terminate()  # it can only be a living process from a previous iteration

"""
if __name__ == '__main__':
    title = "test"
    Productdict = defaultdict(int)
    Productdict["milk"] = 5
    Productdict["yogurt"] = 10
    Productdict["dasd"] = 0
    plotProcessStart()
    QueueErlang.put(["histogram", title, Productdict])
    i = 0
    while i < 300000:
        print("im still alive {}".format(i))
        i = i + 1
    title = "test2"
    Productdict = defaultdict(int)
    Productdict["milk"] = 30
    Productdict["yogurt"] = 15
    Productdict["dasd"] = 20
    QueueErlang.put(["histogram", title, Productdict])
    i = 0
    while i < 300000:
        print("im still alive {}".format(i))
        i = i + 1
    plotProcessStop()
"""

