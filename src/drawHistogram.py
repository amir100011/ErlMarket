import matplotlib.pyplot as plt
from erlport.erlterms import List
from collections import defaultdict
from multiprocessing import Process, Queue
import tensorflow as tf
import os


first = 1
log_dir = r"logdir/"
QueueErlang = Queue()  # communication channel between erlang and the plot process
Products = {'dairy': ['cheese', 'milk', 'yogurt'],
            'meat': ['steak', 'chicken'],
            'bakery': ['bread', 'buns'],
            'unified': ['cheese', 'milk', 'yogurt', 'steak', 'chicken', 'bread', 'buns']}

# stop signal to the histogram
def processStopHistogram():
    QueueErlang.put(["stopHistogram"])

# receiving data from erlang to buffer the tensorboard with
def processBudgetVsExpenceFromErlang(income, expence):
    messageToPlotProcess = ["plot", income, expence]
    QueueErlang.put(messageToPlotProcess)


# receiving data from erlang to create the histogram
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

# drawing the histogram, matplotlib forces a process to freeze when using plt.show(), we bypassed it by creating a new process each time we want
# show the histogram ( this is why the histogram is flickering) and kill the last process who showed the histogram
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
    plotProcess = Process(target=preplotProcessLoop, args=())
    plotProcess.start()


def plotProcessStop():
    QueueErlang.put(["terminate"])


def preplotProcessLoop():
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)
    summaryWriter = tf.summary.create_file_writer(log_dir)
    plotProcessLoop(summaryWriter)



# this resembles a recieve block in erlang, we send messages through QueueErlang and decipher them
def plotProcessLoop(summaryWriter: tf.summary.SummaryWriter, drawHistogramProcess=None, iteration=0):
    global first
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
            plotProcessLoop(summaryWriter, drawHistogramProcess, iteration)  # recursive call for further instructions
    if item[0] =="stopHistogram":
        print("got to terminate draw histogram")
        if drawHistogramProcess is not None:
            drawHistogramProcess.terminate()  # it can only be a living process from a previous iteration
        else:
            print("From python: i shouldn't be here.....")
        plotProcessLoop(summaryWriter, None, iteration)
    if item[0] == "plot":
        [_, income, expence] = item
        with summaryWriter.as_default():
            tf.summary.scalar('income', income, step=iteration)
            tf.summary.scalar('expence', expence, step=iteration)
        plotProcessLoop(summaryWriter, drawHistogramProcess, iteration + 1)
    if item[0] == "terminate":
        print("I got to terminate")
        if drawHistogramProcess is not None:
                drawHistogramProcess.terminate()  # it can only be a living process from a previous iteration
        return
    else:
        plotProcessLoop(summaryWriter, drawHistogramProcess, iteration)


# main is for testing, please ignore
if __name__ == '__main__':
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)
    a = tf.summary.create_file_writer(log_dir)
    iteration = 0
    income = 5
    expence = 10
    with a.as_default():
        tf.summary.scalar('income', income, step=iteration)
        tf.summary.scalar('accuracy', expence, step=iteration)
    #webbrowser.open('http://example.com')
