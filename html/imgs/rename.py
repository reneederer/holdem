import os
for filename in os.listdir("."):
    if filename.startswith("2"):
        os.rename(filename, "two" + filename[1:])
    if filename.startswith("3"):
        os.rename(filename, "three" + filename[1:])
    if filename.startswith("4"):
        os.rename(filename, "four" + filename[1:])
    if filename.startswith("5"):
        os.rename(filename, "five" + filename[1:])
    if filename.startswith("6"):
        os.rename(filename, "six" + filename[1:])
    if filename.startswith("7"):
        os.rename(filename, "seven" + filename[1:])
    if filename.startswith("8"):
        os.rename(filename, "eight" + filename[1:])
    if filename.startswith("9"):
        os.rename(filename, "nine" + filename[1:])
    if filename.startswith("10"):
        os.rename(filename, "ten" + filename[1:])
    if filename.endswith("2.svg"):
        os.remove(filename[:-5] + ".svg")
        os.rename(filename, filename[:-5] + ".svg")
