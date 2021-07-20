import glob

files=glob.glob('./test/**/*.in',recursive=True)

for file in files:
    print(file)
    with open(file,'r+') as f:
        f.seek(0)
        s=f.read().replace(' ','\n')+' '
        f.seek(0)
        f.write(s)
        