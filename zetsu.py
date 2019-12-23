import base
tab=base.IdentifierTable()
import sys
argumentList = sys.argv 
if len(argumentList)>1:
    fext= argumentList[1].split('.')
    if len(fext)>1 and fext[1]=='zs':
        f = open(argumentList[1], "r")
        res,err=base.run(f.read(),tab)
        if err: print(err.asString())
    else:
        print('File format Not supported')
else:
    while True:
        code=input("ZETSU >>>")
        res,err=base.run(code,tab)
        if err: print(err.asString())