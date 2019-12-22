import base
tab=base.IdentifierTable()
while True:
    code=input("ZETSU >>>")
    res,err=base.run(code,tab)
    if err: print(err.asString())