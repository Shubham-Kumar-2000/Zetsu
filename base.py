import string
LETTERS=string.ascii_letters
DIGITS='0123456789'
LETTERS_DIGITS=LETTERS+DIGITS
#creating tokens
INT = 'T_INT'
FLOAT = 'T_FLOAT'
PLUS = 'T_+'
MINUS = 'T_-'
MULTIPLY='T_*'
DIVIDE='T_/'
LBRAC='T_('
RBRAC='T_)'
POWER= 'T_^'
IDENTIFIER='T_Var'
KEYWORD='T_Keys'
EQUAL='T_='
VOID='T_void'
EE='T_=='
NE="T_!="
LT='T_<'
GT='T_>'
LTE='T_<='
GTE='T_>='
TSTRING='T_str'
VAR="T_variable"
FUNC="T_function"
KEYWORDS=[
	'let',
	'and',
	'or',
	'not',
	'if',
	'then',
	'else',
	'end',
	'print',
	'println',
	'for',
	'while',
	'until',
	'intInput',
	'floatInput',
	'strInput',
	'func',
	'return'
]
class Token:
	def __init__(self,typ,line=0,val=None):
		self.type=typ
		self.val=val
		self.line=line
	def __repr__(self):
		if self.val==None:
			return f'{self.type}'
		return f'[{self.val},{self.type}]'
	def matches(self,typ,value):
		if self.type==typ and self.val==value:
			return True
		return False
#Error
class Error:
	def __init__(self,start,err,stderr):
		self.start=start
		self.err=err
		self.stderr=stderr
	def asString(self):
		result=f'At line {self.start} :\n{self.err} : {self.stderr}'
		return result
class CharErr(Error):
	def __init__(self,start,stderr):
		super().__init__(start,'Unexpected Character',stderr)

class InvalidSyntaxError(Error):
	def __init__(self,start,stderr):
		super().__init__(start,'Invalid Syntax',stderr)

class RuntimeError(Error):
	def __init__(self,start,stderr):
		super().__init__(start,'Runtime error',stderr)

class InputError(Error):
	def __init__(self,start,stderr):
		super().__init__(start,'Input mismatch',stderr)

class Position:
	def __init__(self,index,line):
		self.index=index
		self.line=line
	def adv(self,cChar):
		self.index+=1
		if cChar=='\n':
			self.line+=1
		return self
	def getIndex(self):
		return self.index
	def getLine(self):
		return self.line
	def copy(self):
		return Position(self.index,self.line)
#MAKING LEXER

class Lexer:
	def __init__(self,text):
		self.text=text
		self.pos=Position(-1,0)
		self.cChar=None
		self.adv()
	def adv(self):
		self.pos.adv(self.cChar)
		self.cChar=self.text[self.pos.getIndex()] if self.pos.getIndex()<len(self.text) else None
	def makeTokens(self):
		tokens = []
		while self.cChar != None:
			if self.cChar in '\t' + ' '+'\n':
				self.adv()
			elif self.cChar in DIGITS:
				tokens.append(self.createNumber())
			elif self.cChar in LETTERS:
				tokens.append(self.makeIdentifiers())
			elif self.cChar == '+':
				tokens.append(Token(PLUS, self.pos.getLine()))
				self.adv()
			elif self.cChar == '-':
				tokens.append(Token(MINUS, self.pos.getLine()))
				self.adv()
			elif self.cChar == '=':
				tokens.append(self.makeEqual())
			elif self.cChar == '<':
				tokens.append(self.makeLE())
			elif self.cChar=='!':
				token,err=self.makeNE()
				if err:
					return [],err
				tokens.append(token)
			elif self.cChar=='"':
				token,err=self.makeString()
				if err:
					return [],err
				tokens.append(token)
			elif self.cChar=='>':
				tokens.append(self.makeGE())
				self.adv()
			elif self.cChar == '*':
				tokens.append(Token(MULTIPLY, self.pos.getLine()))
				self.adv()
			elif self.cChar == '/':
				tokens.append(Token(DIVIDE, self.pos.getLine()))
				self.adv()
			elif self.cChar == '(':
				tokens.append(Token(LBRAC, self.pos.getLine()))
				self.adv()
			elif self.cChar == ')':
				tokens.append(Token(RBRAC, self.pos.getLine()))
				self.adv()
			elif self.cChar == '^':
				tokens.append(Token(POWER, self.pos.getLine()))
				self.adv()
			else:
				char = self.cChar
				self.adv()
				return ([], CharErr(self.pos.getLine(), "'" + char + "'"+ ((' after token ' + repr(tokens[-1]) if len(tokens)> 0 else ' '))))
		return (tokens, None)
	def makeNE(self):
		self.adv()
		if self.cChar=='=':
			self.adv()
			return Token(NE,self.pos.getLine(),None),None
		return None,CharErr(self.pos.getLine(),"Expected '=' after '!'")
	def makeEqual(self):
		self.adv()
		if self.cChar=='=':
			self.adv()
			return Token(EE,self.pos.getLine(),None)
		return Token(EQUAL,self.pos.getLine(),None)
	def makeLE(self):
		self.adv()
		if self.cChar=='=':
			self.adv()
			return Token(LTE,self.pos.getLine(),None)
		return Token(LT,self.pos.getLine(),None)
	def makeGE(self):
		self.adv()
		if self.cChar=='=':
			self.adv()
			return Token(GTE,self.pos.getLine(),None)
		return Token(GT,self.pos.getLine(),None)
	def createNumber(self):
		num=''
		fDot=0
		while self.cChar!=None and self.cChar in DIGITS or self.cChar=='.':
			if self.cChar=='.':
				if fDot==1:
					break
				fDot=1
			num+=self.cChar
			self.adv()
		if fDot==0:
			return Token(INT,self.pos.getLine(),int(num))
		else:
			return Token(FLOAT,self.pos.getLine(),float(num))
	def makeIdentifiers(self):
		st=''
		while self.cChar!=None and self.cChar in LETTERS_DIGITS+'_':
			st+=self.cChar
			self.adv()
		if(st in KEYWORDS):
			return Token(KEYWORD,self.pos.getLine(),st)
		return Token(IDENTIFIER,self.pos.getLine(),st)
	def makeString(self):
		st=''
		self.adv()
		while self.cChar!='None' and self.cChar!='"':
			st+=self.cChar
			self.adv()
		if self.cChar=='"':
			self.adv()
			return Token(TSTRING,self.pos.getLine(),st),None
		return None,CharErr(self.pos.getLine(),"Expected ' \" ' ")
class NumNode:
	def __init__(self,tok):
		self.tok=tok
	def __repr__(self):
		return f'{self.tok}'

class BinOpNode:
	def __init__(self,leftNode,opTok,rightNode):
		self.leftNode=leftNode
		self.opTok=opTok
		self.rightNode=rightNode
	def __repr__(self):
		return f'({self.leftNode},{self.opTok},{self.rightNode})'

class UnaryOpNode:
	def __init__(self,opTok,Node):
		self.opTok=opTok
		self.Node=Node
	def __repr__(self):
		return f'({self.opTok},{self.Node})'

class IdentifierTable:
	def __init__(self):
		self.identifiers={
			"True":Token(INT,0,1),
			"False":Token(INT,0,0)
		}
		self.parent=None
	def get(self,name):
		val=self.identifiers.get((VAR,name.val),VOID)
		return val
	def assign(self,name,value):
		self.identifiers[(VAR,name.val)]=value
	def update(self,name,value):
		if self.get(name)==VOID:
			return False
		self.assign(name,value)
		return True
	def remove(self,name):
		del self.identifiers[name]
	def getfunction(self,name):
		val=self.identifiers.get((FUNC,name.val),VOID)
		return val
	def assignfunction(self,name,value):
		self.identifiers[(FUNC,name.val)]=value

class VarAccess:
	def __init__(self,tok):
		self.var=tok
	def __repr__(self):
		return f'[{self.var}]'
	def update(self,val):
		self.val=val

class VarAssign:
	def __init__(self,var,exp):
		self.var=var
		self.exp=exp

class VarUpdate:
	def __init__(self,var,exp):
		self.var=var
		self.exp=exp

class InputNode:
	def __init__(self,typ,line):
		self.typ=typ
		self.line=line

class NextNode:
	def __init__(self,prev,next):
		self.left=prev
		self.right=next
	def __repr__(self):
		return f'({self.left},{self.right})'

class LoopNode:
	def __init__(self,check,excu):
		self.check=check
		self.excu=excu

class ifElseNode:
	def __init__(self,exp,texcu,fexcu):
		self.exp=exp
		self.texcu=texcu
		self.fexcu=fexcu

class createFunctionNode:
	def __init__(self,name,parameterCount,parameterList,retBool,ret,ex):
		self.name=name
		self.paraCount=parameterCount
		self.paraList=parameterList
		self.ret=ret
		self.ex=ex
		self.bool=retBool

class excuFunctionNode:
	def __init__(self,name,parameterCount,parameterList):
		self.name=name
		self.paraCount=parameterCount
		self.paraList=parameterList

class Parser:
	def __init__(self,tokens):
		self.tokens=tokens
		self.tokIndx=-1
		self.currentTok=None
		self.adv()
	def adv(self):
		self.tokIndx+=1
		if(self.tokIndx<len(self.tokens)):
			self.currentTok=self.tokens[self.tokIndx]
		else:
			self.currentTok=None
		return self.currentTok
	def parse(self):
		res=self.exp()
		if res.error:
			return res
		if self.currentTok!=None and self.currentTok.val!='end':
			nextExp=self.parse()
			if nextExp.error:
				return nextExp
			return ParseResult().success(NextNode(res.node,nextExp.node))
		return res
	def factor(self):
		res=ParseResult()
		tok=self.currentTok
		if self.currentTok!= None and tok.type in (PLUS,MINUS):
			res.register(self.adv())
			factor=res.register(self.factor())
			if(res.error):
				return res
			return res.success(UnaryOpNode(tok,factor))
		if(tok.type==IDENTIFIER):
			res.register(self.adv())
			if self.currentTok!=None and self.currentTok.type==LBRAC:
				parameter_count=0
				parameter_list=[]
				res.register(self.adv())
				while self.currentTok!=None and self.currentTok.type!=RBRAC:
					var=res.register(self.exp())
					if res.error:
						return res
					parameter_count+=1
					parameter_list.append(var)
				if self.currentTok==None or self.currentTok.type!=RBRAC:
					return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected ')'"))
				res.register(self.adv())
				return res.success(excuFunctionNode(tok,parameter_count,parameter_list))
			return res.success(VarAccess(tok))
		if self.currentTok!= None and tok.type==LBRAC:
			res.register(self.adv())
			exp=res.register(self.exp())
			if(res.error):
				return res
			if self.currentTok!= None and self.currentTok.type==RBRAC:
				res.register(self.adv())
				return res.success(exp)
			else:
				if(self.currentTok==None):
					return res.fail(InvalidSyntaxError(self.tokens[-1].line,"Expected )"))
				return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected )"))
		if(self.currentTok!= None and tok.type in (INT,FLOAT,TSTRING)):
			res.register(self.adv())
			return res.success(NumNode(tok))
		if(tok==None):
			 return res.fail(InvalidSyntaxError(self.tokens[-1].line,"Expected Int or FLOAT"))
		if tok.matches(KEYWORD,'if'):
			return self.ifelse()
		if tok.matches(KEYWORD,'for'):
			return self.forloop()
		if tok.matches(KEYWORD,'while'):
			return self.whileLoop()
		if tok.matches(KEYWORD,'intInput'):
			res.register(self.adv())
			return res.success(InputNode(INT,tok.line))
		if tok.matches(KEYWORD,'floatInput'):
			res.register(self.adv())
			return res.success(InputNode(FLOAT,tok.line))
		if tok.matches(KEYWORD,'strInput'):
			res.register(self.adv())
			return res.success(InputNode(TSTRING,tok.line))
		return res.fail(InvalidSyntaxError(tok.line,"Expected Int or FLOAT"))
	def forloop(self):
		tok=self.currentTok
		res=ParseResult()
		res.register(self.adv())
		exp=res.register(self.exp())
		if res.error:
			return res
		if self.currentTok== None or self.currentTok.val!= 'until':
			return res.fail(CharErr(tok.line,"Expected 'until' Keyword"))
		res.register(self.adv())
		checkExp=res.register(self.exp())
		if res.error:
			return res
		if self.currentTok== None or self.currentTok.val!= 'then':
			return res.fail(CharErr(tok.line,"Expected 'then' Keyword"))
		res.register(self.adv())
		excu=res.register(self.parse())
		if res.error:
			return res
		res.register(self.adv())
		return res.success(NextNode(exp,LoopNode(checkExp,excu)))
	def whileLoop(self):
		tok=self.currentTok
		res=ParseResult()
		res.register(self.adv())
		checkExp=res.register(self.exp())
		if res.error:
			return res
		if self.currentTok== None or self.currentTok.val!= 'then':
			return res.fail(CharErr(tok.line,"Expected 'then' Keyword"))
		res.register(self.adv())
		excu=res.register(self.parse())
		if res.error:
			return res
		res.register(self.adv())
		return res.success(LoopNode(checkExp,excu))
	def term(self):
		res=ParseResult()
		left=res.register(self.factor())
		if res.error:
			return res
		while self.currentTok!= None and self.currentTok.type in (MULTIPLY,DIVIDE,POWER):
			opToken=self.currentTok
			res.register(self.adv())
			if res.error:
				return res
			right=res.register(self.factor())
			if res.error:
				return res
			left= BinOpNode(left,opToken,right)
		return res.success(left)
	def exp(self):
		res=ParseResult()
		if self.currentTok==None or self.currentTok.val=='end':
			return res.success(NumNode(Token(VOID,0,None)))
		if self.currentTok.matches(KEYWORD,'func'):
			tok=self.currentTok
			res.register(self.adv())
			if self.currentTok.type!=IDENTIFIER:
				return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected Identifier"))
			funcName=self.currentTok
			res.register(self.adv())
			if self.currentTok.type!=LBRAC:
				return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected '('"))
			parameter_count=0
			parameter_list=[]
			res.register(self.adv())
			while self.currentTok!=None and self.currentTok.type!=RBRAC:
				if self.currentTok.matches(KEYWORD,'let')==False:
					return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected 'let' keyword"))
				res.register(self.adv())
				if self.currentTok!=None and self.currentTok.type!=IDENTIFIER:
					return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected Identifier"))
				parameter_count+=1
				parameter_list.append(self.currentTok)
				res.register(self.adv())
			if self.currentTok==None or self.currentTok.type!=RBRAC:
				return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected ')'"))
			res.register(self.adv())
			retBool=True
			if not self.currentTok.matches(KEYWORD,'return'):
				retBool=False
			else:
				res.register(self.adv())
			if retBool:
				if not self.currentTok.matches(KEYWORD,'let'):
					return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected 'let' keyword"))
				res.register(self.adv())
				if self.currentTok!=None and self.currentTok.type!=IDENTIFIER:
					return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected Identifier"))
				ret=self.currentTok
				res.register(self.adv())
			else:
				ret=Token(VOID,tok.line,None)
			if not self.currentTok.matches(KEYWORD,'then'):
				return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected 'then' keyword"))
			res.register(self.adv())
			ex=res.register(self.parse())
			res.register(self.adv())
			if res.error:
				return res
			return res.success(createFunctionNode(funcName,parameter_count,parameter_list,retBool,ret,ex))
		if self.currentTok.matches(KEYWORD,'print')or self.currentTok.matches(KEYWORD,'println'):
			tok=self.currentTok
			res.register(self.adv())
			exp=res.register(self.exp())
			if res.error:
				return res
			return res.success(UnaryOpNode(tok,exp))
		if self.currentTok.matches(KEYWORD,'let'):
			res.register(self.adv())
			if self.currentTok== None or self.currentTok.type!= IDENTIFIER:
				return res.fail(InvalidSyntaxError(self.currentTok.line,"Expected Identifier"))
			varName=self.currentTok
			res.register(self.adv())
			if self.currentTok== None or self.currentTok.type!= EQUAL:
				return res.success(VarAssign(varName,NumNode(Token(VOID,varName.line,None))))
			res.register(self.adv())
			exp=res.register(self.exp())
			if res.error:
				return res
			return res.success(VarAssign(varName,exp))
		if self.currentTok.type==IDENTIFIER and (self.tokIndx+1)<len(self.tokens) and self.tokens[self.tokIndx+1].type==EQUAL:
			varName=self.currentTok
			res.register(self.adv())
			res.register(self.adv())
			exp=res.register(self.exp())
			if res.error:
				return res
			return res.success(VarUpdate(varName,exp))
		left=res.register(self.compOP())
		if res.error:
			return res
		while self.currentTok!= None and self.currentTok.type==KEYWORD and self.currentTok.val in ('and','or'):
			opToken=self.currentTok
			res.register(self.adv())
			if res.error:
				return res
			right=res.register(self.compOP())
			if res.error:
				return res
			left= BinOpNode(left,opToken,right)
		return res.success(left)
	def compOP(self):
		res=ParseResult()
		if  self.currentTok!= None and self.currentTok.matches(KEYWORD,'not'):
			tok=self.currentTok
			res.register(self.adv())
			comp=res.register(self.compOP())
			if res.error:
				return res
			return res.success(UnaryOpNode(tok,comp))
		left=res.register(self.arithOP())
		if res.error:
			return res
		while self.currentTok!= None and self.currentTok.type in (EE,NE,LT,LTE,GT,GTE):
			opToken=self.currentTok
			res.register(self.adv())
			if res.error:
				return res
			right=res.register(self.arithOP())
			if res.error:
				return res
			left= BinOpNode(left,opToken,right)
		return res.success(left)
	def arithOP(self):
		res=ParseResult()
		left=res.register(self.term())
		if res.error:
			return res
		while self.currentTok!= None and self.currentTok.type in (PLUS,MINUS):
			opToken=self.currentTok
			res.register(self.adv())
			if res.error:
				return res
			right=res.register(self.term())
			if res.error:
				return res
			left= BinOpNode(left,opToken,right)
		return res.success(left)
	def ifelse(self):
		res=ParseResult()
		tok=self.currentTok
		res.register(self.adv())
		exp=res.register(self.exp())
		if res.error:
			return res
		if self.currentTok== None or self.currentTok.val!= 'then':
			return res.fail(CharErr(tok.line,"Expected 'then' Keyword"))
		res.register(self.adv())
		texcu=res.register(self.parse())
		if res.error:
			return res
		res.register(self.adv())
		if self.currentTok== None or self.currentTok.val!= 'else':
			return res.success(ifElseNode(exp,texcu,NumNode(Token(VOID,tok.line,None))))
		res.register(self.adv())
		fexcu=res.register(self.parse())
		if res.error:
			return res
		res.register(self.adv())
		return res.success(ifElseNode(exp,texcu,fexcu))

class ParseResult:
	def __init__(self):
		self.error=None
		self.node=None
	def register(self,res):
		if isinstance(res,ParseResult):
			if(res.error):
				self.error=res.error
			return res.node
		return res
	def success(self,node):
		self.node=node
		return self
	def fail(self,error):
		self.error=error
		return self

class RTresult:
	def __init__(self):
		self.error=None
		self.node=None
	def register(self,res):
		if isinstance(res,RTresult):
			if(res.error):
				self.error=res.error
			return res.node
		return res
	def success(self,node):
		self.node=node
		return self
	def fail(self,error):
		self.error=error
		return self

class Interpreter:
	def __init__(self,ast,table):
		self.ast=ast
		self.table=table
	def visit(self,node):
		if isinstance(node,createFunctionNode):
			return self.createFuncOp(node)
		if isinstance(node,excuFunctionNode):
			return self.excuFuncOp(node)
		if isinstance(node,InputNode):
			return self.inOp(node)
		if isinstance(node,LoopNode):
			return self.loopOp(node)
		if isinstance(node,ifElseNode):
			return self.ifElseOp(node)
		if isinstance(node,NextNode):
			return self.nextOp(node)
		if isinstance(node,VarAccess):
			return self.varAccessOp(node)
		if isinstance(node,VarUpdate):
			return self.varUpdateOp(node)
		if isinstance(node,VarAssign):
			return self.varAssignOp(node)
		if isinstance(node,BinOpNode):
			return self.performBinOp(node)
		if isinstance(node,UnaryOpNode):
			return self.performUnaryOp(node)
		if isinstance(node,NumNode):
			return RTresult().success(node.tok)
	def createFuncOp(self,node):
		self.table.assignfunction(node.name,node)
		return RTresult().success(Token(VOID,node.name.line,None))
	def excuFuncOp(self,node):
		res=RTresult()
		val=self.table.getfunction(node.name)
		if val==VOID:
			return res.fail(RuntimeError(node.name.line,"Function not Defined"))
		if val.paraCount!=node.paraCount:
			return res.fail(RuntimeError(node.name.line,"Function parameter mismatch"))
		newTable=IdentifierTable()
		funcInterpreter=Interpreter(val.ex,newTable)
		para=0
		while para<val.paraCount:
			assign=res.register(self.visit(node.paraList[para]))
			if res.error:
				return res
			perform=VarAssign(val.paraList[para],NumNode(assign))
			ex=res.register(funcInterpreter.visit(perform))
			if res.error:
				return res
			para+=1
		if val.bool:
			perform=VarAssign(val.ret,NumNode(Token(VOID,val.name.line,None)))
			ex=res.register(funcInterpreter.visit(perform))
			if res.error:
				return res
		ex=res.register(funcInterpreter.visit(val.ex))
		if res.error:
			return res
		if val.bool:
			perform=VarAccess(val.ret)
			ex=res.register(funcInterpreter.visit(perform))
			if res.error:
				return res
		else:
			ex=Token(VOID,node.name.line,None)
		return res.success(ex)
	def inOp(self,node):
		i=input()
		res=RTresult()
		try:
			if node.typ==INT:
				val = int(i)
			elif node.typ==FLOAT:
				val = float(i)
			else:
				val=val
		except ValueError:
			return res.fail(InputError(node.line,('expected '+node.typ)))
		return res.success(Token(node.typ,node.line,val))
	def loopOp(self,node):
		res=RTresult()
		exp=res.register(self.visit(node.check))
		if res.error:
			return res
		while exp.val!=0:
			ex=res.register(self.visit(node.excu))
			if res.error:
				return res
			exp=res.register(self.visit(node.check))
			if res.error:
				return res
		return res.success(ex)	
	def ifElseOp(self,node):
		res=RTresult()
		exp=res.register(self.visit(node.exp))
		if res.error:
			return res
		if exp.val!=0:
			ex=res.register(self.visit(node.texcu))
		else:
			ex=res.register(self.visit(node.fexcu))
		if res.error:
			return res
		return res.success(ex)
	def nextOp(self,node):
		res=RTresult()
		l=res.register(self.visit(node.left))
		if res.error:
			return res
		r=res.register(self.visit(node.right))
		if res.error:
			return res
		return res.success(r)
	def varAssignOp(self,node):
		res=RTresult()
		val=res.register(self.visit(node.exp))
		if res.error:
			return res
		self.table.assign(node.var,val)
		return res.success(Token(val.type,node.var.line,val.val))
	def varUpdateOp(self,node):
		res=RTresult()
		val=res.register(self.visit(node.exp))
		if res.error:
			return res
		if self.table.update(node.var,val):
			return res.success(Token(val.type,node.var.line,val.val))
		return res.fail(RuntimeError(node.var.line,"Identifier not Defined"))
	def varAccessOp(self,node):
		res=RTresult()
		val=self.table.get(node.var)
		if val==VOID:
			return res.fail(RuntimeError(node.var.line,"Identifier not Defined"))
		return res.success(val)
	def performBinOp(self,node):
		err=False
		res=RTresult()
		l=res.register(self.visit(node.leftNode))
		if res.error:
			return res
		r=res.register(self.visit(node.rightNode))
		if res.error:
			return res
		if node.opTok.type==KEYWORD and (not (l.type==TSTRING or r.type==TSTRING)):
			if node.opTok.val=='and':
				result=int(l.val and r.val)
			if node.opTok.val=='or':
				result=int(l.val or r.val)
			return res.success(Token(INT,node.opTok.line,result))
		if l.val==None:
			return res.fail(RuntimeError(l.line,'Recieved None is expression'))
		if r.val==None:
			return res.fail(RuntimeError(r.line,'Recieved None is expression'))
		if l.type==TSTRING or r.type==TSTRING:
			r=r.val
			l=l.val
			typ=INT
			if(node.opTok.type==PLUS):
				result=l+r
				typ=TSTRING
			elif(node.opTok.type==EE):
				result=int(l==r)
			elif(node.opTok.type==NE):
				result=int(l!=r)
			else:
				return res.fail(RuntimeError(node.opTok.line,"Incompitable operation on Operands"))
			return res.success(Token(typ,node.opTok.line,result))
		r=r.val
		l=l.val
		result=0
		if(node.opTok.type==PLUS):
			result=l+r
		if(node.opTok.type==MINUS):
			result=l-r
		if(node.opTok.type==MULTIPLY):
			result=l*r
		if(node.opTok.type==POWER):
			result=pow(l,r)
		if(node.opTok.type==DIVIDE):
			if r==0:
				err=True
				stderr=RuntimeError(node.opTok.line,"Divide by zero")
			else:
				result=l/r
		if(node.opTok.type==EE):
			result=int(l==r)
		if(node.opTok.type==NE):
			result=int(l!=r)
		if(node.opTok.type==LTE):
			result=int(l<=r)
		if(node.opTok.type==GTE):
			result=int(l>=r)
		if(node.opTok.type==LT):
			result=int(l<r)
		if(node.opTok.type==GT):
			result=int(l>r)
		if err:
			return res.fail(stderr)
		if isinstance(result,int):
			return res.success(Token(INT,node.opTok.line,result))
		return res.success(Token(FLOAT,node.opTok.line,result))
	def performUnaryOp(self,node):
		res=RTresult()
		tok=res.register(self.visit(node.Node))
		if res.error:
			return res
		if node.opTok.type==KEYWORD:
			if node.opTok.val=='not':
				tok.val=int(not tok.val)
			if node.opTok.val=='println':
				print(tok.val)
			if node.opTok.val=='print':
				print(tok.val,end='')
			return res.success(tok)
		if tok.val==None:
			return res.fail(RuntimeError(tok.line,'Recieved None is expression'))
		if tok.type==TSTRING:
			return res.fail(RuntimeError(node.opTok.line,"Incompitable operation on Operand"))
		if(node.opTok.type==MINUS):
			tok.val=0-tok.val
		return res.success(tok)

def run(text,i):
	lexer=Lexer(text)
	tokens,error=lexer.makeTokens()
	if error:
		return None, error
	par=Parser(tokens)
	ast=par.parse()
	if ast.error:
		return ast.node,ast.error
	res=Interpreter(ast.node,i).visit(ast.node)
	return '',res.error