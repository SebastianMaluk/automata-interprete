import ply.lex as lex
import ply.yacc as yacc

reserved={
    'mas' : 'SU',
    'menos' : 'RE',
    'es' : 'ASIGN'
}

tokens = [
    'N',
    'ID'
    ]+list(reserved.values())
    
t_SU=r'mas'
t_RE=r'menos'
t_ASIGN = r'es'

def t_N(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t
    
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t
    
t_ignore  = ' \t'

def t_error(t):
    print(":(")
    t.lexer.skip(1)

precedence=(('left', 'SU', 'RE'),)

variables={}

def p_resultado(t):
    'resultado : s'
    print(t[1])

def p_asignacion(t):
    'resultado : ID ASIGN s'
    variables[t[1]]=t[3]

def p_expr_num(t):
    's : N'
    t[0]=t[1]
    
def p_expr_id(t):
    's : ID'
    try:
        t[0] = variables[t[1]]
    except LookupError:
        print("Variable indefinida '%s'" % t[1])
        t[0] = 0
    
def p_expr_op(t):
    '''s : s SU s
        | s RE s'''
    if t[2] == 'mas':
        t[0]=t[1]+t[3]
    elif t[2] == 'menos':
        t[0]=t[1]-t[3]

def p_error(t):
    print(":'(")


lexer=lex.lex()
parser=yacc.yacc()
while True:
    try:
        data = input()
    except EOFError:
        break
    parser.parse(data)
